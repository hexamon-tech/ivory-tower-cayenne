{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Tower.Cayenne.Types where

import Ivory.Language
import Ivory.Tower
import Ivory.Serialize

import Control.Monad (foldM)
import GHC.TypeLits (KnownNat)

type StoredRef s a = Ref s ('Stored a)

data Sensor s =
    DigitalIn     (StoredRef s Uint8)  -- ^ Digital input (8 bits)
  | DigitalOut    (StoredRef s Uint8)  -- ^ Digital output (8 bits)
  | AnalogIn      (StoredRef s IFloat) -- ^ Analog input
  | AnalogOut     (StoredRef s IFloat) -- ^ Analog output
  | Illum         (StoredRef s Uint16) -- ^ Illuminance sensor (Lux)
  | Presence      (StoredRef s Uint8)  -- ^ Presence
  | Temperature   (StoredRef s IFloat) -- ^ Temperature (Celsius)
  | Humidity      (StoredRef s IFloat) -- ^ Humidity (%)
  | Barometer     (StoredRef s IFloat) -- ^ Barometer (hPa)
  | Voltage       (StoredRef s IFloat) -- ^ Voltage (V)
  | Current       (StoredRef s IFloat) -- ^ Current (A)
  | Percentage    (StoredRef s IFloat) -- ^ Percentage
  | Pressure      (StoredRef s IFloat) -- ^ Pressure
  | Power         (StoredRef s IFloat) -- ^ Power (W)
  | Energy        (StoredRef s IFloat) -- ^ Energy (J)
  | Direction     (StoredRef s IFloat) -- ^ Angle (Deg)
  | Accelerometer (StoredRef s IFloat) (StoredRef s IFloat) (StoredRef s IFloat)  -- ^ Accelerometer (G)
  | Gyrometer     (StoredRef s IFloat) (StoredRef s IFloat) (StoredRef s IFloat)  -- ^ Gyrometer (°/s)
  | GPS           (StoredRef s IFloat) (StoredRef s IFloat) (StoredRef s IFloat)  -- ^ GPS Latitude (°) ,Longitude (°), Altitude (m)

packCayenne :: (KnownNat len)
            => Ref s ('Array len ('Stored Uint8))
            -> [Sensor s1]
            -> Ivory (AllocEffects s2) Uint32
packCayenne buf sensors = packCayenneChannels buf (map (\x -> (0, x)) sensors)

packCayenneChannels :: (KnownNat len)
                    => Ref s ('Array len ('Stored Uint8))
                    -> [(Integer, Sensor s1)]
                    -> Ivory (AllocEffects s2) Uint32
packCayenneChannels buf sensors = do
  foldM (\off (chan, sensor) -> do

    -- channel
    (c :: Ref ('Stack s2) ('Stored Uint8)) <- local $ ival $ fromIntegral chan
    packInto buf off (constRef c)

    -- sensor id
    (i :: Ref ('Stack s2) ('Stored Uint8)) <- local $ ival $ fromIntegral (toID sensor)
    packInto buf (off + 1) (constRef i)

    -- sensor data
    noff <- packSensor buf (off + 2) sensor
    return noff -- (off + 2 + len)

    ) 0 sensors

packSensor :: KnownNat len
           => Ref s ('Array len ('Stored Uint8))
           -> Uint32
           -> Sensor s1
           -> Ivory (AllocEffects s2) Uint32
packSensor buf off (DigitalIn val)   = packUint8 buf off val
packSensor buf off (DigitalOut val)  = packUint8 buf off val
packSensor buf off (AnalogIn val)    = packFloat16Scaled (*100) buf off val
packSensor buf off (AnalogOut val)   = packFloat16Scaled (*100) buf off val
packSensor buf off (Illum val)       = packInto buf off (constRef val) >> return (off + 2)
packSensor buf off (Presence val)    = packUint8 buf off val
packSensor buf off (Temperature val) = packFloat16Scaled (*10) buf off val
packSensor buf off (Humidity val)    = do
  cval <- deref val
  x <- local $ ival $ (castDefault :: IFloat -> Uint8) $ roundF $ (*2) cval
  packUint8 buf off x
packSensor buf off (Barometer val)   = packFloat16Scaled (*10) buf off val
packSensor buf off (Voltage val)     = packFloat16Scaled (*10) buf off val
packSensor buf off (Current val)     = packFloat16Scaled (*10) buf off val
packSensor buf off (Percentage val)  = packFloat16Scaled id    buf off val
packSensor buf off (Pressure val)    = packFloat16Scaled (*10) buf off val
packSensor buf off (Power val)       = packFloat16Scaled (*10) buf off val
packSensor buf off (Energy val)      = packFloat16Scaled (*10) buf off val
packSensor buf off (Direction val)   = packFloat16Scaled id    buf off val

packSensor buf off (Accelerometer x y z) = do
  let pack o val = packFloat16Scaled (*1000) buf o val
  foldM pack off [x, y, z]

packSensor buf off (Gyrometer x y z) = do
  let pack o val = packFloat16Scaled (*100) buf o val
  foldM pack off [x, y, z]

packSensor buf off (GPS lat lon alt) = do
  let pack o val = packFloat24Scaled (*10000) buf o val
  o2 <- foldM pack off [lat, lon]
  packFloat24Scaled (*100) buf o2 alt

packUint8 :: (KnownNat len)
          => Ref s ('Array len ('Stored Uint8))
          -> Uint32
          -> Ref s1 ('Stored Uint8)
          -> Ivory (AllocEffects s2) Uint32
packUint8 buf off val = packInto buf off (constRef val) >> return (off + 1)

packFloat16Scaled :: (KnownNat len)
                  => (IFloat -> IFloat)
                  -> Ref s ('Array len ('Stored Uint8))
                  -> Uint32
                  -> Ref s1 ('Stored IFloat)
                  -> Ivory (AllocEffects s2) Uint32
packFloat16Scaled scale buf off val = do
  cval <- deref val
  x <- local $ ival $ (castDefault :: IFloat -> Uint16) $ roundF $ scale cval
  packInto buf off (constRef x)
  return (off + 2)

packFloat24Scaled :: (KnownNat len)
                  => (IFloat -> IFloat)
                  -> Ref s ('Array len ('Stored Uint8))
                  -> Uint32
                  -> Ref s1 ('Stored IFloat)
                  -> Ivory (AllocEffects s2) Uint32
packFloat24Scaled scale buf off val = do
  cval <- deref val
  let val' = (castDefault :: IFloat -> Uint32) $ roundF $ scale cval
  h <- local $ ival $ (bitCast :: Uint32 -> Uint8) $ val' `iShiftR` 16
  l <- local $ ival $ (bitCast :: Uint32 -> Uint16) $ val'
  packInto buf off (constRef h)
  packInto buf (off + 1) (constRef l)
  return (off + 3)

toID :: Sensor s -> Int
toID (DigitalIn _)         = 0x00
toID (DigitalOut _)        = 0x01
toID (AnalogIn _)          = 0x02
toID (AnalogOut _)         = 0x03
toID (Illum _)             = 0x65
toID (Presence _)          = 0x66
toID (Temperature _)       = 0x67
toID (Humidity _)          = 0x68
toID (Accelerometer _ _ _) = 0x71
toID (Barometer _)         = 0x73
toID (Voltage _)           = 0x74
toID (Current _)           = 0x75
toID (Percentage _)        = 0x78
toID (Pressure _)          = 0x7b
toID (Power _)             = 0x80
toID (Energy _)            = 0x83
toID (Direction _)         = 0x84
toID (Gyrometer _ _ _)     = 0x86
toID (GPS _ _ _)           = 0x88

cayenneTowerDeps :: Tower e ()
cayenneTowerDeps = do
  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

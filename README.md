ivory-tower-cayenne
===================

Encode sensor data with [Cayenne Low Power Protocol](https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md#cayenne-low-power-payload)

Usage
------

To create a message containing `Temperature` and `Humidity`
readings and pack it into string use the following example:

```haskell
import Ivory.Tower.Cayenne

...

(arr :: Ref ('Stack s) ('Array len ('Stored Uint8))) <- local $ izero
len <- packCayenne arr [
    Temperature (lastSample ~> sample_th_temperature)
  , Humidity    (lastSample ~> sample_th_humidity)
  ]

(strbuf :: Ref ('Stack s) UARTBuffer) <- local $ izero
refCopy (strbuf ~> stringDataL) arr
store (strbuf ~> stringLengthL) (signCast len)
```

It is also possible to specify sensor channels with `packCayenneChannels` like this:

```haskell
len <- packCayenneChannels arr [
    (0, Temperature (sample0 ~> sample_th_temperature))
    (1, Temperature (sample1 ~> sample_th_temperature))
  ]
```

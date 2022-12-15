{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Flat.Orphans where

import Data.Fixed
import Data.Time
import Flat as F


deriving via Integer instance Flat Day

instance Flat DiffTime where
  encode = F.encode . diffTimeToPicoseconds
  decode = picosecondsToDiffTime <$> F.decode
  size = F.size . diffTimeToPicoseconds

utcTimeToTuple :: UTCTime -> (Day, DiffTime)
utcTimeToTuple UTCTime{..} = (utctDay, utctDayTime)

instance Flat UTCTime where
  encode = F.encode . utcTimeToTuple
  decode = uncurry UTCTime  <$> F.decode
  size = F.size . utcTimeToTuple

timeOfDayToTuple :: TimeOfDay -> (Int, Int, Pico)
timeOfDayToTuple TimeOfDay{..} = (todHour, todMin, todSec)

tupleToTimeOfDay :: (Int, Int, Pico) -> TimeOfDay
tupleToTimeOfDay (todHour, todMin, todSec) = TimeOfDay{..}

instance Flat TimeOfDay where
  encode = F.encode . timeOfDayToTuple
  decode = tupleToTimeOfDay <$> F.decode
  size = F.size . timeOfDayToTuple

localTimeToTuple :: LocalTime -> (Day, TimeOfDay)
localTimeToTuple LocalTime{..} = (localDay, localTimeOfDay)

instance Flat LocalTime where
  size   = F.size . localTimeToTuple
  encode = F.encode . localTimeToTuple
  decode = uncurry LocalTime <$> F.decode

timeZoneToTuple :: TimeZone -> (Int, Bool, String)
timeZoneToTuple TimeZone{..} =
  (timeZoneMinutes, timeZoneSummerOnly, timeZoneName)

tupleToTimeZone :: (Int, Bool, String) -> TimeZone
tupleToTimeZone (timeZoneMinutes, timeZoneSummerOnly, timeZoneName) =
  TimeZone{..}

instance Flat TimeZone where
  size   = F.size . timeZoneToTuple
  encode = F.encode . timeZoneToTuple
  decode = tupleToTimeZone <$> F.decode

zonedTimeToTuple :: ZonedTime -> (LocalTime, TimeZone)
zonedTimeToTuple ZonedTime{..} = (zonedTimeToLocalTime, zonedTimeZone)

instance Flat ZonedTime where
  size   = F.size . zonedTimeToTuple
  encode = F.encode . zonedTimeToTuple
  decode = uncurry ZonedTime <$> F.decode

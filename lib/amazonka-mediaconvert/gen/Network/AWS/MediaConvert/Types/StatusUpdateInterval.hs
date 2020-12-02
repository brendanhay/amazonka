{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StatusUpdateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.StatusUpdateInterval where

import Network.AWS.Prelude

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
data StatusUpdateInterval
  = Seconds10
  | Seconds12
  | Seconds120
  | Seconds15
  | Seconds180
  | Seconds20
  | Seconds240
  | Seconds30
  | Seconds300
  | Seconds360
  | Seconds420
  | Seconds480
  | Seconds540
  | Seconds60
  | Seconds600
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText StatusUpdateInterval where
  parser =
    takeLowerText >>= \case
      "seconds_10" -> pure Seconds10
      "seconds_12" -> pure Seconds12
      "seconds_120" -> pure Seconds120
      "seconds_15" -> pure Seconds15
      "seconds_180" -> pure Seconds180
      "seconds_20" -> pure Seconds20
      "seconds_240" -> pure Seconds240
      "seconds_30" -> pure Seconds30
      "seconds_300" -> pure Seconds300
      "seconds_360" -> pure Seconds360
      "seconds_420" -> pure Seconds420
      "seconds_480" -> pure Seconds480
      "seconds_540" -> pure Seconds540
      "seconds_60" -> pure Seconds60
      "seconds_600" -> pure Seconds600
      e ->
        fromTextError $
          "Failure parsing StatusUpdateInterval from value: '" <> e
            <> "'. Accepted values: seconds_10, seconds_12, seconds_120, seconds_15, seconds_180, seconds_20, seconds_240, seconds_30, seconds_300, seconds_360, seconds_420, seconds_480, seconds_540, seconds_60, seconds_600"

instance ToText StatusUpdateInterval where
  toText = \case
    Seconds10 -> "SECONDS_10"
    Seconds12 -> "SECONDS_12"
    Seconds120 -> "SECONDS_120"
    Seconds15 -> "SECONDS_15"
    Seconds180 -> "SECONDS_180"
    Seconds20 -> "SECONDS_20"
    Seconds240 -> "SECONDS_240"
    Seconds30 -> "SECONDS_30"
    Seconds300 -> "SECONDS_300"
    Seconds360 -> "SECONDS_360"
    Seconds420 -> "SECONDS_420"
    Seconds480 -> "SECONDS_480"
    Seconds540 -> "SECONDS_540"
    Seconds60 -> "SECONDS_60"
    Seconds600 -> "SECONDS_600"

instance Hashable StatusUpdateInterval

instance NFData StatusUpdateInterval

instance ToByteString StatusUpdateInterval

instance ToQuery StatusUpdateInterval

instance ToHeader StatusUpdateInterval

instance ToJSON StatusUpdateInterval where
  toJSON = toJSONText

instance FromJSON StatusUpdateInterval where
  parseJSON = parseJSONText "StatusUpdateInterval"

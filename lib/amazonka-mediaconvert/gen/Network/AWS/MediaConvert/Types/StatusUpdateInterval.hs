{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StatusUpdateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.StatusUpdateInterval
  ( StatusUpdateInterval
    ( StatusUpdateInterval'
    , StatusUpdateIntervalSeconds10
    , StatusUpdateIntervalSeconds12
    , StatusUpdateIntervalSeconds15
    , StatusUpdateIntervalSeconds20
    , StatusUpdateIntervalSeconds30
    , StatusUpdateIntervalSeconds60
    , StatusUpdateIntervalSeconds120
    , StatusUpdateIntervalSeconds180
    , StatusUpdateIntervalSeconds240
    , StatusUpdateIntervalSeconds300
    , StatusUpdateIntervalSeconds360
    , StatusUpdateIntervalSeconds420
    , StatusUpdateIntervalSeconds480
    , StatusUpdateIntervalSeconds540
    , StatusUpdateIntervalSeconds600
    , fromStatusUpdateInterval
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
newtype StatusUpdateInterval = StatusUpdateInterval'{fromStatusUpdateInterval
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern StatusUpdateIntervalSeconds10 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds10 = StatusUpdateInterval' "SECONDS_10"

pattern StatusUpdateIntervalSeconds12 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds12 = StatusUpdateInterval' "SECONDS_12"

pattern StatusUpdateIntervalSeconds15 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds15 = StatusUpdateInterval' "SECONDS_15"

pattern StatusUpdateIntervalSeconds20 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds20 = StatusUpdateInterval' "SECONDS_20"

pattern StatusUpdateIntervalSeconds30 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds30 = StatusUpdateInterval' "SECONDS_30"

pattern StatusUpdateIntervalSeconds60 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds60 = StatusUpdateInterval' "SECONDS_60"

pattern StatusUpdateIntervalSeconds120 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds120 = StatusUpdateInterval' "SECONDS_120"

pattern StatusUpdateIntervalSeconds180 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds180 = StatusUpdateInterval' "SECONDS_180"

pattern StatusUpdateIntervalSeconds240 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds240 = StatusUpdateInterval' "SECONDS_240"

pattern StatusUpdateIntervalSeconds300 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds300 = StatusUpdateInterval' "SECONDS_300"

pattern StatusUpdateIntervalSeconds360 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds360 = StatusUpdateInterval' "SECONDS_360"

pattern StatusUpdateIntervalSeconds420 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds420 = StatusUpdateInterval' "SECONDS_420"

pattern StatusUpdateIntervalSeconds480 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds480 = StatusUpdateInterval' "SECONDS_480"

pattern StatusUpdateIntervalSeconds540 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds540 = StatusUpdateInterval' "SECONDS_540"

pattern StatusUpdateIntervalSeconds600 :: StatusUpdateInterval
pattern StatusUpdateIntervalSeconds600 = StatusUpdateInterval' "SECONDS_600"

{-# COMPLETE 
  StatusUpdateIntervalSeconds10,

  StatusUpdateIntervalSeconds12,

  StatusUpdateIntervalSeconds15,

  StatusUpdateIntervalSeconds20,

  StatusUpdateIntervalSeconds30,

  StatusUpdateIntervalSeconds60,

  StatusUpdateIntervalSeconds120,

  StatusUpdateIntervalSeconds180,

  StatusUpdateIntervalSeconds240,

  StatusUpdateIntervalSeconds300,

  StatusUpdateIntervalSeconds360,

  StatusUpdateIntervalSeconds420,

  StatusUpdateIntervalSeconds480,

  StatusUpdateIntervalSeconds540,

  StatusUpdateIntervalSeconds600,
  StatusUpdateInterval'
  #-}

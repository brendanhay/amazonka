{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Frequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Frequency
  ( Frequency
    ( Frequency'
    , FrequencyOnce
    , FrequencyHourly
    , FrequencyDaily
    , FrequencyWeekly
    , FrequencyMonthly
    , FrequencyEvent
    , fromFrequency
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Frequency = Frequency'{fromFrequency :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern FrequencyOnce :: Frequency
pattern FrequencyOnce = Frequency' "ONCE"

pattern FrequencyHourly :: Frequency
pattern FrequencyHourly = Frequency' "HOURLY"

pattern FrequencyDaily :: Frequency
pattern FrequencyDaily = Frequency' "DAILY"

pattern FrequencyWeekly :: Frequency
pattern FrequencyWeekly = Frequency' "WEEKLY"

pattern FrequencyMonthly :: Frequency
pattern FrequencyMonthly = Frequency' "MONTHLY"

pattern FrequencyEvent :: Frequency
pattern FrequencyEvent = Frequency' "EVENT"

{-# COMPLETE 
  FrequencyOnce,

  FrequencyHourly,

  FrequencyDaily,

  FrequencyWeekly,

  FrequencyMonthly,

  FrequencyEvent,
  Frequency'
  #-}

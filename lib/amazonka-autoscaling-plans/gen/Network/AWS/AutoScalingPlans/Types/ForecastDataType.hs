{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ForecastDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ForecastDataType
  ( ForecastDataType
    ( ForecastDataType'
    , ForecastDataTypeCapacityForecast
    , ForecastDataTypeLoadForecast
    , ForecastDataTypeScheduledActionMinCapacity
    , ForecastDataTypeScheduledActionMaxCapacity
    , fromForecastDataType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ForecastDataType = ForecastDataType'{fromForecastDataType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern ForecastDataTypeCapacityForecast :: ForecastDataType
pattern ForecastDataTypeCapacityForecast = ForecastDataType' "CapacityForecast"

pattern ForecastDataTypeLoadForecast :: ForecastDataType
pattern ForecastDataTypeLoadForecast = ForecastDataType' "LoadForecast"

pattern ForecastDataTypeScheduledActionMinCapacity :: ForecastDataType
pattern ForecastDataTypeScheduledActionMinCapacity = ForecastDataType' "ScheduledActionMinCapacity"

pattern ForecastDataTypeScheduledActionMaxCapacity :: ForecastDataType
pattern ForecastDataTypeScheduledActionMaxCapacity = ForecastDataType' "ScheduledActionMaxCapacity"

{-# COMPLETE 
  ForecastDataTypeCapacityForecast,

  ForecastDataTypeLoadForecast,

  ForecastDataTypeScheduledActionMinCapacity,

  ForecastDataTypeScheduledActionMaxCapacity,
  ForecastDataType'
  #-}

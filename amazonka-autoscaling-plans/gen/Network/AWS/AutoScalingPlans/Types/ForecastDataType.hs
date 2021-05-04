{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ForecastDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ForecastDataType
  ( ForecastDataType
      ( ..,
        ForecastDataType_CapacityForecast,
        ForecastDataType_LoadForecast,
        ForecastDataType_ScheduledActionMaxCapacity,
        ForecastDataType_ScheduledActionMinCapacity
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ForecastDataType = ForecastDataType'
  { fromForecastDataType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ForecastDataType_CapacityForecast :: ForecastDataType
pattern ForecastDataType_CapacityForecast = ForecastDataType' "CapacityForecast"

pattern ForecastDataType_LoadForecast :: ForecastDataType
pattern ForecastDataType_LoadForecast = ForecastDataType' "LoadForecast"

pattern ForecastDataType_ScheduledActionMaxCapacity :: ForecastDataType
pattern ForecastDataType_ScheduledActionMaxCapacity = ForecastDataType' "ScheduledActionMaxCapacity"

pattern ForecastDataType_ScheduledActionMinCapacity :: ForecastDataType
pattern ForecastDataType_ScheduledActionMinCapacity = ForecastDataType' "ScheduledActionMinCapacity"

{-# COMPLETE
  ForecastDataType_CapacityForecast,
  ForecastDataType_LoadForecast,
  ForecastDataType_ScheduledActionMaxCapacity,
  ForecastDataType_ScheduledActionMinCapacity,
  ForecastDataType'
  #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScalingPlans.Types.ForecastDataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ForecastDataType
  ( ForecastDataType
      ( ..,
        ForecastDataType_CapacityForecast,
        ForecastDataType_LoadForecast,
        ForecastDataType_ScheduledActionMaxCapacity,
        ForecastDataType_ScheduledActionMinCapacity
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ForecastDataType = ForecastDataType'
  { fromForecastDataType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

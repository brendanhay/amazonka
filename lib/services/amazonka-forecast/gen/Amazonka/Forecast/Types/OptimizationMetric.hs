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
-- Module      : Amazonka.Forecast.Types.OptimizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.OptimizationMetric
  ( OptimizationMetric
      ( ..,
        OptimizationMetric_AverageWeightedQuantileLoss,
        OptimizationMetric_MAPE,
        OptimizationMetric_MASE,
        OptimizationMetric_RMSE,
        OptimizationMetric_WAPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OptimizationMetric = OptimizationMetric'
  { fromOptimizationMetric ::
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

pattern OptimizationMetric_AverageWeightedQuantileLoss :: OptimizationMetric
pattern OptimizationMetric_AverageWeightedQuantileLoss = OptimizationMetric' "AverageWeightedQuantileLoss"

pattern OptimizationMetric_MAPE :: OptimizationMetric
pattern OptimizationMetric_MAPE = OptimizationMetric' "MAPE"

pattern OptimizationMetric_MASE :: OptimizationMetric
pattern OptimizationMetric_MASE = OptimizationMetric' "MASE"

pattern OptimizationMetric_RMSE :: OptimizationMetric
pattern OptimizationMetric_RMSE = OptimizationMetric' "RMSE"

pattern OptimizationMetric_WAPE :: OptimizationMetric
pattern OptimizationMetric_WAPE = OptimizationMetric' "WAPE"

{-# COMPLETE
  OptimizationMetric_AverageWeightedQuantileLoss,
  OptimizationMetric_MAPE,
  OptimizationMetric_MASE,
  OptimizationMetric_RMSE,
  OptimizationMetric_WAPE,
  OptimizationMetric'
  #-}

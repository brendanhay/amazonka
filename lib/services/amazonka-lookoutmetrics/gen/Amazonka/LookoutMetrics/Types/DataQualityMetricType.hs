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
-- Module      : Amazonka.LookoutMetrics.Types.DataQualityMetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DataQualityMetricType
  ( DataQualityMetricType
      ( ..,
        DataQualityMetricType_BACKTEST_INFERENCE_DATA_END_TIME_STAMP,
        DataQualityMetricType_BACKTEST_INFERENCE_DATA_START_TIME_STAMP,
        DataQualityMetricType_BACKTEST_TRAINING_DATA_END_TIME_STAMP,
        DataQualityMetricType_BACKTEST_TRAINING_DATA_START_TIME_STAMP,
        DataQualityMetricType_COLUMN_COMPLETENESS,
        DataQualityMetricType_DIMENSION_UNIQUENESS,
        DataQualityMetricType_INVALID_ROWS_COMPLIANCE,
        DataQualityMetricType_ROWS_PARTIAL_COMPLIANCE,
        DataQualityMetricType_ROWS_PROCESSED,
        DataQualityMetricType_TIME_SERIES_COUNT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataQualityMetricType = DataQualityMetricType'
  { fromDataQualityMetricType ::
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

pattern DataQualityMetricType_BACKTEST_INFERENCE_DATA_END_TIME_STAMP :: DataQualityMetricType
pattern DataQualityMetricType_BACKTEST_INFERENCE_DATA_END_TIME_STAMP = DataQualityMetricType' "BACKTEST_INFERENCE_DATA_END_TIME_STAMP"

pattern DataQualityMetricType_BACKTEST_INFERENCE_DATA_START_TIME_STAMP :: DataQualityMetricType
pattern DataQualityMetricType_BACKTEST_INFERENCE_DATA_START_TIME_STAMP = DataQualityMetricType' "BACKTEST_INFERENCE_DATA_START_TIME_STAMP"

pattern DataQualityMetricType_BACKTEST_TRAINING_DATA_END_TIME_STAMP :: DataQualityMetricType
pattern DataQualityMetricType_BACKTEST_TRAINING_DATA_END_TIME_STAMP = DataQualityMetricType' "BACKTEST_TRAINING_DATA_END_TIME_STAMP"

pattern DataQualityMetricType_BACKTEST_TRAINING_DATA_START_TIME_STAMP :: DataQualityMetricType
pattern DataQualityMetricType_BACKTEST_TRAINING_DATA_START_TIME_STAMP = DataQualityMetricType' "BACKTEST_TRAINING_DATA_START_TIME_STAMP"

pattern DataQualityMetricType_COLUMN_COMPLETENESS :: DataQualityMetricType
pattern DataQualityMetricType_COLUMN_COMPLETENESS = DataQualityMetricType' "COLUMN_COMPLETENESS"

pattern DataQualityMetricType_DIMENSION_UNIQUENESS :: DataQualityMetricType
pattern DataQualityMetricType_DIMENSION_UNIQUENESS = DataQualityMetricType' "DIMENSION_UNIQUENESS"

pattern DataQualityMetricType_INVALID_ROWS_COMPLIANCE :: DataQualityMetricType
pattern DataQualityMetricType_INVALID_ROWS_COMPLIANCE = DataQualityMetricType' "INVALID_ROWS_COMPLIANCE"

pattern DataQualityMetricType_ROWS_PARTIAL_COMPLIANCE :: DataQualityMetricType
pattern DataQualityMetricType_ROWS_PARTIAL_COMPLIANCE = DataQualityMetricType' "ROWS_PARTIAL_COMPLIANCE"

pattern DataQualityMetricType_ROWS_PROCESSED :: DataQualityMetricType
pattern DataQualityMetricType_ROWS_PROCESSED = DataQualityMetricType' "ROWS_PROCESSED"

pattern DataQualityMetricType_TIME_SERIES_COUNT :: DataQualityMetricType
pattern DataQualityMetricType_TIME_SERIES_COUNT = DataQualityMetricType' "TIME_SERIES_COUNT"

{-# COMPLETE
  DataQualityMetricType_BACKTEST_INFERENCE_DATA_END_TIME_STAMP,
  DataQualityMetricType_BACKTEST_INFERENCE_DATA_START_TIME_STAMP,
  DataQualityMetricType_BACKTEST_TRAINING_DATA_END_TIME_STAMP,
  DataQualityMetricType_BACKTEST_TRAINING_DATA_START_TIME_STAMP,
  DataQualityMetricType_COLUMN_COMPLETENESS,
  DataQualityMetricType_DIMENSION_UNIQUENESS,
  DataQualityMetricType_INVALID_ROWS_COMPLIANCE,
  DataQualityMetricType_ROWS_PARTIAL_COMPLIANCE,
  DataQualityMetricType_ROWS_PROCESSED,
  DataQualityMetricType_TIME_SERIES_COUNT,
  DataQualityMetricType'
  #-}

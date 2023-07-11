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
-- Module      : Amazonka.Kendra.Types.MetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.MetricType
  ( MetricType
      ( ..,
        MetricType_AGG_QUERY_DOC_METRICS,
        MetricType_DOCS_BY_CLICK_COUNT,
        MetricType_QUERIES_BY_COUNT,
        MetricType_QUERIES_BY_ZERO_CLICK_RATE,
        MetricType_QUERIES_BY_ZERO_RESULT_RATE,
        MetricType_TREND_QUERY_DOC_METRICS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricType = MetricType'
  { fromMetricType ::
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

pattern MetricType_AGG_QUERY_DOC_METRICS :: MetricType
pattern MetricType_AGG_QUERY_DOC_METRICS = MetricType' "AGG_QUERY_DOC_METRICS"

pattern MetricType_DOCS_BY_CLICK_COUNT :: MetricType
pattern MetricType_DOCS_BY_CLICK_COUNT = MetricType' "DOCS_BY_CLICK_COUNT"

pattern MetricType_QUERIES_BY_COUNT :: MetricType
pattern MetricType_QUERIES_BY_COUNT = MetricType' "QUERIES_BY_COUNT"

pattern MetricType_QUERIES_BY_ZERO_CLICK_RATE :: MetricType
pattern MetricType_QUERIES_BY_ZERO_CLICK_RATE = MetricType' "QUERIES_BY_ZERO_CLICK_RATE"

pattern MetricType_QUERIES_BY_ZERO_RESULT_RATE :: MetricType
pattern MetricType_QUERIES_BY_ZERO_RESULT_RATE = MetricType' "QUERIES_BY_ZERO_RESULT_RATE"

pattern MetricType_TREND_QUERY_DOC_METRICS :: MetricType
pattern MetricType_TREND_QUERY_DOC_METRICS = MetricType' "TREND_QUERY_DOC_METRICS"

{-# COMPLETE
  MetricType_AGG_QUERY_DOC_METRICS,
  MetricType_DOCS_BY_CLICK_COUNT,
  MetricType_QUERIES_BY_COUNT,
  MetricType_QUERIES_BY_ZERO_CLICK_RATE,
  MetricType_QUERIES_BY_ZERO_RESULT_RATE,
  MetricType_TREND_QUERY_DOC_METRICS,
  MetricType'
  #-}

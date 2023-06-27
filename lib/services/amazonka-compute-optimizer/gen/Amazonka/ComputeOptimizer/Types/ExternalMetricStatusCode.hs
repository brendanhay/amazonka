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
-- Module      : Amazonka.ComputeOptimizer.Types.ExternalMetricStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExternalMetricStatusCode
  ( ExternalMetricStatusCode
      ( ..,
        ExternalMetricStatusCode_DATADOG_INTEGRATION_ERROR,
        ExternalMetricStatusCode_DYNATRACE_INTEGRATION_ERROR,
        ExternalMetricStatusCode_INSTANA_INTEGRATION_ERROR,
        ExternalMetricStatusCode_INSUFFICIENT_DATADOG_METRICS,
        ExternalMetricStatusCode_INSUFFICIENT_DYNATRACE_METRICS,
        ExternalMetricStatusCode_INSUFFICIENT_INSTANA_METRICS,
        ExternalMetricStatusCode_INSUFFICIENT_NEWRELIC_METRICS,
        ExternalMetricStatusCode_INTEGRATION_SUCCESS,
        ExternalMetricStatusCode_NEWRELIC_INTEGRATION_ERROR,
        ExternalMetricStatusCode_NO_EXTERNAL_METRIC_SET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExternalMetricStatusCode = ExternalMetricStatusCode'
  { fromExternalMetricStatusCode ::
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

pattern ExternalMetricStatusCode_DATADOG_INTEGRATION_ERROR :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_DATADOG_INTEGRATION_ERROR = ExternalMetricStatusCode' "DATADOG_INTEGRATION_ERROR"

pattern ExternalMetricStatusCode_DYNATRACE_INTEGRATION_ERROR :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_DYNATRACE_INTEGRATION_ERROR = ExternalMetricStatusCode' "DYNATRACE_INTEGRATION_ERROR"

pattern ExternalMetricStatusCode_INSTANA_INTEGRATION_ERROR :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INSTANA_INTEGRATION_ERROR = ExternalMetricStatusCode' "INSTANA_INTEGRATION_ERROR"

pattern ExternalMetricStatusCode_INSUFFICIENT_DATADOG_METRICS :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INSUFFICIENT_DATADOG_METRICS = ExternalMetricStatusCode' "INSUFFICIENT_DATADOG_METRICS"

pattern ExternalMetricStatusCode_INSUFFICIENT_DYNATRACE_METRICS :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INSUFFICIENT_DYNATRACE_METRICS = ExternalMetricStatusCode' "INSUFFICIENT_DYNATRACE_METRICS"

pattern ExternalMetricStatusCode_INSUFFICIENT_INSTANA_METRICS :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INSUFFICIENT_INSTANA_METRICS = ExternalMetricStatusCode' "INSUFFICIENT_INSTANA_METRICS"

pattern ExternalMetricStatusCode_INSUFFICIENT_NEWRELIC_METRICS :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INSUFFICIENT_NEWRELIC_METRICS = ExternalMetricStatusCode' "INSUFFICIENT_NEWRELIC_METRICS"

pattern ExternalMetricStatusCode_INTEGRATION_SUCCESS :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_INTEGRATION_SUCCESS = ExternalMetricStatusCode' "INTEGRATION_SUCCESS"

pattern ExternalMetricStatusCode_NEWRELIC_INTEGRATION_ERROR :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_NEWRELIC_INTEGRATION_ERROR = ExternalMetricStatusCode' "NEWRELIC_INTEGRATION_ERROR"

pattern ExternalMetricStatusCode_NO_EXTERNAL_METRIC_SET :: ExternalMetricStatusCode
pattern ExternalMetricStatusCode_NO_EXTERNAL_METRIC_SET = ExternalMetricStatusCode' "NO_EXTERNAL_METRIC_SET"

{-# COMPLETE
  ExternalMetricStatusCode_DATADOG_INTEGRATION_ERROR,
  ExternalMetricStatusCode_DYNATRACE_INTEGRATION_ERROR,
  ExternalMetricStatusCode_INSTANA_INTEGRATION_ERROR,
  ExternalMetricStatusCode_INSUFFICIENT_DATADOG_METRICS,
  ExternalMetricStatusCode_INSUFFICIENT_DYNATRACE_METRICS,
  ExternalMetricStatusCode_INSUFFICIENT_INSTANA_METRICS,
  ExternalMetricStatusCode_INSUFFICIENT_NEWRELIC_METRICS,
  ExternalMetricStatusCode_INTEGRATION_SUCCESS,
  ExternalMetricStatusCode_NEWRELIC_INTEGRATION_ERROR,
  ExternalMetricStatusCode_NO_EXTERNAL_METRIC_SET,
  ExternalMetricStatusCode'
  #-}

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
-- Module      : Amazonka.ComputeOptimizer.Types.ExportableLambdaFunctionField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExportableLambdaFunctionField
  ( ExportableLambdaFunctionField
      ( ..,
        ExportableLambdaFunctionField_AccountId,
        ExportableLambdaFunctionField_CurrentConfigurationMemorySize,
        ExportableLambdaFunctionField_CurrentConfigurationTimeout,
        ExportableLambdaFunctionField_CurrentCostAverage,
        ExportableLambdaFunctionField_CurrentCostTotal,
        ExportableLambdaFunctionField_CurrentPerformanceRisk,
        ExportableLambdaFunctionField_Finding,
        ExportableLambdaFunctionField_FindingReasonCodes,
        ExportableLambdaFunctionField_FunctionArn,
        ExportableLambdaFunctionField_FunctionVersion,
        ExportableLambdaFunctionField_LastRefreshTimestamp,
        ExportableLambdaFunctionField_LookbackPeriodInDays,
        ExportableLambdaFunctionField_NumberOfInvocations,
        ExportableLambdaFunctionField_RecommendationOptionsConfigurationMemorySize,
        ExportableLambdaFunctionField_RecommendationOptionsCostHigh,
        ExportableLambdaFunctionField_RecommendationOptionsCostLow,
        ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
        ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsValue,
        ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationExpected,
        ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationLowerBound,
        ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationUpperBound,
        ExportableLambdaFunctionField_RecommendationOptionsSavingsOpportunityPercentage,
        ExportableLambdaFunctionField_Tags,
        ExportableLambdaFunctionField_UtilizationMetricsDurationAverage,
        ExportableLambdaFunctionField_UtilizationMetricsDurationMaximum,
        ExportableLambdaFunctionField_UtilizationMetricsMemoryAverage,
        ExportableLambdaFunctionField_UtilizationMetricsMemoryMaximum
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportableLambdaFunctionField = ExportableLambdaFunctionField'
  { fromExportableLambdaFunctionField ::
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

pattern ExportableLambdaFunctionField_AccountId :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_AccountId = ExportableLambdaFunctionField' "AccountId"

pattern ExportableLambdaFunctionField_CurrentConfigurationMemorySize :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_CurrentConfigurationMemorySize = ExportableLambdaFunctionField' "CurrentConfigurationMemorySize"

pattern ExportableLambdaFunctionField_CurrentConfigurationTimeout :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_CurrentConfigurationTimeout = ExportableLambdaFunctionField' "CurrentConfigurationTimeout"

pattern ExportableLambdaFunctionField_CurrentCostAverage :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_CurrentCostAverage = ExportableLambdaFunctionField' "CurrentCostAverage"

pattern ExportableLambdaFunctionField_CurrentCostTotal :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_CurrentCostTotal = ExportableLambdaFunctionField' "CurrentCostTotal"

pattern ExportableLambdaFunctionField_CurrentPerformanceRisk :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_CurrentPerformanceRisk = ExportableLambdaFunctionField' "CurrentPerformanceRisk"

pattern ExportableLambdaFunctionField_Finding :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_Finding = ExportableLambdaFunctionField' "Finding"

pattern ExportableLambdaFunctionField_FindingReasonCodes :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_FindingReasonCodes = ExportableLambdaFunctionField' "FindingReasonCodes"

pattern ExportableLambdaFunctionField_FunctionArn :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_FunctionArn = ExportableLambdaFunctionField' "FunctionArn"

pattern ExportableLambdaFunctionField_FunctionVersion :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_FunctionVersion = ExportableLambdaFunctionField' "FunctionVersion"

pattern ExportableLambdaFunctionField_LastRefreshTimestamp :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_LastRefreshTimestamp = ExportableLambdaFunctionField' "LastRefreshTimestamp"

pattern ExportableLambdaFunctionField_LookbackPeriodInDays :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_LookbackPeriodInDays = ExportableLambdaFunctionField' "LookbackPeriodInDays"

pattern ExportableLambdaFunctionField_NumberOfInvocations :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_NumberOfInvocations = ExportableLambdaFunctionField' "NumberOfInvocations"

pattern ExportableLambdaFunctionField_RecommendationOptionsConfigurationMemorySize :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsConfigurationMemorySize = ExportableLambdaFunctionField' "RecommendationOptionsConfigurationMemorySize"

pattern ExportableLambdaFunctionField_RecommendationOptionsCostHigh :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsCostHigh = ExportableLambdaFunctionField' "RecommendationOptionsCostHigh"

pattern ExportableLambdaFunctionField_RecommendationOptionsCostLow :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsCostLow = ExportableLambdaFunctionField' "RecommendationOptionsCostLow"

pattern ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsCurrency :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsCurrency = ExportableLambdaFunctionField' "RecommendationOptionsEstimatedMonthlySavingsCurrency"

pattern ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsValue :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsValue = ExportableLambdaFunctionField' "RecommendationOptionsEstimatedMonthlySavingsValue"

pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationExpected :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationExpected = ExportableLambdaFunctionField' "RecommendationOptionsProjectedUtilizationMetricsDurationExpected"

pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationLowerBound :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationLowerBound = ExportableLambdaFunctionField' "RecommendationOptionsProjectedUtilizationMetricsDurationLowerBound"

pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationUpperBound :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationUpperBound = ExportableLambdaFunctionField' "RecommendationOptionsProjectedUtilizationMetricsDurationUpperBound"

pattern ExportableLambdaFunctionField_RecommendationOptionsSavingsOpportunityPercentage :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_RecommendationOptionsSavingsOpportunityPercentage = ExportableLambdaFunctionField' "RecommendationOptionsSavingsOpportunityPercentage"

pattern ExportableLambdaFunctionField_Tags :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_Tags = ExportableLambdaFunctionField' "Tags"

pattern ExportableLambdaFunctionField_UtilizationMetricsDurationAverage :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_UtilizationMetricsDurationAverage = ExportableLambdaFunctionField' "UtilizationMetricsDurationAverage"

pattern ExportableLambdaFunctionField_UtilizationMetricsDurationMaximum :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_UtilizationMetricsDurationMaximum = ExportableLambdaFunctionField' "UtilizationMetricsDurationMaximum"

pattern ExportableLambdaFunctionField_UtilizationMetricsMemoryAverage :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_UtilizationMetricsMemoryAverage = ExportableLambdaFunctionField' "UtilizationMetricsMemoryAverage"

pattern ExportableLambdaFunctionField_UtilizationMetricsMemoryMaximum :: ExportableLambdaFunctionField
pattern ExportableLambdaFunctionField_UtilizationMetricsMemoryMaximum = ExportableLambdaFunctionField' "UtilizationMetricsMemoryMaximum"

{-# COMPLETE
  ExportableLambdaFunctionField_AccountId,
  ExportableLambdaFunctionField_CurrentConfigurationMemorySize,
  ExportableLambdaFunctionField_CurrentConfigurationTimeout,
  ExportableLambdaFunctionField_CurrentCostAverage,
  ExportableLambdaFunctionField_CurrentCostTotal,
  ExportableLambdaFunctionField_CurrentPerformanceRisk,
  ExportableLambdaFunctionField_Finding,
  ExportableLambdaFunctionField_FindingReasonCodes,
  ExportableLambdaFunctionField_FunctionArn,
  ExportableLambdaFunctionField_FunctionVersion,
  ExportableLambdaFunctionField_LastRefreshTimestamp,
  ExportableLambdaFunctionField_LookbackPeriodInDays,
  ExportableLambdaFunctionField_NumberOfInvocations,
  ExportableLambdaFunctionField_RecommendationOptionsConfigurationMemorySize,
  ExportableLambdaFunctionField_RecommendationOptionsCostHigh,
  ExportableLambdaFunctionField_RecommendationOptionsCostLow,
  ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
  ExportableLambdaFunctionField_RecommendationOptionsEstimatedMonthlySavingsValue,
  ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationExpected,
  ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationLowerBound,
  ExportableLambdaFunctionField_RecommendationOptionsProjectedUtilizationMetricsDurationUpperBound,
  ExportableLambdaFunctionField_RecommendationOptionsSavingsOpportunityPercentage,
  ExportableLambdaFunctionField_Tags,
  ExportableLambdaFunctionField_UtilizationMetricsDurationAverage,
  ExportableLambdaFunctionField_UtilizationMetricsDurationMaximum,
  ExportableLambdaFunctionField_UtilizationMetricsMemoryAverage,
  ExportableLambdaFunctionField_UtilizationMetricsMemoryMaximum,
  ExportableLambdaFunctionField'
  #-}

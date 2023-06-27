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
-- Module      : Amazonka.ComputeOptimizer.Types.ExportableECSServiceField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExportableECSServiceField
  ( ExportableECSServiceField
      ( ..,
        ExportableECSServiceField_AccountId,
        ExportableECSServiceField_CurrentPerformanceRisk,
        ExportableECSServiceField_CurrentServiceConfigurationAutoScalingConfiguration,
        ExportableECSServiceField_CurrentServiceConfigurationCpu,
        ExportableECSServiceField_CurrentServiceConfigurationMemory,
        ExportableECSServiceField_CurrentServiceConfigurationTaskDefinitionArn,
        ExportableECSServiceField_CurrentServiceContainerConfigurations,
        ExportableECSServiceField_Finding,
        ExportableECSServiceField_FindingReasonCodes,
        ExportableECSServiceField_LastRefreshTimestamp,
        ExportableECSServiceField_LaunchType,
        ExportableECSServiceField_LookbackPeriodInDays,
        ExportableECSServiceField_RecommendationOptionsContainerRecommendations,
        ExportableECSServiceField_RecommendationOptionsCpu,
        ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
        ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsValue,
        ExportableECSServiceField_RecommendationOptionsMemory,
        ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsCpuMaximum,
        ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsMemoryMaximum,
        ExportableECSServiceField_RecommendationOptionsSavingsOpportunityPercentage,
        ExportableECSServiceField_ServiceArn,
        ExportableECSServiceField_Tags,
        ExportableECSServiceField_UtilizationMetricsCpuMaximum,
        ExportableECSServiceField_UtilizationMetricsMemoryMaximum
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportableECSServiceField = ExportableECSServiceField'
  { fromExportableECSServiceField ::
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

pattern ExportableECSServiceField_AccountId :: ExportableECSServiceField
pattern ExportableECSServiceField_AccountId = ExportableECSServiceField' "AccountId"

pattern ExportableECSServiceField_CurrentPerformanceRisk :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentPerformanceRisk = ExportableECSServiceField' "CurrentPerformanceRisk"

pattern ExportableECSServiceField_CurrentServiceConfigurationAutoScalingConfiguration :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentServiceConfigurationAutoScalingConfiguration = ExportableECSServiceField' "CurrentServiceConfigurationAutoScalingConfiguration"

pattern ExportableECSServiceField_CurrentServiceConfigurationCpu :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentServiceConfigurationCpu = ExportableECSServiceField' "CurrentServiceConfigurationCpu"

pattern ExportableECSServiceField_CurrentServiceConfigurationMemory :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentServiceConfigurationMemory = ExportableECSServiceField' "CurrentServiceConfigurationMemory"

pattern ExportableECSServiceField_CurrentServiceConfigurationTaskDefinitionArn :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentServiceConfigurationTaskDefinitionArn = ExportableECSServiceField' "CurrentServiceConfigurationTaskDefinitionArn"

pattern ExportableECSServiceField_CurrentServiceContainerConfigurations :: ExportableECSServiceField
pattern ExportableECSServiceField_CurrentServiceContainerConfigurations = ExportableECSServiceField' "CurrentServiceContainerConfigurations"

pattern ExportableECSServiceField_Finding :: ExportableECSServiceField
pattern ExportableECSServiceField_Finding = ExportableECSServiceField' "Finding"

pattern ExportableECSServiceField_FindingReasonCodes :: ExportableECSServiceField
pattern ExportableECSServiceField_FindingReasonCodes = ExportableECSServiceField' "FindingReasonCodes"

pattern ExportableECSServiceField_LastRefreshTimestamp :: ExportableECSServiceField
pattern ExportableECSServiceField_LastRefreshTimestamp = ExportableECSServiceField' "LastRefreshTimestamp"

pattern ExportableECSServiceField_LaunchType :: ExportableECSServiceField
pattern ExportableECSServiceField_LaunchType = ExportableECSServiceField' "LaunchType"

pattern ExportableECSServiceField_LookbackPeriodInDays :: ExportableECSServiceField
pattern ExportableECSServiceField_LookbackPeriodInDays = ExportableECSServiceField' "LookbackPeriodInDays"

pattern ExportableECSServiceField_RecommendationOptionsContainerRecommendations :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsContainerRecommendations = ExportableECSServiceField' "RecommendationOptionsContainerRecommendations"

pattern ExportableECSServiceField_RecommendationOptionsCpu :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsCpu = ExportableECSServiceField' "RecommendationOptionsCpu"

pattern ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsCurrency :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsCurrency = ExportableECSServiceField' "RecommendationOptionsEstimatedMonthlySavingsCurrency"

pattern ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsValue :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsValue = ExportableECSServiceField' "RecommendationOptionsEstimatedMonthlySavingsValue"

pattern ExportableECSServiceField_RecommendationOptionsMemory :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsMemory = ExportableECSServiceField' "RecommendationOptionsMemory"

pattern ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsCpuMaximum :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsCpuMaximum = ExportableECSServiceField' "RecommendationOptionsProjectedUtilizationMetricsCpuMaximum"

pattern ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsMemoryMaximum :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsMemoryMaximum = ExportableECSServiceField' "RecommendationOptionsProjectedUtilizationMetricsMemoryMaximum"

pattern ExportableECSServiceField_RecommendationOptionsSavingsOpportunityPercentage :: ExportableECSServiceField
pattern ExportableECSServiceField_RecommendationOptionsSavingsOpportunityPercentage = ExportableECSServiceField' "RecommendationOptionsSavingsOpportunityPercentage"

pattern ExportableECSServiceField_ServiceArn :: ExportableECSServiceField
pattern ExportableECSServiceField_ServiceArn = ExportableECSServiceField' "ServiceArn"

pattern ExportableECSServiceField_Tags :: ExportableECSServiceField
pattern ExportableECSServiceField_Tags = ExportableECSServiceField' "Tags"

pattern ExportableECSServiceField_UtilizationMetricsCpuMaximum :: ExportableECSServiceField
pattern ExportableECSServiceField_UtilizationMetricsCpuMaximum = ExportableECSServiceField' "UtilizationMetricsCpuMaximum"

pattern ExportableECSServiceField_UtilizationMetricsMemoryMaximum :: ExportableECSServiceField
pattern ExportableECSServiceField_UtilizationMetricsMemoryMaximum = ExportableECSServiceField' "UtilizationMetricsMemoryMaximum"

{-# COMPLETE
  ExportableECSServiceField_AccountId,
  ExportableECSServiceField_CurrentPerformanceRisk,
  ExportableECSServiceField_CurrentServiceConfigurationAutoScalingConfiguration,
  ExportableECSServiceField_CurrentServiceConfigurationCpu,
  ExportableECSServiceField_CurrentServiceConfigurationMemory,
  ExportableECSServiceField_CurrentServiceConfigurationTaskDefinitionArn,
  ExportableECSServiceField_CurrentServiceContainerConfigurations,
  ExportableECSServiceField_Finding,
  ExportableECSServiceField_FindingReasonCodes,
  ExportableECSServiceField_LastRefreshTimestamp,
  ExportableECSServiceField_LaunchType,
  ExportableECSServiceField_LookbackPeriodInDays,
  ExportableECSServiceField_RecommendationOptionsContainerRecommendations,
  ExportableECSServiceField_RecommendationOptionsCpu,
  ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
  ExportableECSServiceField_RecommendationOptionsEstimatedMonthlySavingsValue,
  ExportableECSServiceField_RecommendationOptionsMemory,
  ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsCpuMaximum,
  ExportableECSServiceField_RecommendationOptionsProjectedUtilizationMetricsMemoryMaximum,
  ExportableECSServiceField_RecommendationOptionsSavingsOpportunityPercentage,
  ExportableECSServiceField_ServiceArn,
  ExportableECSServiceField_Tags,
  ExportableECSServiceField_UtilizationMetricsCpuMaximum,
  ExportableECSServiceField_UtilizationMetricsMemoryMaximum,
  ExportableECSServiceField'
  #-}

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
-- Module      : Amazonka.ComputeOptimizer.Types.ExportableVolumeField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExportableVolumeField
  ( ExportableVolumeField
      ( ..,
        ExportableVolumeField_AccountId,
        ExportableVolumeField_CurrentConfigurationVolumeBaselineIOPS,
        ExportableVolumeField_CurrentConfigurationVolumeBaselineThroughput,
        ExportableVolumeField_CurrentConfigurationVolumeBurstIOPS,
        ExportableVolumeField_CurrentConfigurationVolumeBurstThroughput,
        ExportableVolumeField_CurrentConfigurationVolumeSize,
        ExportableVolumeField_CurrentConfigurationVolumeType,
        ExportableVolumeField_CurrentMonthlyPrice,
        ExportableVolumeField_CurrentPerformanceRisk,
        ExportableVolumeField_Finding,
        ExportableVolumeField_LastRefreshTimestamp,
        ExportableVolumeField_LookbackPeriodInDays,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineIOPS,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineThroughput,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstIOPS,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstThroughput,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeSize,
        ExportableVolumeField_RecommendationOptionsConfigurationVolumeType,
        ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
        ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsValue,
        ExportableVolumeField_RecommendationOptionsMonthlyPrice,
        ExportableVolumeField_RecommendationOptionsPerformanceRisk,
        ExportableVolumeField_RecommendationOptionsSavingsOpportunityPercentage,
        ExportableVolumeField_RootVolume,
        ExportableVolumeField_Tags,
        ExportableVolumeField_UtilizationMetricsVolumeReadBytesPerSecondMaximum,
        ExportableVolumeField_UtilizationMetricsVolumeReadOpsPerSecondMaximum,
        ExportableVolumeField_UtilizationMetricsVolumeWriteBytesPerSecondMaximum,
        ExportableVolumeField_UtilizationMetricsVolumeWriteOpsPerSecondMaximum,
        ExportableVolumeField_VolumeArn
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportableVolumeField = ExportableVolumeField'
  { fromExportableVolumeField ::
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

pattern ExportableVolumeField_AccountId :: ExportableVolumeField
pattern ExportableVolumeField_AccountId = ExportableVolumeField' "AccountId"

pattern ExportableVolumeField_CurrentConfigurationVolumeBaselineIOPS :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeBaselineIOPS = ExportableVolumeField' "CurrentConfigurationVolumeBaselineIOPS"

pattern ExportableVolumeField_CurrentConfigurationVolumeBaselineThroughput :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeBaselineThroughput = ExportableVolumeField' "CurrentConfigurationVolumeBaselineThroughput"

pattern ExportableVolumeField_CurrentConfigurationVolumeBurstIOPS :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeBurstIOPS = ExportableVolumeField' "CurrentConfigurationVolumeBurstIOPS"

pattern ExportableVolumeField_CurrentConfigurationVolumeBurstThroughput :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeBurstThroughput = ExportableVolumeField' "CurrentConfigurationVolumeBurstThroughput"

pattern ExportableVolumeField_CurrentConfigurationVolumeSize :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeSize = ExportableVolumeField' "CurrentConfigurationVolumeSize"

pattern ExportableVolumeField_CurrentConfigurationVolumeType :: ExportableVolumeField
pattern ExportableVolumeField_CurrentConfigurationVolumeType = ExportableVolumeField' "CurrentConfigurationVolumeType"

pattern ExportableVolumeField_CurrentMonthlyPrice :: ExportableVolumeField
pattern ExportableVolumeField_CurrentMonthlyPrice = ExportableVolumeField' "CurrentMonthlyPrice"

pattern ExportableVolumeField_CurrentPerformanceRisk :: ExportableVolumeField
pattern ExportableVolumeField_CurrentPerformanceRisk = ExportableVolumeField' "CurrentPerformanceRisk"

pattern ExportableVolumeField_Finding :: ExportableVolumeField
pattern ExportableVolumeField_Finding = ExportableVolumeField' "Finding"

pattern ExportableVolumeField_LastRefreshTimestamp :: ExportableVolumeField
pattern ExportableVolumeField_LastRefreshTimestamp = ExportableVolumeField' "LastRefreshTimestamp"

pattern ExportableVolumeField_LookbackPeriodInDays :: ExportableVolumeField
pattern ExportableVolumeField_LookbackPeriodInDays = ExportableVolumeField' "LookbackPeriodInDays"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineIOPS :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineIOPS = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeBaselineIOPS"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineThroughput :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineThroughput = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeBaselineThroughput"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstIOPS :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstIOPS = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeBurstIOPS"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstThroughput :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstThroughput = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeBurstThroughput"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeSize :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeSize = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeSize"

pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeType :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsConfigurationVolumeType = ExportableVolumeField' "RecommendationOptionsConfigurationVolumeType"

pattern ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsCurrency :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsCurrency = ExportableVolumeField' "RecommendationOptionsEstimatedMonthlySavingsCurrency"

pattern ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsValue :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsValue = ExportableVolumeField' "RecommendationOptionsEstimatedMonthlySavingsValue"

pattern ExportableVolumeField_RecommendationOptionsMonthlyPrice :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsMonthlyPrice = ExportableVolumeField' "RecommendationOptionsMonthlyPrice"

pattern ExportableVolumeField_RecommendationOptionsPerformanceRisk :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsPerformanceRisk = ExportableVolumeField' "RecommendationOptionsPerformanceRisk"

pattern ExportableVolumeField_RecommendationOptionsSavingsOpportunityPercentage :: ExportableVolumeField
pattern ExportableVolumeField_RecommendationOptionsSavingsOpportunityPercentage = ExportableVolumeField' "RecommendationOptionsSavingsOpportunityPercentage"

pattern ExportableVolumeField_RootVolume :: ExportableVolumeField
pattern ExportableVolumeField_RootVolume = ExportableVolumeField' "RootVolume"

pattern ExportableVolumeField_Tags :: ExportableVolumeField
pattern ExportableVolumeField_Tags = ExportableVolumeField' "Tags"

pattern ExportableVolumeField_UtilizationMetricsVolumeReadBytesPerSecondMaximum :: ExportableVolumeField
pattern ExportableVolumeField_UtilizationMetricsVolumeReadBytesPerSecondMaximum = ExportableVolumeField' "UtilizationMetricsVolumeReadBytesPerSecondMaximum"

pattern ExportableVolumeField_UtilizationMetricsVolumeReadOpsPerSecondMaximum :: ExportableVolumeField
pattern ExportableVolumeField_UtilizationMetricsVolumeReadOpsPerSecondMaximum = ExportableVolumeField' "UtilizationMetricsVolumeReadOpsPerSecondMaximum"

pattern ExportableVolumeField_UtilizationMetricsVolumeWriteBytesPerSecondMaximum :: ExportableVolumeField
pattern ExportableVolumeField_UtilizationMetricsVolumeWriteBytesPerSecondMaximum = ExportableVolumeField' "UtilizationMetricsVolumeWriteBytesPerSecondMaximum"

pattern ExportableVolumeField_UtilizationMetricsVolumeWriteOpsPerSecondMaximum :: ExportableVolumeField
pattern ExportableVolumeField_UtilizationMetricsVolumeWriteOpsPerSecondMaximum = ExportableVolumeField' "UtilizationMetricsVolumeWriteOpsPerSecondMaximum"

pattern ExportableVolumeField_VolumeArn :: ExportableVolumeField
pattern ExportableVolumeField_VolumeArn = ExportableVolumeField' "VolumeArn"

{-# COMPLETE
  ExportableVolumeField_AccountId,
  ExportableVolumeField_CurrentConfigurationVolumeBaselineIOPS,
  ExportableVolumeField_CurrentConfigurationVolumeBaselineThroughput,
  ExportableVolumeField_CurrentConfigurationVolumeBurstIOPS,
  ExportableVolumeField_CurrentConfigurationVolumeBurstThroughput,
  ExportableVolumeField_CurrentConfigurationVolumeSize,
  ExportableVolumeField_CurrentConfigurationVolumeType,
  ExportableVolumeField_CurrentMonthlyPrice,
  ExportableVolumeField_CurrentPerformanceRisk,
  ExportableVolumeField_Finding,
  ExportableVolumeField_LastRefreshTimestamp,
  ExportableVolumeField_LookbackPeriodInDays,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineIOPS,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeBaselineThroughput,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstIOPS,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeBurstThroughput,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeSize,
  ExportableVolumeField_RecommendationOptionsConfigurationVolumeType,
  ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsCurrency,
  ExportableVolumeField_RecommendationOptionsEstimatedMonthlySavingsValue,
  ExportableVolumeField_RecommendationOptionsMonthlyPrice,
  ExportableVolumeField_RecommendationOptionsPerformanceRisk,
  ExportableVolumeField_RecommendationOptionsSavingsOpportunityPercentage,
  ExportableVolumeField_RootVolume,
  ExportableVolumeField_Tags,
  ExportableVolumeField_UtilizationMetricsVolumeReadBytesPerSecondMaximum,
  ExportableVolumeField_UtilizationMetricsVolumeReadOpsPerSecondMaximum,
  ExportableVolumeField_UtilizationMetricsVolumeWriteBytesPerSecondMaximum,
  ExportableVolumeField_UtilizationMetricsVolumeWriteOpsPerSecondMaximum,
  ExportableVolumeField_VolumeArn,
  ExportableVolumeField'
  #-}

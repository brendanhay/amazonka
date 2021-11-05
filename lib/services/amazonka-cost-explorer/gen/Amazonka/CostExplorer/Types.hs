{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnknownSubscriptionException,
    _BillExpirationException,
    _RequestChangedException,
    _UnresolvableUsageUnitException,
    _ServiceQuotaExceededException,
    _InvalidNextTokenException,
    _DataUnavailableException,
    _UnknownMonitorException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * AccountScope
    AccountScope (..),

    -- * AnomalyFeedbackType
    AnomalyFeedbackType (..),

    -- * AnomalySubscriptionFrequency
    AnomalySubscriptionFrequency (..),

    -- * Context
    Context (..),

    -- * CostCategoryInheritedValueDimensionName
    CostCategoryInheritedValueDimensionName (..),

    -- * CostCategoryRuleType
    CostCategoryRuleType (..),

    -- * CostCategoryRuleVersion
    CostCategoryRuleVersion (..),

    -- * CostCategorySplitChargeMethod
    CostCategorySplitChargeMethod (..),

    -- * CostCategorySplitChargeRuleParameterType
    CostCategorySplitChargeRuleParameterType (..),

    -- * CostCategoryStatus
    CostCategoryStatus (..),

    -- * CostCategoryStatusComponent
    CostCategoryStatusComponent (..),

    -- * Dimension
    Dimension (..),

    -- * FindingReasonCode
    FindingReasonCode (..),

    -- * Granularity
    Granularity (..),

    -- * GroupDefinitionType
    GroupDefinitionType (..),

    -- * LookbackPeriodInDays
    LookbackPeriodInDays (..),

    -- * MatchOption
    MatchOption (..),

    -- * Metric
    Metric (..),

    -- * MonitorDimension
    MonitorDimension (..),

    -- * MonitorType
    MonitorType (..),

    -- * NumericOperator
    NumericOperator (..),

    -- * OfferingClass
    OfferingClass (..),

    -- * PaymentOption
    PaymentOption (..),

    -- * PlatformDifference
    PlatformDifference (..),

    -- * RecommendationTarget
    RecommendationTarget (..),

    -- * RightsizingType
    RightsizingType (..),

    -- * SavingsPlansDataType
    SavingsPlansDataType (..),

    -- * SortOrder
    SortOrder (..),

    -- * SubscriberStatus
    SubscriberStatus (..),

    -- * SubscriberType
    SubscriberType (..),

    -- * SupportedSavingsPlansType
    SupportedSavingsPlansType (..),

    -- * TermInYears
    TermInYears (..),

    -- * Anomaly
    Anomaly (..),
    newAnomaly,
    anomaly_anomalyStartDate,
    anomaly_dimensionValue,
    anomaly_rootCauses,
    anomaly_anomalyEndDate,
    anomaly_feedback,
    anomaly_anomalyId,
    anomaly_anomalyScore,
    anomaly_impact,
    anomaly_monitorArn,

    -- * AnomalyDateInterval
    AnomalyDateInterval (..),
    newAnomalyDateInterval,
    anomalyDateInterval_endDate,
    anomalyDateInterval_startDate,

    -- * AnomalyMonitor
    AnomalyMonitor (..),
    newAnomalyMonitor,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_monitorDimension,
    anomalyMonitor_creationDate,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_monitorArn,
    anomalyMonitor_monitorName,
    anomalyMonitor_monitorType,

    -- * AnomalyScore
    AnomalyScore (..),
    newAnomalyScore,
    anomalyScore_maxScore,
    anomalyScore_currentScore,

    -- * AnomalySubscription
    AnomalySubscription (..),
    newAnomalySubscription,
    anomalySubscription_accountId,
    anomalySubscription_subscriptionArn,
    anomalySubscription_monitorArnList,
    anomalySubscription_subscribers,
    anomalySubscription_threshold,
    anomalySubscription_frequency,
    anomalySubscription_subscriptionName,

    -- * CostCategory
    CostCategory (..),
    newCostCategory,
    costCategory_processingStatus,
    costCategory_effectiveEnd,
    costCategory_splitChargeRules,
    costCategory_defaultValue,
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- * CostCategoryInheritedValueDimension
    CostCategoryInheritedValueDimension (..),
    newCostCategoryInheritedValueDimension,
    costCategoryInheritedValueDimension_dimensionName,
    costCategoryInheritedValueDimension_dimensionKey,

    -- * CostCategoryProcessingStatus
    CostCategoryProcessingStatus (..),
    newCostCategoryProcessingStatus,
    costCategoryProcessingStatus_status,
    costCategoryProcessingStatus_component,

    -- * CostCategoryReference
    CostCategoryReference (..),
    newCostCategoryReference,
    costCategoryReference_effectiveStart,
    costCategoryReference_values,
    costCategoryReference_costCategoryArn,
    costCategoryReference_processingStatus,
    costCategoryReference_numberOfRules,
    costCategoryReference_name,
    costCategoryReference_effectiveEnd,
    costCategoryReference_defaultValue,

    -- * CostCategoryRule
    CostCategoryRule (..),
    newCostCategoryRule,
    costCategoryRule_inheritedValue,
    costCategoryRule_value,
    costCategoryRule_rule,
    costCategoryRule_type,

    -- * CostCategorySplitChargeRule
    CostCategorySplitChargeRule (..),
    newCostCategorySplitChargeRule,
    costCategorySplitChargeRule_parameters,
    costCategorySplitChargeRule_source,
    costCategorySplitChargeRule_targets,
    costCategorySplitChargeRule_method,

    -- * CostCategorySplitChargeRuleParameter
    CostCategorySplitChargeRuleParameter (..),
    newCostCategorySplitChargeRuleParameter,
    costCategorySplitChargeRuleParameter_type,
    costCategorySplitChargeRuleParameter_values,

    -- * CostCategoryValues
    CostCategoryValues (..),
    newCostCategoryValues,
    costCategoryValues_values,
    costCategoryValues_key,
    costCategoryValues_matchOptions,

    -- * Coverage
    Coverage (..),
    newCoverage,
    coverage_coverageNormalizedUnits,
    coverage_coverageHours,
    coverage_coverageCost,

    -- * CoverageByTime
    CoverageByTime (..),
    newCoverageByTime,
    coverageByTime_groups,
    coverageByTime_timePeriod,
    coverageByTime_total,

    -- * CoverageCost
    CoverageCost (..),
    newCoverageCost,
    coverageCost_onDemandCost,

    -- * CoverageHours
    CoverageHours (..),
    newCoverageHours,
    coverageHours_coverageHoursPercentage,
    coverageHours_onDemandHours,
    coverageHours_totalRunningHours,
    coverageHours_reservedHours,

    -- * CoverageNormalizedUnits
    CoverageNormalizedUnits (..),
    newCoverageNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,
    coverageNormalizedUnits_totalRunningNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,

    -- * CurrentInstance
    CurrentInstance (..),
    newCurrentInstance,
    currentInstance_resourceId,
    currentInstance_currencyCode,
    currentInstance_resourceUtilization,
    currentInstance_resourceDetails,
    currentInstance_totalRunningHoursInLookbackPeriod,
    currentInstance_reservationCoveredHoursInLookbackPeriod,
    currentInstance_onDemandHoursInLookbackPeriod,
    currentInstance_monthlyCost,
    currentInstance_instanceName,
    currentInstance_savingsPlansCoveredHoursInLookbackPeriod,
    currentInstance_tags,

    -- * DateInterval
    DateInterval (..),
    newDateInterval,
    dateInterval_start,
    dateInterval_end,

    -- * DimensionValues
    DimensionValues (..),
    newDimensionValues,
    dimensionValues_values,
    dimensionValues_key,
    dimensionValues_matchOptions,

    -- * DimensionValuesWithAttributes
    DimensionValuesWithAttributes (..),
    newDimensionValuesWithAttributes,
    dimensionValuesWithAttributes_value,
    dimensionValuesWithAttributes_attributes,

    -- * DiskResourceUtilization
    DiskResourceUtilization (..),
    newDiskResourceUtilization,
    diskResourceUtilization_diskWriteOpsPerSecond,
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskReadBytesPerSecond,

    -- * EBSResourceUtilization
    EBSResourceUtilization (..),
    newEBSResourceUtilization,
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsReadBytesPerSecond,

    -- * EC2InstanceDetails
    EC2InstanceDetails (..),
    newEC2InstanceDetails,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_platform,
    eC2InstanceDetails_family,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_tenancy,
    eC2InstanceDetails_region,

    -- * EC2ResourceDetails
    EC2ResourceDetails (..),
    newEC2ResourceDetails,
    eC2ResourceDetails_platform,
    eC2ResourceDetails_vcpu,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_sku,
    eC2ResourceDetails_region,
    eC2ResourceDetails_hourlyOnDemandRate,

    -- * EC2ResourceUtilization
    EC2ResourceUtilization (..),
    newEC2ResourceUtilization,
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_networkResourceUtilization,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_diskResourceUtilization,

    -- * EC2Specification
    EC2Specification (..),
    newEC2Specification,
    eC2Specification_offeringClass,

    -- * ESInstanceDetails
    ESInstanceDetails (..),
    newESInstanceDetails,
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_instanceSize,
    eSInstanceDetails_sizeFlexEligible,
    eSInstanceDetails_region,

    -- * ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (..),
    newElastiCacheInstanceDetails,
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_productDescription,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_sizeFlexEligible,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_nodeType,

    -- * Expression
    Expression (..),
    newExpression,
    expression_not,
    expression_and,
    expression_or,
    expression_costCategories,
    expression_dimensions,
    expression_tags,

    -- * ForecastResult
    ForecastResult (..),
    newForecastResult,
    forecastResult_timePeriod,
    forecastResult_meanValue,
    forecastResult_predictionIntervalUpperBound,
    forecastResult_predictionIntervalLowerBound,

    -- * Group
    Group (..),
    newGroup,
    group_metrics,
    group_keys,

    -- * GroupDefinition
    GroupDefinition (..),
    newGroupDefinition,
    groupDefinition_key,
    groupDefinition_type,

    -- * Impact
    Impact (..),
    newImpact,
    impact_totalImpact,
    impact_maxImpact,

    -- * InstanceDetails
    InstanceDetails (..),
    newInstanceDetails,
    instanceDetails_eSInstanceDetails,
    instanceDetails_rDSInstanceDetails,
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_eC2InstanceDetails,
    instanceDetails_redshiftInstanceDetails,

    -- * MetricValue
    MetricValue (..),
    newMetricValue,
    metricValue_amount,
    metricValue_unit,

    -- * ModifyRecommendationDetail
    ModifyRecommendationDetail (..),
    newModifyRecommendationDetail,
    modifyRecommendationDetail_targetInstances,

    -- * NetworkResourceUtilization
    NetworkResourceUtilization (..),
    newNetworkResourceUtilization,
    networkResourceUtilization_networkPacketsOutPerSecond,
    networkResourceUtilization_networkInBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,
    networkResourceUtilization_networkOutBytesPerSecond,

    -- * RDSInstanceDetails
    RDSInstanceDetails (..),
    newRDSInstanceDetails,
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_family,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_sizeFlexEligible,
    rDSInstanceDetails_region,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_databaseEdition,

    -- * RedshiftInstanceDetails
    RedshiftInstanceDetails (..),
    newRedshiftInstanceDetails,
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_sizeFlexEligible,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_nodeType,

    -- * ReservationAggregates
    ReservationAggregates (..),
    newReservationAggregates,
    reservationAggregates_purchasedHours,
    reservationAggregates_totalActualHours,
    reservationAggregates_utilizationPercentage,
    reservationAggregates_totalAmortizedFee,
    reservationAggregates_unusedUnits,
    reservationAggregates_unrealizedSavings,
    reservationAggregates_rICostForUnusedHours,
    reservationAggregates_unusedHours,
    reservationAggregates_realizedSavings,
    reservationAggregates_purchasedUnits,
    reservationAggregates_amortizedUpfrontFee,
    reservationAggregates_amortizedRecurringFee,
    reservationAggregates_utilizationPercentageInUnits,
    reservationAggregates_netRISavings,
    reservationAggregates_onDemandCostOfRIHoursUsed,
    reservationAggregates_totalPotentialRISavings,
    reservationAggregates_totalActualUnits,

    -- * ReservationCoverageGroup
    ReservationCoverageGroup (..),
    newReservationCoverageGroup,
    reservationCoverageGroup_coverage,
    reservationCoverageGroup_attributes,

    -- * ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (..),
    newReservationPurchaseRecommendation,
    reservationPurchaseRecommendation_termInYears,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_lookbackPeriodInDays,
    reservationPurchaseRecommendation_paymentOption,

    -- * ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail (..),
    newReservationPurchaseRecommendationDetail,
    reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost,
    reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_currencyCode,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase,
    reservationPurchaseRecommendationDetail_averageUtilization,
    reservationPurchaseRecommendationDetail_accountId,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationDetail_upfrontCost,
    reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost,
    reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase,
    reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod,
    reservationPurchaseRecommendationDetail_instanceDetails,
    reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths,

    -- * ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata (..),
    newReservationPurchaseRecommendationMetadata,
    reservationPurchaseRecommendationMetadata_recommendationId,
    reservationPurchaseRecommendationMetadata_generationTimestamp,

    -- * ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (..),
    newReservationPurchaseRecommendationSummary,
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,

    -- * ReservationUtilizationGroup
    ReservationUtilizationGroup (..),
    newReservationUtilizationGroup,
    reservationUtilizationGroup_value,
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_utilization,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_eC2ResourceDetails,

    -- * ResourceUtilization
    ResourceUtilization (..),
    newResourceUtilization,
    resourceUtilization_eC2ResourceUtilization,

    -- * ResultByTime
    ResultByTime (..),
    newResultByTime,
    resultByTime_groups,
    resultByTime_timePeriod,
    resultByTime_total,
    resultByTime_estimated,

    -- * RightsizingRecommendation
    RightsizingRecommendation (..),
    newRightsizingRecommendation,
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_modifyRecommendationDetail,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_rightsizingType,
    rightsizingRecommendation_terminateRecommendationDetail,

    -- * RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration (..),
    newRightsizingRecommendationConfiguration,
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- * RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata (..),
    newRightsizingRecommendationMetadata,
    rightsizingRecommendationMetadata_recommendationId,
    rightsizingRecommendationMetadata_generationTimestamp,
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,

    -- * RightsizingRecommendationSummary
    RightsizingRecommendationSummary (..),
    newRightsizingRecommendationSummary,
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_savingsCurrencyCode,
    rightsizingRecommendationSummary_totalRecommendationCount,
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,

    -- * RootCause
    RootCause (..),
    newRootCause,
    rootCause_service,
    rootCause_usageType,
    rootCause_linkedAccount,
    rootCause_region,

    -- * SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (..),
    newSavingsPlansAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,

    -- * SavingsPlansCoverage
    SavingsPlansCoverage (..),
    newSavingsPlansCoverage,
    savingsPlansCoverage_timePeriod,
    savingsPlansCoverage_coverage,
    savingsPlansCoverage_attributes,

    -- * SavingsPlansCoverageData
    SavingsPlansCoverageData (..),
    newSavingsPlansCoverageData,
    savingsPlansCoverageData_onDemandCost,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_totalCost,

    -- * SavingsPlansDetails
    SavingsPlansDetails (..),
    newSavingsPlansDetails,
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,
    savingsPlansDetails_region,

    -- * SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation (..),
    newSavingsPlansPurchaseRecommendation,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_termInYears,
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,

    -- * SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail (..),
    newSavingsPlansPurchaseRecommendationDetail,
    savingsPlansPurchaseRecommendationDetail_currencyCode,
    savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_savingsPlansDetails,
    savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedROI,
    savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationDetail_accountId,
    savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationDetail_upfrontCost,
    savingsPlansPurchaseRecommendationDetail_estimatedSPCost,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization,
    savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase,

    -- * SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata (..),
    newSavingsPlansPurchaseRecommendationMetadata,
    savingsPlansPurchaseRecommendationMetadata_recommendationId,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,

    -- * SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary (..),
    newSavingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendationSummary_currencyCode,
    savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_estimatedTotalCost,
    savingsPlansPurchaseRecommendationSummary_estimatedROI,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationSummary_totalRecommendationCount,
    savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend,
    savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase,

    -- * SavingsPlansSavings
    SavingsPlansSavings (..),
    newSavingsPlansSavings,
    savingsPlansSavings_netSavings,
    savingsPlansSavings_onDemandCostEquivalent,

    -- * SavingsPlansUtilization
    SavingsPlansUtilization (..),
    newSavingsPlansUtilization,
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_utilizationPercentage,
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_usedCommitment,

    -- * SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates (..),
    newSavingsPlansUtilizationAggregates,
    savingsPlansUtilizationAggregates_amortizedCommitment,
    savingsPlansUtilizationAggregates_savings,
    savingsPlansUtilizationAggregates_utilization,

    -- * SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime (..),
    newSavingsPlansUtilizationByTime,
    savingsPlansUtilizationByTime_amortizedCommitment,
    savingsPlansUtilizationByTime_savings,
    savingsPlansUtilizationByTime_timePeriod,
    savingsPlansUtilizationByTime_utilization,

    -- * SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail (..),
    newSavingsPlansUtilizationDetail,
    savingsPlansUtilizationDetail_amortizedCommitment,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_utilization,
    savingsPlansUtilizationDetail_savingsPlanArn,

    -- * ServiceSpecification
    ServiceSpecification (..),
    newServiceSpecification,
    serviceSpecification_eC2Specification,

    -- * SortDefinition
    SortDefinition (..),
    newSortDefinition,
    sortDefinition_sortOrder,
    sortDefinition_key,

    -- * Subscriber
    Subscriber (..),
    newSubscriber,
    subscriber_status,
    subscriber_address,
    subscriber_type,

    -- * TagValues
    TagValues (..),
    newTagValues,
    tagValues_values,
    tagValues_key,
    tagValues_matchOptions,

    -- * TargetInstance
    TargetInstance (..),
    newTargetInstance,
    targetInstance_currencyCode,
    targetInstance_resourceDetails,
    targetInstance_platformDifferences,
    targetInstance_defaultTargetInstance,
    targetInstance_estimatedMonthlyCost,
    targetInstance_estimatedMonthlySavings,
    targetInstance_expectedResourceUtilization,

    -- * TerminateRecommendationDetail
    TerminateRecommendationDetail (..),
    newTerminateRecommendationDetail,
    terminateRecommendationDetail_currencyCode,
    terminateRecommendationDetail_estimatedMonthlySavings,

    -- * TotalImpactFilter
    TotalImpactFilter (..),
    newTotalImpactFilter,
    totalImpactFilter_endValue,
    totalImpactFilter_numericOperator,
    totalImpactFilter_startValue,

    -- * UtilizationByTime
    UtilizationByTime (..),
    newUtilizationByTime,
    utilizationByTime_groups,
    utilizationByTime_timePeriod,
    utilizationByTime_total,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.AccountScope
import Amazonka.CostExplorer.Types.Anomaly
import Amazonka.CostExplorer.Types.AnomalyDateInterval
import Amazonka.CostExplorer.Types.AnomalyFeedbackType
import Amazonka.CostExplorer.Types.AnomalyMonitor
import Amazonka.CostExplorer.Types.AnomalyScore
import Amazonka.CostExplorer.Types.AnomalySubscription
import Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
import Amazonka.CostExplorer.Types.Context
import Amazonka.CostExplorer.Types.CostCategory
import Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimension
import Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimensionName
import Amazonka.CostExplorer.Types.CostCategoryProcessingStatus
import Amazonka.CostExplorer.Types.CostCategoryReference
import Amazonka.CostExplorer.Types.CostCategoryRule
import Amazonka.CostExplorer.Types.CostCategoryRuleType
import Amazonka.CostExplorer.Types.CostCategoryRuleVersion
import Amazonka.CostExplorer.Types.CostCategorySplitChargeMethod
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRule
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameter
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameterType
import Amazonka.CostExplorer.Types.CostCategoryStatus
import Amazonka.CostExplorer.Types.CostCategoryStatusComponent
import Amazonka.CostExplorer.Types.CostCategoryValues
import Amazonka.CostExplorer.Types.Coverage
import Amazonka.CostExplorer.Types.CoverageByTime
import Amazonka.CostExplorer.Types.CoverageCost
import Amazonka.CostExplorer.Types.CoverageHours
import Amazonka.CostExplorer.Types.CoverageNormalizedUnits
import Amazonka.CostExplorer.Types.CurrentInstance
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.Dimension
import Amazonka.CostExplorer.Types.DimensionValues
import Amazonka.CostExplorer.Types.DimensionValuesWithAttributes
import Amazonka.CostExplorer.Types.DiskResourceUtilization
import Amazonka.CostExplorer.Types.EBSResourceUtilization
import Amazonka.CostExplorer.Types.EC2InstanceDetails
import Amazonka.CostExplorer.Types.EC2ResourceDetails
import Amazonka.CostExplorer.Types.EC2ResourceUtilization
import Amazonka.CostExplorer.Types.EC2Specification
import Amazonka.CostExplorer.Types.ESInstanceDetails
import Amazonka.CostExplorer.Types.ElastiCacheInstanceDetails
import Amazonka.CostExplorer.Types.Expression
import Amazonka.CostExplorer.Types.FindingReasonCode
import Amazonka.CostExplorer.Types.ForecastResult
import Amazonka.CostExplorer.Types.Granularity
import Amazonka.CostExplorer.Types.Group
import Amazonka.CostExplorer.Types.GroupDefinition
import Amazonka.CostExplorer.Types.GroupDefinitionType
import Amazonka.CostExplorer.Types.Impact
import Amazonka.CostExplorer.Types.InstanceDetails
import Amazonka.CostExplorer.Types.LookbackPeriodInDays
import Amazonka.CostExplorer.Types.MatchOption
import Amazonka.CostExplorer.Types.Metric
import Amazonka.CostExplorer.Types.MetricValue
import Amazonka.CostExplorer.Types.ModifyRecommendationDetail
import Amazonka.CostExplorer.Types.MonitorDimension
import Amazonka.CostExplorer.Types.MonitorType
import Amazonka.CostExplorer.Types.NetworkResourceUtilization
import Amazonka.CostExplorer.Types.NumericOperator
import Amazonka.CostExplorer.Types.OfferingClass
import Amazonka.CostExplorer.Types.PaymentOption
import Amazonka.CostExplorer.Types.PlatformDifference
import Amazonka.CostExplorer.Types.RDSInstanceDetails
import Amazonka.CostExplorer.Types.RecommendationTarget
import Amazonka.CostExplorer.Types.RedshiftInstanceDetails
import Amazonka.CostExplorer.Types.ReservationAggregates
import Amazonka.CostExplorer.Types.ReservationCoverageGroup
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendation
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
import Amazonka.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Amazonka.CostExplorer.Types.ReservationUtilizationGroup
import Amazonka.CostExplorer.Types.ResourceDetails
import Amazonka.CostExplorer.Types.ResourceUtilization
import Amazonka.CostExplorer.Types.ResultByTime
import Amazonka.CostExplorer.Types.RightsizingRecommendation
import Amazonka.CostExplorer.Types.RightsizingRecommendationConfiguration
import Amazonka.CostExplorer.Types.RightsizingRecommendationMetadata
import Amazonka.CostExplorer.Types.RightsizingRecommendationSummary
import Amazonka.CostExplorer.Types.RightsizingType
import Amazonka.CostExplorer.Types.RootCause
import Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Amazonka.CostExplorer.Types.SavingsPlansCoverage
import Amazonka.CostExplorer.Types.SavingsPlansCoverageData
import Amazonka.CostExplorer.Types.SavingsPlansDataType
import Amazonka.CostExplorer.Types.SavingsPlansDetails
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendation
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
import Amazonka.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Amazonka.CostExplorer.Types.SavingsPlansSavings
import Amazonka.CostExplorer.Types.SavingsPlansUtilization
import Amazonka.CostExplorer.Types.SavingsPlansUtilizationAggregates
import Amazonka.CostExplorer.Types.SavingsPlansUtilizationByTime
import Amazonka.CostExplorer.Types.SavingsPlansUtilizationDetail
import Amazonka.CostExplorer.Types.ServiceSpecification
import Amazonka.CostExplorer.Types.SortDefinition
import Amazonka.CostExplorer.Types.SortOrder
import Amazonka.CostExplorer.Types.Subscriber
import Amazonka.CostExplorer.Types.SubscriberStatus
import Amazonka.CostExplorer.Types.SubscriberType
import Amazonka.CostExplorer.Types.SupportedSavingsPlansType
import Amazonka.CostExplorer.Types.TagValues
import Amazonka.CostExplorer.Types.TargetInstance
import Amazonka.CostExplorer.Types.TermInYears
import Amazonka.CostExplorer.Types.TerminateRecommendationDetail
import Amazonka.CostExplorer.Types.TotalImpactFilter
import Amazonka.CostExplorer.Types.UtilizationByTime
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CostExplorer",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ce",
      Core._serviceSigningName = "ce",
      Core._serviceVersion = "2017-10-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CostExplorer",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The cost anomaly subscription does not exist for the account.
_UnknownSubscriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownSubscriptionException =
  Core._MatchServiceError
    defaultService
    "UnknownSubscriptionException"

-- | The requested report expired. Update the date interval and try again.
_BillExpirationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BillExpirationException =
  Core._MatchServiceError
    defaultService
    "BillExpirationException"

-- | Your request parameters changed between pages. Try again with the old
-- parameters or without a pagination token.
_RequestChangedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestChangedException =
  Core._MatchServiceError
    defaultService
    "RequestChangedException"

-- | Cost Explorer was unable to identify the usage unit. Provide
-- @UsageType\/UsageTypeGroup@ filter selections that contain matching
-- units, for example: @hours@.
_UnresolvableUsageUnitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnresolvableUsageUnitException =
  Core._MatchServiceError
    defaultService
    "UnresolvableUsageUnitException"

-- | You\'ve reached the limit on the number of resources you can create, or
-- exceeded the size of an individual resource.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The pagination token is invalid. Try again without a pagination token.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The requested data is unavailable.
_DataUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataUnavailableException =
  Core._MatchServiceError
    defaultService
    "DataUnavailableException"

-- | The cost anomaly monitor does not exist for the account.
_UnknownMonitorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownMonitorException =
  Core._MatchServiceError
    defaultService
    "UnknownMonitorException"

-- | The specified ARN in the request doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You made too many calls in a short period of time. Try again later.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

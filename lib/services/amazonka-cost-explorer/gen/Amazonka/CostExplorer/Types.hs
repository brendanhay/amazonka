{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BillExpirationException,
    _DataUnavailableException,
    _GenerationExistsException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _RequestChangedException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _TooManyTagsException,
    _UnknownMonitorException,
    _UnknownSubscriptionException,
    _UnresolvableUsageUnitException,

    -- * AccountScope
    AccountScope (..),

    -- * AnomalyFeedbackType
    AnomalyFeedbackType (..),

    -- * AnomalySubscriptionFrequency
    AnomalySubscriptionFrequency (..),

    -- * Context
    Context (..),

    -- * CostAllocationTagStatus
    CostAllocationTagStatus (..),

    -- * CostAllocationTagType
    CostAllocationTagType (..),

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

    -- * GenerationStatus
    GenerationStatus (..),

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
    anomaly_anomalyEndDate,
    anomaly_anomalyStartDate,
    anomaly_dimensionValue,
    anomaly_feedback,
    anomaly_rootCauses,
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
    anomalyMonitor_creationDate,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_monitorArn,
    anomalyMonitor_monitorDimension,
    anomalyMonitor_monitorSpecification,
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
    anomalySubscription_threshold,
    anomalySubscription_thresholdExpression,
    anomalySubscription_monitorArnList,
    anomalySubscription_subscribers,
    anomalySubscription_frequency,
    anomalySubscription_subscriptionName,

    -- * CostAllocationTag
    CostAllocationTag (..),
    newCostAllocationTag,
    costAllocationTag_tagKey,
    costAllocationTag_type,
    costAllocationTag_status,

    -- * CostAllocationTagStatusEntry
    CostAllocationTagStatusEntry (..),
    newCostAllocationTagStatusEntry,
    costAllocationTagStatusEntry_tagKey,
    costAllocationTagStatusEntry_status,

    -- * CostCategory
    CostCategory (..),
    newCostCategory,
    costCategory_defaultValue,
    costCategory_effectiveEnd,
    costCategory_processingStatus,
    costCategory_splitChargeRules,
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- * CostCategoryInheritedValueDimension
    CostCategoryInheritedValueDimension (..),
    newCostCategoryInheritedValueDimension,
    costCategoryInheritedValueDimension_dimensionKey,
    costCategoryInheritedValueDimension_dimensionName,

    -- * CostCategoryProcessingStatus
    CostCategoryProcessingStatus (..),
    newCostCategoryProcessingStatus,
    costCategoryProcessingStatus_component,
    costCategoryProcessingStatus_status,

    -- * CostCategoryReference
    CostCategoryReference (..),
    newCostCategoryReference,
    costCategoryReference_costCategoryArn,
    costCategoryReference_defaultValue,
    costCategoryReference_effectiveEnd,
    costCategoryReference_effectiveStart,
    costCategoryReference_name,
    costCategoryReference_numberOfRules,
    costCategoryReference_processingStatus,
    costCategoryReference_values,

    -- * CostCategoryRule
    CostCategoryRule (..),
    newCostCategoryRule,
    costCategoryRule_inheritedValue,
    costCategoryRule_rule,
    costCategoryRule_type,
    costCategoryRule_value,

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
    costCategoryValues_key,
    costCategoryValues_matchOptions,
    costCategoryValues_values,

    -- * Coverage
    Coverage (..),
    newCoverage,
    coverage_coverageCost,
    coverage_coverageHours,
    coverage_coverageNormalizedUnits,

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
    coverageHours_reservedHours,
    coverageHours_totalRunningHours,

    -- * CoverageNormalizedUnits
    CoverageNormalizedUnits (..),
    newCoverageNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,
    coverageNormalizedUnits_totalRunningNormalizedUnits,

    -- * CurrentInstance
    CurrentInstance (..),
    newCurrentInstance,
    currentInstance_currencyCode,
    currentInstance_instanceName,
    currentInstance_monthlyCost,
    currentInstance_onDemandHoursInLookbackPeriod,
    currentInstance_reservationCoveredHoursInLookbackPeriod,
    currentInstance_resourceDetails,
    currentInstance_resourceId,
    currentInstance_resourceUtilization,
    currentInstance_savingsPlansCoveredHoursInLookbackPeriod,
    currentInstance_tags,
    currentInstance_totalRunningHoursInLookbackPeriod,

    -- * DateInterval
    DateInterval (..),
    newDateInterval,
    dateInterval_start,
    dateInterval_end,

    -- * DimensionValues
    DimensionValues (..),
    newDimensionValues,
    dimensionValues_key,
    dimensionValues_matchOptions,
    dimensionValues_values,

    -- * DimensionValuesWithAttributes
    DimensionValuesWithAttributes (..),
    newDimensionValuesWithAttributes,
    dimensionValuesWithAttributes_attributes,
    dimensionValuesWithAttributes_value,

    -- * DiskResourceUtilization
    DiskResourceUtilization (..),
    newDiskResourceUtilization,
    diskResourceUtilization_diskReadBytesPerSecond,
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskWriteOpsPerSecond,

    -- * EBSResourceUtilization
    EBSResourceUtilization (..),
    newEBSResourceUtilization,
    eBSResourceUtilization_ebsReadBytesPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,

    -- * EC2InstanceDetails
    EC2InstanceDetails (..),
    newEC2InstanceDetails,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_family,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_platform,
    eC2InstanceDetails_region,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_tenancy,

    -- * EC2ResourceDetails
    EC2ResourceDetails (..),
    newEC2ResourceDetails,
    eC2ResourceDetails_hourlyOnDemandRate,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_platform,
    eC2ResourceDetails_region,
    eC2ResourceDetails_sku,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_vcpu,

    -- * EC2ResourceUtilization
    EC2ResourceUtilization (..),
    newEC2ResourceUtilization,
    eC2ResourceUtilization_diskResourceUtilization,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_networkResourceUtilization,

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
    eSInstanceDetails_region,
    eSInstanceDetails_sizeFlexEligible,

    -- * ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (..),
    newElastiCacheInstanceDetails,
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_nodeType,
    elastiCacheInstanceDetails_productDescription,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_sizeFlexEligible,

    -- * Expression
    Expression (..),
    newExpression,
    expression_and,
    expression_costCategories,
    expression_dimensions,
    expression_not,
    expression_or,
    expression_tags,

    -- * ForecastResult
    ForecastResult (..),
    newForecastResult,
    forecastResult_meanValue,
    forecastResult_predictionIntervalLowerBound,
    forecastResult_predictionIntervalUpperBound,
    forecastResult_timePeriod,

    -- * GenerationSummary
    GenerationSummary (..),
    newGenerationSummary,
    generationSummary_estimatedCompletionTime,
    generationSummary_generationCompletionTime,
    generationSummary_generationStartedTime,
    generationSummary_generationStatus,
    generationSummary_recommendationId,

    -- * Group
    Group (..),
    newGroup,
    group_keys,
    group_metrics,

    -- * GroupDefinition
    GroupDefinition (..),
    newGroupDefinition,
    groupDefinition_key,
    groupDefinition_type,

    -- * Impact
    Impact (..),
    newImpact,
    impact_totalActualSpend,
    impact_totalExpectedSpend,
    impact_totalImpact,
    impact_totalImpactPercentage,
    impact_maxImpact,

    -- * InstanceDetails
    InstanceDetails (..),
    newInstanceDetails,
    instanceDetails_eC2InstanceDetails,
    instanceDetails_eSInstanceDetails,
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_rDSInstanceDetails,
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
    networkResourceUtilization_networkInBytesPerSecond,
    networkResourceUtilization_networkOutBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,
    networkResourceUtilization_networkPacketsOutPerSecond,

    -- * RDSInstanceDetails
    RDSInstanceDetails (..),
    newRDSInstanceDetails,
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_databaseEdition,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_family,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_region,
    rDSInstanceDetails_sizeFlexEligible,

    -- * RedshiftInstanceDetails
    RedshiftInstanceDetails (..),
    newRedshiftInstanceDetails,
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_nodeType,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_sizeFlexEligible,

    -- * ReservationAggregates
    ReservationAggregates (..),
    newReservationAggregates,
    reservationAggregates_amortizedRecurringFee,
    reservationAggregates_amortizedUpfrontFee,
    reservationAggregates_netRISavings,
    reservationAggregates_onDemandCostOfRIHoursUsed,
    reservationAggregates_purchasedHours,
    reservationAggregates_purchasedUnits,
    reservationAggregates_rICostForUnusedHours,
    reservationAggregates_realizedSavings,
    reservationAggregates_totalActualHours,
    reservationAggregates_totalActualUnits,
    reservationAggregates_totalAmortizedFee,
    reservationAggregates_totalPotentialRISavings,
    reservationAggregates_unrealizedSavings,
    reservationAggregates_unusedHours,
    reservationAggregates_unusedUnits,
    reservationAggregates_utilizationPercentage,
    reservationAggregates_utilizationPercentageInUnits,

    -- * ReservationCoverageGroup
    ReservationCoverageGroup (..),
    newReservationCoverageGroup,
    reservationCoverageGroup_attributes,
    reservationCoverageGroup_coverage,

    -- * ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (..),
    newReservationPurchaseRecommendation,
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_lookbackPeriodInDays,
    reservationPurchaseRecommendation_paymentOption,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_termInYears,

    -- * ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail (..),
    newReservationPurchaseRecommendationDetail,
    reservationPurchaseRecommendationDetail_accountId,
    reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_averageUtilization,
    reservationPurchaseRecommendationDetail_currencyCode,
    reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths,
    reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod,
    reservationPurchaseRecommendationDetail_instanceDetails,
    reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase,
    reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase,
    reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost,
    reservationPurchaseRecommendationDetail_upfrontCost,

    -- * ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata (..),
    newReservationPurchaseRecommendationMetadata,
    reservationPurchaseRecommendationMetadata_generationTimestamp,
    reservationPurchaseRecommendationMetadata_recommendationId,

    -- * ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (..),
    newReservationPurchaseRecommendationSummary,
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,

    -- * ReservationUtilizationGroup
    ReservationUtilizationGroup (..),
    newReservationUtilizationGroup,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_utilization,
    reservationUtilizationGroup_value,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_eC2ResourceDetails,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_key,
    resourceTag_value,

    -- * ResourceUtilization
    ResourceUtilization (..),
    newResourceUtilization,
    resourceUtilization_eC2ResourceUtilization,

    -- * ResultByTime
    ResultByTime (..),
    newResultByTime,
    resultByTime_estimated,
    resultByTime_groups,
    resultByTime_timePeriod,
    resultByTime_total,

    -- * RightsizingRecommendation
    RightsizingRecommendation (..),
    newRightsizingRecommendation,
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_modifyRecommendationDetail,
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
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_generationTimestamp,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,
    rightsizingRecommendationMetadata_recommendationId,

    -- * RightsizingRecommendationSummary
    RightsizingRecommendationSummary (..),
    newRightsizingRecommendationSummary,
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,
    rightsizingRecommendationSummary_savingsCurrencyCode,
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_totalRecommendationCount,

    -- * RootCause
    RootCause (..),
    newRootCause,
    rootCause_linkedAccount,
    rootCause_linkedAccountName,
    rootCause_region,
    rootCause_service,
    rootCause_usageType,

    -- * SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (..),
    newSavingsPlansAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,

    -- * SavingsPlansCoverage
    SavingsPlansCoverage (..),
    newSavingsPlansCoverage,
    savingsPlansCoverage_attributes,
    savingsPlansCoverage_coverage,
    savingsPlansCoverage_timePeriod,

    -- * SavingsPlansCoverageData
    SavingsPlansCoverageData (..),
    newSavingsPlansCoverageData,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_onDemandCost,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
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
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_termInYears,

    -- * SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail (..),
    newSavingsPlansPurchaseRecommendationDetail,
    savingsPlansPurchaseRecommendationDetail_accountId,
    savingsPlansPurchaseRecommendationDetail_currencyCode,
    savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization,
    savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationDetail_estimatedROI,
    savingsPlansPurchaseRecommendationDetail_estimatedSPCost,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationDetail_savingsPlansDetails,
    savingsPlansPurchaseRecommendationDetail_upfrontCost,

    -- * SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata (..),
    newSavingsPlansPurchaseRecommendationMetadata,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,
    savingsPlansPurchaseRecommendationMetadata_recommendationId,

    -- * SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary (..),
    newSavingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendationSummary_currencyCode,
    savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend,
    savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationSummary_estimatedROI,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationSummary_estimatedTotalCost,
    savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_totalRecommendationCount,

    -- * SavingsPlansSavings
    SavingsPlansSavings (..),
    newSavingsPlansSavings,
    savingsPlansSavings_netSavings,
    savingsPlansSavings_onDemandCostEquivalent,

    -- * SavingsPlansUtilization
    SavingsPlansUtilization (..),
    newSavingsPlansUtilization,
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_usedCommitment,
    savingsPlansUtilization_utilizationPercentage,

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
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_savingsPlanArn,
    savingsPlansUtilizationDetail_utilization,

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
    subscriber_address,
    subscriber_status,
    subscriber_type,

    -- * TagValues
    TagValues (..),
    newTagValues,
    tagValues_key,
    tagValues_matchOptions,
    tagValues_values,

    -- * TargetInstance
    TargetInstance (..),
    newTargetInstance,
    targetInstance_currencyCode,
    targetInstance_defaultTargetInstance,
    targetInstance_estimatedMonthlyCost,
    targetInstance_estimatedMonthlySavings,
    targetInstance_expectedResourceUtilization,
    targetInstance_platformDifferences,
    targetInstance_resourceDetails,

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

    -- * UpdateCostAllocationTagsStatusError
    UpdateCostAllocationTagsStatusError (..),
    newUpdateCostAllocationTagsStatusError,
    updateCostAllocationTagsStatusError_code,
    updateCostAllocationTagsStatusError_message,
    updateCostAllocationTagsStatusError_tagKey,

    -- * UtilizationByTime
    UtilizationByTime (..),
    newUtilizationByTime,
    utilizationByTime_groups,
    utilizationByTime_timePeriod,
    utilizationByTime_total,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.AccountScope
import Amazonka.CostExplorer.Types.Anomaly
import Amazonka.CostExplorer.Types.AnomalyDateInterval
import Amazonka.CostExplorer.Types.AnomalyFeedbackType
import Amazonka.CostExplorer.Types.AnomalyMonitor
import Amazonka.CostExplorer.Types.AnomalyScore
import Amazonka.CostExplorer.Types.AnomalySubscription
import Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
import Amazonka.CostExplorer.Types.Context
import Amazonka.CostExplorer.Types.CostAllocationTag
import Amazonka.CostExplorer.Types.CostAllocationTagStatus
import Amazonka.CostExplorer.Types.CostAllocationTagStatusEntry
import Amazonka.CostExplorer.Types.CostAllocationTagType
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
import Amazonka.CostExplorer.Types.GenerationStatus
import Amazonka.CostExplorer.Types.GenerationSummary
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
import Amazonka.CostExplorer.Types.ResourceTag
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
import Amazonka.CostExplorer.Types.UpdateCostAllocationTagsStatusError
import Amazonka.CostExplorer.Types.UtilizationByTime
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-10-25@ of the Amazon Cost Explorer Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CostExplorer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ce",
      Core.signingName = "ce",
      Core.version = "2017-10-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CostExplorer",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested report expired. Update the date interval and try again.
_BillExpirationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BillExpirationException =
  Core._MatchServiceError
    defaultService
    "BillExpirationException"

-- | The requested data is unavailable.
_DataUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DataUnavailableException =
  Core._MatchServiceError
    defaultService
    "DataUnavailableException"

-- | A request to generate a recommendation is already in progress.
_GenerationExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GenerationExistsException =
  Core._MatchServiceError
    defaultService
    "GenerationExistsException"

-- | The pagination token is invalid. Try again without a pagination token.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | You made too many calls in a short period of time. Try again later.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Your request parameters changed between pages. Try again with the old
-- parameters or without a pagination token.
_RequestChangedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestChangedException =
  Core._MatchServiceError
    defaultService
    "RequestChangedException"

-- | The specified ARN in the request doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You\'ve reached the limit on the number of resources you can create, or
-- exceeded the size of an individual resource.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Can occur if you specify a number of tags for a resource greater than
-- the maximum 50 user tags per resource.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The cost anomaly monitor does not exist for the account.
_UnknownMonitorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnknownMonitorException =
  Core._MatchServiceError
    defaultService
    "UnknownMonitorException"

-- | The cost anomaly subscription does not exist for the account.
_UnknownSubscriptionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnknownSubscriptionException =
  Core._MatchServiceError
    defaultService
    "UnknownSubscriptionException"

-- | Cost Explorer was unable to identify the usage unit. Provide
-- @UsageType\/UsageTypeGroup@ filter selections that contain matching
-- units, for example: @hours@.
_UnresolvableUsageUnitException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnresolvableUsageUnitException =
  Core._MatchServiceError
    defaultService
    "UnresolvableUsageUnitException"

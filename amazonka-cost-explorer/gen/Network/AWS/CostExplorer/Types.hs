{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceQuotaExceededException,
    _RequestChangedException,
    _UnknownSubscriptionException,
    _UnknownMonitorException,
    _DataUnavailableException,
    _InvalidNextTokenException,
    _BillExpirationException,
    _UnresolvableUsageUnitException,
    _LimitExceededException,
    _ResourceNotFoundException,

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
    anomaly_dimensionValue,
    anomaly_rootCauses,
    anomaly_feedback,
    anomaly_anomalyStartDate,
    anomaly_anomalyEndDate,
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
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_creationDate,
    anomalyMonitor_monitorDimension,
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
    costCategory_splitChargeRules,
    costCategory_processingStatus,
    costCategory_defaultValue,
    costCategory_effectiveEnd,
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
    costCategoryReference_numberOfRules,
    costCategoryReference_costCategoryArn,
    costCategoryReference_values,
    costCategoryReference_name,
    costCategoryReference_processingStatus,
    costCategoryReference_defaultValue,
    costCategoryReference_effectiveStart,
    costCategoryReference_effectiveEnd,

    -- * CostCategoryRule
    CostCategoryRule (..),
    newCostCategoryRule,
    costCategoryRule_inheritedValue,
    costCategoryRule_rule,
    costCategoryRule_value,
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
    costCategoryValues_key,
    costCategoryValues_values,
    costCategoryValues_matchOptions,

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
    coverageHours_reservedHours,
    coverageHours_totalRunningHours,
    coverageHours_onDemandHours,
    coverageHours_coverageHoursPercentage,

    -- * CoverageNormalizedUnits
    CoverageNormalizedUnits (..),
    newCoverageNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,
    coverageNormalizedUnits_totalRunningNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,

    -- * CurrentInstance
    CurrentInstance (..),
    newCurrentInstance,
    currentInstance_resourceId,
    currentInstance_savingsPlansCoveredHoursInLookbackPeriod,
    currentInstance_instanceName,
    currentInstance_onDemandHoursInLookbackPeriod,
    currentInstance_currencyCode,
    currentInstance_tags,
    currentInstance_reservationCoveredHoursInLookbackPeriod,
    currentInstance_monthlyCost,
    currentInstance_resourceUtilization,
    currentInstance_resourceDetails,
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
    dimensionValues_values,
    dimensionValues_matchOptions,

    -- * DimensionValuesWithAttributes
    DimensionValuesWithAttributes (..),
    newDimensionValuesWithAttributes,
    dimensionValuesWithAttributes_attributes,
    dimensionValuesWithAttributes_value,

    -- * DiskResourceUtilization
    DiskResourceUtilization (..),
    newDiskResourceUtilization,
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskReadBytesPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskWriteOpsPerSecond,

    -- * EBSResourceUtilization
    EBSResourceUtilization (..),
    newEBSResourceUtilization,
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,
    eBSResourceUtilization_ebsReadBytesPerSecond,

    -- * EC2InstanceDetails
    EC2InstanceDetails (..),
    newEC2InstanceDetails,
    eC2InstanceDetails_platform,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_tenancy,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_family,
    eC2InstanceDetails_region,

    -- * EC2ResourceDetails
    EC2ResourceDetails (..),
    newEC2ResourceDetails,
    eC2ResourceDetails_platform,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_vcpu,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_hourlyOnDemandRate,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_region,
    eC2ResourceDetails_sku,

    -- * EC2ResourceUtilization
    EC2ResourceUtilization (..),
    newEC2ResourceUtilization,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_networkResourceUtilization,
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_diskResourceUtilization,

    -- * EC2Specification
    EC2Specification (..),
    newEC2Specification,
    eC2Specification_offeringClass,

    -- * ESInstanceDetails
    ESInstanceDetails (..),
    newESInstanceDetails,
    eSInstanceDetails_sizeFlexEligible,
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_instanceSize,
    eSInstanceDetails_region,

    -- * ElastiCacheInstanceDetails
    ElastiCacheInstanceDetails (..),
    newElastiCacheInstanceDetails,
    elastiCacheInstanceDetails_sizeFlexEligible,
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_nodeType,
    elastiCacheInstanceDetails_productDescription,

    -- * Expression
    Expression (..),
    newExpression,
    expression_costCategories,
    expression_not,
    expression_or,
    expression_tags,
    expression_and,
    expression_dimensions,

    -- * ForecastResult
    ForecastResult (..),
    newForecastResult,
    forecastResult_meanValue,
    forecastResult_timePeriod,
    forecastResult_predictionIntervalLowerBound,
    forecastResult_predictionIntervalUpperBound,

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
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_redshiftInstanceDetails,
    instanceDetails_eSInstanceDetails,
    instanceDetails_eC2InstanceDetails,
    instanceDetails_rDSInstanceDetails,

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
    networkResourceUtilization_networkPacketsOutPerSecond,
    networkResourceUtilization_networkOutBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,

    -- * RDSInstanceDetails
    RDSInstanceDetails (..),
    newRDSInstanceDetails,
    rDSInstanceDetails_databaseEdition,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_sizeFlexEligible,
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_family,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_region,

    -- * RedshiftInstanceDetails
    RedshiftInstanceDetails (..),
    newRedshiftInstanceDetails,
    redshiftInstanceDetails_sizeFlexEligible,
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_nodeType,

    -- * ReservationAggregates
    ReservationAggregates (..),
    newReservationAggregates,
    reservationAggregates_unusedHours,
    reservationAggregates_realizedSavings,
    reservationAggregates_totalActualHours,
    reservationAggregates_purchasedHours,
    reservationAggregates_unrealizedSavings,
    reservationAggregates_onDemandCostOfRIHoursUsed,
    reservationAggregates_amortizedRecurringFee,
    reservationAggregates_rICostForUnusedHours,
    reservationAggregates_unusedUnits,
    reservationAggregates_totalActualUnits,
    reservationAggregates_totalPotentialRISavings,
    reservationAggregates_totalAmortizedFee,
    reservationAggregates_netRISavings,
    reservationAggregates_utilizationPercentageInUnits,
    reservationAggregates_amortizedUpfrontFee,
    reservationAggregates_purchasedUnits,
    reservationAggregates_utilizationPercentage,

    -- * ReservationCoverageGroup
    ReservationCoverageGroup (..),
    newReservationCoverageGroup,
    reservationCoverageGroup_attributes,
    reservationCoverageGroup_coverage,

    -- * ReservationPurchaseRecommendation
    ReservationPurchaseRecommendation (..),
    newReservationPurchaseRecommendation,
    reservationPurchaseRecommendation_paymentOption,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_termInYears,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_lookbackPeriodInDays,

    -- * ReservationPurchaseRecommendationDetail
    ReservationPurchaseRecommendationDetail (..),
    newReservationPurchaseRecommendationDetail,
    reservationPurchaseRecommendationDetail_upfrontCost,
    reservationPurchaseRecommendationDetail_accountId,
    reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationDetail_averageUtilization,
    reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase,
    reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_instanceDetails,
    reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod,
    reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase,
    reservationPurchaseRecommendationDetail_currencyCode,
    reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths,
    reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost,

    -- * ReservationPurchaseRecommendationMetadata
    ReservationPurchaseRecommendationMetadata (..),
    newReservationPurchaseRecommendationMetadata,
    reservationPurchaseRecommendationMetadata_recommendationId,
    reservationPurchaseRecommendationMetadata_generationTimestamp,

    -- * ReservationPurchaseRecommendationSummary
    ReservationPurchaseRecommendationSummary (..),
    newReservationPurchaseRecommendationSummary,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,

    -- * ReservationUtilizationGroup
    ReservationUtilizationGroup (..),
    newReservationUtilizationGroup,
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_utilization,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_value,

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
    resultByTime_estimated,
    resultByTime_total,

    -- * RightsizingRecommendation
    RightsizingRecommendation (..),
    newRightsizingRecommendation,
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_terminateRecommendationDetail,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_rightsizingType,
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_modifyRecommendationDetail,

    -- * RightsizingRecommendationConfiguration
    RightsizingRecommendationConfiguration (..),
    newRightsizingRecommendationConfiguration,
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- * RightsizingRecommendationMetadata
    RightsizingRecommendationMetadata (..),
    newRightsizingRecommendationMetadata,
    rightsizingRecommendationMetadata_recommendationId,
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,
    rightsizingRecommendationMetadata_generationTimestamp,

    -- * RightsizingRecommendationSummary
    RightsizingRecommendationSummary (..),
    newRightsizingRecommendationSummary,
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_totalRecommendationCount,
    rightsizingRecommendationSummary_savingsCurrencyCode,

    -- * RootCause
    RootCause (..),
    newRootCause,
    rootCause_service,
    rootCause_usageType,
    rootCause_region,
    rootCause_linkedAccount,

    -- * SavingsPlansAmortizedCommitment
    SavingsPlansAmortizedCommitment (..),
    newSavingsPlansAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,

    -- * SavingsPlansCoverage
    SavingsPlansCoverage (..),
    newSavingsPlansCoverage,
    savingsPlansCoverage_timePeriod,
    savingsPlansCoverage_attributes,
    savingsPlansCoverage_coverage,

    -- * SavingsPlansCoverageData
    SavingsPlansCoverageData (..),
    newSavingsPlansCoverageData,
    savingsPlansCoverageData_totalCost,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_onDemandCost,

    -- * SavingsPlansDetails
    SavingsPlansDetails (..),
    newSavingsPlansDetails,
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,
    savingsPlansDetails_region,

    -- * SavingsPlansPurchaseRecommendation
    SavingsPlansPurchaseRecommendation (..),
    newSavingsPlansPurchaseRecommendation,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_termInYears,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,

    -- * SavingsPlansPurchaseRecommendationDetail
    SavingsPlansPurchaseRecommendationDetail (..),
    newSavingsPlansPurchaseRecommendationDetail,
    savingsPlansPurchaseRecommendationDetail_upfrontCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost,
    savingsPlansPurchaseRecommendationDetail_accountId,
    savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization,
    savingsPlansPurchaseRecommendationDetail_savingsPlansDetails,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_currencyCode,
    savingsPlansPurchaseRecommendationDetail_estimatedSPCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationDetail_estimatedROI,
    savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend,

    -- * SavingsPlansPurchaseRecommendationMetadata
    SavingsPlansPurchaseRecommendationMetadata (..),
    newSavingsPlansPurchaseRecommendationMetadata,
    savingsPlansPurchaseRecommendationMetadata_recommendationId,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,

    -- * SavingsPlansPurchaseRecommendationSummary
    SavingsPlansPurchaseRecommendationSummary (..),
    newSavingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_estimatedTotalCost,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationSummary_currencyCode,
    savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationSummary_estimatedROI,
    savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend,
    savingsPlansPurchaseRecommendationSummary_totalRecommendationCount,
    savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase,

    -- * SavingsPlansSavings
    SavingsPlansSavings (..),
    newSavingsPlansSavings,
    savingsPlansSavings_onDemandCostEquivalent,
    savingsPlansSavings_netSavings,

    -- * SavingsPlansUtilization
    SavingsPlansUtilization (..),
    newSavingsPlansUtilization,
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_usedCommitment,
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_utilizationPercentage,

    -- * SavingsPlansUtilizationAggregates
    SavingsPlansUtilizationAggregates (..),
    newSavingsPlansUtilizationAggregates,
    savingsPlansUtilizationAggregates_savings,
    savingsPlansUtilizationAggregates_amortizedCommitment,
    savingsPlansUtilizationAggregates_utilization,

    -- * SavingsPlansUtilizationByTime
    SavingsPlansUtilizationByTime (..),
    newSavingsPlansUtilizationByTime,
    savingsPlansUtilizationByTime_savings,
    savingsPlansUtilizationByTime_amortizedCommitment,
    savingsPlansUtilizationByTime_timePeriod,
    savingsPlansUtilizationByTime_utilization,

    -- * SavingsPlansUtilizationDetail
    SavingsPlansUtilizationDetail (..),
    newSavingsPlansUtilizationDetail,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_utilization,
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_amortizedCommitment,
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
    tagValues_key,
    tagValues_values,
    tagValues_matchOptions,

    -- * TargetInstance
    TargetInstance (..),
    newTargetInstance,
    targetInstance_estimatedMonthlySavings,
    targetInstance_currencyCode,
    targetInstance_estimatedMonthlyCost,
    targetInstance_expectedResourceUtilization,
    targetInstance_platformDifferences,
    targetInstance_defaultTargetInstance,
    targetInstance_resourceDetails,

    -- * TerminateRecommendationDetail
    TerminateRecommendationDetail (..),
    newTerminateRecommendationDetail,
    terminateRecommendationDetail_estimatedMonthlySavings,
    terminateRecommendationDetail_currencyCode,

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

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.AccountScope
import Network.AWS.CostExplorer.Types.Anomaly
import Network.AWS.CostExplorer.Types.AnomalyDateInterval
import Network.AWS.CostExplorer.Types.AnomalyFeedbackType
import Network.AWS.CostExplorer.Types.AnomalyMonitor
import Network.AWS.CostExplorer.Types.AnomalyScore
import Network.AWS.CostExplorer.Types.AnomalySubscription
import Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
import Network.AWS.CostExplorer.Types.Context
import Network.AWS.CostExplorer.Types.CostCategory
import Network.AWS.CostExplorer.Types.CostCategoryInheritedValueDimension
import Network.AWS.CostExplorer.Types.CostCategoryInheritedValueDimensionName
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import Network.AWS.CostExplorer.Types.CostCategoryReference
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryRuleType
import Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
import Network.AWS.CostExplorer.Types.CostCategorySplitChargeMethod
import Network.AWS.CostExplorer.Types.CostCategorySplitChargeRule
import Network.AWS.CostExplorer.Types.CostCategorySplitChargeRuleParameter
import Network.AWS.CostExplorer.Types.CostCategorySplitChargeRuleParameterType
import Network.AWS.CostExplorer.Types.CostCategoryStatus
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.CoverageByTime
import Network.AWS.CostExplorer.Types.CoverageCost
import Network.AWS.CostExplorer.Types.CoverageHours
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
import Network.AWS.CostExplorer.Types.DiskResourceUtilization
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import Network.AWS.CostExplorer.Types.EC2Specification
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.FindingReasonCode
import Network.AWS.CostExplorer.Types.ForecastResult
import Network.AWS.CostExplorer.Types.Granularity
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.GroupDefinition
import Network.AWS.CostExplorer.Types.GroupDefinitionType
import Network.AWS.CostExplorer.Types.Impact
import Network.AWS.CostExplorer.Types.InstanceDetails
import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.CostExplorer.Types.MatchOption
import Network.AWS.CostExplorer.Types.Metric
import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.MonitorDimension
import Network.AWS.CostExplorer.Types.MonitorType
import Network.AWS.CostExplorer.Types.NetworkResourceUtilization
import Network.AWS.CostExplorer.Types.NumericOperator
import Network.AWS.CostExplorer.Types.OfferingClass
import Network.AWS.CostExplorer.Types.PaymentOption
import Network.AWS.CostExplorer.Types.PlatformDifference
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RecommendationTarget
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.ResultByTime
import Network.AWS.CostExplorer.Types.RightsizingRecommendation
import Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
import Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
import Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
import Network.AWS.CostExplorer.Types.RightsizingType
import Network.AWS.CostExplorer.Types.RootCause
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansCoverage
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import Network.AWS.CostExplorer.Types.SavingsPlansDataType
import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.SortDefinition
import Network.AWS.CostExplorer.Types.SortOrder
import Network.AWS.CostExplorer.Types.Subscriber
import Network.AWS.CostExplorer.Types.SubscriberStatus
import Network.AWS.CostExplorer.Types.SubscriberType
import Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.CostExplorer.Types.TargetInstance
import Network.AWS.CostExplorer.Types.TermInYears
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import Network.AWS.CostExplorer.Types.TotalImpactFilter
import Network.AWS.CostExplorer.Types.UtilizationByTime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | You\'ve reached the limit on the number of resources you can create, or
-- exceeded the size of an individual resource.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Your request parameters changed between pages. Try again with the old
-- parameters or without a pagination token.
_RequestChangedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestChangedException =
  Core._MatchServiceError
    defaultService
    "RequestChangedException"

-- | The cost anomaly subscription does not exist for the account.
_UnknownSubscriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownSubscriptionException =
  Core._MatchServiceError
    defaultService
    "UnknownSubscriptionException"

-- | The cost anomaly monitor does not exist for the account.
_UnknownMonitorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnknownMonitorException =
  Core._MatchServiceError
    defaultService
    "UnknownMonitorException"

-- | The requested data is unavailable.
_DataUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DataUnavailableException =
  Core._MatchServiceError
    defaultService
    "DataUnavailableException"

-- | The pagination token is invalid. Try again without a pagination token.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The requested report expired. Update the date interval and try again.
_BillExpirationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BillExpirationException =
  Core._MatchServiceError
    defaultService
    "BillExpirationException"

-- | Cost Explorer was unable to identify the usage unit. Provide
-- @UsageType\/UsageTypeGroup@ filter selections that contain matching
-- units, for example: @hours@.
_UnresolvableUsageUnitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnresolvableUsageUnitException =
  Core._MatchServiceError
    defaultService
    "UnresolvableUsageUnitException"

-- | You made too many calls in a short period of time. Try again later.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified ARN in the request doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

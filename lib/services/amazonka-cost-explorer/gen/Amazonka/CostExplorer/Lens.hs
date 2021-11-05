{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Lens
  ( -- * Operations

    -- ** GetReservationUtilization
    getReservationUtilization_groupBy,
    getReservationUtilization_nextPageToken,
    getReservationUtilization_granularity,
    getReservationUtilization_filter,
    getReservationUtilization_maxResults,
    getReservationUtilization_sortBy,
    getReservationUtilization_timePeriod,
    getReservationUtilizationResponse_nextPageToken,
    getReservationUtilizationResponse_total,
    getReservationUtilizationResponse_httpStatus,
    getReservationUtilizationResponse_utilizationsByTime,

    -- ** GetSavingsPlansCoverage
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_sortBy,
    getSavingsPlansCoverage_timePeriod,
    getSavingsPlansCoverageResponse_nextToken,
    getSavingsPlansCoverageResponse_httpStatus,
    getSavingsPlansCoverageResponse_savingsPlansCoverages,

    -- ** GetTags
    getTags_nextPageToken,
    getTags_searchString,
    getTags_tagKey,
    getTags_filter,
    getTags_maxResults,
    getTags_sortBy,
    getTags_timePeriod,
    getTagsResponse_nextPageToken,
    getTagsResponse_httpStatus,
    getTagsResponse_tags,
    getTagsResponse_returnSize,
    getTagsResponse_totalSize,

    -- ** GetRightsizingRecommendation
    getRightsizingRecommendation_nextPageToken,
    getRightsizingRecommendation_configuration,
    getRightsizingRecommendation_filter,
    getRightsizingRecommendation_pageSize,
    getRightsizingRecommendation_service,
    getRightsizingRecommendationResponse_summary,
    getRightsizingRecommendationResponse_nextPageToken,
    getRightsizingRecommendationResponse_rightsizingRecommendations,
    getRightsizingRecommendationResponse_metadata,
    getRightsizingRecommendationResponse_configuration,
    getRightsizingRecommendationResponse_httpStatus,

    -- ** GetCostAndUsageWithResources
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_filter,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_httpStatus,

    -- ** GetUsageForecast
    getUsageForecast_predictionIntervalLevel,
    getUsageForecast_filter,
    getUsageForecast_timePeriod,
    getUsageForecast_metric,
    getUsageForecast_granularity,
    getUsageForecastResponse_forecastResultsByTime,
    getUsageForecastResponse_total,
    getUsageForecastResponse_httpStatus,

    -- ** GetReservationCoverage
    getReservationCoverage_groupBy,
    getReservationCoverage_nextPageToken,
    getReservationCoverage_metrics,
    getReservationCoverage_granularity,
    getReservationCoverage_filter,
    getReservationCoverage_maxResults,
    getReservationCoverage_sortBy,
    getReservationCoverage_timePeriod,
    getReservationCoverageResponse_nextPageToken,
    getReservationCoverageResponse_total,
    getReservationCoverageResponse_httpStatus,
    getReservationCoverageResponse_coveragesByTime,

    -- ** GetCostCategories
    getCostCategories_nextPageToken,
    getCostCategories_searchString,
    getCostCategories_costCategoryName,
    getCostCategories_filter,
    getCostCategories_maxResults,
    getCostCategories_sortBy,
    getCostCategories_timePeriod,
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_costCategoryNames,
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_httpStatus,
    getCostCategoriesResponse_returnSize,
    getCostCategoriesResponse_totalSize,

    -- ** GetCostForecast
    getCostForecast_predictionIntervalLevel,
    getCostForecast_filter,
    getCostForecast_timePeriod,
    getCostForecast_metric,
    getCostForecast_granularity,
    getCostForecastResponse_forecastResultsByTime,
    getCostForecastResponse_total,
    getCostForecastResponse_httpStatus,

    -- ** GetDimensionValues
    getDimensionValues_nextPageToken,
    getDimensionValues_context,
    getDimensionValues_searchString,
    getDimensionValues_filter,
    getDimensionValues_maxResults,
    getDimensionValues_sortBy,
    getDimensionValues_timePeriod,
    getDimensionValues_dimension,
    getDimensionValuesResponse_nextPageToken,
    getDimensionValuesResponse_httpStatus,
    getDimensionValuesResponse_dimensionValues,
    getDimensionValuesResponse_returnSize,
    getDimensionValuesResponse_totalSize,

    -- ** GetAnomalies
    getAnomalies_nextPageToken,
    getAnomalies_totalImpact,
    getAnomalies_maxResults,
    getAnomalies_feedback,
    getAnomalies_monitorArn,
    getAnomalies_dateInterval,
    getAnomaliesResponse_nextPageToken,
    getAnomaliesResponse_httpStatus,
    getAnomaliesResponse_anomalies,

    -- ** GetReservationPurchaseRecommendation
    getReservationPurchaseRecommendation_nextPageToken,
    getReservationPurchaseRecommendation_termInYears,
    getReservationPurchaseRecommendation_serviceSpecification,
    getReservationPurchaseRecommendation_accountScope,
    getReservationPurchaseRecommendation_accountId,
    getReservationPurchaseRecommendation_filter,
    getReservationPurchaseRecommendation_pageSize,
    getReservationPurchaseRecommendation_lookbackPeriodInDays,
    getReservationPurchaseRecommendation_paymentOption,
    getReservationPurchaseRecommendation_service,
    getReservationPurchaseRecommendationResponse_nextPageToken,
    getReservationPurchaseRecommendationResponse_recommendations,
    getReservationPurchaseRecommendationResponse_metadata,
    getReservationPurchaseRecommendationResponse_httpStatus,

    -- ** DeleteAnomalyMonitor
    deleteAnomalyMonitor_monitorArn,
    deleteAnomalyMonitorResponse_httpStatus,

    -- ** UpdateAnomalyMonitor
    updateAnomalyMonitor_monitorName,
    updateAnomalyMonitor_monitorArn,
    updateAnomalyMonitorResponse_httpStatus,
    updateAnomalyMonitorResponse_monitorArn,

    -- ** ListCostCategoryDefinitions
    listCostCategoryDefinitions_effectiveOn,
    listCostCategoryDefinitions_nextToken,
    listCostCategoryDefinitions_maxResults,
    listCostCategoryDefinitionsResponse_costCategoryReferences,
    listCostCategoryDefinitionsResponse_nextToken,
    listCostCategoryDefinitionsResponse_httpStatus,

    -- ** UpdateCostCategoryDefinition
    updateCostCategoryDefinition_splitChargeRules,
    updateCostCategoryDefinition_defaultValue,
    updateCostCategoryDefinition_costCategoryArn,
    updateCostCategoryDefinition_ruleVersion,
    updateCostCategoryDefinition_rules,
    updateCostCategoryDefinitionResponse_effectiveStart,
    updateCostCategoryDefinitionResponse_costCategoryArn,
    updateCostCategoryDefinitionResponse_httpStatus,

    -- ** DeleteCostCategoryDefinition
    deleteCostCategoryDefinition_costCategoryArn,
    deleteCostCategoryDefinitionResponse_costCategoryArn,
    deleteCostCategoryDefinitionResponse_effectiveEnd,
    deleteCostCategoryDefinitionResponse_httpStatus,

    -- ** GetAnomalySubscriptions
    getAnomalySubscriptions_subscriptionArnList,
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptions_monitorArn,
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,

    -- ** CreateCostCategoryDefinition
    createCostCategoryDefinition_splitChargeRules,
    createCostCategoryDefinition_defaultValue,
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_httpStatus,

    -- ** GetAnomalyMonitors
    getAnomalyMonitors_nextPageToken,
    getAnomalyMonitors_monitorArnList,
    getAnomalyMonitors_maxResults,
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,

    -- ** DeleteAnomalySubscription
    deleteAnomalySubscription_subscriptionArn,
    deleteAnomalySubscriptionResponse_httpStatus,

    -- ** UpdateAnomalySubscription
    updateAnomalySubscription_subscriptionName,
    updateAnomalySubscription_frequency,
    updateAnomalySubscription_threshold,
    updateAnomalySubscription_monitorArnList,
    updateAnomalySubscription_subscribers,
    updateAnomalySubscription_subscriptionArn,
    updateAnomalySubscriptionResponse_httpStatus,
    updateAnomalySubscriptionResponse_subscriptionArn,

    -- ** GetCostAndUsage
    getCostAndUsage_groupBy,
    getCostAndUsage_nextPageToken,
    getCostAndUsage_filter,
    getCostAndUsage_timePeriod,
    getCostAndUsage_granularity,
    getCostAndUsage_metrics,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_httpStatus,

    -- ** GetSavingsPlansPurchaseRecommendation
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_pageSize,
    getSavingsPlansPurchaseRecommendation_savingsPlansType,
    getSavingsPlansPurchaseRecommendation_termInYears,
    getSavingsPlansPurchaseRecommendation_paymentOption,
    getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    getSavingsPlansPurchaseRecommendationResponse_nextPageToken,
    getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation,
    getSavingsPlansPurchaseRecommendationResponse_metadata,
    getSavingsPlansPurchaseRecommendationResponse_httpStatus,

    -- ** ProvideAnomalyFeedback
    provideAnomalyFeedback_anomalyId,
    provideAnomalyFeedback_feedback,
    provideAnomalyFeedbackResponse_httpStatus,
    provideAnomalyFeedbackResponse_anomalyId,

    -- ** GetSavingsPlansUtilization
    getSavingsPlansUtilization_granularity,
    getSavingsPlansUtilization_filter,
    getSavingsPlansUtilization_sortBy,
    getSavingsPlansUtilization_timePeriod,
    getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime,
    getSavingsPlansUtilizationResponse_httpStatus,
    getSavingsPlansUtilizationResponse_total,

    -- ** DescribeCostCategoryDefinition
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,

    -- ** CreateAnomalySubscription
    createAnomalySubscription_anomalySubscription,
    createAnomalySubscriptionResponse_httpStatus,
    createAnomalySubscriptionResponse_subscriptionArn,

    -- ** CreateAnomalyMonitor
    createAnomalyMonitor_anomalyMonitor,
    createAnomalyMonitorResponse_httpStatus,
    createAnomalyMonitorResponse_monitorArn,

    -- ** GetSavingsPlansUtilizationDetails
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_filter,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_timePeriod,
    getSavingsPlansUtilizationDetailsResponse_nextToken,
    getSavingsPlansUtilizationDetailsResponse_total,
    getSavingsPlansUtilizationDetailsResponse_httpStatus,
    getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails,
    getSavingsPlansUtilizationDetailsResponse_timePeriod,

    -- * Types

    -- ** Anomaly
    anomaly_anomalyStartDate,
    anomaly_dimensionValue,
    anomaly_rootCauses,
    anomaly_anomalyEndDate,
    anomaly_feedback,
    anomaly_anomalyId,
    anomaly_anomalyScore,
    anomaly_impact,
    anomaly_monitorArn,

    -- ** AnomalyDateInterval
    anomalyDateInterval_endDate,
    anomalyDateInterval_startDate,

    -- ** AnomalyMonitor
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_monitorDimension,
    anomalyMonitor_creationDate,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_monitorArn,
    anomalyMonitor_monitorName,
    anomalyMonitor_monitorType,

    -- ** AnomalyScore
    anomalyScore_maxScore,
    anomalyScore_currentScore,

    -- ** AnomalySubscription
    anomalySubscription_accountId,
    anomalySubscription_subscriptionArn,
    anomalySubscription_monitorArnList,
    anomalySubscription_subscribers,
    anomalySubscription_threshold,
    anomalySubscription_frequency,
    anomalySubscription_subscriptionName,

    -- ** CostCategory
    costCategory_processingStatus,
    costCategory_effectiveEnd,
    costCategory_splitChargeRules,
    costCategory_defaultValue,
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- ** CostCategoryInheritedValueDimension
    costCategoryInheritedValueDimension_dimensionName,
    costCategoryInheritedValueDimension_dimensionKey,

    -- ** CostCategoryProcessingStatus
    costCategoryProcessingStatus_status,
    costCategoryProcessingStatus_component,

    -- ** CostCategoryReference
    costCategoryReference_effectiveStart,
    costCategoryReference_values,
    costCategoryReference_costCategoryArn,
    costCategoryReference_processingStatus,
    costCategoryReference_numberOfRules,
    costCategoryReference_name,
    costCategoryReference_effectiveEnd,
    costCategoryReference_defaultValue,

    -- ** CostCategoryRule
    costCategoryRule_inheritedValue,
    costCategoryRule_value,
    costCategoryRule_rule,
    costCategoryRule_type,

    -- ** CostCategorySplitChargeRule
    costCategorySplitChargeRule_parameters,
    costCategorySplitChargeRule_source,
    costCategorySplitChargeRule_targets,
    costCategorySplitChargeRule_method,

    -- ** CostCategorySplitChargeRuleParameter
    costCategorySplitChargeRuleParameter_type,
    costCategorySplitChargeRuleParameter_values,

    -- ** CostCategoryValues
    costCategoryValues_values,
    costCategoryValues_key,
    costCategoryValues_matchOptions,

    -- ** Coverage
    coverage_coverageNormalizedUnits,
    coverage_coverageHours,
    coverage_coverageCost,

    -- ** CoverageByTime
    coverageByTime_groups,
    coverageByTime_timePeriod,
    coverageByTime_total,

    -- ** CoverageCost
    coverageCost_onDemandCost,

    -- ** CoverageHours
    coverageHours_coverageHoursPercentage,
    coverageHours_onDemandHours,
    coverageHours_totalRunningHours,
    coverageHours_reservedHours,

    -- ** CoverageNormalizedUnits
    coverageNormalizedUnits_reservedNormalizedUnits,
    coverageNormalizedUnits_totalRunningNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,

    -- ** CurrentInstance
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

    -- ** DateInterval
    dateInterval_start,
    dateInterval_end,

    -- ** DimensionValues
    dimensionValues_values,
    dimensionValues_key,
    dimensionValues_matchOptions,

    -- ** DimensionValuesWithAttributes
    dimensionValuesWithAttributes_value,
    dimensionValuesWithAttributes_attributes,

    -- ** DiskResourceUtilization
    diskResourceUtilization_diskWriteOpsPerSecond,
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskReadBytesPerSecond,

    -- ** EBSResourceUtilization
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsReadBytesPerSecond,

    -- ** EC2InstanceDetails
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_platform,
    eC2InstanceDetails_family,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_tenancy,
    eC2InstanceDetails_region,

    -- ** EC2ResourceDetails
    eC2ResourceDetails_platform,
    eC2ResourceDetails_vcpu,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_sku,
    eC2ResourceDetails_region,
    eC2ResourceDetails_hourlyOnDemandRate,

    -- ** EC2ResourceUtilization
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_networkResourceUtilization,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_diskResourceUtilization,

    -- ** EC2Specification
    eC2Specification_offeringClass,

    -- ** ESInstanceDetails
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_instanceSize,
    eSInstanceDetails_sizeFlexEligible,
    eSInstanceDetails_region,

    -- ** ElastiCacheInstanceDetails
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_productDescription,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_sizeFlexEligible,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_nodeType,

    -- ** Expression
    expression_not,
    expression_and,
    expression_or,
    expression_costCategories,
    expression_dimensions,
    expression_tags,

    -- ** ForecastResult
    forecastResult_timePeriod,
    forecastResult_meanValue,
    forecastResult_predictionIntervalUpperBound,
    forecastResult_predictionIntervalLowerBound,

    -- ** Group
    group_metrics,
    group_keys,

    -- ** GroupDefinition
    groupDefinition_key,
    groupDefinition_type,

    -- ** Impact
    impact_totalImpact,
    impact_maxImpact,

    -- ** InstanceDetails
    instanceDetails_eSInstanceDetails,
    instanceDetails_rDSInstanceDetails,
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_eC2InstanceDetails,
    instanceDetails_redshiftInstanceDetails,

    -- ** MetricValue
    metricValue_amount,
    metricValue_unit,

    -- ** ModifyRecommendationDetail
    modifyRecommendationDetail_targetInstances,

    -- ** NetworkResourceUtilization
    networkResourceUtilization_networkPacketsOutPerSecond,
    networkResourceUtilization_networkInBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,
    networkResourceUtilization_networkOutBytesPerSecond,

    -- ** RDSInstanceDetails
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_family,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_sizeFlexEligible,
    rDSInstanceDetails_region,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_databaseEdition,

    -- ** RedshiftInstanceDetails
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_sizeFlexEligible,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_nodeType,

    -- ** ReservationAggregates
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

    -- ** ReservationCoverageGroup
    reservationCoverageGroup_coverage,
    reservationCoverageGroup_attributes,

    -- ** ReservationPurchaseRecommendation
    reservationPurchaseRecommendation_termInYears,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_lookbackPeriodInDays,
    reservationPurchaseRecommendation_paymentOption,

    -- ** ReservationPurchaseRecommendationDetail
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

    -- ** ReservationPurchaseRecommendationMetadata
    reservationPurchaseRecommendationMetadata_recommendationId,
    reservationPurchaseRecommendationMetadata_generationTimestamp,

    -- ** ReservationPurchaseRecommendationSummary
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,

    -- ** ReservationUtilizationGroup
    reservationUtilizationGroup_value,
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_utilization,

    -- ** ResourceDetails
    resourceDetails_eC2ResourceDetails,

    -- ** ResourceUtilization
    resourceUtilization_eC2ResourceUtilization,

    -- ** ResultByTime
    resultByTime_groups,
    resultByTime_timePeriod,
    resultByTime_total,
    resultByTime_estimated,

    -- ** RightsizingRecommendation
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_modifyRecommendationDetail,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_rightsizingType,
    rightsizingRecommendation_terminateRecommendationDetail,

    -- ** RightsizingRecommendationConfiguration
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    rightsizingRecommendationMetadata_recommendationId,
    rightsizingRecommendationMetadata_generationTimestamp,
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,

    -- ** RightsizingRecommendationSummary
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_savingsCurrencyCode,
    rightsizingRecommendationSummary_totalRecommendationCount,
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,

    -- ** RootCause
    rootCause_service,
    rootCause_usageType,
    rootCause_linkedAccount,
    rootCause_region,

    -- ** SavingsPlansAmortizedCommitment
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,

    -- ** SavingsPlansCoverage
    savingsPlansCoverage_timePeriod,
    savingsPlansCoverage_coverage,
    savingsPlansCoverage_attributes,

    -- ** SavingsPlansCoverageData
    savingsPlansCoverageData_onDemandCost,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_totalCost,

    -- ** SavingsPlansDetails
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,
    savingsPlansDetails_region,

    -- ** SavingsPlansPurchaseRecommendation
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_termInYears,
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,

    -- ** SavingsPlansPurchaseRecommendationDetail
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

    -- ** SavingsPlansPurchaseRecommendationMetadata
    savingsPlansPurchaseRecommendationMetadata_recommendationId,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,

    -- ** SavingsPlansPurchaseRecommendationSummary
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

    -- ** SavingsPlansSavings
    savingsPlansSavings_netSavings,
    savingsPlansSavings_onDemandCostEquivalent,

    -- ** SavingsPlansUtilization
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_utilizationPercentage,
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_usedCommitment,

    -- ** SavingsPlansUtilizationAggregates
    savingsPlansUtilizationAggregates_amortizedCommitment,
    savingsPlansUtilizationAggregates_savings,
    savingsPlansUtilizationAggregates_utilization,

    -- ** SavingsPlansUtilizationByTime
    savingsPlansUtilizationByTime_amortizedCommitment,
    savingsPlansUtilizationByTime_savings,
    savingsPlansUtilizationByTime_timePeriod,
    savingsPlansUtilizationByTime_utilization,

    -- ** SavingsPlansUtilizationDetail
    savingsPlansUtilizationDetail_amortizedCommitment,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_utilization,
    savingsPlansUtilizationDetail_savingsPlanArn,

    -- ** ServiceSpecification
    serviceSpecification_eC2Specification,

    -- ** SortDefinition
    sortDefinition_sortOrder,
    sortDefinition_key,

    -- ** Subscriber
    subscriber_status,
    subscriber_address,
    subscriber_type,

    -- ** TagValues
    tagValues_values,
    tagValues_key,
    tagValues_matchOptions,

    -- ** TargetInstance
    targetInstance_currencyCode,
    targetInstance_resourceDetails,
    targetInstance_platformDifferences,
    targetInstance_defaultTargetInstance,
    targetInstance_estimatedMonthlyCost,
    targetInstance_estimatedMonthlySavings,
    targetInstance_expectedResourceUtilization,

    -- ** TerminateRecommendationDetail
    terminateRecommendationDetail_currencyCode,
    terminateRecommendationDetail_estimatedMonthlySavings,

    -- ** TotalImpactFilter
    totalImpactFilter_endValue,
    totalImpactFilter_numericOperator,
    totalImpactFilter_startValue,

    -- ** UtilizationByTime
    utilizationByTime_groups,
    utilizationByTime_timePeriod,
    utilizationByTime_total,
  )
where

import Amazonka.CostExplorer.CreateAnomalyMonitor
import Amazonka.CostExplorer.CreateAnomalySubscription
import Amazonka.CostExplorer.CreateCostCategoryDefinition
import Amazonka.CostExplorer.DeleteAnomalyMonitor
import Amazonka.CostExplorer.DeleteAnomalySubscription
import Amazonka.CostExplorer.DeleteCostCategoryDefinition
import Amazonka.CostExplorer.DescribeCostCategoryDefinition
import Amazonka.CostExplorer.GetAnomalies
import Amazonka.CostExplorer.GetAnomalyMonitors
import Amazonka.CostExplorer.GetAnomalySubscriptions
import Amazonka.CostExplorer.GetCostAndUsage
import Amazonka.CostExplorer.GetCostAndUsageWithResources
import Amazonka.CostExplorer.GetCostCategories
import Amazonka.CostExplorer.GetCostForecast
import Amazonka.CostExplorer.GetDimensionValues
import Amazonka.CostExplorer.GetReservationCoverage
import Amazonka.CostExplorer.GetReservationPurchaseRecommendation
import Amazonka.CostExplorer.GetReservationUtilization
import Amazonka.CostExplorer.GetRightsizingRecommendation
import Amazonka.CostExplorer.GetSavingsPlansCoverage
import Amazonka.CostExplorer.GetSavingsPlansPurchaseRecommendation
import Amazonka.CostExplorer.GetSavingsPlansUtilization
import Amazonka.CostExplorer.GetSavingsPlansUtilizationDetails
import Amazonka.CostExplorer.GetTags
import Amazonka.CostExplorer.GetUsageForecast
import Amazonka.CostExplorer.ListCostCategoryDefinitions
import Amazonka.CostExplorer.ProvideAnomalyFeedback
import Amazonka.CostExplorer.Types.Anomaly
import Amazonka.CostExplorer.Types.AnomalyDateInterval
import Amazonka.CostExplorer.Types.AnomalyMonitor
import Amazonka.CostExplorer.Types.AnomalyScore
import Amazonka.CostExplorer.Types.AnomalySubscription
import Amazonka.CostExplorer.Types.CostCategory
import Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimension
import Amazonka.CostExplorer.Types.CostCategoryProcessingStatus
import Amazonka.CostExplorer.Types.CostCategoryReference
import Amazonka.CostExplorer.Types.CostCategoryRule
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRule
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameter
import Amazonka.CostExplorer.Types.CostCategoryValues
import Amazonka.CostExplorer.Types.Coverage
import Amazonka.CostExplorer.Types.CoverageByTime
import Amazonka.CostExplorer.Types.CoverageCost
import Amazonka.CostExplorer.Types.CoverageHours
import Amazonka.CostExplorer.Types.CoverageNormalizedUnits
import Amazonka.CostExplorer.Types.CurrentInstance
import Amazonka.CostExplorer.Types.DateInterval
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
import Amazonka.CostExplorer.Types.ForecastResult
import Amazonka.CostExplorer.Types.Group
import Amazonka.CostExplorer.Types.GroupDefinition
import Amazonka.CostExplorer.Types.Impact
import Amazonka.CostExplorer.Types.InstanceDetails
import Amazonka.CostExplorer.Types.MetricValue
import Amazonka.CostExplorer.Types.ModifyRecommendationDetail
import Amazonka.CostExplorer.Types.NetworkResourceUtilization
import Amazonka.CostExplorer.Types.RDSInstanceDetails
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
import Amazonka.CostExplorer.Types.RootCause
import Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Amazonka.CostExplorer.Types.SavingsPlansCoverage
import Amazonka.CostExplorer.Types.SavingsPlansCoverageData
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
import Amazonka.CostExplorer.Types.Subscriber
import Amazonka.CostExplorer.Types.TagValues
import Amazonka.CostExplorer.Types.TargetInstance
import Amazonka.CostExplorer.Types.TerminateRecommendationDetail
import Amazonka.CostExplorer.Types.TotalImpactFilter
import Amazonka.CostExplorer.Types.UtilizationByTime
import Amazonka.CostExplorer.UpdateAnomalyMonitor
import Amazonka.CostExplorer.UpdateAnomalySubscription
import Amazonka.CostExplorer.UpdateCostCategoryDefinition

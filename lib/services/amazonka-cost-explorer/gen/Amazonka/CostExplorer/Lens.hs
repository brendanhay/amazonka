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

    -- ** CreateAnomalyMonitor
    createAnomalyMonitor_resourceTags,
    createAnomalyMonitor_anomalyMonitor,
    createAnomalyMonitorResponse_httpStatus,
    createAnomalyMonitorResponse_monitorArn,

    -- ** CreateAnomalySubscription
    createAnomalySubscription_resourceTags,
    createAnomalySubscription_anomalySubscription,
    createAnomalySubscriptionResponse_httpStatus,
    createAnomalySubscriptionResponse_subscriptionArn,

    -- ** CreateCostCategoryDefinition
    createCostCategoryDefinition_splitChargeRules,
    createCostCategoryDefinition_defaultValue,
    createCostCategoryDefinition_resourceTags,
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_httpStatus,

    -- ** DeleteAnomalyMonitor
    deleteAnomalyMonitor_monitorArn,
    deleteAnomalyMonitorResponse_httpStatus,

    -- ** DeleteAnomalySubscription
    deleteAnomalySubscription_subscriptionArn,
    deleteAnomalySubscriptionResponse_httpStatus,

    -- ** DeleteCostCategoryDefinition
    deleteCostCategoryDefinition_costCategoryArn,
    deleteCostCategoryDefinitionResponse_effectiveEnd,
    deleteCostCategoryDefinitionResponse_costCategoryArn,
    deleteCostCategoryDefinitionResponse_httpStatus,

    -- ** DescribeCostCategoryDefinition
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,

    -- ** GetAnomalies
    getAnomalies_nextPageToken,
    getAnomalies_totalImpact,
    getAnomalies_monitorArn,
    getAnomalies_feedback,
    getAnomalies_maxResults,
    getAnomalies_dateInterval,
    getAnomaliesResponse_nextPageToken,
    getAnomaliesResponse_httpStatus,
    getAnomaliesResponse_anomalies,

    -- ** GetAnomalyMonitors
    getAnomalyMonitors_nextPageToken,
    getAnomalyMonitors_monitorArnList,
    getAnomalyMonitors_maxResults,
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,

    -- ** GetAnomalySubscriptions
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_subscriptionArnList,
    getAnomalySubscriptions_monitorArn,
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,

    -- ** GetCostAndUsage
    getCostAndUsage_nextPageToken,
    getCostAndUsage_groupBy,
    getCostAndUsage_filter,
    getCostAndUsage_timePeriod,
    getCostAndUsage_granularity,
    getCostAndUsage_metrics,
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_httpStatus,

    -- ** GetCostAndUsageWithResources
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_filter,
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_httpStatus,

    -- ** GetCostCategories
    getCostCategories_nextPageToken,
    getCostCategories_sortBy,
    getCostCategories_searchString,
    getCostCategories_filter,
    getCostCategories_maxResults,
    getCostCategories_costCategoryName,
    getCostCategories_timePeriod,
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_costCategoryNames,
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
    getDimensionValues_sortBy,
    getDimensionValues_searchString,
    getDimensionValues_filter,
    getDimensionValues_maxResults,
    getDimensionValues_timePeriod,
    getDimensionValues_dimension,
    getDimensionValuesResponse_nextPageToken,
    getDimensionValuesResponse_httpStatus,
    getDimensionValuesResponse_dimensionValues,
    getDimensionValuesResponse_returnSize,
    getDimensionValuesResponse_totalSize,

    -- ** GetReservationCoverage
    getReservationCoverage_nextPageToken,
    getReservationCoverage_granularity,
    getReservationCoverage_groupBy,
    getReservationCoverage_metrics,
    getReservationCoverage_sortBy,
    getReservationCoverage_filter,
    getReservationCoverage_maxResults,
    getReservationCoverage_timePeriod,
    getReservationCoverageResponse_nextPageToken,
    getReservationCoverageResponse_total,
    getReservationCoverageResponse_httpStatus,
    getReservationCoverageResponse_coveragesByTime,

    -- ** GetReservationPurchaseRecommendation
    getReservationPurchaseRecommendation_nextPageToken,
    getReservationPurchaseRecommendation_serviceSpecification,
    getReservationPurchaseRecommendation_lookbackPeriodInDays,
    getReservationPurchaseRecommendation_filter,
    getReservationPurchaseRecommendation_pageSize,
    getReservationPurchaseRecommendation_accountId,
    getReservationPurchaseRecommendation_termInYears,
    getReservationPurchaseRecommendation_paymentOption,
    getReservationPurchaseRecommendation_accountScope,
    getReservationPurchaseRecommendation_service,
    getReservationPurchaseRecommendationResponse_nextPageToken,
    getReservationPurchaseRecommendationResponse_metadata,
    getReservationPurchaseRecommendationResponse_recommendations,
    getReservationPurchaseRecommendationResponse_httpStatus,

    -- ** GetReservationUtilization
    getReservationUtilization_nextPageToken,
    getReservationUtilization_granularity,
    getReservationUtilization_groupBy,
    getReservationUtilization_sortBy,
    getReservationUtilization_filter,
    getReservationUtilization_maxResults,
    getReservationUtilization_timePeriod,
    getReservationUtilizationResponse_nextPageToken,
    getReservationUtilizationResponse_total,
    getReservationUtilizationResponse_httpStatus,
    getReservationUtilizationResponse_utilizationsByTime,

    -- ** GetRightsizingRecommendation
    getRightsizingRecommendation_nextPageToken,
    getRightsizingRecommendation_configuration,
    getRightsizingRecommendation_filter,
    getRightsizingRecommendation_pageSize,
    getRightsizingRecommendation_service,
    getRightsizingRecommendationResponse_nextPageToken,
    getRightsizingRecommendationResponse_metadata,
    getRightsizingRecommendationResponse_configuration,
    getRightsizingRecommendationResponse_summary,
    getRightsizingRecommendationResponse_rightsizingRecommendations,
    getRightsizingRecommendationResponse_httpStatus,

    -- ** GetSavingsPlansCoverage
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_sortBy,
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_timePeriod,
    getSavingsPlansCoverageResponse_nextToken,
    getSavingsPlansCoverageResponse_httpStatus,
    getSavingsPlansCoverageResponse_savingsPlansCoverages,

    -- ** GetSavingsPlansPurchaseRecommendation
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_pageSize,
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_savingsPlansType,
    getSavingsPlansPurchaseRecommendation_termInYears,
    getSavingsPlansPurchaseRecommendation_paymentOption,
    getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    getSavingsPlansPurchaseRecommendationResponse_nextPageToken,
    getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation,
    getSavingsPlansPurchaseRecommendationResponse_metadata,
    getSavingsPlansPurchaseRecommendationResponse_httpStatus,

    -- ** GetSavingsPlansUtilization
    getSavingsPlansUtilization_granularity,
    getSavingsPlansUtilization_sortBy,
    getSavingsPlansUtilization_filter,
    getSavingsPlansUtilization_timePeriod,
    getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime,
    getSavingsPlansUtilizationResponse_httpStatus,
    getSavingsPlansUtilizationResponse_total,

    -- ** GetSavingsPlansUtilizationDetails
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_filter,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_timePeriod,
    getSavingsPlansUtilizationDetailsResponse_nextToken,
    getSavingsPlansUtilizationDetailsResponse_total,
    getSavingsPlansUtilizationDetailsResponse_httpStatus,
    getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails,
    getSavingsPlansUtilizationDetailsResponse_timePeriod,

    -- ** GetTags
    getTags_nextPageToken,
    getTags_sortBy,
    getTags_searchString,
    getTags_filter,
    getTags_tagKey,
    getTags_maxResults,
    getTags_timePeriod,
    getTagsResponse_nextPageToken,
    getTagsResponse_httpStatus,
    getTagsResponse_tags,
    getTagsResponse_returnSize,
    getTagsResponse_totalSize,

    -- ** GetUsageForecast
    getUsageForecast_predictionIntervalLevel,
    getUsageForecast_filter,
    getUsageForecast_timePeriod,
    getUsageForecast_metric,
    getUsageForecast_granularity,
    getUsageForecastResponse_forecastResultsByTime,
    getUsageForecastResponse_total,
    getUsageForecastResponse_httpStatus,

    -- ** ListCostAllocationTags
    listCostAllocationTags_tagKeys,
    listCostAllocationTags_nextToken,
    listCostAllocationTags_type,
    listCostAllocationTags_status,
    listCostAllocationTags_maxResults,
    listCostAllocationTagsResponse_nextToken,
    listCostAllocationTagsResponse_costAllocationTags,
    listCostAllocationTagsResponse_httpStatus,

    -- ** ListCostCategoryDefinitions
    listCostCategoryDefinitions_effectiveOn,
    listCostCategoryDefinitions_nextToken,
    listCostCategoryDefinitions_maxResults,
    listCostCategoryDefinitionsResponse_nextToken,
    listCostCategoryDefinitionsResponse_costCategoryReferences,
    listCostCategoryDefinitionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- ** ProvideAnomalyFeedback
    provideAnomalyFeedback_anomalyId,
    provideAnomalyFeedback_feedback,
    provideAnomalyFeedbackResponse_httpStatus,
    provideAnomalyFeedbackResponse_anomalyId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_resourceTags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_resourceTagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAnomalyMonitor
    updateAnomalyMonitor_monitorName,
    updateAnomalyMonitor_monitorArn,
    updateAnomalyMonitorResponse_httpStatus,
    updateAnomalyMonitorResponse_monitorArn,

    -- ** UpdateAnomalySubscription
    updateAnomalySubscription_monitorArnList,
    updateAnomalySubscription_frequency,
    updateAnomalySubscription_subscriptionName,
    updateAnomalySubscription_subscribers,
    updateAnomalySubscription_threshold,
    updateAnomalySubscription_subscriptionArn,
    updateAnomalySubscriptionResponse_httpStatus,
    updateAnomalySubscriptionResponse_subscriptionArn,

    -- ** UpdateCostAllocationTagsStatus
    updateCostAllocationTagsStatus_costAllocationTagsStatus,
    updateCostAllocationTagsStatusResponse_errors,
    updateCostAllocationTagsStatusResponse_httpStatus,

    -- ** UpdateCostCategoryDefinition
    updateCostCategoryDefinition_splitChargeRules,
    updateCostCategoryDefinition_defaultValue,
    updateCostCategoryDefinition_costCategoryArn,
    updateCostCategoryDefinition_ruleVersion,
    updateCostCategoryDefinition_rules,
    updateCostCategoryDefinitionResponse_effectiveStart,
    updateCostCategoryDefinitionResponse_costCategoryArn,
    updateCostCategoryDefinitionResponse_httpStatus,

    -- * Types

    -- ** Anomaly
    anomaly_dimensionValue,
    anomaly_anomalyEndDate,
    anomaly_feedback,
    anomaly_anomalyStartDate,
    anomaly_rootCauses,
    anomaly_anomalyId,
    anomaly_anomalyScore,
    anomaly_impact,
    anomaly_monitorArn,

    -- ** AnomalyDateInterval
    anomalyDateInterval_endDate,
    anomalyDateInterval_startDate,

    -- ** AnomalyMonitor
    anomalyMonitor_monitorDimension,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_monitorArn,
    anomalyMonitor_creationDate,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_monitorName,
    anomalyMonitor_monitorType,

    -- ** AnomalyScore
    anomalyScore_maxScore,
    anomalyScore_currentScore,

    -- ** AnomalySubscription
    anomalySubscription_subscriptionArn,
    anomalySubscription_accountId,
    anomalySubscription_monitorArnList,
    anomalySubscription_subscribers,
    anomalySubscription_threshold,
    anomalySubscription_frequency,
    anomalySubscription_subscriptionName,

    -- ** CostAllocationTag
    costAllocationTag_tagKey,
    costAllocationTag_type,
    costAllocationTag_status,

    -- ** CostAllocationTagStatusEntry
    costAllocationTagStatusEntry_tagKey,
    costAllocationTagStatusEntry_status,

    -- ** CostCategory
    costCategory_splitChargeRules,
    costCategory_effectiveEnd,
    costCategory_defaultValue,
    costCategory_processingStatus,
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- ** CostCategoryInheritedValueDimension
    costCategoryInheritedValueDimension_dimensionKey,
    costCategoryInheritedValueDimension_dimensionName,

    -- ** CostCategoryProcessingStatus
    costCategoryProcessingStatus_status,
    costCategoryProcessingStatus_component,

    -- ** CostCategoryReference
    costCategoryReference_name,
    costCategoryReference_effectiveEnd,
    costCategoryReference_defaultValue,
    costCategoryReference_processingStatus,
    costCategoryReference_effectiveStart,
    costCategoryReference_values,
    costCategoryReference_costCategoryArn,
    costCategoryReference_numberOfRules,

    -- ** CostCategoryRule
    costCategoryRule_type,
    costCategoryRule_inheritedValue,
    costCategoryRule_rule,
    costCategoryRule_value,

    -- ** CostCategorySplitChargeRule
    costCategorySplitChargeRule_parameters,
    costCategorySplitChargeRule_source,
    costCategorySplitChargeRule_targets,
    costCategorySplitChargeRule_method,

    -- ** CostCategorySplitChargeRuleParameter
    costCategorySplitChargeRuleParameter_type,
    costCategorySplitChargeRuleParameter_values,

    -- ** CostCategoryValues
    costCategoryValues_key,
    costCategoryValues_matchOptions,
    costCategoryValues_values,

    -- ** Coverage
    coverage_coverageNormalizedUnits,
    coverage_coverageHours,
    coverage_coverageCost,

    -- ** CoverageByTime
    coverageByTime_total,
    coverageByTime_timePeriod,
    coverageByTime_groups,

    -- ** CoverageCost
    coverageCost_onDemandCost,

    -- ** CoverageHours
    coverageHours_onDemandHours,
    coverageHours_totalRunningHours,
    coverageHours_reservedHours,
    coverageHours_coverageHoursPercentage,

    -- ** CoverageNormalizedUnits
    coverageNormalizedUnits_totalRunningNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,

    -- ** CurrentInstance
    currentInstance_resourceId,
    currentInstance_tags,
    currentInstance_savingsPlansCoveredHoursInLookbackPeriod,
    currentInstance_instanceName,
    currentInstance_reservationCoveredHoursInLookbackPeriod,
    currentInstance_totalRunningHoursInLookbackPeriod,
    currentInstance_resourceDetails,
    currentInstance_currencyCode,
    currentInstance_resourceUtilization,
    currentInstance_monthlyCost,
    currentInstance_onDemandHoursInLookbackPeriod,

    -- ** DateInterval
    dateInterval_start,
    dateInterval_end,

    -- ** DimensionValues
    dimensionValues_key,
    dimensionValues_matchOptions,
    dimensionValues_values,

    -- ** DimensionValuesWithAttributes
    dimensionValuesWithAttributes_attributes,
    dimensionValuesWithAttributes_value,

    -- ** DiskResourceUtilization
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskWriteOpsPerSecond,
    diskResourceUtilization_diskReadBytesPerSecond,

    -- ** EBSResourceUtilization
    eBSResourceUtilization_ebsWriteOpsPerSecond,
    eBSResourceUtilization_ebsReadBytesPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsWriteBytesPerSecond,

    -- ** EC2InstanceDetails
    eC2InstanceDetails_platform,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_region,
    eC2InstanceDetails_family,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_tenancy,

    -- ** EC2ResourceDetails
    eC2ResourceDetails_hourlyOnDemandRate,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_vcpu,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_platform,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_region,
    eC2ResourceDetails_sku,

    -- ** EC2ResourceUtilization
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_diskResourceUtilization,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_networkResourceUtilization,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,

    -- ** EC2Specification
    eC2Specification_offeringClass,

    -- ** ESInstanceDetails
    eSInstanceDetails_sizeFlexEligible,
    eSInstanceDetails_region,
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_instanceSize,

    -- ** ElastiCacheInstanceDetails
    elastiCacheInstanceDetails_sizeFlexEligible,
    elastiCacheInstanceDetails_nodeType,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_productDescription,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_currentGeneration,

    -- ** Expression
    expression_tags,
    expression_costCategories,
    expression_dimensions,
    expression_or,
    expression_not,
    expression_and,

    -- ** ForecastResult
    forecastResult_predictionIntervalLowerBound,
    forecastResult_predictionIntervalUpperBound,
    forecastResult_meanValue,
    forecastResult_timePeriod,

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
    instanceDetails_eC2InstanceDetails,
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_rDSInstanceDetails,
    instanceDetails_redshiftInstanceDetails,
    instanceDetails_eSInstanceDetails,

    -- ** MetricValue
    metricValue_unit,
    metricValue_amount,

    -- ** ModifyRecommendationDetail
    modifyRecommendationDetail_targetInstances,

    -- ** NetworkResourceUtilization
    networkResourceUtilization_networkOutBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,
    networkResourceUtilization_networkPacketsOutPerSecond,
    networkResourceUtilization_networkInBytesPerSecond,

    -- ** RDSInstanceDetails
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_sizeFlexEligible,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_region,
    rDSInstanceDetails_family,
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_databaseEdition,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_licenseModel,

    -- ** RedshiftInstanceDetails
    redshiftInstanceDetails_sizeFlexEligible,
    redshiftInstanceDetails_nodeType,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_currentGeneration,

    -- ** ReservationAggregates
    reservationAggregates_totalAmortizedFee,
    reservationAggregates_onDemandCostOfRIHoursUsed,
    reservationAggregates_totalActualHours,
    reservationAggregates_amortizedRecurringFee,
    reservationAggregates_amortizedUpfrontFee,
    reservationAggregates_totalActualUnits,
    reservationAggregates_purchasedHours,
    reservationAggregates_netRISavings,
    reservationAggregates_rICostForUnusedHours,
    reservationAggregates_unusedUnits,
    reservationAggregates_unusedHours,
    reservationAggregates_unrealizedSavings,
    reservationAggregates_totalPotentialRISavings,
    reservationAggregates_utilizationPercentageInUnits,
    reservationAggregates_purchasedUnits,
    reservationAggregates_utilizationPercentage,
    reservationAggregates_realizedSavings,

    -- ** ReservationCoverageGroup
    reservationCoverageGroup_coverage,
    reservationCoverageGroup_attributes,

    -- ** ReservationPurchaseRecommendation
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_lookbackPeriodInDays,
    reservationPurchaseRecommendation_termInYears,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_paymentOption,
    reservationPurchaseRecommendation_accountScope,

    -- ** ReservationPurchaseRecommendationDetail
    reservationPurchaseRecommendationDetail_recommendedNumberOfInstancesToPurchase,
    reservationPurchaseRecommendationDetail_maximumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationDetail_minimumNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost,
    reservationPurchaseRecommendationDetail_instanceDetails,
    reservationPurchaseRecommendationDetail_upfrontCost,
    reservationPurchaseRecommendationDetail_estimatedMonthlyOnDemandCost,
    reservationPurchaseRecommendationDetail_averageUtilization,
    reservationPurchaseRecommendationDetail_estimatedBreakEvenInMonths,
    reservationPurchaseRecommendationDetail_currencyCode,
    reservationPurchaseRecommendationDetail_accountId,
    reservationPurchaseRecommendationDetail_maximumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod,
    reservationPurchaseRecommendationDetail_averageNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase,
    reservationPurchaseRecommendationDetail_minimumNormalizedUnitsUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsPercentage,

    -- ** ReservationPurchaseRecommendationMetadata
    reservationPurchaseRecommendationMetadata_recommendationId,
    reservationPurchaseRecommendationMetadata_generationTimestamp,

    -- ** ReservationPurchaseRecommendationSummary
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,

    -- ** ReservationUtilizationGroup
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_utilization,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_value,

    -- ** ResourceDetails
    resourceDetails_eC2ResourceDetails,

    -- ** ResourceTag
    resourceTag_key,
    resourceTag_value,

    -- ** ResourceUtilization
    resourceUtilization_eC2ResourceUtilization,

    -- ** ResultByTime
    resultByTime_total,
    resultByTime_estimated,
    resultByTime_timePeriod,
    resultByTime_groups,

    -- ** RightsizingRecommendation
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_terminateRecommendationDetail,
    rightsizingRecommendation_modifyRecommendationDetail,
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_rightsizingType,

    -- ** RightsizingRecommendationConfiguration
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    rightsizingRecommendationMetadata_recommendationId,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_generationTimestamp,

    -- ** RightsizingRecommendationSummary
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_savingsCurrencyCode,
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,
    rightsizingRecommendationSummary_totalRecommendationCount,

    -- ** RootCause
    rootCause_usageType,
    rootCause_service,
    rootCause_region,
    rootCause_linkedAccount,

    -- ** SavingsPlansAmortizedCommitment
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,

    -- ** SavingsPlansCoverage
    savingsPlansCoverage_coverage,
    savingsPlansCoverage_timePeriod,
    savingsPlansCoverage_attributes,

    -- ** SavingsPlansCoverageData
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_totalCost,
    savingsPlansCoverageData_onDemandCost,

    -- ** SavingsPlansDetails
    savingsPlansDetails_region,
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,

    -- ** SavingsPlansPurchaseRecommendation
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendation_termInYears,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_accountScope,

    -- ** SavingsPlansPurchaseRecommendationDetail
    savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationDetail_upfrontCost,
    savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization,
    savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_currencyCode,
    savingsPlansPurchaseRecommendationDetail_accountId,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost,
    savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedSPCost,
    savingsPlansPurchaseRecommendationDetail_savingsPlansDetails,
    savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedROI,

    -- ** SavingsPlansPurchaseRecommendationMetadata
    savingsPlansPurchaseRecommendationMetadata_recommendationId,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,

    -- ** SavingsPlansPurchaseRecommendationSummary
    savingsPlansPurchaseRecommendationSummary_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationSummary_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationSummary_dailyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationSummary_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationSummary_estimatedTotalCost,
    savingsPlansPurchaseRecommendationSummary_currentOnDemandSpend,
    savingsPlansPurchaseRecommendationSummary_currencyCode,
    savingsPlansPurchaseRecommendationSummary_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationSummary_estimatedROI,
    savingsPlansPurchaseRecommendationSummary_totalRecommendationCount,

    -- ** SavingsPlansSavings
    savingsPlansSavings_netSavings,
    savingsPlansSavings_onDemandCostEquivalent,

    -- ** SavingsPlansUtilization
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_usedCommitment,
    savingsPlansUtilization_utilizationPercentage,

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
    savingsPlansUtilizationDetail_utilization,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_savingsPlanArn,
    savingsPlansUtilizationDetail_attributes,

    -- ** ServiceSpecification
    serviceSpecification_eC2Specification,

    -- ** SortDefinition
    sortDefinition_sortOrder,
    sortDefinition_key,

    -- ** Subscriber
    subscriber_type,
    subscriber_status,
    subscriber_address,

    -- ** TagValues
    tagValues_key,
    tagValues_matchOptions,
    tagValues_values,

    -- ** TargetInstance
    targetInstance_estimatedMonthlyCost,
    targetInstance_platformDifferences,
    targetInstance_expectedResourceUtilization,
    targetInstance_defaultTargetInstance,
    targetInstance_estimatedMonthlySavings,
    targetInstance_resourceDetails,
    targetInstance_currencyCode,

    -- ** TerminateRecommendationDetail
    terminateRecommendationDetail_estimatedMonthlySavings,
    terminateRecommendationDetail_currencyCode,

    -- ** TotalImpactFilter
    totalImpactFilter_endValue,
    totalImpactFilter_numericOperator,
    totalImpactFilter_startValue,

    -- ** UpdateCostAllocationTagsStatusError
    updateCostAllocationTagsStatusError_message,
    updateCostAllocationTagsStatusError_code,
    updateCostAllocationTagsStatusError_tagKey,

    -- ** UtilizationByTime
    utilizationByTime_total,
    utilizationByTime_timePeriod,
    utilizationByTime_groups,
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
import Amazonka.CostExplorer.ListCostAllocationTags
import Amazonka.CostExplorer.ListCostCategoryDefinitions
import Amazonka.CostExplorer.ListTagsForResource
import Amazonka.CostExplorer.ProvideAnomalyFeedback
import Amazonka.CostExplorer.TagResource
import Amazonka.CostExplorer.Types.Anomaly
import Amazonka.CostExplorer.Types.AnomalyDateInterval
import Amazonka.CostExplorer.Types.AnomalyMonitor
import Amazonka.CostExplorer.Types.AnomalyScore
import Amazonka.CostExplorer.Types.AnomalySubscription
import Amazonka.CostExplorer.Types.CostAllocationTag
import Amazonka.CostExplorer.Types.CostAllocationTagStatusEntry
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
import Amazonka.CostExplorer.Types.ResourceTag
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
import Amazonka.CostExplorer.Types.UpdateCostAllocationTagsStatusError
import Amazonka.CostExplorer.Types.UtilizationByTime
import Amazonka.CostExplorer.UntagResource
import Amazonka.CostExplorer.UpdateAnomalyMonitor
import Amazonka.CostExplorer.UpdateAnomalySubscription
import Amazonka.CostExplorer.UpdateCostAllocationTagsStatus
import Amazonka.CostExplorer.UpdateCostCategoryDefinition

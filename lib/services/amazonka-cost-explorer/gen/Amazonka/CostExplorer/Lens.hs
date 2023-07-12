{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createCostCategoryDefinition_defaultValue,
    createCostCategoryDefinition_effectiveStart,
    createCostCategoryDefinition_resourceTags,
    createCostCategoryDefinition_splitChargeRules,
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_httpStatus,

    -- ** DeleteAnomalyMonitor
    deleteAnomalyMonitor_monitorArn,
    deleteAnomalyMonitorResponse_httpStatus,

    -- ** DeleteAnomalySubscription
    deleteAnomalySubscription_subscriptionArn,
    deleteAnomalySubscriptionResponse_httpStatus,

    -- ** DeleteCostCategoryDefinition
    deleteCostCategoryDefinition_costCategoryArn,
    deleteCostCategoryDefinitionResponse_costCategoryArn,
    deleteCostCategoryDefinitionResponse_effectiveEnd,
    deleteCostCategoryDefinitionResponse_httpStatus,

    -- ** DescribeCostCategoryDefinition
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,

    -- ** GetAnomalies
    getAnomalies_feedback,
    getAnomalies_maxResults,
    getAnomalies_monitorArn,
    getAnomalies_nextPageToken,
    getAnomalies_totalImpact,
    getAnomalies_dateInterval,
    getAnomaliesResponse_nextPageToken,
    getAnomaliesResponse_httpStatus,
    getAnomaliesResponse_anomalies,

    -- ** GetAnomalyMonitors
    getAnomalyMonitors_maxResults,
    getAnomalyMonitors_monitorArnList,
    getAnomalyMonitors_nextPageToken,
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,

    -- ** GetAnomalySubscriptions
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptions_monitorArn,
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_subscriptionArnList,
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,

    -- ** GetCostAndUsage
    getCostAndUsage_filter,
    getCostAndUsage_groupBy,
    getCostAndUsage_nextPageToken,
    getCostAndUsage_timePeriod,
    getCostAndUsage_granularity,
    getCostAndUsage_metrics,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_httpStatus,

    -- ** GetCostAndUsageWithResources
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_filter,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_httpStatus,

    -- ** GetCostCategories
    getCostCategories_costCategoryName,
    getCostCategories_filter,
    getCostCategories_maxResults,
    getCostCategories_nextPageToken,
    getCostCategories_searchString,
    getCostCategories_sortBy,
    getCostCategories_timePeriod,
    getCostCategoriesResponse_costCategoryNames,
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_httpStatus,
    getCostCategoriesResponse_returnSize,
    getCostCategoriesResponse_totalSize,

    -- ** GetCostForecast
    getCostForecast_filter,
    getCostForecast_predictionIntervalLevel,
    getCostForecast_timePeriod,
    getCostForecast_metric,
    getCostForecast_granularity,
    getCostForecastResponse_forecastResultsByTime,
    getCostForecastResponse_total,
    getCostForecastResponse_httpStatus,

    -- ** GetDimensionValues
    getDimensionValues_context,
    getDimensionValues_filter,
    getDimensionValues_maxResults,
    getDimensionValues_nextPageToken,
    getDimensionValues_searchString,
    getDimensionValues_sortBy,
    getDimensionValues_timePeriod,
    getDimensionValues_dimension,
    getDimensionValuesResponse_nextPageToken,
    getDimensionValuesResponse_httpStatus,
    getDimensionValuesResponse_dimensionValues,
    getDimensionValuesResponse_returnSize,
    getDimensionValuesResponse_totalSize,

    -- ** GetReservationCoverage
    getReservationCoverage_filter,
    getReservationCoverage_granularity,
    getReservationCoverage_groupBy,
    getReservationCoverage_maxResults,
    getReservationCoverage_metrics,
    getReservationCoverage_nextPageToken,
    getReservationCoverage_sortBy,
    getReservationCoverage_timePeriod,
    getReservationCoverageResponse_nextPageToken,
    getReservationCoverageResponse_total,
    getReservationCoverageResponse_httpStatus,
    getReservationCoverageResponse_coveragesByTime,

    -- ** GetReservationPurchaseRecommendation
    getReservationPurchaseRecommendation_accountId,
    getReservationPurchaseRecommendation_accountScope,
    getReservationPurchaseRecommendation_filter,
    getReservationPurchaseRecommendation_lookbackPeriodInDays,
    getReservationPurchaseRecommendation_nextPageToken,
    getReservationPurchaseRecommendation_pageSize,
    getReservationPurchaseRecommendation_paymentOption,
    getReservationPurchaseRecommendation_serviceSpecification,
    getReservationPurchaseRecommendation_termInYears,
    getReservationPurchaseRecommendation_service,
    getReservationPurchaseRecommendationResponse_metadata,
    getReservationPurchaseRecommendationResponse_nextPageToken,
    getReservationPurchaseRecommendationResponse_recommendations,
    getReservationPurchaseRecommendationResponse_httpStatus,

    -- ** GetReservationUtilization
    getReservationUtilization_filter,
    getReservationUtilization_granularity,
    getReservationUtilization_groupBy,
    getReservationUtilization_maxResults,
    getReservationUtilization_nextPageToken,
    getReservationUtilization_sortBy,
    getReservationUtilization_timePeriod,
    getReservationUtilizationResponse_nextPageToken,
    getReservationUtilizationResponse_total,
    getReservationUtilizationResponse_httpStatus,
    getReservationUtilizationResponse_utilizationsByTime,

    -- ** GetRightsizingRecommendation
    getRightsizingRecommendation_configuration,
    getRightsizingRecommendation_filter,
    getRightsizingRecommendation_nextPageToken,
    getRightsizingRecommendation_pageSize,
    getRightsizingRecommendation_service,
    getRightsizingRecommendationResponse_configuration,
    getRightsizingRecommendationResponse_metadata,
    getRightsizingRecommendationResponse_nextPageToken,
    getRightsizingRecommendationResponse_rightsizingRecommendations,
    getRightsizingRecommendationResponse_summary,
    getRightsizingRecommendationResponse_httpStatus,

    -- ** GetSavingsPlansCoverage
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_sortBy,
    getSavingsPlansCoverage_timePeriod,
    getSavingsPlansCoverageResponse_nextToken,
    getSavingsPlansCoverageResponse_httpStatus,
    getSavingsPlansCoverageResponse_savingsPlansCoverages,

    -- ** GetSavingsPlansPurchaseRecommendation
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_pageSize,
    getSavingsPlansPurchaseRecommendation_savingsPlansType,
    getSavingsPlansPurchaseRecommendation_termInYears,
    getSavingsPlansPurchaseRecommendation_paymentOption,
    getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    getSavingsPlansPurchaseRecommendationResponse_metadata,
    getSavingsPlansPurchaseRecommendationResponse_nextPageToken,
    getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation,
    getSavingsPlansPurchaseRecommendationResponse_httpStatus,

    -- ** GetSavingsPlansUtilization
    getSavingsPlansUtilization_filter,
    getSavingsPlansUtilization_granularity,
    getSavingsPlansUtilization_sortBy,
    getSavingsPlansUtilization_timePeriod,
    getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime,
    getSavingsPlansUtilizationResponse_httpStatus,
    getSavingsPlansUtilizationResponse_total,

    -- ** GetSavingsPlansUtilizationDetails
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_filter,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_timePeriod,
    getSavingsPlansUtilizationDetailsResponse_nextToken,
    getSavingsPlansUtilizationDetailsResponse_total,
    getSavingsPlansUtilizationDetailsResponse_httpStatus,
    getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails,
    getSavingsPlansUtilizationDetailsResponse_timePeriod,

    -- ** GetTags
    getTags_filter,
    getTags_maxResults,
    getTags_nextPageToken,
    getTags_searchString,
    getTags_sortBy,
    getTags_tagKey,
    getTags_timePeriod,
    getTagsResponse_nextPageToken,
    getTagsResponse_httpStatus,
    getTagsResponse_tags,
    getTagsResponse_returnSize,
    getTagsResponse_totalSize,

    -- ** GetUsageForecast
    getUsageForecast_filter,
    getUsageForecast_predictionIntervalLevel,
    getUsageForecast_timePeriod,
    getUsageForecast_metric,
    getUsageForecast_granularity,
    getUsageForecastResponse_forecastResultsByTime,
    getUsageForecastResponse_total,
    getUsageForecastResponse_httpStatus,

    -- ** ListCostAllocationTags
    listCostAllocationTags_maxResults,
    listCostAllocationTags_nextToken,
    listCostAllocationTags_status,
    listCostAllocationTags_tagKeys,
    listCostAllocationTags_type,
    listCostAllocationTagsResponse_costAllocationTags,
    listCostAllocationTagsResponse_nextToken,
    listCostAllocationTagsResponse_httpStatus,

    -- ** ListCostCategoryDefinitions
    listCostCategoryDefinitions_effectiveOn,
    listCostCategoryDefinitions_maxResults,
    listCostCategoryDefinitions_nextToken,
    listCostCategoryDefinitionsResponse_costCategoryReferences,
    listCostCategoryDefinitionsResponse_nextToken,
    listCostCategoryDefinitionsResponse_httpStatus,

    -- ** ListSavingsPlansPurchaseRecommendationGeneration
    listSavingsPlansPurchaseRecommendationGeneration_generationStatus,
    listSavingsPlansPurchaseRecommendationGeneration_nextPageToken,
    listSavingsPlansPurchaseRecommendationGeneration_pageSize,
    listSavingsPlansPurchaseRecommendationGeneration_recommendationIds,
    listSavingsPlansPurchaseRecommendationGenerationResponse_generationSummaryList,
    listSavingsPlansPurchaseRecommendationGenerationResponse_nextPageToken,
    listSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,

    -- ** ProvideAnomalyFeedback
    provideAnomalyFeedback_anomalyId,
    provideAnomalyFeedback_feedback,
    provideAnomalyFeedbackResponse_httpStatus,
    provideAnomalyFeedbackResponse_anomalyId,

    -- ** StartSavingsPlansPurchaseRecommendationGeneration
    startSavingsPlansPurchaseRecommendationGenerationResponse_estimatedCompletionTime,
    startSavingsPlansPurchaseRecommendationGenerationResponse_generationStartedTime,
    startSavingsPlansPurchaseRecommendationGenerationResponse_recommendationId,
    startSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus,

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
    updateAnomalySubscription_frequency,
    updateAnomalySubscription_monitorArnList,
    updateAnomalySubscription_subscribers,
    updateAnomalySubscription_subscriptionName,
    updateAnomalySubscription_threshold,
    updateAnomalySubscription_thresholdExpression,
    updateAnomalySubscription_subscriptionArn,
    updateAnomalySubscriptionResponse_httpStatus,
    updateAnomalySubscriptionResponse_subscriptionArn,

    -- ** UpdateCostAllocationTagsStatus
    updateCostAllocationTagsStatus_costAllocationTagsStatus,
    updateCostAllocationTagsStatusResponse_errors,
    updateCostAllocationTagsStatusResponse_httpStatus,

    -- ** UpdateCostCategoryDefinition
    updateCostCategoryDefinition_defaultValue,
    updateCostCategoryDefinition_effectiveStart,
    updateCostCategoryDefinition_splitChargeRules,
    updateCostCategoryDefinition_costCategoryArn,
    updateCostCategoryDefinition_ruleVersion,
    updateCostCategoryDefinition_rules,
    updateCostCategoryDefinitionResponse_costCategoryArn,
    updateCostCategoryDefinitionResponse_effectiveStart,
    updateCostCategoryDefinitionResponse_httpStatus,

    -- * Types

    -- ** Anomaly
    anomaly_anomalyEndDate,
    anomaly_anomalyStartDate,
    anomaly_dimensionValue,
    anomaly_feedback,
    anomaly_rootCauses,
    anomaly_anomalyId,
    anomaly_anomalyScore,
    anomaly_impact,
    anomaly_monitorArn,

    -- ** AnomalyDateInterval
    anomalyDateInterval_endDate,
    anomalyDateInterval_startDate,

    -- ** AnomalyMonitor
    anomalyMonitor_creationDate,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_monitorArn,
    anomalyMonitor_monitorDimension,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_monitorName,
    anomalyMonitor_monitorType,

    -- ** AnomalyScore
    anomalyScore_maxScore,
    anomalyScore_currentScore,

    -- ** AnomalySubscription
    anomalySubscription_accountId,
    anomalySubscription_subscriptionArn,
    anomalySubscription_threshold,
    anomalySubscription_thresholdExpression,
    anomalySubscription_monitorArnList,
    anomalySubscription_subscribers,
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
    costCategory_defaultValue,
    costCategory_effectiveEnd,
    costCategory_processingStatus,
    costCategory_splitChargeRules,
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- ** CostCategoryInheritedValueDimension
    costCategoryInheritedValueDimension_dimensionKey,
    costCategoryInheritedValueDimension_dimensionName,

    -- ** CostCategoryProcessingStatus
    costCategoryProcessingStatus_component,
    costCategoryProcessingStatus_status,

    -- ** CostCategoryReference
    costCategoryReference_costCategoryArn,
    costCategoryReference_defaultValue,
    costCategoryReference_effectiveEnd,
    costCategoryReference_effectiveStart,
    costCategoryReference_name,
    costCategoryReference_numberOfRules,
    costCategoryReference_processingStatus,
    costCategoryReference_values,

    -- ** CostCategoryRule
    costCategoryRule_inheritedValue,
    costCategoryRule_rule,
    costCategoryRule_type,
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
    coverage_coverageCost,
    coverage_coverageHours,
    coverage_coverageNormalizedUnits,

    -- ** CoverageByTime
    coverageByTime_groups,
    coverageByTime_timePeriod,
    coverageByTime_total,

    -- ** CoverageCost
    coverageCost_onDemandCost,

    -- ** CoverageHours
    coverageHours_coverageHoursPercentage,
    coverageHours_onDemandHours,
    coverageHours_reservedHours,
    coverageHours_totalRunningHours,

    -- ** CoverageNormalizedUnits
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_onDemandNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,
    coverageNormalizedUnits_totalRunningNormalizedUnits,

    -- ** CurrentInstance
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
    diskResourceUtilization_diskReadBytesPerSecond,
    diskResourceUtilization_diskReadOpsPerSecond,
    diskResourceUtilization_diskWriteBytesPerSecond,
    diskResourceUtilization_diskWriteOpsPerSecond,

    -- ** EBSResourceUtilization
    eBSResourceUtilization_ebsReadBytesPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,

    -- ** EC2InstanceDetails
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_family,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_platform,
    eC2InstanceDetails_region,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_tenancy,

    -- ** EC2ResourceDetails
    eC2ResourceDetails_hourlyOnDemandRate,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_platform,
    eC2ResourceDetails_region,
    eC2ResourceDetails_sku,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_vcpu,

    -- ** EC2ResourceUtilization
    eC2ResourceUtilization_diskResourceUtilization,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_maxCpuUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_networkResourceUtilization,

    -- ** EC2Specification
    eC2Specification_offeringClass,

    -- ** ESInstanceDetails
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_instanceSize,
    eSInstanceDetails_region,
    eSInstanceDetails_sizeFlexEligible,

    -- ** ElastiCacheInstanceDetails
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_nodeType,
    elastiCacheInstanceDetails_productDescription,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_sizeFlexEligible,

    -- ** Expression
    expression_and,
    expression_costCategories,
    expression_dimensions,
    expression_not,
    expression_or,
    expression_tags,

    -- ** ForecastResult
    forecastResult_meanValue,
    forecastResult_predictionIntervalLowerBound,
    forecastResult_predictionIntervalUpperBound,
    forecastResult_timePeriod,

    -- ** GenerationSummary
    generationSummary_estimatedCompletionTime,
    generationSummary_generationCompletionTime,
    generationSummary_generationStartedTime,
    generationSummary_generationStatus,
    generationSummary_recommendationId,

    -- ** Group
    group_keys,
    group_metrics,

    -- ** GroupDefinition
    groupDefinition_key,
    groupDefinition_type,

    -- ** Impact
    impact_totalActualSpend,
    impact_totalExpectedSpend,
    impact_totalImpact,
    impact_totalImpactPercentage,
    impact_maxImpact,

    -- ** InstanceDetails
    instanceDetails_eC2InstanceDetails,
    instanceDetails_eSInstanceDetails,
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_rDSInstanceDetails,
    instanceDetails_redshiftInstanceDetails,

    -- ** MetricValue
    metricValue_amount,
    metricValue_unit,

    -- ** ModifyRecommendationDetail
    modifyRecommendationDetail_targetInstances,

    -- ** NetworkResourceUtilization
    networkResourceUtilization_networkInBytesPerSecond,
    networkResourceUtilization_networkOutBytesPerSecond,
    networkResourceUtilization_networkPacketsInPerSecond,
    networkResourceUtilization_networkPacketsOutPerSecond,

    -- ** RDSInstanceDetails
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_databaseEdition,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_family,
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_region,
    rDSInstanceDetails_sizeFlexEligible,

    -- ** RedshiftInstanceDetails
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_nodeType,
    redshiftInstanceDetails_region,
    redshiftInstanceDetails_sizeFlexEligible,

    -- ** ReservationAggregates
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

    -- ** ReservationCoverageGroup
    reservationCoverageGroup_attributes,
    reservationCoverageGroup_coverage,

    -- ** ReservationPurchaseRecommendation
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_lookbackPeriodInDays,
    reservationPurchaseRecommendation_paymentOption,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_termInYears,

    -- ** ReservationPurchaseRecommendationDetail
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

    -- ** ReservationPurchaseRecommendationMetadata
    reservationPurchaseRecommendationMetadata_generationTimestamp,
    reservationPurchaseRecommendationMetadata_recommendationId,

    -- ** ReservationPurchaseRecommendationSummary
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,

    -- ** ReservationUtilizationGroup
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_utilization,
    reservationUtilizationGroup_value,

    -- ** ResourceDetails
    resourceDetails_eC2ResourceDetails,

    -- ** ResourceTag
    resourceTag_key,
    resourceTag_value,

    -- ** ResourceUtilization
    resourceUtilization_eC2ResourceUtilization,

    -- ** ResultByTime
    resultByTime_estimated,
    resultByTime_groups,
    resultByTime_timePeriod,
    resultByTime_total,

    -- ** RightsizingRecommendation
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_findingReasonCodes,
    rightsizingRecommendation_modifyRecommendationDetail,
    rightsizingRecommendation_rightsizingType,
    rightsizingRecommendation_terminateRecommendationDetail,

    -- ** RightsizingRecommendationConfiguration
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_generationTimestamp,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,
    rightsizingRecommendationMetadata_recommendationId,

    -- ** RightsizingRecommendationSummary
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,
    rightsizingRecommendationSummary_savingsCurrencyCode,
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_totalRecommendationCount,

    -- ** RootCause
    rootCause_linkedAccount,
    rootCause_linkedAccountName,
    rootCause_region,
    rootCause_service,
    rootCause_usageType,

    -- ** SavingsPlansAmortizedCommitment
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,

    -- ** SavingsPlansCoverage
    savingsPlansCoverage_attributes,
    savingsPlansCoverage_coverage,
    savingsPlansCoverage_timePeriod,

    -- ** SavingsPlansCoverageData
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_onDemandCost,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_totalCost,

    -- ** SavingsPlansDetails
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,
    savingsPlansDetails_region,

    -- ** SavingsPlansPurchaseRecommendation
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_termInYears,

    -- ** SavingsPlansPurchaseRecommendationDetail
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

    -- ** SavingsPlansPurchaseRecommendationMetadata
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,
    savingsPlansPurchaseRecommendationMetadata_recommendationId,

    -- ** SavingsPlansPurchaseRecommendationSummary
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
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_savingsPlanArn,
    savingsPlansUtilizationDetail_utilization,

    -- ** ServiceSpecification
    serviceSpecification_eC2Specification,

    -- ** SortDefinition
    sortDefinition_sortOrder,
    sortDefinition_key,

    -- ** Subscriber
    subscriber_address,
    subscriber_status,
    subscriber_type,

    -- ** TagValues
    tagValues_key,
    tagValues_matchOptions,
    tagValues_values,

    -- ** TargetInstance
    targetInstance_currencyCode,
    targetInstance_defaultTargetInstance,
    targetInstance_estimatedMonthlyCost,
    targetInstance_estimatedMonthlySavings,
    targetInstance_expectedResourceUtilization,
    targetInstance_platformDifferences,
    targetInstance_resourceDetails,

    -- ** TerminateRecommendationDetail
    terminateRecommendationDetail_currencyCode,
    terminateRecommendationDetail_estimatedMonthlySavings,

    -- ** TotalImpactFilter
    totalImpactFilter_endValue,
    totalImpactFilter_numericOperator,
    totalImpactFilter_startValue,

    -- ** UpdateCostAllocationTagsStatusError
    updateCostAllocationTagsStatusError_code,
    updateCostAllocationTagsStatusError_message,
    updateCostAllocationTagsStatusError_tagKey,

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
import Amazonka.CostExplorer.ListCostAllocationTags
import Amazonka.CostExplorer.ListCostCategoryDefinitions
import Amazonka.CostExplorer.ListSavingsPlansPurchaseRecommendationGeneration
import Amazonka.CostExplorer.ListTagsForResource
import Amazonka.CostExplorer.ProvideAnomalyFeedback
import Amazonka.CostExplorer.StartSavingsPlansPurchaseRecommendationGeneration
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
import Amazonka.CostExplorer.Types.GenerationSummary
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

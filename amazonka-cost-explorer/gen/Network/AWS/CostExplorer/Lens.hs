{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Lens
  ( -- * Operations

    -- ** ListCostCategoryDefinitions
    listCostCategoryDefinitions_nextToken,
    listCostCategoryDefinitions_maxResults,
    listCostCategoryDefinitions_effectiveOn,
    listCostCategoryDefinitionsResponse_nextToken,
    listCostCategoryDefinitionsResponse_costCategoryReferences,
    listCostCategoryDefinitionsResponse_httpStatus,

    -- ** GetRightsizingRecommendation
    getRightsizingRecommendation_pageSize,
    getRightsizingRecommendation_configuration,
    getRightsizingRecommendation_nextPageToken,
    getRightsizingRecommendation_filter,
    getRightsizingRecommendation_service,
    getRightsizingRecommendationResponse_configuration,
    getRightsizingRecommendationResponse_metadata,
    getRightsizingRecommendationResponse_nextPageToken,
    getRightsizingRecommendationResponse_summary,
    getRightsizingRecommendationResponse_rightsizingRecommendations,
    getRightsizingRecommendationResponse_httpStatus,

    -- ** GetAnomalySubscriptions
    getAnomalySubscriptions_maxResults,
    getAnomalySubscriptions_nextPageToken,
    getAnomalySubscriptions_subscriptionArnList,
    getAnomalySubscriptions_monitorArn,
    getAnomalySubscriptionsResponse_nextPageToken,
    getAnomalySubscriptionsResponse_httpStatus,
    getAnomalySubscriptionsResponse_anomalySubscriptions,

    -- ** GetAnomalies
    getAnomalies_maxResults,
    getAnomalies_nextPageToken,
    getAnomalies_monitorArn,
    getAnomalies_feedback,
    getAnomalies_totalImpact,
    getAnomalies_dateInterval,
    getAnomaliesResponse_nextPageToken,
    getAnomaliesResponse_httpStatus,
    getAnomaliesResponse_anomalies,

    -- ** GetSavingsPlansUtilizationDetails
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_filter,
    getSavingsPlansUtilizationDetails_timePeriod,
    getSavingsPlansUtilizationDetailsResponse_nextToken,
    getSavingsPlansUtilizationDetailsResponse_total,
    getSavingsPlansUtilizationDetailsResponse_httpStatus,
    getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails,
    getSavingsPlansUtilizationDetailsResponse_timePeriod,

    -- ** GetCostForecast
    getCostForecast_predictionIntervalLevel,
    getCostForecast_filter,
    getCostForecast_timePeriod,
    getCostForecast_metric,
    getCostForecast_granularity,
    getCostForecastResponse_forecastResultsByTime,
    getCostForecastResponse_total,
    getCostForecastResponse_httpStatus,

    -- ** GetCostAndUsage
    getCostAndUsage_granularity,
    getCostAndUsage_nextPageToken,
    getCostAndUsage_groupBy,
    getCostAndUsage_filter,
    getCostAndUsage_timePeriod,
    getCostAndUsage_metrics,
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_httpStatus,

    -- ** GetSavingsPlansPurchaseRecommendation
    getSavingsPlansPurchaseRecommendation_pageSize,
    getSavingsPlansPurchaseRecommendation_accountScope,
    getSavingsPlansPurchaseRecommendation_nextPageToken,
    getSavingsPlansPurchaseRecommendation_filter,
    getSavingsPlansPurchaseRecommendation_savingsPlansType,
    getSavingsPlansPurchaseRecommendation_termInYears,
    getSavingsPlansPurchaseRecommendation_paymentOption,
    getSavingsPlansPurchaseRecommendation_lookbackPeriodInDays,
    getSavingsPlansPurchaseRecommendationResponse_metadata,
    getSavingsPlansPurchaseRecommendationResponse_nextPageToken,
    getSavingsPlansPurchaseRecommendationResponse_savingsPlansPurchaseRecommendation,
    getSavingsPlansPurchaseRecommendationResponse_httpStatus,

    -- ** UpdateAnomalySubscription
    updateAnomalySubscription_threshold,
    updateAnomalySubscription_subscriptionName,
    updateAnomalySubscription_subscribers,
    updateAnomalySubscription_frequency,
    updateAnomalySubscription_monitorArnList,
    updateAnomalySubscription_subscriptionArn,
    updateAnomalySubscriptionResponse_httpStatus,
    updateAnomalySubscriptionResponse_subscriptionArn,

    -- ** DeleteAnomalySubscription
    deleteAnomalySubscription_subscriptionArn,
    deleteAnomalySubscriptionResponse_httpStatus,

    -- ** GetReservationCoverage
    getReservationCoverage_granularity,
    getReservationCoverage_maxResults,
    getReservationCoverage_nextPageToken,
    getReservationCoverage_metrics,
    getReservationCoverage_groupBy,
    getReservationCoverage_sortBy,
    getReservationCoverage_filter,
    getReservationCoverage_timePeriod,
    getReservationCoverageResponse_total,
    getReservationCoverageResponse_nextPageToken,
    getReservationCoverageResponse_httpStatus,
    getReservationCoverageResponse_coveragesByTime,

    -- ** GetUsageForecast
    getUsageForecast_predictionIntervalLevel,
    getUsageForecast_filter,
    getUsageForecast_timePeriod,
    getUsageForecast_metric,
    getUsageForecast_granularity,
    getUsageForecastResponse_forecastResultsByTime,
    getUsageForecastResponse_total,
    getUsageForecastResponse_httpStatus,

    -- ** GetCostAndUsageWithResources
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_filter,
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_httpStatus,

    -- ** GetTags
    getTags_maxResults,
    getTags_searchString,
    getTags_tagKey,
    getTags_nextPageToken,
    getTags_sortBy,
    getTags_filter,
    getTags_timePeriod,
    getTagsResponse_nextPageToken,
    getTagsResponse_httpStatus,
    getTagsResponse_tags,
    getTagsResponse_returnSize,
    getTagsResponse_totalSize,

    -- ** DeleteCostCategoryDefinition
    deleteCostCategoryDefinition_costCategoryArn,
    deleteCostCategoryDefinitionResponse_costCategoryArn,
    deleteCostCategoryDefinitionResponse_effectiveEnd,
    deleteCostCategoryDefinitionResponse_httpStatus,

    -- ** UpdateCostCategoryDefinition
    updateCostCategoryDefinition_costCategoryArn,
    updateCostCategoryDefinition_ruleVersion,
    updateCostCategoryDefinition_rules,
    updateCostCategoryDefinitionResponse_costCategoryArn,
    updateCostCategoryDefinitionResponse_effectiveStart,
    updateCostCategoryDefinitionResponse_httpStatus,

    -- ** GetSavingsPlansCoverage
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_sortBy,
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_timePeriod,
    getSavingsPlansCoverageResponse_nextToken,
    getSavingsPlansCoverageResponse_httpStatus,
    getSavingsPlansCoverageResponse_savingsPlansCoverages,

    -- ** DeleteAnomalyMonitor
    deleteAnomalyMonitor_monitorArn,
    deleteAnomalyMonitorResponse_httpStatus,

    -- ** GetReservationUtilization
    getReservationUtilization_granularity,
    getReservationUtilization_maxResults,
    getReservationUtilization_nextPageToken,
    getReservationUtilization_groupBy,
    getReservationUtilization_sortBy,
    getReservationUtilization_filter,
    getReservationUtilization_timePeriod,
    getReservationUtilizationResponse_total,
    getReservationUtilizationResponse_nextPageToken,
    getReservationUtilizationResponse_httpStatus,
    getReservationUtilizationResponse_utilizationsByTime,

    -- ** GetReservationPurchaseRecommendation
    getReservationPurchaseRecommendation_accountId,
    getReservationPurchaseRecommendation_paymentOption,
    getReservationPurchaseRecommendation_pageSize,
    getReservationPurchaseRecommendation_accountScope,
    getReservationPurchaseRecommendation_serviceSpecification,
    getReservationPurchaseRecommendation_termInYears,
    getReservationPurchaseRecommendation_nextPageToken,
    getReservationPurchaseRecommendation_lookbackPeriodInDays,
    getReservationPurchaseRecommendation_filter,
    getReservationPurchaseRecommendation_service,
    getReservationPurchaseRecommendationResponse_metadata,
    getReservationPurchaseRecommendationResponse_recommendations,
    getReservationPurchaseRecommendationResponse_nextPageToken,
    getReservationPurchaseRecommendationResponse_httpStatus,

    -- ** UpdateAnomalyMonitor
    updateAnomalyMonitor_monitorName,
    updateAnomalyMonitor_monitorArn,
    updateAnomalyMonitorResponse_httpStatus,
    updateAnomalyMonitorResponse_monitorArn,

    -- ** CreateAnomalyMonitor
    createAnomalyMonitor_anomalyMonitor,
    createAnomalyMonitorResponse_httpStatus,
    createAnomalyMonitorResponse_monitorArn,

    -- ** GetDimensionValues
    getDimensionValues_maxResults,
    getDimensionValues_searchString,
    getDimensionValues_nextPageToken,
    getDimensionValues_context,
    getDimensionValues_sortBy,
    getDimensionValues_filter,
    getDimensionValues_timePeriod,
    getDimensionValues_dimension,
    getDimensionValuesResponse_nextPageToken,
    getDimensionValuesResponse_httpStatus,
    getDimensionValuesResponse_dimensionValues,
    getDimensionValuesResponse_returnSize,
    getDimensionValuesResponse_totalSize,

    -- ** CreateAnomalySubscription
    createAnomalySubscription_anomalySubscription,
    createAnomalySubscriptionResponse_httpStatus,
    createAnomalySubscriptionResponse_subscriptionArn,

    -- ** DescribeCostCategoryDefinition
    describeCostCategoryDefinition_effectiveOn,
    describeCostCategoryDefinition_costCategoryArn,
    describeCostCategoryDefinitionResponse_costCategory,
    describeCostCategoryDefinitionResponse_httpStatus,

    -- ** GetCostCategories
    getCostCategories_maxResults,
    getCostCategories_searchString,
    getCostCategories_nextPageToken,
    getCostCategories_costCategoryName,
    getCostCategories_sortBy,
    getCostCategories_filter,
    getCostCategories_timePeriod,
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_costCategoryNames,
    getCostCategoriesResponse_httpStatus,
    getCostCategoriesResponse_returnSize,
    getCostCategoriesResponse_totalSize,

    -- ** GetSavingsPlansUtilization
    getSavingsPlansUtilization_granularity,
    getSavingsPlansUtilization_sortBy,
    getSavingsPlansUtilization_filter,
    getSavingsPlansUtilization_timePeriod,
    getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime,
    getSavingsPlansUtilizationResponse_httpStatus,
    getSavingsPlansUtilizationResponse_total,

    -- ** ProvideAnomalyFeedback
    provideAnomalyFeedback_anomalyId,
    provideAnomalyFeedback_feedback,
    provideAnomalyFeedbackResponse_httpStatus,
    provideAnomalyFeedbackResponse_anomalyId,

    -- ** GetAnomalyMonitors
    getAnomalyMonitors_maxResults,
    getAnomalyMonitors_nextPageToken,
    getAnomalyMonitors_monitorArnList,
    getAnomalyMonitorsResponse_nextPageToken,
    getAnomalyMonitorsResponse_httpStatus,
    getAnomalyMonitorsResponse_anomalyMonitors,

    -- ** CreateCostCategoryDefinition
    createCostCategoryDefinition_name,
    createCostCategoryDefinition_ruleVersion,
    createCostCategoryDefinition_rules,
    createCostCategoryDefinitionResponse_costCategoryArn,
    createCostCategoryDefinitionResponse_effectiveStart,
    createCostCategoryDefinitionResponse_httpStatus,

    -- * Types

    -- ** Anomaly
    anomaly_dimensionValue,
    anomaly_rootCauses,
    anomaly_feedback,
    anomaly_anomalyStartDate,
    anomaly_anomalyEndDate,
    anomaly_anomalyId,
    anomaly_anomalyScore,
    anomaly_impact,
    anomaly_monitorArn,

    -- ** AnomalyDateInterval
    anomalyDateInterval_endDate,
    anomalyDateInterval_startDate,

    -- ** AnomalyMonitor
    anomalyMonitor_lastEvaluatedDate,
    anomalyMonitor_monitorSpecification,
    anomalyMonitor_lastUpdatedDate,
    anomalyMonitor_creationDate,
    anomalyMonitor_dimensionalValueCount,
    anomalyMonitor_monitorDimension,
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
    costCategory_costCategoryArn,
    costCategory_effectiveStart,
    costCategory_name,
    costCategory_ruleVersion,
    costCategory_rules,

    -- ** CostCategoryProcessingStatus
    costCategoryProcessingStatus_status,
    costCategoryProcessingStatus_component,

    -- ** CostCategoryReference
    costCategoryReference_numberOfRules,
    costCategoryReference_costCategoryArn,
    costCategoryReference_values,
    costCategoryReference_processingStatus,
    costCategoryReference_name,
    costCategoryReference_effectiveStart,
    costCategoryReference_effectiveEnd,

    -- ** CostCategoryRule
    costCategoryRule_value,
    costCategoryRule_rule,

    -- ** CostCategoryValues
    costCategoryValues_key,
    costCategoryValues_values,
    costCategoryValues_matchOptions,

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
    coverageHours_reservedHours,
    coverageHours_totalRunningHours,
    coverageHours_onDemandHours,
    coverageHours_coverageHoursPercentage,

    -- ** CoverageNormalizedUnits
    coverageNormalizedUnits_onDemandNormalizedUnits,
    coverageNormalizedUnits_coverageNormalizedUnitsPercentage,
    coverageNormalizedUnits_totalRunningNormalizedUnits,
    coverageNormalizedUnits_reservedNormalizedUnits,

    -- ** CurrentInstance
    currentInstance_resourceId,
    currentInstance_instanceName,
    currentInstance_savingsPlansCoveredHoursInLookbackPeriod,
    currentInstance_onDemandHoursInLookbackPeriod,
    currentInstance_currencyCode,
    currentInstance_tags,
    currentInstance_reservationCoveredHoursInLookbackPeriod,
    currentInstance_monthlyCost,
    currentInstance_resourceUtilization,
    currentInstance_resourceDetails,
    currentInstance_totalRunningHoursInLookbackPeriod,

    -- ** DateInterval
    dateInterval_start,
    dateInterval_end,

    -- ** DimensionValues
    dimensionValues_key,
    dimensionValues_values,
    dimensionValues_matchOptions,

    -- ** DimensionValuesWithAttributes
    dimensionValuesWithAttributes_attributes,
    dimensionValuesWithAttributes_value,

    -- ** EBSResourceUtilization
    eBSResourceUtilization_ebsWriteBytesPerSecond,
    eBSResourceUtilization_ebsReadOpsPerSecond,
    eBSResourceUtilization_ebsWriteOpsPerSecond,
    eBSResourceUtilization_ebsReadBytesPerSecond,

    -- ** EC2InstanceDetails
    eC2InstanceDetails_platform,
    eC2InstanceDetails_instanceType,
    eC2InstanceDetails_tenancy,
    eC2InstanceDetails_currentGeneration,
    eC2InstanceDetails_sizeFlexEligible,
    eC2InstanceDetails_availabilityZone,
    eC2InstanceDetails_family,
    eC2InstanceDetails_region,

    -- ** EC2ResourceDetails
    eC2ResourceDetails_platform,
    eC2ResourceDetails_instanceType,
    eC2ResourceDetails_memory,
    eC2ResourceDetails_vcpu,
    eC2ResourceDetails_hourlyOnDemandRate,
    eC2ResourceDetails_storage,
    eC2ResourceDetails_networkPerformance,
    eC2ResourceDetails_region,
    eC2ResourceDetails_sku,

    -- ** EC2ResourceUtilization
    eC2ResourceUtilization_maxStorageUtilizationPercentage,
    eC2ResourceUtilization_maxMemoryUtilizationPercentage,
    eC2ResourceUtilization_eBSResourceUtilization,
    eC2ResourceUtilization_maxCpuUtilizationPercentage,

    -- ** EC2Specification
    eC2Specification_offeringClass,

    -- ** ESInstanceDetails
    eSInstanceDetails_instanceClass,
    eSInstanceDetails_currentGeneration,
    eSInstanceDetails_sizeFlexEligible,
    eSInstanceDetails_instanceSize,
    eSInstanceDetails_region,

    -- ** ElastiCacheInstanceDetails
    elastiCacheInstanceDetails_currentGeneration,
    elastiCacheInstanceDetails_sizeFlexEligible,
    elastiCacheInstanceDetails_family,
    elastiCacheInstanceDetails_nodeType,
    elastiCacheInstanceDetails_region,
    elastiCacheInstanceDetails_productDescription,

    -- ** Expression
    expression_not,
    expression_or,
    expression_costCategories,
    expression_tags,
    expression_and,
    expression_dimensions,

    -- ** ForecastResult
    forecastResult_meanValue,
    forecastResult_timePeriod,
    forecastResult_predictionIntervalLowerBound,
    forecastResult_predictionIntervalUpperBound,

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
    instanceDetails_elastiCacheInstanceDetails,
    instanceDetails_redshiftInstanceDetails,
    instanceDetails_eSInstanceDetails,
    instanceDetails_eC2InstanceDetails,
    instanceDetails_rDSInstanceDetails,

    -- ** MetricValue
    metricValue_amount,
    metricValue_unit,

    -- ** ModifyRecommendationDetail
    modifyRecommendationDetail_targetInstances,

    -- ** RDSInstanceDetails
    rDSInstanceDetails_instanceType,
    rDSInstanceDetails_databaseEdition,
    rDSInstanceDetails_deploymentOption,
    rDSInstanceDetails_currentGeneration,
    rDSInstanceDetails_sizeFlexEligible,
    rDSInstanceDetails_licenseModel,
    rDSInstanceDetails_family,
    rDSInstanceDetails_databaseEngine,
    rDSInstanceDetails_region,

    -- ** RedshiftInstanceDetails
    redshiftInstanceDetails_currentGeneration,
    redshiftInstanceDetails_sizeFlexEligible,
    redshiftInstanceDetails_family,
    redshiftInstanceDetails_nodeType,
    redshiftInstanceDetails_region,

    -- ** ReservationAggregates
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
    reservationAggregates_netRISavings,
    reservationAggregates_totalAmortizedFee,
    reservationAggregates_utilizationPercentageInUnits,
    reservationAggregates_amortizedUpfrontFee,
    reservationAggregates_utilizationPercentage,
    reservationAggregates_purchasedUnits,

    -- ** ReservationCoverageGroup
    reservationCoverageGroup_attributes,
    reservationCoverageGroup_coverage,

    -- ** ReservationPurchaseRecommendation
    reservationPurchaseRecommendation_paymentOption,
    reservationPurchaseRecommendation_accountScope,
    reservationPurchaseRecommendation_recommendationDetails,
    reservationPurchaseRecommendation_serviceSpecification,
    reservationPurchaseRecommendation_termInYears,
    reservationPurchaseRecommendation_recommendationSummary,
    reservationPurchaseRecommendation_lookbackPeriodInDays,

    -- ** ReservationPurchaseRecommendationDetail
    reservationPurchaseRecommendationDetail_upfrontCost,
    reservationPurchaseRecommendationDetail_accountId,
    reservationPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationDetail_recurringStandardMonthlyCost,
    reservationPurchaseRecommendationDetail_recommendedNormalizedUnitsToPurchase,
    reservationPurchaseRecommendationDetail_averageUtilization,
    reservationPurchaseRecommendationDetail_averageNumberOfInstancesUsedPerHour,
    reservationPurchaseRecommendationDetail_estimatedReservationCostForLookbackPeriod,
    reservationPurchaseRecommendationDetail_instanceDetails,
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

    -- ** ReservationPurchaseRecommendationMetadata
    reservationPurchaseRecommendationMetadata_recommendationId,
    reservationPurchaseRecommendationMetadata_generationTimestamp,

    -- ** ReservationPurchaseRecommendationSummary
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsAmount,
    reservationPurchaseRecommendationSummary_currencyCode,
    reservationPurchaseRecommendationSummary_totalEstimatedMonthlySavingsPercentage,

    -- ** ReservationUtilizationGroup
    reservationUtilizationGroup_key,
    reservationUtilizationGroup_utilization,
    reservationUtilizationGroup_attributes,
    reservationUtilizationGroup_value,

    -- ** ResourceDetails
    resourceDetails_eC2ResourceDetails,

    -- ** ResourceUtilization
    resourceUtilization_eC2ResourceUtilization,

    -- ** ResultByTime
    resultByTime_groups,
    resultByTime_timePeriod,
    resultByTime_estimated,
    resultByTime_total,

    -- ** RightsizingRecommendation
    rightsizingRecommendation_accountId,
    rightsizingRecommendation_terminateRecommendationDetail,
    rightsizingRecommendation_rightsizingType,
    rightsizingRecommendation_currentInstance,
    rightsizingRecommendation_modifyRecommendationDetail,

    -- ** RightsizingRecommendationConfiguration
    rightsizingRecommendationConfiguration_recommendationTarget,
    rightsizingRecommendationConfiguration_benefitsConsidered,

    -- ** RightsizingRecommendationMetadata
    rightsizingRecommendationMetadata_recommendationId,
    rightsizingRecommendationMetadata_additionalMetadata,
    rightsizingRecommendationMetadata_lookbackPeriodInDays,
    rightsizingRecommendationMetadata_generationTimestamp,

    -- ** RightsizingRecommendationSummary
    rightsizingRecommendationSummary_estimatedTotalMonthlySavingsAmount,
    rightsizingRecommendationSummary_savingsPercentage,
    rightsizingRecommendationSummary_totalRecommendationCount,
    rightsizingRecommendationSummary_savingsCurrencyCode,

    -- ** RootCause
    rootCause_service,
    rootCause_usageType,
    rootCause_linkedAccount,
    rootCause_region,

    -- ** SavingsPlansAmortizedCommitment
    savingsPlansAmortizedCommitment_amortizedUpfrontCommitment,
    savingsPlansAmortizedCommitment_amortizedRecurringCommitment,
    savingsPlansAmortizedCommitment_totalAmortizedCommitment,

    -- ** SavingsPlansCoverage
    savingsPlansCoverage_timePeriod,
    savingsPlansCoverage_attributes,
    savingsPlansCoverage_coverage,

    -- ** SavingsPlansCoverageData
    savingsPlansCoverageData_totalCost,
    savingsPlansCoverageData_coveragePercentage,
    savingsPlansCoverageData_spendCoveredBySavingsPlans,
    savingsPlansCoverageData_onDemandCost,

    -- ** SavingsPlansDetails
    savingsPlansDetails_instanceFamily,
    savingsPlansDetails_offeringId,
    savingsPlansDetails_region,

    -- ** SavingsPlansPurchaseRecommendation
    savingsPlansPurchaseRecommendation_paymentOption,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationSummary,
    savingsPlansPurchaseRecommendation_accountScope,
    savingsPlansPurchaseRecommendation_termInYears,
    savingsPlansPurchaseRecommendation_savingsPlansPurchaseRecommendationDetails,
    savingsPlansPurchaseRecommendation_savingsPlansType,
    savingsPlansPurchaseRecommendation_lookbackPeriodInDays,

    -- ** SavingsPlansPurchaseRecommendationDetail
    savingsPlansPurchaseRecommendationDetail_upfrontCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCost,
    savingsPlansPurchaseRecommendationDetail_accountId,
    savingsPlansPurchaseRecommendationDetail_estimatedMonthlySavingsAmount,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsAmount,
    savingsPlansPurchaseRecommendationDetail_currentMaximumHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_hourlyCommitmentToPurchase,
    savingsPlansPurchaseRecommendationDetail_estimatedAverageUtilization,
    savingsPlansPurchaseRecommendationDetail_currentAverageHourlyOnDemandSpend,
    savingsPlansPurchaseRecommendationDetail_estimatedSavingsPercentage,
    savingsPlansPurchaseRecommendationDetail_savingsPlansDetails,
    savingsPlansPurchaseRecommendationDetail_currencyCode,
    savingsPlansPurchaseRecommendationDetail_estimatedSPCost,
    savingsPlansPurchaseRecommendationDetail_estimatedOnDemandCostWithCurrentCommitment,
    savingsPlansPurchaseRecommendationDetail_estimatedROI,
    savingsPlansPurchaseRecommendationDetail_currentMinimumHourlyOnDemandSpend,

    -- ** SavingsPlansPurchaseRecommendationMetadata
    savingsPlansPurchaseRecommendationMetadata_recommendationId,
    savingsPlansPurchaseRecommendationMetadata_additionalMetadata,
    savingsPlansPurchaseRecommendationMetadata_generationTimestamp,

    -- ** SavingsPlansPurchaseRecommendationSummary
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

    -- ** SavingsPlansSavings
    savingsPlansSavings_onDemandCostEquivalent,
    savingsPlansSavings_netSavings,

    -- ** SavingsPlansUtilization
    savingsPlansUtilization_unusedCommitment,
    savingsPlansUtilization_usedCommitment,
    savingsPlansUtilization_totalCommitment,
    savingsPlansUtilization_utilizationPercentage,

    -- ** SavingsPlansUtilizationAggregates
    savingsPlansUtilizationAggregates_savings,
    savingsPlansUtilizationAggregates_amortizedCommitment,
    savingsPlansUtilizationAggregates_utilization,

    -- ** SavingsPlansUtilizationByTime
    savingsPlansUtilizationByTime_savings,
    savingsPlansUtilizationByTime_amortizedCommitment,
    savingsPlansUtilizationByTime_timePeriod,
    savingsPlansUtilizationByTime_utilization,

    -- ** SavingsPlansUtilizationDetail
    savingsPlansUtilizationDetail_savings,
    savingsPlansUtilizationDetail_utilization,
    savingsPlansUtilizationDetail_attributes,
    savingsPlansUtilizationDetail_amortizedCommitment,
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
    tagValues_key,
    tagValues_values,
    tagValues_matchOptions,

    -- ** TargetInstance
    targetInstance_estimatedMonthlySavings,
    targetInstance_currencyCode,
    targetInstance_estimatedMonthlyCost,
    targetInstance_expectedResourceUtilization,
    targetInstance_defaultTargetInstance,
    targetInstance_resourceDetails,

    -- ** TerminateRecommendationDetail
    terminateRecommendationDetail_estimatedMonthlySavings,
    terminateRecommendationDetail_currencyCode,

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

import Network.AWS.CostExplorer.CreateAnomalyMonitor
import Network.AWS.CostExplorer.CreateAnomalySubscription
import Network.AWS.CostExplorer.CreateCostCategoryDefinition
import Network.AWS.CostExplorer.DeleteAnomalyMonitor
import Network.AWS.CostExplorer.DeleteAnomalySubscription
import Network.AWS.CostExplorer.DeleteCostCategoryDefinition
import Network.AWS.CostExplorer.DescribeCostCategoryDefinition
import Network.AWS.CostExplorer.GetAnomalies
import Network.AWS.CostExplorer.GetAnomalyMonitors
import Network.AWS.CostExplorer.GetAnomalySubscriptions
import Network.AWS.CostExplorer.GetCostAndUsage
import Network.AWS.CostExplorer.GetCostAndUsageWithResources
import Network.AWS.CostExplorer.GetCostCategories
import Network.AWS.CostExplorer.GetCostForecast
import Network.AWS.CostExplorer.GetDimensionValues
import Network.AWS.CostExplorer.GetReservationCoverage
import Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
import Network.AWS.CostExplorer.GetReservationUtilization
import Network.AWS.CostExplorer.GetRightsizingRecommendation
import Network.AWS.CostExplorer.GetSavingsPlansCoverage
import Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.GetSavingsPlansUtilization
import Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
import Network.AWS.CostExplorer.GetTags
import Network.AWS.CostExplorer.GetUsageForecast
import Network.AWS.CostExplorer.ListCostCategoryDefinitions
import Network.AWS.CostExplorer.ProvideAnomalyFeedback
import Network.AWS.CostExplorer.Types.Anomaly
import Network.AWS.CostExplorer.Types.AnomalyDateInterval
import Network.AWS.CostExplorer.Types.AnomalyMonitor
import Network.AWS.CostExplorer.Types.AnomalyScore
import Network.AWS.CostExplorer.Types.AnomalySubscription
import Network.AWS.CostExplorer.Types.CostCategory
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import Network.AWS.CostExplorer.Types.CostCategoryReference
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.CoverageByTime
import Network.AWS.CostExplorer.Types.CoverageCost
import Network.AWS.CostExplorer.Types.CoverageHours
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import Network.AWS.CostExplorer.Types.EC2Specification
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.ForecastResult
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.GroupDefinition
import Network.AWS.CostExplorer.Types.Impact
import Network.AWS.CostExplorer.Types.InstanceDetails
import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
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
import Network.AWS.CostExplorer.Types.RootCause
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansCoverage
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
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
import Network.AWS.CostExplorer.Types.Subscriber
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.CostExplorer.Types.TargetInstance
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import Network.AWS.CostExplorer.Types.TotalImpactFilter
import Network.AWS.CostExplorer.Types.UtilizationByTime
import Network.AWS.CostExplorer.UpdateAnomalyMonitor
import Network.AWS.CostExplorer.UpdateAnomalySubscription
import Network.AWS.CostExplorer.UpdateCostCategoryDefinition

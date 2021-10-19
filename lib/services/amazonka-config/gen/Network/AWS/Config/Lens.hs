{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Lens
  ( -- * Operations

    -- ** DescribePendingAggregationRequests
    describePendingAggregationRequests_nextToken,
    describePendingAggregationRequests_limit,
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,

    -- ** DescribeRemediationExecutionStatus
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_configRuleName,
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_httpStatus,

    -- ** GetResourceConfigHistory
    getResourceConfigHistory_chronologicalOrder,
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_resourceType,
    getResourceConfigHistory_resourceId,
    getResourceConfigHistoryResponse_nextToken,
    getResourceConfigHistoryResponse_configurationItems,
    getResourceConfigHistoryResponse_httpStatus,

    -- ** GetStoredQuery
    getStoredQuery_queryName,
    getStoredQueryResponse_storedQuery,
    getStoredQueryResponse_httpStatus,

    -- ** GetAggregateResourceConfig
    getAggregateResourceConfig_configurationAggregatorName,
    getAggregateResourceConfig_resourceIdentifier,
    getAggregateResourceConfigResponse_configurationItem,
    getAggregateResourceConfigResponse_httpStatus,

    -- ** DescribeConfigurationAggregators
    describeConfigurationAggregators_nextToken,
    describeConfigurationAggregators_limit,
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregatorsResponse_nextToken,
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_httpStatus,

    -- ** DescribeComplianceByConfigRule
    describeComplianceByConfigRule_configRuleNames,
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_httpStatus,

    -- ** DescribeRetentionConfigurations
    describeRetentionConfigurations_retentionConfigurationNames,
    describeRetentionConfigurations_nextToken,
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_httpStatus,

    -- ** StopConfigurationRecorder
    stopConfigurationRecorder_configurationRecorderName,

    -- ** GetAggregateConfigRuleComplianceSummary
    getAggregateConfigRuleComplianceSummary_filters,
    getAggregateConfigRuleComplianceSummary_nextToken,
    getAggregateConfigRuleComplianceSummary_limit,
    getAggregateConfigRuleComplianceSummary_groupByKey,
    getAggregateConfigRuleComplianceSummary_configurationAggregatorName,
    getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts,
    getAggregateConfigRuleComplianceSummaryResponse_nextToken,
    getAggregateConfigRuleComplianceSummaryResponse_groupByKey,
    getAggregateConfigRuleComplianceSummaryResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** BatchGetResourceConfig
    batchGetResourceConfig_resourceKeys,
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_unprocessedResourceKeys,
    batchGetResourceConfigResponse_httpStatus,

    -- ** DescribeConfigRules
    describeConfigRules_configRuleNames,
    describeConfigRules_nextToken,
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_httpStatus,

    -- ** PutRetentionConfiguration
    putRetentionConfiguration_retentionPeriodInDays,
    putRetentionConfigurationResponse_retentionConfiguration,
    putRetentionConfigurationResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConformancePacks
    describeAggregateComplianceByConformancePacks_filters,
    describeAggregateComplianceByConformancePacks_nextToken,
    describeAggregateComplianceByConformancePacks_limit,
    describeAggregateComplianceByConformancePacks_configurationAggregatorName,
    describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks,
    describeAggregateComplianceByConformancePacksResponse_nextToken,
    describeAggregateComplianceByConformancePacksResponse_httpStatus,

    -- ** GetOrganizationConformancePackDetailedStatus
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_nextToken,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_organizationConformancePackName,
    getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses,
    getOrganizationConformancePackDetailedStatusResponse_nextToken,
    getOrganizationConformancePackDetailedStatusResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConfigRules
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,

    -- ** DeleteEvaluationResults
    deleteEvaluationResults_configRuleName,
    deleteEvaluationResultsResponse_httpStatus,

    -- ** PutConfigRule
    putConfigRule_tags,
    putConfigRule_configRule,

    -- ** GetConformancePackComplianceDetails
    getConformancePackComplianceDetails_filters,
    getConformancePackComplianceDetails_nextToken,
    getConformancePackComplianceDetails_limit,
    getConformancePackComplianceDetails_conformancePackName,
    getConformancePackComplianceDetailsResponse_nextToken,
    getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults,
    getConformancePackComplianceDetailsResponse_httpStatus,
    getConformancePackComplianceDetailsResponse_conformancePackName,

    -- ** DeleteConfigRule
    deleteConfigRule_configRuleName,

    -- ** DeleteRetentionConfiguration
    deleteRetentionConfiguration_retentionConfigurationName,

    -- ** ListStoredQueries
    listStoredQueries_nextToken,
    listStoredQueries_maxResults,
    listStoredQueriesResponse_storedQueryMetadata,
    listStoredQueriesResponse_nextToken,
    listStoredQueriesResponse_httpStatus,

    -- ** SelectResourceConfig
    selectResourceConfig_nextToken,
    selectResourceConfig_limit,
    selectResourceConfig_expression,
    selectResourceConfigResponse_results,
    selectResourceConfigResponse_queryInfo,
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_httpStatus,

    -- ** ListAggregateDiscoveredResources
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_nextToken,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_configurationAggregatorName,
    listAggregateDiscoveredResources_resourceType,
    listAggregateDiscoveredResourcesResponse_nextToken,
    listAggregateDiscoveredResourcesResponse_resourceIdentifiers,
    listAggregateDiscoveredResourcesResponse_httpStatus,

    -- ** DescribeOrganizationConfigRuleStatuses
    describeOrganizationConfigRuleStatuses_organizationConfigRuleNames,
    describeOrganizationConfigRuleStatuses_nextToken,
    describeOrganizationConfigRuleStatuses_limit,
    describeOrganizationConfigRuleStatusesResponse_nextToken,
    describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses,
    describeOrganizationConfigRuleStatusesResponse_httpStatus,

    -- ** DescribeOrganizationConformancePackStatuses
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_limit,
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_httpStatus,

    -- ** GetComplianceDetailsByResource
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_resourceType,
    getComplianceDetailsByResource_resourceId,
    getComplianceDetailsByResourceResponse_evaluationResults,
    getComplianceDetailsByResourceResponse_nextToken,
    getComplianceDetailsByResourceResponse_httpStatus,

    -- ** DeletePendingAggregationRequest
    deletePendingAggregationRequest_requesterAccountId,
    deletePendingAggregationRequest_requesterAwsRegion,

    -- ** DeliverConfigSnapshot
    deliverConfigSnapshot_deliveryChannelName,
    deliverConfigSnapshotResponse_configSnapshotId,
    deliverConfigSnapshotResponse_httpStatus,

    -- ** BatchGetAggregateResourceConfig
    batchGetAggregateResourceConfig_configurationAggregatorName,
    batchGetAggregateResourceConfig_resourceIdentifiers,
    batchGetAggregateResourceConfigResponse_baseConfigurationItems,
    batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers,
    batchGetAggregateResourceConfigResponse_httpStatus,

    -- ** DescribeConfigRuleEvaluationStatus
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_nextToken,
    describeConfigRuleEvaluationStatus_limit,
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_httpStatus,

    -- ** GetDiscoveredResourceCounts
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_limit,
    getDiscoveredResourceCounts_resourceTypes,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_httpStatus,

    -- ** DescribeRemediationExceptions
    describeRemediationExceptions_nextToken,
    describeRemediationExceptions_limit,
    describeRemediationExceptions_resourceKeys,
    describeRemediationExceptions_configRuleName,
    describeRemediationExceptionsResponse_nextToken,
    describeRemediationExceptionsResponse_remediationExceptions,
    describeRemediationExceptionsResponse_httpStatus,

    -- ** DeleteOrganizationConformancePack
    deleteOrganizationConformancePack_organizationConformancePackName,

    -- ** PutOrganizationConfigRule
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
    putOrganizationConfigRule_organizationConfigRuleName,
    putOrganizationConfigRuleResponse_organizationConfigRuleArn,
    putOrganizationConfigRuleResponse_httpStatus,

    -- ** PutOrganizationConformancePack
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_organizationConformancePackName,
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,

    -- ** DeleteOrganizationConfigRule
    deleteOrganizationConfigRule_organizationConfigRuleName,

    -- ** PutResourceConfig
    putResourceConfig_resourceName,
    putResourceConfig_tags,
    putResourceConfig_resourceType,
    putResourceConfig_schemaVersionId,
    putResourceConfig_resourceId,
    putResourceConfig_configuration,

    -- ** StartConfigRulesEvaluation
    startConfigRulesEvaluation_configRuleNames,
    startConfigRulesEvaluationResponse_httpStatus,

    -- ** DescribeOrganizationConfigRules
    describeOrganizationConfigRules_organizationConfigRuleNames,
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_limit,
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_httpStatus,

    -- ** SelectAggregateResourceConfig
    selectAggregateResourceConfig_nextToken,
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_expression,
    selectAggregateResourceConfig_configurationAggregatorName,
    selectAggregateResourceConfigResponse_results,
    selectAggregateResourceConfigResponse_queryInfo,
    selectAggregateResourceConfigResponse_nextToken,
    selectAggregateResourceConfigResponse_httpStatus,

    -- ** DescribeComplianceByResource
    describeComplianceByResource_resourceId,
    describeComplianceByResource_resourceType,
    describeComplianceByResource_complianceTypes,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_limit,
    describeComplianceByResourceResponse_complianceByResources,
    describeComplianceByResourceResponse_nextToken,
    describeComplianceByResourceResponse_httpStatus,

    -- ** DescribeOrganizationConformancePacks
    describeOrganizationConformancePacks_nextToken,
    describeOrganizationConformancePacks_limit,
    describeOrganizationConformancePacks_organizationConformancePackNames,
    describeOrganizationConformancePacksResponse_organizationConformancePacks,
    describeOrganizationConformancePacksResponse_nextToken,
    describeOrganizationConformancePacksResponse_httpStatus,

    -- ** DeleteResourceConfig
    deleteResourceConfig_resourceType,
    deleteResourceConfig_resourceId,

    -- ** PutEvaluations
    putEvaluations_evaluations,
    putEvaluations_testMode,
    putEvaluations_resultToken,
    putEvaluationsResponse_failedEvaluations,
    putEvaluationsResponse_httpStatus,

    -- ** DescribeConfigurationRecorders
    describeConfigurationRecorders_configurationRecorderNames,
    describeConfigurationRecordersResponse_configurationRecorders,
    describeConfigurationRecordersResponse_httpStatus,

    -- ** DescribeConformancePackCompliance
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_nextToken,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_conformancePackName,
    describeConformancePackComplianceResponse_nextToken,
    describeConformancePackComplianceResponse_httpStatus,
    describeConformancePackComplianceResponse_conformancePackName,
    describeConformancePackComplianceResponse_conformancePackRuleComplianceList,

    -- ** GetAggregateComplianceDetailsByConfigRule
    getAggregateComplianceDetailsByConfigRule_nextToken,
    getAggregateComplianceDetailsByConfigRule_limit,
    getAggregateComplianceDetailsByConfigRule_complianceType,
    getAggregateComplianceDetailsByConfigRule_configurationAggregatorName,
    getAggregateComplianceDetailsByConfigRule_configRuleName,
    getAggregateComplianceDetailsByConfigRule_accountId,
    getAggregateComplianceDetailsByConfigRule_awsRegion,
    getAggregateComplianceDetailsByConfigRuleResponse_nextToken,
    getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults,
    getAggregateComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** GetAggregateDiscoveredResourceCounts
    getAggregateDiscoveredResourceCounts_filters,
    getAggregateDiscoveredResourceCounts_nextToken,
    getAggregateDiscoveredResourceCounts_limit,
    getAggregateDiscoveredResourceCounts_groupByKey,
    getAggregateDiscoveredResourceCounts_configurationAggregatorName,
    getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts,
    getAggregateDiscoveredResourceCountsResponse_nextToken,
    getAggregateDiscoveredResourceCountsResponse_groupByKey,
    getAggregateDiscoveredResourceCountsResponse_httpStatus,
    getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources,

    -- ** GetAggregateConformancePackComplianceSummary
    getAggregateConformancePackComplianceSummary_filters,
    getAggregateConformancePackComplianceSummary_nextToken,
    getAggregateConformancePackComplianceSummary_limit,
    getAggregateConformancePackComplianceSummary_groupByKey,
    getAggregateConformancePackComplianceSummary_configurationAggregatorName,
    getAggregateConformancePackComplianceSummaryResponse_nextToken,
    getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries,
    getAggregateConformancePackComplianceSummaryResponse_groupByKey,
    getAggregateConformancePackComplianceSummaryResponse_httpStatus,

    -- ** StartConfigurationRecorder
    startConfigurationRecorder_configurationRecorderName,

    -- ** DescribeConformancePacks
    describeConformancePacks_conformancePackNames,
    describeConformancePacks_nextToken,
    describeConformancePacks_limit,
    describeConformancePacksResponse_nextToken,
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_httpStatus,

    -- ** PutExternalEvaluation
    putExternalEvaluation_configRuleName,
    putExternalEvaluation_externalEvaluation,
    putExternalEvaluationResponse_httpStatus,

    -- ** DeleteRemediationExceptions
    deleteRemediationExceptions_configRuleName,
    deleteRemediationExceptions_resourceKeys,
    deleteRemediationExceptionsResponse_failedBatches,
    deleteRemediationExceptionsResponse_httpStatus,

    -- ** PutRemediationExceptions
    putRemediationExceptions_message,
    putRemediationExceptions_expirationTime,
    putRemediationExceptions_configRuleName,
    putRemediationExceptions_resourceKeys,
    putRemediationExceptionsResponse_failedBatches,
    putRemediationExceptionsResponse_httpStatus,

    -- ** GetOrganizationConfigRuleDetailedStatus
    getOrganizationConfigRuleDetailedStatus_filters,
    getOrganizationConfigRuleDetailedStatus_nextToken,
    getOrganizationConfigRuleDetailedStatus_limit,
    getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName,
    getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus,
    getOrganizationConfigRuleDetailedStatusResponse_nextToken,
    getOrganizationConfigRuleDetailedStatusResponse_httpStatus,

    -- ** PutRemediationConfigurations
    putRemediationConfigurations_remediationConfigurations,
    putRemediationConfigurationsResponse_failedBatches,
    putRemediationConfigurationsResponse_httpStatus,

    -- ** DeleteConformancePack
    deleteConformancePack_conformancePackName,

    -- ** PutConformancePack
    putConformancePack_deliveryS3Bucket,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_templateS3Uri,
    putConformancePack_conformancePackInputParameters,
    putConformancePack_templateBody,
    putConformancePack_conformancePackName,
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,

    -- ** StartRemediationExecution
    startRemediationExecution_configRuleName,
    startRemediationExecution_resourceKeys,
    startRemediationExecutionResponse_failureMessage,
    startRemediationExecutionResponse_failedItems,
    startRemediationExecutionResponse_httpStatus,

    -- ** DescribeConformancePackStatus
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_limit,
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_httpStatus,

    -- ** GetComplianceSummaryByConfigRule
    getComplianceSummaryByConfigRuleResponse_complianceSummary,
    getComplianceSummaryByConfigRuleResponse_httpStatus,

    -- ** PutStoredQuery
    putStoredQuery_tags,
    putStoredQuery_storedQuery,
    putStoredQueryResponse_queryArn,
    putStoredQueryResponse_httpStatus,

    -- ** PutConfigurationAggregator
    putConfigurationAggregator_organizationAggregationSource,
    putConfigurationAggregator_accountAggregationSources,
    putConfigurationAggregator_tags,
    putConfigurationAggregator_configurationAggregatorName,
    putConfigurationAggregatorResponse_configurationAggregator,
    putConfigurationAggregatorResponse_httpStatus,

    -- ** DeleteStoredQuery
    deleteStoredQuery_queryName,
    deleteStoredQueryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** DeleteConfigurationAggregator
    deleteConfigurationAggregator_configurationAggregatorName,

    -- ** DescribeConfigurationRecorderStatus
    describeConfigurationRecorderStatus_configurationRecorderNames,
    describeConfigurationRecorderStatusResponse_configurationRecordersStatus,
    describeConfigurationRecorderStatusResponse_httpStatus,

    -- ** PutConfigurationRecorder
    putConfigurationRecorder_configurationRecorder,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DeleteConfigurationRecorder
    deleteConfigurationRecorder_configurationRecorderName,

    -- ** GetConformancePackComplianceSummary
    getConformancePackComplianceSummary_nextToken,
    getConformancePackComplianceSummary_limit,
    getConformancePackComplianceSummary_conformancePackNames,
    getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList,
    getConformancePackComplianceSummaryResponse_nextToken,
    getConformancePackComplianceSummaryResponse_httpStatus,

    -- ** GetComplianceSummaryByResourceType
    getComplianceSummaryByResourceType_resourceTypes,
    getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType,
    getComplianceSummaryByResourceTypeResponse_httpStatus,

    -- ** DescribeDeliveryChannelStatus
    describeDeliveryChannelStatus_deliveryChannelNames,
    describeDeliveryChannelStatusResponse_deliveryChannelsStatus,
    describeDeliveryChannelStatusResponse_httpStatus,

    -- ** PutDeliveryChannel
    putDeliveryChannel_deliveryChannel,

    -- ** GetComplianceDetailsByConfigRule
    getComplianceDetailsByConfigRule_complianceTypes,
    getComplianceDetailsByConfigRule_nextToken,
    getComplianceDetailsByConfigRule_limit,
    getComplianceDetailsByConfigRule_configRuleName,
    getComplianceDetailsByConfigRuleResponse_evaluationResults,
    getComplianceDetailsByConfigRuleResponse_nextToken,
    getComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** DeleteAggregationAuthorization
    deleteAggregationAuthorization_authorizedAccountId,
    deleteAggregationAuthorization_authorizedAwsRegion,

    -- ** DeleteDeliveryChannel
    deleteDeliveryChannel_deliveryChannelName,

    -- ** DeleteRemediationConfiguration
    deleteRemediationConfiguration_resourceType,
    deleteRemediationConfiguration_configRuleName,
    deleteRemediationConfigurationResponse_httpStatus,

    -- ** PutAggregationAuthorization
    putAggregationAuthorization_tags,
    putAggregationAuthorization_authorizedAccountId,
    putAggregationAuthorization_authorizedAwsRegion,
    putAggregationAuthorizationResponse_aggregationAuthorization,
    putAggregationAuthorizationResponse_httpStatus,

    -- ** DescribeConfigurationAggregatorSourcesStatus
    describeConfigurationAggregatorSourcesStatus_nextToken,
    describeConfigurationAggregatorSourcesStatus_limit,
    describeConfigurationAggregatorSourcesStatus_updateStatus,
    describeConfigurationAggregatorSourcesStatus_configurationAggregatorName,
    describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList,
    describeConfigurationAggregatorSourcesStatusResponse_nextToken,
    describeConfigurationAggregatorSourcesStatusResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_limit,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_resourceIdentifiers,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** DescribeRemediationConfigurations
    describeRemediationConfigurations_configRuleNames,
    describeRemediationConfigurationsResponse_remediationConfigurations,
    describeRemediationConfigurationsResponse_httpStatus,

    -- ** DescribeDeliveryChannels
    describeDeliveryChannels_deliveryChannelNames,
    describeDeliveryChannelsResponse_deliveryChannels,
    describeDeliveryChannelsResponse_httpStatus,

    -- ** DescribeAggregationAuthorizations
    describeAggregationAuthorizations_nextToken,
    describeAggregationAuthorizations_limit,
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_httpStatus,

    -- * Types

    -- ** AccountAggregationSource
    accountAggregationSource_awsRegions,
    accountAggregationSource_allAwsRegions,
    accountAggregationSource_accountIds,

    -- ** AggregateComplianceByConfigRule
    aggregateComplianceByConfigRule_compliance,
    aggregateComplianceByConfigRule_configRuleName,
    aggregateComplianceByConfigRule_accountId,
    aggregateComplianceByConfigRule_awsRegion,

    -- ** AggregateComplianceByConformancePack
    aggregateComplianceByConformancePack_compliance,
    aggregateComplianceByConformancePack_conformancePackName,
    aggregateComplianceByConformancePack_accountId,
    aggregateComplianceByConformancePack_awsRegion,

    -- ** AggregateComplianceCount
    aggregateComplianceCount_groupName,
    aggregateComplianceCount_complianceSummary,

    -- ** AggregateConformancePackCompliance
    aggregateConformancePackCompliance_totalRuleCount,
    aggregateConformancePackCompliance_compliantRuleCount,
    aggregateConformancePackCompliance_nonCompliantRuleCount,
    aggregateConformancePackCompliance_complianceType,

    -- ** AggregateConformancePackComplianceCount
    aggregateConformancePackComplianceCount_compliantConformancePackCount,
    aggregateConformancePackComplianceCount_nonCompliantConformancePackCount,

    -- ** AggregateConformancePackComplianceFilters
    aggregateConformancePackComplianceFilters_conformancePackName,
    aggregateConformancePackComplianceFilters_accountId,
    aggregateConformancePackComplianceFilters_complianceType,
    aggregateConformancePackComplianceFilters_awsRegion,

    -- ** AggregateConformancePackComplianceSummary
    aggregateConformancePackComplianceSummary_groupName,
    aggregateConformancePackComplianceSummary_complianceSummary,

    -- ** AggregateConformancePackComplianceSummaryFilters
    aggregateConformancePackComplianceSummaryFilters_accountId,
    aggregateConformancePackComplianceSummaryFilters_awsRegion,

    -- ** AggregateEvaluationResult
    aggregateEvaluationResult_evaluationResultIdentifier,
    aggregateEvaluationResult_annotation,
    aggregateEvaluationResult_configRuleInvokedTime,
    aggregateEvaluationResult_resultRecordedTime,
    aggregateEvaluationResult_accountId,
    aggregateEvaluationResult_complianceType,
    aggregateEvaluationResult_awsRegion,

    -- ** AggregateResourceIdentifier
    aggregateResourceIdentifier_resourceName,
    aggregateResourceIdentifier_sourceAccountId,
    aggregateResourceIdentifier_sourceRegion,
    aggregateResourceIdentifier_resourceId,
    aggregateResourceIdentifier_resourceType,

    -- ** AggregatedSourceStatus
    aggregatedSourceStatus_lastErrorCode,
    aggregatedSourceStatus_lastUpdateStatus,
    aggregatedSourceStatus_sourceType,
    aggregatedSourceStatus_sourceId,
    aggregatedSourceStatus_lastErrorMessage,
    aggregatedSourceStatus_awsRegion,
    aggregatedSourceStatus_lastUpdateTime,

    -- ** AggregationAuthorization
    aggregationAuthorization_creationTime,
    aggregationAuthorization_authorizedAwsRegion,
    aggregationAuthorization_aggregationAuthorizationArn,
    aggregationAuthorization_authorizedAccountId,

    -- ** BaseConfigurationItem
    baseConfigurationItem_resourceId,
    baseConfigurationItem_resourceType,
    baseConfigurationItem_configurationStateId,
    baseConfigurationItem_arn,
    baseConfigurationItem_resourceName,
    baseConfigurationItem_resourceCreationTime,
    baseConfigurationItem_configurationItemStatus,
    baseConfigurationItem_configurationItemCaptureTime,
    baseConfigurationItem_accountId,
    baseConfigurationItem_supplementaryConfiguration,
    baseConfigurationItem_availabilityZone,
    baseConfigurationItem_version,
    baseConfigurationItem_awsRegion,
    baseConfigurationItem_configuration,

    -- ** Compliance
    compliance_complianceContributorCount,
    compliance_complianceType,

    -- ** ComplianceByConfigRule
    complianceByConfigRule_compliance,
    complianceByConfigRule_configRuleName,

    -- ** ComplianceByResource
    complianceByResource_resourceId,
    complianceByResource_resourceType,
    complianceByResource_compliance,

    -- ** ComplianceContributorCount
    complianceContributorCount_cappedCount,
    complianceContributorCount_capExceeded,

    -- ** ComplianceSummary
    complianceSummary_complianceSummaryTimestamp,
    complianceSummary_compliantResourceCount,
    complianceSummary_nonCompliantResourceCount,

    -- ** ComplianceSummaryByResourceType
    complianceSummaryByResourceType_resourceType,
    complianceSummaryByResourceType_complianceSummary,

    -- ** ConfigExportDeliveryInfo
    configExportDeliveryInfo_lastErrorCode,
    configExportDeliveryInfo_lastAttemptTime,
    configExportDeliveryInfo_lastSuccessfulTime,
    configExportDeliveryInfo_lastStatus,
    configExportDeliveryInfo_lastErrorMessage,
    configExportDeliveryInfo_nextDeliveryTime,

    -- ** ConfigRule
    configRule_inputParameters,
    configRule_configRuleName,
    configRule_createdBy,
    configRule_maximumExecutionFrequency,
    configRule_configRuleId,
    configRule_scope,
    configRule_configRuleState,
    configRule_description,
    configRule_configRuleArn,
    configRule_source,

    -- ** ConfigRuleComplianceFilters
    configRuleComplianceFilters_configRuleName,
    configRuleComplianceFilters_accountId,
    configRuleComplianceFilters_complianceType,
    configRuleComplianceFilters_awsRegion,

    -- ** ConfigRuleComplianceSummaryFilters
    configRuleComplianceSummaryFilters_accountId,
    configRuleComplianceSummaryFilters_awsRegion,

    -- ** ConfigRuleEvaluationStatus
    configRuleEvaluationStatus_lastErrorCode,
    configRuleEvaluationStatus_lastFailedEvaluationTime,
    configRuleEvaluationStatus_firstActivatedTime,
    configRuleEvaluationStatus_lastSuccessfulEvaluationTime,
    configRuleEvaluationStatus_lastDeactivatedTime,
    configRuleEvaluationStatus_configRuleName,
    configRuleEvaluationStatus_lastErrorMessage,
    configRuleEvaluationStatus_configRuleId,
    configRuleEvaluationStatus_lastFailedInvocationTime,
    configRuleEvaluationStatus_firstEvaluationStarted,
    configRuleEvaluationStatus_lastSuccessfulInvocationTime,
    configRuleEvaluationStatus_configRuleArn,

    -- ** ConfigSnapshotDeliveryProperties
    configSnapshotDeliveryProperties_deliveryFrequency,

    -- ** ConfigStreamDeliveryInfo
    configStreamDeliveryInfo_lastErrorCode,
    configStreamDeliveryInfo_lastStatusChangeTime,
    configStreamDeliveryInfo_lastStatus,
    configStreamDeliveryInfo_lastErrorMessage,

    -- ** ConfigurationAggregator
    configurationAggregator_configurationAggregatorArn,
    configurationAggregator_creationTime,
    configurationAggregator_organizationAggregationSource,
    configurationAggregator_lastUpdatedTime,
    configurationAggregator_accountAggregationSources,
    configurationAggregator_createdBy,
    configurationAggregator_configurationAggregatorName,

    -- ** ConfigurationItem
    configurationItem_resourceId,
    configurationItem_resourceType,
    configurationItem_configurationStateId,
    configurationItem_arn,
    configurationItem_resourceName,
    configurationItem_resourceCreationTime,
    configurationItem_configurationItemStatus,
    configurationItem_configurationItemCaptureTime,
    configurationItem_accountId,
    configurationItem_supplementaryConfiguration,
    configurationItem_availabilityZone,
    configurationItem_relationships,
    configurationItem_version,
    configurationItem_awsRegion,
    configurationItem_relatedEvents,
    configurationItem_configuration,
    configurationItem_configurationItemMD5Hash,
    configurationItem_tags,

    -- ** ConfigurationRecorder
    configurationRecorder_name,
    configurationRecorder_recordingGroup,
    configurationRecorder_roleARN,

    -- ** ConfigurationRecorderStatus
    configurationRecorderStatus_lastErrorCode,
    configurationRecorderStatus_lastStopTime,
    configurationRecorderStatus_lastStatusChangeTime,
    configurationRecorderStatus_recording,
    configurationRecorderStatus_lastStatus,
    configurationRecorderStatus_lastErrorMessage,
    configurationRecorderStatus_name,
    configurationRecorderStatus_lastStartTime,

    -- ** ConformancePackComplianceFilters
    conformancePackComplianceFilters_configRuleNames,
    conformancePackComplianceFilters_complianceType,

    -- ** ConformancePackComplianceSummary
    conformancePackComplianceSummary_conformancePackName,
    conformancePackComplianceSummary_conformancePackComplianceStatus,

    -- ** ConformancePackDetail
    conformancePackDetail_deliveryS3Bucket,
    conformancePackDetail_deliveryS3KeyPrefix,
    conformancePackDetail_createdBy,
    conformancePackDetail_lastUpdateRequestedTime,
    conformancePackDetail_conformancePackInputParameters,
    conformancePackDetail_conformancePackName,
    conformancePackDetail_conformancePackArn,
    conformancePackDetail_conformancePackId,

    -- ** ConformancePackEvaluationFilters
    conformancePackEvaluationFilters_resourceIds,
    conformancePackEvaluationFilters_resourceType,
    conformancePackEvaluationFilters_configRuleNames,
    conformancePackEvaluationFilters_complianceType,

    -- ** ConformancePackEvaluationResult
    conformancePackEvaluationResult_annotation,
    conformancePackEvaluationResult_complianceType,
    conformancePackEvaluationResult_evaluationResultIdentifier,
    conformancePackEvaluationResult_configRuleInvokedTime,
    conformancePackEvaluationResult_resultRecordedTime,

    -- ** ConformancePackInputParameter
    conformancePackInputParameter_parameterName,
    conformancePackInputParameter_parameterValue,

    -- ** ConformancePackRuleCompliance
    conformancePackRuleCompliance_controls,
    conformancePackRuleCompliance_configRuleName,
    conformancePackRuleCompliance_complianceType,

    -- ** ConformancePackStatusDetail
    conformancePackStatusDetail_conformancePackStatusReason,
    conformancePackStatusDetail_lastUpdateCompletedTime,
    conformancePackStatusDetail_conformancePackName,
    conformancePackStatusDetail_conformancePackId,
    conformancePackStatusDetail_conformancePackArn,
    conformancePackStatusDetail_conformancePackState,
    conformancePackStatusDetail_stackArn,
    conformancePackStatusDetail_lastUpdateRequestedTime,

    -- ** DeliveryChannel
    deliveryChannel_s3KeyPrefix,
    deliveryChannel_snsTopicARN,
    deliveryChannel_name,
    deliveryChannel_s3KmsKeyArn,
    deliveryChannel_configSnapshotDeliveryProperties,
    deliveryChannel_s3BucketName,

    -- ** DeliveryChannelStatus
    deliveryChannelStatus_configSnapshotDeliveryInfo,
    deliveryChannelStatus_configStreamDeliveryInfo,
    deliveryChannelStatus_configHistoryDeliveryInfo,
    deliveryChannelStatus_name,

    -- ** Evaluation
    evaluation_annotation,
    evaluation_complianceResourceType,
    evaluation_complianceResourceId,
    evaluation_complianceType,
    evaluation_orderingTimestamp,

    -- ** EvaluationResult
    evaluationResult_evaluationResultIdentifier,
    evaluationResult_annotation,
    evaluationResult_configRuleInvokedTime,
    evaluationResult_resultRecordedTime,
    evaluationResult_resultToken,
    evaluationResult_complianceType,

    -- ** EvaluationResultIdentifier
    evaluationResultIdentifier_evaluationResultQualifier,
    evaluationResultIdentifier_orderingTimestamp,

    -- ** EvaluationResultQualifier
    evaluationResultQualifier_resourceId,
    evaluationResultQualifier_resourceType,
    evaluationResultQualifier_configRuleName,

    -- ** ExecutionControls
    executionControls_ssmControls,

    -- ** ExternalEvaluation
    externalEvaluation_annotation,
    externalEvaluation_complianceResourceType,
    externalEvaluation_complianceResourceId,
    externalEvaluation_complianceType,
    externalEvaluation_orderingTimestamp,

    -- ** FailedDeleteRemediationExceptionsBatch
    failedDeleteRemediationExceptionsBatch_failureMessage,
    failedDeleteRemediationExceptionsBatch_failedItems,

    -- ** FailedRemediationBatch
    failedRemediationBatch_failureMessage,
    failedRemediationBatch_failedItems,

    -- ** FailedRemediationExceptionBatch
    failedRemediationExceptionBatch_failureMessage,
    failedRemediationExceptionBatch_failedItems,

    -- ** FieldInfo
    fieldInfo_name,

    -- ** GroupedResourceCount
    groupedResourceCount_groupName,
    groupedResourceCount_resourceCount,

    -- ** MemberAccountStatus
    memberAccountStatus_errorCode,
    memberAccountStatus_errorMessage,
    memberAccountStatus_lastUpdateTime,
    memberAccountStatus_accountId,
    memberAccountStatus_configRuleName,
    memberAccountStatus_memberAccountRuleStatus,

    -- ** OrganizationAggregationSource
    organizationAggregationSource_awsRegions,
    organizationAggregationSource_allAwsRegions,
    organizationAggregationSource_roleArn,

    -- ** OrganizationConfigRule
    organizationConfigRule_organizationManagedRuleMetadata,
    organizationConfigRule_excludedAccounts,
    organizationConfigRule_organizationCustomRuleMetadata,
    organizationConfigRule_lastUpdateTime,
    organizationConfigRule_organizationConfigRuleName,
    organizationConfigRule_organizationConfigRuleArn,

    -- ** OrganizationConfigRuleStatus
    organizationConfigRuleStatus_errorCode,
    organizationConfigRuleStatus_errorMessage,
    organizationConfigRuleStatus_lastUpdateTime,
    organizationConfigRuleStatus_organizationConfigRuleName,
    organizationConfigRuleStatus_organizationRuleStatus,

    -- ** OrganizationConformancePack
    organizationConformancePack_deliveryS3Bucket,
    organizationConformancePack_deliveryS3KeyPrefix,
    organizationConformancePack_conformancePackInputParameters,
    organizationConformancePack_excludedAccounts,
    organizationConformancePack_organizationConformancePackName,
    organizationConformancePack_organizationConformancePackArn,
    organizationConformancePack_lastUpdateTime,

    -- ** OrganizationConformancePackDetailedStatus
    organizationConformancePackDetailedStatus_errorCode,
    organizationConformancePackDetailedStatus_errorMessage,
    organizationConformancePackDetailedStatus_lastUpdateTime,
    organizationConformancePackDetailedStatus_accountId,
    organizationConformancePackDetailedStatus_conformancePackName,
    organizationConformancePackDetailedStatus_status,

    -- ** OrganizationConformancePackStatus
    organizationConformancePackStatus_errorCode,
    organizationConformancePackStatus_errorMessage,
    organizationConformancePackStatus_lastUpdateTime,
    organizationConformancePackStatus_organizationConformancePackName,
    organizationConformancePackStatus_status,

    -- ** OrganizationCustomRuleMetadata
    organizationCustomRuleMetadata_inputParameters,
    organizationCustomRuleMetadata_resourceIdScope,
    organizationCustomRuleMetadata_tagValueScope,
    organizationCustomRuleMetadata_maximumExecutionFrequency,
    organizationCustomRuleMetadata_tagKeyScope,
    organizationCustomRuleMetadata_resourceTypesScope,
    organizationCustomRuleMetadata_description,
    organizationCustomRuleMetadata_lambdaFunctionArn,
    organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes,

    -- ** OrganizationManagedRuleMetadata
    organizationManagedRuleMetadata_inputParameters,
    organizationManagedRuleMetadata_resourceIdScope,
    organizationManagedRuleMetadata_tagValueScope,
    organizationManagedRuleMetadata_maximumExecutionFrequency,
    organizationManagedRuleMetadata_tagKeyScope,
    organizationManagedRuleMetadata_resourceTypesScope,
    organizationManagedRuleMetadata_description,
    organizationManagedRuleMetadata_ruleIdentifier,

    -- ** OrganizationResourceDetailedStatusFilters
    organizationResourceDetailedStatusFilters_status,
    organizationResourceDetailedStatusFilters_accountId,

    -- ** PendingAggregationRequest
    pendingAggregationRequest_requesterAccountId,
    pendingAggregationRequest_requesterAwsRegion,

    -- ** QueryInfo
    queryInfo_selectFields,

    -- ** RecordingGroup
    recordingGroup_allSupported,
    recordingGroup_includeGlobalResourceTypes,
    recordingGroup_resourceTypes,

    -- ** Relationship
    relationship_resourceId,
    relationship_resourceType,
    relationship_resourceName,
    relationship_relationshipName,

    -- ** RemediationConfiguration
    remediationConfiguration_resourceType,
    remediationConfiguration_arn,
    remediationConfiguration_automatic,
    remediationConfiguration_createdByService,
    remediationConfiguration_retryAttemptSeconds,
    remediationConfiguration_executionControls,
    remediationConfiguration_parameters,
    remediationConfiguration_maximumAutomaticAttempts,
    remediationConfiguration_targetVersion,
    remediationConfiguration_configRuleName,
    remediationConfiguration_targetType,
    remediationConfiguration_targetId,

    -- ** RemediationException
    remediationException_message,
    remediationException_expirationTime,
    remediationException_configRuleName,
    remediationException_resourceType,
    remediationException_resourceId,

    -- ** RemediationExceptionResourceKey
    remediationExceptionResourceKey_resourceId,
    remediationExceptionResourceKey_resourceType,

    -- ** RemediationExecutionStatus
    remediationExecutionStatus_state,
    remediationExecutionStatus_lastUpdatedTime,
    remediationExecutionStatus_resourceKey,
    remediationExecutionStatus_stepDetails,
    remediationExecutionStatus_invocationTime,

    -- ** RemediationExecutionStep
    remediationExecutionStep_state,
    remediationExecutionStep_startTime,
    remediationExecutionStep_name,
    remediationExecutionStep_stopTime,
    remediationExecutionStep_errorMessage,

    -- ** RemediationParameterValue
    remediationParameterValue_staticValue,
    remediationParameterValue_resourceValue,

    -- ** ResourceCount
    resourceCount_resourceType,
    resourceCount_count,

    -- ** ResourceCountFilters
    resourceCountFilters_resourceType,
    resourceCountFilters_accountId,
    resourceCountFilters_region,

    -- ** ResourceFilters
    resourceFilters_resourceId,
    resourceFilters_resourceName,
    resourceFilters_accountId,
    resourceFilters_region,

    -- ** ResourceIdentifier
    resourceIdentifier_resourceId,
    resourceIdentifier_resourceType,
    resourceIdentifier_resourceName,
    resourceIdentifier_resourceDeletionTime,

    -- ** ResourceKey
    resourceKey_resourceType,
    resourceKey_resourceId,

    -- ** ResourceValue
    resourceValue_value,

    -- ** RetentionConfiguration
    retentionConfiguration_name,
    retentionConfiguration_retentionPeriodInDays,

    -- ** Scope
    scope_complianceResourceTypes,
    scope_complianceResourceId,
    scope_tagValue,
    scope_tagKey,

    -- ** Source
    source_sourceDetails,
    source_owner,
    source_sourceIdentifier,

    -- ** SourceDetail
    sourceDetail_messageType,
    sourceDetail_maximumExecutionFrequency,
    sourceDetail_eventSource,

    -- ** SsmControls
    ssmControls_concurrentExecutionRatePercentage,
    ssmControls_errorPercentage,

    -- ** StaticValue
    staticValue_values,

    -- ** StatusDetailFilters
    statusDetailFilters_memberAccountRuleStatus,
    statusDetailFilters_accountId,

    -- ** StoredQuery
    storedQuery_queryId,
    storedQuery_queryArn,
    storedQuery_expression,
    storedQuery_description,
    storedQuery_queryName,

    -- ** StoredQueryMetadata
    storedQueryMetadata_description,
    storedQueryMetadata_queryId,
    storedQueryMetadata_queryArn,
    storedQueryMetadata_queryName,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.Config.BatchGetAggregateResourceConfig
import Network.AWS.Config.BatchGetResourceConfig
import Network.AWS.Config.DeleteAggregationAuthorization
import Network.AWS.Config.DeleteConfigRule
import Network.AWS.Config.DeleteConfigurationAggregator
import Network.AWS.Config.DeleteConfigurationRecorder
import Network.AWS.Config.DeleteConformancePack
import Network.AWS.Config.DeleteDeliveryChannel
import Network.AWS.Config.DeleteEvaluationResults
import Network.AWS.Config.DeleteOrganizationConfigRule
import Network.AWS.Config.DeleteOrganizationConformancePack
import Network.AWS.Config.DeletePendingAggregationRequest
import Network.AWS.Config.DeleteRemediationConfiguration
import Network.AWS.Config.DeleteRemediationExceptions
import Network.AWS.Config.DeleteResourceConfig
import Network.AWS.Config.DeleteRetentionConfiguration
import Network.AWS.Config.DeleteStoredQuery
import Network.AWS.Config.DeliverConfigSnapshot
import Network.AWS.Config.DescribeAggregateComplianceByConfigRules
import Network.AWS.Config.DescribeAggregateComplianceByConformancePacks
import Network.AWS.Config.DescribeAggregationAuthorizations
import Network.AWS.Config.DescribeComplianceByConfigRule
import Network.AWS.Config.DescribeComplianceByResource
import Network.AWS.Config.DescribeConfigRuleEvaluationStatus
import Network.AWS.Config.DescribeConfigRules
import Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
import Network.AWS.Config.DescribeConfigurationAggregators
import Network.AWS.Config.DescribeConfigurationRecorderStatus
import Network.AWS.Config.DescribeConfigurationRecorders
import Network.AWS.Config.DescribeConformancePackCompliance
import Network.AWS.Config.DescribeConformancePackStatus
import Network.AWS.Config.DescribeConformancePacks
import Network.AWS.Config.DescribeDeliveryChannelStatus
import Network.AWS.Config.DescribeDeliveryChannels
import Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
import Network.AWS.Config.DescribeOrganizationConfigRules
import Network.AWS.Config.DescribeOrganizationConformancePackStatuses
import Network.AWS.Config.DescribeOrganizationConformancePacks
import Network.AWS.Config.DescribePendingAggregationRequests
import Network.AWS.Config.DescribeRemediationConfigurations
import Network.AWS.Config.DescribeRemediationExceptions
import Network.AWS.Config.DescribeRemediationExecutionStatus
import Network.AWS.Config.DescribeRetentionConfigurations
import Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
import Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
import Network.AWS.Config.GetAggregateConformancePackComplianceSummary
import Network.AWS.Config.GetAggregateDiscoveredResourceCounts
import Network.AWS.Config.GetAggregateResourceConfig
import Network.AWS.Config.GetComplianceDetailsByConfigRule
import Network.AWS.Config.GetComplianceDetailsByResource
import Network.AWS.Config.GetComplianceSummaryByConfigRule
import Network.AWS.Config.GetComplianceSummaryByResourceType
import Network.AWS.Config.GetConformancePackComplianceDetails
import Network.AWS.Config.GetConformancePackComplianceSummary
import Network.AWS.Config.GetDiscoveredResourceCounts
import Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
import Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
import Network.AWS.Config.GetResourceConfigHistory
import Network.AWS.Config.GetStoredQuery
import Network.AWS.Config.ListAggregateDiscoveredResources
import Network.AWS.Config.ListDiscoveredResources
import Network.AWS.Config.ListStoredQueries
import Network.AWS.Config.ListTagsForResource
import Network.AWS.Config.PutAggregationAuthorization
import Network.AWS.Config.PutConfigRule
import Network.AWS.Config.PutConfigurationAggregator
import Network.AWS.Config.PutConfigurationRecorder
import Network.AWS.Config.PutConformancePack
import Network.AWS.Config.PutDeliveryChannel
import Network.AWS.Config.PutEvaluations
import Network.AWS.Config.PutExternalEvaluation
import Network.AWS.Config.PutOrganizationConfigRule
import Network.AWS.Config.PutOrganizationConformancePack
import Network.AWS.Config.PutRemediationConfigurations
import Network.AWS.Config.PutRemediationExceptions
import Network.AWS.Config.PutResourceConfig
import Network.AWS.Config.PutRetentionConfiguration
import Network.AWS.Config.PutStoredQuery
import Network.AWS.Config.SelectAggregateResourceConfig
import Network.AWS.Config.SelectResourceConfig
import Network.AWS.Config.StartConfigRulesEvaluation
import Network.AWS.Config.StartConfigurationRecorder
import Network.AWS.Config.StartRemediationExecution
import Network.AWS.Config.StopConfigurationRecorder
import Network.AWS.Config.TagResource
import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.AggregateComplianceByConfigRule
import Network.AWS.Config.Types.AggregateComplianceByConformancePack
import Network.AWS.Config.Types.AggregateComplianceCount
import Network.AWS.Config.Types.AggregateConformancePackCompliance
import Network.AWS.Config.Types.AggregateConformancePackComplianceCount
import Network.AWS.Config.Types.AggregateConformancePackComplianceFilters
import Network.AWS.Config.Types.AggregateConformancePackComplianceSummary
import Network.AWS.Config.Types.AggregateConformancePackComplianceSummaryFilters
import Network.AWS.Config.Types.AggregateEvaluationResult
import Network.AWS.Config.Types.AggregateResourceIdentifier
import Network.AWS.Config.Types.AggregatedSourceStatus
import Network.AWS.Config.Types.AggregationAuthorization
import Network.AWS.Config.Types.BaseConfigurationItem
import Network.AWS.Config.Types.Compliance
import Network.AWS.Config.Types.ComplianceByConfigRule
import Network.AWS.Config.Types.ComplianceByResource
import Network.AWS.Config.Types.ComplianceContributorCount
import Network.AWS.Config.Types.ComplianceSummary
import Network.AWS.Config.Types.ComplianceSummaryByResourceType
import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigRule
import Network.AWS.Config.Types.ConfigRuleComplianceFilters
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
import Network.AWS.Config.Types.ConfigRuleEvaluationStatus
import Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import Network.AWS.Config.Types.ConfigurationAggregator
import Network.AWS.Config.Types.ConfigurationItem
import Network.AWS.Config.Types.ConfigurationRecorder
import Network.AWS.Config.Types.ConfigurationRecorderStatus
import Network.AWS.Config.Types.ConformancePackComplianceFilters
import Network.AWS.Config.Types.ConformancePackComplianceSummary
import Network.AWS.Config.Types.ConformancePackDetail
import Network.AWS.Config.Types.ConformancePackEvaluationFilters
import Network.AWS.Config.Types.ConformancePackEvaluationResult
import Network.AWS.Config.Types.ConformancePackInputParameter
import Network.AWS.Config.Types.ConformancePackRuleCompliance
import Network.AWS.Config.Types.ConformancePackStatusDetail
import Network.AWS.Config.Types.DeliveryChannel
import Network.AWS.Config.Types.DeliveryChannelStatus
import Network.AWS.Config.Types.Evaluation
import Network.AWS.Config.Types.EvaluationResult
import Network.AWS.Config.Types.EvaluationResultIdentifier
import Network.AWS.Config.Types.EvaluationResultQualifier
import Network.AWS.Config.Types.ExecutionControls
import Network.AWS.Config.Types.ExternalEvaluation
import Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
import Network.AWS.Config.Types.FailedRemediationBatch
import Network.AWS.Config.Types.FailedRemediationExceptionBatch
import Network.AWS.Config.Types.FieldInfo
import Network.AWS.Config.Types.GroupedResourceCount
import Network.AWS.Config.Types.MemberAccountStatus
import Network.AWS.Config.Types.OrganizationAggregationSource
import Network.AWS.Config.Types.OrganizationConfigRule
import Network.AWS.Config.Types.OrganizationConfigRuleStatus
import Network.AWS.Config.Types.OrganizationConformancePack
import Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
import Network.AWS.Config.Types.OrganizationConformancePackStatus
import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
import Network.AWS.Config.Types.PendingAggregationRequest
import Network.AWS.Config.Types.QueryInfo
import Network.AWS.Config.Types.RecordingGroup
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.RemediationConfiguration
import Network.AWS.Config.Types.RemediationException
import Network.AWS.Config.Types.RemediationExceptionResourceKey
import Network.AWS.Config.Types.RemediationExecutionStatus
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.RemediationParameterValue
import Network.AWS.Config.Types.ResourceCount
import Network.AWS.Config.Types.ResourceCountFilters
import Network.AWS.Config.Types.ResourceFilters
import Network.AWS.Config.Types.ResourceIdentifier
import Network.AWS.Config.Types.ResourceKey
import Network.AWS.Config.Types.ResourceValue
import Network.AWS.Config.Types.RetentionConfiguration
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import Network.AWS.Config.Types.SourceDetail
import Network.AWS.Config.Types.SsmControls
import Network.AWS.Config.Types.StaticValue
import Network.AWS.Config.Types.StatusDetailFilters
import Network.AWS.Config.Types.StoredQuery
import Network.AWS.Config.Types.StoredQueryMetadata
import Network.AWS.Config.Types.Tag
import Network.AWS.Config.UntagResource

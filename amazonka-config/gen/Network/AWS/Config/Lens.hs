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

    -- ** DescribeComplianceByConfigRule
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_configRuleNames,
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_httpStatus,

    -- ** GetAggregateComplianceDetailsByConfigRule
    getAggregateComplianceDetailsByConfigRule_nextToken,
    getAggregateComplianceDetailsByConfigRule_complianceType,
    getAggregateComplianceDetailsByConfigRule_limit,
    getAggregateComplianceDetailsByConfigRule_configurationAggregatorName,
    getAggregateComplianceDetailsByConfigRule_configRuleName,
    getAggregateComplianceDetailsByConfigRule_accountId,
    getAggregateComplianceDetailsByConfigRule_awsRegion,
    getAggregateComplianceDetailsByConfigRuleResponse_nextToken,
    getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults,
    getAggregateComplianceDetailsByConfigRuleResponse_httpStatus,

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
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregators_limit,
    describeConfigurationAggregatorsResponse_nextToken,
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_httpStatus,

    -- ** DescribePendingAggregationRequests
    describePendingAggregationRequests_nextToken,
    describePendingAggregationRequests_limit,
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,

    -- ** DescribeRemediationExecutionStatus
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_configRuleName,
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_httpStatus,

    -- ** GetResourceConfigHistory
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_chronologicalOrder,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_resourceType,
    getResourceConfigHistory_resourceId,
    getResourceConfigHistoryResponse_nextToken,
    getResourceConfigHistoryResponse_configurationItems,
    getResourceConfigHistoryResponse_httpStatus,

    -- ** DescribeConformancePackCompliance
    describeConformancePackCompliance_nextToken,
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_conformancePackName,
    describeConformancePackComplianceResponse_nextToken,
    describeConformancePackComplianceResponse_httpStatus,
    describeConformancePackComplianceResponse_conformancePackName,
    describeConformancePackComplianceResponse_conformancePackRuleComplianceList,

    -- ** DescribeOrganizationConformancePacks
    describeOrganizationConformancePacks_nextToken,
    describeOrganizationConformancePacks_organizationConformancePackNames,
    describeOrganizationConformancePacks_limit,
    describeOrganizationConformancePacksResponse_nextToken,
    describeOrganizationConformancePacksResponse_organizationConformancePacks,
    describeOrganizationConformancePacksResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_nextToken,
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_limit,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_resourceIdentifiers,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** DescribeAggregationAuthorizations
    describeAggregationAuthorizations_nextToken,
    describeAggregationAuthorizations_limit,
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_httpStatus,

    -- ** DescribeRemediationConfigurations
    describeRemediationConfigurations_configRuleNames,
    describeRemediationConfigurationsResponse_remediationConfigurations,
    describeRemediationConfigurationsResponse_httpStatus,

    -- ** DeleteResourceConfig
    deleteResourceConfig_resourceType,
    deleteResourceConfig_resourceId,

    -- ** DescribeComplianceByResource
    describeComplianceByResource_resourceId,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_complianceTypes,
    describeComplianceByResource_resourceType,
    describeComplianceByResource_limit,
    describeComplianceByResourceResponse_nextToken,
    describeComplianceByResourceResponse_complianceByResources,
    describeComplianceByResourceResponse_httpStatus,

    -- ** StartConfigRulesEvaluation
    startConfigRulesEvaluation_configRuleNames,
    startConfigRulesEvaluationResponse_httpStatus,

    -- ** DescribeRemediationExceptions
    describeRemediationExceptions_nextToken,
    describeRemediationExceptions_resourceKeys,
    describeRemediationExceptions_limit,
    describeRemediationExceptions_configRuleName,
    describeRemediationExceptionsResponse_nextToken,
    describeRemediationExceptionsResponse_remediationExceptions,
    describeRemediationExceptionsResponse_httpStatus,

    -- ** DeleteOrganizationConformancePack
    deleteOrganizationConformancePack_organizationConformancePackName,

    -- ** GetComplianceDetailsByConfigRule
    getComplianceDetailsByConfigRule_nextToken,
    getComplianceDetailsByConfigRule_complianceTypes,
    getComplianceDetailsByConfigRule_limit,
    getComplianceDetailsByConfigRule_configRuleName,
    getComplianceDetailsByConfigRuleResponse_nextToken,
    getComplianceDetailsByConfigRuleResponse_evaluationResults,
    getComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** PutOrganizationConfigRule
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationConfigRuleName,
    putOrganizationConfigRuleResponse_organizationConfigRuleArn,
    putOrganizationConfigRuleResponse_httpStatus,

    -- ** GetDiscoveredResourceCounts
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_resourceTypes,
    getDiscoveredResourceCounts_limit,
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_httpStatus,

    -- ** GetComplianceSummaryByResourceType
    getComplianceSummaryByResourceType_resourceTypes,
    getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType,
    getComplianceSummaryByResourceTypeResponse_httpStatus,

    -- ** PutDeliveryChannel
    putDeliveryChannel_deliveryChannel,

    -- ** DescribeConfigurationAggregatorSourcesStatus
    describeConfigurationAggregatorSourcesStatus_nextToken,
    describeConfigurationAggregatorSourcesStatus_updateStatus,
    describeConfigurationAggregatorSourcesStatus_limit,
    describeConfigurationAggregatorSourcesStatus_configurationAggregatorName,
    describeConfigurationAggregatorSourcesStatusResponse_nextToken,
    describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList,
    describeConfigurationAggregatorSourcesStatusResponse_httpStatus,

    -- ** DeleteRemediationConfiguration
    deleteRemediationConfiguration_resourceType,
    deleteRemediationConfiguration_configRuleName,
    deleteRemediationConfigurationResponse_httpStatus,

    -- ** DeleteAggregationAuthorization
    deleteAggregationAuthorization_authorizedAccountId,
    deleteAggregationAuthorization_authorizedAwsRegion,

    -- ** DescribeConfigurationRecorderStatus
    describeConfigurationRecorderStatus_configurationRecorderNames,
    describeConfigurationRecorderStatusResponse_configurationRecordersStatus,
    describeConfigurationRecorderStatusResponse_httpStatus,

    -- ** DeleteConfigurationRecorder
    deleteConfigurationRecorder_configurationRecorderName,

    -- ** GetConformancePackComplianceSummary
    getConformancePackComplianceSummary_nextToken,
    getConformancePackComplianceSummary_limit,
    getConformancePackComplianceSummary_conformancePackNames,
    getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList,
    getConformancePackComplianceSummaryResponse_nextToken,
    getConformancePackComplianceSummaryResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DescribeConfigRuleEvaluationStatus
    describeConfigRuleEvaluationStatus_nextToken,
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_limit,
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_httpStatus,

    -- ** DeleteConfigurationAggregator
    deleteConfigurationAggregator_configurationAggregatorName,

    -- ** DeleteStoredQuery
    deleteStoredQuery_queryName,
    deleteStoredQueryResponse_httpStatus,

    -- ** DescribeOrganizationConfigRuleStatuses
    describeOrganizationConfigRuleStatuses_nextToken,
    describeOrganizationConfigRuleStatuses_organizationConfigRuleNames,
    describeOrganizationConfigRuleStatuses_limit,
    describeOrganizationConfigRuleStatusesResponse_nextToken,
    describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses,
    describeOrganizationConfigRuleStatusesResponse_httpStatus,

    -- ** GetComplianceDetailsByResource
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_resourceType,
    getComplianceDetailsByResource_resourceId,
    getComplianceDetailsByResourceResponse_nextToken,
    getComplianceDetailsByResourceResponse_evaluationResults,
    getComplianceDetailsByResourceResponse_httpStatus,

    -- ** ListAggregateDiscoveredResources
    listAggregateDiscoveredResources_nextToken,
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_configurationAggregatorName,
    listAggregateDiscoveredResources_resourceType,
    listAggregateDiscoveredResourcesResponse_nextToken,
    listAggregateDiscoveredResourcesResponse_resourceIdentifiers,
    listAggregateDiscoveredResourcesResponse_httpStatus,

    -- ** SelectResourceConfig
    selectResourceConfig_nextToken,
    selectResourceConfig_limit,
    selectResourceConfig_expression,
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_queryInfo,
    selectResourceConfigResponse_results,
    selectResourceConfigResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** PutRetentionConfiguration
    putRetentionConfiguration_retentionPeriodInDays,
    putRetentionConfigurationResponse_retentionConfiguration,
    putRetentionConfigurationResponse_httpStatus,

    -- ** GetConformancePackComplianceDetails
    getConformancePackComplianceDetails_nextToken,
    getConformancePackComplianceDetails_filters,
    getConformancePackComplianceDetails_limit,
    getConformancePackComplianceDetails_conformancePackName,
    getConformancePackComplianceDetailsResponse_nextToken,
    getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults,
    getConformancePackComplianceDetailsResponse_httpStatus,
    getConformancePackComplianceDetailsResponse_conformancePackName,

    -- ** DeleteEvaluationResults
    deleteEvaluationResults_configRuleName,
    deleteEvaluationResultsResponse_httpStatus,

    -- ** PutRemediationConfigurations
    putRemediationConfigurations_remediationConfigurations,
    putRemediationConfigurationsResponse_failedBatches,
    putRemediationConfigurationsResponse_httpStatus,

    -- ** PutConfigRule
    putConfigRule_tags,
    putConfigRule_configRule,

    -- ** PutConformancePack
    putConformancePack_templateS3Uri,
    putConformancePack_deliveryS3Bucket,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_templateBody,
    putConformancePack_conformancePackInputParameters,
    putConformancePack_conformancePackName,
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,

    -- ** ListStoredQueries
    listStoredQueries_nextToken,
    listStoredQueries_maxResults,
    listStoredQueriesResponse_nextToken,
    listStoredQueriesResponse_storedQueryMetadata,
    listStoredQueriesResponse_httpStatus,

    -- ** DeleteRemediationExceptions
    deleteRemediationExceptions_configRuleName,
    deleteRemediationExceptions_resourceKeys,
    deleteRemediationExceptionsResponse_failedBatches,
    deleteRemediationExceptionsResponse_httpStatus,

    -- ** PutExternalEvaluation
    putExternalEvaluation_configRuleName,
    putExternalEvaluation_externalEvaluation,
    putExternalEvaluationResponse_httpStatus,

    -- ** BatchGetResourceConfig
    batchGetResourceConfig_resourceKeys,
    batchGetResourceConfigResponse_unprocessedResourceKeys,
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_httpStatus,

    -- ** GetAggregateDiscoveredResourceCounts
    getAggregateDiscoveredResourceCounts_nextToken,
    getAggregateDiscoveredResourceCounts_filters,
    getAggregateDiscoveredResourceCounts_groupByKey,
    getAggregateDiscoveredResourceCounts_limit,
    getAggregateDiscoveredResourceCounts_configurationAggregatorName,
    getAggregateDiscoveredResourceCountsResponse_nextToken,
    getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts,
    getAggregateDiscoveredResourceCountsResponse_groupByKey,
    getAggregateDiscoveredResourceCountsResponse_httpStatus,
    getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources,

    -- ** PutEvaluations
    putEvaluations_testMode,
    putEvaluations_evaluations,
    putEvaluations_resultToken,
    putEvaluationsResponse_failedEvaluations,
    putEvaluationsResponse_httpStatus,

    -- ** DescribeConfigurationRecorders
    describeConfigurationRecorders_configurationRecorderNames,
    describeConfigurationRecordersResponse_configurationRecorders,
    describeConfigurationRecordersResponse_httpStatus,

    -- ** DescribeOrganizationConfigRules
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_organizationConfigRuleNames,
    describeOrganizationConfigRules_limit,
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_httpStatus,

    -- ** SelectAggregateResourceConfig
    selectAggregateResourceConfig_nextToken,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_expression,
    selectAggregateResourceConfig_configurationAggregatorName,
    selectAggregateResourceConfigResponse_nextToken,
    selectAggregateResourceConfigResponse_queryInfo,
    selectAggregateResourceConfigResponse_results,
    selectAggregateResourceConfigResponse_httpStatus,

    -- ** PutResourceConfig
    putResourceConfig_tags,
    putResourceConfig_resourceName,
    putResourceConfig_resourceType,
    putResourceConfig_schemaVersionId,
    putResourceConfig_resourceId,
    putResourceConfig_configuration,

    -- ** DescribeDeliveryChannels
    describeDeliveryChannels_deliveryChannelNames,
    describeDeliveryChannelsResponse_deliveryChannels,
    describeDeliveryChannelsResponse_httpStatus,

    -- ** DeleteDeliveryChannel
    deleteDeliveryChannel_deliveryChannelName,

    -- ** DescribeDeliveryChannelStatus
    describeDeliveryChannelStatus_deliveryChannelNames,
    describeDeliveryChannelStatusResponse_deliveryChannelsStatus,
    describeDeliveryChannelStatusResponse_httpStatus,

    -- ** DeleteOrganizationConfigRule
    deleteOrganizationConfigRule_organizationConfigRuleName,

    -- ** PutAggregationAuthorization
    putAggregationAuthorization_tags,
    putAggregationAuthorization_authorizedAccountId,
    putAggregationAuthorization_authorizedAwsRegion,
    putAggregationAuthorizationResponse_aggregationAuthorization,
    putAggregationAuthorizationResponse_httpStatus,

    -- ** PutOrganizationConformancePack
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_organizationConformancePackName,
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,

    -- ** PutConfigurationRecorder
    putConfigurationRecorder_configurationRecorder,

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

    -- ** GetComplianceSummaryByConfigRule
    getComplianceSummaryByConfigRuleResponse_complianceSummary,
    getComplianceSummaryByConfigRuleResponse_httpStatus,

    -- ** DescribeOrganizationConformancePackStatuses
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,
    describeOrganizationConformancePackStatuses_limit,
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_httpStatus,

    -- ** PutStoredQuery
    putStoredQuery_tags,
    putStoredQuery_storedQuery,
    putStoredQueryResponse_queryArn,
    putStoredQueryResponse_httpStatus,

    -- ** PutConfigurationAggregator
    putConfigurationAggregator_tags,
    putConfigurationAggregator_accountAggregationSources,
    putConfigurationAggregator_organizationAggregationSource,
    putConfigurationAggregator_configurationAggregatorName,
    putConfigurationAggregatorResponse_configurationAggregator,
    putConfigurationAggregatorResponse_httpStatus,

    -- ** GetOrganizationConfigRuleDetailedStatus
    getOrganizationConfigRuleDetailedStatus_nextToken,
    getOrganizationConfigRuleDetailedStatus_filters,
    getOrganizationConfigRuleDetailedStatus_limit,
    getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName,
    getOrganizationConfigRuleDetailedStatusResponse_nextToken,
    getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus,
    getOrganizationConfigRuleDetailedStatusResponse_httpStatus,

    -- ** StartRemediationExecution
    startRemediationExecution_configRuleName,
    startRemediationExecution_resourceKeys,
    startRemediationExecutionResponse_failureMessage,
    startRemediationExecutionResponse_failedItems,
    startRemediationExecutionResponse_httpStatus,

    -- ** DescribeConformancePackStatus
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatus_limit,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_httpStatus,

    -- ** DeleteConfigRule
    deleteConfigRule_configRuleName,

    -- ** DeleteRetentionConfiguration
    deleteRetentionConfiguration_retentionConfigurationName,

    -- ** DeleteConformancePack
    deleteConformancePack_conformancePackName,

    -- ** GetOrganizationConformancePackDetailedStatus
    getOrganizationConformancePackDetailedStatus_nextToken,
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_organizationConformancePackName,
    getOrganizationConformancePackDetailedStatusResponse_nextToken,
    getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses,
    getOrganizationConformancePackDetailedStatusResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConfigRules
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,

    -- ** StartConfigurationRecorder
    startConfigurationRecorder_configurationRecorderName,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetAggregateConfigRuleComplianceSummary
    getAggregateConfigRuleComplianceSummary_nextToken,
    getAggregateConfigRuleComplianceSummary_filters,
    getAggregateConfigRuleComplianceSummary_groupByKey,
    getAggregateConfigRuleComplianceSummary_limit,
    getAggregateConfigRuleComplianceSummary_configurationAggregatorName,
    getAggregateConfigRuleComplianceSummaryResponse_nextToken,
    getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts,
    getAggregateConfigRuleComplianceSummaryResponse_groupByKey,
    getAggregateConfigRuleComplianceSummaryResponse_httpStatus,

    -- ** PutRemediationExceptions
    putRemediationExceptions_expirationTime,
    putRemediationExceptions_message,
    putRemediationExceptions_configRuleName,
    putRemediationExceptions_resourceKeys,
    putRemediationExceptionsResponse_failedBatches,
    putRemediationExceptionsResponse_httpStatus,

    -- ** DescribeConfigRules
    describeConfigRules_nextToken,
    describeConfigRules_configRuleNames,
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_httpStatus,

    -- ** StopConfigurationRecorder
    stopConfigurationRecorder_configurationRecorderName,

    -- ** DescribeConformancePacks
    describeConformancePacks_nextToken,
    describeConformancePacks_conformancePackNames,
    describeConformancePacks_limit,
    describeConformancePacksResponse_nextToken,
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_httpStatus,

    -- ** DescribeRetentionConfigurations
    describeRetentionConfigurations_nextToken,
    describeRetentionConfigurations_retentionConfigurationNames,
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_httpStatus,

    -- * Types

    -- ** AccountAggregationSource
    accountAggregationSource_allAwsRegions,
    accountAggregationSource_awsRegions,
    accountAggregationSource_accountIds,

    -- ** AggregateComplianceByConfigRule
    aggregateComplianceByConfigRule_accountId,
    aggregateComplianceByConfigRule_configRuleName,
    aggregateComplianceByConfigRule_compliance,
    aggregateComplianceByConfigRule_awsRegion,

    -- ** AggregateComplianceCount
    aggregateComplianceCount_complianceSummary,
    aggregateComplianceCount_groupName,

    -- ** AggregateEvaluationResult
    aggregateEvaluationResult_annotation,
    aggregateEvaluationResult_evaluationResultIdentifier,
    aggregateEvaluationResult_accountId,
    aggregateEvaluationResult_resultRecordedTime,
    aggregateEvaluationResult_complianceType,
    aggregateEvaluationResult_configRuleInvokedTime,
    aggregateEvaluationResult_awsRegion,

    -- ** AggregateResourceIdentifier
    aggregateResourceIdentifier_resourceName,
    aggregateResourceIdentifier_sourceAccountId,
    aggregateResourceIdentifier_sourceRegion,
    aggregateResourceIdentifier_resourceId,
    aggregateResourceIdentifier_resourceType,

    -- ** AggregatedSourceStatus
    aggregatedSourceStatus_lastUpdateStatus,
    aggregatedSourceStatus_lastErrorMessage,
    aggregatedSourceStatus_lastUpdateTime,
    aggregatedSourceStatus_sourceId,
    aggregatedSourceStatus_lastErrorCode,
    aggregatedSourceStatus_awsRegion,
    aggregatedSourceStatus_sourceType,

    -- ** AggregationAuthorization
    aggregationAuthorization_creationTime,
    aggregationAuthorization_authorizedAccountId,
    aggregationAuthorization_authorizedAwsRegion,
    aggregationAuthorization_aggregationAuthorizationArn,

    -- ** BaseConfigurationItem
    baseConfigurationItem_resourceId,
    baseConfigurationItem_accountId,
    baseConfigurationItem_configuration,
    baseConfigurationItem_arn,
    baseConfigurationItem_version,
    baseConfigurationItem_configurationStateId,
    baseConfigurationItem_resourceType,
    baseConfigurationItem_supplementaryConfiguration,
    baseConfigurationItem_availabilityZone,
    baseConfigurationItem_configurationItemCaptureTime,
    baseConfigurationItem_configurationItemStatus,
    baseConfigurationItem_resourceCreationTime,
    baseConfigurationItem_awsRegion,
    baseConfigurationItem_resourceName,

    -- ** Compliance
    compliance_complianceType,
    compliance_complianceContributorCount,

    -- ** ComplianceByConfigRule
    complianceByConfigRule_configRuleName,
    complianceByConfigRule_compliance,

    -- ** ComplianceByResource
    complianceByResource_resourceId,
    complianceByResource_resourceType,
    complianceByResource_compliance,

    -- ** ComplianceContributorCount
    complianceContributorCount_capExceeded,
    complianceContributorCount_cappedCount,

    -- ** ComplianceSummary
    complianceSummary_complianceSummaryTimestamp,
    complianceSummary_nonCompliantResourceCount,
    complianceSummary_compliantResourceCount,

    -- ** ComplianceSummaryByResourceType
    complianceSummaryByResourceType_complianceSummary,
    complianceSummaryByResourceType_resourceType,

    -- ** ConfigExportDeliveryInfo
    configExportDeliveryInfo_lastErrorMessage,
    configExportDeliveryInfo_nextDeliveryTime,
    configExportDeliveryInfo_lastSuccessfulTime,
    configExportDeliveryInfo_lastErrorCode,
    configExportDeliveryInfo_lastStatus,
    configExportDeliveryInfo_lastAttemptTime,

    -- ** ConfigRule
    configRule_configRuleId,
    configRule_configRuleArn,
    configRule_maximumExecutionFrequency,
    configRule_configRuleName,
    configRule_configRuleState,
    configRule_scope,
    configRule_inputParameters,
    configRule_description,
    configRule_createdBy,
    configRule_source,

    -- ** ConfigRuleComplianceFilters
    configRuleComplianceFilters_accountId,
    configRuleComplianceFilters_configRuleName,
    configRuleComplianceFilters_complianceType,
    configRuleComplianceFilters_awsRegion,

    -- ** ConfigRuleComplianceSummaryFilters
    configRuleComplianceSummaryFilters_accountId,
    configRuleComplianceSummaryFilters_awsRegion,

    -- ** ConfigRuleEvaluationStatus
    configRuleEvaluationStatus_lastErrorMessage,
    configRuleEvaluationStatus_configRuleId,
    configRuleEvaluationStatus_configRuleArn,
    configRuleEvaluationStatus_configRuleName,
    configRuleEvaluationStatus_firstEvaluationStarted,
    configRuleEvaluationStatus_lastFailedEvaluationTime,
    configRuleEvaluationStatus_firstActivatedTime,
    configRuleEvaluationStatus_lastErrorCode,
    configRuleEvaluationStatus_lastFailedInvocationTime,
    configRuleEvaluationStatus_lastSuccessfulInvocationTime,
    configRuleEvaluationStatus_lastDeactivatedTime,
    configRuleEvaluationStatus_lastSuccessfulEvaluationTime,

    -- ** ConfigSnapshotDeliveryProperties
    configSnapshotDeliveryProperties_deliveryFrequency,

    -- ** ConfigStreamDeliveryInfo
    configStreamDeliveryInfo_lastErrorMessage,
    configStreamDeliveryInfo_lastErrorCode,
    configStreamDeliveryInfo_lastStatus,
    configStreamDeliveryInfo_lastStatusChangeTime,

    -- ** ConfigurationAggregator
    configurationAggregator_creationTime,
    configurationAggregator_configurationAggregatorArn,
    configurationAggregator_configurationAggregatorName,
    configurationAggregator_accountAggregationSources,
    configurationAggregator_createdBy,
    configurationAggregator_lastUpdatedTime,
    configurationAggregator_organizationAggregationSource,

    -- ** ConfigurationItem
    configurationItem_relationships,
    configurationItem_resourceId,
    configurationItem_accountId,
    configurationItem_relatedEvents,
    configurationItem_configuration,
    configurationItem_arn,
    configurationItem_version,
    configurationItem_configurationStateId,
    configurationItem_resourceType,
    configurationItem_supplementaryConfiguration,
    configurationItem_availabilityZone,
    configurationItem_configurationItemCaptureTime,
    configurationItem_configurationItemStatus,
    configurationItem_tags,
    configurationItem_resourceCreationTime,
    configurationItem_configurationItemMD5Hash,
    configurationItem_awsRegion,
    configurationItem_resourceName,

    -- ** ConfigurationRecorder
    configurationRecorder_roleARN,
    configurationRecorder_name,
    configurationRecorder_recordingGroup,

    -- ** ConfigurationRecorderStatus
    configurationRecorderStatus_lastStopTime,
    configurationRecorderStatus_lastStartTime,
    configurationRecorderStatus_lastErrorMessage,
    configurationRecorderStatus_recording,
    configurationRecorderStatus_name,
    configurationRecorderStatus_lastErrorCode,
    configurationRecorderStatus_lastStatus,
    configurationRecorderStatus_lastStatusChangeTime,

    -- ** ConformancePackComplianceFilters
    conformancePackComplianceFilters_complianceType,
    conformancePackComplianceFilters_configRuleNames,

    -- ** ConformancePackComplianceSummary
    conformancePackComplianceSummary_conformancePackName,
    conformancePackComplianceSummary_conformancePackComplianceStatus,

    -- ** ConformancePackDetail
    conformancePackDetail_lastUpdateRequestedTime,
    conformancePackDetail_deliveryS3Bucket,
    conformancePackDetail_deliveryS3KeyPrefix,
    conformancePackDetail_createdBy,
    conformancePackDetail_conformancePackInputParameters,
    conformancePackDetail_conformancePackName,
    conformancePackDetail_conformancePackArn,
    conformancePackDetail_conformancePackId,

    -- ** ConformancePackEvaluationFilters
    conformancePackEvaluationFilters_complianceType,
    conformancePackEvaluationFilters_resourceType,
    conformancePackEvaluationFilters_resourceIds,
    conformancePackEvaluationFilters_configRuleNames,

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
    deliveryChannel_s3KmsKeyArn,
    deliveryChannel_name,
    deliveryChannel_s3KeyPrefix,
    deliveryChannel_s3BucketName,
    deliveryChannel_configSnapshotDeliveryProperties,
    deliveryChannel_snsTopicARN,

    -- ** DeliveryChannelStatus
    deliveryChannelStatus_configSnapshotDeliveryInfo,
    deliveryChannelStatus_configStreamDeliveryInfo,
    deliveryChannelStatus_name,
    deliveryChannelStatus_configHistoryDeliveryInfo,

    -- ** Evaluation
    evaluation_annotation,
    evaluation_complianceResourceType,
    evaluation_complianceResourceId,
    evaluation_complianceType,
    evaluation_orderingTimestamp,

    -- ** EvaluationResult
    evaluationResult_annotation,
    evaluationResult_evaluationResultIdentifier,
    evaluationResult_resultRecordedTime,
    evaluationResult_complianceType,
    evaluationResult_configRuleInvokedTime,
    evaluationResult_resultToken,

    -- ** EvaluationResultIdentifier
    evaluationResultIdentifier_evaluationResultQualifier,
    evaluationResultIdentifier_orderingTimestamp,

    -- ** EvaluationResultQualifier
    evaluationResultQualifier_resourceId,
    evaluationResultQualifier_configRuleName,
    evaluationResultQualifier_resourceType,

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
    memberAccountStatus_lastUpdateTime,
    memberAccountStatus_errorMessage,
    memberAccountStatus_errorCode,
    memberAccountStatus_accountId,
    memberAccountStatus_configRuleName,
    memberAccountStatus_memberAccountRuleStatus,

    -- ** OrganizationAggregationSource
    organizationAggregationSource_allAwsRegions,
    organizationAggregationSource_awsRegions,
    organizationAggregationSource_roleArn,

    -- ** OrganizationConfigRule
    organizationConfigRule_lastUpdateTime,
    organizationConfigRule_organizationManagedRuleMetadata,
    organizationConfigRule_organizationCustomRuleMetadata,
    organizationConfigRule_excludedAccounts,
    organizationConfigRule_organizationConfigRuleName,
    organizationConfigRule_organizationConfigRuleArn,

    -- ** OrganizationConfigRuleStatus
    organizationConfigRuleStatus_lastUpdateTime,
    organizationConfigRuleStatus_errorMessage,
    organizationConfigRuleStatus_errorCode,
    organizationConfigRuleStatus_organizationConfigRuleName,
    organizationConfigRuleStatus_organizationRuleStatus,

    -- ** OrganizationConformancePack
    organizationConformancePack_deliveryS3Bucket,
    organizationConformancePack_deliveryS3KeyPrefix,
    organizationConformancePack_excludedAccounts,
    organizationConformancePack_conformancePackInputParameters,
    organizationConformancePack_organizationConformancePackName,
    organizationConformancePack_organizationConformancePackArn,
    organizationConformancePack_lastUpdateTime,

    -- ** OrganizationConformancePackDetailedStatus
    organizationConformancePackDetailedStatus_lastUpdateTime,
    organizationConformancePackDetailedStatus_errorMessage,
    organizationConformancePackDetailedStatus_errorCode,
    organizationConformancePackDetailedStatus_accountId,
    organizationConformancePackDetailedStatus_conformancePackName,
    organizationConformancePackDetailedStatus_status,

    -- ** OrganizationConformancePackStatus
    organizationConformancePackStatus_lastUpdateTime,
    organizationConformancePackStatus_errorMessage,
    organizationConformancePackStatus_errorCode,
    organizationConformancePackStatus_organizationConformancePackName,
    organizationConformancePackStatus_status,

    -- ** OrganizationCustomRuleMetadata
    organizationCustomRuleMetadata_tagKeyScope,
    organizationCustomRuleMetadata_maximumExecutionFrequency,
    organizationCustomRuleMetadata_resourceIdScope,
    organizationCustomRuleMetadata_inputParameters,
    organizationCustomRuleMetadata_description,
    organizationCustomRuleMetadata_resourceTypesScope,
    organizationCustomRuleMetadata_tagValueScope,
    organizationCustomRuleMetadata_lambdaFunctionArn,
    organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes,

    -- ** OrganizationManagedRuleMetadata
    organizationManagedRuleMetadata_tagKeyScope,
    organizationManagedRuleMetadata_maximumExecutionFrequency,
    organizationManagedRuleMetadata_resourceIdScope,
    organizationManagedRuleMetadata_inputParameters,
    organizationManagedRuleMetadata_description,
    organizationManagedRuleMetadata_resourceTypesScope,
    organizationManagedRuleMetadata_tagValueScope,
    organizationManagedRuleMetadata_ruleIdentifier,

    -- ** OrganizationResourceDetailedStatusFilters
    organizationResourceDetailedStatusFilters_status,
    organizationResourceDetailedStatusFilters_accountId,

    -- ** PendingAggregationRequest
    pendingAggregationRequest_requesterAwsRegion,
    pendingAggregationRequest_requesterAccountId,

    -- ** QueryInfo
    queryInfo_selectFields,

    -- ** RecordingGroup
    recordingGroup_allSupported,
    recordingGroup_resourceTypes,
    recordingGroup_includeGlobalResourceTypes,

    -- ** Relationship
    relationship_resourceId,
    relationship_resourceType,
    relationship_relationshipName,
    relationship_resourceName,

    -- ** RemediationConfiguration
    remediationConfiguration_retryAttemptSeconds,
    remediationConfiguration_executionControls,
    remediationConfiguration_targetVersion,
    remediationConfiguration_arn,
    remediationConfiguration_automatic,
    remediationConfiguration_resourceType,
    remediationConfiguration_createdByService,
    remediationConfiguration_maximumAutomaticAttempts,
    remediationConfiguration_parameters,
    remediationConfiguration_configRuleName,
    remediationConfiguration_targetType,
    remediationConfiguration_targetId,

    -- ** RemediationException
    remediationException_expirationTime,
    remediationException_message,
    remediationException_configRuleName,
    remediationException_resourceType,
    remediationException_resourceId,

    -- ** RemediationExceptionResourceKey
    remediationExceptionResourceKey_resourceId,
    remediationExceptionResourceKey_resourceType,

    -- ** RemediationExecutionStatus
    remediationExecutionStatus_invocationTime,
    remediationExecutionStatus_resourceKey,
    remediationExecutionStatus_state,
    remediationExecutionStatus_stepDetails,
    remediationExecutionStatus_lastUpdatedTime,

    -- ** RemediationExecutionStep
    remediationExecutionStep_startTime,
    remediationExecutionStep_stopTime,
    remediationExecutionStep_state,
    remediationExecutionStep_name,
    remediationExecutionStep_errorMessage,

    -- ** RemediationParameterValue
    remediationParameterValue_resourceValue,
    remediationParameterValue_staticValue,

    -- ** ResourceCount
    resourceCount_resourceType,
    resourceCount_count,

    -- ** ResourceCountFilters
    resourceCountFilters_accountId,
    resourceCountFilters_resourceType,
    resourceCountFilters_region,

    -- ** ResourceFilters
    resourceFilters_resourceId,
    resourceFilters_accountId,
    resourceFilters_region,
    resourceFilters_resourceName,

    -- ** ResourceIdentifier
    resourceIdentifier_resourceId,
    resourceIdentifier_resourceType,
    resourceIdentifier_resourceDeletionTime,
    resourceIdentifier_resourceName,

    -- ** ResourceKey
    resourceKey_resourceType,
    resourceKey_resourceId,

    -- ** ResourceValue
    resourceValue_value,

    -- ** RetentionConfiguration
    retentionConfiguration_name,
    retentionConfiguration_retentionPeriodInDays,

    -- ** Scope
    scope_tagValue,
    scope_tagKey,
    scope_complianceResourceId,
    scope_complianceResourceTypes,

    -- ** Source
    source_sourceDetails,
    source_owner,
    source_sourceIdentifier,

    -- ** SourceDetail
    sourceDetail_eventSource,
    sourceDetail_maximumExecutionFrequency,
    sourceDetail_messageType,

    -- ** SsmControls
    ssmControls_errorPercentage,
    ssmControls_concurrentExecutionRatePercentage,

    -- ** StaticValue
    staticValue_values,

    -- ** StatusDetailFilters
    statusDetailFilters_accountId,
    statusDetailFilters_memberAccountRuleStatus,

    -- ** StoredQuery
    storedQuery_queryArn,
    storedQuery_queryId,
    storedQuery_description,
    storedQuery_expression,
    storedQuery_queryName,

    -- ** StoredQueryMetadata
    storedQueryMetadata_description,
    storedQueryMetadata_queryId,
    storedQueryMetadata_queryArn,
    storedQueryMetadata_queryName,

    -- ** Tag
    tag_key,
    tag_value,
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
import Network.AWS.Config.Types.AggregateComplianceCount
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

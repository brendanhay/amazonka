{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Lens
  ( -- * Operations

    -- ** BatchGetAggregateResourceConfig
    batchGetAggregateResourceConfig_configurationAggregatorName,
    batchGetAggregateResourceConfig_resourceIdentifiers,
    batchGetAggregateResourceConfigResponse_baseConfigurationItems,
    batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers,
    batchGetAggregateResourceConfigResponse_httpStatus,

    -- ** BatchGetResourceConfig
    batchGetResourceConfig_resourceKeys,
    batchGetResourceConfigResponse_unprocessedResourceKeys,
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_httpStatus,

    -- ** DeleteAggregationAuthorization
    deleteAggregationAuthorization_authorizedAccountId,
    deleteAggregationAuthorization_authorizedAwsRegion,

    -- ** DeleteConfigRule
    deleteConfigRule_configRuleName,

    -- ** DeleteConfigurationAggregator
    deleteConfigurationAggregator_configurationAggregatorName,

    -- ** DeleteConfigurationRecorder
    deleteConfigurationRecorder_configurationRecorderName,

    -- ** DeleteConformancePack
    deleteConformancePack_conformancePackName,

    -- ** DeleteDeliveryChannel
    deleteDeliveryChannel_deliveryChannelName,

    -- ** DeleteEvaluationResults
    deleteEvaluationResults_configRuleName,
    deleteEvaluationResultsResponse_httpStatus,

    -- ** DeleteOrganizationConfigRule
    deleteOrganizationConfigRule_organizationConfigRuleName,

    -- ** DeleteOrganizationConformancePack
    deleteOrganizationConformancePack_organizationConformancePackName,

    -- ** DeletePendingAggregationRequest
    deletePendingAggregationRequest_requesterAccountId,
    deletePendingAggregationRequest_requesterAwsRegion,

    -- ** DeleteRemediationConfiguration
    deleteRemediationConfiguration_resourceType,
    deleteRemediationConfiguration_configRuleName,
    deleteRemediationConfigurationResponse_httpStatus,

    -- ** DeleteRemediationExceptions
    deleteRemediationExceptions_configRuleName,
    deleteRemediationExceptions_resourceKeys,
    deleteRemediationExceptionsResponse_failedBatches,
    deleteRemediationExceptionsResponse_httpStatus,

    -- ** DeleteResourceConfig
    deleteResourceConfig_resourceType,
    deleteResourceConfig_resourceId,

    -- ** DeleteRetentionConfiguration
    deleteRetentionConfiguration_retentionConfigurationName,

    -- ** DeleteStoredQuery
    deleteStoredQuery_queryName,
    deleteStoredQueryResponse_httpStatus,

    -- ** DeliverConfigSnapshot
    deliverConfigSnapshot_deliveryChannelName,
    deliverConfigSnapshotResponse_configSnapshotId,
    deliverConfigSnapshotResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConfigRules
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConformancePacks
    describeAggregateComplianceByConformancePacks_nextToken,
    describeAggregateComplianceByConformancePacks_filters,
    describeAggregateComplianceByConformancePacks_limit,
    describeAggregateComplianceByConformancePacks_configurationAggregatorName,
    describeAggregateComplianceByConformancePacksResponse_nextToken,
    describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks,
    describeAggregateComplianceByConformancePacksResponse_httpStatus,

    -- ** DescribeAggregationAuthorizations
    describeAggregationAuthorizations_nextToken,
    describeAggregationAuthorizations_limit,
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_httpStatus,

    -- ** DescribeComplianceByConfigRule
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_configRuleNames,
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_httpStatus,

    -- ** DescribeComplianceByResource
    describeComplianceByResource_resourceId,
    describeComplianceByResource_resourceType,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_complianceTypes,
    describeComplianceByResource_limit,
    describeComplianceByResourceResponse_nextToken,
    describeComplianceByResourceResponse_complianceByResources,
    describeComplianceByResourceResponse_httpStatus,

    -- ** DescribeConfigRuleEvaluationStatus
    describeConfigRuleEvaluationStatus_nextToken,
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_limit,
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_httpStatus,

    -- ** DescribeConfigRules
    describeConfigRules_nextToken,
    describeConfigRules_configRuleNames,
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_httpStatus,

    -- ** DescribeConfigurationAggregatorSourcesStatus
    describeConfigurationAggregatorSourcesStatus_nextToken,
    describeConfigurationAggregatorSourcesStatus_updateStatus,
    describeConfigurationAggregatorSourcesStatus_limit,
    describeConfigurationAggregatorSourcesStatus_configurationAggregatorName,
    describeConfigurationAggregatorSourcesStatusResponse_nextToken,
    describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList,
    describeConfigurationAggregatorSourcesStatusResponse_httpStatus,

    -- ** DescribeConfigurationAggregators
    describeConfigurationAggregators_nextToken,
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregators_limit,
    describeConfigurationAggregatorsResponse_nextToken,
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_httpStatus,

    -- ** DescribeConfigurationRecorderStatus
    describeConfigurationRecorderStatus_configurationRecorderNames,
    describeConfigurationRecorderStatusResponse_configurationRecordersStatus,
    describeConfigurationRecorderStatusResponse_httpStatus,

    -- ** DescribeConfigurationRecorders
    describeConfigurationRecorders_configurationRecorderNames,
    describeConfigurationRecordersResponse_configurationRecorders,
    describeConfigurationRecordersResponse_httpStatus,

    -- ** DescribeConformancePackCompliance
    describeConformancePackCompliance_nextToken,
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_conformancePackName,
    describeConformancePackComplianceResponse_nextToken,
    describeConformancePackComplianceResponse_httpStatus,
    describeConformancePackComplianceResponse_conformancePackName,
    describeConformancePackComplianceResponse_conformancePackRuleComplianceList,

    -- ** DescribeConformancePackStatus
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_limit,
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_httpStatus,

    -- ** DescribeConformancePacks
    describeConformancePacks_nextToken,
    describeConformancePacks_limit,
    describeConformancePacks_conformancePackNames,
    describeConformancePacksResponse_nextToken,
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_httpStatus,

    -- ** DescribeDeliveryChannelStatus
    describeDeliveryChannelStatus_deliveryChannelNames,
    describeDeliveryChannelStatusResponse_deliveryChannelsStatus,
    describeDeliveryChannelStatusResponse_httpStatus,

    -- ** DescribeDeliveryChannels
    describeDeliveryChannels_deliveryChannelNames,
    describeDeliveryChannelsResponse_deliveryChannels,
    describeDeliveryChannelsResponse_httpStatus,

    -- ** DescribeOrganizationConfigRuleStatuses
    describeOrganizationConfigRuleStatuses_nextToken,
    describeOrganizationConfigRuleStatuses_limit,
    describeOrganizationConfigRuleStatuses_organizationConfigRuleNames,
    describeOrganizationConfigRuleStatusesResponse_nextToken,
    describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses,
    describeOrganizationConfigRuleStatusesResponse_httpStatus,

    -- ** DescribeOrganizationConfigRules
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_limit,
    describeOrganizationConfigRules_organizationConfigRuleNames,
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_httpStatus,

    -- ** DescribeOrganizationConformancePackStatuses
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_limit,
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_httpStatus,

    -- ** DescribeOrganizationConformancePacks
    describeOrganizationConformancePacks_organizationConformancePackNames,
    describeOrganizationConformancePacks_nextToken,
    describeOrganizationConformancePacks_limit,
    describeOrganizationConformancePacksResponse_nextToken,
    describeOrganizationConformancePacksResponse_organizationConformancePacks,
    describeOrganizationConformancePacksResponse_httpStatus,

    -- ** DescribePendingAggregationRequests
    describePendingAggregationRequests_nextToken,
    describePendingAggregationRequests_limit,
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,

    -- ** DescribeRemediationConfigurations
    describeRemediationConfigurations_configRuleNames,
    describeRemediationConfigurationsResponse_remediationConfigurations,
    describeRemediationConfigurationsResponse_httpStatus,

    -- ** DescribeRemediationExceptions
    describeRemediationExceptions_nextToken,
    describeRemediationExceptions_resourceKeys,
    describeRemediationExceptions_limit,
    describeRemediationExceptions_configRuleName,
    describeRemediationExceptionsResponse_nextToken,
    describeRemediationExceptionsResponse_remediationExceptions,
    describeRemediationExceptionsResponse_httpStatus,

    -- ** DescribeRemediationExecutionStatus
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_configRuleName,
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_httpStatus,

    -- ** DescribeRetentionConfigurations
    describeRetentionConfigurations_nextToken,
    describeRetentionConfigurations_retentionConfigurationNames,
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_httpStatus,

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

    -- ** GetAggregateConformancePackComplianceSummary
    getAggregateConformancePackComplianceSummary_nextToken,
    getAggregateConformancePackComplianceSummary_filters,
    getAggregateConformancePackComplianceSummary_groupByKey,
    getAggregateConformancePackComplianceSummary_limit,
    getAggregateConformancePackComplianceSummary_configurationAggregatorName,
    getAggregateConformancePackComplianceSummaryResponse_nextToken,
    getAggregateConformancePackComplianceSummaryResponse_groupByKey,
    getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries,
    getAggregateConformancePackComplianceSummaryResponse_httpStatus,

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

    -- ** GetAggregateResourceConfig
    getAggregateResourceConfig_configurationAggregatorName,
    getAggregateResourceConfig_resourceIdentifier,
    getAggregateResourceConfigResponse_configurationItem,
    getAggregateResourceConfigResponse_httpStatus,

    -- ** GetComplianceDetailsByConfigRule
    getComplianceDetailsByConfigRule_nextToken,
    getComplianceDetailsByConfigRule_complianceTypes,
    getComplianceDetailsByConfigRule_limit,
    getComplianceDetailsByConfigRule_configRuleName,
    getComplianceDetailsByConfigRuleResponse_evaluationResults,
    getComplianceDetailsByConfigRuleResponse_nextToken,
    getComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** GetComplianceDetailsByResource
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_resourceType,
    getComplianceDetailsByResource_resourceId,
    getComplianceDetailsByResourceResponse_evaluationResults,
    getComplianceDetailsByResourceResponse_nextToken,
    getComplianceDetailsByResourceResponse_httpStatus,

    -- ** GetComplianceSummaryByConfigRule
    getComplianceSummaryByConfigRuleResponse_complianceSummary,
    getComplianceSummaryByConfigRuleResponse_httpStatus,

    -- ** GetComplianceSummaryByResourceType
    getComplianceSummaryByResourceType_resourceTypes,
    getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType,
    getComplianceSummaryByResourceTypeResponse_httpStatus,

    -- ** GetConformancePackComplianceDetails
    getConformancePackComplianceDetails_nextToken,
    getConformancePackComplianceDetails_filters,
    getConformancePackComplianceDetails_limit,
    getConformancePackComplianceDetails_conformancePackName,
    getConformancePackComplianceDetailsResponse_nextToken,
    getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults,
    getConformancePackComplianceDetailsResponse_httpStatus,
    getConformancePackComplianceDetailsResponse_conformancePackName,

    -- ** GetConformancePackComplianceSummary
    getConformancePackComplianceSummary_nextToken,
    getConformancePackComplianceSummary_limit,
    getConformancePackComplianceSummary_conformancePackNames,
    getConformancePackComplianceSummaryResponse_nextToken,
    getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList,
    getConformancePackComplianceSummaryResponse_httpStatus,

    -- ** GetCustomRulePolicy
    getCustomRulePolicy_configRuleName,
    getCustomRulePolicyResponse_policyText,
    getCustomRulePolicyResponse_httpStatus,

    -- ** GetDiscoveredResourceCounts
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_resourceTypes,
    getDiscoveredResourceCounts_limit,
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_httpStatus,

    -- ** GetOrganizationConfigRuleDetailedStatus
    getOrganizationConfigRuleDetailedStatus_nextToken,
    getOrganizationConfigRuleDetailedStatus_filters,
    getOrganizationConfigRuleDetailedStatus_limit,
    getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName,
    getOrganizationConfigRuleDetailedStatusResponse_nextToken,
    getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus,
    getOrganizationConfigRuleDetailedStatusResponse_httpStatus,

    -- ** GetOrganizationConformancePackDetailedStatus
    getOrganizationConformancePackDetailedStatus_nextToken,
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_organizationConformancePackName,
    getOrganizationConformancePackDetailedStatusResponse_nextToken,
    getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses,
    getOrganizationConformancePackDetailedStatusResponse_httpStatus,

    -- ** GetOrganizationCustomRulePolicy
    getOrganizationCustomRulePolicy_organizationConfigRuleName,
    getOrganizationCustomRulePolicyResponse_policyText,
    getOrganizationCustomRulePolicyResponse_httpStatus,

    -- ** GetResourceConfigHistory
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_chronologicalOrder,
    getResourceConfigHistory_resourceType,
    getResourceConfigHistory_resourceId,
    getResourceConfigHistoryResponse_nextToken,
    getResourceConfigHistoryResponse_configurationItems,
    getResourceConfigHistoryResponse_httpStatus,

    -- ** GetStoredQuery
    getStoredQuery_queryName,
    getStoredQueryResponse_storedQuery,
    getStoredQueryResponse_httpStatus,

    -- ** ListAggregateDiscoveredResources
    listAggregateDiscoveredResources_nextToken,
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_configurationAggregatorName,
    listAggregateDiscoveredResources_resourceType,
    listAggregateDiscoveredResourcesResponse_resourceIdentifiers,
    listAggregateDiscoveredResourcesResponse_nextToken,
    listAggregateDiscoveredResourcesResponse_httpStatus,

    -- ** ListConformancePackComplianceScores
    listConformancePackComplianceScores_sortOrder,
    listConformancePackComplianceScores_nextToken,
    listConformancePackComplianceScores_filters,
    listConformancePackComplianceScores_sortBy,
    listConformancePackComplianceScores_limit,
    listConformancePackComplianceScoresResponse_nextToken,
    listConformancePackComplianceScoresResponse_httpStatus,
    listConformancePackComplianceScoresResponse_conformancePackComplianceScores,

    -- ** ListDiscoveredResources
    listDiscoveredResources_nextToken,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_limit,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_resourceIdentifiers,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListStoredQueries
    listStoredQueries_nextToken,
    listStoredQueries_maxResults,
    listStoredQueriesResponse_nextToken,
    listStoredQueriesResponse_storedQueryMetadata,
    listStoredQueriesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_limit,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** PutAggregationAuthorization
    putAggregationAuthorization_tags,
    putAggregationAuthorization_authorizedAccountId,
    putAggregationAuthorization_authorizedAwsRegion,
    putAggregationAuthorizationResponse_aggregationAuthorization,
    putAggregationAuthorizationResponse_httpStatus,

    -- ** PutConfigRule
    putConfigRule_tags,
    putConfigRule_configRule,

    -- ** PutConfigurationAggregator
    putConfigurationAggregator_tags,
    putConfigurationAggregator_accountAggregationSources,
    putConfigurationAggregator_organizationAggregationSource,
    putConfigurationAggregator_configurationAggregatorName,
    putConfigurationAggregatorResponse_configurationAggregator,
    putConfigurationAggregatorResponse_httpStatus,

    -- ** PutConfigurationRecorder
    putConfigurationRecorder_configurationRecorder,

    -- ** PutConformancePack
    putConformancePack_templateS3Uri,
    putConformancePack_conformancePackInputParameters,
    putConformancePack_templateBody,
    putConformancePack_deliveryS3Bucket,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_conformancePackName,
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,

    -- ** PutDeliveryChannel
    putDeliveryChannel_deliveryChannel,

    -- ** PutEvaluations
    putEvaluations_testMode,
    putEvaluations_evaluations,
    putEvaluations_resultToken,
    putEvaluationsResponse_failedEvaluations,
    putEvaluationsResponse_httpStatus,

    -- ** PutExternalEvaluation
    putExternalEvaluation_configRuleName,
    putExternalEvaluation_externalEvaluation,
    putExternalEvaluationResponse_httpStatus,

    -- ** PutOrganizationConfigRule
    putOrganizationConfigRule_organizationCustomPolicyRuleMetadata,
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
    putOrganizationConfigRule_organizationConfigRuleName,
    putOrganizationConfigRuleResponse_organizationConfigRuleArn,
    putOrganizationConfigRuleResponse_httpStatus,

    -- ** PutOrganizationConformancePack
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_organizationConformancePackName,
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,

    -- ** PutRemediationConfigurations
    putRemediationConfigurations_remediationConfigurations,
    putRemediationConfigurationsResponse_failedBatches,
    putRemediationConfigurationsResponse_httpStatus,

    -- ** PutRemediationExceptions
    putRemediationExceptions_message,
    putRemediationExceptions_expirationTime,
    putRemediationExceptions_configRuleName,
    putRemediationExceptions_resourceKeys,
    putRemediationExceptionsResponse_failedBatches,
    putRemediationExceptionsResponse_httpStatus,

    -- ** PutResourceConfig
    putResourceConfig_tags,
    putResourceConfig_resourceName,
    putResourceConfig_resourceType,
    putResourceConfig_schemaVersionId,
    putResourceConfig_resourceId,
    putResourceConfig_configuration,

    -- ** PutRetentionConfiguration
    putRetentionConfiguration_retentionPeriodInDays,
    putRetentionConfigurationResponse_retentionConfiguration,
    putRetentionConfigurationResponse_httpStatus,

    -- ** PutStoredQuery
    putStoredQuery_tags,
    putStoredQuery_storedQuery,
    putStoredQueryResponse_queryArn,
    putStoredQueryResponse_httpStatus,

    -- ** SelectAggregateResourceConfig
    selectAggregateResourceConfig_nextToken,
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_expression,
    selectAggregateResourceConfig_configurationAggregatorName,
    selectAggregateResourceConfigResponse_queryInfo,
    selectAggregateResourceConfigResponse_nextToken,
    selectAggregateResourceConfigResponse_results,
    selectAggregateResourceConfigResponse_httpStatus,

    -- ** SelectResourceConfig
    selectResourceConfig_nextToken,
    selectResourceConfig_limit,
    selectResourceConfig_expression,
    selectResourceConfigResponse_queryInfo,
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_results,
    selectResourceConfigResponse_httpStatus,

    -- ** StartConfigRulesEvaluation
    startConfigRulesEvaluation_configRuleNames,
    startConfigRulesEvaluationResponse_httpStatus,

    -- ** StartConfigurationRecorder
    startConfigurationRecorder_configurationRecorderName,

    -- ** StartRemediationExecution
    startRemediationExecution_configRuleName,
    startRemediationExecution_resourceKeys,
    startRemediationExecutionResponse_failedItems,
    startRemediationExecutionResponse_failureMessage,
    startRemediationExecutionResponse_httpStatus,

    -- ** StopConfigurationRecorder
    stopConfigurationRecorder_configurationRecorderName,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Types

    -- ** AccountAggregationSource
    accountAggregationSource_awsRegions,
    accountAggregationSource_allAwsRegions,
    accountAggregationSource_accountIds,

    -- ** AggregateComplianceByConfigRule
    aggregateComplianceByConfigRule_configRuleName,
    aggregateComplianceByConfigRule_accountId,
    aggregateComplianceByConfigRule_awsRegion,
    aggregateComplianceByConfigRule_compliance,

    -- ** AggregateComplianceByConformancePack
    aggregateComplianceByConformancePack_conformancePackName,
    aggregateComplianceByConformancePack_accountId,
    aggregateComplianceByConformancePack_awsRegion,
    aggregateComplianceByConformancePack_compliance,

    -- ** AggregateComplianceCount
    aggregateComplianceCount_groupName,
    aggregateComplianceCount_complianceSummary,

    -- ** AggregateConformancePackCompliance
    aggregateConformancePackCompliance_nonCompliantRuleCount,
    aggregateConformancePackCompliance_totalRuleCount,
    aggregateConformancePackCompliance_compliantRuleCount,
    aggregateConformancePackCompliance_complianceType,

    -- ** AggregateConformancePackComplianceCount
    aggregateConformancePackComplianceCount_compliantConformancePackCount,
    aggregateConformancePackComplianceCount_nonCompliantConformancePackCount,

    -- ** AggregateConformancePackComplianceFilters
    aggregateConformancePackComplianceFilters_conformancePackName,
    aggregateConformancePackComplianceFilters_accountId,
    aggregateConformancePackComplianceFilters_awsRegion,
    aggregateConformancePackComplianceFilters_complianceType,

    -- ** AggregateConformancePackComplianceSummary
    aggregateConformancePackComplianceSummary_groupName,
    aggregateConformancePackComplianceSummary_complianceSummary,

    -- ** AggregateConformancePackComplianceSummaryFilters
    aggregateConformancePackComplianceSummaryFilters_accountId,
    aggregateConformancePackComplianceSummaryFilters_awsRegion,

    -- ** AggregateEvaluationResult
    aggregateEvaluationResult_evaluationResultIdentifier,
    aggregateEvaluationResult_configRuleInvokedTime,
    aggregateEvaluationResult_resultRecordedTime,
    aggregateEvaluationResult_annotation,
    aggregateEvaluationResult_accountId,
    aggregateEvaluationResult_awsRegion,
    aggregateEvaluationResult_complianceType,

    -- ** AggregateResourceIdentifier
    aggregateResourceIdentifier_resourceName,
    aggregateResourceIdentifier_sourceAccountId,
    aggregateResourceIdentifier_sourceRegion,
    aggregateResourceIdentifier_resourceId,
    aggregateResourceIdentifier_resourceType,

    -- ** AggregatedSourceStatus
    aggregatedSourceStatus_lastErrorCode,
    aggregatedSourceStatus_sourceId,
    aggregatedSourceStatus_sourceType,
    aggregatedSourceStatus_awsRegion,
    aggregatedSourceStatus_lastUpdateStatus,
    aggregatedSourceStatus_lastUpdateTime,
    aggregatedSourceStatus_lastErrorMessage,

    -- ** AggregationAuthorization
    aggregationAuthorization_authorizedAwsRegion,
    aggregationAuthorization_authorizedAccountId,
    aggregationAuthorization_aggregationAuthorizationArn,
    aggregationAuthorization_creationTime,

    -- ** BaseConfigurationItem
    baseConfigurationItem_resourceId,
    baseConfigurationItem_resourceType,
    baseConfigurationItem_resourceName,
    baseConfigurationItem_resourceCreationTime,
    baseConfigurationItem_supplementaryConfiguration,
    baseConfigurationItem_configurationStateId,
    baseConfigurationItem_configuration,
    baseConfigurationItem_arn,
    baseConfigurationItem_configurationItemStatus,
    baseConfigurationItem_availabilityZone,
    baseConfigurationItem_accountId,
    baseConfigurationItem_awsRegion,
    baseConfigurationItem_version,
    baseConfigurationItem_configurationItemCaptureTime,

    -- ** Compliance
    compliance_complianceContributorCount,
    compliance_complianceType,

    -- ** ComplianceByConfigRule
    complianceByConfigRule_configRuleName,
    complianceByConfigRule_compliance,

    -- ** ComplianceByResource
    complianceByResource_resourceId,
    complianceByResource_resourceType,
    complianceByResource_compliance,

    -- ** ComplianceContributorCount
    complianceContributorCount_cappedCount,
    complianceContributorCount_capExceeded,

    -- ** ComplianceSummary
    complianceSummary_compliantResourceCount,
    complianceSummary_complianceSummaryTimestamp,
    complianceSummary_nonCompliantResourceCount,

    -- ** ComplianceSummaryByResourceType
    complianceSummaryByResourceType_resourceType,
    complianceSummaryByResourceType_complianceSummary,

    -- ** ConfigExportDeliveryInfo
    configExportDeliveryInfo_lastErrorCode,
    configExportDeliveryInfo_nextDeliveryTime,
    configExportDeliveryInfo_lastStatus,
    configExportDeliveryInfo_lastSuccessfulTime,
    configExportDeliveryInfo_lastErrorMessage,
    configExportDeliveryInfo_lastAttemptTime,

    -- ** ConfigRule
    configRule_maximumExecutionFrequency,
    configRule_inputParameters,
    configRule_configRuleArn,
    configRule_description,
    configRule_configRuleId,
    configRule_configRuleName,
    configRule_scope,
    configRule_configRuleState,
    configRule_createdBy,
    configRule_source,

    -- ** ConfigRuleComplianceFilters
    configRuleComplianceFilters_configRuleName,
    configRuleComplianceFilters_accountId,
    configRuleComplianceFilters_awsRegion,
    configRuleComplianceFilters_complianceType,

    -- ** ConfigRuleComplianceSummaryFilters
    configRuleComplianceSummaryFilters_accountId,
    configRuleComplianceSummaryFilters_awsRegion,

    -- ** ConfigRuleEvaluationStatus
    configRuleEvaluationStatus_lastDebugLogDeliveryStatus,
    configRuleEvaluationStatus_lastDebugLogDeliveryStatusReason,
    configRuleEvaluationStatus_firstEvaluationStarted,
    configRuleEvaluationStatus_lastErrorCode,
    configRuleEvaluationStatus_lastSuccessfulEvaluationTime,
    configRuleEvaluationStatus_lastFailedEvaluationTime,
    configRuleEvaluationStatus_lastFailedInvocationTime,
    configRuleEvaluationStatus_lastDeactivatedTime,
    configRuleEvaluationStatus_configRuleArn,
    configRuleEvaluationStatus_firstActivatedTime,
    configRuleEvaluationStatus_configRuleId,
    configRuleEvaluationStatus_configRuleName,
    configRuleEvaluationStatus_lastDebugLogDeliveryTime,
    configRuleEvaluationStatus_lastSuccessfulInvocationTime,
    configRuleEvaluationStatus_lastErrorMessage,

    -- ** ConfigSnapshotDeliveryProperties
    configSnapshotDeliveryProperties_deliveryFrequency,

    -- ** ConfigStreamDeliveryInfo
    configStreamDeliveryInfo_lastErrorCode,
    configStreamDeliveryInfo_lastStatus,
    configStreamDeliveryInfo_lastStatusChangeTime,
    configStreamDeliveryInfo_lastErrorMessage,

    -- ** ConfigurationAggregator
    configurationAggregator_configurationAggregatorArn,
    configurationAggregator_accountAggregationSources,
    configurationAggregator_lastUpdatedTime,
    configurationAggregator_configurationAggregatorName,
    configurationAggregator_organizationAggregationSource,
    configurationAggregator_creationTime,
    configurationAggregator_createdBy,

    -- ** ConfigurationItem
    configurationItem_tags,
    configurationItem_resourceId,
    configurationItem_resourceType,
    configurationItem_resourceName,
    configurationItem_resourceCreationTime,
    configurationItem_supplementaryConfiguration,
    configurationItem_configurationStateId,
    configurationItem_configurationItemMD5Hash,
    configurationItem_configuration,
    configurationItem_arn,
    configurationItem_configurationItemStatus,
    configurationItem_availabilityZone,
    configurationItem_accountId,
    configurationItem_awsRegion,
    configurationItem_relatedEvents,
    configurationItem_relationships,
    configurationItem_version,
    configurationItem_configurationItemCaptureTime,

    -- ** ConfigurationRecorder
    configurationRecorder_name,
    configurationRecorder_roleARN,
    configurationRecorder_recordingGroup,

    -- ** ConfigurationRecorderStatus
    configurationRecorderStatus_name,
    configurationRecorderStatus_lastErrorCode,
    configurationRecorderStatus_lastStartTime,
    configurationRecorderStatus_lastStatus,
    configurationRecorderStatus_lastStatusChangeTime,
    configurationRecorderStatus_recording,
    configurationRecorderStatus_lastStopTime,
    configurationRecorderStatus_lastErrorMessage,

    -- ** ConformancePackComplianceFilters
    conformancePackComplianceFilters_configRuleNames,
    conformancePackComplianceFilters_complianceType,

    -- ** ConformancePackComplianceScore
    conformancePackComplianceScore_score,
    conformancePackComplianceScore_conformancePackName,
    conformancePackComplianceScore_lastUpdatedTime,

    -- ** ConformancePackComplianceScoresFilters
    conformancePackComplianceScoresFilters_conformancePackNames,

    -- ** ConformancePackComplianceSummary
    conformancePackComplianceSummary_conformancePackName,
    conformancePackComplianceSummary_conformancePackComplianceStatus,

    -- ** ConformancePackDetail
    conformancePackDetail_lastUpdateRequestedTime,
    conformancePackDetail_conformancePackInputParameters,
    conformancePackDetail_deliveryS3Bucket,
    conformancePackDetail_createdBy,
    conformancePackDetail_deliveryS3KeyPrefix,
    conformancePackDetail_conformancePackName,
    conformancePackDetail_conformancePackArn,
    conformancePackDetail_conformancePackId,

    -- ** ConformancePackEvaluationFilters
    conformancePackEvaluationFilters_resourceType,
    conformancePackEvaluationFilters_resourceIds,
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
    conformancePackRuleCompliance_configRuleName,
    conformancePackRuleCompliance_controls,
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

    -- ** CustomPolicyDetails
    customPolicyDetails_enableDebugLogDelivery,
    customPolicyDetails_policyRuntime,
    customPolicyDetails_policyText,

    -- ** DeliveryChannel
    deliveryChannel_s3KeyPrefix,
    deliveryChannel_name,
    deliveryChannel_s3BucketName,
    deliveryChannel_snsTopicARN,
    deliveryChannel_configSnapshotDeliveryProperties,
    deliveryChannel_s3KmsKeyArn,

    -- ** DeliveryChannelStatus
    deliveryChannelStatus_name,
    deliveryChannelStatus_configHistoryDeliveryInfo,
    deliveryChannelStatus_configSnapshotDeliveryInfo,
    deliveryChannelStatus_configStreamDeliveryInfo,

    -- ** Evaluation
    evaluation_annotation,
    evaluation_complianceResourceType,
    evaluation_complianceResourceId,
    evaluation_complianceType,
    evaluation_orderingTimestamp,

    -- ** EvaluationResult
    evaluationResult_evaluationResultIdentifier,
    evaluationResult_configRuleInvokedTime,
    evaluationResult_resultToken,
    evaluationResult_resultRecordedTime,
    evaluationResult_annotation,
    evaluationResult_complianceType,

    -- ** EvaluationResultIdentifier
    evaluationResultIdentifier_orderingTimestamp,
    evaluationResultIdentifier_evaluationResultQualifier,

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
    failedDeleteRemediationExceptionsBatch_failedItems,
    failedDeleteRemediationExceptionsBatch_failureMessage,

    -- ** FailedRemediationBatch
    failedRemediationBatch_failedItems,
    failedRemediationBatch_failureMessage,

    -- ** FailedRemediationExceptionBatch
    failedRemediationExceptionBatch_failedItems,
    failedRemediationExceptionBatch_failureMessage,

    -- ** FieldInfo
    fieldInfo_name,

    -- ** GroupedResourceCount
    groupedResourceCount_groupName,
    groupedResourceCount_resourceCount,

    -- ** MemberAccountStatus
    memberAccountStatus_errorMessage,
    memberAccountStatus_errorCode,
    memberAccountStatus_lastUpdateTime,
    memberAccountStatus_accountId,
    memberAccountStatus_configRuleName,
    memberAccountStatus_memberAccountRuleStatus,

    -- ** OrganizationAggregationSource
    organizationAggregationSource_awsRegions,
    organizationAggregationSource_allAwsRegions,
    organizationAggregationSource_roleArn,

    -- ** OrganizationConfigRule
    organizationConfigRule_organizationCustomPolicyRuleMetadata,
    organizationConfigRule_excludedAccounts,
    organizationConfigRule_lastUpdateTime,
    organizationConfigRule_organizationManagedRuleMetadata,
    organizationConfigRule_organizationCustomRuleMetadata,
    organizationConfigRule_organizationConfigRuleName,
    organizationConfigRule_organizationConfigRuleArn,

    -- ** OrganizationConfigRuleStatus
    organizationConfigRuleStatus_errorMessage,
    organizationConfigRuleStatus_errorCode,
    organizationConfigRuleStatus_lastUpdateTime,
    organizationConfigRuleStatus_organizationConfigRuleName,
    organizationConfigRuleStatus_organizationRuleStatus,

    -- ** OrganizationConformancePack
    organizationConformancePack_excludedAccounts,
    organizationConformancePack_conformancePackInputParameters,
    organizationConformancePack_deliveryS3Bucket,
    organizationConformancePack_deliveryS3KeyPrefix,
    organizationConformancePack_organizationConformancePackName,
    organizationConformancePack_organizationConformancePackArn,
    organizationConformancePack_lastUpdateTime,

    -- ** OrganizationConformancePackDetailedStatus
    organizationConformancePackDetailedStatus_errorMessage,
    organizationConformancePackDetailedStatus_errorCode,
    organizationConformancePackDetailedStatus_lastUpdateTime,
    organizationConformancePackDetailedStatus_accountId,
    organizationConformancePackDetailedStatus_conformancePackName,
    organizationConformancePackDetailedStatus_status,

    -- ** OrganizationConformancePackStatus
    organizationConformancePackStatus_errorMessage,
    organizationConformancePackStatus_errorCode,
    organizationConformancePackStatus_lastUpdateTime,
    organizationConformancePackStatus_organizationConformancePackName,
    organizationConformancePackStatus_status,

    -- ** OrganizationCustomPolicyRuleMetadata
    organizationCustomPolicyRuleMetadata_maximumExecutionFrequency,
    organizationCustomPolicyRuleMetadata_resourceTypesScope,
    organizationCustomPolicyRuleMetadata_inputParameters,
    organizationCustomPolicyRuleMetadata_tagValueScope,
    organizationCustomPolicyRuleMetadata_resourceIdScope,
    organizationCustomPolicyRuleMetadata_debugLogDeliveryAccounts,
    organizationCustomPolicyRuleMetadata_description,
    organizationCustomPolicyRuleMetadata_organizationConfigRuleTriggerTypes,
    organizationCustomPolicyRuleMetadata_tagKeyScope,
    organizationCustomPolicyRuleMetadata_policyRuntime,
    organizationCustomPolicyRuleMetadata_policyText,

    -- ** OrganizationCustomPolicyRuleMetadataNoPolicy
    organizationCustomPolicyRuleMetadataNoPolicy_maximumExecutionFrequency,
    organizationCustomPolicyRuleMetadataNoPolicy_resourceTypesScope,
    organizationCustomPolicyRuleMetadataNoPolicy_inputParameters,
    organizationCustomPolicyRuleMetadataNoPolicy_tagValueScope,
    organizationCustomPolicyRuleMetadataNoPolicy_resourceIdScope,
    organizationCustomPolicyRuleMetadataNoPolicy_debugLogDeliveryAccounts,
    organizationCustomPolicyRuleMetadataNoPolicy_description,
    organizationCustomPolicyRuleMetadataNoPolicy_organizationConfigRuleTriggerTypes,
    organizationCustomPolicyRuleMetadataNoPolicy_policyRuntime,
    organizationCustomPolicyRuleMetadataNoPolicy_tagKeyScope,

    -- ** OrganizationCustomRuleMetadata
    organizationCustomRuleMetadata_maximumExecutionFrequency,
    organizationCustomRuleMetadata_resourceTypesScope,
    organizationCustomRuleMetadata_inputParameters,
    organizationCustomRuleMetadata_tagValueScope,
    organizationCustomRuleMetadata_resourceIdScope,
    organizationCustomRuleMetadata_description,
    organizationCustomRuleMetadata_tagKeyScope,
    organizationCustomRuleMetadata_lambdaFunctionArn,
    organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes,

    -- ** OrganizationManagedRuleMetadata
    organizationManagedRuleMetadata_maximumExecutionFrequency,
    organizationManagedRuleMetadata_resourceTypesScope,
    organizationManagedRuleMetadata_inputParameters,
    organizationManagedRuleMetadata_tagValueScope,
    organizationManagedRuleMetadata_resourceIdScope,
    organizationManagedRuleMetadata_description,
    organizationManagedRuleMetadata_tagKeyScope,
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
    recordingGroup_includeGlobalResourceTypes,
    recordingGroup_resourceTypes,
    recordingGroup_allSupported,

    -- ** Relationship
    relationship_resourceId,
    relationship_resourceType,
    relationship_resourceName,
    relationship_relationshipName,

    -- ** RemediationConfiguration
    remediationConfiguration_resourceType,
    remediationConfiguration_maximumAutomaticAttempts,
    remediationConfiguration_arn,
    remediationConfiguration_executionControls,
    remediationConfiguration_automatic,
    remediationConfiguration_targetVersion,
    remediationConfiguration_createdByService,
    remediationConfiguration_retryAttemptSeconds,
    remediationConfiguration_parameters,
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
    remediationExecutionStatus_stepDetails,
    remediationExecutionStatus_invocationTime,
    remediationExecutionStatus_state,
    remediationExecutionStatus_lastUpdatedTime,
    remediationExecutionStatus_resourceKey,

    -- ** RemediationExecutionStep
    remediationExecutionStep_name,
    remediationExecutionStep_errorMessage,
    remediationExecutionStep_stopTime,
    remediationExecutionStep_state,
    remediationExecutionStep_startTime,

    -- ** RemediationParameterValue
    remediationParameterValue_staticValue,
    remediationParameterValue_resourceValue,

    -- ** ResourceCount
    resourceCount_resourceType,
    resourceCount_count,

    -- ** ResourceCountFilters
    resourceCountFilters_resourceType,
    resourceCountFilters_region,
    resourceCountFilters_accountId,

    -- ** ResourceFilters
    resourceFilters_resourceId,
    resourceFilters_resourceName,
    resourceFilters_region,
    resourceFilters_accountId,

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
    scope_tagValue,
    scope_tagKey,
    scope_complianceResourceId,
    scope_complianceResourceTypes,

    -- ** Source
    source_customPolicyDetails,
    source_sourceDetails,
    source_sourceIdentifier,
    source_owner,

    -- ** SourceDetail
    sourceDetail_maximumExecutionFrequency,
    sourceDetail_messageType,
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
    storedQuery_description,
    storedQuery_expression,
    storedQuery_queryArn,
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

import Amazonka.Config.BatchGetAggregateResourceConfig
import Amazonka.Config.BatchGetResourceConfig
import Amazonka.Config.DeleteAggregationAuthorization
import Amazonka.Config.DeleteConfigRule
import Amazonka.Config.DeleteConfigurationAggregator
import Amazonka.Config.DeleteConfigurationRecorder
import Amazonka.Config.DeleteConformancePack
import Amazonka.Config.DeleteDeliveryChannel
import Amazonka.Config.DeleteEvaluationResults
import Amazonka.Config.DeleteOrganizationConfigRule
import Amazonka.Config.DeleteOrganizationConformancePack
import Amazonka.Config.DeletePendingAggregationRequest
import Amazonka.Config.DeleteRemediationConfiguration
import Amazonka.Config.DeleteRemediationExceptions
import Amazonka.Config.DeleteResourceConfig
import Amazonka.Config.DeleteRetentionConfiguration
import Amazonka.Config.DeleteStoredQuery
import Amazonka.Config.DeliverConfigSnapshot
import Amazonka.Config.DescribeAggregateComplianceByConfigRules
import Amazonka.Config.DescribeAggregateComplianceByConformancePacks
import Amazonka.Config.DescribeAggregationAuthorizations
import Amazonka.Config.DescribeComplianceByConfigRule
import Amazonka.Config.DescribeComplianceByResource
import Amazonka.Config.DescribeConfigRuleEvaluationStatus
import Amazonka.Config.DescribeConfigRules
import Amazonka.Config.DescribeConfigurationAggregatorSourcesStatus
import Amazonka.Config.DescribeConfigurationAggregators
import Amazonka.Config.DescribeConfigurationRecorderStatus
import Amazonka.Config.DescribeConfigurationRecorders
import Amazonka.Config.DescribeConformancePackCompliance
import Amazonka.Config.DescribeConformancePackStatus
import Amazonka.Config.DescribeConformancePacks
import Amazonka.Config.DescribeDeliveryChannelStatus
import Amazonka.Config.DescribeDeliveryChannels
import Amazonka.Config.DescribeOrganizationConfigRuleStatuses
import Amazonka.Config.DescribeOrganizationConfigRules
import Amazonka.Config.DescribeOrganizationConformancePackStatuses
import Amazonka.Config.DescribeOrganizationConformancePacks
import Amazonka.Config.DescribePendingAggregationRequests
import Amazonka.Config.DescribeRemediationConfigurations
import Amazonka.Config.DescribeRemediationExceptions
import Amazonka.Config.DescribeRemediationExecutionStatus
import Amazonka.Config.DescribeRetentionConfigurations
import Amazonka.Config.GetAggregateComplianceDetailsByConfigRule
import Amazonka.Config.GetAggregateConfigRuleComplianceSummary
import Amazonka.Config.GetAggregateConformancePackComplianceSummary
import Amazonka.Config.GetAggregateDiscoveredResourceCounts
import Amazonka.Config.GetAggregateResourceConfig
import Amazonka.Config.GetComplianceDetailsByConfigRule
import Amazonka.Config.GetComplianceDetailsByResource
import Amazonka.Config.GetComplianceSummaryByConfigRule
import Amazonka.Config.GetComplianceSummaryByResourceType
import Amazonka.Config.GetConformancePackComplianceDetails
import Amazonka.Config.GetConformancePackComplianceSummary
import Amazonka.Config.GetCustomRulePolicy
import Amazonka.Config.GetDiscoveredResourceCounts
import Amazonka.Config.GetOrganizationConfigRuleDetailedStatus
import Amazonka.Config.GetOrganizationConformancePackDetailedStatus
import Amazonka.Config.GetOrganizationCustomRulePolicy
import Amazonka.Config.GetResourceConfigHistory
import Amazonka.Config.GetStoredQuery
import Amazonka.Config.ListAggregateDiscoveredResources
import Amazonka.Config.ListConformancePackComplianceScores
import Amazonka.Config.ListDiscoveredResources
import Amazonka.Config.ListStoredQueries
import Amazonka.Config.ListTagsForResource
import Amazonka.Config.PutAggregationAuthorization
import Amazonka.Config.PutConfigRule
import Amazonka.Config.PutConfigurationAggregator
import Amazonka.Config.PutConfigurationRecorder
import Amazonka.Config.PutConformancePack
import Amazonka.Config.PutDeliveryChannel
import Amazonka.Config.PutEvaluations
import Amazonka.Config.PutExternalEvaluation
import Amazonka.Config.PutOrganizationConfigRule
import Amazonka.Config.PutOrganizationConformancePack
import Amazonka.Config.PutRemediationConfigurations
import Amazonka.Config.PutRemediationExceptions
import Amazonka.Config.PutResourceConfig
import Amazonka.Config.PutRetentionConfiguration
import Amazonka.Config.PutStoredQuery
import Amazonka.Config.SelectAggregateResourceConfig
import Amazonka.Config.SelectResourceConfig
import Amazonka.Config.StartConfigRulesEvaluation
import Amazonka.Config.StartConfigurationRecorder
import Amazonka.Config.StartRemediationExecution
import Amazonka.Config.StopConfigurationRecorder
import Amazonka.Config.TagResource
import Amazonka.Config.Types.AccountAggregationSource
import Amazonka.Config.Types.AggregateComplianceByConfigRule
import Amazonka.Config.Types.AggregateComplianceByConformancePack
import Amazonka.Config.Types.AggregateComplianceCount
import Amazonka.Config.Types.AggregateConformancePackCompliance
import Amazonka.Config.Types.AggregateConformancePackComplianceCount
import Amazonka.Config.Types.AggregateConformancePackComplianceFilters
import Amazonka.Config.Types.AggregateConformancePackComplianceSummary
import Amazonka.Config.Types.AggregateConformancePackComplianceSummaryFilters
import Amazonka.Config.Types.AggregateEvaluationResult
import Amazonka.Config.Types.AggregateResourceIdentifier
import Amazonka.Config.Types.AggregatedSourceStatus
import Amazonka.Config.Types.AggregationAuthorization
import Amazonka.Config.Types.BaseConfigurationItem
import Amazonka.Config.Types.Compliance
import Amazonka.Config.Types.ComplianceByConfigRule
import Amazonka.Config.Types.ComplianceByResource
import Amazonka.Config.Types.ComplianceContributorCount
import Amazonka.Config.Types.ComplianceSummary
import Amazonka.Config.Types.ComplianceSummaryByResourceType
import Amazonka.Config.Types.ConfigExportDeliveryInfo
import Amazonka.Config.Types.ConfigRule
import Amazonka.Config.Types.ConfigRuleComplianceFilters
import Amazonka.Config.Types.ConfigRuleComplianceSummaryFilters
import Amazonka.Config.Types.ConfigRuleEvaluationStatus
import Amazonka.Config.Types.ConfigSnapshotDeliveryProperties
import Amazonka.Config.Types.ConfigStreamDeliveryInfo
import Amazonka.Config.Types.ConfigurationAggregator
import Amazonka.Config.Types.ConfigurationItem
import Amazonka.Config.Types.ConfigurationRecorder
import Amazonka.Config.Types.ConfigurationRecorderStatus
import Amazonka.Config.Types.ConformancePackComplianceFilters
import Amazonka.Config.Types.ConformancePackComplianceScore
import Amazonka.Config.Types.ConformancePackComplianceScoresFilters
import Amazonka.Config.Types.ConformancePackComplianceSummary
import Amazonka.Config.Types.ConformancePackDetail
import Amazonka.Config.Types.ConformancePackEvaluationFilters
import Amazonka.Config.Types.ConformancePackEvaluationResult
import Amazonka.Config.Types.ConformancePackInputParameter
import Amazonka.Config.Types.ConformancePackRuleCompliance
import Amazonka.Config.Types.ConformancePackStatusDetail
import Amazonka.Config.Types.CustomPolicyDetails
import Amazonka.Config.Types.DeliveryChannel
import Amazonka.Config.Types.DeliveryChannelStatus
import Amazonka.Config.Types.Evaluation
import Amazonka.Config.Types.EvaluationResult
import Amazonka.Config.Types.EvaluationResultIdentifier
import Amazonka.Config.Types.EvaluationResultQualifier
import Amazonka.Config.Types.ExecutionControls
import Amazonka.Config.Types.ExternalEvaluation
import Amazonka.Config.Types.FailedDeleteRemediationExceptionsBatch
import Amazonka.Config.Types.FailedRemediationBatch
import Amazonka.Config.Types.FailedRemediationExceptionBatch
import Amazonka.Config.Types.FieldInfo
import Amazonka.Config.Types.GroupedResourceCount
import Amazonka.Config.Types.MemberAccountStatus
import Amazonka.Config.Types.OrganizationAggregationSource
import Amazonka.Config.Types.OrganizationConfigRule
import Amazonka.Config.Types.OrganizationConfigRuleStatus
import Amazonka.Config.Types.OrganizationConformancePack
import Amazonka.Config.Types.OrganizationConformancePackDetailedStatus
import Amazonka.Config.Types.OrganizationConformancePackStatus
import Amazonka.Config.Types.OrganizationCustomPolicyRuleMetadata
import Amazonka.Config.Types.OrganizationCustomPolicyRuleMetadataNoPolicy
import Amazonka.Config.Types.OrganizationCustomRuleMetadata
import Amazonka.Config.Types.OrganizationManagedRuleMetadata
import Amazonka.Config.Types.OrganizationResourceDetailedStatusFilters
import Amazonka.Config.Types.PendingAggregationRequest
import Amazonka.Config.Types.QueryInfo
import Amazonka.Config.Types.RecordingGroup
import Amazonka.Config.Types.Relationship
import Amazonka.Config.Types.RemediationConfiguration
import Amazonka.Config.Types.RemediationException
import Amazonka.Config.Types.RemediationExceptionResourceKey
import Amazonka.Config.Types.RemediationExecutionStatus
import Amazonka.Config.Types.RemediationExecutionStep
import Amazonka.Config.Types.RemediationParameterValue
import Amazonka.Config.Types.ResourceCount
import Amazonka.Config.Types.ResourceCountFilters
import Amazonka.Config.Types.ResourceFilters
import Amazonka.Config.Types.ResourceIdentifier
import Amazonka.Config.Types.ResourceKey
import Amazonka.Config.Types.ResourceValue
import Amazonka.Config.Types.RetentionConfiguration
import Amazonka.Config.Types.Scope
import Amazonka.Config.Types.Source
import Amazonka.Config.Types.SourceDetail
import Amazonka.Config.Types.SsmControls
import Amazonka.Config.Types.StaticValue
import Amazonka.Config.Types.StatusDetailFilters
import Amazonka.Config.Types.StoredQuery
import Amazonka.Config.Types.StoredQueryMetadata
import Amazonka.Config.Types.Tag
import Amazonka.Config.UntagResource

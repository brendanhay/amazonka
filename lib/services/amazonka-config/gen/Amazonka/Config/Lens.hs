{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_unprocessedResourceKeys,
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
    describeAggregateComplianceByConfigRules_filters,
    describeAggregateComplianceByConfigRules_limit,
    describeAggregateComplianceByConfigRules_nextToken,
    describeAggregateComplianceByConfigRules_configurationAggregatorName,
    describeAggregateComplianceByConfigRulesResponse_aggregateComplianceByConfigRules,
    describeAggregateComplianceByConfigRulesResponse_nextToken,
    describeAggregateComplianceByConfigRulesResponse_httpStatus,

    -- ** DescribeAggregateComplianceByConformancePacks
    describeAggregateComplianceByConformancePacks_filters,
    describeAggregateComplianceByConformancePacks_limit,
    describeAggregateComplianceByConformancePacks_nextToken,
    describeAggregateComplianceByConformancePacks_configurationAggregatorName,
    describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks,
    describeAggregateComplianceByConformancePacksResponse_nextToken,
    describeAggregateComplianceByConformancePacksResponse_httpStatus,

    -- ** DescribeAggregationAuthorizations
    describeAggregationAuthorizations_limit,
    describeAggregationAuthorizations_nextToken,
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_httpStatus,

    -- ** DescribeComplianceByConfigRule
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_configRuleNames,
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_httpStatus,

    -- ** DescribeComplianceByResource
    describeComplianceByResource_complianceTypes,
    describeComplianceByResource_limit,
    describeComplianceByResource_nextToken,
    describeComplianceByResource_resourceId,
    describeComplianceByResource_resourceType,
    describeComplianceByResourceResponse_complianceByResources,
    describeComplianceByResourceResponse_nextToken,
    describeComplianceByResourceResponse_httpStatus,

    -- ** DescribeConfigRuleEvaluationStatus
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_limit,
    describeConfigRuleEvaluationStatus_nextToken,
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_httpStatus,

    -- ** DescribeConfigRules
    describeConfigRules_configRuleNames,
    describeConfigRules_filters,
    describeConfigRules_nextToken,
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_httpStatus,

    -- ** DescribeConfigurationAggregatorSourcesStatus
    describeConfigurationAggregatorSourcesStatus_limit,
    describeConfigurationAggregatorSourcesStatus_nextToken,
    describeConfigurationAggregatorSourcesStatus_updateStatus,
    describeConfigurationAggregatorSourcesStatus_configurationAggregatorName,
    describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList,
    describeConfigurationAggregatorSourcesStatusResponse_nextToken,
    describeConfigurationAggregatorSourcesStatusResponse_httpStatus,

    -- ** DescribeConfigurationAggregators
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregators_limit,
    describeConfigurationAggregators_nextToken,
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_nextToken,
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
    describeConformancePackCompliance_filters,
    describeConformancePackCompliance_limit,
    describeConformancePackCompliance_nextToken,
    describeConformancePackCompliance_conformancePackName,
    describeConformancePackComplianceResponse_nextToken,
    describeConformancePackComplianceResponse_httpStatus,
    describeConformancePackComplianceResponse_conformancePackName,
    describeConformancePackComplianceResponse_conformancePackRuleComplianceList,

    -- ** DescribeConformancePackStatus
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatus_limit,
    describeConformancePackStatus_nextToken,
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_httpStatus,

    -- ** DescribeConformancePacks
    describeConformancePacks_conformancePackNames,
    describeConformancePacks_limit,
    describeConformancePacks_nextToken,
    describeConformancePacksResponse_conformancePackDetails,
    describeConformancePacksResponse_nextToken,
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
    describeOrganizationConfigRuleStatuses_limit,
    describeOrganizationConfigRuleStatuses_nextToken,
    describeOrganizationConfigRuleStatuses_organizationConfigRuleNames,
    describeOrganizationConfigRuleStatusesResponse_nextToken,
    describeOrganizationConfigRuleStatusesResponse_organizationConfigRuleStatuses,
    describeOrganizationConfigRuleStatusesResponse_httpStatus,

    -- ** DescribeOrganizationConfigRules
    describeOrganizationConfigRules_limit,
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_organizationConfigRuleNames,
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_httpStatus,

    -- ** DescribeOrganizationConformancePackStatuses
    describeOrganizationConformancePackStatuses_limit,
    describeOrganizationConformancePackStatuses_nextToken,
    describeOrganizationConformancePackStatuses_organizationConformancePackNames,
    describeOrganizationConformancePackStatusesResponse_nextToken,
    describeOrganizationConformancePackStatusesResponse_organizationConformancePackStatuses,
    describeOrganizationConformancePackStatusesResponse_httpStatus,

    -- ** DescribeOrganizationConformancePacks
    describeOrganizationConformancePacks_limit,
    describeOrganizationConformancePacks_nextToken,
    describeOrganizationConformancePacks_organizationConformancePackNames,
    describeOrganizationConformancePacksResponse_nextToken,
    describeOrganizationConformancePacksResponse_organizationConformancePacks,
    describeOrganizationConformancePacksResponse_httpStatus,

    -- ** DescribePendingAggregationRequests
    describePendingAggregationRequests_limit,
    describePendingAggregationRequests_nextToken,
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,

    -- ** DescribeRemediationConfigurations
    describeRemediationConfigurations_configRuleNames,
    describeRemediationConfigurationsResponse_remediationConfigurations,
    describeRemediationConfigurationsResponse_httpStatus,

    -- ** DescribeRemediationExceptions
    describeRemediationExceptions_limit,
    describeRemediationExceptions_nextToken,
    describeRemediationExceptions_resourceKeys,
    describeRemediationExceptions_configRuleName,
    describeRemediationExceptionsResponse_nextToken,
    describeRemediationExceptionsResponse_remediationExceptions,
    describeRemediationExceptionsResponse_httpStatus,

    -- ** DescribeRemediationExecutionStatus
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_configRuleName,
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_httpStatus,

    -- ** DescribeRetentionConfigurations
    describeRetentionConfigurations_nextToken,
    describeRetentionConfigurations_retentionConfigurationNames,
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_httpStatus,

    -- ** GetAggregateComplianceDetailsByConfigRule
    getAggregateComplianceDetailsByConfigRule_complianceType,
    getAggregateComplianceDetailsByConfigRule_limit,
    getAggregateComplianceDetailsByConfigRule_nextToken,
    getAggregateComplianceDetailsByConfigRule_configurationAggregatorName,
    getAggregateComplianceDetailsByConfigRule_configRuleName,
    getAggregateComplianceDetailsByConfigRule_accountId,
    getAggregateComplianceDetailsByConfigRule_awsRegion,
    getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults,
    getAggregateComplianceDetailsByConfigRuleResponse_nextToken,
    getAggregateComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** GetAggregateConfigRuleComplianceSummary
    getAggregateConfigRuleComplianceSummary_filters,
    getAggregateConfigRuleComplianceSummary_groupByKey,
    getAggregateConfigRuleComplianceSummary_limit,
    getAggregateConfigRuleComplianceSummary_nextToken,
    getAggregateConfigRuleComplianceSummary_configurationAggregatorName,
    getAggregateConfigRuleComplianceSummaryResponse_aggregateComplianceCounts,
    getAggregateConfigRuleComplianceSummaryResponse_groupByKey,
    getAggregateConfigRuleComplianceSummaryResponse_nextToken,
    getAggregateConfigRuleComplianceSummaryResponse_httpStatus,

    -- ** GetAggregateConformancePackComplianceSummary
    getAggregateConformancePackComplianceSummary_filters,
    getAggregateConformancePackComplianceSummary_groupByKey,
    getAggregateConformancePackComplianceSummary_limit,
    getAggregateConformancePackComplianceSummary_nextToken,
    getAggregateConformancePackComplianceSummary_configurationAggregatorName,
    getAggregateConformancePackComplianceSummaryResponse_aggregateConformancePackComplianceSummaries,
    getAggregateConformancePackComplianceSummaryResponse_groupByKey,
    getAggregateConformancePackComplianceSummaryResponse_nextToken,
    getAggregateConformancePackComplianceSummaryResponse_httpStatus,

    -- ** GetAggregateDiscoveredResourceCounts
    getAggregateDiscoveredResourceCounts_filters,
    getAggregateDiscoveredResourceCounts_groupByKey,
    getAggregateDiscoveredResourceCounts_limit,
    getAggregateDiscoveredResourceCounts_nextToken,
    getAggregateDiscoveredResourceCounts_configurationAggregatorName,
    getAggregateDiscoveredResourceCountsResponse_groupByKey,
    getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts,
    getAggregateDiscoveredResourceCountsResponse_nextToken,
    getAggregateDiscoveredResourceCountsResponse_httpStatus,
    getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources,

    -- ** GetAggregateResourceConfig
    getAggregateResourceConfig_configurationAggregatorName,
    getAggregateResourceConfig_resourceIdentifier,
    getAggregateResourceConfigResponse_configurationItem,
    getAggregateResourceConfigResponse_httpStatus,

    -- ** GetComplianceDetailsByConfigRule
    getComplianceDetailsByConfigRule_complianceTypes,
    getComplianceDetailsByConfigRule_limit,
    getComplianceDetailsByConfigRule_nextToken,
    getComplianceDetailsByConfigRule_configRuleName,
    getComplianceDetailsByConfigRuleResponse_evaluationResults,
    getComplianceDetailsByConfigRuleResponse_nextToken,
    getComplianceDetailsByConfigRuleResponse_httpStatus,

    -- ** GetComplianceDetailsByResource
    getComplianceDetailsByResource_complianceTypes,
    getComplianceDetailsByResource_nextToken,
    getComplianceDetailsByResource_resourceEvaluationId,
    getComplianceDetailsByResource_resourceId,
    getComplianceDetailsByResource_resourceType,
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
    getConformancePackComplianceDetails_filters,
    getConformancePackComplianceDetails_limit,
    getConformancePackComplianceDetails_nextToken,
    getConformancePackComplianceDetails_conformancePackName,
    getConformancePackComplianceDetailsResponse_conformancePackRuleEvaluationResults,
    getConformancePackComplianceDetailsResponse_nextToken,
    getConformancePackComplianceDetailsResponse_httpStatus,
    getConformancePackComplianceDetailsResponse_conformancePackName,

    -- ** GetConformancePackComplianceSummary
    getConformancePackComplianceSummary_limit,
    getConformancePackComplianceSummary_nextToken,
    getConformancePackComplianceSummary_conformancePackNames,
    getConformancePackComplianceSummaryResponse_conformancePackComplianceSummaryList,
    getConformancePackComplianceSummaryResponse_nextToken,
    getConformancePackComplianceSummaryResponse_httpStatus,

    -- ** GetCustomRulePolicy
    getCustomRulePolicy_configRuleName,
    getCustomRulePolicyResponse_policyText,
    getCustomRulePolicyResponse_httpStatus,

    -- ** GetDiscoveredResourceCounts
    getDiscoveredResourceCounts_limit,
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_resourceTypes,
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_httpStatus,

    -- ** GetOrganizationConfigRuleDetailedStatus
    getOrganizationConfigRuleDetailedStatus_filters,
    getOrganizationConfigRuleDetailedStatus_limit,
    getOrganizationConfigRuleDetailedStatus_nextToken,
    getOrganizationConfigRuleDetailedStatus_organizationConfigRuleName,
    getOrganizationConfigRuleDetailedStatusResponse_nextToken,
    getOrganizationConfigRuleDetailedStatusResponse_organizationConfigRuleDetailedStatus,
    getOrganizationConfigRuleDetailedStatusResponse_httpStatus,

    -- ** GetOrganizationConformancePackDetailedStatus
    getOrganizationConformancePackDetailedStatus_filters,
    getOrganizationConformancePackDetailedStatus_limit,
    getOrganizationConformancePackDetailedStatus_nextToken,
    getOrganizationConformancePackDetailedStatus_organizationConformancePackName,
    getOrganizationConformancePackDetailedStatusResponse_nextToken,
    getOrganizationConformancePackDetailedStatusResponse_organizationConformancePackDetailedStatuses,
    getOrganizationConformancePackDetailedStatusResponse_httpStatus,

    -- ** GetOrganizationCustomRulePolicy
    getOrganizationCustomRulePolicy_organizationConfigRuleName,
    getOrganizationCustomRulePolicyResponse_policyText,
    getOrganizationCustomRulePolicyResponse_httpStatus,

    -- ** GetResourceConfigHistory
    getResourceConfigHistory_chronologicalOrder,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_resourceType,
    getResourceConfigHistory_resourceId,
    getResourceConfigHistoryResponse_configurationItems,
    getResourceConfigHistoryResponse_nextToken,
    getResourceConfigHistoryResponse_httpStatus,

    -- ** GetResourceEvaluationSummary
    getResourceEvaluationSummary_resourceEvaluationId,
    getResourceEvaluationSummaryResponse_compliance,
    getResourceEvaluationSummaryResponse_evaluationContext,
    getResourceEvaluationSummaryResponse_evaluationMode,
    getResourceEvaluationSummaryResponse_evaluationStartTimestamp,
    getResourceEvaluationSummaryResponse_evaluationStatus,
    getResourceEvaluationSummaryResponse_resourceDetails,
    getResourceEvaluationSummaryResponse_resourceEvaluationId,
    getResourceEvaluationSummaryResponse_httpStatus,

    -- ** GetStoredQuery
    getStoredQuery_queryName,
    getStoredQueryResponse_storedQuery,
    getStoredQueryResponse_httpStatus,

    -- ** ListAggregateDiscoveredResources
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_nextToken,
    listAggregateDiscoveredResources_configurationAggregatorName,
    listAggregateDiscoveredResources_resourceType,
    listAggregateDiscoveredResourcesResponse_nextToken,
    listAggregateDiscoveredResourcesResponse_resourceIdentifiers,
    listAggregateDiscoveredResourcesResponse_httpStatus,

    -- ** ListConformancePackComplianceScores
    listConformancePackComplianceScores_filters,
    listConformancePackComplianceScores_limit,
    listConformancePackComplianceScores_nextToken,
    listConformancePackComplianceScores_sortBy,
    listConformancePackComplianceScores_sortOrder,
    listConformancePackComplianceScoresResponse_nextToken,
    listConformancePackComplianceScoresResponse_httpStatus,
    listConformancePackComplianceScoresResponse_conformancePackComplianceScores,

    -- ** ListDiscoveredResources
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_limit,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_resourceType,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_resourceIdentifiers,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListResourceEvaluations
    listResourceEvaluations_filters,
    listResourceEvaluations_limit,
    listResourceEvaluations_nextToken,
    listResourceEvaluationsResponse_nextToken,
    listResourceEvaluationsResponse_resourceEvaluations,
    listResourceEvaluationsResponse_httpStatus,

    -- ** ListStoredQueries
    listStoredQueries_maxResults,
    listStoredQueries_nextToken,
    listStoredQueriesResponse_nextToken,
    listStoredQueriesResponse_storedQueryMetadata,
    listStoredQueriesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_limit,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
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
    putConfigurationAggregator_accountAggregationSources,
    putConfigurationAggregator_organizationAggregationSource,
    putConfigurationAggregator_tags,
    putConfigurationAggregator_configurationAggregatorName,
    putConfigurationAggregatorResponse_configurationAggregator,
    putConfigurationAggregatorResponse_httpStatus,

    -- ** PutConfigurationRecorder
    putConfigurationRecorder_configurationRecorder,

    -- ** PutConformancePack
    putConformancePack_conformancePackInputParameters,
    putConformancePack_deliveryS3Bucket,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_templateBody,
    putConformancePack_templateS3Uri,
    putConformancePack_templateSSMDocumentDetails,
    putConformancePack_conformancePackName,
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,

    -- ** PutDeliveryChannel
    putDeliveryChannel_deliveryChannel,

    -- ** PutEvaluations
    putEvaluations_evaluations,
    putEvaluations_testMode,
    putEvaluations_resultToken,
    putEvaluationsResponse_failedEvaluations,
    putEvaluationsResponse_httpStatus,

    -- ** PutExternalEvaluation
    putExternalEvaluation_configRuleName,
    putExternalEvaluation_externalEvaluation,
    putExternalEvaluationResponse_httpStatus,

    -- ** PutOrganizationConfigRule
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationCustomPolicyRuleMetadata,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_organizationConfigRuleName,
    putOrganizationConfigRuleResponse_organizationConfigRuleArn,
    putOrganizationConfigRuleResponse_httpStatus,

    -- ** PutOrganizationConformancePack
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_organizationConformancePackName,
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,

    -- ** PutRemediationConfigurations
    putRemediationConfigurations_remediationConfigurations,
    putRemediationConfigurationsResponse_failedBatches,
    putRemediationConfigurationsResponse_httpStatus,

    -- ** PutRemediationExceptions
    putRemediationExceptions_expirationTime,
    putRemediationExceptions_message,
    putRemediationExceptions_configRuleName,
    putRemediationExceptions_resourceKeys,
    putRemediationExceptionsResponse_failedBatches,
    putRemediationExceptionsResponse_httpStatus,

    -- ** PutResourceConfig
    putResourceConfig_resourceName,
    putResourceConfig_tags,
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
    selectAggregateResourceConfig_limit,
    selectAggregateResourceConfig_maxResults,
    selectAggregateResourceConfig_nextToken,
    selectAggregateResourceConfig_expression,
    selectAggregateResourceConfig_configurationAggregatorName,
    selectAggregateResourceConfigResponse_nextToken,
    selectAggregateResourceConfigResponse_queryInfo,
    selectAggregateResourceConfigResponse_results,
    selectAggregateResourceConfigResponse_httpStatus,

    -- ** SelectResourceConfig
    selectResourceConfig_limit,
    selectResourceConfig_nextToken,
    selectResourceConfig_expression,
    selectResourceConfigResponse_nextToken,
    selectResourceConfigResponse_queryInfo,
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

    -- ** StartResourceEvaluation
    startResourceEvaluation_clientToken,
    startResourceEvaluation_evaluationContext,
    startResourceEvaluation_evaluationTimeout,
    startResourceEvaluation_resourceDetails,
    startResourceEvaluation_evaluationMode,
    startResourceEvaluationResponse_resourceEvaluationId,
    startResourceEvaluationResponse_httpStatus,

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
    accountAggregationSource_allAwsRegions,
    accountAggregationSource_awsRegions,
    accountAggregationSource_accountIds,

    -- ** AggregateComplianceByConfigRule
    aggregateComplianceByConfigRule_accountId,
    aggregateComplianceByConfigRule_awsRegion,
    aggregateComplianceByConfigRule_compliance,
    aggregateComplianceByConfigRule_configRuleName,

    -- ** AggregateComplianceByConformancePack
    aggregateComplianceByConformancePack_accountId,
    aggregateComplianceByConformancePack_awsRegion,
    aggregateComplianceByConformancePack_compliance,
    aggregateComplianceByConformancePack_conformancePackName,

    -- ** AggregateComplianceCount
    aggregateComplianceCount_complianceSummary,
    aggregateComplianceCount_groupName,

    -- ** AggregateConformancePackCompliance
    aggregateConformancePackCompliance_complianceType,
    aggregateConformancePackCompliance_compliantRuleCount,
    aggregateConformancePackCompliance_nonCompliantRuleCount,
    aggregateConformancePackCompliance_totalRuleCount,

    -- ** AggregateConformancePackComplianceCount
    aggregateConformancePackComplianceCount_compliantConformancePackCount,
    aggregateConformancePackComplianceCount_nonCompliantConformancePackCount,

    -- ** AggregateConformancePackComplianceFilters
    aggregateConformancePackComplianceFilters_accountId,
    aggregateConformancePackComplianceFilters_awsRegion,
    aggregateConformancePackComplianceFilters_complianceType,
    aggregateConformancePackComplianceFilters_conformancePackName,

    -- ** AggregateConformancePackComplianceSummary
    aggregateConformancePackComplianceSummary_complianceSummary,
    aggregateConformancePackComplianceSummary_groupName,

    -- ** AggregateConformancePackComplianceSummaryFilters
    aggregateConformancePackComplianceSummaryFilters_accountId,
    aggregateConformancePackComplianceSummaryFilters_awsRegion,

    -- ** AggregateEvaluationResult
    aggregateEvaluationResult_accountId,
    aggregateEvaluationResult_annotation,
    aggregateEvaluationResult_awsRegion,
    aggregateEvaluationResult_complianceType,
    aggregateEvaluationResult_configRuleInvokedTime,
    aggregateEvaluationResult_evaluationResultIdentifier,
    aggregateEvaluationResult_resultRecordedTime,

    -- ** AggregateResourceIdentifier
    aggregateResourceIdentifier_resourceName,
    aggregateResourceIdentifier_sourceAccountId,
    aggregateResourceIdentifier_sourceRegion,
    aggregateResourceIdentifier_resourceId,
    aggregateResourceIdentifier_resourceType,

    -- ** AggregatedSourceStatus
    aggregatedSourceStatus_awsRegion,
    aggregatedSourceStatus_lastErrorCode,
    aggregatedSourceStatus_lastErrorMessage,
    aggregatedSourceStatus_lastUpdateStatus,
    aggregatedSourceStatus_lastUpdateTime,
    aggregatedSourceStatus_sourceId,
    aggregatedSourceStatus_sourceType,

    -- ** AggregationAuthorization
    aggregationAuthorization_aggregationAuthorizationArn,
    aggregationAuthorization_authorizedAccountId,
    aggregationAuthorization_authorizedAwsRegion,
    aggregationAuthorization_creationTime,

    -- ** BaseConfigurationItem
    baseConfigurationItem_accountId,
    baseConfigurationItem_arn,
    baseConfigurationItem_availabilityZone,
    baseConfigurationItem_awsRegion,
    baseConfigurationItem_configuration,
    baseConfigurationItem_configurationItemCaptureTime,
    baseConfigurationItem_configurationItemStatus,
    baseConfigurationItem_configurationStateId,
    baseConfigurationItem_resourceCreationTime,
    baseConfigurationItem_resourceId,
    baseConfigurationItem_resourceName,
    baseConfigurationItem_resourceType,
    baseConfigurationItem_supplementaryConfiguration,
    baseConfigurationItem_version,

    -- ** Compliance
    compliance_complianceContributorCount,
    compliance_complianceType,

    -- ** ComplianceByConfigRule
    complianceByConfigRule_compliance,
    complianceByConfigRule_configRuleName,

    -- ** ComplianceByResource
    complianceByResource_compliance,
    complianceByResource_resourceId,
    complianceByResource_resourceType,

    -- ** ComplianceContributorCount
    complianceContributorCount_capExceeded,
    complianceContributorCount_cappedCount,

    -- ** ComplianceSummary
    complianceSummary_complianceSummaryTimestamp,
    complianceSummary_compliantResourceCount,
    complianceSummary_nonCompliantResourceCount,

    -- ** ComplianceSummaryByResourceType
    complianceSummaryByResourceType_complianceSummary,
    complianceSummaryByResourceType_resourceType,

    -- ** ConfigExportDeliveryInfo
    configExportDeliveryInfo_lastAttemptTime,
    configExportDeliveryInfo_lastErrorCode,
    configExportDeliveryInfo_lastErrorMessage,
    configExportDeliveryInfo_lastStatus,
    configExportDeliveryInfo_lastSuccessfulTime,
    configExportDeliveryInfo_nextDeliveryTime,

    -- ** ConfigRule
    configRule_configRuleArn,
    configRule_configRuleId,
    configRule_configRuleName,
    configRule_configRuleState,
    configRule_createdBy,
    configRule_description,
    configRule_evaluationModes,
    configRule_inputParameters,
    configRule_maximumExecutionFrequency,
    configRule_scope,
    configRule_source,

    -- ** ConfigRuleComplianceFilters
    configRuleComplianceFilters_accountId,
    configRuleComplianceFilters_awsRegion,
    configRuleComplianceFilters_complianceType,
    configRuleComplianceFilters_configRuleName,

    -- ** ConfigRuleComplianceSummaryFilters
    configRuleComplianceSummaryFilters_accountId,
    configRuleComplianceSummaryFilters_awsRegion,

    -- ** ConfigRuleEvaluationStatus
    configRuleEvaluationStatus_configRuleArn,
    configRuleEvaluationStatus_configRuleId,
    configRuleEvaluationStatus_configRuleName,
    configRuleEvaluationStatus_firstActivatedTime,
    configRuleEvaluationStatus_firstEvaluationStarted,
    configRuleEvaluationStatus_lastDeactivatedTime,
    configRuleEvaluationStatus_lastDebugLogDeliveryStatus,
    configRuleEvaluationStatus_lastDebugLogDeliveryStatusReason,
    configRuleEvaluationStatus_lastDebugLogDeliveryTime,
    configRuleEvaluationStatus_lastErrorCode,
    configRuleEvaluationStatus_lastErrorMessage,
    configRuleEvaluationStatus_lastFailedEvaluationTime,
    configRuleEvaluationStatus_lastFailedInvocationTime,
    configRuleEvaluationStatus_lastSuccessfulEvaluationTime,
    configRuleEvaluationStatus_lastSuccessfulInvocationTime,

    -- ** ConfigSnapshotDeliveryProperties
    configSnapshotDeliveryProperties_deliveryFrequency,

    -- ** ConfigStreamDeliveryInfo
    configStreamDeliveryInfo_lastErrorCode,
    configStreamDeliveryInfo_lastErrorMessage,
    configStreamDeliveryInfo_lastStatus,
    configStreamDeliveryInfo_lastStatusChangeTime,

    -- ** ConfigurationAggregator
    configurationAggregator_accountAggregationSources,
    configurationAggregator_configurationAggregatorArn,
    configurationAggregator_configurationAggregatorName,
    configurationAggregator_createdBy,
    configurationAggregator_creationTime,
    configurationAggregator_lastUpdatedTime,
    configurationAggregator_organizationAggregationSource,

    -- ** ConfigurationItem
    configurationItem_accountId,
    configurationItem_arn,
    configurationItem_availabilityZone,
    configurationItem_awsRegion,
    configurationItem_configuration,
    configurationItem_configurationItemCaptureTime,
    configurationItem_configurationItemMD5Hash,
    configurationItem_configurationItemStatus,
    configurationItem_configurationStateId,
    configurationItem_relatedEvents,
    configurationItem_relationships,
    configurationItem_resourceCreationTime,
    configurationItem_resourceId,
    configurationItem_resourceName,
    configurationItem_resourceType,
    configurationItem_supplementaryConfiguration,
    configurationItem_tags,
    configurationItem_version,

    -- ** ConfigurationRecorder
    configurationRecorder_name,
    configurationRecorder_recordingGroup,
    configurationRecorder_roleARN,

    -- ** ConfigurationRecorderStatus
    configurationRecorderStatus_lastErrorCode,
    configurationRecorderStatus_lastErrorMessage,
    configurationRecorderStatus_lastStartTime,
    configurationRecorderStatus_lastStatus,
    configurationRecorderStatus_lastStatusChangeTime,
    configurationRecorderStatus_lastStopTime,
    configurationRecorderStatus_name,
    configurationRecorderStatus_recording,

    -- ** ConformancePackComplianceFilters
    conformancePackComplianceFilters_complianceType,
    conformancePackComplianceFilters_configRuleNames,

    -- ** ConformancePackComplianceScore
    conformancePackComplianceScore_conformancePackName,
    conformancePackComplianceScore_lastUpdatedTime,
    conformancePackComplianceScore_score,

    -- ** ConformancePackComplianceScoresFilters
    conformancePackComplianceScoresFilters_conformancePackNames,

    -- ** ConformancePackComplianceSummary
    conformancePackComplianceSummary_conformancePackName,
    conformancePackComplianceSummary_conformancePackComplianceStatus,

    -- ** ConformancePackDetail
    conformancePackDetail_conformancePackInputParameters,
    conformancePackDetail_createdBy,
    conformancePackDetail_deliveryS3Bucket,
    conformancePackDetail_deliveryS3KeyPrefix,
    conformancePackDetail_lastUpdateRequestedTime,
    conformancePackDetail_templateSSMDocumentDetails,
    conformancePackDetail_conformancePackName,
    conformancePackDetail_conformancePackArn,
    conformancePackDetail_conformancePackId,

    -- ** ConformancePackEvaluationFilters
    conformancePackEvaluationFilters_complianceType,
    conformancePackEvaluationFilters_configRuleNames,
    conformancePackEvaluationFilters_resourceIds,
    conformancePackEvaluationFilters_resourceType,

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
    conformancePackRuleCompliance_complianceType,
    conformancePackRuleCompliance_configRuleName,
    conformancePackRuleCompliance_controls,

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
    deliveryChannel_configSnapshotDeliveryProperties,
    deliveryChannel_name,
    deliveryChannel_s3BucketName,
    deliveryChannel_s3KeyPrefix,
    deliveryChannel_s3KmsKeyArn,
    deliveryChannel_snsTopicARN,

    -- ** DeliveryChannelStatus
    deliveryChannelStatus_configHistoryDeliveryInfo,
    deliveryChannelStatus_configSnapshotDeliveryInfo,
    deliveryChannelStatus_configStreamDeliveryInfo,
    deliveryChannelStatus_name,

    -- ** DescribeConfigRulesFilters
    describeConfigRulesFilters_evaluationMode,

    -- ** Evaluation
    evaluation_annotation,
    evaluation_complianceResourceType,
    evaluation_complianceResourceId,
    evaluation_complianceType,
    evaluation_orderingTimestamp,

    -- ** EvaluationContext
    evaluationContext_evaluationContextIdentifier,

    -- ** EvaluationModeConfiguration
    evaluationModeConfiguration_mode,

    -- ** EvaluationResult
    evaluationResult_annotation,
    evaluationResult_complianceType,
    evaluationResult_configRuleInvokedTime,
    evaluationResult_evaluationResultIdentifier,
    evaluationResult_resultRecordedTime,
    evaluationResult_resultToken,

    -- ** EvaluationResultIdentifier
    evaluationResultIdentifier_evaluationResultQualifier,
    evaluationResultIdentifier_orderingTimestamp,
    evaluationResultIdentifier_resourceEvaluationId,

    -- ** EvaluationResultQualifier
    evaluationResultQualifier_configRuleName,
    evaluationResultQualifier_evaluationMode,
    evaluationResultQualifier_resourceId,
    evaluationResultQualifier_resourceType,

    -- ** EvaluationStatus
    evaluationStatus_failureReason,
    evaluationStatus_status,

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
    memberAccountStatus_errorCode,
    memberAccountStatus_errorMessage,
    memberAccountStatus_lastUpdateTime,
    memberAccountStatus_accountId,
    memberAccountStatus_configRuleName,
    memberAccountStatus_memberAccountRuleStatus,

    -- ** OrganizationAggregationSource
    organizationAggregationSource_allAwsRegions,
    organizationAggregationSource_awsRegions,
    organizationAggregationSource_roleArn,

    -- ** OrganizationConfigRule
    organizationConfigRule_excludedAccounts,
    organizationConfigRule_lastUpdateTime,
    organizationConfigRule_organizationCustomPolicyRuleMetadata,
    organizationConfigRule_organizationCustomRuleMetadata,
    organizationConfigRule_organizationManagedRuleMetadata,
    organizationConfigRule_organizationConfigRuleName,
    organizationConfigRule_organizationConfigRuleArn,

    -- ** OrganizationConfigRuleStatus
    organizationConfigRuleStatus_errorCode,
    organizationConfigRuleStatus_errorMessage,
    organizationConfigRuleStatus_lastUpdateTime,
    organizationConfigRuleStatus_organizationConfigRuleName,
    organizationConfigRuleStatus_organizationRuleStatus,

    -- ** OrganizationConformancePack
    organizationConformancePack_conformancePackInputParameters,
    organizationConformancePack_deliveryS3Bucket,
    organizationConformancePack_deliveryS3KeyPrefix,
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

    -- ** OrganizationCustomPolicyRuleMetadata
    organizationCustomPolicyRuleMetadata_debugLogDeliveryAccounts,
    organizationCustomPolicyRuleMetadata_description,
    organizationCustomPolicyRuleMetadata_inputParameters,
    organizationCustomPolicyRuleMetadata_maximumExecutionFrequency,
    organizationCustomPolicyRuleMetadata_organizationConfigRuleTriggerTypes,
    organizationCustomPolicyRuleMetadata_resourceIdScope,
    organizationCustomPolicyRuleMetadata_resourceTypesScope,
    organizationCustomPolicyRuleMetadata_tagKeyScope,
    organizationCustomPolicyRuleMetadata_tagValueScope,
    organizationCustomPolicyRuleMetadata_policyRuntime,
    organizationCustomPolicyRuleMetadata_policyText,

    -- ** OrganizationCustomPolicyRuleMetadataNoPolicy
    organizationCustomPolicyRuleMetadataNoPolicy_debugLogDeliveryAccounts,
    organizationCustomPolicyRuleMetadataNoPolicy_description,
    organizationCustomPolicyRuleMetadataNoPolicy_inputParameters,
    organizationCustomPolicyRuleMetadataNoPolicy_maximumExecutionFrequency,
    organizationCustomPolicyRuleMetadataNoPolicy_organizationConfigRuleTriggerTypes,
    organizationCustomPolicyRuleMetadataNoPolicy_policyRuntime,
    organizationCustomPolicyRuleMetadataNoPolicy_resourceIdScope,
    organizationCustomPolicyRuleMetadataNoPolicy_resourceTypesScope,
    organizationCustomPolicyRuleMetadataNoPolicy_tagKeyScope,
    organizationCustomPolicyRuleMetadataNoPolicy_tagValueScope,

    -- ** OrganizationCustomRuleMetadata
    organizationCustomRuleMetadata_description,
    organizationCustomRuleMetadata_inputParameters,
    organizationCustomRuleMetadata_maximumExecutionFrequency,
    organizationCustomRuleMetadata_resourceIdScope,
    organizationCustomRuleMetadata_resourceTypesScope,
    organizationCustomRuleMetadata_tagKeyScope,
    organizationCustomRuleMetadata_tagValueScope,
    organizationCustomRuleMetadata_lambdaFunctionArn,
    organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes,

    -- ** OrganizationManagedRuleMetadata
    organizationManagedRuleMetadata_description,
    organizationManagedRuleMetadata_inputParameters,
    organizationManagedRuleMetadata_maximumExecutionFrequency,
    organizationManagedRuleMetadata_resourceIdScope,
    organizationManagedRuleMetadata_resourceTypesScope,
    organizationManagedRuleMetadata_tagKeyScope,
    organizationManagedRuleMetadata_tagValueScope,
    organizationManagedRuleMetadata_ruleIdentifier,

    -- ** OrganizationResourceDetailedStatusFilters
    organizationResourceDetailedStatusFilters_accountId,
    organizationResourceDetailedStatusFilters_status,

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
    relationship_relationshipName,
    relationship_resourceId,
    relationship_resourceName,
    relationship_resourceType,

    -- ** RemediationConfiguration
    remediationConfiguration_arn,
    remediationConfiguration_automatic,
    remediationConfiguration_createdByService,
    remediationConfiguration_executionControls,
    remediationConfiguration_maximumAutomaticAttempts,
    remediationConfiguration_parameters,
    remediationConfiguration_resourceType,
    remediationConfiguration_retryAttemptSeconds,
    remediationConfiguration_targetVersion,
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
    remediationExecutionStatus_lastUpdatedTime,
    remediationExecutionStatus_resourceKey,
    remediationExecutionStatus_state,
    remediationExecutionStatus_stepDetails,

    -- ** RemediationExecutionStep
    remediationExecutionStep_errorMessage,
    remediationExecutionStep_name,
    remediationExecutionStep_startTime,
    remediationExecutionStep_state,
    remediationExecutionStep_stopTime,

    -- ** RemediationParameterValue
    remediationParameterValue_resourceValue,
    remediationParameterValue_staticValue,

    -- ** ResourceCount
    resourceCount_count,
    resourceCount_resourceType,

    -- ** ResourceCountFilters
    resourceCountFilters_accountId,
    resourceCountFilters_region,
    resourceCountFilters_resourceType,

    -- ** ResourceDetails
    resourceDetails_resourceConfigurationSchemaType,
    resourceDetails_resourceId,
    resourceDetails_resourceType,
    resourceDetails_resourceConfiguration,

    -- ** ResourceEvaluation
    resourceEvaluation_evaluationMode,
    resourceEvaluation_evaluationStartTimestamp,
    resourceEvaluation_resourceEvaluationId,

    -- ** ResourceEvaluationFilters
    resourceEvaluationFilters_evaluationContextIdentifier,
    resourceEvaluationFilters_evaluationMode,
    resourceEvaluationFilters_timeWindow,

    -- ** ResourceFilters
    resourceFilters_accountId,
    resourceFilters_region,
    resourceFilters_resourceId,
    resourceFilters_resourceName,

    -- ** ResourceIdentifier
    resourceIdentifier_resourceDeletionTime,
    resourceIdentifier_resourceId,
    resourceIdentifier_resourceName,
    resourceIdentifier_resourceType,

    -- ** ResourceKey
    resourceKey_resourceType,
    resourceKey_resourceId,

    -- ** ResourceValue
    resourceValue_value,

    -- ** RetentionConfiguration
    retentionConfiguration_name,
    retentionConfiguration_retentionPeriodInDays,

    -- ** Scope
    scope_complianceResourceId,
    scope_complianceResourceTypes,
    scope_tagKey,
    scope_tagValue,

    -- ** Source
    source_customPolicyDetails,
    source_sourceDetails,
    source_sourceIdentifier,
    source_owner,

    -- ** SourceDetail
    sourceDetail_eventSource,
    sourceDetail_maximumExecutionFrequency,
    sourceDetail_messageType,

    -- ** SsmControls
    ssmControls_concurrentExecutionRatePercentage,
    ssmControls_errorPercentage,

    -- ** StaticValue
    staticValue_values,

    -- ** StatusDetailFilters
    statusDetailFilters_accountId,
    statusDetailFilters_memberAccountRuleStatus,

    -- ** StoredQuery
    storedQuery_description,
    storedQuery_expression,
    storedQuery_queryArn,
    storedQuery_queryId,
    storedQuery_queryName,

    -- ** StoredQueryMetadata
    storedQueryMetadata_description,
    storedQueryMetadata_queryId,
    storedQueryMetadata_queryArn,
    storedQueryMetadata_queryName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TemplateSSMDocumentDetails
    templateSSMDocumentDetails_documentVersion,
    templateSSMDocumentDetails_documentName,

    -- ** TimeWindow
    timeWindow_endTime,
    timeWindow_startTime,
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
import Amazonka.Config.GetResourceEvaluationSummary
import Amazonka.Config.GetStoredQuery
import Amazonka.Config.ListAggregateDiscoveredResources
import Amazonka.Config.ListConformancePackComplianceScores
import Amazonka.Config.ListDiscoveredResources
import Amazonka.Config.ListResourceEvaluations
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
import Amazonka.Config.StartResourceEvaluation
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
import Amazonka.Config.Types.DescribeConfigRulesFilters
import Amazonka.Config.Types.Evaluation
import Amazonka.Config.Types.EvaluationContext
import Amazonka.Config.Types.EvaluationModeConfiguration
import Amazonka.Config.Types.EvaluationResult
import Amazonka.Config.Types.EvaluationResultIdentifier
import Amazonka.Config.Types.EvaluationResultQualifier
import Amazonka.Config.Types.EvaluationStatus
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
import Amazonka.Config.Types.ResourceDetails
import Amazonka.Config.Types.ResourceEvaluation
import Amazonka.Config.Types.ResourceEvaluationFilters
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
import Amazonka.Config.Types.TemplateSSMDocumentDetails
import Amazonka.Config.Types.TimeWindow
import Amazonka.Config.UntagResource

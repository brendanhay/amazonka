{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Config
--
-- AWS Config provides a way to keep track of the configurations of all the
-- AWS resources associated with your AWS account. You can use AWS Config
-- to get the current and historical configurations of each AWS resource
-- and also to get information about the relationship between the
-- resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2)
-- instance, an Elastic Block Store (EBS) volume, an elastic network
-- Interface (ENI), or a security group. For a complete list of resources
-- currently supported by AWS Config, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources>.
--
-- You can access and manage AWS Config through the AWS Management Console,
-- the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS
-- SDKs for AWS Config. This reference guide contains documentation for the
-- AWS Config API and the AWS CLI commands that you can use to manage AWS
-- Config. The AWS Config API uses the Signature Version 4 protocol for
-- signing requests. For more information about how to sign a request with
-- this protocol, see
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
-- For detailed information about AWS Config features and their associated
-- actions or commands, as well as how to work with AWS Management Console,
-- see
-- <https://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config>
-- in the /AWS Config Developer Guide/.
module Network.AWS.Config
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NoSuchRetentionConfigurationException
    _NoSuchRetentionConfigurationException,

    -- ** NoSuchConfigRuleException
    _NoSuchConfigRuleException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** NoSuchOrganizationConformancePackException
    _NoSuchOrganizationConformancePackException,

    -- ** NoSuchOrganizationConfigRuleException
    _NoSuchOrganizationConfigRuleException,

    -- ** InvalidResultTokenException
    _InvalidResultTokenException,

    -- ** InvalidRecordingGroupException
    _InvalidRecordingGroupException,

    -- ** InvalidExpressionException
    _InvalidExpressionException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidS3KeyPrefixException
    _InvalidS3KeyPrefixException,

    -- ** MaxNumberOfDeliveryChannelsExceededException
    _MaxNumberOfDeliveryChannelsExceededException,

    -- ** InvalidDeliveryChannelNameException
    _InvalidDeliveryChannelNameException,

    -- ** InvalidConfigurationRecorderNameException
    _InvalidConfigurationRecorderNameException,

    -- ** NoRunningConfigurationRecorderException
    _NoRunningConfigurationRecorderException,

    -- ** NoAvailableDeliveryChannelException
    _NoAvailableDeliveryChannelException,

    -- ** LastDeliveryChannelDeleteFailedException
    _LastDeliveryChannelDeleteFailedException,

    -- ** ConformancePackTemplateValidationException
    _ConformancePackTemplateValidationException,

    -- ** OversizedConfigurationItemException
    _OversizedConfigurationItemException,

    -- ** MaxNumberOfConformancePacksExceededException
    _MaxNumberOfConformancePacksExceededException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** NoSuchConfigurationAggregatorException
    _NoSuchConfigurationAggregatorException,

    -- ** NoSuchConformancePackException
    _NoSuchConformancePackException,

    -- ** OrganizationAccessDeniedException
    _OrganizationAccessDeniedException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** NoSuchRemediationConfigurationException
    _NoSuchRemediationConfigurationException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** NoSuchConfigRuleInConformancePackException
    _NoSuchConfigRuleInConformancePackException,

    -- ** ValidationException
    _ValidationException,

    -- ** InvalidSNSTopicARNException
    _InvalidSNSTopicARNException,

    -- ** NoSuchDeliveryChannelException
    _NoSuchDeliveryChannelException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidS3KmsKeyArnException
    _InvalidS3KmsKeyArnException,

    -- ** NoSuchConfigurationRecorderException
    _NoSuchConfigurationRecorderException,

    -- ** InsufficientDeliveryPolicyException
    _InsufficientDeliveryPolicyException,

    -- ** MaxNumberOfOrganizationConfigRulesExceededException
    _MaxNumberOfOrganizationConfigRulesExceededException,

    -- ** MaxNumberOfOrganizationConformancePacksExceededException
    _MaxNumberOfOrganizationConformancePacksExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidLimitException
    _InvalidLimitException,

    -- ** OrganizationConformancePackTemplateValidationException
    _OrganizationConformancePackTemplateValidationException,

    -- ** MaxNumberOfConfigurationRecordersExceededException
    _MaxNumberOfConfigurationRecordersExceededException,

    -- ** NoSuchRemediationExceptionException
    _NoSuchRemediationExceptionException,

    -- ** ResourceConcurrentModificationException
    _ResourceConcurrentModificationException,

    -- ** MaxNumberOfConfigRulesExceededException
    _MaxNumberOfConfigRulesExceededException,

    -- ** NoAvailableConfigurationRecorderException
    _NoAvailableConfigurationRecorderException,

    -- ** NoSuchBucketException
    _NoSuchBucketException,

    -- ** MaxActiveResourcesExceededException
    _MaxActiveResourcesExceededException,

    -- ** MaxNumberOfRetentionConfigurationsExceededException
    _MaxNumberOfRetentionConfigurationsExceededException,

    -- ** OrganizationAllFeaturesNotEnabledException
    _OrganizationAllFeaturesNotEnabledException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** RemediationInProgressException
    _RemediationInProgressException,

    -- ** ResourceNotDiscoveredException
    _ResourceNotDiscoveredException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeComplianceByConfigRule (Paginated)
    DescribeComplianceByConfigRule (DescribeComplianceByConfigRule'),
    newDescribeComplianceByConfigRule,
    DescribeComplianceByConfigRuleResponse (DescribeComplianceByConfigRuleResponse'),
    newDescribeComplianceByConfigRuleResponse,

    -- ** GetAggregateComplianceDetailsByConfigRule (Paginated)
    GetAggregateComplianceDetailsByConfigRule (GetAggregateComplianceDetailsByConfigRule'),
    newGetAggregateComplianceDetailsByConfigRule,
    GetAggregateComplianceDetailsByConfigRuleResponse (GetAggregateComplianceDetailsByConfigRuleResponse'),
    newGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** GetStoredQuery
    GetStoredQuery (GetStoredQuery'),
    newGetStoredQuery,
    GetStoredQueryResponse (GetStoredQueryResponse'),
    newGetStoredQueryResponse,

    -- ** GetAggregateResourceConfig
    GetAggregateResourceConfig (GetAggregateResourceConfig'),
    newGetAggregateResourceConfig,
    GetAggregateResourceConfigResponse (GetAggregateResourceConfigResponse'),
    newGetAggregateResourceConfigResponse,

    -- ** DescribeConfigurationAggregators (Paginated)
    DescribeConfigurationAggregators (DescribeConfigurationAggregators'),
    newDescribeConfigurationAggregators,
    DescribeConfigurationAggregatorsResponse (DescribeConfigurationAggregatorsResponse'),
    newDescribeConfigurationAggregatorsResponse,

    -- ** DescribePendingAggregationRequests (Paginated)
    DescribePendingAggregationRequests (DescribePendingAggregationRequests'),
    newDescribePendingAggregationRequests,
    DescribePendingAggregationRequestsResponse (DescribePendingAggregationRequestsResponse'),
    newDescribePendingAggregationRequestsResponse,

    -- ** DescribeRemediationExecutionStatus (Paginated)
    DescribeRemediationExecutionStatus (DescribeRemediationExecutionStatus'),
    newDescribeRemediationExecutionStatus,
    DescribeRemediationExecutionStatusResponse (DescribeRemediationExecutionStatusResponse'),
    newDescribeRemediationExecutionStatusResponse,

    -- ** GetResourceConfigHistory (Paginated)
    GetResourceConfigHistory (GetResourceConfigHistory'),
    newGetResourceConfigHistory,
    GetResourceConfigHistoryResponse (GetResourceConfigHistoryResponse'),
    newGetResourceConfigHistoryResponse,

    -- ** DescribeConformancePackCompliance
    DescribeConformancePackCompliance (DescribeConformancePackCompliance'),
    newDescribeConformancePackCompliance,
    DescribeConformancePackComplianceResponse (DescribeConformancePackComplianceResponse'),
    newDescribeConformancePackComplianceResponse,

    -- ** DescribeOrganizationConformancePacks
    DescribeOrganizationConformancePacks (DescribeOrganizationConformancePacks'),
    newDescribeOrganizationConformancePacks,
    DescribeOrganizationConformancePacksResponse (DescribeOrganizationConformancePacksResponse'),
    newDescribeOrganizationConformancePacksResponse,

    -- ** ListDiscoveredResources (Paginated)
    ListDiscoveredResources (ListDiscoveredResources'),
    newListDiscoveredResources,
    ListDiscoveredResourcesResponse (ListDiscoveredResourcesResponse'),
    newListDiscoveredResourcesResponse,

    -- ** DescribeAggregationAuthorizations (Paginated)
    DescribeAggregationAuthorizations (DescribeAggregationAuthorizations'),
    newDescribeAggregationAuthorizations,
    DescribeAggregationAuthorizationsResponse (DescribeAggregationAuthorizationsResponse'),
    newDescribeAggregationAuthorizationsResponse,

    -- ** DescribeRemediationConfigurations
    DescribeRemediationConfigurations (DescribeRemediationConfigurations'),
    newDescribeRemediationConfigurations,
    DescribeRemediationConfigurationsResponse (DescribeRemediationConfigurationsResponse'),
    newDescribeRemediationConfigurationsResponse,

    -- ** DeleteResourceConfig
    DeleteResourceConfig (DeleteResourceConfig'),
    newDeleteResourceConfig,
    DeleteResourceConfigResponse (DeleteResourceConfigResponse'),
    newDeleteResourceConfigResponse,

    -- ** DescribeComplianceByResource (Paginated)
    DescribeComplianceByResource (DescribeComplianceByResource'),
    newDescribeComplianceByResource,
    DescribeComplianceByResourceResponse (DescribeComplianceByResourceResponse'),
    newDescribeComplianceByResourceResponse,

    -- ** StartConfigRulesEvaluation
    StartConfigRulesEvaluation (StartConfigRulesEvaluation'),
    newStartConfigRulesEvaluation,
    StartConfigRulesEvaluationResponse (StartConfigRulesEvaluationResponse'),
    newStartConfigRulesEvaluationResponse,

    -- ** DescribeRemediationExceptions
    DescribeRemediationExceptions (DescribeRemediationExceptions'),
    newDescribeRemediationExceptions,
    DescribeRemediationExceptionsResponse (DescribeRemediationExceptionsResponse'),
    newDescribeRemediationExceptionsResponse,

    -- ** DeleteOrganizationConformancePack
    DeleteOrganizationConformancePack (DeleteOrganizationConformancePack'),
    newDeleteOrganizationConformancePack,
    DeleteOrganizationConformancePackResponse (DeleteOrganizationConformancePackResponse'),
    newDeleteOrganizationConformancePackResponse,

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    GetComplianceDetailsByConfigRule (GetComplianceDetailsByConfigRule'),
    newGetComplianceDetailsByConfigRule,
    GetComplianceDetailsByConfigRuleResponse (GetComplianceDetailsByConfigRuleResponse'),
    newGetComplianceDetailsByConfigRuleResponse,

    -- ** PutOrganizationConfigRule
    PutOrganizationConfigRule (PutOrganizationConfigRule'),
    newPutOrganizationConfigRule,
    PutOrganizationConfigRuleResponse (PutOrganizationConfigRuleResponse'),
    newPutOrganizationConfigRuleResponse,

    -- ** GetDiscoveredResourceCounts
    GetDiscoveredResourceCounts (GetDiscoveredResourceCounts'),
    newGetDiscoveredResourceCounts,
    GetDiscoveredResourceCountsResponse (GetDiscoveredResourceCountsResponse'),
    newGetDiscoveredResourceCountsResponse,

    -- ** GetComplianceSummaryByResourceType
    GetComplianceSummaryByResourceType (GetComplianceSummaryByResourceType'),
    newGetComplianceSummaryByResourceType,
    GetComplianceSummaryByResourceTypeResponse (GetComplianceSummaryByResourceTypeResponse'),
    newGetComplianceSummaryByResourceTypeResponse,

    -- ** PutDeliveryChannel
    PutDeliveryChannel (PutDeliveryChannel'),
    newPutDeliveryChannel,
    PutDeliveryChannelResponse (PutDeliveryChannelResponse'),
    newPutDeliveryChannelResponse,

    -- ** DescribeConfigurationAggregatorSourcesStatus (Paginated)
    DescribeConfigurationAggregatorSourcesStatus (DescribeConfigurationAggregatorSourcesStatus'),
    newDescribeConfigurationAggregatorSourcesStatus,
    DescribeConfigurationAggregatorSourcesStatusResponse (DescribeConfigurationAggregatorSourcesStatusResponse'),
    newDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** DeleteRemediationConfiguration
    DeleteRemediationConfiguration (DeleteRemediationConfiguration'),
    newDeleteRemediationConfiguration,
    DeleteRemediationConfigurationResponse (DeleteRemediationConfigurationResponse'),
    newDeleteRemediationConfigurationResponse,

    -- ** DeleteAggregationAuthorization
    DeleteAggregationAuthorization (DeleteAggregationAuthorization'),
    newDeleteAggregationAuthorization,
    DeleteAggregationAuthorizationResponse (DeleteAggregationAuthorizationResponse'),
    newDeleteAggregationAuthorizationResponse,

    -- ** DescribeConfigurationRecorderStatus
    DescribeConfigurationRecorderStatus (DescribeConfigurationRecorderStatus'),
    newDescribeConfigurationRecorderStatus,
    DescribeConfigurationRecorderStatusResponse (DescribeConfigurationRecorderStatusResponse'),
    newDescribeConfigurationRecorderStatusResponse,

    -- ** DeleteConfigurationRecorder
    DeleteConfigurationRecorder (DeleteConfigurationRecorder'),
    newDeleteConfigurationRecorder,
    DeleteConfigurationRecorderResponse (DeleteConfigurationRecorderResponse'),
    newDeleteConfigurationRecorderResponse,

    -- ** GetConformancePackComplianceSummary
    GetConformancePackComplianceSummary (GetConformancePackComplianceSummary'),
    newGetConformancePackComplianceSummary,
    GetConformancePackComplianceSummaryResponse (GetConformancePackComplianceSummaryResponse'),
    newGetConformancePackComplianceSummaryResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeConfigRuleEvaluationStatus (Paginated)
    DescribeConfigRuleEvaluationStatus (DescribeConfigRuleEvaluationStatus'),
    newDescribeConfigRuleEvaluationStatus,
    DescribeConfigRuleEvaluationStatusResponse (DescribeConfigRuleEvaluationStatusResponse'),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- ** DeleteConfigurationAggregator
    DeleteConfigurationAggregator (DeleteConfigurationAggregator'),
    newDeleteConfigurationAggregator,
    DeleteConfigurationAggregatorResponse (DeleteConfigurationAggregatorResponse'),
    newDeleteConfigurationAggregatorResponse,

    -- ** DeleteStoredQuery
    DeleteStoredQuery (DeleteStoredQuery'),
    newDeleteStoredQuery,
    DeleteStoredQueryResponse (DeleteStoredQueryResponse'),
    newDeleteStoredQueryResponse,

    -- ** DescribeOrganizationConfigRuleStatuses
    DescribeOrganizationConfigRuleStatuses (DescribeOrganizationConfigRuleStatuses'),
    newDescribeOrganizationConfigRuleStatuses,
    DescribeOrganizationConfigRuleStatusesResponse (DescribeOrganizationConfigRuleStatusesResponse'),
    newDescribeOrganizationConfigRuleStatusesResponse,

    -- ** GetComplianceDetailsByResource (Paginated)
    GetComplianceDetailsByResource (GetComplianceDetailsByResource'),
    newGetComplianceDetailsByResource,
    GetComplianceDetailsByResourceResponse (GetComplianceDetailsByResourceResponse'),
    newGetComplianceDetailsByResourceResponse,

    -- ** ListAggregateDiscoveredResources (Paginated)
    ListAggregateDiscoveredResources (ListAggregateDiscoveredResources'),
    newListAggregateDiscoveredResources,
    ListAggregateDiscoveredResourcesResponse (ListAggregateDiscoveredResourcesResponse'),
    newListAggregateDiscoveredResourcesResponse,

    -- ** SelectResourceConfig
    SelectResourceConfig (SelectResourceConfig'),
    newSelectResourceConfig,
    SelectResourceConfigResponse (SelectResourceConfigResponse'),
    newSelectResourceConfigResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** PutRetentionConfiguration
    PutRetentionConfiguration (PutRetentionConfiguration'),
    newPutRetentionConfiguration,
    PutRetentionConfigurationResponse (PutRetentionConfigurationResponse'),
    newPutRetentionConfigurationResponse,

    -- ** GetConformancePackComplianceDetails
    GetConformancePackComplianceDetails (GetConformancePackComplianceDetails'),
    newGetConformancePackComplianceDetails,
    GetConformancePackComplianceDetailsResponse (GetConformancePackComplianceDetailsResponse'),
    newGetConformancePackComplianceDetailsResponse,

    -- ** DeleteEvaluationResults
    DeleteEvaluationResults (DeleteEvaluationResults'),
    newDeleteEvaluationResults,
    DeleteEvaluationResultsResponse (DeleteEvaluationResultsResponse'),
    newDeleteEvaluationResultsResponse,

    -- ** PutRemediationConfigurations
    PutRemediationConfigurations (PutRemediationConfigurations'),
    newPutRemediationConfigurations,
    PutRemediationConfigurationsResponse (PutRemediationConfigurationsResponse'),
    newPutRemediationConfigurationsResponse,

    -- ** PutConfigRule
    PutConfigRule (PutConfigRule'),
    newPutConfigRule,
    PutConfigRuleResponse (PutConfigRuleResponse'),
    newPutConfigRuleResponse,

    -- ** PutConformancePack
    PutConformancePack (PutConformancePack'),
    newPutConformancePack,
    PutConformancePackResponse (PutConformancePackResponse'),
    newPutConformancePackResponse,

    -- ** ListStoredQueries
    ListStoredQueries (ListStoredQueries'),
    newListStoredQueries,
    ListStoredQueriesResponse (ListStoredQueriesResponse'),
    newListStoredQueriesResponse,

    -- ** DeleteRemediationExceptions
    DeleteRemediationExceptions (DeleteRemediationExceptions'),
    newDeleteRemediationExceptions,
    DeleteRemediationExceptionsResponse (DeleteRemediationExceptionsResponse'),
    newDeleteRemediationExceptionsResponse,

    -- ** PutExternalEvaluation
    PutExternalEvaluation (PutExternalEvaluation'),
    newPutExternalEvaluation,
    PutExternalEvaluationResponse (PutExternalEvaluationResponse'),
    newPutExternalEvaluationResponse,

    -- ** BatchGetResourceConfig
    BatchGetResourceConfig (BatchGetResourceConfig'),
    newBatchGetResourceConfig,
    BatchGetResourceConfigResponse (BatchGetResourceConfigResponse'),
    newBatchGetResourceConfigResponse,

    -- ** GetAggregateDiscoveredResourceCounts
    GetAggregateDiscoveredResourceCounts (GetAggregateDiscoveredResourceCounts'),
    newGetAggregateDiscoveredResourceCounts,
    GetAggregateDiscoveredResourceCountsResponse (GetAggregateDiscoveredResourceCountsResponse'),
    newGetAggregateDiscoveredResourceCountsResponse,

    -- ** PutEvaluations
    PutEvaluations (PutEvaluations'),
    newPutEvaluations,
    PutEvaluationsResponse (PutEvaluationsResponse'),
    newPutEvaluationsResponse,

    -- ** DescribeConfigurationRecorders
    DescribeConfigurationRecorders (DescribeConfigurationRecorders'),
    newDescribeConfigurationRecorders,
    DescribeConfigurationRecordersResponse (DescribeConfigurationRecordersResponse'),
    newDescribeConfigurationRecordersResponse,

    -- ** DescribeOrganizationConfigRules
    DescribeOrganizationConfigRules (DescribeOrganizationConfigRules'),
    newDescribeOrganizationConfigRules,
    DescribeOrganizationConfigRulesResponse (DescribeOrganizationConfigRulesResponse'),
    newDescribeOrganizationConfigRulesResponse,

    -- ** SelectAggregateResourceConfig
    SelectAggregateResourceConfig (SelectAggregateResourceConfig'),
    newSelectAggregateResourceConfig,
    SelectAggregateResourceConfigResponse (SelectAggregateResourceConfigResponse'),
    newSelectAggregateResourceConfigResponse,

    -- ** PutResourceConfig
    PutResourceConfig (PutResourceConfig'),
    newPutResourceConfig,
    PutResourceConfigResponse (PutResourceConfigResponse'),
    newPutResourceConfigResponse,

    -- ** DescribeDeliveryChannels
    DescribeDeliveryChannels (DescribeDeliveryChannels'),
    newDescribeDeliveryChannels,
    DescribeDeliveryChannelsResponse (DescribeDeliveryChannelsResponse'),
    newDescribeDeliveryChannelsResponse,

    -- ** DeleteDeliveryChannel
    DeleteDeliveryChannel (DeleteDeliveryChannel'),
    newDeleteDeliveryChannel,
    DeleteDeliveryChannelResponse (DeleteDeliveryChannelResponse'),
    newDeleteDeliveryChannelResponse,

    -- ** DescribeDeliveryChannelStatus
    DescribeDeliveryChannelStatus (DescribeDeliveryChannelStatus'),
    newDescribeDeliveryChannelStatus,
    DescribeDeliveryChannelStatusResponse (DescribeDeliveryChannelStatusResponse'),
    newDescribeDeliveryChannelStatusResponse,

    -- ** DeleteOrganizationConfigRule
    DeleteOrganizationConfigRule (DeleteOrganizationConfigRule'),
    newDeleteOrganizationConfigRule,
    DeleteOrganizationConfigRuleResponse (DeleteOrganizationConfigRuleResponse'),
    newDeleteOrganizationConfigRuleResponse,

    -- ** PutAggregationAuthorization
    PutAggregationAuthorization (PutAggregationAuthorization'),
    newPutAggregationAuthorization,
    PutAggregationAuthorizationResponse (PutAggregationAuthorizationResponse'),
    newPutAggregationAuthorizationResponse,

    -- ** PutOrganizationConformancePack
    PutOrganizationConformancePack (PutOrganizationConformancePack'),
    newPutOrganizationConformancePack,
    PutOrganizationConformancePackResponse (PutOrganizationConformancePackResponse'),
    newPutOrganizationConformancePackResponse,

    -- ** PutConfigurationRecorder
    PutConfigurationRecorder (PutConfigurationRecorder'),
    newPutConfigurationRecorder,
    PutConfigurationRecorderResponse (PutConfigurationRecorderResponse'),
    newPutConfigurationRecorderResponse,

    -- ** DeletePendingAggregationRequest
    DeletePendingAggregationRequest (DeletePendingAggregationRequest'),
    newDeletePendingAggregationRequest,
    DeletePendingAggregationRequestResponse (DeletePendingAggregationRequestResponse'),
    newDeletePendingAggregationRequestResponse,

    -- ** DeliverConfigSnapshot
    DeliverConfigSnapshot (DeliverConfigSnapshot'),
    newDeliverConfigSnapshot,
    DeliverConfigSnapshotResponse (DeliverConfigSnapshotResponse'),
    newDeliverConfigSnapshotResponse,

    -- ** BatchGetAggregateResourceConfig
    BatchGetAggregateResourceConfig (BatchGetAggregateResourceConfig'),
    newBatchGetAggregateResourceConfig,
    BatchGetAggregateResourceConfigResponse (BatchGetAggregateResourceConfigResponse'),
    newBatchGetAggregateResourceConfigResponse,

    -- ** GetComplianceSummaryByConfigRule
    GetComplianceSummaryByConfigRule (GetComplianceSummaryByConfigRule'),
    newGetComplianceSummaryByConfigRule,
    GetComplianceSummaryByConfigRuleResponse (GetComplianceSummaryByConfigRuleResponse'),
    newGetComplianceSummaryByConfigRuleResponse,

    -- ** DescribeOrganizationConformancePackStatuses
    DescribeOrganizationConformancePackStatuses (DescribeOrganizationConformancePackStatuses'),
    newDescribeOrganizationConformancePackStatuses,
    DescribeOrganizationConformancePackStatusesResponse (DescribeOrganizationConformancePackStatusesResponse'),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- ** PutStoredQuery
    PutStoredQuery (PutStoredQuery'),
    newPutStoredQuery,
    PutStoredQueryResponse (PutStoredQueryResponse'),
    newPutStoredQueryResponse,

    -- ** PutConfigurationAggregator
    PutConfigurationAggregator (PutConfigurationAggregator'),
    newPutConfigurationAggregator,
    PutConfigurationAggregatorResponse (PutConfigurationAggregatorResponse'),
    newPutConfigurationAggregatorResponse,

    -- ** GetOrganizationConfigRuleDetailedStatus
    GetOrganizationConfigRuleDetailedStatus (GetOrganizationConfigRuleDetailedStatus'),
    newGetOrganizationConfigRuleDetailedStatus,
    GetOrganizationConfigRuleDetailedStatusResponse (GetOrganizationConfigRuleDetailedStatusResponse'),
    newGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** StartRemediationExecution
    StartRemediationExecution (StartRemediationExecution'),
    newStartRemediationExecution,
    StartRemediationExecutionResponse (StartRemediationExecutionResponse'),
    newStartRemediationExecutionResponse,

    -- ** DescribeConformancePackStatus
    DescribeConformancePackStatus (DescribeConformancePackStatus'),
    newDescribeConformancePackStatus,
    DescribeConformancePackStatusResponse (DescribeConformancePackStatusResponse'),
    newDescribeConformancePackStatusResponse,

    -- ** DeleteConfigRule
    DeleteConfigRule (DeleteConfigRule'),
    newDeleteConfigRule,
    DeleteConfigRuleResponse (DeleteConfigRuleResponse'),
    newDeleteConfigRuleResponse,

    -- ** DeleteRetentionConfiguration
    DeleteRetentionConfiguration (DeleteRetentionConfiguration'),
    newDeleteRetentionConfiguration,
    DeleteRetentionConfigurationResponse (DeleteRetentionConfigurationResponse'),
    newDeleteRetentionConfigurationResponse,

    -- ** DeleteConformancePack
    DeleteConformancePack (DeleteConformancePack'),
    newDeleteConformancePack,
    DeleteConformancePackResponse (DeleteConformancePackResponse'),
    newDeleteConformancePackResponse,

    -- ** GetOrganizationConformancePackDetailedStatus
    GetOrganizationConformancePackDetailedStatus (GetOrganizationConformancePackDetailedStatus'),
    newGetOrganizationConformancePackDetailedStatus,
    GetOrganizationConformancePackDetailedStatusResponse (GetOrganizationConformancePackDetailedStatusResponse'),
    newGetOrganizationConformancePackDetailedStatusResponse,

    -- ** DescribeAggregateComplianceByConfigRules (Paginated)
    DescribeAggregateComplianceByConfigRules (DescribeAggregateComplianceByConfigRules'),
    newDescribeAggregateComplianceByConfigRules,
    DescribeAggregateComplianceByConfigRulesResponse (DescribeAggregateComplianceByConfigRulesResponse'),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- ** StartConfigurationRecorder
    StartConfigurationRecorder (StartConfigurationRecorder'),
    newStartConfigurationRecorder,
    StartConfigurationRecorderResponse (StartConfigurationRecorderResponse'),
    newStartConfigurationRecorderResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetAggregateConfigRuleComplianceSummary
    GetAggregateConfigRuleComplianceSummary (GetAggregateConfigRuleComplianceSummary'),
    newGetAggregateConfigRuleComplianceSummary,
    GetAggregateConfigRuleComplianceSummaryResponse (GetAggregateConfigRuleComplianceSummaryResponse'),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- ** PutRemediationExceptions
    PutRemediationExceptions (PutRemediationExceptions'),
    newPutRemediationExceptions,
    PutRemediationExceptionsResponse (PutRemediationExceptionsResponse'),
    newPutRemediationExceptionsResponse,

    -- ** DescribeConfigRules (Paginated)
    DescribeConfigRules (DescribeConfigRules'),
    newDescribeConfigRules,
    DescribeConfigRulesResponse (DescribeConfigRulesResponse'),
    newDescribeConfigRulesResponse,

    -- ** StopConfigurationRecorder
    StopConfigurationRecorder (StopConfigurationRecorder'),
    newStopConfigurationRecorder,
    StopConfigurationRecorderResponse (StopConfigurationRecorderResponse'),
    newStopConfigurationRecorderResponse,

    -- ** DescribeConformancePacks
    DescribeConformancePacks (DescribeConformancePacks'),
    newDescribeConformancePacks,
    DescribeConformancePacksResponse (DescribeConformancePacksResponse'),
    newDescribeConformancePacksResponse,

    -- ** DescribeRetentionConfigurations (Paginated)
    DescribeRetentionConfigurations (DescribeRetentionConfigurations'),
    newDescribeRetentionConfigurations,
    DescribeRetentionConfigurationsResponse (DescribeRetentionConfigurationsResponse'),
    newDescribeRetentionConfigurationsResponse,

    -- * Types

    -- ** AggregatedSourceStatusType
    AggregatedSourceStatusType (..),

    -- ** AggregatedSourceType
    AggregatedSourceType (..),

    -- ** ChronologicalOrder
    ChronologicalOrder (..),

    -- ** ComplianceType
    ComplianceType (..),

    -- ** ConfigRuleComplianceSummaryGroupKey
    ConfigRuleComplianceSummaryGroupKey (..),

    -- ** ConfigRuleState
    ConfigRuleState (..),

    -- ** ConfigurationItemStatus
    ConfigurationItemStatus (..),

    -- ** ConformancePackComplianceType
    ConformancePackComplianceType (..),

    -- ** ConformancePackState
    ConformancePackState (..),

    -- ** DeliveryStatus
    DeliveryStatus (..),

    -- ** EventSource
    EventSource (..),

    -- ** MaximumExecutionFrequency
    MaximumExecutionFrequency (..),

    -- ** MemberAccountRuleStatus
    MemberAccountRuleStatus (..),

    -- ** MessageType
    MessageType (..),

    -- ** OrganizationConfigRuleTriggerType
    OrganizationConfigRuleTriggerType (..),

    -- ** OrganizationResourceDetailedStatus
    OrganizationResourceDetailedStatus (..),

    -- ** OrganizationResourceStatus
    OrganizationResourceStatus (..),

    -- ** OrganizationRuleStatus
    OrganizationRuleStatus (..),

    -- ** Owner
    Owner (..),

    -- ** RecorderStatus
    RecorderStatus (..),

    -- ** RemediationExecutionState
    RemediationExecutionState (..),

    -- ** RemediationExecutionStepState
    RemediationExecutionStepState (..),

    -- ** RemediationTargetType
    RemediationTargetType (..),

    -- ** ResourceCountGroupKey
    ResourceCountGroupKey (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ResourceValueType
    ResourceValueType (..),

    -- ** AccountAggregationSource
    AccountAggregationSource (AccountAggregationSource'),
    newAccountAggregationSource,

    -- ** AggregateComplianceByConfigRule
    AggregateComplianceByConfigRule (AggregateComplianceByConfigRule'),
    newAggregateComplianceByConfigRule,

    -- ** AggregateComplianceCount
    AggregateComplianceCount (AggregateComplianceCount'),
    newAggregateComplianceCount,

    -- ** AggregateEvaluationResult
    AggregateEvaluationResult (AggregateEvaluationResult'),
    newAggregateEvaluationResult,

    -- ** AggregateResourceIdentifier
    AggregateResourceIdentifier (AggregateResourceIdentifier'),
    newAggregateResourceIdentifier,

    -- ** AggregatedSourceStatus
    AggregatedSourceStatus (AggregatedSourceStatus'),
    newAggregatedSourceStatus,

    -- ** AggregationAuthorization
    AggregationAuthorization (AggregationAuthorization'),
    newAggregationAuthorization,

    -- ** BaseConfigurationItem
    BaseConfigurationItem (BaseConfigurationItem'),
    newBaseConfigurationItem,

    -- ** Compliance
    Compliance (Compliance'),
    newCompliance,

    -- ** ComplianceByConfigRule
    ComplianceByConfigRule (ComplianceByConfigRule'),
    newComplianceByConfigRule,

    -- ** ComplianceByResource
    ComplianceByResource (ComplianceByResource'),
    newComplianceByResource,

    -- ** ComplianceContributorCount
    ComplianceContributorCount (ComplianceContributorCount'),
    newComplianceContributorCount,

    -- ** ComplianceSummary
    ComplianceSummary (ComplianceSummary'),
    newComplianceSummary,

    -- ** ComplianceSummaryByResourceType
    ComplianceSummaryByResourceType (ComplianceSummaryByResourceType'),
    newComplianceSummaryByResourceType,

    -- ** ConfigExportDeliveryInfo
    ConfigExportDeliveryInfo (ConfigExportDeliveryInfo'),
    newConfigExportDeliveryInfo,

    -- ** ConfigRule
    ConfigRule (ConfigRule'),
    newConfigRule,

    -- ** ConfigRuleComplianceFilters
    ConfigRuleComplianceFilters (ConfigRuleComplianceFilters'),
    newConfigRuleComplianceFilters,

    -- ** ConfigRuleComplianceSummaryFilters
    ConfigRuleComplianceSummaryFilters (ConfigRuleComplianceSummaryFilters'),
    newConfigRuleComplianceSummaryFilters,

    -- ** ConfigRuleEvaluationStatus
    ConfigRuleEvaluationStatus (ConfigRuleEvaluationStatus'),
    newConfigRuleEvaluationStatus,

    -- ** ConfigSnapshotDeliveryProperties
    ConfigSnapshotDeliveryProperties (ConfigSnapshotDeliveryProperties'),
    newConfigSnapshotDeliveryProperties,

    -- ** ConfigStreamDeliveryInfo
    ConfigStreamDeliveryInfo (ConfigStreamDeliveryInfo'),
    newConfigStreamDeliveryInfo,

    -- ** ConfigurationAggregator
    ConfigurationAggregator (ConfigurationAggregator'),
    newConfigurationAggregator,

    -- ** ConfigurationItem
    ConfigurationItem (ConfigurationItem'),
    newConfigurationItem,

    -- ** ConfigurationRecorder
    ConfigurationRecorder (ConfigurationRecorder'),
    newConfigurationRecorder,

    -- ** ConfigurationRecorderStatus
    ConfigurationRecorderStatus (ConfigurationRecorderStatus'),
    newConfigurationRecorderStatus,

    -- ** ConformancePackComplianceFilters
    ConformancePackComplianceFilters (ConformancePackComplianceFilters'),
    newConformancePackComplianceFilters,

    -- ** ConformancePackComplianceSummary
    ConformancePackComplianceSummary (ConformancePackComplianceSummary'),
    newConformancePackComplianceSummary,

    -- ** ConformancePackDetail
    ConformancePackDetail (ConformancePackDetail'),
    newConformancePackDetail,

    -- ** ConformancePackEvaluationFilters
    ConformancePackEvaluationFilters (ConformancePackEvaluationFilters'),
    newConformancePackEvaluationFilters,

    -- ** ConformancePackEvaluationResult
    ConformancePackEvaluationResult (ConformancePackEvaluationResult'),
    newConformancePackEvaluationResult,

    -- ** ConformancePackInputParameter
    ConformancePackInputParameter (ConformancePackInputParameter'),
    newConformancePackInputParameter,

    -- ** ConformancePackRuleCompliance
    ConformancePackRuleCompliance (ConformancePackRuleCompliance'),
    newConformancePackRuleCompliance,

    -- ** ConformancePackStatusDetail
    ConformancePackStatusDetail (ConformancePackStatusDetail'),
    newConformancePackStatusDetail,

    -- ** DeliveryChannel
    DeliveryChannel (DeliveryChannel'),
    newDeliveryChannel,

    -- ** DeliveryChannelStatus
    DeliveryChannelStatus (DeliveryChannelStatus'),
    newDeliveryChannelStatus,

    -- ** Evaluation
    Evaluation (Evaluation'),
    newEvaluation,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** EvaluationResultIdentifier
    EvaluationResultIdentifier (EvaluationResultIdentifier'),
    newEvaluationResultIdentifier,

    -- ** EvaluationResultQualifier
    EvaluationResultQualifier (EvaluationResultQualifier'),
    newEvaluationResultQualifier,

    -- ** ExecutionControls
    ExecutionControls (ExecutionControls'),
    newExecutionControls,

    -- ** ExternalEvaluation
    ExternalEvaluation (ExternalEvaluation'),
    newExternalEvaluation,

    -- ** FailedDeleteRemediationExceptionsBatch
    FailedDeleteRemediationExceptionsBatch (FailedDeleteRemediationExceptionsBatch'),
    newFailedDeleteRemediationExceptionsBatch,

    -- ** FailedRemediationBatch
    FailedRemediationBatch (FailedRemediationBatch'),
    newFailedRemediationBatch,

    -- ** FailedRemediationExceptionBatch
    FailedRemediationExceptionBatch (FailedRemediationExceptionBatch'),
    newFailedRemediationExceptionBatch,

    -- ** FieldInfo
    FieldInfo (FieldInfo'),
    newFieldInfo,

    -- ** GroupedResourceCount
    GroupedResourceCount (GroupedResourceCount'),
    newGroupedResourceCount,

    -- ** MemberAccountStatus
    MemberAccountStatus (MemberAccountStatus'),
    newMemberAccountStatus,

    -- ** OrganizationAggregationSource
    OrganizationAggregationSource (OrganizationAggregationSource'),
    newOrganizationAggregationSource,

    -- ** OrganizationConfigRule
    OrganizationConfigRule (OrganizationConfigRule'),
    newOrganizationConfigRule,

    -- ** OrganizationConfigRuleStatus
    OrganizationConfigRuleStatus (OrganizationConfigRuleStatus'),
    newOrganizationConfigRuleStatus,

    -- ** OrganizationConformancePack
    OrganizationConformancePack (OrganizationConformancePack'),
    newOrganizationConformancePack,

    -- ** OrganizationConformancePackDetailedStatus
    OrganizationConformancePackDetailedStatus (OrganizationConformancePackDetailedStatus'),
    newOrganizationConformancePackDetailedStatus,

    -- ** OrganizationConformancePackStatus
    OrganizationConformancePackStatus (OrganizationConformancePackStatus'),
    newOrganizationConformancePackStatus,

    -- ** OrganizationCustomRuleMetadata
    OrganizationCustomRuleMetadata (OrganizationCustomRuleMetadata'),
    newOrganizationCustomRuleMetadata,

    -- ** OrganizationManagedRuleMetadata
    OrganizationManagedRuleMetadata (OrganizationManagedRuleMetadata'),
    newOrganizationManagedRuleMetadata,

    -- ** OrganizationResourceDetailedStatusFilters
    OrganizationResourceDetailedStatusFilters (OrganizationResourceDetailedStatusFilters'),
    newOrganizationResourceDetailedStatusFilters,

    -- ** PendingAggregationRequest
    PendingAggregationRequest (PendingAggregationRequest'),
    newPendingAggregationRequest,

    -- ** QueryInfo
    QueryInfo (QueryInfo'),
    newQueryInfo,

    -- ** RecordingGroup
    RecordingGroup (RecordingGroup'),
    newRecordingGroup,

    -- ** Relationship
    Relationship (Relationship'),
    newRelationship,

    -- ** RemediationConfiguration
    RemediationConfiguration (RemediationConfiguration'),
    newRemediationConfiguration,

    -- ** RemediationException
    RemediationException (RemediationException'),
    newRemediationException,

    -- ** RemediationExceptionResourceKey
    RemediationExceptionResourceKey (RemediationExceptionResourceKey'),
    newRemediationExceptionResourceKey,

    -- ** RemediationExecutionStatus
    RemediationExecutionStatus (RemediationExecutionStatus'),
    newRemediationExecutionStatus,

    -- ** RemediationExecutionStep
    RemediationExecutionStep (RemediationExecutionStep'),
    newRemediationExecutionStep,

    -- ** RemediationParameterValue
    RemediationParameterValue (RemediationParameterValue'),
    newRemediationParameterValue,

    -- ** ResourceCount
    ResourceCount (ResourceCount'),
    newResourceCount,

    -- ** ResourceCountFilters
    ResourceCountFilters (ResourceCountFilters'),
    newResourceCountFilters,

    -- ** ResourceFilters
    ResourceFilters (ResourceFilters'),
    newResourceFilters,

    -- ** ResourceIdentifier
    ResourceIdentifier (ResourceIdentifier'),
    newResourceIdentifier,

    -- ** ResourceKey
    ResourceKey (ResourceKey'),
    newResourceKey,

    -- ** ResourceValue
    ResourceValue (ResourceValue'),
    newResourceValue,

    -- ** RetentionConfiguration
    RetentionConfiguration (RetentionConfiguration'),
    newRetentionConfiguration,

    -- ** Scope
    Scope (Scope'),
    newScope,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SourceDetail
    SourceDetail (SourceDetail'),
    newSourceDetail,

    -- ** SsmControls
    SsmControls (SsmControls'),
    newSsmControls,

    -- ** StaticValue
    StaticValue (StaticValue'),
    newStaticValue,

    -- ** StatusDetailFilters
    StatusDetailFilters (StatusDetailFilters'),
    newStatusDetailFilters,

    -- ** StoredQuery
    StoredQuery (StoredQuery'),
    newStoredQuery,

    -- ** StoredQueryMetadata
    StoredQueryMetadata (StoredQueryMetadata'),
    newStoredQueryMetadata,

    -- ** Tag
    Tag (Tag'),
    newTag,
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
import Network.AWS.Config.Lens
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
import Network.AWS.Config.Types
import Network.AWS.Config.UntagResource
import Network.AWS.Config.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Config'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

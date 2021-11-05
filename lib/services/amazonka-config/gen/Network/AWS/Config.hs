{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-11-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Config
--
-- Config provides a way to keep track of the configurations of all the
-- Amazon Web Services resources associated with your Amazon Web Services
-- account. You can use Config to get the current and historical
-- configurations of each Amazon Web Services resource and also to get
-- information about the relationship between the resources. An Amazon Web
-- Services resource can be an Amazon Compute Cloud (Amazon EC2) instance,
-- an Elastic Block Store (EBS) volume, an elastic network Interface (ENI),
-- or a security group. For a complete list of resources currently
-- supported by Config, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported Amazon Web Services resources>.
--
-- You can access and manage Config through the Amazon Web Services
-- Management Console, the Amazon Web Services Command Line Interface
-- (Amazon Web Services CLI), the Config API, or the Amazon Web Services
-- SDKs for Config. This reference guide contains documentation for the
-- Config API and the Amazon Web Services CLI commands that you can use to
-- manage Config. The Config API uses the Signature Version 4 protocol for
-- signing requests. For more information about how to sign a request with
-- this protocol, see
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
-- For detailed information about Config features and their associated
-- actions or commands, as well as how to work with Amazon Web Services
-- Management Console, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is Config>
-- in the /Config Developer Guide/.
module Amazonka.Config
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NoSuchRemediationConfigurationException
    _NoSuchRemediationConfigurationException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** NoSuchOrganizationConformancePackException
    _NoSuchOrganizationConformancePackException,

    -- ** InvalidSNSTopicARNException
    _InvalidSNSTopicARNException,

    -- ** InvalidRecordingGroupException
    _InvalidRecordingGroupException,

    -- ** InvalidExpressionException
    _InvalidExpressionException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** ValidationException
    _ValidationException,

    -- ** OrganizationAccessDeniedException
    _OrganizationAccessDeniedException,

    -- ** NoSuchConfigurationAggregatorException
    _NoSuchConfigurationAggregatorException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** OversizedConfigurationItemException
    _OversizedConfigurationItemException,

    -- ** ResourceConcurrentModificationException
    _ResourceConcurrentModificationException,

    -- ** LastDeliveryChannelDeleteFailedException
    _LastDeliveryChannelDeleteFailedException,

    -- ** ConformancePackTemplateValidationException
    _ConformancePackTemplateValidationException,

    -- ** NoSuchRemediationExceptionException
    _NoSuchRemediationExceptionException,

    -- ** InvalidLimitException
    _InvalidLimitException,

    -- ** MaxNumberOfOrganizationConformancePacksExceededException
    _MaxNumberOfOrganizationConformancePacksExceededException,

    -- ** InvalidDeliveryChannelNameException
    _InvalidDeliveryChannelNameException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidS3KmsKeyArnException
    _InvalidS3KmsKeyArnException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidResultTokenException
    _InvalidResultTokenException,

    -- ** NoSuchConfigRuleInConformancePackException
    _NoSuchConfigRuleInConformancePackException,

    -- ** NoSuchOrganizationConfigRuleException
    _NoSuchOrganizationConfigRuleException,

    -- ** NoSuchDeliveryChannelException
    _NoSuchDeliveryChannelException,

    -- ** NoSuchConfigRuleException
    _NoSuchConfigRuleException,

    -- ** NoSuchConformancePackException
    _NoSuchConformancePackException,

    -- ** NoSuchRetentionConfigurationException
    _NoSuchRetentionConfigurationException,

    -- ** RemediationInProgressException
    _RemediationInProgressException,

    -- ** OrganizationAllFeaturesNotEnabledException
    _OrganizationAllFeaturesNotEnabledException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** ResourceNotDiscoveredException
    _ResourceNotDiscoveredException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** MaxNumberOfRetentionConfigurationsExceededException
    _MaxNumberOfRetentionConfigurationsExceededException,

    -- ** MaxNumberOfConformancePacksExceededException
    _MaxNumberOfConformancePacksExceededException,

    -- ** MaxNumberOfConfigRulesExceededException
    _MaxNumberOfConfigRulesExceededException,

    -- ** NoAvailableConfigurationRecorderException
    _NoAvailableConfigurationRecorderException,

    -- ** NoSuchBucketException
    _NoSuchBucketException,

    -- ** MaxActiveResourcesExceededException
    _MaxActiveResourcesExceededException,

    -- ** NoAvailableDeliveryChannelException
    _NoAvailableDeliveryChannelException,

    -- ** OrganizationConformancePackTemplateValidationException
    _OrganizationConformancePackTemplateValidationException,

    -- ** InvalidConfigurationRecorderNameException
    _InvalidConfigurationRecorderNameException,

    -- ** NoRunningConfigurationRecorderException
    _NoRunningConfigurationRecorderException,

    -- ** MaxNumberOfConfigurationRecordersExceededException
    _MaxNumberOfConfigurationRecordersExceededException,

    -- ** InsufficientDeliveryPolicyException
    _InsufficientDeliveryPolicyException,

    -- ** MaxNumberOfDeliveryChannelsExceededException
    _MaxNumberOfDeliveryChannelsExceededException,

    -- ** MaxNumberOfOrganizationConfigRulesExceededException
    _MaxNumberOfOrganizationConfigRulesExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** NoSuchConfigurationRecorderException
    _NoSuchConfigurationRecorderException,

    -- ** InvalidS3KeyPrefixException
    _InvalidS3KeyPrefixException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

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

    -- ** DescribeComplianceByConfigRule (Paginated)
    DescribeComplianceByConfigRule (DescribeComplianceByConfigRule'),
    newDescribeComplianceByConfigRule,
    DescribeComplianceByConfigRuleResponse (DescribeComplianceByConfigRuleResponse'),
    newDescribeComplianceByConfigRuleResponse,

    -- ** DescribeRetentionConfigurations (Paginated)
    DescribeRetentionConfigurations (DescribeRetentionConfigurations'),
    newDescribeRetentionConfigurations,
    DescribeRetentionConfigurationsResponse (DescribeRetentionConfigurationsResponse'),
    newDescribeRetentionConfigurationsResponse,

    -- ** StopConfigurationRecorder
    StopConfigurationRecorder (StopConfigurationRecorder'),
    newStopConfigurationRecorder,
    StopConfigurationRecorderResponse (StopConfigurationRecorderResponse'),
    newStopConfigurationRecorderResponse,

    -- ** GetAggregateConfigRuleComplianceSummary
    GetAggregateConfigRuleComplianceSummary (GetAggregateConfigRuleComplianceSummary'),
    newGetAggregateConfigRuleComplianceSummary,
    GetAggregateConfigRuleComplianceSummaryResponse (GetAggregateConfigRuleComplianceSummaryResponse'),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** BatchGetResourceConfig
    BatchGetResourceConfig (BatchGetResourceConfig'),
    newBatchGetResourceConfig,
    BatchGetResourceConfigResponse (BatchGetResourceConfigResponse'),
    newBatchGetResourceConfigResponse,

    -- ** DescribeConfigRules (Paginated)
    DescribeConfigRules (DescribeConfigRules'),
    newDescribeConfigRules,
    DescribeConfigRulesResponse (DescribeConfigRulesResponse'),
    newDescribeConfigRulesResponse,

    -- ** PutRetentionConfiguration
    PutRetentionConfiguration (PutRetentionConfiguration'),
    newPutRetentionConfiguration,
    PutRetentionConfigurationResponse (PutRetentionConfigurationResponse'),
    newPutRetentionConfigurationResponse,

    -- ** DescribeAggregateComplianceByConformancePacks (Paginated)
    DescribeAggregateComplianceByConformancePacks (DescribeAggregateComplianceByConformancePacks'),
    newDescribeAggregateComplianceByConformancePacks,
    DescribeAggregateComplianceByConformancePacksResponse (DescribeAggregateComplianceByConformancePacksResponse'),
    newDescribeAggregateComplianceByConformancePacksResponse,

    -- ** GetOrganizationConformancePackDetailedStatus (Paginated)
    GetOrganizationConformancePackDetailedStatus (GetOrganizationConformancePackDetailedStatus'),
    newGetOrganizationConformancePackDetailedStatus,
    GetOrganizationConformancePackDetailedStatusResponse (GetOrganizationConformancePackDetailedStatusResponse'),
    newGetOrganizationConformancePackDetailedStatusResponse,

    -- ** DescribeAggregateComplianceByConfigRules (Paginated)
    DescribeAggregateComplianceByConfigRules (DescribeAggregateComplianceByConfigRules'),
    newDescribeAggregateComplianceByConfigRules,
    DescribeAggregateComplianceByConfigRulesResponse (DescribeAggregateComplianceByConfigRulesResponse'),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- ** DeleteEvaluationResults
    DeleteEvaluationResults (DeleteEvaluationResults'),
    newDeleteEvaluationResults,
    DeleteEvaluationResultsResponse (DeleteEvaluationResultsResponse'),
    newDeleteEvaluationResultsResponse,

    -- ** PutConfigRule
    PutConfigRule (PutConfigRule'),
    newPutConfigRule,
    PutConfigRuleResponse (PutConfigRuleResponse'),
    newPutConfigRuleResponse,

    -- ** GetConformancePackComplianceDetails
    GetConformancePackComplianceDetails (GetConformancePackComplianceDetails'),
    newGetConformancePackComplianceDetails,
    GetConformancePackComplianceDetailsResponse (GetConformancePackComplianceDetailsResponse'),
    newGetConformancePackComplianceDetailsResponse,

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

    -- ** ListStoredQueries
    ListStoredQueries (ListStoredQueries'),
    newListStoredQueries,
    ListStoredQueriesResponse (ListStoredQueriesResponse'),
    newListStoredQueriesResponse,

    -- ** SelectResourceConfig (Paginated)
    SelectResourceConfig (SelectResourceConfig'),
    newSelectResourceConfig,
    SelectResourceConfigResponse (SelectResourceConfigResponse'),
    newSelectResourceConfigResponse,

    -- ** ListAggregateDiscoveredResources (Paginated)
    ListAggregateDiscoveredResources (ListAggregateDiscoveredResources'),
    newListAggregateDiscoveredResources,
    ListAggregateDiscoveredResourcesResponse (ListAggregateDiscoveredResourcesResponse'),
    newListAggregateDiscoveredResourcesResponse,

    -- ** DescribeOrganizationConfigRuleStatuses (Paginated)
    DescribeOrganizationConfigRuleStatuses (DescribeOrganizationConfigRuleStatuses'),
    newDescribeOrganizationConfigRuleStatuses,
    DescribeOrganizationConfigRuleStatusesResponse (DescribeOrganizationConfigRuleStatusesResponse'),
    newDescribeOrganizationConfigRuleStatusesResponse,

    -- ** DescribeOrganizationConformancePackStatuses (Paginated)
    DescribeOrganizationConformancePackStatuses (DescribeOrganizationConformancePackStatuses'),
    newDescribeOrganizationConformancePackStatuses,
    DescribeOrganizationConformancePackStatusesResponse (DescribeOrganizationConformancePackStatusesResponse'),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- ** GetComplianceDetailsByResource (Paginated)
    GetComplianceDetailsByResource (GetComplianceDetailsByResource'),
    newGetComplianceDetailsByResource,
    GetComplianceDetailsByResourceResponse (GetComplianceDetailsByResourceResponse'),
    newGetComplianceDetailsByResourceResponse,

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

    -- ** DescribeConfigRuleEvaluationStatus (Paginated)
    DescribeConfigRuleEvaluationStatus (DescribeConfigRuleEvaluationStatus'),
    newDescribeConfigRuleEvaluationStatus,
    DescribeConfigRuleEvaluationStatusResponse (DescribeConfigRuleEvaluationStatusResponse'),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- ** GetDiscoveredResourceCounts
    GetDiscoveredResourceCounts (GetDiscoveredResourceCounts'),
    newGetDiscoveredResourceCounts,
    GetDiscoveredResourceCountsResponse (GetDiscoveredResourceCountsResponse'),
    newGetDiscoveredResourceCountsResponse,

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

    -- ** PutOrganizationConfigRule
    PutOrganizationConfigRule (PutOrganizationConfigRule'),
    newPutOrganizationConfigRule,
    PutOrganizationConfigRuleResponse (PutOrganizationConfigRuleResponse'),
    newPutOrganizationConfigRuleResponse,

    -- ** PutOrganizationConformancePack
    PutOrganizationConformancePack (PutOrganizationConformancePack'),
    newPutOrganizationConformancePack,
    PutOrganizationConformancePackResponse (PutOrganizationConformancePackResponse'),
    newPutOrganizationConformancePackResponse,

    -- ** DeleteOrganizationConfigRule
    DeleteOrganizationConfigRule (DeleteOrganizationConfigRule'),
    newDeleteOrganizationConfigRule,
    DeleteOrganizationConfigRuleResponse (DeleteOrganizationConfigRuleResponse'),
    newDeleteOrganizationConfigRuleResponse,

    -- ** PutResourceConfig
    PutResourceConfig (PutResourceConfig'),
    newPutResourceConfig,
    PutResourceConfigResponse (PutResourceConfigResponse'),
    newPutResourceConfigResponse,

    -- ** StartConfigRulesEvaluation
    StartConfigRulesEvaluation (StartConfigRulesEvaluation'),
    newStartConfigRulesEvaluation,
    StartConfigRulesEvaluationResponse (StartConfigRulesEvaluationResponse'),
    newStartConfigRulesEvaluationResponse,

    -- ** DescribeOrganizationConfigRules (Paginated)
    DescribeOrganizationConfigRules (DescribeOrganizationConfigRules'),
    newDescribeOrganizationConfigRules,
    DescribeOrganizationConfigRulesResponse (DescribeOrganizationConfigRulesResponse'),
    newDescribeOrganizationConfigRulesResponse,

    -- ** SelectAggregateResourceConfig (Paginated)
    SelectAggregateResourceConfig (SelectAggregateResourceConfig'),
    newSelectAggregateResourceConfig,
    SelectAggregateResourceConfigResponse (SelectAggregateResourceConfigResponse'),
    newSelectAggregateResourceConfigResponse,

    -- ** DescribeComplianceByResource (Paginated)
    DescribeComplianceByResource (DescribeComplianceByResource'),
    newDescribeComplianceByResource,
    DescribeComplianceByResourceResponse (DescribeComplianceByResourceResponse'),
    newDescribeComplianceByResourceResponse,

    -- ** DescribeOrganizationConformancePacks (Paginated)
    DescribeOrganizationConformancePacks (DescribeOrganizationConformancePacks'),
    newDescribeOrganizationConformancePacks,
    DescribeOrganizationConformancePacksResponse (DescribeOrganizationConformancePacksResponse'),
    newDescribeOrganizationConformancePacksResponse,

    -- ** DeleteResourceConfig
    DeleteResourceConfig (DeleteResourceConfig'),
    newDeleteResourceConfig,
    DeleteResourceConfigResponse (DeleteResourceConfigResponse'),
    newDeleteResourceConfigResponse,

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

    -- ** DescribeConformancePackCompliance
    DescribeConformancePackCompliance (DescribeConformancePackCompliance'),
    newDescribeConformancePackCompliance,
    DescribeConformancePackComplianceResponse (DescribeConformancePackComplianceResponse'),
    newDescribeConformancePackComplianceResponse,

    -- ** GetAggregateComplianceDetailsByConfigRule (Paginated)
    GetAggregateComplianceDetailsByConfigRule (GetAggregateComplianceDetailsByConfigRule'),
    newGetAggregateComplianceDetailsByConfigRule,
    GetAggregateComplianceDetailsByConfigRuleResponse (GetAggregateComplianceDetailsByConfigRuleResponse'),
    newGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** GetAggregateDiscoveredResourceCounts
    GetAggregateDiscoveredResourceCounts (GetAggregateDiscoveredResourceCounts'),
    newGetAggregateDiscoveredResourceCounts,
    GetAggregateDiscoveredResourceCountsResponse (GetAggregateDiscoveredResourceCountsResponse'),
    newGetAggregateDiscoveredResourceCountsResponse,

    -- ** GetAggregateConformancePackComplianceSummary
    GetAggregateConformancePackComplianceSummary (GetAggregateConformancePackComplianceSummary'),
    newGetAggregateConformancePackComplianceSummary,
    GetAggregateConformancePackComplianceSummaryResponse (GetAggregateConformancePackComplianceSummaryResponse'),
    newGetAggregateConformancePackComplianceSummaryResponse,

    -- ** StartConfigurationRecorder
    StartConfigurationRecorder (StartConfigurationRecorder'),
    newStartConfigurationRecorder,
    StartConfigurationRecorderResponse (StartConfigurationRecorderResponse'),
    newStartConfigurationRecorderResponse,

    -- ** DescribeConformancePacks (Paginated)
    DescribeConformancePacks (DescribeConformancePacks'),
    newDescribeConformancePacks,
    DescribeConformancePacksResponse (DescribeConformancePacksResponse'),
    newDescribeConformancePacksResponse,

    -- ** PutExternalEvaluation
    PutExternalEvaluation (PutExternalEvaluation'),
    newPutExternalEvaluation,
    PutExternalEvaluationResponse (PutExternalEvaluationResponse'),
    newPutExternalEvaluationResponse,

    -- ** DeleteRemediationExceptions
    DeleteRemediationExceptions (DeleteRemediationExceptions'),
    newDeleteRemediationExceptions,
    DeleteRemediationExceptionsResponse (DeleteRemediationExceptionsResponse'),
    newDeleteRemediationExceptionsResponse,

    -- ** PutRemediationExceptions
    PutRemediationExceptions (PutRemediationExceptions'),
    newPutRemediationExceptions,
    PutRemediationExceptionsResponse (PutRemediationExceptionsResponse'),
    newPutRemediationExceptionsResponse,

    -- ** GetOrganizationConfigRuleDetailedStatus (Paginated)
    GetOrganizationConfigRuleDetailedStatus (GetOrganizationConfigRuleDetailedStatus'),
    newGetOrganizationConfigRuleDetailedStatus,
    GetOrganizationConfigRuleDetailedStatusResponse (GetOrganizationConfigRuleDetailedStatusResponse'),
    newGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** PutRemediationConfigurations
    PutRemediationConfigurations (PutRemediationConfigurations'),
    newPutRemediationConfigurations,
    PutRemediationConfigurationsResponse (PutRemediationConfigurationsResponse'),
    newPutRemediationConfigurationsResponse,

    -- ** DeleteConformancePack
    DeleteConformancePack (DeleteConformancePack'),
    newDeleteConformancePack,
    DeleteConformancePackResponse (DeleteConformancePackResponse'),
    newDeleteConformancePackResponse,

    -- ** PutConformancePack
    PutConformancePack (PutConformancePack'),
    newPutConformancePack,
    PutConformancePackResponse (PutConformancePackResponse'),
    newPutConformancePackResponse,

    -- ** StartRemediationExecution
    StartRemediationExecution (StartRemediationExecution'),
    newStartRemediationExecution,
    StartRemediationExecutionResponse (StartRemediationExecutionResponse'),
    newStartRemediationExecutionResponse,

    -- ** DescribeConformancePackStatus (Paginated)
    DescribeConformancePackStatus (DescribeConformancePackStatus'),
    newDescribeConformancePackStatus,
    DescribeConformancePackStatusResponse (DescribeConformancePackStatusResponse'),
    newDescribeConformancePackStatusResponse,

    -- ** GetComplianceSummaryByConfigRule
    GetComplianceSummaryByConfigRule (GetComplianceSummaryByConfigRule'),
    newGetComplianceSummaryByConfigRule,
    GetComplianceSummaryByConfigRuleResponse (GetComplianceSummaryByConfigRuleResponse'),
    newGetComplianceSummaryByConfigRuleResponse,

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

    -- ** DeleteStoredQuery
    DeleteStoredQuery (DeleteStoredQuery'),
    newDeleteStoredQuery,
    DeleteStoredQueryResponse (DeleteStoredQueryResponse'),
    newDeleteStoredQueryResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteConfigurationAggregator
    DeleteConfigurationAggregator (DeleteConfigurationAggregator'),
    newDeleteConfigurationAggregator,
    DeleteConfigurationAggregatorResponse (DeleteConfigurationAggregatorResponse'),
    newDeleteConfigurationAggregatorResponse,

    -- ** DescribeConfigurationRecorderStatus
    DescribeConfigurationRecorderStatus (DescribeConfigurationRecorderStatus'),
    newDescribeConfigurationRecorderStatus,
    DescribeConfigurationRecorderStatusResponse (DescribeConfigurationRecorderStatusResponse'),
    newDescribeConfigurationRecorderStatusResponse,

    -- ** PutConfigurationRecorder
    PutConfigurationRecorder (PutConfigurationRecorder'),
    newPutConfigurationRecorder,
    PutConfigurationRecorderResponse (PutConfigurationRecorderResponse'),
    newPutConfigurationRecorderResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteConfigurationRecorder
    DeleteConfigurationRecorder (DeleteConfigurationRecorder'),
    newDeleteConfigurationRecorder,
    DeleteConfigurationRecorderResponse (DeleteConfigurationRecorderResponse'),
    newDeleteConfigurationRecorderResponse,

    -- ** GetConformancePackComplianceSummary (Paginated)
    GetConformancePackComplianceSummary (GetConformancePackComplianceSummary'),
    newGetConformancePackComplianceSummary,
    GetConformancePackComplianceSummaryResponse (GetConformancePackComplianceSummaryResponse'),
    newGetConformancePackComplianceSummaryResponse,

    -- ** GetComplianceSummaryByResourceType
    GetComplianceSummaryByResourceType (GetComplianceSummaryByResourceType'),
    newGetComplianceSummaryByResourceType,
    GetComplianceSummaryByResourceTypeResponse (GetComplianceSummaryByResourceTypeResponse'),
    newGetComplianceSummaryByResourceTypeResponse,

    -- ** DescribeDeliveryChannelStatus
    DescribeDeliveryChannelStatus (DescribeDeliveryChannelStatus'),
    newDescribeDeliveryChannelStatus,
    DescribeDeliveryChannelStatusResponse (DescribeDeliveryChannelStatusResponse'),
    newDescribeDeliveryChannelStatusResponse,

    -- ** PutDeliveryChannel
    PutDeliveryChannel (PutDeliveryChannel'),
    newPutDeliveryChannel,
    PutDeliveryChannelResponse (PutDeliveryChannelResponse'),
    newPutDeliveryChannelResponse,

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    GetComplianceDetailsByConfigRule (GetComplianceDetailsByConfigRule'),
    newGetComplianceDetailsByConfigRule,
    GetComplianceDetailsByConfigRuleResponse (GetComplianceDetailsByConfigRuleResponse'),
    newGetComplianceDetailsByConfigRuleResponse,

    -- ** DeleteAggregationAuthorization
    DeleteAggregationAuthorization (DeleteAggregationAuthorization'),
    newDeleteAggregationAuthorization,
    DeleteAggregationAuthorizationResponse (DeleteAggregationAuthorizationResponse'),
    newDeleteAggregationAuthorizationResponse,

    -- ** DeleteDeliveryChannel
    DeleteDeliveryChannel (DeleteDeliveryChannel'),
    newDeleteDeliveryChannel,
    DeleteDeliveryChannelResponse (DeleteDeliveryChannelResponse'),
    newDeleteDeliveryChannelResponse,

    -- ** DeleteRemediationConfiguration
    DeleteRemediationConfiguration (DeleteRemediationConfiguration'),
    newDeleteRemediationConfiguration,
    DeleteRemediationConfigurationResponse (DeleteRemediationConfigurationResponse'),
    newDeleteRemediationConfigurationResponse,

    -- ** PutAggregationAuthorization
    PutAggregationAuthorization (PutAggregationAuthorization'),
    newPutAggregationAuthorization,
    PutAggregationAuthorizationResponse (PutAggregationAuthorizationResponse'),
    newPutAggregationAuthorizationResponse,

    -- ** DescribeConfigurationAggregatorSourcesStatus (Paginated)
    DescribeConfigurationAggregatorSourcesStatus (DescribeConfigurationAggregatorSourcesStatus'),
    newDescribeConfigurationAggregatorSourcesStatus,
    DescribeConfigurationAggregatorSourcesStatusResponse (DescribeConfigurationAggregatorSourcesStatusResponse'),
    newDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** ListDiscoveredResources (Paginated)
    ListDiscoveredResources (ListDiscoveredResources'),
    newListDiscoveredResources,
    ListDiscoveredResourcesResponse (ListDiscoveredResourcesResponse'),
    newListDiscoveredResourcesResponse,

    -- ** DescribeRemediationConfigurations
    DescribeRemediationConfigurations (DescribeRemediationConfigurations'),
    newDescribeRemediationConfigurations,
    DescribeRemediationConfigurationsResponse (DescribeRemediationConfigurationsResponse'),
    newDescribeRemediationConfigurationsResponse,

    -- ** DescribeDeliveryChannels
    DescribeDeliveryChannels (DescribeDeliveryChannels'),
    newDescribeDeliveryChannels,
    DescribeDeliveryChannelsResponse (DescribeDeliveryChannelsResponse'),
    newDescribeDeliveryChannelsResponse,

    -- ** DescribeAggregationAuthorizations (Paginated)
    DescribeAggregationAuthorizations (DescribeAggregationAuthorizations'),
    newDescribeAggregationAuthorizations,
    DescribeAggregationAuthorizationsResponse (DescribeAggregationAuthorizationsResponse'),
    newDescribeAggregationAuthorizationsResponse,

    -- * Types

    -- ** AggregateConformancePackComplianceSummaryGroupKey
    AggregateConformancePackComplianceSummaryGroupKey (..),

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

    -- ** AggregateComplianceByConformancePack
    AggregateComplianceByConformancePack (AggregateComplianceByConformancePack'),
    newAggregateComplianceByConformancePack,

    -- ** AggregateComplianceCount
    AggregateComplianceCount (AggregateComplianceCount'),
    newAggregateComplianceCount,

    -- ** AggregateConformancePackCompliance
    AggregateConformancePackCompliance (AggregateConformancePackCompliance'),
    newAggregateConformancePackCompliance,

    -- ** AggregateConformancePackComplianceCount
    AggregateConformancePackComplianceCount (AggregateConformancePackComplianceCount'),
    newAggregateConformancePackComplianceCount,

    -- ** AggregateConformancePackComplianceFilters
    AggregateConformancePackComplianceFilters (AggregateConformancePackComplianceFilters'),
    newAggregateConformancePackComplianceFilters,

    -- ** AggregateConformancePackComplianceSummary
    AggregateConformancePackComplianceSummary (AggregateConformancePackComplianceSummary'),
    newAggregateConformancePackComplianceSummary,

    -- ** AggregateConformancePackComplianceSummaryFilters
    AggregateConformancePackComplianceSummaryFilters (AggregateConformancePackComplianceSummaryFilters'),
    newAggregateConformancePackComplianceSummaryFilters,

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
import Amazonka.Config.GetDiscoveredResourceCounts
import Amazonka.Config.GetOrganizationConfigRuleDetailedStatus
import Amazonka.Config.GetOrganizationConformancePackDetailedStatus
import Amazonka.Config.GetResourceConfigHistory
import Amazonka.Config.GetStoredQuery
import Amazonka.Config.Lens
import Amazonka.Config.ListAggregateDiscoveredResources
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
import Amazonka.Config.Types
import Amazonka.Config.UntagResource
import Amazonka.Config.Waiters

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

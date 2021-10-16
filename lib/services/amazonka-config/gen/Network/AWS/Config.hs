{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Config
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
module Network.AWS.Config
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NoSuchRetentionConfigurationException
    _NoSuchRetentionConfigurationException,

    -- ** NoSuchConfigRuleException
    _NoSuchConfigRuleException,

    -- ** NoSuchOrganizationConformancePackException
    _NoSuchOrganizationConformancePackException,

    -- ** InvalidExpressionException
    _InvalidExpressionException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** NoSuchOrganizationConfigRuleException
    _NoSuchOrganizationConfigRuleException,

    -- ** InvalidResultTokenException
    _InvalidResultTokenException,

    -- ** InvalidRecordingGroupException
    _InvalidRecordingGroupException,

    -- ** InvalidS3KeyPrefixException
    _InvalidS3KeyPrefixException,

    -- ** TooManyTagsException
    _TooManyTagsException,

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

    -- ** ConformancePackTemplateValidationException
    _ConformancePackTemplateValidationException,

    -- ** LastDeliveryChannelDeleteFailedException
    _LastDeliveryChannelDeleteFailedException,

    -- ** MaxNumberOfConformancePacksExceededException
    _MaxNumberOfConformancePacksExceededException,

    -- ** OversizedConfigurationItemException
    _OversizedConfigurationItemException,

    -- ** NoSuchConfigurationAggregatorException
    _NoSuchConfigurationAggregatorException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** OrganizationAccessDeniedException
    _OrganizationAccessDeniedException,

    -- ** NoSuchConformancePackException
    _NoSuchConformancePackException,

    -- ** InvalidSNSTopicARNException
    _InvalidSNSTopicARNException,

    -- ** NoSuchRemediationConfigurationException
    _NoSuchRemediationConfigurationException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** NoSuchConfigRuleInConformancePackException
    _NoSuchConfigRuleInConformancePackException,

    -- ** ValidationException
    _ValidationException,

    -- ** NoSuchDeliveryChannelException
    _NoSuchDeliveryChannelException,

    -- ** InvalidS3KmsKeyArnException
    _InvalidS3KmsKeyArnException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** NoSuchConfigurationRecorderException
    _NoSuchConfigurationRecorderException,

    -- ** MaxNumberOfOrganizationConfigRulesExceededException
    _MaxNumberOfOrganizationConfigRulesExceededException,

    -- ** MaxNumberOfOrganizationConformancePacksExceededException
    _MaxNumberOfOrganizationConformancePacksExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InsufficientDeliveryPolicyException
    _InsufficientDeliveryPolicyException,

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

    -- ** MaxNumberOfRetentionConfigurationsExceededException
    _MaxNumberOfRetentionConfigurationsExceededException,

    -- ** NoSuchBucketException
    _NoSuchBucketException,

    -- ** MaxActiveResourcesExceededException
    _MaxActiveResourcesExceededException,

    -- ** NoAvailableConfigurationRecorderException
    _NoAvailableConfigurationRecorderException,

    -- ** MaxNumberOfConfigRulesExceededException
    _MaxNumberOfConfigRulesExceededException,

    -- ** OrganizationAllFeaturesNotEnabledException
    _OrganizationAllFeaturesNotEnabledException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** ResourceNotDiscoveredException
    _ResourceNotDiscoveredException,

    -- ** RemediationInProgressException
    _RemediationInProgressException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeComplianceByConfigRule (Paginated)
    DescribeComplianceByConfigRule (DescribeComplianceByConfigRule'),
    newDescribeComplianceByConfigRule,
    DescribeComplianceByConfigRuleResponse (DescribeComplianceByConfigRuleResponse'),
    newDescribeComplianceByConfigRuleResponse,

    -- ** GetAggregateResourceConfig
    GetAggregateResourceConfig (GetAggregateResourceConfig'),
    newGetAggregateResourceConfig,
    GetAggregateResourceConfigResponse (GetAggregateResourceConfigResponse'),
    newGetAggregateResourceConfigResponse,

    -- ** GetStoredQuery
    GetStoredQuery (GetStoredQuery'),
    newGetStoredQuery,
    GetStoredQueryResponse (GetStoredQueryResponse'),
    newGetStoredQueryResponse,

    -- ** DescribeConfigurationAggregators (Paginated)
    DescribeConfigurationAggregators (DescribeConfigurationAggregators'),
    newDescribeConfigurationAggregators,
    DescribeConfigurationAggregatorsResponse (DescribeConfigurationAggregatorsResponse'),
    newDescribeConfigurationAggregatorsResponse,

    -- ** GetAggregateComplianceDetailsByConfigRule (Paginated)
    GetAggregateComplianceDetailsByConfigRule (GetAggregateComplianceDetailsByConfigRule'),
    newGetAggregateComplianceDetailsByConfigRule,
    GetAggregateComplianceDetailsByConfigRuleResponse (GetAggregateComplianceDetailsByConfigRuleResponse'),
    newGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** GetResourceConfigHistory (Paginated)
    GetResourceConfigHistory (GetResourceConfigHistory'),
    newGetResourceConfigHistory,
    GetResourceConfigHistoryResponse (GetResourceConfigHistoryResponse'),
    newGetResourceConfigHistoryResponse,

    -- ** DescribeRemediationExecutionStatus (Paginated)
    DescribeRemediationExecutionStatus (DescribeRemediationExecutionStatus'),
    newDescribeRemediationExecutionStatus,
    DescribeRemediationExecutionStatusResponse (DescribeRemediationExecutionStatusResponse'),
    newDescribeRemediationExecutionStatusResponse,

    -- ** DescribePendingAggregationRequests (Paginated)
    DescribePendingAggregationRequests (DescribePendingAggregationRequests'),
    newDescribePendingAggregationRequests,
    DescribePendingAggregationRequestsResponse (DescribePendingAggregationRequestsResponse'),
    newDescribePendingAggregationRequestsResponse,

    -- ** DescribeConformancePackCompliance
    DescribeConformancePackCompliance (DescribeConformancePackCompliance'),
    newDescribeConformancePackCompliance,
    DescribeConformancePackComplianceResponse (DescribeConformancePackComplianceResponse'),
    newDescribeConformancePackComplianceResponse,

    -- ** StartConfigRulesEvaluation
    StartConfigRulesEvaluation (StartConfigRulesEvaluation'),
    newStartConfigRulesEvaluation,
    StartConfigRulesEvaluationResponse (StartConfigRulesEvaluationResponse'),
    newStartConfigRulesEvaluationResponse,

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

    -- ** DescribeConfigurationAggregatorSourcesStatus (Paginated)
    DescribeConfigurationAggregatorSourcesStatus (DescribeConfigurationAggregatorSourcesStatus'),
    newDescribeConfigurationAggregatorSourcesStatus,
    DescribeConfigurationAggregatorSourcesStatusResponse (DescribeConfigurationAggregatorSourcesStatusResponse'),
    newDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** DeleteOrganizationConformancePack
    DeleteOrganizationConformancePack (DeleteOrganizationConformancePack'),
    newDeleteOrganizationConformancePack,
    DeleteOrganizationConformancePackResponse (DeleteOrganizationConformancePackResponse'),
    newDeleteOrganizationConformancePackResponse,

    -- ** DeleteAggregationAuthorization
    DeleteAggregationAuthorization (DeleteAggregationAuthorization'),
    newDeleteAggregationAuthorization,
    DeleteAggregationAuthorizationResponse (DeleteAggregationAuthorizationResponse'),
    newDeleteAggregationAuthorizationResponse,

    -- ** DescribeRemediationExceptions
    DescribeRemediationExceptions (DescribeRemediationExceptions'),
    newDescribeRemediationExceptions,
    DescribeRemediationExceptionsResponse (DescribeRemediationExceptionsResponse'),
    newDescribeRemediationExceptionsResponse,

    -- ** DeleteRemediationConfiguration
    DeleteRemediationConfiguration (DeleteRemediationConfiguration'),
    newDeleteRemediationConfiguration,
    DeleteRemediationConfigurationResponse (DeleteRemediationConfigurationResponse'),
    newDeleteRemediationConfigurationResponse,

    -- ** GetComplianceSummaryByResourceType
    GetComplianceSummaryByResourceType (GetComplianceSummaryByResourceType'),
    newGetComplianceSummaryByResourceType,
    GetComplianceSummaryByResourceTypeResponse (GetComplianceSummaryByResourceTypeResponse'),
    newGetComplianceSummaryByResourceTypeResponse,

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    GetComplianceDetailsByConfigRule (GetComplianceDetailsByConfigRule'),
    newGetComplianceDetailsByConfigRule,
    GetComplianceDetailsByConfigRuleResponse (GetComplianceDetailsByConfigRuleResponse'),
    newGetComplianceDetailsByConfigRuleResponse,

    -- ** GetDiscoveredResourceCounts
    GetDiscoveredResourceCounts (GetDiscoveredResourceCounts'),
    newGetDiscoveredResourceCounts,
    GetDiscoveredResourceCountsResponse (GetDiscoveredResourceCountsResponse'),
    newGetDiscoveredResourceCountsResponse,

    -- ** PutDeliveryChannel
    PutDeliveryChannel (PutDeliveryChannel'),
    newPutDeliveryChannel,
    PutDeliveryChannelResponse (PutDeliveryChannelResponse'),
    newPutDeliveryChannelResponse,

    -- ** PutOrganizationConfigRule
    PutOrganizationConfigRule (PutOrganizationConfigRule'),
    newPutOrganizationConfigRule,
    PutOrganizationConfigRuleResponse (PutOrganizationConfigRuleResponse'),
    newPutOrganizationConfigRuleResponse,

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

    -- ** DescribeConfigurationRecorderStatus
    DescribeConfigurationRecorderStatus (DescribeConfigurationRecorderStatus'),
    newDescribeConfigurationRecorderStatus,
    DescribeConfigurationRecorderStatusResponse (DescribeConfigurationRecorderStatusResponse'),
    newDescribeConfigurationRecorderStatusResponse,

    -- ** DescribeConfigRuleEvaluationStatus (Paginated)
    DescribeConfigRuleEvaluationStatus (DescribeConfigRuleEvaluationStatus'),
    newDescribeConfigRuleEvaluationStatus,
    DescribeConfigRuleEvaluationStatusResponse (DescribeConfigRuleEvaluationStatusResponse'),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteConfigurationAggregator
    DeleteConfigurationAggregator (DeleteConfigurationAggregator'),
    newDeleteConfigurationAggregator,
    DeleteConfigurationAggregatorResponse (DeleteConfigurationAggregatorResponse'),
    newDeleteConfigurationAggregatorResponse,

    -- ** ListAggregateDiscoveredResources (Paginated)
    ListAggregateDiscoveredResources (ListAggregateDiscoveredResources'),
    newListAggregateDiscoveredResources,
    ListAggregateDiscoveredResourcesResponse (ListAggregateDiscoveredResourcesResponse'),
    newListAggregateDiscoveredResourcesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeOrganizationConfigRuleStatuses (Paginated)
    DescribeOrganizationConfigRuleStatuses (DescribeOrganizationConfigRuleStatuses'),
    newDescribeOrganizationConfigRuleStatuses,
    DescribeOrganizationConfigRuleStatusesResponse (DescribeOrganizationConfigRuleStatusesResponse'),
    newDescribeOrganizationConfigRuleStatusesResponse,

    -- ** SelectResourceConfig (Paginated)
    SelectResourceConfig (SelectResourceConfig'),
    newSelectResourceConfig,
    SelectResourceConfigResponse (SelectResourceConfigResponse'),
    newSelectResourceConfigResponse,

    -- ** DeleteStoredQuery
    DeleteStoredQuery (DeleteStoredQuery'),
    newDeleteStoredQuery,
    DeleteStoredQueryResponse (DeleteStoredQueryResponse'),
    newDeleteStoredQueryResponse,

    -- ** GetComplianceDetailsByResource (Paginated)
    GetComplianceDetailsByResource (GetComplianceDetailsByResource'),
    newGetComplianceDetailsByResource,
    GetComplianceDetailsByResourceResponse (GetComplianceDetailsByResourceResponse'),
    newGetComplianceDetailsByResourceResponse,

    -- ** ListStoredQueries
    ListStoredQueries (ListStoredQueries'),
    newListStoredQueries,
    ListStoredQueriesResponse (ListStoredQueriesResponse'),
    newListStoredQueriesResponse,

    -- ** DescribeAggregateComplianceByConformancePacks (Paginated)
    DescribeAggregateComplianceByConformancePacks (DescribeAggregateComplianceByConformancePacks'),
    newDescribeAggregateComplianceByConformancePacks,
    DescribeAggregateComplianceByConformancePacksResponse (DescribeAggregateComplianceByConformancePacksResponse'),
    newDescribeAggregateComplianceByConformancePacksResponse,

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

    -- ** PutRetentionConfiguration
    PutRetentionConfiguration (PutRetentionConfiguration'),
    newPutRetentionConfiguration,
    PutRetentionConfigurationResponse (PutRetentionConfigurationResponse'),
    newPutRetentionConfigurationResponse,

    -- ** PutConformancePack
    PutConformancePack (PutConformancePack'),
    newPutConformancePack,
    PutConformancePackResponse (PutConformancePackResponse'),
    newPutConformancePackResponse,

    -- ** GetConformancePackComplianceDetails
    GetConformancePackComplianceDetails (GetConformancePackComplianceDetails'),
    newGetConformancePackComplianceDetails,
    GetConformancePackComplianceDetailsResponse (GetConformancePackComplianceDetailsResponse'),
    newGetConformancePackComplianceDetailsResponse,

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

    -- ** DeleteRemediationExceptions
    DeleteRemediationExceptions (DeleteRemediationExceptions'),
    newDeleteRemediationExceptions,
    DeleteRemediationExceptionsResponse (DeleteRemediationExceptionsResponse'),
    newDeleteRemediationExceptionsResponse,

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

    -- ** SelectAggregateResourceConfig (Paginated)
    SelectAggregateResourceConfig (SelectAggregateResourceConfig'),
    newSelectAggregateResourceConfig,
    SelectAggregateResourceConfigResponse (SelectAggregateResourceConfigResponse'),
    newSelectAggregateResourceConfigResponse,

    -- ** DescribeDeliveryChannels
    DescribeDeliveryChannels (DescribeDeliveryChannels'),
    newDescribeDeliveryChannels,
    DescribeDeliveryChannelsResponse (DescribeDeliveryChannelsResponse'),
    newDescribeDeliveryChannelsResponse,

    -- ** PutResourceConfig
    PutResourceConfig (PutResourceConfig'),
    newPutResourceConfig,
    PutResourceConfigResponse (PutResourceConfigResponse'),
    newPutResourceConfigResponse,

    -- ** DescribeOrganizationConfigRules (Paginated)
    DescribeOrganizationConfigRules (DescribeOrganizationConfigRules'),
    newDescribeOrganizationConfigRules,
    DescribeOrganizationConfigRulesResponse (DescribeOrganizationConfigRulesResponse'),
    newDescribeOrganizationConfigRulesResponse,

    -- ** DeleteDeliveryChannel
    DeleteDeliveryChannel (DeleteDeliveryChannel'),
    newDeleteDeliveryChannel,
    DeleteDeliveryChannelResponse (DeleteDeliveryChannelResponse'),
    newDeleteDeliveryChannelResponse,

    -- ** PutOrganizationConformancePack
    PutOrganizationConformancePack (PutOrganizationConformancePack'),
    newPutOrganizationConformancePack,
    PutOrganizationConformancePackResponse (PutOrganizationConformancePackResponse'),
    newPutOrganizationConformancePackResponse,

    -- ** PutAggregationAuthorization
    PutAggregationAuthorization (PutAggregationAuthorization'),
    newPutAggregationAuthorization,
    PutAggregationAuthorizationResponse (PutAggregationAuthorizationResponse'),
    newPutAggregationAuthorizationResponse,

    -- ** DeleteOrganizationConfigRule
    DeleteOrganizationConfigRule (DeleteOrganizationConfigRule'),
    newDeleteOrganizationConfigRule,
    DeleteOrganizationConfigRuleResponse (DeleteOrganizationConfigRuleResponse'),
    newDeleteOrganizationConfigRuleResponse,

    -- ** DescribeDeliveryChannelStatus
    DescribeDeliveryChannelStatus (DescribeDeliveryChannelStatus'),
    newDescribeDeliveryChannelStatus,
    DescribeDeliveryChannelStatusResponse (DescribeDeliveryChannelStatusResponse'),
    newDescribeDeliveryChannelStatusResponse,

    -- ** BatchGetAggregateResourceConfig
    BatchGetAggregateResourceConfig (BatchGetAggregateResourceConfig'),
    newBatchGetAggregateResourceConfig,
    BatchGetAggregateResourceConfigResponse (BatchGetAggregateResourceConfigResponse'),
    newBatchGetAggregateResourceConfigResponse,

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

    -- ** PutConfigurationAggregator
    PutConfigurationAggregator (PutConfigurationAggregator'),
    newPutConfigurationAggregator,
    PutConfigurationAggregatorResponse (PutConfigurationAggregatorResponse'),
    newPutConfigurationAggregatorResponse,

    -- ** PutStoredQuery
    PutStoredQuery (PutStoredQuery'),
    newPutStoredQuery,
    PutStoredQueryResponse (PutStoredQueryResponse'),
    newPutStoredQueryResponse,

    -- ** GetComplianceSummaryByConfigRule
    GetComplianceSummaryByConfigRule (GetComplianceSummaryByConfigRule'),
    newGetComplianceSummaryByConfigRule,
    GetComplianceSummaryByConfigRuleResponse (GetComplianceSummaryByConfigRuleResponse'),
    newGetComplianceSummaryByConfigRuleResponse,

    -- ** DescribeOrganizationConformancePackStatuses (Paginated)
    DescribeOrganizationConformancePackStatuses (DescribeOrganizationConformancePackStatuses'),
    newDescribeOrganizationConformancePackStatuses,
    DescribeOrganizationConformancePackStatusesResponse (DescribeOrganizationConformancePackStatusesResponse'),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- ** GetOrganizationConfigRuleDetailedStatus (Paginated)
    GetOrganizationConfigRuleDetailedStatus (GetOrganizationConfigRuleDetailedStatus'),
    newGetOrganizationConfigRuleDetailedStatus,
    GetOrganizationConfigRuleDetailedStatusResponse (GetOrganizationConfigRuleDetailedStatusResponse'),
    newGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** DescribeAggregateComplianceByConfigRules (Paginated)
    DescribeAggregateComplianceByConfigRules (DescribeAggregateComplianceByConfigRules'),
    newDescribeAggregateComplianceByConfigRules,
    DescribeAggregateComplianceByConfigRulesResponse (DescribeAggregateComplianceByConfigRulesResponse'),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- ** DeleteConfigRule
    DeleteConfigRule (DeleteConfigRule'),
    newDeleteConfigRule,
    DeleteConfigRuleResponse (DeleteConfigRuleResponse'),
    newDeleteConfigRuleResponse,

    -- ** DescribeConformancePackStatus (Paginated)
    DescribeConformancePackStatus (DescribeConformancePackStatus'),
    newDescribeConformancePackStatus,
    DescribeConformancePackStatusResponse (DescribeConformancePackStatusResponse'),
    newDescribeConformancePackStatusResponse,

    -- ** DeleteConformancePack
    DeleteConformancePack (DeleteConformancePack'),
    newDeleteConformancePack,
    DeleteConformancePackResponse (DeleteConformancePackResponse'),
    newDeleteConformancePackResponse,

    -- ** StartRemediationExecution
    StartRemediationExecution (StartRemediationExecution'),
    newStartRemediationExecution,
    StartRemediationExecutionResponse (StartRemediationExecutionResponse'),
    newStartRemediationExecutionResponse,

    -- ** GetOrganizationConformancePackDetailedStatus (Paginated)
    GetOrganizationConformancePackDetailedStatus (GetOrganizationConformancePackDetailedStatus'),
    newGetOrganizationConformancePackDetailedStatus,
    GetOrganizationConformancePackDetailedStatusResponse (GetOrganizationConformancePackDetailedStatusResponse'),
    newGetOrganizationConformancePackDetailedStatusResponse,

    -- ** DeleteRetentionConfiguration
    DeleteRetentionConfiguration (DeleteRetentionConfiguration'),
    newDeleteRetentionConfiguration,
    DeleteRetentionConfigurationResponse (DeleteRetentionConfigurationResponse'),
    newDeleteRetentionConfigurationResponse,

    -- ** GetAggregateConformancePackComplianceSummary
    GetAggregateConformancePackComplianceSummary (GetAggregateConformancePackComplianceSummary'),
    newGetAggregateConformancePackComplianceSummary,
    GetAggregateConformancePackComplianceSummaryResponse (GetAggregateConformancePackComplianceSummaryResponse'),
    newGetAggregateConformancePackComplianceSummaryResponse,

    -- ** PutRemediationExceptions
    PutRemediationExceptions (PutRemediationExceptions'),
    newPutRemediationExceptions,
    PutRemediationExceptionsResponse (PutRemediationExceptionsResponse'),
    newPutRemediationExceptionsResponse,

    -- ** StopConfigurationRecorder
    StopConfigurationRecorder (StopConfigurationRecorder'),
    newStopConfigurationRecorder,
    StopConfigurationRecorderResponse (StopConfigurationRecorderResponse'),
    newStopConfigurationRecorderResponse,

    -- ** DescribeConfigRules (Paginated)
    DescribeConfigRules (DescribeConfigRules'),
    newDescribeConfigRules,
    DescribeConfigRulesResponse (DescribeConfigRulesResponse'),
    newDescribeConfigRulesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeRetentionConfigurations (Paginated)
    DescribeRetentionConfigurations (DescribeRetentionConfigurations'),
    newDescribeRetentionConfigurations,
    DescribeRetentionConfigurationsResponse (DescribeRetentionConfigurationsResponse'),
    newDescribeRetentionConfigurationsResponse,

    -- ** StartConfigurationRecorder
    StartConfigurationRecorder (StartConfigurationRecorder'),
    newStartConfigurationRecorder,
    StartConfigurationRecorderResponse (StartConfigurationRecorderResponse'),
    newStartConfigurationRecorderResponse,

    -- ** GetAggregateConfigRuleComplianceSummary
    GetAggregateConfigRuleComplianceSummary (GetAggregateConfigRuleComplianceSummary'),
    newGetAggregateConfigRuleComplianceSummary,
    GetAggregateConfigRuleComplianceSummaryResponse (GetAggregateConfigRuleComplianceSummaryResponse'),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- ** DescribeConformancePacks (Paginated)
    DescribeConformancePacks (DescribeConformancePacks'),
    newDescribeConformancePacks,
    DescribeConformancePacksResponse (DescribeConformancePacksResponse'),
    newDescribeConformancePacksResponse,

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

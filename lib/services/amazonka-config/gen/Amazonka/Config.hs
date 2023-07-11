{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Config
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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

    -- ** ConformancePackTemplateValidationException
    _ConformancePackTemplateValidationException,

    -- ** IdempotentParameterMismatch
    _IdempotentParameterMismatch,

    -- ** InsufficientDeliveryPolicyException
    _InsufficientDeliveryPolicyException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** InvalidConfigurationRecorderNameException
    _InvalidConfigurationRecorderNameException,

    -- ** InvalidDeliveryChannelNameException
    _InvalidDeliveryChannelNameException,

    -- ** InvalidExpressionException
    _InvalidExpressionException,

    -- ** InvalidLimitException
    _InvalidLimitException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidRecordingGroupException
    _InvalidRecordingGroupException,

    -- ** InvalidResultTokenException
    _InvalidResultTokenException,

    -- ** InvalidRoleException
    _InvalidRoleException,

    -- ** InvalidS3KeyPrefixException
    _InvalidS3KeyPrefixException,

    -- ** InvalidS3KmsKeyArnException
    _InvalidS3KmsKeyArnException,

    -- ** InvalidSNSTopicARNException
    _InvalidSNSTopicARNException,

    -- ** InvalidTimeRangeException
    _InvalidTimeRangeException,

    -- ** LastDeliveryChannelDeleteFailedException
    _LastDeliveryChannelDeleteFailedException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MaxActiveResourcesExceededException
    _MaxActiveResourcesExceededException,

    -- ** MaxNumberOfConfigRulesExceededException
    _MaxNumberOfConfigRulesExceededException,

    -- ** MaxNumberOfConfigurationRecordersExceededException
    _MaxNumberOfConfigurationRecordersExceededException,

    -- ** MaxNumberOfConformancePacksExceededException
    _MaxNumberOfConformancePacksExceededException,

    -- ** MaxNumberOfDeliveryChannelsExceededException
    _MaxNumberOfDeliveryChannelsExceededException,

    -- ** MaxNumberOfOrganizationConfigRulesExceededException
    _MaxNumberOfOrganizationConfigRulesExceededException,

    -- ** MaxNumberOfOrganizationConformancePacksExceededException
    _MaxNumberOfOrganizationConformancePacksExceededException,

    -- ** MaxNumberOfRetentionConfigurationsExceededException
    _MaxNumberOfRetentionConfigurationsExceededException,

    -- ** NoAvailableConfigurationRecorderException
    _NoAvailableConfigurationRecorderException,

    -- ** NoAvailableDeliveryChannelException
    _NoAvailableDeliveryChannelException,

    -- ** NoAvailableOrganizationException
    _NoAvailableOrganizationException,

    -- ** NoRunningConfigurationRecorderException
    _NoRunningConfigurationRecorderException,

    -- ** NoSuchBucketException
    _NoSuchBucketException,

    -- ** NoSuchConfigRuleException
    _NoSuchConfigRuleException,

    -- ** NoSuchConfigRuleInConformancePackException
    _NoSuchConfigRuleInConformancePackException,

    -- ** NoSuchConfigurationAggregatorException
    _NoSuchConfigurationAggregatorException,

    -- ** NoSuchConfigurationRecorderException
    _NoSuchConfigurationRecorderException,

    -- ** NoSuchConformancePackException
    _NoSuchConformancePackException,

    -- ** NoSuchDeliveryChannelException
    _NoSuchDeliveryChannelException,

    -- ** NoSuchOrganizationConfigRuleException
    _NoSuchOrganizationConfigRuleException,

    -- ** NoSuchOrganizationConformancePackException
    _NoSuchOrganizationConformancePackException,

    -- ** NoSuchRemediationConfigurationException
    _NoSuchRemediationConfigurationException,

    -- ** NoSuchRemediationExceptionException
    _NoSuchRemediationExceptionException,

    -- ** NoSuchRetentionConfigurationException
    _NoSuchRetentionConfigurationException,

    -- ** OrganizationAccessDeniedException
    _OrganizationAccessDeniedException,

    -- ** OrganizationAllFeaturesNotEnabledException
    _OrganizationAllFeaturesNotEnabledException,

    -- ** OrganizationConformancePackTemplateValidationException
    _OrganizationConformancePackTemplateValidationException,

    -- ** OversizedConfigurationItemException
    _OversizedConfigurationItemException,

    -- ** RemediationInProgressException
    _RemediationInProgressException,

    -- ** ResourceConcurrentModificationException
    _ResourceConcurrentModificationException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotDiscoveredException
    _ResourceNotDiscoveredException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetAggregateResourceConfig
    BatchGetAggregateResourceConfig (BatchGetAggregateResourceConfig'),
    newBatchGetAggregateResourceConfig,
    BatchGetAggregateResourceConfigResponse (BatchGetAggregateResourceConfigResponse'),
    newBatchGetAggregateResourceConfigResponse,

    -- ** BatchGetResourceConfig
    BatchGetResourceConfig (BatchGetResourceConfig'),
    newBatchGetResourceConfig,
    BatchGetResourceConfigResponse (BatchGetResourceConfigResponse'),
    newBatchGetResourceConfigResponse,

    -- ** DeleteAggregationAuthorization
    DeleteAggregationAuthorization (DeleteAggregationAuthorization'),
    newDeleteAggregationAuthorization,
    DeleteAggregationAuthorizationResponse (DeleteAggregationAuthorizationResponse'),
    newDeleteAggregationAuthorizationResponse,

    -- ** DeleteConfigRule
    DeleteConfigRule (DeleteConfigRule'),
    newDeleteConfigRule,
    DeleteConfigRuleResponse (DeleteConfigRuleResponse'),
    newDeleteConfigRuleResponse,

    -- ** DeleteConfigurationAggregator
    DeleteConfigurationAggregator (DeleteConfigurationAggregator'),
    newDeleteConfigurationAggregator,
    DeleteConfigurationAggregatorResponse (DeleteConfigurationAggregatorResponse'),
    newDeleteConfigurationAggregatorResponse,

    -- ** DeleteConfigurationRecorder
    DeleteConfigurationRecorder (DeleteConfigurationRecorder'),
    newDeleteConfigurationRecorder,
    DeleteConfigurationRecorderResponse (DeleteConfigurationRecorderResponse'),
    newDeleteConfigurationRecorderResponse,

    -- ** DeleteConformancePack
    DeleteConformancePack (DeleteConformancePack'),
    newDeleteConformancePack,
    DeleteConformancePackResponse (DeleteConformancePackResponse'),
    newDeleteConformancePackResponse,

    -- ** DeleteDeliveryChannel
    DeleteDeliveryChannel (DeleteDeliveryChannel'),
    newDeleteDeliveryChannel,
    DeleteDeliveryChannelResponse (DeleteDeliveryChannelResponse'),
    newDeleteDeliveryChannelResponse,

    -- ** DeleteEvaluationResults
    DeleteEvaluationResults (DeleteEvaluationResults'),
    newDeleteEvaluationResults,
    DeleteEvaluationResultsResponse (DeleteEvaluationResultsResponse'),
    newDeleteEvaluationResultsResponse,

    -- ** DeleteOrganizationConfigRule
    DeleteOrganizationConfigRule (DeleteOrganizationConfigRule'),
    newDeleteOrganizationConfigRule,
    DeleteOrganizationConfigRuleResponse (DeleteOrganizationConfigRuleResponse'),
    newDeleteOrganizationConfigRuleResponse,

    -- ** DeleteOrganizationConformancePack
    DeleteOrganizationConformancePack (DeleteOrganizationConformancePack'),
    newDeleteOrganizationConformancePack,
    DeleteOrganizationConformancePackResponse (DeleteOrganizationConformancePackResponse'),
    newDeleteOrganizationConformancePackResponse,

    -- ** DeletePendingAggregationRequest
    DeletePendingAggregationRequest (DeletePendingAggregationRequest'),
    newDeletePendingAggregationRequest,
    DeletePendingAggregationRequestResponse (DeletePendingAggregationRequestResponse'),
    newDeletePendingAggregationRequestResponse,

    -- ** DeleteRemediationConfiguration
    DeleteRemediationConfiguration (DeleteRemediationConfiguration'),
    newDeleteRemediationConfiguration,
    DeleteRemediationConfigurationResponse (DeleteRemediationConfigurationResponse'),
    newDeleteRemediationConfigurationResponse,

    -- ** DeleteRemediationExceptions
    DeleteRemediationExceptions (DeleteRemediationExceptions'),
    newDeleteRemediationExceptions,
    DeleteRemediationExceptionsResponse (DeleteRemediationExceptionsResponse'),
    newDeleteRemediationExceptionsResponse,

    -- ** DeleteResourceConfig
    DeleteResourceConfig (DeleteResourceConfig'),
    newDeleteResourceConfig,
    DeleteResourceConfigResponse (DeleteResourceConfigResponse'),
    newDeleteResourceConfigResponse,

    -- ** DeleteRetentionConfiguration
    DeleteRetentionConfiguration (DeleteRetentionConfiguration'),
    newDeleteRetentionConfiguration,
    DeleteRetentionConfigurationResponse (DeleteRetentionConfigurationResponse'),
    newDeleteRetentionConfigurationResponse,

    -- ** DeleteStoredQuery
    DeleteStoredQuery (DeleteStoredQuery'),
    newDeleteStoredQuery,
    DeleteStoredQueryResponse (DeleteStoredQueryResponse'),
    newDeleteStoredQueryResponse,

    -- ** DeliverConfigSnapshot
    DeliverConfigSnapshot (DeliverConfigSnapshot'),
    newDeliverConfigSnapshot,
    DeliverConfigSnapshotResponse (DeliverConfigSnapshotResponse'),
    newDeliverConfigSnapshotResponse,

    -- ** DescribeAggregateComplianceByConfigRules (Paginated)
    DescribeAggregateComplianceByConfigRules (DescribeAggregateComplianceByConfigRules'),
    newDescribeAggregateComplianceByConfigRules,
    DescribeAggregateComplianceByConfigRulesResponse (DescribeAggregateComplianceByConfigRulesResponse'),
    newDescribeAggregateComplianceByConfigRulesResponse,

    -- ** DescribeAggregateComplianceByConformancePacks (Paginated)
    DescribeAggregateComplianceByConformancePacks (DescribeAggregateComplianceByConformancePacks'),
    newDescribeAggregateComplianceByConformancePacks,
    DescribeAggregateComplianceByConformancePacksResponse (DescribeAggregateComplianceByConformancePacksResponse'),
    newDescribeAggregateComplianceByConformancePacksResponse,

    -- ** DescribeAggregationAuthorizations (Paginated)
    DescribeAggregationAuthorizations (DescribeAggregationAuthorizations'),
    newDescribeAggregationAuthorizations,
    DescribeAggregationAuthorizationsResponse (DescribeAggregationAuthorizationsResponse'),
    newDescribeAggregationAuthorizationsResponse,

    -- ** DescribeComplianceByConfigRule (Paginated)
    DescribeComplianceByConfigRule (DescribeComplianceByConfigRule'),
    newDescribeComplianceByConfigRule,
    DescribeComplianceByConfigRuleResponse (DescribeComplianceByConfigRuleResponse'),
    newDescribeComplianceByConfigRuleResponse,

    -- ** DescribeComplianceByResource (Paginated)
    DescribeComplianceByResource (DescribeComplianceByResource'),
    newDescribeComplianceByResource,
    DescribeComplianceByResourceResponse (DescribeComplianceByResourceResponse'),
    newDescribeComplianceByResourceResponse,

    -- ** DescribeConfigRuleEvaluationStatus (Paginated)
    DescribeConfigRuleEvaluationStatus (DescribeConfigRuleEvaluationStatus'),
    newDescribeConfigRuleEvaluationStatus,
    DescribeConfigRuleEvaluationStatusResponse (DescribeConfigRuleEvaluationStatusResponse'),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- ** DescribeConfigRules (Paginated)
    DescribeConfigRules (DescribeConfigRules'),
    newDescribeConfigRules,
    DescribeConfigRulesResponse (DescribeConfigRulesResponse'),
    newDescribeConfigRulesResponse,

    -- ** DescribeConfigurationAggregatorSourcesStatus (Paginated)
    DescribeConfigurationAggregatorSourcesStatus (DescribeConfigurationAggregatorSourcesStatus'),
    newDescribeConfigurationAggregatorSourcesStatus,
    DescribeConfigurationAggregatorSourcesStatusResponse (DescribeConfigurationAggregatorSourcesStatusResponse'),
    newDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** DescribeConfigurationAggregators (Paginated)
    DescribeConfigurationAggregators (DescribeConfigurationAggregators'),
    newDescribeConfigurationAggregators,
    DescribeConfigurationAggregatorsResponse (DescribeConfigurationAggregatorsResponse'),
    newDescribeConfigurationAggregatorsResponse,

    -- ** DescribeConfigurationRecorderStatus
    DescribeConfigurationRecorderStatus (DescribeConfigurationRecorderStatus'),
    newDescribeConfigurationRecorderStatus,
    DescribeConfigurationRecorderStatusResponse (DescribeConfigurationRecorderStatusResponse'),
    newDescribeConfigurationRecorderStatusResponse,

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

    -- ** DescribeConformancePackStatus (Paginated)
    DescribeConformancePackStatus (DescribeConformancePackStatus'),
    newDescribeConformancePackStatus,
    DescribeConformancePackStatusResponse (DescribeConformancePackStatusResponse'),
    newDescribeConformancePackStatusResponse,

    -- ** DescribeConformancePacks (Paginated)
    DescribeConformancePacks (DescribeConformancePacks'),
    newDescribeConformancePacks,
    DescribeConformancePacksResponse (DescribeConformancePacksResponse'),
    newDescribeConformancePacksResponse,

    -- ** DescribeDeliveryChannelStatus
    DescribeDeliveryChannelStatus (DescribeDeliveryChannelStatus'),
    newDescribeDeliveryChannelStatus,
    DescribeDeliveryChannelStatusResponse (DescribeDeliveryChannelStatusResponse'),
    newDescribeDeliveryChannelStatusResponse,

    -- ** DescribeDeliveryChannels
    DescribeDeliveryChannels (DescribeDeliveryChannels'),
    newDescribeDeliveryChannels,
    DescribeDeliveryChannelsResponse (DescribeDeliveryChannelsResponse'),
    newDescribeDeliveryChannelsResponse,

    -- ** DescribeOrganizationConfigRuleStatuses (Paginated)
    DescribeOrganizationConfigRuleStatuses (DescribeOrganizationConfigRuleStatuses'),
    newDescribeOrganizationConfigRuleStatuses,
    DescribeOrganizationConfigRuleStatusesResponse (DescribeOrganizationConfigRuleStatusesResponse'),
    newDescribeOrganizationConfigRuleStatusesResponse,

    -- ** DescribeOrganizationConfigRules (Paginated)
    DescribeOrganizationConfigRules (DescribeOrganizationConfigRules'),
    newDescribeOrganizationConfigRules,
    DescribeOrganizationConfigRulesResponse (DescribeOrganizationConfigRulesResponse'),
    newDescribeOrganizationConfigRulesResponse,

    -- ** DescribeOrganizationConformancePackStatuses (Paginated)
    DescribeOrganizationConformancePackStatuses (DescribeOrganizationConformancePackStatuses'),
    newDescribeOrganizationConformancePackStatuses,
    DescribeOrganizationConformancePackStatusesResponse (DescribeOrganizationConformancePackStatusesResponse'),
    newDescribeOrganizationConformancePackStatusesResponse,

    -- ** DescribeOrganizationConformancePacks (Paginated)
    DescribeOrganizationConformancePacks (DescribeOrganizationConformancePacks'),
    newDescribeOrganizationConformancePacks,
    DescribeOrganizationConformancePacksResponse (DescribeOrganizationConformancePacksResponse'),
    newDescribeOrganizationConformancePacksResponse,

    -- ** DescribePendingAggregationRequests (Paginated)
    DescribePendingAggregationRequests (DescribePendingAggregationRequests'),
    newDescribePendingAggregationRequests,
    DescribePendingAggregationRequestsResponse (DescribePendingAggregationRequestsResponse'),
    newDescribePendingAggregationRequestsResponse,

    -- ** DescribeRemediationConfigurations
    DescribeRemediationConfigurations (DescribeRemediationConfigurations'),
    newDescribeRemediationConfigurations,
    DescribeRemediationConfigurationsResponse (DescribeRemediationConfigurationsResponse'),
    newDescribeRemediationConfigurationsResponse,

    -- ** DescribeRemediationExceptions
    DescribeRemediationExceptions (DescribeRemediationExceptions'),
    newDescribeRemediationExceptions,
    DescribeRemediationExceptionsResponse (DescribeRemediationExceptionsResponse'),
    newDescribeRemediationExceptionsResponse,

    -- ** DescribeRemediationExecutionStatus (Paginated)
    DescribeRemediationExecutionStatus (DescribeRemediationExecutionStatus'),
    newDescribeRemediationExecutionStatus,
    DescribeRemediationExecutionStatusResponse (DescribeRemediationExecutionStatusResponse'),
    newDescribeRemediationExecutionStatusResponse,

    -- ** DescribeRetentionConfigurations (Paginated)
    DescribeRetentionConfigurations (DescribeRetentionConfigurations'),
    newDescribeRetentionConfigurations,
    DescribeRetentionConfigurationsResponse (DescribeRetentionConfigurationsResponse'),
    newDescribeRetentionConfigurationsResponse,

    -- ** GetAggregateComplianceDetailsByConfigRule (Paginated)
    GetAggregateComplianceDetailsByConfigRule (GetAggregateComplianceDetailsByConfigRule'),
    newGetAggregateComplianceDetailsByConfigRule,
    GetAggregateComplianceDetailsByConfigRuleResponse (GetAggregateComplianceDetailsByConfigRuleResponse'),
    newGetAggregateComplianceDetailsByConfigRuleResponse,

    -- ** GetAggregateConfigRuleComplianceSummary
    GetAggregateConfigRuleComplianceSummary (GetAggregateConfigRuleComplianceSummary'),
    newGetAggregateConfigRuleComplianceSummary,
    GetAggregateConfigRuleComplianceSummaryResponse (GetAggregateConfigRuleComplianceSummaryResponse'),
    newGetAggregateConfigRuleComplianceSummaryResponse,

    -- ** GetAggregateConformancePackComplianceSummary
    GetAggregateConformancePackComplianceSummary (GetAggregateConformancePackComplianceSummary'),
    newGetAggregateConformancePackComplianceSummary,
    GetAggregateConformancePackComplianceSummaryResponse (GetAggregateConformancePackComplianceSummaryResponse'),
    newGetAggregateConformancePackComplianceSummaryResponse,

    -- ** GetAggregateDiscoveredResourceCounts
    GetAggregateDiscoveredResourceCounts (GetAggregateDiscoveredResourceCounts'),
    newGetAggregateDiscoveredResourceCounts,
    GetAggregateDiscoveredResourceCountsResponse (GetAggregateDiscoveredResourceCountsResponse'),
    newGetAggregateDiscoveredResourceCountsResponse,

    -- ** GetAggregateResourceConfig
    GetAggregateResourceConfig (GetAggregateResourceConfig'),
    newGetAggregateResourceConfig,
    GetAggregateResourceConfigResponse (GetAggregateResourceConfigResponse'),
    newGetAggregateResourceConfigResponse,

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    GetComplianceDetailsByConfigRule (GetComplianceDetailsByConfigRule'),
    newGetComplianceDetailsByConfigRule,
    GetComplianceDetailsByConfigRuleResponse (GetComplianceDetailsByConfigRuleResponse'),
    newGetComplianceDetailsByConfigRuleResponse,

    -- ** GetComplianceDetailsByResource (Paginated)
    GetComplianceDetailsByResource (GetComplianceDetailsByResource'),
    newGetComplianceDetailsByResource,
    GetComplianceDetailsByResourceResponse (GetComplianceDetailsByResourceResponse'),
    newGetComplianceDetailsByResourceResponse,

    -- ** GetComplianceSummaryByConfigRule
    GetComplianceSummaryByConfigRule (GetComplianceSummaryByConfigRule'),
    newGetComplianceSummaryByConfigRule,
    GetComplianceSummaryByConfigRuleResponse (GetComplianceSummaryByConfigRuleResponse'),
    newGetComplianceSummaryByConfigRuleResponse,

    -- ** GetComplianceSummaryByResourceType
    GetComplianceSummaryByResourceType (GetComplianceSummaryByResourceType'),
    newGetComplianceSummaryByResourceType,
    GetComplianceSummaryByResourceTypeResponse (GetComplianceSummaryByResourceTypeResponse'),
    newGetComplianceSummaryByResourceTypeResponse,

    -- ** GetConformancePackComplianceDetails
    GetConformancePackComplianceDetails (GetConformancePackComplianceDetails'),
    newGetConformancePackComplianceDetails,
    GetConformancePackComplianceDetailsResponse (GetConformancePackComplianceDetailsResponse'),
    newGetConformancePackComplianceDetailsResponse,

    -- ** GetConformancePackComplianceSummary (Paginated)
    GetConformancePackComplianceSummary (GetConformancePackComplianceSummary'),
    newGetConformancePackComplianceSummary,
    GetConformancePackComplianceSummaryResponse (GetConformancePackComplianceSummaryResponse'),
    newGetConformancePackComplianceSummaryResponse,

    -- ** GetCustomRulePolicy
    GetCustomRulePolicy (GetCustomRulePolicy'),
    newGetCustomRulePolicy,
    GetCustomRulePolicyResponse (GetCustomRulePolicyResponse'),
    newGetCustomRulePolicyResponse,

    -- ** GetDiscoveredResourceCounts
    GetDiscoveredResourceCounts (GetDiscoveredResourceCounts'),
    newGetDiscoveredResourceCounts,
    GetDiscoveredResourceCountsResponse (GetDiscoveredResourceCountsResponse'),
    newGetDiscoveredResourceCountsResponse,

    -- ** GetOrganizationConfigRuleDetailedStatus (Paginated)
    GetOrganizationConfigRuleDetailedStatus (GetOrganizationConfigRuleDetailedStatus'),
    newGetOrganizationConfigRuleDetailedStatus,
    GetOrganizationConfigRuleDetailedStatusResponse (GetOrganizationConfigRuleDetailedStatusResponse'),
    newGetOrganizationConfigRuleDetailedStatusResponse,

    -- ** GetOrganizationConformancePackDetailedStatus (Paginated)
    GetOrganizationConformancePackDetailedStatus (GetOrganizationConformancePackDetailedStatus'),
    newGetOrganizationConformancePackDetailedStatus,
    GetOrganizationConformancePackDetailedStatusResponse (GetOrganizationConformancePackDetailedStatusResponse'),
    newGetOrganizationConformancePackDetailedStatusResponse,

    -- ** GetOrganizationCustomRulePolicy
    GetOrganizationCustomRulePolicy (GetOrganizationCustomRulePolicy'),
    newGetOrganizationCustomRulePolicy,
    GetOrganizationCustomRulePolicyResponse (GetOrganizationCustomRulePolicyResponse'),
    newGetOrganizationCustomRulePolicyResponse,

    -- ** GetResourceConfigHistory (Paginated)
    GetResourceConfigHistory (GetResourceConfigHistory'),
    newGetResourceConfigHistory,
    GetResourceConfigHistoryResponse (GetResourceConfigHistoryResponse'),
    newGetResourceConfigHistoryResponse,

    -- ** GetResourceEvaluationSummary
    GetResourceEvaluationSummary (GetResourceEvaluationSummary'),
    newGetResourceEvaluationSummary,
    GetResourceEvaluationSummaryResponse (GetResourceEvaluationSummaryResponse'),
    newGetResourceEvaluationSummaryResponse,

    -- ** GetStoredQuery
    GetStoredQuery (GetStoredQuery'),
    newGetStoredQuery,
    GetStoredQueryResponse (GetStoredQueryResponse'),
    newGetStoredQueryResponse,

    -- ** ListAggregateDiscoveredResources (Paginated)
    ListAggregateDiscoveredResources (ListAggregateDiscoveredResources'),
    newListAggregateDiscoveredResources,
    ListAggregateDiscoveredResourcesResponse (ListAggregateDiscoveredResourcesResponse'),
    newListAggregateDiscoveredResourcesResponse,

    -- ** ListConformancePackComplianceScores
    ListConformancePackComplianceScores (ListConformancePackComplianceScores'),
    newListConformancePackComplianceScores,
    ListConformancePackComplianceScoresResponse (ListConformancePackComplianceScoresResponse'),
    newListConformancePackComplianceScoresResponse,

    -- ** ListDiscoveredResources (Paginated)
    ListDiscoveredResources (ListDiscoveredResources'),
    newListDiscoveredResources,
    ListDiscoveredResourcesResponse (ListDiscoveredResourcesResponse'),
    newListDiscoveredResourcesResponse,

    -- ** ListResourceEvaluations (Paginated)
    ListResourceEvaluations (ListResourceEvaluations'),
    newListResourceEvaluations,
    ListResourceEvaluationsResponse (ListResourceEvaluationsResponse'),
    newListResourceEvaluationsResponse,

    -- ** ListStoredQueries
    ListStoredQueries (ListStoredQueries'),
    newListStoredQueries,
    ListStoredQueriesResponse (ListStoredQueriesResponse'),
    newListStoredQueriesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutAggregationAuthorization
    PutAggregationAuthorization (PutAggregationAuthorization'),
    newPutAggregationAuthorization,
    PutAggregationAuthorizationResponse (PutAggregationAuthorizationResponse'),
    newPutAggregationAuthorizationResponse,

    -- ** PutConfigRule
    PutConfigRule (PutConfigRule'),
    newPutConfigRule,
    PutConfigRuleResponse (PutConfigRuleResponse'),
    newPutConfigRuleResponse,

    -- ** PutConfigurationAggregator
    PutConfigurationAggregator (PutConfigurationAggregator'),
    newPutConfigurationAggregator,
    PutConfigurationAggregatorResponse (PutConfigurationAggregatorResponse'),
    newPutConfigurationAggregatorResponse,

    -- ** PutConfigurationRecorder
    PutConfigurationRecorder (PutConfigurationRecorder'),
    newPutConfigurationRecorder,
    PutConfigurationRecorderResponse (PutConfigurationRecorderResponse'),
    newPutConfigurationRecorderResponse,

    -- ** PutConformancePack
    PutConformancePack (PutConformancePack'),
    newPutConformancePack,
    PutConformancePackResponse (PutConformancePackResponse'),
    newPutConformancePackResponse,

    -- ** PutDeliveryChannel
    PutDeliveryChannel (PutDeliveryChannel'),
    newPutDeliveryChannel,
    PutDeliveryChannelResponse (PutDeliveryChannelResponse'),
    newPutDeliveryChannelResponse,

    -- ** PutEvaluations
    PutEvaluations (PutEvaluations'),
    newPutEvaluations,
    PutEvaluationsResponse (PutEvaluationsResponse'),
    newPutEvaluationsResponse,

    -- ** PutExternalEvaluation
    PutExternalEvaluation (PutExternalEvaluation'),
    newPutExternalEvaluation,
    PutExternalEvaluationResponse (PutExternalEvaluationResponse'),
    newPutExternalEvaluationResponse,

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

    -- ** PutRemediationConfigurations
    PutRemediationConfigurations (PutRemediationConfigurations'),
    newPutRemediationConfigurations,
    PutRemediationConfigurationsResponse (PutRemediationConfigurationsResponse'),
    newPutRemediationConfigurationsResponse,

    -- ** PutRemediationExceptions
    PutRemediationExceptions (PutRemediationExceptions'),
    newPutRemediationExceptions,
    PutRemediationExceptionsResponse (PutRemediationExceptionsResponse'),
    newPutRemediationExceptionsResponse,

    -- ** PutResourceConfig
    PutResourceConfig (PutResourceConfig'),
    newPutResourceConfig,
    PutResourceConfigResponse (PutResourceConfigResponse'),
    newPutResourceConfigResponse,

    -- ** PutRetentionConfiguration
    PutRetentionConfiguration (PutRetentionConfiguration'),
    newPutRetentionConfiguration,
    PutRetentionConfigurationResponse (PutRetentionConfigurationResponse'),
    newPutRetentionConfigurationResponse,

    -- ** PutStoredQuery
    PutStoredQuery (PutStoredQuery'),
    newPutStoredQuery,
    PutStoredQueryResponse (PutStoredQueryResponse'),
    newPutStoredQueryResponse,

    -- ** SelectAggregateResourceConfig (Paginated)
    SelectAggregateResourceConfig (SelectAggregateResourceConfig'),
    newSelectAggregateResourceConfig,
    SelectAggregateResourceConfigResponse (SelectAggregateResourceConfigResponse'),
    newSelectAggregateResourceConfigResponse,

    -- ** SelectResourceConfig (Paginated)
    SelectResourceConfig (SelectResourceConfig'),
    newSelectResourceConfig,
    SelectResourceConfigResponse (SelectResourceConfigResponse'),
    newSelectResourceConfigResponse,

    -- ** StartConfigRulesEvaluation
    StartConfigRulesEvaluation (StartConfigRulesEvaluation'),
    newStartConfigRulesEvaluation,
    StartConfigRulesEvaluationResponse (StartConfigRulesEvaluationResponse'),
    newStartConfigRulesEvaluationResponse,

    -- ** StartConfigurationRecorder
    StartConfigurationRecorder (StartConfigurationRecorder'),
    newStartConfigurationRecorder,
    StartConfigurationRecorderResponse (StartConfigurationRecorderResponse'),
    newStartConfigurationRecorderResponse,

    -- ** StartRemediationExecution
    StartRemediationExecution (StartRemediationExecution'),
    newStartRemediationExecution,
    StartRemediationExecutionResponse (StartRemediationExecutionResponse'),
    newStartRemediationExecutionResponse,

    -- ** StartResourceEvaluation
    StartResourceEvaluation (StartResourceEvaluation'),
    newStartResourceEvaluation,
    StartResourceEvaluationResponse (StartResourceEvaluationResponse'),
    newStartResourceEvaluationResponse,

    -- ** StopConfigurationRecorder
    StopConfigurationRecorder (StopConfigurationRecorder'),
    newStopConfigurationRecorder,
    StopConfigurationRecorderResponse (StopConfigurationRecorderResponse'),
    newStopConfigurationRecorderResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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

    -- ** EvaluationMode
    EvaluationMode (..),

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

    -- ** OrganizationConfigRuleTriggerTypeNoSN
    OrganizationConfigRuleTriggerTypeNoSN (..),

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

    -- ** ResourceConfigurationSchemaType
    ResourceConfigurationSchemaType (..),

    -- ** ResourceCountGroupKey
    ResourceCountGroupKey (..),

    -- ** ResourceEvaluationStatus
    ResourceEvaluationStatus (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ResourceValueType
    ResourceValueType (..),

    -- ** SortBy
    SortBy (..),

    -- ** SortOrder
    SortOrder (..),

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

    -- ** ConformancePackComplianceScore
    ConformancePackComplianceScore (ConformancePackComplianceScore'),
    newConformancePackComplianceScore,

    -- ** ConformancePackComplianceScoresFilters
    ConformancePackComplianceScoresFilters (ConformancePackComplianceScoresFilters'),
    newConformancePackComplianceScoresFilters,

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

    -- ** CustomPolicyDetails
    CustomPolicyDetails (CustomPolicyDetails'),
    newCustomPolicyDetails,

    -- ** DeliveryChannel
    DeliveryChannel (DeliveryChannel'),
    newDeliveryChannel,

    -- ** DeliveryChannelStatus
    DeliveryChannelStatus (DeliveryChannelStatus'),
    newDeliveryChannelStatus,

    -- ** DescribeConfigRulesFilters
    DescribeConfigRulesFilters (DescribeConfigRulesFilters'),
    newDescribeConfigRulesFilters,

    -- ** Evaluation
    Evaluation (Evaluation'),
    newEvaluation,

    -- ** EvaluationContext
    EvaluationContext (EvaluationContext'),
    newEvaluationContext,

    -- ** EvaluationModeConfiguration
    EvaluationModeConfiguration (EvaluationModeConfiguration'),
    newEvaluationModeConfiguration,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** EvaluationResultIdentifier
    EvaluationResultIdentifier (EvaluationResultIdentifier'),
    newEvaluationResultIdentifier,

    -- ** EvaluationResultQualifier
    EvaluationResultQualifier (EvaluationResultQualifier'),
    newEvaluationResultQualifier,

    -- ** EvaluationStatus
    EvaluationStatus (EvaluationStatus'),
    newEvaluationStatus,

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

    -- ** OrganizationCustomPolicyRuleMetadata
    OrganizationCustomPolicyRuleMetadata (OrganizationCustomPolicyRuleMetadata'),
    newOrganizationCustomPolicyRuleMetadata,

    -- ** OrganizationCustomPolicyRuleMetadataNoPolicy
    OrganizationCustomPolicyRuleMetadataNoPolicy (OrganizationCustomPolicyRuleMetadataNoPolicy'),
    newOrganizationCustomPolicyRuleMetadataNoPolicy,

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

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** ResourceEvaluation
    ResourceEvaluation (ResourceEvaluation'),
    newResourceEvaluation,

    -- ** ResourceEvaluationFilters
    ResourceEvaluationFilters (ResourceEvaluationFilters'),
    newResourceEvaluationFilters,

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

    -- ** TemplateSSMDocumentDetails
    TemplateSSMDocumentDetails (TemplateSSMDocumentDetails'),
    newTemplateSSMDocumentDetails,

    -- ** TimeWindow
    TimeWindow (TimeWindow'),
    newTimeWindow,
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
import Amazonka.Config.Lens
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

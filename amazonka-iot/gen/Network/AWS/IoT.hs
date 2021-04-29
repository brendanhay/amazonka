{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT
--
-- AWS IoT provides secure, bi-directional communication between
-- Internet-connected devices (such as sensors, actuators, embedded
-- devices, or smart appliances) and the AWS cloud. You can discover your
-- custom IoT-Data endpoint to communicate with, configure rules for data
-- processing and integration with other services, organize resources
-- associated with each device (Registry), configure logging, and create
-- and manage policies and credentials to authenticate devices.
--
-- The service endpoints that expose this API are listed in
-- <https://docs.aws.amazon.com/general/latest/gr/iot-core.html AWS IoT Core Endpoints and Quotas>.
-- You must use the endpoint for the region that has the resources you want
-- to access.
--
-- The service name used by
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html AWS Signature Version 4>
-- to sign the request is: /execute-api/.
--
-- For more information about how AWS IoT works, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide>.
--
-- For information about how to use the credentials provider for AWS IoT,
-- see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/authorizing-direct-aws.html Authorizing Direct Calls to AWS Services>.
module Network.AWS.IoT
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IndexNotReadyException
    _IndexNotReadyException,

    -- ** TransferAlreadyCompletedException
    _TransferAlreadyCompletedException,

    -- ** InvalidQueryException
    _InvalidQueryException,

    -- ** CertificateConflictException
    _CertificateConflictException,

    -- ** TaskAlreadyExistsException
    _TaskAlreadyExistsException,

    -- ** CertificateValidationException
    _CertificateValidationException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InternalException
    _InternalException,

    -- ** MalformedPolicyException
    _MalformedPolicyException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** CertificateStateException
    _CertificateStateException,

    -- ** InvalidAggregationException
    _InvalidAggregationException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceRegistrationFailureException
    _ResourceRegistrationFailureException,

    -- ** SqlParseException
    _SqlParseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** NotConfiguredException
    _NotConfiguredException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** VersionConflictException
    _VersionConflictException,

    -- ** RegistrationCodeValidationException
    _RegistrationCodeValidationException,

    -- ** VersionsLimitExceededException
    _VersionsLimitExceededException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** InvalidResponseException
    _InvalidResponseException,

    -- ** TransferConflictException
    _TransferConflictException,

    -- ** ConflictingResourceUpdateException
    _ConflictingResourceUpdateException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListThingRegistrationTaskReports (Paginated)
    ListThingRegistrationTaskReports (ListThingRegistrationTaskReports'),
    newListThingRegistrationTaskReports,
    ListThingRegistrationTaskReportsResponse (ListThingRegistrationTaskReportsResponse'),
    newListThingRegistrationTaskReportsResponse,

    -- ** CreateProvisioningClaim
    CreateProvisioningClaim (CreateProvisioningClaim'),
    newCreateProvisioningClaim,
    CreateProvisioningClaimResponse (CreateProvisioningClaimResponse'),
    newCreateProvisioningClaimResponse,

    -- ** UpdateIndexingConfiguration
    UpdateIndexingConfiguration (UpdateIndexingConfiguration'),
    newUpdateIndexingConfiguration,
    UpdateIndexingConfigurationResponse (UpdateIndexingConfigurationResponse'),
    newUpdateIndexingConfigurationResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** DeleteJobExecution
    DeleteJobExecution (DeleteJobExecution'),
    newDeleteJobExecution,
    DeleteJobExecutionResponse (DeleteJobExecutionResponse'),
    newDeleteJobExecutionResponse,

    -- ** ListMitigationActions (Paginated)
    ListMitigationActions (ListMitigationActions'),
    newListMitigationActions,
    ListMitigationActionsResponse (ListMitigationActionsResponse'),
    newListMitigationActionsResponse,

    -- ** StartDetectMitigationActionsTask
    StartDetectMitigationActionsTask (StartDetectMitigationActionsTask'),
    newStartDetectMitigationActionsTask,
    StartDetectMitigationActionsTaskResponse (StartDetectMitigationActionsTaskResponse'),
    newStartDetectMitigationActionsTaskResponse,

    -- ** GetCardinality
    GetCardinality (GetCardinality'),
    newGetCardinality,
    GetCardinalityResponse (GetCardinalityResponse'),
    newGetCardinalityResponse,

    -- ** ListViolationEvents (Paginated)
    ListViolationEvents (ListViolationEvents'),
    newListViolationEvents,
    ListViolationEventsResponse (ListViolationEventsResponse'),
    newListViolationEventsResponse,

    -- ** UpdateCertificate
    UpdateCertificate (UpdateCertificate'),
    newUpdateCertificate,
    UpdateCertificateResponse (UpdateCertificateResponse'),
    newUpdateCertificateResponse,

    -- ** DeleteMitigationAction
    DeleteMitigationAction (DeleteMitigationAction'),
    newDeleteMitigationAction,
    DeleteMitigationActionResponse (DeleteMitigationActionResponse'),
    newDeleteMitigationActionResponse,

    -- ** UpdateMitigationAction
    UpdateMitigationAction (UpdateMitigationAction'),
    newUpdateMitigationAction,
    UpdateMitigationActionResponse (UpdateMitigationActionResponse'),
    newUpdateMitigationActionResponse,

    -- ** DescribeProvisioningTemplate
    DescribeProvisioningTemplate (DescribeProvisioningTemplate'),
    newDescribeProvisioningTemplate,
    DescribeProvisioningTemplateResponse (DescribeProvisioningTemplateResponse'),
    newDescribeProvisioningTemplateResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** CreateDimension
    CreateDimension (CreateDimension'),
    newCreateDimension,
    CreateDimensionResponse (CreateDimensionResponse'),
    newCreateDimensionResponse,

    -- ** UpdateDomainConfiguration
    UpdateDomainConfiguration (UpdateDomainConfiguration'),
    newUpdateDomainConfiguration,
    UpdateDomainConfigurationResponse (UpdateDomainConfigurationResponse'),
    newUpdateDomainConfigurationResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** ListAuditTasks (Paginated)
    ListAuditTasks (ListAuditTasks'),
    newListAuditTasks,
    ListAuditTasksResponse (ListAuditTasksResponse'),
    newListAuditTasksResponse,

    -- ** RejectCertificateTransfer
    RejectCertificateTransfer (RejectCertificateTransfer'),
    newRejectCertificateTransfer,
    RejectCertificateTransferResponse (RejectCertificateTransferResponse'),
    newRejectCertificateTransferResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** SetLoggingOptions
    SetLoggingOptions (SetLoggingOptions'),
    newSetLoggingOptions,
    SetLoggingOptionsResponse (SetLoggingOptionsResponse'),
    newSetLoggingOptionsResponse,

    -- ** CreateMitigationAction
    CreateMitigationAction (CreateMitigationAction'),
    newCreateMitigationAction,
    CreateMitigationActionResponse (CreateMitigationActionResponse'),
    newCreateMitigationActionResponse,

    -- ** GetTopicRule
    GetTopicRule (GetTopicRule'),
    newGetTopicRule,
    GetTopicRuleResponse (GetTopicRuleResponse'),
    newGetTopicRuleResponse,

    -- ** DescribeThingType
    DescribeThingType (DescribeThingType'),
    newDescribeThingType,
    DescribeThingTypeResponse (DescribeThingTypeResponse'),
    newDescribeThingTypeResponse,

    -- ** ListThingsInThingGroup (Paginated)
    ListThingsInThingGroup (ListThingsInThingGroup'),
    newListThingsInThingGroup,
    ListThingsInThingGroupResponse (ListThingsInThingGroupResponse'),
    newListThingsInThingGroupResponse,

    -- ** DescribeScheduledAudit
    DescribeScheduledAudit (DescribeScheduledAudit'),
    newDescribeScheduledAudit,
    DescribeScheduledAuditResponse (DescribeScheduledAuditResponse'),
    newDescribeScheduledAuditResponse,

    -- ** ListDomainConfigurations (Paginated)
    ListDomainConfigurations (ListDomainConfigurations'),
    newListDomainConfigurations,
    ListDomainConfigurationsResponse (ListDomainConfigurationsResponse'),
    newListDomainConfigurationsResponse,

    -- ** DeleteDomainConfiguration
    DeleteDomainConfiguration (DeleteDomainConfiguration'),
    newDeleteDomainConfiguration,
    DeleteDomainConfigurationResponse (DeleteDomainConfigurationResponse'),
    newDeleteDomainConfigurationResponse,

    -- ** GetV2LoggingOptions
    GetV2LoggingOptions (GetV2LoggingOptions'),
    newGetV2LoggingOptions,
    GetV2LoggingOptionsResponse (GetV2LoggingOptionsResponse'),
    newGetV2LoggingOptionsResponse,

    -- ** CreateSecurityProfile
    CreateSecurityProfile (CreateSecurityProfile'),
    newCreateSecurityProfile,
    CreateSecurityProfileResponse (CreateSecurityProfileResponse'),
    newCreateSecurityProfileResponse,

    -- ** DeleteTopicRule
    DeleteTopicRule (DeleteTopicRule'),
    newDeleteTopicRule,
    DeleteTopicRuleResponse (DeleteTopicRuleResponse'),
    newDeleteTopicRuleResponse,

    -- ** DeleteCACertificate
    DeleteCACertificate (DeleteCACertificate'),
    newDeleteCACertificate,
    DeleteCACertificateResponse (DeleteCACertificateResponse'),
    newDeleteCACertificateResponse,

    -- ** DeleteCustomMetric
    DeleteCustomMetric (DeleteCustomMetric'),
    newDeleteCustomMetric,
    DeleteCustomMetricResponse (DeleteCustomMetricResponse'),
    newDeleteCustomMetricResponse,

    -- ** UpdateCustomMetric
    UpdateCustomMetric (UpdateCustomMetric'),
    newUpdateCustomMetric,
    UpdateCustomMetricResponse (UpdateCustomMetricResponse'),
    newUpdateCustomMetricResponse,

    -- ** CancelAuditTask
    CancelAuditTask (CancelAuditTask'),
    newCancelAuditTask,
    CancelAuditTaskResponse (CancelAuditTaskResponse'),
    newCancelAuditTaskResponse,

    -- ** ListRoleAliases (Paginated)
    ListRoleAliases (ListRoleAliases'),
    newListRoleAliases,
    ListRoleAliasesResponse (ListRoleAliasesResponse'),
    newListRoleAliasesResponse,

    -- ** StartAuditMitigationActionsTask
    StartAuditMitigationActionsTask (StartAuditMitigationActionsTask'),
    newStartAuditMitigationActionsTask,
    StartAuditMitigationActionsTaskResponse (StartAuditMitigationActionsTaskResponse'),
    newStartAuditMitigationActionsTaskResponse,

    -- ** AttachSecurityProfile
    AttachSecurityProfile (AttachSecurityProfile'),
    newAttachSecurityProfile,
    AttachSecurityProfileResponse (AttachSecurityProfileResponse'),
    newAttachSecurityProfileResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** TransferCertificate
    TransferCertificate (TransferCertificate'),
    newTransferCertificate,
    TransferCertificateResponse (TransferCertificateResponse'),
    newTransferCertificateResponse,

    -- ** CreateKeysAndCertificate
    CreateKeysAndCertificate (CreateKeysAndCertificate'),
    newCreateKeysAndCertificate,
    CreateKeysAndCertificateResponse (CreateKeysAndCertificateResponse'),
    newCreateKeysAndCertificateResponse,

    -- ** UpdateCACertificate
    UpdateCACertificate (UpdateCACertificate'),
    newUpdateCACertificate,
    UpdateCACertificateResponse (UpdateCACertificateResponse'),
    newUpdateCACertificateResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** CreateRoleAlias
    CreateRoleAlias (CreateRoleAlias'),
    newCreateRoleAlias,
    CreateRoleAliasResponse (CreateRoleAliasResponse'),
    newCreateRoleAliasResponse,

    -- ** ListThingsInBillingGroup (Paginated)
    ListThingsInBillingGroup (ListThingsInBillingGroup'),
    newListThingsInBillingGroup,
    ListThingsInBillingGroupResponse (ListThingsInBillingGroupResponse'),
    newListThingsInBillingGroupResponse,

    -- ** ListTargetsForSecurityProfile (Paginated)
    ListTargetsForSecurityProfile (ListTargetsForSecurityProfile'),
    newListTargetsForSecurityProfile,
    ListTargetsForSecurityProfileResponse (ListTargetsForSecurityProfileResponse'),
    newListTargetsForSecurityProfileResponse,

    -- ** ListCustomMetrics (Paginated)
    ListCustomMetrics (ListCustomMetrics'),
    newListCustomMetrics,
    ListCustomMetricsResponse (ListCustomMetricsResponse'),
    newListCustomMetricsResponse,

    -- ** DescribeProvisioningTemplateVersion
    DescribeProvisioningTemplateVersion (DescribeProvisioningTemplateVersion'),
    newDescribeProvisioningTemplateVersion,
    DescribeProvisioningTemplateVersionResponse (DescribeProvisioningTemplateVersionResponse'),
    newDescribeProvisioningTemplateVersionResponse,

    -- ** GetPercentiles
    GetPercentiles (GetPercentiles'),
    newGetPercentiles,
    GetPercentilesResponse (GetPercentilesResponse'),
    newGetPercentilesResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** CreateCustomMetric
    CreateCustomMetric (CreateCustomMetric'),
    newCreateCustomMetric,
    CreateCustomMetricResponse (CreateCustomMetricResponse'),
    newCreateCustomMetricResponse,

    -- ** DisableTopicRule
    DisableTopicRule (DisableTopicRule'),
    newDisableTopicRule,
    DisableTopicRuleResponse (DisableTopicRuleResponse'),
    newDisableTopicRuleResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeAuditMitigationActionsTask
    DescribeAuditMitigationActionsTask (DescribeAuditMitigationActionsTask'),
    newDescribeAuditMitigationActionsTask,
    DescribeAuditMitigationActionsTaskResponse (DescribeAuditMitigationActionsTaskResponse'),
    newDescribeAuditMitigationActionsTaskResponse,

    -- ** SetV2LoggingLevel
    SetV2LoggingLevel (SetV2LoggingLevel'),
    newSetV2LoggingLevel,
    SetV2LoggingLevelResponse (SetV2LoggingLevelResponse'),
    newSetV2LoggingLevelResponse,

    -- ** ListJobExecutionsForThing (Paginated)
    ListJobExecutionsForThing (ListJobExecutionsForThing'),
    newListJobExecutionsForThing,
    ListJobExecutionsForThingResponse (ListJobExecutionsForThingResponse'),
    newListJobExecutionsForThingResponse,

    -- ** CreateThing
    CreateThing (CreateThing'),
    newCreateThing,
    CreateThingResponse (CreateThingResponse'),
    newCreateThingResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** UpdateProvisioningTemplate
    UpdateProvisioningTemplate (UpdateProvisioningTemplate'),
    newUpdateProvisioningTemplate,
    UpdateProvisioningTemplateResponse (UpdateProvisioningTemplateResponse'),
    newUpdateProvisioningTemplateResponse,

    -- ** StartThingRegistrationTask
    StartThingRegistrationTask (StartThingRegistrationTask'),
    newStartThingRegistrationTask,
    StartThingRegistrationTaskResponse (StartThingRegistrationTaskResponse'),
    newStartThingRegistrationTaskResponse,

    -- ** SetDefaultAuthorizer
    SetDefaultAuthorizer (SetDefaultAuthorizer'),
    newSetDefaultAuthorizer,
    SetDefaultAuthorizerResponse (SetDefaultAuthorizerResponse'),
    newSetDefaultAuthorizerResponse,

    -- ** DeleteProvisioningTemplate
    DeleteProvisioningTemplate (DeleteProvisioningTemplate'),
    newDeleteProvisioningTemplate,
    DeleteProvisioningTemplateResponse (DeleteProvisioningTemplateResponse'),
    newDeleteProvisioningTemplateResponse,

    -- ** DescribeMitigationAction
    DescribeMitigationAction (DescribeMitigationAction'),
    newDescribeMitigationAction,
    DescribeMitigationActionResponse (DescribeMitigationActionResponse'),
    newDescribeMitigationActionResponse,

    -- ** DeleteV2LoggingLevel
    DeleteV2LoggingLevel (DeleteV2LoggingLevel'),
    newDeleteV2LoggingLevel,
    DeleteV2LoggingLevelResponse (DeleteV2LoggingLevelResponse'),
    newDeleteV2LoggingLevelResponse,

    -- ** DescribeJobExecution
    DescribeJobExecution (DescribeJobExecution'),
    newDescribeJobExecution,
    DescribeJobExecutionResponse (DescribeJobExecutionResponse'),
    newDescribeJobExecutionResponse,

    -- ** StopThingRegistrationTask
    StopThingRegistrationTask (StopThingRegistrationTask'),
    newStopThingRegistrationTask,
    StopThingRegistrationTaskResponse (StopThingRegistrationTaskResponse'),
    newStopThingRegistrationTaskResponse,

    -- ** CreateScheduledAudit
    CreateScheduledAudit (CreateScheduledAudit'),
    newCreateScheduledAudit,
    CreateScheduledAuditResponse (CreateScheduledAuditResponse'),
    newCreateScheduledAuditResponse,

    -- ** GetIndexingConfiguration
    GetIndexingConfiguration (GetIndexingConfiguration'),
    newGetIndexingConfiguration,
    GetIndexingConfigurationResponse (GetIndexingConfigurationResponse'),
    newGetIndexingConfigurationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListV2LoggingLevels (Paginated)
    ListV2LoggingLevels (ListV2LoggingLevels'),
    newListV2LoggingLevels,
    ListV2LoggingLevelsResponse (ListV2LoggingLevelsResponse'),
    newListV2LoggingLevelsResponse,

    -- ** ListProvisioningTemplates (Paginated)
    ListProvisioningTemplates (ListProvisioningTemplates'),
    newListProvisioningTemplates,
    ListProvisioningTemplatesResponse (ListProvisioningTemplatesResponse'),
    newListProvisioningTemplatesResponse,

    -- ** ListAuditMitigationActionsExecutions (Paginated)
    ListAuditMitigationActionsExecutions (ListAuditMitigationActionsExecutions'),
    newListAuditMitigationActionsExecutions,
    ListAuditMitigationActionsExecutionsResponse (ListAuditMitigationActionsExecutionsResponse'),
    newListAuditMitigationActionsExecutionsResponse,

    -- ** DeleteAuditSuppression
    DeleteAuditSuppression (DeleteAuditSuppression'),
    newDeleteAuditSuppression,
    DeleteAuditSuppressionResponse (DeleteAuditSuppressionResponse'),
    newDeleteAuditSuppressionResponse,

    -- ** ListDetectMitigationActionsTasks (Paginated)
    ListDetectMitigationActionsTasks (ListDetectMitigationActionsTasks'),
    newListDetectMitigationActionsTasks,
    ListDetectMitigationActionsTasksResponse (ListDetectMitigationActionsTasksResponse'),
    newListDetectMitigationActionsTasksResponse,

    -- ** UpdateStream
    UpdateStream (UpdateStream'),
    newUpdateStream,
    UpdateStreamResponse (UpdateStreamResponse'),
    newUpdateStreamResponse,

    -- ** DeleteRegistrationCode
    DeleteRegistrationCode (DeleteRegistrationCode'),
    newDeleteRegistrationCode,
    DeleteRegistrationCodeResponse (DeleteRegistrationCodeResponse'),
    newDeleteRegistrationCodeResponse,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    CreateAuthorizerResponse (CreateAuthorizerResponse'),
    newCreateAuthorizerResponse,

    -- ** DescribeDimension
    DescribeDimension (DescribeDimension'),
    newDescribeDimension,
    DescribeDimensionResponse (DescribeDimensionResponse'),
    newDescribeDimensionResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** DeleteAccountAuditConfiguration
    DeleteAccountAuditConfiguration (DeleteAccountAuditConfiguration'),
    newDeleteAccountAuditConfiguration,
    DeleteAccountAuditConfigurationResponse (DeleteAccountAuditConfigurationResponse'),
    newDeleteAccountAuditConfigurationResponse,

    -- ** ListThings (Paginated)
    ListThings (ListThings'),
    newListThings,
    ListThingsResponse (ListThingsResponse'),
    newListThingsResponse,

    -- ** SetV2LoggingOptions
    SetV2LoggingOptions (SetV2LoggingOptions'),
    newSetV2LoggingOptions,
    SetV2LoggingOptionsResponse (SetV2LoggingOptionsResponse'),
    newSetV2LoggingOptionsResponse,

    -- ** UpdateThing
    UpdateThing (UpdateThing'),
    newUpdateThing,
    UpdateThingResponse (UpdateThingResponse'),
    newUpdateThingResponse,

    -- ** AddThingToThingGroup
    AddThingToThingGroup (AddThingToThingGroup'),
    newAddThingToThingGroup,
    AddThingToThingGroupResponse (AddThingToThingGroupResponse'),
    newAddThingToThingGroupResponse,

    -- ** GetLoggingOptions
    GetLoggingOptions (GetLoggingOptions'),
    newGetLoggingOptions,
    GetLoggingOptionsResponse (GetLoggingOptionsResponse'),
    newGetLoggingOptionsResponse,

    -- ** UpdateAuditSuppression
    UpdateAuditSuppression (UpdateAuditSuppression'),
    newUpdateAuditSuppression,
    UpdateAuditSuppressionResponse (UpdateAuditSuppressionResponse'),
    newUpdateAuditSuppressionResponse,

    -- ** ListScheduledAudits (Paginated)
    ListScheduledAudits (ListScheduledAudits'),
    newListScheduledAudits,
    ListScheduledAuditsResponse (ListScheduledAuditsResponse'),
    newListScheduledAuditsResponse,

    -- ** AttachThingPrincipal
    AttachThingPrincipal (AttachThingPrincipal'),
    newAttachThingPrincipal,
    AttachThingPrincipalResponse (AttachThingPrincipalResponse'),
    newAttachThingPrincipalResponse,

    -- ** DeleteThing
    DeleteThing (DeleteThing'),
    newDeleteThing,
    DeleteThingResponse (DeleteThingResponse'),
    newDeleteThingResponse,

    -- ** ListCertificatesByCA (Paginated)
    ListCertificatesByCA (ListCertificatesByCA'),
    newListCertificatesByCA,
    ListCertificatesByCAResponse (ListCertificatesByCAResponse'),
    newListCertificatesByCAResponse,

    -- ** ListThingGroupsForThing (Paginated)
    ListThingGroupsForThing (ListThingGroupsForThing'),
    newListThingGroupsForThing,
    ListThingGroupsForThingResponse (ListThingGroupsForThingResponse'),
    newListThingGroupsForThingResponse,

    -- ** UpdateBillingGroup
    UpdateBillingGroup (UpdateBillingGroup'),
    newUpdateBillingGroup,
    UpdateBillingGroupResponse (UpdateBillingGroupResponse'),
    newUpdateBillingGroupResponse,

    -- ** DeleteBillingGroup
    DeleteBillingGroup (DeleteBillingGroup'),
    newDeleteBillingGroup,
    DeleteBillingGroupResponse (DeleteBillingGroupResponse'),
    newDeleteBillingGroupResponse,

    -- ** UpdateAccountAuditConfiguration
    UpdateAccountAuditConfiguration (UpdateAccountAuditConfiguration'),
    newUpdateAccountAuditConfiguration,
    UpdateAccountAuditConfigurationResponse (UpdateAccountAuditConfigurationResponse'),
    newUpdateAccountAuditConfigurationResponse,

    -- ** DescribeThingRegistrationTask
    DescribeThingRegistrationTask (DescribeThingRegistrationTask'),
    newDescribeThingRegistrationTask,
    DescribeThingRegistrationTaskResponse (DescribeThingRegistrationTaskResponse'),
    newDescribeThingRegistrationTaskResponse,

    -- ** DescribeCustomMetric
    DescribeCustomMetric (DescribeCustomMetric'),
    newDescribeCustomMetric,
    DescribeCustomMetricResponse (DescribeCustomMetricResponse'),
    newDescribeCustomMetricResponse,

    -- ** DescribeCACertificate
    DescribeCACertificate (DescribeCACertificate'),
    newDescribeCACertificate,
    DescribeCACertificateResponse (DescribeCACertificateResponse'),
    newDescribeCACertificateResponse,

    -- ** DeleteProvisioningTemplateVersion
    DeleteProvisioningTemplateVersion (DeleteProvisioningTemplateVersion'),
    newDeleteProvisioningTemplateVersion,
    DeleteProvisioningTemplateVersionResponse (DeleteProvisioningTemplateVersionResponse'),
    newDeleteProvisioningTemplateVersionResponse,

    -- ** DeleteOTAUpdate
    DeleteOTAUpdate (DeleteOTAUpdate'),
    newDeleteOTAUpdate,
    DeleteOTAUpdateResponse (DeleteOTAUpdateResponse'),
    newDeleteOTAUpdateResponse,

    -- ** RegisterCertificateWithoutCA
    RegisterCertificateWithoutCA (RegisterCertificateWithoutCA'),
    newRegisterCertificateWithoutCA,
    RegisterCertificateWithoutCAResponse (RegisterCertificateWithoutCAResponse'),
    newRegisterCertificateWithoutCAResponse,

    -- ** ListDetectMitigationActionsExecutions (Paginated)
    ListDetectMitigationActionsExecutions (ListDetectMitigationActionsExecutions'),
    newListDetectMitigationActionsExecutions,
    ListDetectMitigationActionsExecutionsResponse (ListDetectMitigationActionsExecutionsResponse'),
    newListDetectMitigationActionsExecutionsResponse,

    -- ** CreateDynamicThingGroup
    CreateDynamicThingGroup (CreateDynamicThingGroup'),
    newCreateDynamicThingGroup,
    CreateDynamicThingGroupResponse (CreateDynamicThingGroupResponse'),
    newCreateDynamicThingGroupResponse,

    -- ** GetRegistrationCode
    GetRegistrationCode (GetRegistrationCode'),
    newGetRegistrationCode,
    GetRegistrationCodeResponse (GetRegistrationCodeResponse'),
    newGetRegistrationCodeResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** DetachSecurityProfile
    DetachSecurityProfile (DetachSecurityProfile'),
    newDetachSecurityProfile,
    DetachSecurityProfileResponse (DetachSecurityProfileResponse'),
    newDetachSecurityProfileResponse,

    -- ** TestInvokeAuthorizer
    TestInvokeAuthorizer (TestInvokeAuthorizer'),
    newTestInvokeAuthorizer,
    TestInvokeAuthorizerResponse (TestInvokeAuthorizerResponse'),
    newTestInvokeAuthorizerResponse,

    -- ** RemoveThingFromThingGroup
    RemoveThingFromThingGroup (RemoveThingFromThingGroup'),
    newRemoveThingFromThingGroup,
    RemoveThingFromThingGroupResponse (RemoveThingFromThingGroupResponse'),
    newRemoveThingFromThingGroupResponse,

    -- ** GetBehaviorModelTrainingSummaries (Paginated)
    GetBehaviorModelTrainingSummaries (GetBehaviorModelTrainingSummaries'),
    newGetBehaviorModelTrainingSummaries,
    GetBehaviorModelTrainingSummariesResponse (GetBehaviorModelTrainingSummariesResponse'),
    newGetBehaviorModelTrainingSummariesResponse,

    -- ** CreateProvisioningTemplateVersion
    CreateProvisioningTemplateVersion (CreateProvisioningTemplateVersion'),
    newCreateProvisioningTemplateVersion,
    CreateProvisioningTemplateVersionResponse (CreateProvisioningTemplateVersionResponse'),
    newCreateProvisioningTemplateVersionResponse,

    -- ** ListPrincipalThings (Paginated)
    ListPrincipalThings (ListPrincipalThings'),
    newListPrincipalThings,
    ListPrincipalThingsResponse (ListPrincipalThingsResponse'),
    newListPrincipalThingsResponse,

    -- ** ListAuditMitigationActionsTasks (Paginated)
    ListAuditMitigationActionsTasks (ListAuditMitigationActionsTasks'),
    newListAuditMitigationActionsTasks,
    ListAuditMitigationActionsTasksResponse (ListAuditMitigationActionsTasksResponse'),
    newListAuditMitigationActionsTasksResponse,

    -- ** DescribeRoleAlias
    DescribeRoleAlias (DescribeRoleAlias'),
    newDescribeRoleAlias,
    DescribeRoleAliasResponse (DescribeRoleAliasResponse'),
    newDescribeRoleAliasResponse,

    -- ** CreateTopicRuleDestination
    CreateTopicRuleDestination (CreateTopicRuleDestination'),
    newCreateTopicRuleDestination,
    CreateTopicRuleDestinationResponse (CreateTopicRuleDestinationResponse'),
    newCreateTopicRuleDestinationResponse,

    -- ** CreateOTAUpdate
    CreateOTAUpdate (CreateOTAUpdate'),
    newCreateOTAUpdate,
    CreateOTAUpdateResponse (CreateOTAUpdateResponse'),
    newCreateOTAUpdateResponse,

    -- ** DeleteDynamicThingGroup
    DeleteDynamicThingGroup (DeleteDynamicThingGroup'),
    newDeleteDynamicThingGroup,
    DeleteDynamicThingGroupResponse (DeleteDynamicThingGroupResponse'),
    newDeleteDynamicThingGroupResponse,

    -- ** UpdateDynamicThingGroup
    UpdateDynamicThingGroup (UpdateDynamicThingGroup'),
    newUpdateDynamicThingGroup,
    UpdateDynamicThingGroupResponse (UpdateDynamicThingGroupResponse'),
    newUpdateDynamicThingGroupResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** ListThingPrincipals (Paginated)
    ListThingPrincipals (ListThingPrincipals'),
    newListThingPrincipals,
    ListThingPrincipalsResponse (ListThingPrincipalsResponse'),
    newListThingPrincipalsResponse,

    -- ** DescribeDefaultAuthorizer
    DescribeDefaultAuthorizer (DescribeDefaultAuthorizer'),
    newDescribeDefaultAuthorizer,
    DescribeDefaultAuthorizerResponse (DescribeDefaultAuthorizerResponse'),
    newDescribeDefaultAuthorizerResponse,

    -- ** CreateThingGroup
    CreateThingGroup (CreateThingGroup'),
    newCreateThingGroup,
    CreateThingGroupResponse (CreateThingGroupResponse'),
    newCreateThingGroupResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** DeleteSecurityProfile
    DeleteSecurityProfile (DeleteSecurityProfile'),
    newDeleteSecurityProfile,
    DeleteSecurityProfileResponse (DeleteSecurityProfileResponse'),
    newDeleteSecurityProfileResponse,

    -- ** ValidateSecurityProfileBehaviors
    ValidateSecurityProfileBehaviors (ValidateSecurityProfileBehaviors'),
    newValidateSecurityProfileBehaviors,
    ValidateSecurityProfileBehaviorsResponse (ValidateSecurityProfileBehaviorsResponse'),
    newValidateSecurityProfileBehaviorsResponse,

    -- ** CreateDomainConfiguration
    CreateDomainConfiguration (CreateDomainConfiguration'),
    newCreateDomainConfiguration,
    CreateDomainConfigurationResponse (CreateDomainConfigurationResponse'),
    newCreateDomainConfigurationResponse,

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** ListCertificates (Paginated)
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** UpdateSecurityProfile
    UpdateSecurityProfile (UpdateSecurityProfile'),
    newUpdateSecurityProfile,
    UpdateSecurityProfileResponse (UpdateSecurityProfileResponse'),
    newUpdateSecurityProfileResponse,

    -- ** ListActiveViolations (Paginated)
    ListActiveViolations (ListActiveViolations'),
    newListActiveViolations,
    ListActiveViolationsResponse (ListActiveViolationsResponse'),
    newListActiveViolationsResponse,

    -- ** DescribeAuthorizer
    DescribeAuthorizer (DescribeAuthorizer'),
    newDescribeAuthorizer,
    DescribeAuthorizerResponse (DescribeAuthorizerResponse'),
    newDescribeAuthorizerResponse,

    -- ** DescribeAccountAuditConfiguration
    DescribeAccountAuditConfiguration (DescribeAccountAuditConfiguration'),
    newDescribeAccountAuditConfiguration,
    DescribeAccountAuditConfigurationResponse (DescribeAccountAuditConfigurationResponse'),
    newDescribeAccountAuditConfigurationResponse,

    -- ** DeprecateThingType
    DeprecateThingType (DeprecateThingType'),
    newDeprecateThingType,
    DeprecateThingTypeResponse (DeprecateThingTypeResponse'),
    newDeprecateThingTypeResponse,

    -- ** DeleteDimension
    DeleteDimension (DeleteDimension'),
    newDeleteDimension,
    DeleteDimensionResponse (DeleteDimensionResponse'),
    newDeleteDimensionResponse,

    -- ** DescribeBillingGroup
    DescribeBillingGroup (DescribeBillingGroup'),
    newDescribeBillingGroup,
    DescribeBillingGroupResponse (DescribeBillingGroupResponse'),
    newDescribeBillingGroupResponse,

    -- ** UpdateDimension
    UpdateDimension (UpdateDimension'),
    newUpdateDimension,
    UpdateDimensionResponse (UpdateDimensionResponse'),
    newUpdateDimensionResponse,

    -- ** ConfirmTopicRuleDestination
    ConfirmTopicRuleDestination (ConfirmTopicRuleDestination'),
    newConfirmTopicRuleDestination,
    ConfirmTopicRuleDestinationResponse (ConfirmTopicRuleDestinationResponse'),
    newConfirmTopicRuleDestinationResponse,

    -- ** DescribeDetectMitigationActionsTask
    DescribeDetectMitigationActionsTask (DescribeDetectMitigationActionsTask'),
    newDescribeDetectMitigationActionsTask,
    DescribeDetectMitigationActionsTaskResponse (DescribeDetectMitigationActionsTaskResponse'),
    newDescribeDetectMitigationActionsTaskResponse,

    -- ** ListThingRegistrationTasks (Paginated)
    ListThingRegistrationTasks (ListThingRegistrationTasks'),
    newListThingRegistrationTasks,
    ListThingRegistrationTasksResponse (ListThingRegistrationTasksResponse'),
    newListThingRegistrationTasksResponse,

    -- ** ListDimensions (Paginated)
    ListDimensions (ListDimensions'),
    newListDimensions,
    ListDimensionsResponse (ListDimensionsResponse'),
    newListDimensionsResponse,

    -- ** DescribeAuditSuppression
    DescribeAuditSuppression (DescribeAuditSuppression'),
    newDescribeAuditSuppression,
    DescribeAuditSuppressionResponse (DescribeAuditSuppressionResponse'),
    newDescribeAuditSuppressionResponse,

    -- ** ListAuditFindings (Paginated)
    ListAuditFindings (ListAuditFindings'),
    newListAuditFindings,
    ListAuditFindingsResponse (ListAuditFindingsResponse'),
    newListAuditFindingsResponse,

    -- ** DescribeThing
    DescribeThing (DescribeThing'),
    newDescribeThing,
    DescribeThingResponse (DescribeThingResponse'),
    newDescribeThingResponse,

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** DetachThingPrincipal
    DetachThingPrincipal (DetachThingPrincipal'),
    newDetachThingPrincipal,
    DetachThingPrincipalResponse (DetachThingPrincipalResponse'),
    newDetachThingPrincipalResponse,

    -- ** StartOnDemandAuditTask
    StartOnDemandAuditTask (StartOnDemandAuditTask'),
    newStartOnDemandAuditTask,
    StartOnDemandAuditTaskResponse (StartOnDemandAuditTaskResponse'),
    newStartOnDemandAuditTaskResponse,

    -- ** ListAttachedPolicies (Paginated)
    ListAttachedPolicies (ListAttachedPolicies'),
    newListAttachedPolicies,
    ListAttachedPoliciesResponse (ListAttachedPoliciesResponse'),
    newListAttachedPoliciesResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ListCACertificates (Paginated)
    ListCACertificates (ListCACertificates'),
    newListCACertificates,
    ListCACertificatesResponse (ListCACertificatesResponse'),
    newListCACertificatesResponse,

    -- ** EnableTopicRule
    EnableTopicRule (EnableTopicRule'),
    newEnableTopicRule,
    EnableTopicRuleResponse (EnableTopicRuleResponse'),
    newEnableTopicRuleResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** RegisterCACertificate
    RegisterCACertificate (RegisterCACertificate'),
    newRegisterCACertificate,
    RegisterCACertificateResponse (RegisterCACertificateResponse'),
    newRegisterCACertificateResponse,

    -- ** ListSecurityProfilesForTarget (Paginated)
    ListSecurityProfilesForTarget (ListSecurityProfilesForTarget'),
    newListSecurityProfilesForTarget,
    ListSecurityProfilesForTargetResponse (ListSecurityProfilesForTargetResponse'),
    newListSecurityProfilesForTargetResponse,

    -- ** UpdateEventConfigurations
    UpdateEventConfigurations (UpdateEventConfigurations'),
    newUpdateEventConfigurations,
    UpdateEventConfigurationsResponse (UpdateEventConfigurationsResponse'),
    newUpdateEventConfigurationsResponse,

    -- ** GetJobDocument
    GetJobDocument (GetJobDocument'),
    newGetJobDocument,
    GetJobDocumentResponse (GetJobDocumentResponse'),
    newGetJobDocumentResponse,

    -- ** ListTopicRules (Paginated)
    ListTopicRules (ListTopicRules'),
    newListTopicRules,
    ListTopicRulesResponse (ListTopicRulesResponse'),
    newListTopicRulesResponse,

    -- ** DescribeThingGroup
    DescribeThingGroup (DescribeThingGroup'),
    newDescribeThingGroup,
    DescribeThingGroupResponse (DescribeThingGroupResponse'),
    newDescribeThingGroupResponse,

    -- ** AcceptCertificateTransfer
    AcceptCertificateTransfer (AcceptCertificateTransfer'),
    newAcceptCertificateTransfer,
    AcceptCertificateTransferResponse (AcceptCertificateTransferResponse'),
    newAcceptCertificateTransferResponse,

    -- ** UpdateThingGroupsForThing
    UpdateThingGroupsForThing (UpdateThingGroupsForThing'),
    newUpdateThingGroupsForThing,
    UpdateThingGroupsForThingResponse (UpdateThingGroupsForThingResponse'),
    newUpdateThingGroupsForThingResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

    -- ** ReplaceTopicRule
    ReplaceTopicRule (ReplaceTopicRule'),
    newReplaceTopicRule,
    ReplaceTopicRuleResponse (ReplaceTopicRuleResponse'),
    newReplaceTopicRuleResponse,

    -- ** DescribeIndex
    DescribeIndex (DescribeIndex'),
    newDescribeIndex,
    DescribeIndexResponse (DescribeIndexResponse'),
    newDescribeIndexResponse,

    -- ** DeletePolicyVersion
    DeletePolicyVersion (DeletePolicyVersion'),
    newDeletePolicyVersion,
    DeletePolicyVersionResponse (DeletePolicyVersionResponse'),
    newDeletePolicyVersionResponse,

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** ClearDefaultAuthorizer
    ClearDefaultAuthorizer (ClearDefaultAuthorizer'),
    newClearDefaultAuthorizer,
    ClearDefaultAuthorizerResponse (ClearDefaultAuthorizerResponse'),
    newClearDefaultAuthorizerResponse,

    -- ** CreateTopicRule
    CreateTopicRule (CreateTopicRule'),
    newCreateTopicRule,
    CreateTopicRuleResponse (CreateTopicRuleResponse'),
    newCreateTopicRuleResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** UpdateRoleAlias
    UpdateRoleAlias (UpdateRoleAlias'),
    newUpdateRoleAlias,
    UpdateRoleAliasResponse (UpdateRoleAliasResponse'),
    newUpdateRoleAliasResponse,

    -- ** DeleteRoleAlias
    DeleteRoleAlias (DeleteRoleAlias'),
    newDeleteRoleAlias,
    DeleteRoleAliasResponse (DeleteRoleAliasResponse'),
    newDeleteRoleAliasResponse,

    -- ** GetStatistics
    GetStatistics (GetStatistics'),
    newGetStatistics,
    GetStatisticsResponse (GetStatisticsResponse'),
    newGetStatisticsResponse,

    -- ** AssociateTargetsWithJob
    AssociateTargetsWithJob (AssociateTargetsWithJob'),
    newAssociateTargetsWithJob,
    AssociateTargetsWithJobResponse (AssociateTargetsWithJobResponse'),
    newAssociateTargetsWithJobResponse,

    -- ** ListPolicyVersions
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

    -- ** CancelJobExecution
    CancelJobExecution (CancelJobExecution'),
    newCancelJobExecution,
    CancelJobExecutionResponse (CancelJobExecutionResponse'),
    newCancelJobExecutionResponse,

    -- ** CancelCertificateTransfer
    CancelCertificateTransfer (CancelCertificateTransfer'),
    newCancelCertificateTransfer,
    CancelCertificateTransferResponse (CancelCertificateTransferResponse'),
    newCancelCertificateTransferResponse,

    -- ** CreateThingType
    CreateThingType (CreateThingType'),
    newCreateThingType,
    CreateThingTypeResponse (CreateThingTypeResponse'),
    newCreateThingTypeResponse,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    UpdateAuthorizerResponse (UpdateAuthorizerResponse'),
    newUpdateAuthorizerResponse,

    -- ** SearchIndex
    SearchIndex (SearchIndex'),
    newSearchIndex,
    SearchIndexResponse (SearchIndexResponse'),
    newSearchIndexResponse,

    -- ** DescribeSecurityProfile
    DescribeSecurityProfile (DescribeSecurityProfile'),
    newDescribeSecurityProfile,
    DescribeSecurityProfileResponse (DescribeSecurityProfileResponse'),
    newDescribeSecurityProfileResponse,

    -- ** ListJobExecutionsForJob (Paginated)
    ListJobExecutionsForJob (ListJobExecutionsForJob'),
    newListJobExecutionsForJob,
    ListJobExecutionsForJobResponse (ListJobExecutionsForJobResponse'),
    newListJobExecutionsForJobResponse,

    -- ** CreateBillingGroup
    CreateBillingGroup (CreateBillingGroup'),
    newCreateBillingGroup,
    CreateBillingGroupResponse (CreateBillingGroupResponse'),
    newCreateBillingGroupResponse,

    -- ** CancelAuditMitigationActionsTask
    CancelAuditMitigationActionsTask (CancelAuditMitigationActionsTask'),
    newCancelAuditMitigationActionsTask,
    CancelAuditMitigationActionsTaskResponse (CancelAuditMitigationActionsTaskResponse'),
    newCancelAuditMitigationActionsTaskResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** RemoveThingFromBillingGroup
    RemoveThingFromBillingGroup (RemoveThingFromBillingGroup'),
    newRemoveThingFromBillingGroup,
    RemoveThingFromBillingGroupResponse (RemoveThingFromBillingGroupResponse'),
    newRemoveThingFromBillingGroupResponse,

    -- ** ListAuthorizers (Paginated)
    ListAuthorizers (ListAuthorizers'),
    newListAuthorizers,
    ListAuthorizersResponse (ListAuthorizersResponse'),
    newListAuthorizersResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** CreateAuditSuppression
    CreateAuditSuppression (CreateAuditSuppression'),
    newCreateAuditSuppression,
    CreateAuditSuppressionResponse (CreateAuditSuppressionResponse'),
    newCreateAuditSuppressionResponse,

    -- ** CreateProvisioningTemplate
    CreateProvisioningTemplate (CreateProvisioningTemplate'),
    newCreateProvisioningTemplate,
    CreateProvisioningTemplateResponse (CreateProvisioningTemplateResponse'),
    newCreateProvisioningTemplateResponse,

    -- ** GetTopicRuleDestination
    GetTopicRuleDestination (GetTopicRuleDestination'),
    newGetTopicRuleDestination,
    GetTopicRuleDestinationResponse (GetTopicRuleDestinationResponse'),
    newGetTopicRuleDestinationResponse,

    -- ** DescribeAuditTask
    DescribeAuditTask (DescribeAuditTask'),
    newDescribeAuditTask,
    DescribeAuditTaskResponse (DescribeAuditTaskResponse'),
    newDescribeAuditTaskResponse,

    -- ** DescribeDomainConfiguration
    DescribeDomainConfiguration (DescribeDomainConfiguration'),
    newDescribeDomainConfiguration,
    DescribeDomainConfigurationResponse (DescribeDomainConfigurationResponse'),
    newDescribeDomainConfigurationResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** ListAuditSuppressions (Paginated)
    ListAuditSuppressions (ListAuditSuppressions'),
    newListAuditSuppressions,
    ListAuditSuppressionsResponse (ListAuditSuppressionsResponse'),
    newListAuditSuppressionsResponse,

    -- ** CreateCertificateFromCsr
    CreateCertificateFromCsr (CreateCertificateFromCsr'),
    newCreateCertificateFromCsr,
    CreateCertificateFromCsrResponse (CreateCertificateFromCsrResponse'),
    newCreateCertificateFromCsrResponse,

    -- ** GetOTAUpdate
    GetOTAUpdate (GetOTAUpdate'),
    newGetOTAUpdate,
    GetOTAUpdateResponse (GetOTAUpdateResponse'),
    newGetOTAUpdateResponse,

    -- ** GetEffectivePolicies
    GetEffectivePolicies (GetEffectivePolicies'),
    newGetEffectivePolicies,
    GetEffectivePoliciesResponse (GetEffectivePoliciesResponse'),
    newGetEffectivePoliciesResponse,

    -- ** UpdateScheduledAudit
    UpdateScheduledAudit (UpdateScheduledAudit'),
    newUpdateScheduledAudit,
    UpdateScheduledAuditResponse (UpdateScheduledAuditResponse'),
    newUpdateScheduledAuditResponse,

    -- ** DescribeAuditFinding
    DescribeAuditFinding (DescribeAuditFinding'),
    newDescribeAuditFinding,
    DescribeAuditFindingResponse (DescribeAuditFindingResponse'),
    newDescribeAuditFindingResponse,

    -- ** DeleteScheduledAudit
    DeleteScheduledAudit (DeleteScheduledAudit'),
    newDeleteScheduledAudit,
    DeleteScheduledAuditResponse (DeleteScheduledAuditResponse'),
    newDeleteScheduledAuditResponse,

    -- ** ListBillingGroups (Paginated)
    ListBillingGroups (ListBillingGroups'),
    newListBillingGroups,
    ListBillingGroupsResponse (ListBillingGroupsResponse'),
    newListBillingGroupsResponse,

    -- ** TestAuthorization
    TestAuthorization (TestAuthorization'),
    newTestAuthorization,
    TestAuthorizationResponse (TestAuthorizationResponse'),
    newTestAuthorizationResponse,

    -- ** ListThingTypes (Paginated)
    ListThingTypes (ListThingTypes'),
    newListThingTypes,
    ListThingTypesResponse (ListThingTypesResponse'),
    newListThingTypesResponse,

    -- ** ListIndices (Paginated)
    ListIndices (ListIndices'),
    newListIndices,
    ListIndicesResponse (ListIndicesResponse'),
    newListIndicesResponse,

    -- ** DeleteThingType
    DeleteThingType (DeleteThingType'),
    newDeleteThingType,
    DeleteThingTypeResponse (DeleteThingTypeResponse'),
    newDeleteThingTypeResponse,

    -- ** RegisterThing
    RegisterThing (RegisterThing'),
    newRegisterThing,
    RegisterThingResponse (RegisterThingResponse'),
    newRegisterThingResponse,

    -- ** ListOutgoingCertificates (Paginated)
    ListOutgoingCertificates (ListOutgoingCertificates'),
    newListOutgoingCertificates,
    ListOutgoingCertificatesResponse (ListOutgoingCertificatesResponse'),
    newListOutgoingCertificatesResponse,

    -- ** DeleteTopicRuleDestination
    DeleteTopicRuleDestination (DeleteTopicRuleDestination'),
    newDeleteTopicRuleDestination,
    DeleteTopicRuleDestinationResponse (DeleteTopicRuleDestinationResponse'),
    newDeleteTopicRuleDestinationResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTopicRuleDestinations (Paginated)
    ListTopicRuleDestinations (ListTopicRuleDestinations'),
    newListTopicRuleDestinations,
    ListTopicRuleDestinationsResponse (ListTopicRuleDestinationsResponse'),
    newListTopicRuleDestinationsResponse,

    -- ** CancelDetectMitigationActionsTask
    CancelDetectMitigationActionsTask (CancelDetectMitigationActionsTask'),
    newCancelDetectMitigationActionsTask,
    CancelDetectMitigationActionsTaskResponse (CancelDetectMitigationActionsTaskResponse'),
    newCancelDetectMitigationActionsTaskResponse,

    -- ** AddThingToBillingGroup
    AddThingToBillingGroup (AddThingToBillingGroup'),
    newAddThingToBillingGroup,
    AddThingToBillingGroupResponse (AddThingToBillingGroupResponse'),
    newAddThingToBillingGroupResponse,

    -- ** DeleteThingGroup
    DeleteThingGroup (DeleteThingGroup'),
    newDeleteThingGroup,
    DeleteThingGroupResponse (DeleteThingGroupResponse'),
    newDeleteThingGroupResponse,

    -- ** DescribeEventConfigurations
    DescribeEventConfigurations (DescribeEventConfigurations'),
    newDescribeEventConfigurations,
    DescribeEventConfigurationsResponse (DescribeEventConfigurationsResponse'),
    newDescribeEventConfigurationsResponse,

    -- ** UpdateTopicRuleDestination
    UpdateTopicRuleDestination (UpdateTopicRuleDestination'),
    newUpdateTopicRuleDestination,
    UpdateTopicRuleDestinationResponse (UpdateTopicRuleDestinationResponse'),
    newUpdateTopicRuleDestinationResponse,

    -- ** ListOTAUpdates (Paginated)
    ListOTAUpdates (ListOTAUpdates'),
    newListOTAUpdates,
    ListOTAUpdatesResponse (ListOTAUpdatesResponse'),
    newListOTAUpdatesResponse,

    -- ** ListThingGroups (Paginated)
    ListThingGroups (ListThingGroups'),
    newListThingGroups,
    ListThingGroupsResponse (ListThingGroupsResponse'),
    newListThingGroupsResponse,

    -- ** ListProvisioningTemplateVersions (Paginated)
    ListProvisioningTemplateVersions (ListProvisioningTemplateVersions'),
    newListProvisioningTemplateVersions,
    ListProvisioningTemplateVersionsResponse (ListProvisioningTemplateVersionsResponse'),
    newListProvisioningTemplateVersionsResponse,

    -- ** UpdateThingGroup
    UpdateThingGroup (UpdateThingGroup'),
    newUpdateThingGroup,
    UpdateThingGroupResponse (UpdateThingGroupResponse'),
    newUpdateThingGroupResponse,

    -- * Types

    -- ** AbortAction
    AbortAction (..),

    -- ** ActionType
    ActionType (..),

    -- ** AlertTargetType
    AlertTargetType (..),

    -- ** AuditCheckRunStatus
    AuditCheckRunStatus (..),

    -- ** AuditFindingSeverity
    AuditFindingSeverity (..),

    -- ** AuditFrequency
    AuditFrequency (..),

    -- ** AuditMitigationActionsExecutionStatus
    AuditMitigationActionsExecutionStatus (..),

    -- ** AuditMitigationActionsTaskStatus
    AuditMitigationActionsTaskStatus (..),

    -- ** AuditNotificationType
    AuditNotificationType (..),

    -- ** AuditTaskStatus
    AuditTaskStatus (..),

    -- ** AuditTaskType
    AuditTaskType (..),

    -- ** AuthDecision
    AuthDecision (..),

    -- ** AuthorizerStatus
    AuthorizerStatus (..),

    -- ** AutoRegistrationStatus
    AutoRegistrationStatus (..),

    -- ** AwsJobAbortCriteriaAbortAction
    AwsJobAbortCriteriaAbortAction (..),

    -- ** AwsJobAbortCriteriaFailureType
    AwsJobAbortCriteriaFailureType (..),

    -- ** BehaviorCriteriaType
    BehaviorCriteriaType (..),

    -- ** CACertificateStatus
    CACertificateStatus (..),

    -- ** CACertificateUpdateAction
    CACertificateUpdateAction (..),

    -- ** CannedAccessControlList
    CannedAccessControlList (..),

    -- ** CertificateMode
    CertificateMode (..),

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** ConfidenceLevel
    ConfidenceLevel (..),

    -- ** CustomMetricType
    CustomMetricType (..),

    -- ** DayOfWeek
    DayOfWeek (..),

    -- ** DetectMitigationActionExecutionStatus
    DetectMitigationActionExecutionStatus (..),

    -- ** DetectMitigationActionsTaskStatus
    DetectMitigationActionsTaskStatus (..),

    -- ** DeviceCertificateUpdateAction
    DeviceCertificateUpdateAction (..),

    -- ** DimensionType
    DimensionType (..),

    -- ** DimensionValueOperator
    DimensionValueOperator (..),

    -- ** DomainConfigurationStatus
    DomainConfigurationStatus (..),

    -- ** DomainType
    DomainType (..),

    -- ** DynamicGroupStatus
    DynamicGroupStatus (..),

    -- ** DynamoKeyType
    DynamoKeyType (..),

    -- ** EventType
    EventType (..),

    -- ** FieldType
    FieldType (..),

    -- ** IndexStatus
    IndexStatus (..),

    -- ** JobExecutionFailureType
    JobExecutionFailureType (..),

    -- ** JobExecutionStatus
    JobExecutionStatus (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** LogTargetType
    LogTargetType (..),

    -- ** MessageFormat
    MessageFormat (..),

    -- ** MitigationActionType
    MitigationActionType (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** OTAUpdateStatus
    OTAUpdateStatus (..),

    -- ** PolicyTemplateName
    PolicyTemplateName (..),

    -- ** Protocol
    Protocol (..),

    -- ** ReportType
    ReportType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ServerCertificateStatus
    ServerCertificateStatus (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** TargetSelection
    TargetSelection (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** ThingConnectivityIndexingMode
    ThingConnectivityIndexingMode (..),

    -- ** ThingGroupIndexingMode
    ThingGroupIndexingMode (..),

    -- ** ThingIndexingMode
    ThingIndexingMode (..),

    -- ** TopicRuleDestinationStatus
    TopicRuleDestinationStatus (..),

    -- ** ViolationEventType
    ViolationEventType (..),

    -- ** AbortConfig
    AbortConfig (AbortConfig'),
    newAbortConfig,

    -- ** AbortCriteria
    AbortCriteria (AbortCriteria'),
    newAbortCriteria,

    -- ** Action
    Action (Action'),
    newAction,

    -- ** ActiveViolation
    ActiveViolation (ActiveViolation'),
    newActiveViolation,

    -- ** AddThingsToThingGroupParams
    AddThingsToThingGroupParams (AddThingsToThingGroupParams'),
    newAddThingsToThingGroupParams,

    -- ** AlertTarget
    AlertTarget (AlertTarget'),
    newAlertTarget,

    -- ** Allowed
    Allowed (Allowed'),
    newAllowed,

    -- ** AssetPropertyTimestamp
    AssetPropertyTimestamp (AssetPropertyTimestamp'),
    newAssetPropertyTimestamp,

    -- ** AssetPropertyValue
    AssetPropertyValue (AssetPropertyValue'),
    newAssetPropertyValue,

    -- ** AssetPropertyVariant
    AssetPropertyVariant (AssetPropertyVariant'),
    newAssetPropertyVariant,

    -- ** AttributePayload
    AttributePayload (AttributePayload'),
    newAttributePayload,

    -- ** AuditCheckConfiguration
    AuditCheckConfiguration (AuditCheckConfiguration'),
    newAuditCheckConfiguration,

    -- ** AuditCheckDetails
    AuditCheckDetails (AuditCheckDetails'),
    newAuditCheckDetails,

    -- ** AuditFinding
    AuditFinding (AuditFinding'),
    newAuditFinding,

    -- ** AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata (AuditMitigationActionExecutionMetadata'),
    newAuditMitigationActionExecutionMetadata,

    -- ** AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata (AuditMitigationActionsTaskMetadata'),
    newAuditMitigationActionsTaskMetadata,

    -- ** AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (AuditMitigationActionsTaskTarget'),
    newAuditMitigationActionsTaskTarget,

    -- ** AuditNotificationTarget
    AuditNotificationTarget (AuditNotificationTarget'),
    newAuditNotificationTarget,

    -- ** AuditSuppression
    AuditSuppression (AuditSuppression'),
    newAuditSuppression,

    -- ** AuditTaskMetadata
    AuditTaskMetadata (AuditTaskMetadata'),
    newAuditTaskMetadata,

    -- ** AuthInfo
    AuthInfo (AuthInfo'),
    newAuthInfo,

    -- ** AuthResult
    AuthResult (AuthResult'),
    newAuthResult,

    -- ** AuthorizerConfig
    AuthorizerConfig (AuthorizerConfig'),
    newAuthorizerConfig,

    -- ** AuthorizerDescription
    AuthorizerDescription (AuthorizerDescription'),
    newAuthorizerDescription,

    -- ** AuthorizerSummary
    AuthorizerSummary (AuthorizerSummary'),
    newAuthorizerSummary,

    -- ** AwsJobAbortConfig
    AwsJobAbortConfig (AwsJobAbortConfig'),
    newAwsJobAbortConfig,

    -- ** AwsJobAbortCriteria
    AwsJobAbortCriteria (AwsJobAbortCriteria'),
    newAwsJobAbortCriteria,

    -- ** AwsJobExecutionsRolloutConfig
    AwsJobExecutionsRolloutConfig (AwsJobExecutionsRolloutConfig'),
    newAwsJobExecutionsRolloutConfig,

    -- ** AwsJobExponentialRolloutRate
    AwsJobExponentialRolloutRate (AwsJobExponentialRolloutRate'),
    newAwsJobExponentialRolloutRate,

    -- ** AwsJobPresignedUrlConfig
    AwsJobPresignedUrlConfig (AwsJobPresignedUrlConfig'),
    newAwsJobPresignedUrlConfig,

    -- ** AwsJobRateIncreaseCriteria
    AwsJobRateIncreaseCriteria (AwsJobRateIncreaseCriteria'),
    newAwsJobRateIncreaseCriteria,

    -- ** AwsJobTimeoutConfig
    AwsJobTimeoutConfig (AwsJobTimeoutConfig'),
    newAwsJobTimeoutConfig,

    -- ** Behavior
    Behavior (Behavior'),
    newBehavior,

    -- ** BehaviorCriteria
    BehaviorCriteria (BehaviorCriteria'),
    newBehaviorCriteria,

    -- ** BehaviorModelTrainingSummary
    BehaviorModelTrainingSummary (BehaviorModelTrainingSummary'),
    newBehaviorModelTrainingSummary,

    -- ** BillingGroupMetadata
    BillingGroupMetadata (BillingGroupMetadata'),
    newBillingGroupMetadata,

    -- ** BillingGroupProperties
    BillingGroupProperties (BillingGroupProperties'),
    newBillingGroupProperties,

    -- ** CACertificate
    CACertificate (CACertificate'),
    newCACertificate,

    -- ** CACertificateDescription
    CACertificateDescription (CACertificateDescription'),
    newCACertificateDescription,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CertificateDescription
    CertificateDescription (CertificateDescription'),
    newCertificateDescription,

    -- ** CertificateValidity
    CertificateValidity (CertificateValidity'),
    newCertificateValidity,

    -- ** CloudwatchAlarmAction
    CloudwatchAlarmAction (CloudwatchAlarmAction'),
    newCloudwatchAlarmAction,

    -- ** CloudwatchLogsAction
    CloudwatchLogsAction (CloudwatchLogsAction'),
    newCloudwatchLogsAction,

    -- ** CloudwatchMetricAction
    CloudwatchMetricAction (CloudwatchMetricAction'),
    newCloudwatchMetricAction,

    -- ** CodeSigning
    CodeSigning (CodeSigning'),
    newCodeSigning,

    -- ** CodeSigningCertificateChain
    CodeSigningCertificateChain (CodeSigningCertificateChain'),
    newCodeSigningCertificateChain,

    -- ** CodeSigningSignature
    CodeSigningSignature (CodeSigningSignature'),
    newCodeSigningSignature,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** CustomCodeSigning
    CustomCodeSigning (CustomCodeSigning'),
    newCustomCodeSigning,

    -- ** Denied
    Denied (Denied'),
    newDenied,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** DetectMitigationActionExecution
    DetectMitigationActionExecution (DetectMitigationActionExecution'),
    newDetectMitigationActionExecution,

    -- ** DetectMitigationActionsTaskStatistics
    DetectMitigationActionsTaskStatistics (DetectMitigationActionsTaskStatistics'),
    newDetectMitigationActionsTaskStatistics,

    -- ** DetectMitigationActionsTaskSummary
    DetectMitigationActionsTaskSummary (DetectMitigationActionsTaskSummary'),
    newDetectMitigationActionsTaskSummary,

    -- ** DetectMitigationActionsTaskTarget
    DetectMitigationActionsTaskTarget (DetectMitigationActionsTaskTarget'),
    newDetectMitigationActionsTaskTarget,

    -- ** DomainConfigurationSummary
    DomainConfigurationSummary (DomainConfigurationSummary'),
    newDomainConfigurationSummary,

    -- ** DynamoDBAction
    DynamoDBAction (DynamoDBAction'),
    newDynamoDBAction,

    -- ** DynamoDBv2Action
    DynamoDBv2Action (DynamoDBv2Action'),
    newDynamoDBv2Action,

    -- ** EffectivePolicy
    EffectivePolicy (EffectivePolicy'),
    newEffectivePolicy,

    -- ** ElasticsearchAction
    ElasticsearchAction (ElasticsearchAction'),
    newElasticsearchAction,

    -- ** EnableIoTLoggingParams
    EnableIoTLoggingParams (EnableIoTLoggingParams'),
    newEnableIoTLoggingParams,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** ExplicitDeny
    ExplicitDeny (ExplicitDeny'),
    newExplicitDeny,

    -- ** ExponentialRolloutRate
    ExponentialRolloutRate (ExponentialRolloutRate'),
    newExponentialRolloutRate,

    -- ** Field
    Field (Field'),
    newField,

    -- ** FileLocation
    FileLocation (FileLocation'),
    newFileLocation,

    -- ** FirehoseAction
    FirehoseAction (FirehoseAction'),
    newFirehoseAction,

    -- ** GroupNameAndArn
    GroupNameAndArn (GroupNameAndArn'),
    newGroupNameAndArn,

    -- ** HttpAction
    HttpAction (HttpAction'),
    newHttpAction,

    -- ** HttpActionHeader
    HttpActionHeader (HttpActionHeader'),
    newHttpActionHeader,

    -- ** HttpAuthorization
    HttpAuthorization (HttpAuthorization'),
    newHttpAuthorization,

    -- ** HttpContext
    HttpContext (HttpContext'),
    newHttpContext,

    -- ** HttpUrlDestinationConfiguration
    HttpUrlDestinationConfiguration (HttpUrlDestinationConfiguration'),
    newHttpUrlDestinationConfiguration,

    -- ** HttpUrlDestinationProperties
    HttpUrlDestinationProperties (HttpUrlDestinationProperties'),
    newHttpUrlDestinationProperties,

    -- ** HttpUrlDestinationSummary
    HttpUrlDestinationSummary (HttpUrlDestinationSummary'),
    newHttpUrlDestinationSummary,

    -- ** ImplicitDeny
    ImplicitDeny (ImplicitDeny'),
    newImplicitDeny,

    -- ** IotAnalyticsAction
    IotAnalyticsAction (IotAnalyticsAction'),
    newIotAnalyticsAction,

    -- ** IotEventsAction
    IotEventsAction (IotEventsAction'),
    newIotEventsAction,

    -- ** IotSiteWiseAction
    IotSiteWiseAction (IotSiteWiseAction'),
    newIotSiteWiseAction,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobExecution
    JobExecution (JobExecution'),
    newJobExecution,

    -- ** JobExecutionStatusDetails
    JobExecutionStatusDetails (JobExecutionStatusDetails'),
    newJobExecutionStatusDetails,

    -- ** JobExecutionSummary
    JobExecutionSummary (JobExecutionSummary'),
    newJobExecutionSummary,

    -- ** JobExecutionSummaryForJob
    JobExecutionSummaryForJob (JobExecutionSummaryForJob'),
    newJobExecutionSummaryForJob,

    -- ** JobExecutionSummaryForThing
    JobExecutionSummaryForThing (JobExecutionSummaryForThing'),
    newJobExecutionSummaryForThing,

    -- ** JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (JobExecutionsRolloutConfig'),
    newJobExecutionsRolloutConfig,

    -- ** JobProcessDetails
    JobProcessDetails (JobProcessDetails'),
    newJobProcessDetails,

    -- ** JobSummary
    JobSummary (JobSummary'),
    newJobSummary,

    -- ** KafkaAction
    KafkaAction (KafkaAction'),
    newKafkaAction,

    -- ** KeyPair
    KeyPair (KeyPair'),
    newKeyPair,

    -- ** KinesisAction
    KinesisAction (KinesisAction'),
    newKinesisAction,

    -- ** LambdaAction
    LambdaAction (LambdaAction'),
    newLambdaAction,

    -- ** LogTarget
    LogTarget (LogTarget'),
    newLogTarget,

    -- ** LogTargetConfiguration
    LogTargetConfiguration (LogTargetConfiguration'),
    newLogTargetConfiguration,

    -- ** LoggingOptionsPayload
    LoggingOptionsPayload (LoggingOptionsPayload'),
    newLoggingOptionsPayload,

    -- ** MachineLearningDetectionConfig
    MachineLearningDetectionConfig (MachineLearningDetectionConfig'),
    newMachineLearningDetectionConfig,

    -- ** MetricDimension
    MetricDimension (MetricDimension'),
    newMetricDimension,

    -- ** MetricToRetain
    MetricToRetain (MetricToRetain'),
    newMetricToRetain,

    -- ** MetricValue
    MetricValue (MetricValue'),
    newMetricValue,

    -- ** MitigationAction
    MitigationAction (MitigationAction'),
    newMitigationAction,

    -- ** MitigationActionIdentifier
    MitigationActionIdentifier (MitigationActionIdentifier'),
    newMitigationActionIdentifier,

    -- ** MitigationActionParams
    MitigationActionParams (MitigationActionParams'),
    newMitigationActionParams,

    -- ** MqttContext
    MqttContext (MqttContext'),
    newMqttContext,

    -- ** NonCompliantResource
    NonCompliantResource (NonCompliantResource'),
    newNonCompliantResource,

    -- ** OTAUpdateFile
    OTAUpdateFile (OTAUpdateFile'),
    newOTAUpdateFile,

    -- ** OTAUpdateInfo
    OTAUpdateInfo (OTAUpdateInfo'),
    newOTAUpdateInfo,

    -- ** OTAUpdateSummary
    OTAUpdateSummary (OTAUpdateSummary'),
    newOTAUpdateSummary,

    -- ** OutgoingCertificate
    OutgoingCertificate (OutgoingCertificate'),
    newOutgoingCertificate,

    -- ** PercentPair
    PercentPair (PercentPair'),
    newPercentPair,

    -- ** Policy
    Policy (Policy'),
    newPolicy,

    -- ** PolicyVersion
    PolicyVersion (PolicyVersion'),
    newPolicyVersion,

    -- ** PolicyVersionIdentifier
    PolicyVersionIdentifier (PolicyVersionIdentifier'),
    newPolicyVersionIdentifier,

    -- ** PresignedUrlConfig
    PresignedUrlConfig (PresignedUrlConfig'),
    newPresignedUrlConfig,

    -- ** ProvisioningHook
    ProvisioningHook (ProvisioningHook'),
    newProvisioningHook,

    -- ** ProvisioningTemplateSummary
    ProvisioningTemplateSummary (ProvisioningTemplateSummary'),
    newProvisioningTemplateSummary,

    -- ** ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (ProvisioningTemplateVersionSummary'),
    newProvisioningTemplateVersionSummary,

    -- ** PublishFindingToSnsParams
    PublishFindingToSnsParams (PublishFindingToSnsParams'),
    newPublishFindingToSnsParams,

    -- ** PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (PutAssetPropertyValueEntry'),
    newPutAssetPropertyValueEntry,

    -- ** PutItemInput
    PutItemInput (PutItemInput'),
    newPutItemInput,

    -- ** RateIncreaseCriteria
    RateIncreaseCriteria (RateIncreaseCriteria'),
    newRateIncreaseCriteria,

    -- ** RegistrationConfig
    RegistrationConfig (RegistrationConfig'),
    newRegistrationConfig,

    -- ** RelatedResource
    RelatedResource (RelatedResource'),
    newRelatedResource,

    -- ** ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams (ReplaceDefaultPolicyVersionParams'),
    newReplaceDefaultPolicyVersionParams,

    -- ** RepublishAction
    RepublishAction (RepublishAction'),
    newRepublishAction,

    -- ** ResourceIdentifier
    ResourceIdentifier (ResourceIdentifier'),
    newResourceIdentifier,

    -- ** RoleAliasDescription
    RoleAliasDescription (RoleAliasDescription'),
    newRoleAliasDescription,

    -- ** S3Action
    S3Action (S3Action'),
    newS3Action,

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SalesforceAction
    SalesforceAction (SalesforceAction'),
    newSalesforceAction,

    -- ** ScheduledAuditMetadata
    ScheduledAuditMetadata (ScheduledAuditMetadata'),
    newScheduledAuditMetadata,

    -- ** SecurityProfileIdentifier
    SecurityProfileIdentifier (SecurityProfileIdentifier'),
    newSecurityProfileIdentifier,

    -- ** SecurityProfileTarget
    SecurityProfileTarget (SecurityProfileTarget'),
    newSecurityProfileTarget,

    -- ** SecurityProfileTargetMapping
    SecurityProfileTargetMapping (SecurityProfileTargetMapping'),
    newSecurityProfileTargetMapping,

    -- ** ServerCertificateSummary
    ServerCertificateSummary (ServerCertificateSummary'),
    newServerCertificateSummary,

    -- ** SigV4Authorization
    SigV4Authorization (SigV4Authorization'),
    newSigV4Authorization,

    -- ** SigningProfileParameter
    SigningProfileParameter (SigningProfileParameter'),
    newSigningProfileParameter,

    -- ** SnsAction
    SnsAction (SnsAction'),
    newSnsAction,

    -- ** SqsAction
    SqsAction (SqsAction'),
    newSqsAction,

    -- ** StartSigningJobParameter
    StartSigningJobParameter (StartSigningJobParameter'),
    newStartSigningJobParameter,

    -- ** StatisticalThreshold
    StatisticalThreshold (StatisticalThreshold'),
    newStatisticalThreshold,

    -- ** Statistics
    Statistics (Statistics'),
    newStatistics,

    -- ** StepFunctionsAction
    StepFunctionsAction (StepFunctionsAction'),
    newStepFunctionsAction,

    -- ** Stream
    Stream (Stream'),
    newStream,

    -- ** StreamFile
    StreamFile (StreamFile'),
    newStreamFile,

    -- ** StreamInfo
    StreamInfo (StreamInfo'),
    newStreamInfo,

    -- ** StreamSummary
    StreamSummary (StreamSummary'),
    newStreamSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TaskStatistics
    TaskStatistics (TaskStatistics'),
    newTaskStatistics,

    -- ** TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (TaskStatisticsForAuditCheck'),
    newTaskStatisticsForAuditCheck,

    -- ** ThingAttribute
    ThingAttribute (ThingAttribute'),
    newThingAttribute,

    -- ** ThingConnectivity
    ThingConnectivity (ThingConnectivity'),
    newThingConnectivity,

    -- ** ThingDocument
    ThingDocument (ThingDocument'),
    newThingDocument,

    -- ** ThingGroupDocument
    ThingGroupDocument (ThingGroupDocument'),
    newThingGroupDocument,

    -- ** ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration (ThingGroupIndexingConfiguration'),
    newThingGroupIndexingConfiguration,

    -- ** ThingGroupMetadata
    ThingGroupMetadata (ThingGroupMetadata'),
    newThingGroupMetadata,

    -- ** ThingGroupProperties
    ThingGroupProperties (ThingGroupProperties'),
    newThingGroupProperties,

    -- ** ThingIndexingConfiguration
    ThingIndexingConfiguration (ThingIndexingConfiguration'),
    newThingIndexingConfiguration,

    -- ** ThingTypeDefinition
    ThingTypeDefinition (ThingTypeDefinition'),
    newThingTypeDefinition,

    -- ** ThingTypeMetadata
    ThingTypeMetadata (ThingTypeMetadata'),
    newThingTypeMetadata,

    -- ** ThingTypeProperties
    ThingTypeProperties (ThingTypeProperties'),
    newThingTypeProperties,

    -- ** TimeoutConfig
    TimeoutConfig (TimeoutConfig'),
    newTimeoutConfig,

    -- ** TimestreamAction
    TimestreamAction (TimestreamAction'),
    newTimestreamAction,

    -- ** TimestreamDimension
    TimestreamDimension (TimestreamDimension'),
    newTimestreamDimension,

    -- ** TimestreamTimestamp
    TimestreamTimestamp (TimestreamTimestamp'),
    newTimestreamTimestamp,

    -- ** TlsContext
    TlsContext (TlsContext'),
    newTlsContext,

    -- ** TopicRule
    TopicRule (TopicRule'),
    newTopicRule,

    -- ** TopicRuleDestination
    TopicRuleDestination (TopicRuleDestination'),
    newTopicRuleDestination,

    -- ** TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (TopicRuleDestinationConfiguration'),
    newTopicRuleDestinationConfiguration,

    -- ** TopicRuleDestinationSummary
    TopicRuleDestinationSummary (TopicRuleDestinationSummary'),
    newTopicRuleDestinationSummary,

    -- ** TopicRuleListItem
    TopicRuleListItem (TopicRuleListItem'),
    newTopicRuleListItem,

    -- ** TopicRulePayload
    TopicRulePayload (TopicRulePayload'),
    newTopicRulePayload,

    -- ** TransferData
    TransferData (TransferData'),
    newTransferData,

    -- ** UpdateCACertificateParams
    UpdateCACertificateParams (UpdateCACertificateParams'),
    newUpdateCACertificateParams,

    -- ** UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams (UpdateDeviceCertificateParams'),
    newUpdateDeviceCertificateParams,

    -- ** ValidationError
    ValidationError (ValidationError'),
    newValidationError,

    -- ** ViolationEvent
    ViolationEvent (ViolationEvent'),
    newViolationEvent,

    -- ** ViolationEventAdditionalInfo
    ViolationEventAdditionalInfo (ViolationEventAdditionalInfo'),
    newViolationEventAdditionalInfo,

    -- ** ViolationEventOccurrenceRange
    ViolationEventOccurrenceRange (ViolationEventOccurrenceRange'),
    newViolationEventOccurrenceRange,

    -- ** VpcDestinationConfiguration
    VpcDestinationConfiguration (VpcDestinationConfiguration'),
    newVpcDestinationConfiguration,

    -- ** VpcDestinationProperties
    VpcDestinationProperties (VpcDestinationProperties'),
    newVpcDestinationProperties,

    -- ** VpcDestinationSummary
    VpcDestinationSummary (VpcDestinationSummary'),
    newVpcDestinationSummary,
  )
where

import Network.AWS.IoT.AcceptCertificateTransfer
import Network.AWS.IoT.AddThingToBillingGroup
import Network.AWS.IoT.AddThingToThingGroup
import Network.AWS.IoT.AssociateTargetsWithJob
import Network.AWS.IoT.AttachPolicy
import Network.AWS.IoT.AttachSecurityProfile
import Network.AWS.IoT.AttachThingPrincipal
import Network.AWS.IoT.CancelAuditMitigationActionsTask
import Network.AWS.IoT.CancelAuditTask
import Network.AWS.IoT.CancelCertificateTransfer
import Network.AWS.IoT.CancelDetectMitigationActionsTask
import Network.AWS.IoT.CancelJob
import Network.AWS.IoT.CancelJobExecution
import Network.AWS.IoT.ClearDefaultAuthorizer
import Network.AWS.IoT.ConfirmTopicRuleDestination
import Network.AWS.IoT.CreateAuditSuppression
import Network.AWS.IoT.CreateAuthorizer
import Network.AWS.IoT.CreateBillingGroup
import Network.AWS.IoT.CreateCertificateFromCsr
import Network.AWS.IoT.CreateCustomMetric
import Network.AWS.IoT.CreateDimension
import Network.AWS.IoT.CreateDomainConfiguration
import Network.AWS.IoT.CreateDynamicThingGroup
import Network.AWS.IoT.CreateJob
import Network.AWS.IoT.CreateKeysAndCertificate
import Network.AWS.IoT.CreateMitigationAction
import Network.AWS.IoT.CreateOTAUpdate
import Network.AWS.IoT.CreatePolicy
import Network.AWS.IoT.CreatePolicyVersion
import Network.AWS.IoT.CreateProvisioningClaim
import Network.AWS.IoT.CreateProvisioningTemplate
import Network.AWS.IoT.CreateProvisioningTemplateVersion
import Network.AWS.IoT.CreateRoleAlias
import Network.AWS.IoT.CreateScheduledAudit
import Network.AWS.IoT.CreateSecurityProfile
import Network.AWS.IoT.CreateStream
import Network.AWS.IoT.CreateThing
import Network.AWS.IoT.CreateThingGroup
import Network.AWS.IoT.CreateThingType
import Network.AWS.IoT.CreateTopicRule
import Network.AWS.IoT.CreateTopicRuleDestination
import Network.AWS.IoT.DeleteAccountAuditConfiguration
import Network.AWS.IoT.DeleteAuditSuppression
import Network.AWS.IoT.DeleteAuthorizer
import Network.AWS.IoT.DeleteBillingGroup
import Network.AWS.IoT.DeleteCACertificate
import Network.AWS.IoT.DeleteCertificate
import Network.AWS.IoT.DeleteCustomMetric
import Network.AWS.IoT.DeleteDimension
import Network.AWS.IoT.DeleteDomainConfiguration
import Network.AWS.IoT.DeleteDynamicThingGroup
import Network.AWS.IoT.DeleteJob
import Network.AWS.IoT.DeleteJobExecution
import Network.AWS.IoT.DeleteMitigationAction
import Network.AWS.IoT.DeleteOTAUpdate
import Network.AWS.IoT.DeletePolicy
import Network.AWS.IoT.DeletePolicyVersion
import Network.AWS.IoT.DeleteProvisioningTemplate
import Network.AWS.IoT.DeleteProvisioningTemplateVersion
import Network.AWS.IoT.DeleteRegistrationCode
import Network.AWS.IoT.DeleteRoleAlias
import Network.AWS.IoT.DeleteScheduledAudit
import Network.AWS.IoT.DeleteSecurityProfile
import Network.AWS.IoT.DeleteStream
import Network.AWS.IoT.DeleteThing
import Network.AWS.IoT.DeleteThingGroup
import Network.AWS.IoT.DeleteThingType
import Network.AWS.IoT.DeleteTopicRule
import Network.AWS.IoT.DeleteTopicRuleDestination
import Network.AWS.IoT.DeleteV2LoggingLevel
import Network.AWS.IoT.DeprecateThingType
import Network.AWS.IoT.DescribeAccountAuditConfiguration
import Network.AWS.IoT.DescribeAuditFinding
import Network.AWS.IoT.DescribeAuditMitigationActionsTask
import Network.AWS.IoT.DescribeAuditSuppression
import Network.AWS.IoT.DescribeAuditTask
import Network.AWS.IoT.DescribeAuthorizer
import Network.AWS.IoT.DescribeBillingGroup
import Network.AWS.IoT.DescribeCACertificate
import Network.AWS.IoT.DescribeCertificate
import Network.AWS.IoT.DescribeCustomMetric
import Network.AWS.IoT.DescribeDefaultAuthorizer
import Network.AWS.IoT.DescribeDetectMitigationActionsTask
import Network.AWS.IoT.DescribeDimension
import Network.AWS.IoT.DescribeDomainConfiguration
import Network.AWS.IoT.DescribeEndpoint
import Network.AWS.IoT.DescribeEventConfigurations
import Network.AWS.IoT.DescribeIndex
import Network.AWS.IoT.DescribeJob
import Network.AWS.IoT.DescribeJobExecution
import Network.AWS.IoT.DescribeMitigationAction
import Network.AWS.IoT.DescribeProvisioningTemplate
import Network.AWS.IoT.DescribeProvisioningTemplateVersion
import Network.AWS.IoT.DescribeRoleAlias
import Network.AWS.IoT.DescribeScheduledAudit
import Network.AWS.IoT.DescribeSecurityProfile
import Network.AWS.IoT.DescribeStream
import Network.AWS.IoT.DescribeThing
import Network.AWS.IoT.DescribeThingGroup
import Network.AWS.IoT.DescribeThingRegistrationTask
import Network.AWS.IoT.DescribeThingType
import Network.AWS.IoT.DetachPolicy
import Network.AWS.IoT.DetachSecurityProfile
import Network.AWS.IoT.DetachThingPrincipal
import Network.AWS.IoT.DisableTopicRule
import Network.AWS.IoT.EnableTopicRule
import Network.AWS.IoT.GetBehaviorModelTrainingSummaries
import Network.AWS.IoT.GetCardinality
import Network.AWS.IoT.GetEffectivePolicies
import Network.AWS.IoT.GetIndexingConfiguration
import Network.AWS.IoT.GetJobDocument
import Network.AWS.IoT.GetLoggingOptions
import Network.AWS.IoT.GetOTAUpdate
import Network.AWS.IoT.GetPercentiles
import Network.AWS.IoT.GetPolicy
import Network.AWS.IoT.GetPolicyVersion
import Network.AWS.IoT.GetRegistrationCode
import Network.AWS.IoT.GetStatistics
import Network.AWS.IoT.GetTopicRule
import Network.AWS.IoT.GetTopicRuleDestination
import Network.AWS.IoT.GetV2LoggingOptions
import Network.AWS.IoT.Lens
import Network.AWS.IoT.ListActiveViolations
import Network.AWS.IoT.ListAttachedPolicies
import Network.AWS.IoT.ListAuditFindings
import Network.AWS.IoT.ListAuditMitigationActionsExecutions
import Network.AWS.IoT.ListAuditMitigationActionsTasks
import Network.AWS.IoT.ListAuditSuppressions
import Network.AWS.IoT.ListAuditTasks
import Network.AWS.IoT.ListAuthorizers
import Network.AWS.IoT.ListBillingGroups
import Network.AWS.IoT.ListCACertificates
import Network.AWS.IoT.ListCertificates
import Network.AWS.IoT.ListCertificatesByCA
import Network.AWS.IoT.ListCustomMetrics
import Network.AWS.IoT.ListDetectMitigationActionsExecutions
import Network.AWS.IoT.ListDetectMitigationActionsTasks
import Network.AWS.IoT.ListDimensions
import Network.AWS.IoT.ListDomainConfigurations
import Network.AWS.IoT.ListIndices
import Network.AWS.IoT.ListJobExecutionsForJob
import Network.AWS.IoT.ListJobExecutionsForThing
import Network.AWS.IoT.ListJobs
import Network.AWS.IoT.ListMitigationActions
import Network.AWS.IoT.ListOTAUpdates
import Network.AWS.IoT.ListOutgoingCertificates
import Network.AWS.IoT.ListPolicies
import Network.AWS.IoT.ListPolicyVersions
import Network.AWS.IoT.ListPrincipalThings
import Network.AWS.IoT.ListProvisioningTemplateVersions
import Network.AWS.IoT.ListProvisioningTemplates
import Network.AWS.IoT.ListRoleAliases
import Network.AWS.IoT.ListScheduledAudits
import Network.AWS.IoT.ListSecurityProfiles
import Network.AWS.IoT.ListSecurityProfilesForTarget
import Network.AWS.IoT.ListStreams
import Network.AWS.IoT.ListTagsForResource
import Network.AWS.IoT.ListTargetsForPolicy
import Network.AWS.IoT.ListTargetsForSecurityProfile
import Network.AWS.IoT.ListThingGroups
import Network.AWS.IoT.ListThingGroupsForThing
import Network.AWS.IoT.ListThingPrincipals
import Network.AWS.IoT.ListThingRegistrationTaskReports
import Network.AWS.IoT.ListThingRegistrationTasks
import Network.AWS.IoT.ListThingTypes
import Network.AWS.IoT.ListThings
import Network.AWS.IoT.ListThingsInBillingGroup
import Network.AWS.IoT.ListThingsInThingGroup
import Network.AWS.IoT.ListTopicRuleDestinations
import Network.AWS.IoT.ListTopicRules
import Network.AWS.IoT.ListV2LoggingLevels
import Network.AWS.IoT.ListViolationEvents
import Network.AWS.IoT.RegisterCACertificate
import Network.AWS.IoT.RegisterCertificate
import Network.AWS.IoT.RegisterCertificateWithoutCA
import Network.AWS.IoT.RegisterThing
import Network.AWS.IoT.RejectCertificateTransfer
import Network.AWS.IoT.RemoveThingFromBillingGroup
import Network.AWS.IoT.RemoveThingFromThingGroup
import Network.AWS.IoT.ReplaceTopicRule
import Network.AWS.IoT.SearchIndex
import Network.AWS.IoT.SetDefaultAuthorizer
import Network.AWS.IoT.SetDefaultPolicyVersion
import Network.AWS.IoT.SetLoggingOptions
import Network.AWS.IoT.SetV2LoggingLevel
import Network.AWS.IoT.SetV2LoggingOptions
import Network.AWS.IoT.StartAuditMitigationActionsTask
import Network.AWS.IoT.StartDetectMitigationActionsTask
import Network.AWS.IoT.StartOnDemandAuditTask
import Network.AWS.IoT.StartThingRegistrationTask
import Network.AWS.IoT.StopThingRegistrationTask
import Network.AWS.IoT.TagResource
import Network.AWS.IoT.TestAuthorization
import Network.AWS.IoT.TestInvokeAuthorizer
import Network.AWS.IoT.TransferCertificate
import Network.AWS.IoT.Types
import Network.AWS.IoT.UntagResource
import Network.AWS.IoT.UpdateAccountAuditConfiguration
import Network.AWS.IoT.UpdateAuditSuppression
import Network.AWS.IoT.UpdateAuthorizer
import Network.AWS.IoT.UpdateBillingGroup
import Network.AWS.IoT.UpdateCACertificate
import Network.AWS.IoT.UpdateCertificate
import Network.AWS.IoT.UpdateCustomMetric
import Network.AWS.IoT.UpdateDimension
import Network.AWS.IoT.UpdateDomainConfiguration
import Network.AWS.IoT.UpdateDynamicThingGroup
import Network.AWS.IoT.UpdateEventConfigurations
import Network.AWS.IoT.UpdateIndexingConfiguration
import Network.AWS.IoT.UpdateJob
import Network.AWS.IoT.UpdateMitigationAction
import Network.AWS.IoT.UpdateProvisioningTemplate
import Network.AWS.IoT.UpdateRoleAlias
import Network.AWS.IoT.UpdateScheduledAudit
import Network.AWS.IoT.UpdateSecurityProfile
import Network.AWS.IoT.UpdateStream
import Network.AWS.IoT.UpdateThing
import Network.AWS.IoT.UpdateThingGroup
import Network.AWS.IoT.UpdateThingGroupsForThing
import Network.AWS.IoT.UpdateTopicRuleDestination
import Network.AWS.IoT.ValidateSecurityProfileBehaviors
import Network.AWS.IoT.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoT'.

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

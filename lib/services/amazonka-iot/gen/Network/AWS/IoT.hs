{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-05-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- IoT
--
-- IoT provides secure, bi-directional communication between
-- Internet-connected devices (such as sensors, actuators, embedded
-- devices, or smart appliances) and the Amazon Web Services cloud. You can
-- discover your custom IoT-Data endpoint to communicate with, configure
-- rules for data processing and integration with other services, organize
-- resources associated with each device (Registry), configure logging, and
-- create and manage policies and credentials to authenticate devices.
--
-- The service endpoints that expose this API are listed in
-- <https://docs.aws.amazon.com/general/latest/gr/iot-core.html Amazon Web Services IoT Core Endpoints and Quotas>.
-- You must use the endpoint for the region that has the resources you want
-- to access.
--
-- The service name used by
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Amazon Web Services Signature Version 4>
-- to sign the request is: /execute-api/.
--
-- For more information about how IoT works, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide>.
--
-- For information about how to use the credentials provider for IoT, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/authorizing-direct-aws.html Authorizing Direct Calls to Amazon Web Services Services>.
module Network.AWS.IoT
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TaskAlreadyExistsException
    _TaskAlreadyExistsException,

    -- ** CertificateConflictException
    _CertificateConflictException,

    -- ** SqlParseException
    _SqlParseException,

    -- ** IndexNotReadyException
    _IndexNotReadyException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** TransferConflictException
    _TransferConflictException,

    -- ** CertificateStateException
    _CertificateStateException,

    -- ** InvalidResponseException
    _InvalidResponseException,

    -- ** RegistrationCodeValidationException
    _RegistrationCodeValidationException,

    -- ** MalformedPolicyException
    _MalformedPolicyException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** NotConfiguredException
    _NotConfiguredException,

    -- ** CertificateValidationException
    _CertificateValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceRegistrationFailureException
    _ResourceRegistrationFailureException,

    -- ** InvalidQueryException
    _InvalidQueryException,

    -- ** TransferAlreadyCompletedException
    _TransferAlreadyCompletedException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidAggregationException
    _InvalidAggregationException,

    -- ** ConflictingResourceUpdateException
    _ConflictingResourceUpdateException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** VersionsLimitExceededException
    _VersionsLimitExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** InternalException
    _InternalException,

    -- ** VersionConflictException
    _VersionConflictException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCardinality
    GetCardinality (GetCardinality'),
    newGetCardinality,
    GetCardinalityResponse (GetCardinalityResponse'),
    newGetCardinalityResponse,

    -- ** CreateDomainConfiguration
    CreateDomainConfiguration (CreateDomainConfiguration'),
    newCreateDomainConfiguration,
    CreateDomainConfigurationResponse (CreateDomainConfigurationResponse'),
    newCreateDomainConfigurationResponse,

    -- ** StartDetectMitigationActionsTask
    StartDetectMitigationActionsTask (StartDetectMitigationActionsTask'),
    newStartDetectMitigationActionsTask,
    StartDetectMitigationActionsTaskResponse (StartDetectMitigationActionsTaskResponse'),
    newStartDetectMitigationActionsTaskResponse,

    -- ** DeleteSecurityProfile
    DeleteSecurityProfile (DeleteSecurityProfile'),
    newDeleteSecurityProfile,
    DeleteSecurityProfileResponse (DeleteSecurityProfileResponse'),
    newDeleteSecurityProfileResponse,

    -- ** UpdateSecurityProfile
    UpdateSecurityProfile (UpdateSecurityProfile'),
    newUpdateSecurityProfile,
    UpdateSecurityProfileResponse (UpdateSecurityProfileResponse'),
    newUpdateSecurityProfileResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** DescribeProvisioningTemplate
    DescribeProvisioningTemplate (DescribeProvisioningTemplate'),
    newDescribeProvisioningTemplate,
    DescribeProvisioningTemplateResponse (DescribeProvisioningTemplateResponse'),
    newDescribeProvisioningTemplateResponse,

    -- ** UpdateMitigationAction
    UpdateMitigationAction (UpdateMitigationAction'),
    newUpdateMitigationAction,
    UpdateMitigationActionResponse (UpdateMitigationActionResponse'),
    newUpdateMitigationActionResponse,

    -- ** DeleteMitigationAction
    DeleteMitigationAction (DeleteMitigationAction'),
    newDeleteMitigationAction,
    DeleteMitigationActionResponse (DeleteMitigationActionResponse'),
    newDeleteMitigationActionResponse,

    -- ** DeleteJobExecution
    DeleteJobExecution (DeleteJobExecution'),
    newDeleteJobExecution,
    DeleteJobExecutionResponse (DeleteJobExecutionResponse'),
    newDeleteJobExecutionResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** DeleteDynamicThingGroup
    DeleteDynamicThingGroup (DeleteDynamicThingGroup'),
    newDeleteDynamicThingGroup,
    DeleteDynamicThingGroupResponse (DeleteDynamicThingGroupResponse'),
    newDeleteDynamicThingGroupResponse,

    -- ** ListThingPrincipals (Paginated)
    ListThingPrincipals (ListThingPrincipals'),
    newListThingPrincipals,
    ListThingPrincipalsResponse (ListThingPrincipalsResponse'),
    newListThingPrincipalsResponse,

    -- ** UpdateDynamicThingGroup
    UpdateDynamicThingGroup (UpdateDynamicThingGroup'),
    newUpdateDynamicThingGroup,
    UpdateDynamicThingGroupResponse (UpdateDynamicThingGroupResponse'),
    newUpdateDynamicThingGroupResponse,

    -- ** DescribeRoleAlias
    DescribeRoleAlias (DescribeRoleAlias'),
    newDescribeRoleAlias,
    DescribeRoleAliasResponse (DescribeRoleAliasResponse'),
    newDescribeRoleAliasResponse,

    -- ** CreateProvisioningTemplateVersion
    CreateProvisioningTemplateVersion (CreateProvisioningTemplateVersion'),
    newCreateProvisioningTemplateVersion,
    CreateProvisioningTemplateVersionResponse (CreateProvisioningTemplateVersionResponse'),
    newCreateProvisioningTemplateVersionResponse,

    -- ** CreateOTAUpdate
    CreateOTAUpdate (CreateOTAUpdate'),
    newCreateOTAUpdate,
    CreateOTAUpdateResponse (CreateOTAUpdateResponse'),
    newCreateOTAUpdateResponse,

    -- ** DescribeDefaultAuthorizer
    DescribeDefaultAuthorizer (DescribeDefaultAuthorizer'),
    newDescribeDefaultAuthorizer,
    DescribeDefaultAuthorizerResponse (DescribeDefaultAuthorizerResponse'),
    newDescribeDefaultAuthorizerResponse,

    -- ** ListAuditMitigationActionsTasks (Paginated)
    ListAuditMitigationActionsTasks (ListAuditMitigationActionsTasks'),
    newListAuditMitigationActionsTasks,
    ListAuditMitigationActionsTasksResponse (ListAuditMitigationActionsTasksResponse'),
    newListAuditMitigationActionsTasksResponse,

    -- ** ListThingRegistrationTaskReports (Paginated)
    ListThingRegistrationTaskReports (ListThingRegistrationTaskReports'),
    newListThingRegistrationTaskReports,
    ListThingRegistrationTaskReportsResponse (ListThingRegistrationTaskReportsResponse'),
    newListThingRegistrationTaskReportsResponse,

    -- ** GetBehaviorModelTrainingSummaries (Paginated)
    GetBehaviorModelTrainingSummaries (GetBehaviorModelTrainingSummaries'),
    newGetBehaviorModelTrainingSummaries,
    GetBehaviorModelTrainingSummariesResponse (GetBehaviorModelTrainingSummariesResponse'),
    newGetBehaviorModelTrainingSummariesResponse,

    -- ** ListPrincipalThings (Paginated)
    ListPrincipalThings (ListPrincipalThings'),
    newListPrincipalThings,
    ListPrincipalThingsResponse (ListPrincipalThingsResponse'),
    newListPrincipalThingsResponse,

    -- ** RemoveThingFromThingGroup
    RemoveThingFromThingGroup (RemoveThingFromThingGroup'),
    newRemoveThingFromThingGroup,
    RemoveThingFromThingGroupResponse (RemoveThingFromThingGroupResponse'),
    newRemoveThingFromThingGroupResponse,

    -- ** DescribeEventConfigurations
    DescribeEventConfigurations (DescribeEventConfigurations'),
    newDescribeEventConfigurations,
    DescribeEventConfigurationsResponse (DescribeEventConfigurationsResponse'),
    newDescribeEventConfigurationsResponse,

    -- ** CancelDetectMitigationActionsTask
    CancelDetectMitigationActionsTask (CancelDetectMitigationActionsTask'),
    newCancelDetectMitigationActionsTask,
    CancelDetectMitigationActionsTaskResponse (CancelDetectMitigationActionsTaskResponse'),
    newCancelDetectMitigationActionsTaskResponse,

    -- ** ListTopicRuleDestinations (Paginated)
    ListTopicRuleDestinations (ListTopicRuleDestinations'),
    newListTopicRuleDestinations,
    ListTopicRuleDestinationsResponse (ListTopicRuleDestinationsResponse'),
    newListTopicRuleDestinationsResponse,

    -- ** RegisterCertificateWithoutCA
    RegisterCertificateWithoutCA (RegisterCertificateWithoutCA'),
    newRegisterCertificateWithoutCA,
    RegisterCertificateWithoutCAResponse (RegisterCertificateWithoutCAResponse'),
    newRegisterCertificateWithoutCAResponse,

    -- ** DescribeCustomMetric
    DescribeCustomMetric (DescribeCustomMetric'),
    newDescribeCustomMetric,
    DescribeCustomMetricResponse (DescribeCustomMetricResponse'),
    newDescribeCustomMetricResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListThingGroups (Paginated)
    ListThingGroups (ListThingGroups'),
    newListThingGroups,
    ListThingGroupsResponse (ListThingGroupsResponse'),
    newListThingGroupsResponse,

    -- ** DescribeJobTemplate
    DescribeJobTemplate (DescribeJobTemplate'),
    newDescribeJobTemplate,
    DescribeJobTemplateResponse (DescribeJobTemplateResponse'),
    newDescribeJobTemplateResponse,

    -- ** ListScheduledAudits (Paginated)
    ListScheduledAudits (ListScheduledAudits'),
    newListScheduledAudits,
    ListScheduledAuditsResponse (ListScheduledAuditsResponse'),
    newListScheduledAuditsResponse,

    -- ** DescribeThingRegistrationTask
    DescribeThingRegistrationTask (DescribeThingRegistrationTask'),
    newDescribeThingRegistrationTask,
    DescribeThingRegistrationTaskResponse (DescribeThingRegistrationTaskResponse'),
    newDescribeThingRegistrationTaskResponse,

    -- ** UpdateScheduledAudit
    UpdateScheduledAudit (UpdateScheduledAudit'),
    newUpdateScheduledAudit,
    UpdateScheduledAuditResponse (UpdateScheduledAuditResponse'),
    newUpdateScheduledAuditResponse,

    -- ** DeleteScheduledAudit
    DeleteScheduledAudit (DeleteScheduledAudit'),
    newDeleteScheduledAudit,
    DeleteScheduledAuditResponse (DeleteScheduledAuditResponse'),
    newDeleteScheduledAuditResponse,

    -- ** DescribeAuditFinding
    DescribeAuditFinding (DescribeAuditFinding'),
    newDescribeAuditFinding,
    DescribeAuditFindingResponse (DescribeAuditFindingResponse'),
    newDescribeAuditFindingResponse,

    -- ** DescribeDimension
    DescribeDimension (DescribeDimension'),
    newDescribeDimension,
    DescribeDimensionResponse (DescribeDimensionResponse'),
    newDescribeDimensionResponse,

    -- ** GetLoggingOptions
    GetLoggingOptions (GetLoggingOptions'),
    newGetLoggingOptions,
    GetLoggingOptionsResponse (GetLoggingOptionsResponse'),
    newGetLoggingOptionsResponse,

    -- ** DeleteAccountAuditConfiguration
    DeleteAccountAuditConfiguration (DeleteAccountAuditConfiguration'),
    newDeleteAccountAuditConfiguration,
    DeleteAccountAuditConfigurationResponse (DeleteAccountAuditConfigurationResponse'),
    newDeleteAccountAuditConfigurationResponse,

    -- ** UpdateAccountAuditConfiguration
    UpdateAccountAuditConfiguration (UpdateAccountAuditConfiguration'),
    newUpdateAccountAuditConfiguration,
    UpdateAccountAuditConfigurationResponse (UpdateAccountAuditConfigurationResponse'),
    newUpdateAccountAuditConfigurationResponse,

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

    -- ** ListThingTypes (Paginated)
    ListThingTypes (ListThingTypes'),
    newListThingTypes,
    ListThingTypesResponse (ListThingTypesResponse'),
    newListThingTypesResponse,

    -- ** SetV2LoggingOptions
    SetV2LoggingOptions (SetV2LoggingOptions'),
    newSetV2LoggingOptions,
    SetV2LoggingOptionsResponse (SetV2LoggingOptionsResponse'),
    newSetV2LoggingOptionsResponse,

    -- ** CreateProvisioningTemplate
    CreateProvisioningTemplate (CreateProvisioningTemplate'),
    newCreateProvisioningTemplate,
    CreateProvisioningTemplateResponse (CreateProvisioningTemplateResponse'),
    newCreateProvisioningTemplateResponse,

    -- ** ListThingGroupsForThing (Paginated)
    ListThingGroupsForThing (ListThingGroupsForThing'),
    newListThingGroupsForThing,
    ListThingGroupsForThingResponse (ListThingGroupsForThingResponse'),
    newListThingGroupsForThingResponse,

    -- ** CreateCertificateFromCsr
    CreateCertificateFromCsr (CreateCertificateFromCsr'),
    newCreateCertificateFromCsr,
    CreateCertificateFromCsrResponse (CreateCertificateFromCsrResponse'),
    newCreateCertificateFromCsrResponse,

    -- ** DeleteThing
    DeleteThing (DeleteThing'),
    newDeleteThing,
    DeleteThingResponse (DeleteThingResponse'),
    newDeleteThingResponse,

    -- ** UpdateThing
    UpdateThing (UpdateThing'),
    newUpdateThing,
    UpdateThingResponse (UpdateThingResponse'),
    newUpdateThingResponse,

    -- ** DeleteProvisioningTemplate
    DeleteProvisioningTemplate (DeleteProvisioningTemplate'),
    newDeleteProvisioningTemplate,
    DeleteProvisioningTemplateResponse (DeleteProvisioningTemplateResponse'),
    newDeleteProvisioningTemplateResponse,

    -- ** UpdateProvisioningTemplate
    UpdateProvisioningTemplate (UpdateProvisioningTemplate'),
    newUpdateProvisioningTemplate,
    UpdateProvisioningTemplateResponse (UpdateProvisioningTemplateResponse'),
    newUpdateProvisioningTemplateResponse,

    -- ** DescribeMitigationAction
    DescribeMitigationAction (DescribeMitigationAction'),
    newDescribeMitigationAction,
    DescribeMitigationActionResponse (DescribeMitigationActionResponse'),
    newDescribeMitigationActionResponse,

    -- ** StartThingRegistrationTask
    StartThingRegistrationTask (StartThingRegistrationTask'),
    newStartThingRegistrationTask,
    StartThingRegistrationTaskResponse (StartThingRegistrationTaskResponse'),
    newStartThingRegistrationTaskResponse,

    -- ** CreateScheduledAudit
    CreateScheduledAudit (CreateScheduledAudit'),
    newCreateScheduledAudit,
    CreateScheduledAuditResponse (CreateScheduledAuditResponse'),
    newCreateScheduledAuditResponse,

    -- ** ListAuthorizers (Paginated)
    ListAuthorizers (ListAuthorizers'),
    newListAuthorizers,
    ListAuthorizersResponse (ListAuthorizersResponse'),
    newListAuthorizersResponse,

    -- ** ListJobExecutionsForJob (Paginated)
    ListJobExecutionsForJob (ListJobExecutionsForJob'),
    newListJobExecutionsForJob,
    ListJobExecutionsForJobResponse (ListJobExecutionsForJobResponse'),
    newListJobExecutionsForJobResponse,

    -- ** RemoveThingFromBillingGroup
    RemoveThingFromBillingGroup (RemoveThingFromBillingGroup'),
    newRemoveThingFromBillingGroup,
    RemoveThingFromBillingGroupResponse (RemoveThingFromBillingGroupResponse'),
    newRemoveThingFromBillingGroupResponse,

    -- ** SearchIndex
    SearchIndex (SearchIndex'),
    newSearchIndex,
    SearchIndexResponse (SearchIndexResponse'),
    newSearchIndexResponse,

    -- ** CreateThingType
    CreateThingType (CreateThingType'),
    newCreateThingType,
    CreateThingTypeResponse (CreateThingTypeResponse'),
    newCreateThingTypeResponse,

    -- ** DescribeSecurityProfile
    DescribeSecurityProfile (DescribeSecurityProfile'),
    newDescribeSecurityProfile,
    DescribeSecurityProfileResponse (DescribeSecurityProfileResponse'),
    newDescribeSecurityProfileResponse,

    -- ** DeleteV2LoggingLevel
    DeleteV2LoggingLevel (DeleteV2LoggingLevel'),
    newDeleteV2LoggingLevel,
    DeleteV2LoggingLevelResponse (DeleteV2LoggingLevelResponse'),
    newDeleteV2LoggingLevelResponse,

    -- ** SetDefaultAuthorizer
    SetDefaultAuthorizer (SetDefaultAuthorizer'),
    newSetDefaultAuthorizer,
    SetDefaultAuthorizerResponse (SetDefaultAuthorizerResponse'),
    newSetDefaultAuthorizerResponse,

    -- ** DescribeJobExecution
    DescribeJobExecution (DescribeJobExecution'),
    newDescribeJobExecution,
    DescribeJobExecutionResponse (DescribeJobExecutionResponse'),
    newDescribeJobExecutionResponse,

    -- ** CancelCertificateTransfer
    CancelCertificateTransfer (CancelCertificateTransfer'),
    newCancelCertificateTransfer,
    CancelCertificateTransferResponse (CancelCertificateTransferResponse'),
    newCancelCertificateTransferResponse,

    -- ** GetIndexingConfiguration
    GetIndexingConfiguration (GetIndexingConfiguration'),
    newGetIndexingConfiguration,
    GetIndexingConfigurationResponse (GetIndexingConfigurationResponse'),
    newGetIndexingConfigurationResponse,

    -- ** ListAuditMitigationActionsExecutions (Paginated)
    ListAuditMitigationActionsExecutions (ListAuditMitigationActionsExecutions'),
    newListAuditMitigationActionsExecutions,
    ListAuditMitigationActionsExecutionsResponse (ListAuditMitigationActionsExecutionsResponse'),
    newListAuditMitigationActionsExecutionsResponse,

    -- ** CreateCustomMetric
    CreateCustomMetric (CreateCustomMetric'),
    newCreateCustomMetric,
    CreateCustomMetricResponse (CreateCustomMetricResponse'),
    newCreateCustomMetricResponse,

    -- ** DescribeAuditMitigationActionsTask
    DescribeAuditMitigationActionsTask (DescribeAuditMitigationActionsTask'),
    newDescribeAuditMitigationActionsTask,
    DescribeAuditMitigationActionsTaskResponse (DescribeAuditMitigationActionsTaskResponse'),
    newDescribeAuditMitigationActionsTaskResponse,

    -- ** GetStatistics
    GetStatistics (GetStatistics'),
    newGetStatistics,
    GetStatisticsResponse (GetStatisticsResponse'),
    newGetStatisticsResponse,

    -- ** DeleteRoleAlias
    DeleteRoleAlias (DeleteRoleAlias'),
    newDeleteRoleAlias,
    DeleteRoleAliasResponse (DeleteRoleAliasResponse'),
    newDeleteRoleAliasResponse,

    -- ** UpdateRoleAlias
    UpdateRoleAlias (UpdateRoleAlias'),
    newUpdateRoleAlias,
    UpdateRoleAliasResponse (UpdateRoleAliasResponse'),
    newUpdateRoleAliasResponse,

    -- ** ListFleetMetrics (Paginated)
    ListFleetMetrics (ListFleetMetrics'),
    newListFleetMetrics,
    ListFleetMetricsResponse (ListFleetMetricsResponse'),
    newListFleetMetricsResponse,

    -- ** DeletePolicyVersion
    DeletePolicyVersion (DeletePolicyVersion'),
    newDeletePolicyVersion,
    DeletePolicyVersionResponse (DeletePolicyVersionResponse'),
    newDeletePolicyVersionResponse,

    -- ** DisableTopicRule
    DisableTopicRule (DisableTopicRule'),
    newDisableTopicRule,
    DisableTopicRuleResponse (DisableTopicRuleResponse'),
    newDisableTopicRuleResponse,

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

    -- ** DescribeIndex
    DescribeIndex (DescribeIndex'),
    newDescribeIndex,
    DescribeIndexResponse (DescribeIndexResponse'),
    newDescribeIndexResponse,

    -- ** AssociateTargetsWithJob
    AssociateTargetsWithJob (AssociateTargetsWithJob'),
    newAssociateTargetsWithJob,
    AssociateTargetsWithJobResponse (AssociateTargetsWithJobResponse'),
    newAssociateTargetsWithJobResponse,

    -- ** AttachSecurityProfile
    AttachSecurityProfile (AttachSecurityProfile'),
    newAttachSecurityProfile,
    AttachSecurityProfileResponse (AttachSecurityProfileResponse'),
    newAttachSecurityProfileResponse,

    -- ** ListAttachedPolicies (Paginated)
    ListAttachedPolicies (ListAttachedPolicies'),
    newListAttachedPolicies,
    ListAttachedPoliciesResponse (ListAttachedPoliciesResponse'),
    newListAttachedPoliciesResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** ListCACertificates (Paginated)
    ListCACertificates (ListCACertificates'),
    newListCACertificates,
    ListCACertificatesResponse (ListCACertificatesResponse'),
    newListCACertificatesResponse,

    -- ** DeleteTopicRule
    DeleteTopicRule (DeleteTopicRule'),
    newDeleteTopicRule,
    DeleteTopicRuleResponse (DeleteTopicRuleResponse'),
    newDeleteTopicRuleResponse,

    -- ** GetJobDocument
    GetJobDocument (GetJobDocument'),
    newGetJobDocument,
    GetJobDocumentResponse (GetJobDocumentResponse'),
    newGetJobDocumentResponse,

    -- ** DescribeProvisioningTemplateVersion
    DescribeProvisioningTemplateVersion (DescribeProvisioningTemplateVersion'),
    newDescribeProvisioningTemplateVersion,
    DescribeProvisioningTemplateVersionResponse (DescribeProvisioningTemplateVersionResponse'),
    newDescribeProvisioningTemplateVersionResponse,

    -- ** ListCustomMetrics (Paginated)
    ListCustomMetrics (ListCustomMetrics'),
    newListCustomMetrics,
    ListCustomMetricsResponse (ListCustomMetricsResponse'),
    newListCustomMetricsResponse,

    -- ** CancelAuditTask
    CancelAuditTask (CancelAuditTask'),
    newCancelAuditTask,
    CancelAuditTaskResponse (CancelAuditTaskResponse'),
    newCancelAuditTaskResponse,

    -- ** CreateRoleAlias
    CreateRoleAlias (CreateRoleAlias'),
    newCreateRoleAlias,
    CreateRoleAliasResponse (CreateRoleAliasResponse'),
    newCreateRoleAliasResponse,

    -- ** DeleteCACertificate
    DeleteCACertificate (DeleteCACertificate'),
    newDeleteCACertificate,
    DeleteCACertificateResponse (DeleteCACertificateResponse'),
    newDeleteCACertificateResponse,

    -- ** UpdateCACertificate
    UpdateCACertificate (UpdateCACertificate'),
    newUpdateCACertificate,
    UpdateCACertificateResponse (UpdateCACertificateResponse'),
    newUpdateCACertificateResponse,

    -- ** ListTopicRules (Paginated)
    ListTopicRules (ListTopicRules'),
    newListTopicRules,
    ListTopicRulesResponse (ListTopicRulesResponse'),
    newListTopicRulesResponse,

    -- ** TransferCertificate
    TransferCertificate (TransferCertificate'),
    newTransferCertificate,
    TransferCertificateResponse (TransferCertificateResponse'),
    newTransferCertificateResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListRoleAliases (Paginated)
    ListRoleAliases (ListRoleAliases'),
    newListRoleAliases,
    ListRoleAliasesResponse (ListRoleAliasesResponse'),
    newListRoleAliasesResponse,

    -- ** StartOnDemandAuditTask
    StartOnDemandAuditTask (StartOnDemandAuditTask'),
    newStartOnDemandAuditTask,
    StartOnDemandAuditTaskResponse (StartOnDemandAuditTaskResponse'),
    newStartOnDemandAuditTaskResponse,

    -- ** DescribeThingGroup
    DescribeThingGroup (DescribeThingGroup'),
    newDescribeThingGroup,
    DescribeThingGroupResponse (DescribeThingGroupResponse'),
    newDescribeThingGroupResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** ListTargetsForSecurityProfile (Paginated)
    ListTargetsForSecurityProfile (ListTargetsForSecurityProfile'),
    newListTargetsForSecurityProfile,
    ListTargetsForSecurityProfileResponse (ListTargetsForSecurityProfileResponse'),
    newListTargetsForSecurityProfileResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** StartAuditMitigationActionsTask
    StartAuditMitigationActionsTask (StartAuditMitigationActionsTask'),
    newStartAuditMitigationActionsTask,
    StartAuditMitigationActionsTaskResponse (StartAuditMitigationActionsTaskResponse'),
    newStartAuditMitigationActionsTaskResponse,

    -- ** DescribeDetectMitigationActionsTask
    DescribeDetectMitigationActionsTask (DescribeDetectMitigationActionsTask'),
    newDescribeDetectMitigationActionsTask,
    DescribeDetectMitigationActionsTaskResponse (DescribeDetectMitigationActionsTaskResponse'),
    newDescribeDetectMitigationActionsTaskResponse,

    -- ** GetTopicRule
    GetTopicRule (GetTopicRule'),
    newGetTopicRule,
    GetTopicRuleResponse (GetTopicRuleResponse'),
    newGetTopicRuleResponse,

    -- ** DescribeThing
    DescribeThing (DescribeThing'),
    newDescribeThing,
    DescribeThingResponse (DescribeThingResponse'),
    newDescribeThingResponse,

    -- ** ListDomainConfigurations (Paginated)
    ListDomainConfigurations (ListDomainConfigurations'),
    newListDomainConfigurations,
    ListDomainConfigurationsResponse (ListDomainConfigurationsResponse'),
    newListDomainConfigurationsResponse,

    -- ** ListAuditTasks (Paginated)
    ListAuditTasks (ListAuditTasks'),
    newListAuditTasks,
    ListAuditTasksResponse (ListAuditTasksResponse'),
    newListAuditTasksResponse,

    -- ** DescribeAccountAuditConfiguration
    DescribeAccountAuditConfiguration (DescribeAccountAuditConfiguration'),
    newDescribeAccountAuditConfiguration,
    DescribeAccountAuditConfigurationResponse (DescribeAccountAuditConfigurationResponse'),
    newDescribeAccountAuditConfigurationResponse,

    -- ** DeleteDimension
    DeleteDimension (DeleteDimension'),
    newDeleteDimension,
    DeleteDimensionResponse (DeleteDimensionResponse'),
    newDeleteDimensionResponse,

    -- ** UpdateDimension
    UpdateDimension (UpdateDimension'),
    newUpdateDimension,
    UpdateDimensionResponse (UpdateDimensionResponse'),
    newUpdateDimensionResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** ListThingsInThingGroup (Paginated)
    ListThingsInThingGroup (ListThingsInThingGroup'),
    newListThingsInThingGroup,
    ListThingsInThingGroupResponse (ListThingsInThingGroupResponse'),
    newListThingsInThingGroupResponse,

    -- ** ListAuditFindings (Paginated)
    ListAuditFindings (ListAuditFindings'),
    newListAuditFindings,
    ListAuditFindingsResponse (ListAuditFindingsResponse'),
    newListAuditFindingsResponse,

    -- ** DescribeScheduledAudit
    DescribeScheduledAudit (DescribeScheduledAudit'),
    newDescribeScheduledAudit,
    DescribeScheduledAuditResponse (DescribeScheduledAuditResponse'),
    newDescribeScheduledAuditResponse,

    -- ** CreateMitigationAction
    CreateMitigationAction (CreateMitigationAction'),
    newCreateMitigationAction,
    CreateMitigationActionResponse (CreateMitigationActionResponse'),
    newCreateMitigationActionResponse,

    -- ** ConfirmTopicRuleDestination
    ConfirmTopicRuleDestination (ConfirmTopicRuleDestination'),
    newConfirmTopicRuleDestination,
    ConfirmTopicRuleDestinationResponse (ConfirmTopicRuleDestinationResponse'),
    newConfirmTopicRuleDestinationResponse,

    -- ** ListCertificates (Paginated)
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** ListMitigationActions (Paginated)
    ListMitigationActions (ListMitigationActions'),
    newListMitigationActions,
    ListMitigationActionsResponse (ListMitigationActionsResponse'),
    newListMitigationActionsResponse,

    -- ** DescribeAuthorizer
    DescribeAuthorizer (DescribeAuthorizer'),
    newDescribeAuthorizer,
    DescribeAuthorizerResponse (DescribeAuthorizerResponse'),
    newDescribeAuthorizerResponse,

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** ListActiveViolations (Paginated)
    ListActiveViolations (ListActiveViolations'),
    newListActiveViolations,
    ListActiveViolationsResponse (ListActiveViolationsResponse'),
    newListActiveViolationsResponse,

    -- ** ValidateSecurityProfileBehaviors
    ValidateSecurityProfileBehaviors (ValidateSecurityProfileBehaviors'),
    newValidateSecurityProfileBehaviors,
    ValidateSecurityProfileBehaviorsResponse (ValidateSecurityProfileBehaviorsResponse'),
    newValidateSecurityProfileBehaviorsResponse,

    -- ** ListViolationEvents (Paginated)
    ListViolationEvents (ListViolationEvents'),
    newListViolationEvents,
    ListViolationEventsResponse (ListViolationEventsResponse'),
    newListViolationEventsResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** UpdateCertificate
    UpdateCertificate (UpdateCertificate'),
    newUpdateCertificate,
    UpdateCertificateResponse (UpdateCertificateResponse'),
    newUpdateCertificateResponse,

    -- ** CreateDimension
    CreateDimension (CreateDimension'),
    newCreateDimension,
    CreateDimensionResponse (CreateDimensionResponse'),
    newCreateDimensionResponse,

    -- ** UpdateIndexingConfiguration
    UpdateIndexingConfiguration (UpdateIndexingConfiguration'),
    newUpdateIndexingConfiguration,
    UpdateIndexingConfigurationResponse (UpdateIndexingConfigurationResponse'),
    newUpdateIndexingConfigurationResponse,

    -- ** GetBucketsAggregation
    GetBucketsAggregation (GetBucketsAggregation'),
    newGetBucketsAggregation,
    GetBucketsAggregationResponse (GetBucketsAggregationResponse'),
    newGetBucketsAggregationResponse,

    -- ** CreateProvisioningClaim
    CreateProvisioningClaim (CreateProvisioningClaim'),
    newCreateProvisioningClaim,
    CreateProvisioningClaimResponse (CreateProvisioningClaimResponse'),
    newCreateProvisioningClaimResponse,

    -- ** TestInvokeAuthorizer
    TestInvokeAuthorizer (TestInvokeAuthorizer'),
    newTestInvokeAuthorizer,
    TestInvokeAuthorizerResponse (TestInvokeAuthorizerResponse'),
    newTestInvokeAuthorizerResponse,

    -- ** PutVerificationStateOnViolation
    PutVerificationStateOnViolation (PutVerificationStateOnViolation'),
    newPutVerificationStateOnViolation,
    PutVerificationStateOnViolationResponse (PutVerificationStateOnViolationResponse'),
    newPutVerificationStateOnViolationResponse,

    -- ** CreateThingGroup
    CreateThingGroup (CreateThingGroup'),
    newCreateThingGroup,
    CreateThingGroupResponse (CreateThingGroupResponse'),
    newCreateThingGroupResponse,

    -- ** DescribeFleetMetric
    DescribeFleetMetric (DescribeFleetMetric'),
    newDescribeFleetMetric,
    DescribeFleetMetricResponse (DescribeFleetMetricResponse'),
    newDescribeFleetMetricResponse,

    -- ** CreateTopicRuleDestination
    CreateTopicRuleDestination (CreateTopicRuleDestination'),
    newCreateTopicRuleDestination,
    CreateTopicRuleDestinationResponse (CreateTopicRuleDestinationResponse'),
    newCreateTopicRuleDestinationResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** AddThingToBillingGroup
    AddThingToBillingGroup (AddThingToBillingGroup'),
    newAddThingToBillingGroup,
    AddThingToBillingGroupResponse (AddThingToBillingGroupResponse'),
    newAddThingToBillingGroupResponse,

    -- ** UpdateTopicRuleDestination
    UpdateTopicRuleDestination (UpdateTopicRuleDestination'),
    newUpdateTopicRuleDestination,
    UpdateTopicRuleDestinationResponse (UpdateTopicRuleDestinationResponse'),
    newUpdateTopicRuleDestinationResponse,

    -- ** DeleteTopicRuleDestination
    DeleteTopicRuleDestination (DeleteTopicRuleDestination'),
    newDeleteTopicRuleDestination,
    DeleteTopicRuleDestinationResponse (DeleteTopicRuleDestinationResponse'),
    newDeleteTopicRuleDestinationResponse,

    -- ** DeleteThingGroup
    DeleteThingGroup (DeleteThingGroup'),
    newDeleteThingGroup,
    DeleteThingGroupResponse (DeleteThingGroupResponse'),
    newDeleteThingGroupResponse,

    -- ** UpdateThingGroup
    UpdateThingGroup (UpdateThingGroup'),
    newUpdateThingGroup,
    UpdateThingGroupResponse (UpdateThingGroupResponse'),
    newUpdateThingGroupResponse,

    -- ** ListOTAUpdates (Paginated)
    ListOTAUpdates (ListOTAUpdates'),
    newListOTAUpdates,
    ListOTAUpdatesResponse (ListOTAUpdatesResponse'),
    newListOTAUpdatesResponse,

    -- ** DeleteOTAUpdate
    DeleteOTAUpdate (DeleteOTAUpdate'),
    newDeleteOTAUpdate,
    DeleteOTAUpdateResponse (DeleteOTAUpdateResponse'),
    newDeleteOTAUpdateResponse,

    -- ** CreateDynamicThingGroup
    CreateDynamicThingGroup (CreateDynamicThingGroup'),
    newCreateDynamicThingGroup,
    CreateDynamicThingGroupResponse (CreateDynamicThingGroupResponse'),
    newCreateDynamicThingGroupResponse,

    -- ** DetachSecurityProfile
    DetachSecurityProfile (DetachSecurityProfile'),
    newDetachSecurityProfile,
    DetachSecurityProfileResponse (DetachSecurityProfileResponse'),
    newDetachSecurityProfileResponse,

    -- ** ListOutgoingCertificates (Paginated)
    ListOutgoingCertificates (ListOutgoingCertificates'),
    newListOutgoingCertificates,
    ListOutgoingCertificatesResponse (ListOutgoingCertificatesResponse'),
    newListOutgoingCertificatesResponse,

    -- ** DeleteProvisioningTemplateVersion
    DeleteProvisioningTemplateVersion (DeleteProvisioningTemplateVersion'),
    newDeleteProvisioningTemplateVersion,
    DeleteProvisioningTemplateVersionResponse (DeleteProvisioningTemplateVersionResponse'),
    newDeleteProvisioningTemplateVersionResponse,

    -- ** DescribeCACertificate
    DescribeCACertificate (DescribeCACertificate'),
    newDescribeCACertificate,
    DescribeCACertificateResponse (DescribeCACertificateResponse'),
    newDescribeCACertificateResponse,

    -- ** ListProvisioningTemplateVersions (Paginated)
    ListProvisioningTemplateVersions (ListProvisioningTemplateVersions'),
    newListProvisioningTemplateVersions,
    ListProvisioningTemplateVersionsResponse (ListProvisioningTemplateVersionsResponse'),
    newListProvisioningTemplateVersionsResponse,

    -- ** GetRegistrationCode
    GetRegistrationCode (GetRegistrationCode'),
    newGetRegistrationCode,
    GetRegistrationCodeResponse (GetRegistrationCodeResponse'),
    newGetRegistrationCodeResponse,

    -- ** ListDetectMitigationActionsExecutions (Paginated)
    ListDetectMitigationActionsExecutions (ListDetectMitigationActionsExecutions'),
    newListDetectMitigationActionsExecutions,
    ListDetectMitigationActionsExecutionsResponse (ListDetectMitigationActionsExecutionsResponse'),
    newListDetectMitigationActionsExecutionsResponse,

    -- ** ListBillingGroups (Paginated)
    ListBillingGroups (ListBillingGroups'),
    newListBillingGroups,
    ListBillingGroupsResponse (ListBillingGroupsResponse'),
    newListBillingGroupsResponse,

    -- ** DeleteThingType
    DeleteThingType (DeleteThingType'),
    newDeleteThingType,
    DeleteThingTypeResponse (DeleteThingTypeResponse'),
    newDeleteThingTypeResponse,

    -- ** DeleteBillingGroup
    DeleteBillingGroup (DeleteBillingGroup'),
    newDeleteBillingGroup,
    DeleteBillingGroupResponse (DeleteBillingGroupResponse'),
    newDeleteBillingGroupResponse,

    -- ** AddThingToThingGroup
    AddThingToThingGroup (AddThingToThingGroup'),
    newAddThingToThingGroup,
    AddThingToThingGroupResponse (AddThingToThingGroupResponse'),
    newAddThingToThingGroupResponse,

    -- ** UpdateBillingGroup
    UpdateBillingGroup (UpdateBillingGroup'),
    newUpdateBillingGroup,
    UpdateBillingGroupResponse (UpdateBillingGroupResponse'),
    newUpdateBillingGroupResponse,

    -- ** GetTopicRuleDestination
    GetTopicRuleDestination (GetTopicRuleDestination'),
    newGetTopicRuleDestination,
    GetTopicRuleDestinationResponse (GetTopicRuleDestinationResponse'),
    newGetTopicRuleDestinationResponse,

    -- ** ListCertificatesByCA (Paginated)
    ListCertificatesByCA (ListCertificatesByCA'),
    newListCertificatesByCA,
    ListCertificatesByCAResponse (ListCertificatesByCAResponse'),
    newListCertificatesByCAResponse,

    -- ** UpdateAuditSuppression
    UpdateAuditSuppression (UpdateAuditSuppression'),
    newUpdateAuditSuppression,
    UpdateAuditSuppressionResponse (UpdateAuditSuppressionResponse'),
    newUpdateAuditSuppressionResponse,

    -- ** AttachThingPrincipal
    AttachThingPrincipal (AttachThingPrincipal'),
    newAttachThingPrincipal,
    AttachThingPrincipalResponse (AttachThingPrincipalResponse'),
    newAttachThingPrincipalResponse,

    -- ** ListThings (Paginated)
    ListThings (ListThings'),
    newListThings,
    ListThingsResponse (ListThingsResponse'),
    newListThingsResponse,

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

    -- ** RegisterThing
    RegisterThing (RegisterThing'),
    newRegisterThing,
    RegisterThingResponse (RegisterThingResponse'),
    newRegisterThingResponse,

    -- ** ListAuditSuppressions (Paginated)
    ListAuditSuppressions (ListAuditSuppressions'),
    newListAuditSuppressions,
    ListAuditSuppressionsResponse (ListAuditSuppressionsResponse'),
    newListAuditSuppressionsResponse,

    -- ** DescribeDomainConfiguration
    DescribeDomainConfiguration (DescribeDomainConfiguration'),
    newDescribeDomainConfiguration,
    DescribeDomainConfigurationResponse (DescribeDomainConfigurationResponse'),
    newDescribeDomainConfigurationResponse,

    -- ** DescribeAuditTask
    DescribeAuditTask (DescribeAuditTask'),
    newDescribeAuditTask,
    DescribeAuditTaskResponse (DescribeAuditTaskResponse'),
    newDescribeAuditTaskResponse,

    -- ** DeleteRegistrationCode
    DeleteRegistrationCode (DeleteRegistrationCode'),
    newDeleteRegistrationCode,
    DeleteRegistrationCodeResponse (DeleteRegistrationCodeResponse'),
    newDeleteRegistrationCodeResponse,

    -- ** UpdateStream
    UpdateStream (UpdateStream'),
    newUpdateStream,
    UpdateStreamResponse (UpdateStreamResponse'),
    newUpdateStreamResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    CreateAuthorizerResponse (CreateAuthorizerResponse'),
    newCreateAuthorizerResponse,

    -- ** TestAuthorization
    TestAuthorization (TestAuthorization'),
    newTestAuthorization,
    TestAuthorizationResponse (TestAuthorizationResponse'),
    newTestAuthorizationResponse,

    -- ** ListIndices (Paginated)
    ListIndices (ListIndices'),
    newListIndices,
    ListIndicesResponse (ListIndicesResponse'),
    newListIndicesResponse,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    UpdateAuthorizerResponse (UpdateAuthorizerResponse'),
    newUpdateAuthorizerResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** CreateThing
    CreateThing (CreateThing'),
    newCreateThing,
    CreateThingResponse (CreateThingResponse'),
    newCreateThingResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** CancelAuditMitigationActionsTask
    CancelAuditMitigationActionsTask (CancelAuditMitigationActionsTask'),
    newCancelAuditMitigationActionsTask,
    CancelAuditMitigationActionsTaskResponse (CancelAuditMitigationActionsTaskResponse'),
    newCancelAuditMitigationActionsTaskResponse,

    -- ** CreateAuditSuppression
    CreateAuditSuppression (CreateAuditSuppression'),
    newCreateAuditSuppression,
    CreateAuditSuppressionResponse (CreateAuditSuppressionResponse'),
    newCreateAuditSuppressionResponse,

    -- ** CreateBillingGroup
    CreateBillingGroup (CreateBillingGroup'),
    newCreateBillingGroup,
    CreateBillingGroupResponse (CreateBillingGroupResponse'),
    newCreateBillingGroupResponse,

    -- ** ListProvisioningTemplates (Paginated)
    ListProvisioningTemplates (ListProvisioningTemplates'),
    newListProvisioningTemplates,
    ListProvisioningTemplatesResponse (ListProvisioningTemplatesResponse'),
    newListProvisioningTemplatesResponse,

    -- ** ListV2LoggingLevels (Paginated)
    ListV2LoggingLevels (ListV2LoggingLevels'),
    newListV2LoggingLevels,
    ListV2LoggingLevelsResponse (ListV2LoggingLevelsResponse'),
    newListV2LoggingLevelsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** StopThingRegistrationTask
    StopThingRegistrationTask (StopThingRegistrationTask'),
    newStopThingRegistrationTask,
    StopThingRegistrationTaskResponse (StopThingRegistrationTaskResponse'),
    newStopThingRegistrationTaskResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

    -- ** CreateJobTemplate
    CreateJobTemplate (CreateJobTemplate'),
    newCreateJobTemplate,
    CreateJobTemplateResponse (CreateJobTemplateResponse'),
    newCreateJobTemplateResponse,

    -- ** ClearDefaultAuthorizer
    ClearDefaultAuthorizer (ClearDefaultAuthorizer'),
    newClearDefaultAuthorizer,
    ClearDefaultAuthorizerResponse (ClearDefaultAuthorizerResponse'),
    newClearDefaultAuthorizerResponse,

    -- ** ReplaceTopicRule
    ReplaceTopicRule (ReplaceTopicRule'),
    newReplaceTopicRule,
    ReplaceTopicRuleResponse (ReplaceTopicRuleResponse'),
    newReplaceTopicRuleResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteFleetMetric
    DeleteFleetMetric (DeleteFleetMetric'),
    newDeleteFleetMetric,
    DeleteFleetMetricResponse (DeleteFleetMetricResponse'),
    newDeleteFleetMetricResponse,

    -- ** UpdateFleetMetric
    UpdateFleetMetric (UpdateFleetMetric'),
    newUpdateFleetMetric,
    UpdateFleetMetricResponse (UpdateFleetMetricResponse'),
    newUpdateFleetMetricResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** CancelJobExecution
    CancelJobExecution (CancelJobExecution'),
    newCancelJobExecution,
    CancelJobExecutionResponse (CancelJobExecutionResponse'),
    newCancelJobExecutionResponse,

    -- ** ListPolicyVersions
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

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

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** CreateKeysAndCertificate
    CreateKeysAndCertificate (CreateKeysAndCertificate'),
    newCreateKeysAndCertificate,
    CreateKeysAndCertificateResponse (CreateKeysAndCertificateResponse'),
    newCreateKeysAndCertificateResponse,

    -- ** ListThingsInBillingGroup (Paginated)
    ListThingsInBillingGroup (ListThingsInBillingGroup'),
    newListThingsInBillingGroup,
    ListThingsInBillingGroupResponse (ListThingsInBillingGroupResponse'),
    newListThingsInBillingGroupResponse,

    -- ** UpdateThingGroupsForThing
    UpdateThingGroupsForThing (UpdateThingGroupsForThing'),
    newUpdateThingGroupsForThing,
    UpdateThingGroupsForThingResponse (UpdateThingGroupsForThingResponse'),
    newUpdateThingGroupsForThingResponse,

    -- ** CreateFleetMetric
    CreateFleetMetric (CreateFleetMetric'),
    newCreateFleetMetric,
    CreateFleetMetricResponse (CreateFleetMetricResponse'),
    newCreateFleetMetricResponse,

    -- ** EnableTopicRule
    EnableTopicRule (EnableTopicRule'),
    newEnableTopicRule,
    EnableTopicRuleResponse (EnableTopicRuleResponse'),
    newEnableTopicRuleResponse,

    -- ** DeleteJobTemplate
    DeleteJobTemplate (DeleteJobTemplate'),
    newDeleteJobTemplate,
    DeleteJobTemplateResponse (DeleteJobTemplateResponse'),
    newDeleteJobTemplateResponse,

    -- ** AcceptCertificateTransfer
    AcceptCertificateTransfer (AcceptCertificateTransfer'),
    newAcceptCertificateTransfer,
    AcceptCertificateTransferResponse (AcceptCertificateTransferResponse'),
    newAcceptCertificateTransferResponse,

    -- ** GetPercentiles
    GetPercentiles (GetPercentiles'),
    newGetPercentiles,
    GetPercentilesResponse (GetPercentilesResponse'),
    newGetPercentilesResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ListJobTemplates (Paginated)
    ListJobTemplates (ListJobTemplates'),
    newListJobTemplates,
    ListJobTemplatesResponse (ListJobTemplatesResponse'),
    newListJobTemplatesResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

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

    -- ** UpdateCustomMetric
    UpdateCustomMetric (UpdateCustomMetric'),
    newUpdateCustomMetric,
    UpdateCustomMetricResponse (UpdateCustomMetricResponse'),
    newUpdateCustomMetricResponse,

    -- ** DeleteCustomMetric
    DeleteCustomMetric (DeleteCustomMetric'),
    newDeleteCustomMetric,
    DeleteCustomMetricResponse (DeleteCustomMetricResponse'),
    newDeleteCustomMetricResponse,

    -- ** RegisterCACertificate
    RegisterCACertificate (RegisterCACertificate'),
    newRegisterCACertificate,
    RegisterCACertificateResponse (RegisterCACertificateResponse'),
    newRegisterCACertificateResponse,

    -- ** DeleteDomainConfiguration
    DeleteDomainConfiguration (DeleteDomainConfiguration'),
    newDeleteDomainConfiguration,
    DeleteDomainConfigurationResponse (DeleteDomainConfigurationResponse'),
    newDeleteDomainConfigurationResponse,

    -- ** UpdateDomainConfiguration
    UpdateDomainConfiguration (UpdateDomainConfiguration'),
    newUpdateDomainConfiguration,
    UpdateDomainConfigurationResponse (UpdateDomainConfigurationResponse'),
    newUpdateDomainConfigurationResponse,

    -- ** SetLoggingOptions
    SetLoggingOptions (SetLoggingOptions'),
    newSetLoggingOptions,
    SetLoggingOptionsResponse (SetLoggingOptionsResponse'),
    newSetLoggingOptionsResponse,

    -- ** DescribeThingType
    DescribeThingType (DescribeThingType'),
    newDescribeThingType,
    DescribeThingTypeResponse (DescribeThingTypeResponse'),
    newDescribeThingTypeResponse,

    -- ** ListDimensions (Paginated)
    ListDimensions (ListDimensions'),
    newListDimensions,
    ListDimensionsResponse (ListDimensionsResponse'),
    newListDimensionsResponse,

    -- ** GetV2LoggingOptions
    GetV2LoggingOptions (GetV2LoggingOptions'),
    newGetV2LoggingOptions,
    GetV2LoggingOptionsResponse (GetV2LoggingOptionsResponse'),
    newGetV2LoggingOptionsResponse,

    -- ** ListThingRegistrationTasks (Paginated)
    ListThingRegistrationTasks (ListThingRegistrationTasks'),
    newListThingRegistrationTasks,
    ListThingRegistrationTasksResponse (ListThingRegistrationTasksResponse'),
    newListThingRegistrationTasksResponse,

    -- ** RejectCertificateTransfer
    RejectCertificateTransfer (RejectCertificateTransfer'),
    newRejectCertificateTransfer,
    RejectCertificateTransferResponse (RejectCertificateTransferResponse'),
    newRejectCertificateTransferResponse,

    -- ** DescribeAuditSuppression
    DescribeAuditSuppression (DescribeAuditSuppression'),
    newDescribeAuditSuppression,
    DescribeAuditSuppressionResponse (DescribeAuditSuppressionResponse'),
    newDescribeAuditSuppressionResponse,

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** CreateSecurityProfile
    CreateSecurityProfile (CreateSecurityProfile'),
    newCreateSecurityProfile,
    CreateSecurityProfileResponse (CreateSecurityProfileResponse'),
    newCreateSecurityProfileResponse,

    -- ** DescribeBillingGroup
    DescribeBillingGroup (DescribeBillingGroup'),
    newDescribeBillingGroup,
    DescribeBillingGroupResponse (DescribeBillingGroupResponse'),
    newDescribeBillingGroupResponse,

    -- ** DetachThingPrincipal
    DetachThingPrincipal (DetachThingPrincipal'),
    newDetachThingPrincipal,
    DetachThingPrincipalResponse (DetachThingPrincipalResponse'),
    newDetachThingPrincipalResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** DeprecateThingType
    DeprecateThingType (DeprecateThingType'),
    newDeprecateThingType,
    DeprecateThingTypeResponse (DeprecateThingTypeResponse'),
    newDeprecateThingTypeResponse,

    -- * Types

    -- ** AbortAction
    AbortAction (..),

    -- ** ActionType
    ActionType (..),

    -- ** AggregationTypeName
    AggregationTypeName (..),

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

    -- ** FleetMetricUnit
    FleetMetricUnit (..),

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

    -- ** VerificationState
    VerificationState (..),

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

    -- ** AggregationType
    AggregationType (AggregationType'),
    newAggregationType,

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

    -- ** Bucket
    Bucket (Bucket'),
    newBucket,

    -- ** BucketsAggregationType
    BucketsAggregationType (BucketsAggregationType'),
    newBucketsAggregationType,

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

    -- ** FleetMetricNameAndArn
    FleetMetricNameAndArn (FleetMetricNameAndArn'),
    newFleetMetricNameAndArn,

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

    -- ** JobTemplateSummary
    JobTemplateSummary (JobTemplateSummary'),
    newJobTemplateSummary,

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

    -- ** OpenSearchAction
    OpenSearchAction (OpenSearchAction'),
    newOpenSearchAction,

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

    -- ** TermsAggregation
    TermsAggregation (TermsAggregation'),
    newTermsAggregation,

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
import Network.AWS.IoT.CreateFleetMetric
import Network.AWS.IoT.CreateJob
import Network.AWS.IoT.CreateJobTemplate
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
import Network.AWS.IoT.DeleteFleetMetric
import Network.AWS.IoT.DeleteJob
import Network.AWS.IoT.DeleteJobExecution
import Network.AWS.IoT.DeleteJobTemplate
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
import Network.AWS.IoT.DescribeFleetMetric
import Network.AWS.IoT.DescribeIndex
import Network.AWS.IoT.DescribeJob
import Network.AWS.IoT.DescribeJobExecution
import Network.AWS.IoT.DescribeJobTemplate
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
import Network.AWS.IoT.GetBucketsAggregation
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
import Network.AWS.IoT.ListFleetMetrics
import Network.AWS.IoT.ListIndices
import Network.AWS.IoT.ListJobExecutionsForJob
import Network.AWS.IoT.ListJobExecutionsForThing
import Network.AWS.IoT.ListJobTemplates
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
import Network.AWS.IoT.PutVerificationStateOnViolation
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
import Network.AWS.IoT.UpdateFleetMetric
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

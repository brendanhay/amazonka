{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoT
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.IoT
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CertificateConflictException
    _CertificateConflictException,

    -- ** CertificateStateException
    _CertificateStateException,

    -- ** CertificateValidationException
    _CertificateValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ConflictingResourceUpdateException
    _ConflictingResourceUpdateException,

    -- ** DeleteConflictException
    _DeleteConflictException,

    -- ** IndexNotReadyException
    _IndexNotReadyException,

    -- ** InternalException
    _InternalException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidAggregationException
    _InvalidAggregationException,

    -- ** InvalidQueryException
    _InvalidQueryException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidResponseException
    _InvalidResponseException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MalformedPolicyException
    _MalformedPolicyException,

    -- ** NotConfiguredException
    _NotConfiguredException,

    -- ** RegistrationCodeValidationException
    _RegistrationCodeValidationException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceRegistrationFailureException
    _ResourceRegistrationFailureException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** SqlParseException
    _SqlParseException,

    -- ** TaskAlreadyExistsException
    _TaskAlreadyExistsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TransferAlreadyCompletedException
    _TransferAlreadyCompletedException,

    -- ** TransferConflictException
    _TransferConflictException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ValidationException
    _ValidationException,

    -- ** VersionConflictException
    _VersionConflictException,

    -- ** VersionsLimitExceededException
    _VersionsLimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptCertificateTransfer
    AcceptCertificateTransfer (AcceptCertificateTransfer'),
    newAcceptCertificateTransfer,
    AcceptCertificateTransferResponse (AcceptCertificateTransferResponse'),
    newAcceptCertificateTransferResponse,

    -- ** AddThingToBillingGroup
    AddThingToBillingGroup (AddThingToBillingGroup'),
    newAddThingToBillingGroup,
    AddThingToBillingGroupResponse (AddThingToBillingGroupResponse'),
    newAddThingToBillingGroupResponse,

    -- ** AddThingToThingGroup
    AddThingToThingGroup (AddThingToThingGroup'),
    newAddThingToThingGroup,
    AddThingToThingGroupResponse (AddThingToThingGroupResponse'),
    newAddThingToThingGroupResponse,

    -- ** AssociateTargetsWithJob
    AssociateTargetsWithJob (AssociateTargetsWithJob'),
    newAssociateTargetsWithJob,
    AssociateTargetsWithJobResponse (AssociateTargetsWithJobResponse'),
    newAssociateTargetsWithJobResponse,

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** AttachSecurityProfile
    AttachSecurityProfile (AttachSecurityProfile'),
    newAttachSecurityProfile,
    AttachSecurityProfileResponse (AttachSecurityProfileResponse'),
    newAttachSecurityProfileResponse,

    -- ** AttachThingPrincipal
    AttachThingPrincipal (AttachThingPrincipal'),
    newAttachThingPrincipal,
    AttachThingPrincipalResponse (AttachThingPrincipalResponse'),
    newAttachThingPrincipalResponse,

    -- ** CancelAuditMitigationActionsTask
    CancelAuditMitigationActionsTask (CancelAuditMitigationActionsTask'),
    newCancelAuditMitigationActionsTask,
    CancelAuditMitigationActionsTaskResponse (CancelAuditMitigationActionsTaskResponse'),
    newCancelAuditMitigationActionsTaskResponse,

    -- ** CancelAuditTask
    CancelAuditTask (CancelAuditTask'),
    newCancelAuditTask,
    CancelAuditTaskResponse (CancelAuditTaskResponse'),
    newCancelAuditTaskResponse,

    -- ** CancelCertificateTransfer
    CancelCertificateTransfer (CancelCertificateTransfer'),
    newCancelCertificateTransfer,
    CancelCertificateTransferResponse (CancelCertificateTransferResponse'),
    newCancelCertificateTransferResponse,

    -- ** CancelDetectMitigationActionsTask
    CancelDetectMitigationActionsTask (CancelDetectMitigationActionsTask'),
    newCancelDetectMitigationActionsTask,
    CancelDetectMitigationActionsTaskResponse (CancelDetectMitigationActionsTaskResponse'),
    newCancelDetectMitigationActionsTaskResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CancelJobExecution
    CancelJobExecution (CancelJobExecution'),
    newCancelJobExecution,
    CancelJobExecutionResponse (CancelJobExecutionResponse'),
    newCancelJobExecutionResponse,

    -- ** ClearDefaultAuthorizer
    ClearDefaultAuthorizer (ClearDefaultAuthorizer'),
    newClearDefaultAuthorizer,
    ClearDefaultAuthorizerResponse (ClearDefaultAuthorizerResponse'),
    newClearDefaultAuthorizerResponse,

    -- ** ConfirmTopicRuleDestination
    ConfirmTopicRuleDestination (ConfirmTopicRuleDestination'),
    newConfirmTopicRuleDestination,
    ConfirmTopicRuleDestinationResponse (ConfirmTopicRuleDestinationResponse'),
    newConfirmTopicRuleDestinationResponse,

    -- ** CreateAuditSuppression
    CreateAuditSuppression (CreateAuditSuppression'),
    newCreateAuditSuppression,
    CreateAuditSuppressionResponse (CreateAuditSuppressionResponse'),
    newCreateAuditSuppressionResponse,

    -- ** CreateAuthorizer
    CreateAuthorizer (CreateAuthorizer'),
    newCreateAuthorizer,
    CreateAuthorizerResponse (CreateAuthorizerResponse'),
    newCreateAuthorizerResponse,

    -- ** CreateBillingGroup
    CreateBillingGroup (CreateBillingGroup'),
    newCreateBillingGroup,
    CreateBillingGroupResponse (CreateBillingGroupResponse'),
    newCreateBillingGroupResponse,

    -- ** CreateCertificateFromCsr
    CreateCertificateFromCsr (CreateCertificateFromCsr'),
    newCreateCertificateFromCsr,
    CreateCertificateFromCsrResponse (CreateCertificateFromCsrResponse'),
    newCreateCertificateFromCsrResponse,

    -- ** CreateCustomMetric
    CreateCustomMetric (CreateCustomMetric'),
    newCreateCustomMetric,
    CreateCustomMetricResponse (CreateCustomMetricResponse'),
    newCreateCustomMetricResponse,

    -- ** CreateDimension
    CreateDimension (CreateDimension'),
    newCreateDimension,
    CreateDimensionResponse (CreateDimensionResponse'),
    newCreateDimensionResponse,

    -- ** CreateDomainConfiguration
    CreateDomainConfiguration (CreateDomainConfiguration'),
    newCreateDomainConfiguration,
    CreateDomainConfigurationResponse (CreateDomainConfigurationResponse'),
    newCreateDomainConfigurationResponse,

    -- ** CreateDynamicThingGroup
    CreateDynamicThingGroup (CreateDynamicThingGroup'),
    newCreateDynamicThingGroup,
    CreateDynamicThingGroupResponse (CreateDynamicThingGroupResponse'),
    newCreateDynamicThingGroupResponse,

    -- ** CreateFleetMetric
    CreateFleetMetric (CreateFleetMetric'),
    newCreateFleetMetric,
    CreateFleetMetricResponse (CreateFleetMetricResponse'),
    newCreateFleetMetricResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** CreateJobTemplate
    CreateJobTemplate (CreateJobTemplate'),
    newCreateJobTemplate,
    CreateJobTemplateResponse (CreateJobTemplateResponse'),
    newCreateJobTemplateResponse,

    -- ** CreateKeysAndCertificate
    CreateKeysAndCertificate (CreateKeysAndCertificate'),
    newCreateKeysAndCertificate,
    CreateKeysAndCertificateResponse (CreateKeysAndCertificateResponse'),
    newCreateKeysAndCertificateResponse,

    -- ** CreateMitigationAction
    CreateMitigationAction (CreateMitigationAction'),
    newCreateMitigationAction,
    CreateMitigationActionResponse (CreateMitigationActionResponse'),
    newCreateMitigationActionResponse,

    -- ** CreateOTAUpdate
    CreateOTAUpdate (CreateOTAUpdate'),
    newCreateOTAUpdate,
    CreateOTAUpdateResponse (CreateOTAUpdateResponse'),
    newCreateOTAUpdateResponse,

    -- ** CreatePackage
    CreatePackage (CreatePackage'),
    newCreatePackage,
    CreatePackageResponse (CreatePackageResponse'),
    newCreatePackageResponse,

    -- ** CreatePackageVersion
    CreatePackageVersion (CreatePackageVersion'),
    newCreatePackageVersion,
    CreatePackageVersionResponse (CreatePackageVersionResponse'),
    newCreatePackageVersionResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** CreatePolicyVersion
    CreatePolicyVersion (CreatePolicyVersion'),
    newCreatePolicyVersion,
    CreatePolicyVersionResponse (CreatePolicyVersionResponse'),
    newCreatePolicyVersionResponse,

    -- ** CreateProvisioningClaim
    CreateProvisioningClaim (CreateProvisioningClaim'),
    newCreateProvisioningClaim,
    CreateProvisioningClaimResponse (CreateProvisioningClaimResponse'),
    newCreateProvisioningClaimResponse,

    -- ** CreateProvisioningTemplate
    CreateProvisioningTemplate (CreateProvisioningTemplate'),
    newCreateProvisioningTemplate,
    CreateProvisioningTemplateResponse (CreateProvisioningTemplateResponse'),
    newCreateProvisioningTemplateResponse,

    -- ** CreateProvisioningTemplateVersion
    CreateProvisioningTemplateVersion (CreateProvisioningTemplateVersion'),
    newCreateProvisioningTemplateVersion,
    CreateProvisioningTemplateVersionResponse (CreateProvisioningTemplateVersionResponse'),
    newCreateProvisioningTemplateVersionResponse,

    -- ** CreateRoleAlias
    CreateRoleAlias (CreateRoleAlias'),
    newCreateRoleAlias,
    CreateRoleAliasResponse (CreateRoleAliasResponse'),
    newCreateRoleAliasResponse,

    -- ** CreateScheduledAudit
    CreateScheduledAudit (CreateScheduledAudit'),
    newCreateScheduledAudit,
    CreateScheduledAuditResponse (CreateScheduledAuditResponse'),
    newCreateScheduledAuditResponse,

    -- ** CreateSecurityProfile
    CreateSecurityProfile (CreateSecurityProfile'),
    newCreateSecurityProfile,
    CreateSecurityProfileResponse (CreateSecurityProfileResponse'),
    newCreateSecurityProfileResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** CreateThing
    CreateThing (CreateThing'),
    newCreateThing,
    CreateThingResponse (CreateThingResponse'),
    newCreateThingResponse,

    -- ** CreateThingGroup
    CreateThingGroup (CreateThingGroup'),
    newCreateThingGroup,
    CreateThingGroupResponse (CreateThingGroupResponse'),
    newCreateThingGroupResponse,

    -- ** CreateThingType
    CreateThingType (CreateThingType'),
    newCreateThingType,
    CreateThingTypeResponse (CreateThingTypeResponse'),
    newCreateThingTypeResponse,

    -- ** CreateTopicRule
    CreateTopicRule (CreateTopicRule'),
    newCreateTopicRule,
    CreateTopicRuleResponse (CreateTopicRuleResponse'),
    newCreateTopicRuleResponse,

    -- ** CreateTopicRuleDestination
    CreateTopicRuleDestination (CreateTopicRuleDestination'),
    newCreateTopicRuleDestination,
    CreateTopicRuleDestinationResponse (CreateTopicRuleDestinationResponse'),
    newCreateTopicRuleDestinationResponse,

    -- ** DeleteAccountAuditConfiguration
    DeleteAccountAuditConfiguration (DeleteAccountAuditConfiguration'),
    newDeleteAccountAuditConfiguration,
    DeleteAccountAuditConfigurationResponse (DeleteAccountAuditConfigurationResponse'),
    newDeleteAccountAuditConfigurationResponse,

    -- ** DeleteAuditSuppression
    DeleteAuditSuppression (DeleteAuditSuppression'),
    newDeleteAuditSuppression,
    DeleteAuditSuppressionResponse (DeleteAuditSuppressionResponse'),
    newDeleteAuditSuppressionResponse,

    -- ** DeleteAuthorizer
    DeleteAuthorizer (DeleteAuthorizer'),
    newDeleteAuthorizer,
    DeleteAuthorizerResponse (DeleteAuthorizerResponse'),
    newDeleteAuthorizerResponse,

    -- ** DeleteBillingGroup
    DeleteBillingGroup (DeleteBillingGroup'),
    newDeleteBillingGroup,
    DeleteBillingGroupResponse (DeleteBillingGroupResponse'),
    newDeleteBillingGroupResponse,

    -- ** DeleteCACertificate
    DeleteCACertificate (DeleteCACertificate'),
    newDeleteCACertificate,
    DeleteCACertificateResponse (DeleteCACertificateResponse'),
    newDeleteCACertificateResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DeleteCustomMetric
    DeleteCustomMetric (DeleteCustomMetric'),
    newDeleteCustomMetric,
    DeleteCustomMetricResponse (DeleteCustomMetricResponse'),
    newDeleteCustomMetricResponse,

    -- ** DeleteDimension
    DeleteDimension (DeleteDimension'),
    newDeleteDimension,
    DeleteDimensionResponse (DeleteDimensionResponse'),
    newDeleteDimensionResponse,

    -- ** DeleteDomainConfiguration
    DeleteDomainConfiguration (DeleteDomainConfiguration'),
    newDeleteDomainConfiguration,
    DeleteDomainConfigurationResponse (DeleteDomainConfigurationResponse'),
    newDeleteDomainConfigurationResponse,

    -- ** DeleteDynamicThingGroup
    DeleteDynamicThingGroup (DeleteDynamicThingGroup'),
    newDeleteDynamicThingGroup,
    DeleteDynamicThingGroupResponse (DeleteDynamicThingGroupResponse'),
    newDeleteDynamicThingGroupResponse,

    -- ** DeleteFleetMetric
    DeleteFleetMetric (DeleteFleetMetric'),
    newDeleteFleetMetric,
    DeleteFleetMetricResponse (DeleteFleetMetricResponse'),
    newDeleteFleetMetricResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteJobExecution
    DeleteJobExecution (DeleteJobExecution'),
    newDeleteJobExecution,
    DeleteJobExecutionResponse (DeleteJobExecutionResponse'),
    newDeleteJobExecutionResponse,

    -- ** DeleteJobTemplate
    DeleteJobTemplate (DeleteJobTemplate'),
    newDeleteJobTemplate,
    DeleteJobTemplateResponse (DeleteJobTemplateResponse'),
    newDeleteJobTemplateResponse,

    -- ** DeleteMitigationAction
    DeleteMitigationAction (DeleteMitigationAction'),
    newDeleteMitigationAction,
    DeleteMitigationActionResponse (DeleteMitigationActionResponse'),
    newDeleteMitigationActionResponse,

    -- ** DeleteOTAUpdate
    DeleteOTAUpdate (DeleteOTAUpdate'),
    newDeleteOTAUpdate,
    DeleteOTAUpdateResponse (DeleteOTAUpdateResponse'),
    newDeleteOTAUpdateResponse,

    -- ** DeletePackage
    DeletePackage (DeletePackage'),
    newDeletePackage,
    DeletePackageResponse (DeletePackageResponse'),
    newDeletePackageResponse,

    -- ** DeletePackageVersion
    DeletePackageVersion (DeletePackageVersion'),
    newDeletePackageVersion,
    DeletePackageVersionResponse (DeletePackageVersionResponse'),
    newDeletePackageVersionResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeletePolicyVersion
    DeletePolicyVersion (DeletePolicyVersion'),
    newDeletePolicyVersion,
    DeletePolicyVersionResponse (DeletePolicyVersionResponse'),
    newDeletePolicyVersionResponse,

    -- ** DeleteProvisioningTemplate
    DeleteProvisioningTemplate (DeleteProvisioningTemplate'),
    newDeleteProvisioningTemplate,
    DeleteProvisioningTemplateResponse (DeleteProvisioningTemplateResponse'),
    newDeleteProvisioningTemplateResponse,

    -- ** DeleteProvisioningTemplateVersion
    DeleteProvisioningTemplateVersion (DeleteProvisioningTemplateVersion'),
    newDeleteProvisioningTemplateVersion,
    DeleteProvisioningTemplateVersionResponse (DeleteProvisioningTemplateVersionResponse'),
    newDeleteProvisioningTemplateVersionResponse,

    -- ** DeleteRegistrationCode
    DeleteRegistrationCode (DeleteRegistrationCode'),
    newDeleteRegistrationCode,
    DeleteRegistrationCodeResponse (DeleteRegistrationCodeResponse'),
    newDeleteRegistrationCodeResponse,

    -- ** DeleteRoleAlias
    DeleteRoleAlias (DeleteRoleAlias'),
    newDeleteRoleAlias,
    DeleteRoleAliasResponse (DeleteRoleAliasResponse'),
    newDeleteRoleAliasResponse,

    -- ** DeleteScheduledAudit
    DeleteScheduledAudit (DeleteScheduledAudit'),
    newDeleteScheduledAudit,
    DeleteScheduledAuditResponse (DeleteScheduledAuditResponse'),
    newDeleteScheduledAuditResponse,

    -- ** DeleteSecurityProfile
    DeleteSecurityProfile (DeleteSecurityProfile'),
    newDeleteSecurityProfile,
    DeleteSecurityProfileResponse (DeleteSecurityProfileResponse'),
    newDeleteSecurityProfileResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** DeleteThing
    DeleteThing (DeleteThing'),
    newDeleteThing,
    DeleteThingResponse (DeleteThingResponse'),
    newDeleteThingResponse,

    -- ** DeleteThingGroup
    DeleteThingGroup (DeleteThingGroup'),
    newDeleteThingGroup,
    DeleteThingGroupResponse (DeleteThingGroupResponse'),
    newDeleteThingGroupResponse,

    -- ** DeleteThingType
    DeleteThingType (DeleteThingType'),
    newDeleteThingType,
    DeleteThingTypeResponse (DeleteThingTypeResponse'),
    newDeleteThingTypeResponse,

    -- ** DeleteTopicRule
    DeleteTopicRule (DeleteTopicRule'),
    newDeleteTopicRule,
    DeleteTopicRuleResponse (DeleteTopicRuleResponse'),
    newDeleteTopicRuleResponse,

    -- ** DeleteTopicRuleDestination
    DeleteTopicRuleDestination (DeleteTopicRuleDestination'),
    newDeleteTopicRuleDestination,
    DeleteTopicRuleDestinationResponse (DeleteTopicRuleDestinationResponse'),
    newDeleteTopicRuleDestinationResponse,

    -- ** DeleteV2LoggingLevel
    DeleteV2LoggingLevel (DeleteV2LoggingLevel'),
    newDeleteV2LoggingLevel,
    DeleteV2LoggingLevelResponse (DeleteV2LoggingLevelResponse'),
    newDeleteV2LoggingLevelResponse,

    -- ** DeprecateThingType
    DeprecateThingType (DeprecateThingType'),
    newDeprecateThingType,
    DeprecateThingTypeResponse (DeprecateThingTypeResponse'),
    newDeprecateThingTypeResponse,

    -- ** DescribeAccountAuditConfiguration
    DescribeAccountAuditConfiguration (DescribeAccountAuditConfiguration'),
    newDescribeAccountAuditConfiguration,
    DescribeAccountAuditConfigurationResponse (DescribeAccountAuditConfigurationResponse'),
    newDescribeAccountAuditConfigurationResponse,

    -- ** DescribeAuditFinding
    DescribeAuditFinding (DescribeAuditFinding'),
    newDescribeAuditFinding,
    DescribeAuditFindingResponse (DescribeAuditFindingResponse'),
    newDescribeAuditFindingResponse,

    -- ** DescribeAuditMitigationActionsTask
    DescribeAuditMitigationActionsTask (DescribeAuditMitigationActionsTask'),
    newDescribeAuditMitigationActionsTask,
    DescribeAuditMitigationActionsTaskResponse (DescribeAuditMitigationActionsTaskResponse'),
    newDescribeAuditMitigationActionsTaskResponse,

    -- ** DescribeAuditSuppression
    DescribeAuditSuppression (DescribeAuditSuppression'),
    newDescribeAuditSuppression,
    DescribeAuditSuppressionResponse (DescribeAuditSuppressionResponse'),
    newDescribeAuditSuppressionResponse,

    -- ** DescribeAuditTask
    DescribeAuditTask (DescribeAuditTask'),
    newDescribeAuditTask,
    DescribeAuditTaskResponse (DescribeAuditTaskResponse'),
    newDescribeAuditTaskResponse,

    -- ** DescribeAuthorizer
    DescribeAuthorizer (DescribeAuthorizer'),
    newDescribeAuthorizer,
    DescribeAuthorizerResponse (DescribeAuthorizerResponse'),
    newDescribeAuthorizerResponse,

    -- ** DescribeBillingGroup
    DescribeBillingGroup (DescribeBillingGroup'),
    newDescribeBillingGroup,
    DescribeBillingGroupResponse (DescribeBillingGroupResponse'),
    newDescribeBillingGroupResponse,

    -- ** DescribeCACertificate
    DescribeCACertificate (DescribeCACertificate'),
    newDescribeCACertificate,
    DescribeCACertificateResponse (DescribeCACertificateResponse'),
    newDescribeCACertificateResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** DescribeCustomMetric
    DescribeCustomMetric (DescribeCustomMetric'),
    newDescribeCustomMetric,
    DescribeCustomMetricResponse (DescribeCustomMetricResponse'),
    newDescribeCustomMetricResponse,

    -- ** DescribeDefaultAuthorizer
    DescribeDefaultAuthorizer (DescribeDefaultAuthorizer'),
    newDescribeDefaultAuthorizer,
    DescribeDefaultAuthorizerResponse (DescribeDefaultAuthorizerResponse'),
    newDescribeDefaultAuthorizerResponse,

    -- ** DescribeDetectMitigationActionsTask
    DescribeDetectMitigationActionsTask (DescribeDetectMitigationActionsTask'),
    newDescribeDetectMitigationActionsTask,
    DescribeDetectMitigationActionsTaskResponse (DescribeDetectMitigationActionsTaskResponse'),
    newDescribeDetectMitigationActionsTaskResponse,

    -- ** DescribeDimension
    DescribeDimension (DescribeDimension'),
    newDescribeDimension,
    DescribeDimensionResponse (DescribeDimensionResponse'),
    newDescribeDimensionResponse,

    -- ** DescribeDomainConfiguration
    DescribeDomainConfiguration (DescribeDomainConfiguration'),
    newDescribeDomainConfiguration,
    DescribeDomainConfigurationResponse (DescribeDomainConfigurationResponse'),
    newDescribeDomainConfigurationResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** DescribeEventConfigurations
    DescribeEventConfigurations (DescribeEventConfigurations'),
    newDescribeEventConfigurations,
    DescribeEventConfigurationsResponse (DescribeEventConfigurationsResponse'),
    newDescribeEventConfigurationsResponse,

    -- ** DescribeFleetMetric
    DescribeFleetMetric (DescribeFleetMetric'),
    newDescribeFleetMetric,
    DescribeFleetMetricResponse (DescribeFleetMetricResponse'),
    newDescribeFleetMetricResponse,

    -- ** DescribeIndex
    DescribeIndex (DescribeIndex'),
    newDescribeIndex,
    DescribeIndexResponse (DescribeIndexResponse'),
    newDescribeIndexResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** DescribeJobExecution
    DescribeJobExecution (DescribeJobExecution'),
    newDescribeJobExecution,
    DescribeJobExecutionResponse (DescribeJobExecutionResponse'),
    newDescribeJobExecutionResponse,

    -- ** DescribeJobTemplate
    DescribeJobTemplate (DescribeJobTemplate'),
    newDescribeJobTemplate,
    DescribeJobTemplateResponse (DescribeJobTemplateResponse'),
    newDescribeJobTemplateResponse,

    -- ** DescribeManagedJobTemplate
    DescribeManagedJobTemplate (DescribeManagedJobTemplate'),
    newDescribeManagedJobTemplate,
    DescribeManagedJobTemplateResponse (DescribeManagedJobTemplateResponse'),
    newDescribeManagedJobTemplateResponse,

    -- ** DescribeMitigationAction
    DescribeMitigationAction (DescribeMitigationAction'),
    newDescribeMitigationAction,
    DescribeMitigationActionResponse (DescribeMitigationActionResponse'),
    newDescribeMitigationActionResponse,

    -- ** DescribeProvisioningTemplate
    DescribeProvisioningTemplate (DescribeProvisioningTemplate'),
    newDescribeProvisioningTemplate,
    DescribeProvisioningTemplateResponse (DescribeProvisioningTemplateResponse'),
    newDescribeProvisioningTemplateResponse,

    -- ** DescribeProvisioningTemplateVersion
    DescribeProvisioningTemplateVersion (DescribeProvisioningTemplateVersion'),
    newDescribeProvisioningTemplateVersion,
    DescribeProvisioningTemplateVersionResponse (DescribeProvisioningTemplateVersionResponse'),
    newDescribeProvisioningTemplateVersionResponse,

    -- ** DescribeRoleAlias
    DescribeRoleAlias (DescribeRoleAlias'),
    newDescribeRoleAlias,
    DescribeRoleAliasResponse (DescribeRoleAliasResponse'),
    newDescribeRoleAliasResponse,

    -- ** DescribeScheduledAudit
    DescribeScheduledAudit (DescribeScheduledAudit'),
    newDescribeScheduledAudit,
    DescribeScheduledAuditResponse (DescribeScheduledAuditResponse'),
    newDescribeScheduledAuditResponse,

    -- ** DescribeSecurityProfile
    DescribeSecurityProfile (DescribeSecurityProfile'),
    newDescribeSecurityProfile,
    DescribeSecurityProfileResponse (DescribeSecurityProfileResponse'),
    newDescribeSecurityProfileResponse,

    -- ** DescribeStream
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** DescribeThing
    DescribeThing (DescribeThing'),
    newDescribeThing,
    DescribeThingResponse (DescribeThingResponse'),
    newDescribeThingResponse,

    -- ** DescribeThingGroup
    DescribeThingGroup (DescribeThingGroup'),
    newDescribeThingGroup,
    DescribeThingGroupResponse (DescribeThingGroupResponse'),
    newDescribeThingGroupResponse,

    -- ** DescribeThingRegistrationTask
    DescribeThingRegistrationTask (DescribeThingRegistrationTask'),
    newDescribeThingRegistrationTask,
    DescribeThingRegistrationTaskResponse (DescribeThingRegistrationTaskResponse'),
    newDescribeThingRegistrationTaskResponse,

    -- ** DescribeThingType
    DescribeThingType (DescribeThingType'),
    newDescribeThingType,
    DescribeThingTypeResponse (DescribeThingTypeResponse'),
    newDescribeThingTypeResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** DetachSecurityProfile
    DetachSecurityProfile (DetachSecurityProfile'),
    newDetachSecurityProfile,
    DetachSecurityProfileResponse (DetachSecurityProfileResponse'),
    newDetachSecurityProfileResponse,

    -- ** DetachThingPrincipal
    DetachThingPrincipal (DetachThingPrincipal'),
    newDetachThingPrincipal,
    DetachThingPrincipalResponse (DetachThingPrincipalResponse'),
    newDetachThingPrincipalResponse,

    -- ** DisableTopicRule
    DisableTopicRule (DisableTopicRule'),
    newDisableTopicRule,
    DisableTopicRuleResponse (DisableTopicRuleResponse'),
    newDisableTopicRuleResponse,

    -- ** EnableTopicRule
    EnableTopicRule (EnableTopicRule'),
    newEnableTopicRule,
    EnableTopicRuleResponse (EnableTopicRuleResponse'),
    newEnableTopicRuleResponse,

    -- ** GetBehaviorModelTrainingSummaries (Paginated)
    GetBehaviorModelTrainingSummaries (GetBehaviorModelTrainingSummaries'),
    newGetBehaviorModelTrainingSummaries,
    GetBehaviorModelTrainingSummariesResponse (GetBehaviorModelTrainingSummariesResponse'),
    newGetBehaviorModelTrainingSummariesResponse,

    -- ** GetBucketsAggregation
    GetBucketsAggregation (GetBucketsAggregation'),
    newGetBucketsAggregation,
    GetBucketsAggregationResponse (GetBucketsAggregationResponse'),
    newGetBucketsAggregationResponse,

    -- ** GetCardinality
    GetCardinality (GetCardinality'),
    newGetCardinality,
    GetCardinalityResponse (GetCardinalityResponse'),
    newGetCardinalityResponse,

    -- ** GetEffectivePolicies
    GetEffectivePolicies (GetEffectivePolicies'),
    newGetEffectivePolicies,
    GetEffectivePoliciesResponse (GetEffectivePoliciesResponse'),
    newGetEffectivePoliciesResponse,

    -- ** GetIndexingConfiguration
    GetIndexingConfiguration (GetIndexingConfiguration'),
    newGetIndexingConfiguration,
    GetIndexingConfigurationResponse (GetIndexingConfigurationResponse'),
    newGetIndexingConfigurationResponse,

    -- ** GetJobDocument
    GetJobDocument (GetJobDocument'),
    newGetJobDocument,
    GetJobDocumentResponse (GetJobDocumentResponse'),
    newGetJobDocumentResponse,

    -- ** GetLoggingOptions
    GetLoggingOptions (GetLoggingOptions'),
    newGetLoggingOptions,
    GetLoggingOptionsResponse (GetLoggingOptionsResponse'),
    newGetLoggingOptionsResponse,

    -- ** GetOTAUpdate
    GetOTAUpdate (GetOTAUpdate'),
    newGetOTAUpdate,
    GetOTAUpdateResponse (GetOTAUpdateResponse'),
    newGetOTAUpdateResponse,

    -- ** GetPackage
    GetPackage (GetPackage'),
    newGetPackage,
    GetPackageResponse (GetPackageResponse'),
    newGetPackageResponse,

    -- ** GetPackageConfiguration
    GetPackageConfiguration (GetPackageConfiguration'),
    newGetPackageConfiguration,
    GetPackageConfigurationResponse (GetPackageConfigurationResponse'),
    newGetPackageConfigurationResponse,

    -- ** GetPackageVersion
    GetPackageVersion (GetPackageVersion'),
    newGetPackageVersion,
    GetPackageVersionResponse (GetPackageVersionResponse'),
    newGetPackageVersionResponse,

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

    -- ** GetPolicyVersion
    GetPolicyVersion (GetPolicyVersion'),
    newGetPolicyVersion,
    GetPolicyVersionResponse (GetPolicyVersionResponse'),
    newGetPolicyVersionResponse,

    -- ** GetRegistrationCode
    GetRegistrationCode (GetRegistrationCode'),
    newGetRegistrationCode,
    GetRegistrationCodeResponse (GetRegistrationCodeResponse'),
    newGetRegistrationCodeResponse,

    -- ** GetStatistics
    GetStatistics (GetStatistics'),
    newGetStatistics,
    GetStatisticsResponse (GetStatisticsResponse'),
    newGetStatisticsResponse,

    -- ** GetTopicRule
    GetTopicRule (GetTopicRule'),
    newGetTopicRule,
    GetTopicRuleResponse (GetTopicRuleResponse'),
    newGetTopicRuleResponse,

    -- ** GetTopicRuleDestination
    GetTopicRuleDestination (GetTopicRuleDestination'),
    newGetTopicRuleDestination,
    GetTopicRuleDestinationResponse (GetTopicRuleDestinationResponse'),
    newGetTopicRuleDestinationResponse,

    -- ** GetV2LoggingOptions
    GetV2LoggingOptions (GetV2LoggingOptions'),
    newGetV2LoggingOptions,
    GetV2LoggingOptionsResponse (GetV2LoggingOptionsResponse'),
    newGetV2LoggingOptionsResponse,

    -- ** ListActiveViolations (Paginated)
    ListActiveViolations (ListActiveViolations'),
    newListActiveViolations,
    ListActiveViolationsResponse (ListActiveViolationsResponse'),
    newListActiveViolationsResponse,

    -- ** ListAttachedPolicies (Paginated)
    ListAttachedPolicies (ListAttachedPolicies'),
    newListAttachedPolicies,
    ListAttachedPoliciesResponse (ListAttachedPoliciesResponse'),
    newListAttachedPoliciesResponse,

    -- ** ListAuditFindings (Paginated)
    ListAuditFindings (ListAuditFindings'),
    newListAuditFindings,
    ListAuditFindingsResponse (ListAuditFindingsResponse'),
    newListAuditFindingsResponse,

    -- ** ListAuditMitigationActionsExecutions (Paginated)
    ListAuditMitigationActionsExecutions (ListAuditMitigationActionsExecutions'),
    newListAuditMitigationActionsExecutions,
    ListAuditMitigationActionsExecutionsResponse (ListAuditMitigationActionsExecutionsResponse'),
    newListAuditMitigationActionsExecutionsResponse,

    -- ** ListAuditMitigationActionsTasks (Paginated)
    ListAuditMitigationActionsTasks (ListAuditMitigationActionsTasks'),
    newListAuditMitigationActionsTasks,
    ListAuditMitigationActionsTasksResponse (ListAuditMitigationActionsTasksResponse'),
    newListAuditMitigationActionsTasksResponse,

    -- ** ListAuditSuppressions (Paginated)
    ListAuditSuppressions (ListAuditSuppressions'),
    newListAuditSuppressions,
    ListAuditSuppressionsResponse (ListAuditSuppressionsResponse'),
    newListAuditSuppressionsResponse,

    -- ** ListAuditTasks (Paginated)
    ListAuditTasks (ListAuditTasks'),
    newListAuditTasks,
    ListAuditTasksResponse (ListAuditTasksResponse'),
    newListAuditTasksResponse,

    -- ** ListAuthorizers (Paginated)
    ListAuthorizers (ListAuthorizers'),
    newListAuthorizers,
    ListAuthorizersResponse (ListAuthorizersResponse'),
    newListAuthorizersResponse,

    -- ** ListBillingGroups (Paginated)
    ListBillingGroups (ListBillingGroups'),
    newListBillingGroups,
    ListBillingGroupsResponse (ListBillingGroupsResponse'),
    newListBillingGroupsResponse,

    -- ** ListCACertificates (Paginated)
    ListCACertificates (ListCACertificates'),
    newListCACertificates,
    ListCACertificatesResponse (ListCACertificatesResponse'),
    newListCACertificatesResponse,

    -- ** ListCertificates (Paginated)
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** ListCertificatesByCA (Paginated)
    ListCertificatesByCA (ListCertificatesByCA'),
    newListCertificatesByCA,
    ListCertificatesByCAResponse (ListCertificatesByCAResponse'),
    newListCertificatesByCAResponse,

    -- ** ListCustomMetrics (Paginated)
    ListCustomMetrics (ListCustomMetrics'),
    newListCustomMetrics,
    ListCustomMetricsResponse (ListCustomMetricsResponse'),
    newListCustomMetricsResponse,

    -- ** ListDetectMitigationActionsExecutions (Paginated)
    ListDetectMitigationActionsExecutions (ListDetectMitigationActionsExecutions'),
    newListDetectMitigationActionsExecutions,
    ListDetectMitigationActionsExecutionsResponse (ListDetectMitigationActionsExecutionsResponse'),
    newListDetectMitigationActionsExecutionsResponse,

    -- ** ListDetectMitigationActionsTasks (Paginated)
    ListDetectMitigationActionsTasks (ListDetectMitigationActionsTasks'),
    newListDetectMitigationActionsTasks,
    ListDetectMitigationActionsTasksResponse (ListDetectMitigationActionsTasksResponse'),
    newListDetectMitigationActionsTasksResponse,

    -- ** ListDimensions (Paginated)
    ListDimensions (ListDimensions'),
    newListDimensions,
    ListDimensionsResponse (ListDimensionsResponse'),
    newListDimensionsResponse,

    -- ** ListDomainConfigurations (Paginated)
    ListDomainConfigurations (ListDomainConfigurations'),
    newListDomainConfigurations,
    ListDomainConfigurationsResponse (ListDomainConfigurationsResponse'),
    newListDomainConfigurationsResponse,

    -- ** ListFleetMetrics (Paginated)
    ListFleetMetrics (ListFleetMetrics'),
    newListFleetMetrics,
    ListFleetMetricsResponse (ListFleetMetricsResponse'),
    newListFleetMetricsResponse,

    -- ** ListIndices (Paginated)
    ListIndices (ListIndices'),
    newListIndices,
    ListIndicesResponse (ListIndicesResponse'),
    newListIndicesResponse,

    -- ** ListJobExecutionsForJob (Paginated)
    ListJobExecutionsForJob (ListJobExecutionsForJob'),
    newListJobExecutionsForJob,
    ListJobExecutionsForJobResponse (ListJobExecutionsForJobResponse'),
    newListJobExecutionsForJobResponse,

    -- ** ListJobExecutionsForThing (Paginated)
    ListJobExecutionsForThing (ListJobExecutionsForThing'),
    newListJobExecutionsForThing,
    ListJobExecutionsForThingResponse (ListJobExecutionsForThingResponse'),
    newListJobExecutionsForThingResponse,

    -- ** ListJobTemplates (Paginated)
    ListJobTemplates (ListJobTemplates'),
    newListJobTemplates,
    ListJobTemplatesResponse (ListJobTemplatesResponse'),
    newListJobTemplatesResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListManagedJobTemplates (Paginated)
    ListManagedJobTemplates (ListManagedJobTemplates'),
    newListManagedJobTemplates,
    ListManagedJobTemplatesResponse (ListManagedJobTemplatesResponse'),
    newListManagedJobTemplatesResponse,

    -- ** ListMetricValues (Paginated)
    ListMetricValues (ListMetricValues'),
    newListMetricValues,
    ListMetricValuesResponse (ListMetricValuesResponse'),
    newListMetricValuesResponse,

    -- ** ListMitigationActions (Paginated)
    ListMitigationActions (ListMitigationActions'),
    newListMitigationActions,
    ListMitigationActionsResponse (ListMitigationActionsResponse'),
    newListMitigationActionsResponse,

    -- ** ListOTAUpdates (Paginated)
    ListOTAUpdates (ListOTAUpdates'),
    newListOTAUpdates,
    ListOTAUpdatesResponse (ListOTAUpdatesResponse'),
    newListOTAUpdatesResponse,

    -- ** ListOutgoingCertificates (Paginated)
    ListOutgoingCertificates (ListOutgoingCertificates'),
    newListOutgoingCertificates,
    ListOutgoingCertificatesResponse (ListOutgoingCertificatesResponse'),
    newListOutgoingCertificatesResponse,

    -- ** ListPackageVersions (Paginated)
    ListPackageVersions (ListPackageVersions'),
    newListPackageVersions,
    ListPackageVersionsResponse (ListPackageVersionsResponse'),
    newListPackageVersionsResponse,

    -- ** ListPackages (Paginated)
    ListPackages (ListPackages'),
    newListPackages,
    ListPackagesResponse (ListPackagesResponse'),
    newListPackagesResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListPolicyVersions
    ListPolicyVersions (ListPolicyVersions'),
    newListPolicyVersions,
    ListPolicyVersionsResponse (ListPolicyVersionsResponse'),
    newListPolicyVersionsResponse,

    -- ** ListPrincipalThings (Paginated)
    ListPrincipalThings (ListPrincipalThings'),
    newListPrincipalThings,
    ListPrincipalThingsResponse (ListPrincipalThingsResponse'),
    newListPrincipalThingsResponse,

    -- ** ListProvisioningTemplateVersions (Paginated)
    ListProvisioningTemplateVersions (ListProvisioningTemplateVersions'),
    newListProvisioningTemplateVersions,
    ListProvisioningTemplateVersionsResponse (ListProvisioningTemplateVersionsResponse'),
    newListProvisioningTemplateVersionsResponse,

    -- ** ListProvisioningTemplates (Paginated)
    ListProvisioningTemplates (ListProvisioningTemplates'),
    newListProvisioningTemplates,
    ListProvisioningTemplatesResponse (ListProvisioningTemplatesResponse'),
    newListProvisioningTemplatesResponse,

    -- ** ListRelatedResourcesForAuditFinding (Paginated)
    ListRelatedResourcesForAuditFinding (ListRelatedResourcesForAuditFinding'),
    newListRelatedResourcesForAuditFinding,
    ListRelatedResourcesForAuditFindingResponse (ListRelatedResourcesForAuditFindingResponse'),
    newListRelatedResourcesForAuditFindingResponse,

    -- ** ListRoleAliases (Paginated)
    ListRoleAliases (ListRoleAliases'),
    newListRoleAliases,
    ListRoleAliasesResponse (ListRoleAliasesResponse'),
    newListRoleAliasesResponse,

    -- ** ListScheduledAudits (Paginated)
    ListScheduledAudits (ListScheduledAudits'),
    newListScheduledAudits,
    ListScheduledAuditsResponse (ListScheduledAuditsResponse'),
    newListScheduledAuditsResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** ListSecurityProfilesForTarget (Paginated)
    ListSecurityProfilesForTarget (ListSecurityProfilesForTarget'),
    newListSecurityProfilesForTarget,
    ListSecurityProfilesForTargetResponse (ListSecurityProfilesForTargetResponse'),
    newListSecurityProfilesForTargetResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTargetsForPolicy (Paginated)
    ListTargetsForPolicy (ListTargetsForPolicy'),
    newListTargetsForPolicy,
    ListTargetsForPolicyResponse (ListTargetsForPolicyResponse'),
    newListTargetsForPolicyResponse,

    -- ** ListTargetsForSecurityProfile (Paginated)
    ListTargetsForSecurityProfile (ListTargetsForSecurityProfile'),
    newListTargetsForSecurityProfile,
    ListTargetsForSecurityProfileResponse (ListTargetsForSecurityProfileResponse'),
    newListTargetsForSecurityProfileResponse,

    -- ** ListThingGroups (Paginated)
    ListThingGroups (ListThingGroups'),
    newListThingGroups,
    ListThingGroupsResponse (ListThingGroupsResponse'),
    newListThingGroupsResponse,

    -- ** ListThingGroupsForThing (Paginated)
    ListThingGroupsForThing (ListThingGroupsForThing'),
    newListThingGroupsForThing,
    ListThingGroupsForThingResponse (ListThingGroupsForThingResponse'),
    newListThingGroupsForThingResponse,

    -- ** ListThingPrincipals (Paginated)
    ListThingPrincipals (ListThingPrincipals'),
    newListThingPrincipals,
    ListThingPrincipalsResponse (ListThingPrincipalsResponse'),
    newListThingPrincipalsResponse,

    -- ** ListThingRegistrationTaskReports (Paginated)
    ListThingRegistrationTaskReports (ListThingRegistrationTaskReports'),
    newListThingRegistrationTaskReports,
    ListThingRegistrationTaskReportsResponse (ListThingRegistrationTaskReportsResponse'),
    newListThingRegistrationTaskReportsResponse,

    -- ** ListThingRegistrationTasks (Paginated)
    ListThingRegistrationTasks (ListThingRegistrationTasks'),
    newListThingRegistrationTasks,
    ListThingRegistrationTasksResponse (ListThingRegistrationTasksResponse'),
    newListThingRegistrationTasksResponse,

    -- ** ListThingTypes (Paginated)
    ListThingTypes (ListThingTypes'),
    newListThingTypes,
    ListThingTypesResponse (ListThingTypesResponse'),
    newListThingTypesResponse,

    -- ** ListThings (Paginated)
    ListThings (ListThings'),
    newListThings,
    ListThingsResponse (ListThingsResponse'),
    newListThingsResponse,

    -- ** ListThingsInBillingGroup (Paginated)
    ListThingsInBillingGroup (ListThingsInBillingGroup'),
    newListThingsInBillingGroup,
    ListThingsInBillingGroupResponse (ListThingsInBillingGroupResponse'),
    newListThingsInBillingGroupResponse,

    -- ** ListThingsInThingGroup (Paginated)
    ListThingsInThingGroup (ListThingsInThingGroup'),
    newListThingsInThingGroup,
    ListThingsInThingGroupResponse (ListThingsInThingGroupResponse'),
    newListThingsInThingGroupResponse,

    -- ** ListTopicRuleDestinations (Paginated)
    ListTopicRuleDestinations (ListTopicRuleDestinations'),
    newListTopicRuleDestinations,
    ListTopicRuleDestinationsResponse (ListTopicRuleDestinationsResponse'),
    newListTopicRuleDestinationsResponse,

    -- ** ListTopicRules (Paginated)
    ListTopicRules (ListTopicRules'),
    newListTopicRules,
    ListTopicRulesResponse (ListTopicRulesResponse'),
    newListTopicRulesResponse,

    -- ** ListV2LoggingLevels (Paginated)
    ListV2LoggingLevels (ListV2LoggingLevels'),
    newListV2LoggingLevels,
    ListV2LoggingLevelsResponse (ListV2LoggingLevelsResponse'),
    newListV2LoggingLevelsResponse,

    -- ** ListViolationEvents (Paginated)
    ListViolationEvents (ListViolationEvents'),
    newListViolationEvents,
    ListViolationEventsResponse (ListViolationEventsResponse'),
    newListViolationEventsResponse,

    -- ** PutVerificationStateOnViolation
    PutVerificationStateOnViolation (PutVerificationStateOnViolation'),
    newPutVerificationStateOnViolation,
    PutVerificationStateOnViolationResponse (PutVerificationStateOnViolationResponse'),
    newPutVerificationStateOnViolationResponse,

    -- ** RegisterCACertificate
    RegisterCACertificate (RegisterCACertificate'),
    newRegisterCACertificate,
    RegisterCACertificateResponse (RegisterCACertificateResponse'),
    newRegisterCACertificateResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** RegisterCertificateWithoutCA
    RegisterCertificateWithoutCA (RegisterCertificateWithoutCA'),
    newRegisterCertificateWithoutCA,
    RegisterCertificateWithoutCAResponse (RegisterCertificateWithoutCAResponse'),
    newRegisterCertificateWithoutCAResponse,

    -- ** RegisterThing
    RegisterThing (RegisterThing'),
    newRegisterThing,
    RegisterThingResponse (RegisterThingResponse'),
    newRegisterThingResponse,

    -- ** RejectCertificateTransfer
    RejectCertificateTransfer (RejectCertificateTransfer'),
    newRejectCertificateTransfer,
    RejectCertificateTransferResponse (RejectCertificateTransferResponse'),
    newRejectCertificateTransferResponse,

    -- ** RemoveThingFromBillingGroup
    RemoveThingFromBillingGroup (RemoveThingFromBillingGroup'),
    newRemoveThingFromBillingGroup,
    RemoveThingFromBillingGroupResponse (RemoveThingFromBillingGroupResponse'),
    newRemoveThingFromBillingGroupResponse,

    -- ** RemoveThingFromThingGroup
    RemoveThingFromThingGroup (RemoveThingFromThingGroup'),
    newRemoveThingFromThingGroup,
    RemoveThingFromThingGroupResponse (RemoveThingFromThingGroupResponse'),
    newRemoveThingFromThingGroupResponse,

    -- ** ReplaceTopicRule
    ReplaceTopicRule (ReplaceTopicRule'),
    newReplaceTopicRule,
    ReplaceTopicRuleResponse (ReplaceTopicRuleResponse'),
    newReplaceTopicRuleResponse,

    -- ** SearchIndex
    SearchIndex (SearchIndex'),
    newSearchIndex,
    SearchIndexResponse (SearchIndexResponse'),
    newSearchIndexResponse,

    -- ** SetDefaultAuthorizer
    SetDefaultAuthorizer (SetDefaultAuthorizer'),
    newSetDefaultAuthorizer,
    SetDefaultAuthorizerResponse (SetDefaultAuthorizerResponse'),
    newSetDefaultAuthorizerResponse,

    -- ** SetDefaultPolicyVersion
    SetDefaultPolicyVersion (SetDefaultPolicyVersion'),
    newSetDefaultPolicyVersion,
    SetDefaultPolicyVersionResponse (SetDefaultPolicyVersionResponse'),
    newSetDefaultPolicyVersionResponse,

    -- ** SetLoggingOptions
    SetLoggingOptions (SetLoggingOptions'),
    newSetLoggingOptions,
    SetLoggingOptionsResponse (SetLoggingOptionsResponse'),
    newSetLoggingOptionsResponse,

    -- ** SetV2LoggingLevel
    SetV2LoggingLevel (SetV2LoggingLevel'),
    newSetV2LoggingLevel,
    SetV2LoggingLevelResponse (SetV2LoggingLevelResponse'),
    newSetV2LoggingLevelResponse,

    -- ** SetV2LoggingOptions
    SetV2LoggingOptions (SetV2LoggingOptions'),
    newSetV2LoggingOptions,
    SetV2LoggingOptionsResponse (SetV2LoggingOptionsResponse'),
    newSetV2LoggingOptionsResponse,

    -- ** StartAuditMitigationActionsTask
    StartAuditMitigationActionsTask (StartAuditMitigationActionsTask'),
    newStartAuditMitigationActionsTask,
    StartAuditMitigationActionsTaskResponse (StartAuditMitigationActionsTaskResponse'),
    newStartAuditMitigationActionsTaskResponse,

    -- ** StartDetectMitigationActionsTask
    StartDetectMitigationActionsTask (StartDetectMitigationActionsTask'),
    newStartDetectMitigationActionsTask,
    StartDetectMitigationActionsTaskResponse (StartDetectMitigationActionsTaskResponse'),
    newStartDetectMitigationActionsTaskResponse,

    -- ** StartOnDemandAuditTask
    StartOnDemandAuditTask (StartOnDemandAuditTask'),
    newStartOnDemandAuditTask,
    StartOnDemandAuditTaskResponse (StartOnDemandAuditTaskResponse'),
    newStartOnDemandAuditTaskResponse,

    -- ** StartThingRegistrationTask
    StartThingRegistrationTask (StartThingRegistrationTask'),
    newStartThingRegistrationTask,
    StartThingRegistrationTaskResponse (StartThingRegistrationTaskResponse'),
    newStartThingRegistrationTaskResponse,

    -- ** StopThingRegistrationTask
    StopThingRegistrationTask (StopThingRegistrationTask'),
    newStopThingRegistrationTask,
    StopThingRegistrationTaskResponse (StopThingRegistrationTaskResponse'),
    newStopThingRegistrationTaskResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestAuthorization
    TestAuthorization (TestAuthorization'),
    newTestAuthorization,
    TestAuthorizationResponse (TestAuthorizationResponse'),
    newTestAuthorizationResponse,

    -- ** TestInvokeAuthorizer
    TestInvokeAuthorizer (TestInvokeAuthorizer'),
    newTestInvokeAuthorizer,
    TestInvokeAuthorizerResponse (TestInvokeAuthorizerResponse'),
    newTestInvokeAuthorizerResponse,

    -- ** TransferCertificate
    TransferCertificate (TransferCertificate'),
    newTransferCertificate,
    TransferCertificateResponse (TransferCertificateResponse'),
    newTransferCertificateResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccountAuditConfiguration
    UpdateAccountAuditConfiguration (UpdateAccountAuditConfiguration'),
    newUpdateAccountAuditConfiguration,
    UpdateAccountAuditConfigurationResponse (UpdateAccountAuditConfigurationResponse'),
    newUpdateAccountAuditConfigurationResponse,

    -- ** UpdateAuditSuppression
    UpdateAuditSuppression (UpdateAuditSuppression'),
    newUpdateAuditSuppression,
    UpdateAuditSuppressionResponse (UpdateAuditSuppressionResponse'),
    newUpdateAuditSuppressionResponse,

    -- ** UpdateAuthorizer
    UpdateAuthorizer (UpdateAuthorizer'),
    newUpdateAuthorizer,
    UpdateAuthorizerResponse (UpdateAuthorizerResponse'),
    newUpdateAuthorizerResponse,

    -- ** UpdateBillingGroup
    UpdateBillingGroup (UpdateBillingGroup'),
    newUpdateBillingGroup,
    UpdateBillingGroupResponse (UpdateBillingGroupResponse'),
    newUpdateBillingGroupResponse,

    -- ** UpdateCACertificate
    UpdateCACertificate (UpdateCACertificate'),
    newUpdateCACertificate,
    UpdateCACertificateResponse (UpdateCACertificateResponse'),
    newUpdateCACertificateResponse,

    -- ** UpdateCertificate
    UpdateCertificate (UpdateCertificate'),
    newUpdateCertificate,
    UpdateCertificateResponse (UpdateCertificateResponse'),
    newUpdateCertificateResponse,

    -- ** UpdateCustomMetric
    UpdateCustomMetric (UpdateCustomMetric'),
    newUpdateCustomMetric,
    UpdateCustomMetricResponse (UpdateCustomMetricResponse'),
    newUpdateCustomMetricResponse,

    -- ** UpdateDimension
    UpdateDimension (UpdateDimension'),
    newUpdateDimension,
    UpdateDimensionResponse (UpdateDimensionResponse'),
    newUpdateDimensionResponse,

    -- ** UpdateDomainConfiguration
    UpdateDomainConfiguration (UpdateDomainConfiguration'),
    newUpdateDomainConfiguration,
    UpdateDomainConfigurationResponse (UpdateDomainConfigurationResponse'),
    newUpdateDomainConfigurationResponse,

    -- ** UpdateDynamicThingGroup
    UpdateDynamicThingGroup (UpdateDynamicThingGroup'),
    newUpdateDynamicThingGroup,
    UpdateDynamicThingGroupResponse (UpdateDynamicThingGroupResponse'),
    newUpdateDynamicThingGroupResponse,

    -- ** UpdateEventConfigurations
    UpdateEventConfigurations (UpdateEventConfigurations'),
    newUpdateEventConfigurations,
    UpdateEventConfigurationsResponse (UpdateEventConfigurationsResponse'),
    newUpdateEventConfigurationsResponse,

    -- ** UpdateFleetMetric
    UpdateFleetMetric (UpdateFleetMetric'),
    newUpdateFleetMetric,
    UpdateFleetMetricResponse (UpdateFleetMetricResponse'),
    newUpdateFleetMetricResponse,

    -- ** UpdateIndexingConfiguration
    UpdateIndexingConfiguration (UpdateIndexingConfiguration'),
    newUpdateIndexingConfiguration,
    UpdateIndexingConfigurationResponse (UpdateIndexingConfigurationResponse'),
    newUpdateIndexingConfigurationResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** UpdateMitigationAction
    UpdateMitigationAction (UpdateMitigationAction'),
    newUpdateMitigationAction,
    UpdateMitigationActionResponse (UpdateMitigationActionResponse'),
    newUpdateMitigationActionResponse,

    -- ** UpdatePackage
    UpdatePackage (UpdatePackage'),
    newUpdatePackage,
    UpdatePackageResponse (UpdatePackageResponse'),
    newUpdatePackageResponse,

    -- ** UpdatePackageConfiguration
    UpdatePackageConfiguration (UpdatePackageConfiguration'),
    newUpdatePackageConfiguration,
    UpdatePackageConfigurationResponse (UpdatePackageConfigurationResponse'),
    newUpdatePackageConfigurationResponse,

    -- ** UpdatePackageVersion
    UpdatePackageVersion (UpdatePackageVersion'),
    newUpdatePackageVersion,
    UpdatePackageVersionResponse (UpdatePackageVersionResponse'),
    newUpdatePackageVersionResponse,

    -- ** UpdateProvisioningTemplate
    UpdateProvisioningTemplate (UpdateProvisioningTemplate'),
    newUpdateProvisioningTemplate,
    UpdateProvisioningTemplateResponse (UpdateProvisioningTemplateResponse'),
    newUpdateProvisioningTemplateResponse,

    -- ** UpdateRoleAlias
    UpdateRoleAlias (UpdateRoleAlias'),
    newUpdateRoleAlias,
    UpdateRoleAliasResponse (UpdateRoleAliasResponse'),
    newUpdateRoleAliasResponse,

    -- ** UpdateScheduledAudit
    UpdateScheduledAudit (UpdateScheduledAudit'),
    newUpdateScheduledAudit,
    UpdateScheduledAuditResponse (UpdateScheduledAuditResponse'),
    newUpdateScheduledAuditResponse,

    -- ** UpdateSecurityProfile
    UpdateSecurityProfile (UpdateSecurityProfile'),
    newUpdateSecurityProfile,
    UpdateSecurityProfileResponse (UpdateSecurityProfileResponse'),
    newUpdateSecurityProfileResponse,

    -- ** UpdateStream
    UpdateStream (UpdateStream'),
    newUpdateStream,
    UpdateStreamResponse (UpdateStreamResponse'),
    newUpdateStreamResponse,

    -- ** UpdateThing
    UpdateThing (UpdateThing'),
    newUpdateThing,
    UpdateThingResponse (UpdateThingResponse'),
    newUpdateThingResponse,

    -- ** UpdateThingGroup
    UpdateThingGroup (UpdateThingGroup'),
    newUpdateThingGroup,
    UpdateThingGroupResponse (UpdateThingGroupResponse'),
    newUpdateThingGroupResponse,

    -- ** UpdateThingGroupsForThing
    UpdateThingGroupsForThing (UpdateThingGroupsForThing'),
    newUpdateThingGroupsForThing,
    UpdateThingGroupsForThingResponse (UpdateThingGroupsForThingResponse'),
    newUpdateThingGroupsForThingResponse,

    -- ** UpdateTopicRuleDestination
    UpdateTopicRuleDestination (UpdateTopicRuleDestination'),
    newUpdateTopicRuleDestination,
    UpdateTopicRuleDestinationResponse (UpdateTopicRuleDestinationResponse'),
    newUpdateTopicRuleDestinationResponse,

    -- ** ValidateSecurityProfileBehaviors
    ValidateSecurityProfileBehaviors (ValidateSecurityProfileBehaviors'),
    newValidateSecurityProfileBehaviors,
    ValidateSecurityProfileBehaviorsResponse (ValidateSecurityProfileBehaviorsResponse'),
    newValidateSecurityProfileBehaviorsResponse,

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

    -- ** DeviceDefenderIndexingMode
    DeviceDefenderIndexingMode (..),

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

    -- ** JobEndBehavior
    JobEndBehavior (..),

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

    -- ** NamedShadowIndexingMode
    NamedShadowIndexingMode (..),

    -- ** OTAUpdateStatus
    OTAUpdateStatus (..),

    -- ** PackageVersionAction
    PackageVersionAction (..),

    -- ** PackageVersionStatus
    PackageVersionStatus (..),

    -- ** PolicyTemplateName
    PolicyTemplateName (..),

    -- ** Protocol
    Protocol (..),

    -- ** ReportType
    ReportType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RetryableFailureType
    RetryableFailureType (..),

    -- ** ServerCertificateStatus
    ServerCertificateStatus (..),

    -- ** ServiceType
    ServiceType (..),

    -- ** TargetSelection
    TargetSelection (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** TemplateType
    TemplateType (..),

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

    -- ** DocumentParameter
    DocumentParameter (DocumentParameter'),
    newDocumentParameter,

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

    -- ** IndexingFilter
    IndexingFilter (IndexingFilter'),
    newIndexingFilter,

    -- ** IotAnalyticsAction
    IotAnalyticsAction (IotAnalyticsAction'),
    newIotAnalyticsAction,

    -- ** IotEventsAction
    IotEventsAction (IotEventsAction'),
    newIotEventsAction,

    -- ** IotSiteWiseAction
    IotSiteWiseAction (IotSiteWiseAction'),
    newIotSiteWiseAction,

    -- ** IssuerCertificateIdentifier
    IssuerCertificateIdentifier (IssuerCertificateIdentifier'),
    newIssuerCertificateIdentifier,

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

    -- ** JobExecutionsRetryConfig
    JobExecutionsRetryConfig (JobExecutionsRetryConfig'),
    newJobExecutionsRetryConfig,

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

    -- ** LocationAction
    LocationAction (LocationAction'),
    newLocationAction,

    -- ** LocationTimestamp
    LocationTimestamp (LocationTimestamp'),
    newLocationTimestamp,

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

    -- ** MaintenanceWindow
    MaintenanceWindow (MaintenanceWindow'),
    newMaintenanceWindow,

    -- ** ManagedJobTemplateSummary
    ManagedJobTemplateSummary (ManagedJobTemplateSummary'),
    newManagedJobTemplateSummary,

    -- ** MetricDatum
    MetricDatum (MetricDatum'),
    newMetricDatum,

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

    -- ** MqttHeaders
    MqttHeaders (MqttHeaders'),
    newMqttHeaders,

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

    -- ** PackageSummary
    PackageSummary (PackageSummary'),
    newPackageSummary,

    -- ** PackageVersionSummary
    PackageVersionSummary (PackageVersionSummary'),
    newPackageVersionSummary,

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

    -- ** RetryCriteria
    RetryCriteria (RetryCriteria'),
    newRetryCriteria,

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

    -- ** ScheduledJobRollout
    ScheduledJobRollout (ScheduledJobRollout'),
    newScheduledJobRollout,

    -- ** SchedulingConfig
    SchedulingConfig (SchedulingConfig'),
    newSchedulingConfig,

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

    -- ** TlsConfig
    TlsConfig (TlsConfig'),
    newTlsConfig,

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

    -- ** UserProperty
    UserProperty (UserProperty'),
    newUserProperty,

    -- ** ValidationError
    ValidationError (ValidationError'),
    newValidationError,

    -- ** VersionUpdateByJobsConfig
    VersionUpdateByJobsConfig (VersionUpdateByJobsConfig'),
    newVersionUpdateByJobsConfig,

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

import Amazonka.IoT.AcceptCertificateTransfer
import Amazonka.IoT.AddThingToBillingGroup
import Amazonka.IoT.AddThingToThingGroup
import Amazonka.IoT.AssociateTargetsWithJob
import Amazonka.IoT.AttachPolicy
import Amazonka.IoT.AttachSecurityProfile
import Amazonka.IoT.AttachThingPrincipal
import Amazonka.IoT.CancelAuditMitigationActionsTask
import Amazonka.IoT.CancelAuditTask
import Amazonka.IoT.CancelCertificateTransfer
import Amazonka.IoT.CancelDetectMitigationActionsTask
import Amazonka.IoT.CancelJob
import Amazonka.IoT.CancelJobExecution
import Amazonka.IoT.ClearDefaultAuthorizer
import Amazonka.IoT.ConfirmTopicRuleDestination
import Amazonka.IoT.CreateAuditSuppression
import Amazonka.IoT.CreateAuthorizer
import Amazonka.IoT.CreateBillingGroup
import Amazonka.IoT.CreateCertificateFromCsr
import Amazonka.IoT.CreateCustomMetric
import Amazonka.IoT.CreateDimension
import Amazonka.IoT.CreateDomainConfiguration
import Amazonka.IoT.CreateDynamicThingGroup
import Amazonka.IoT.CreateFleetMetric
import Amazonka.IoT.CreateJob
import Amazonka.IoT.CreateJobTemplate
import Amazonka.IoT.CreateKeysAndCertificate
import Amazonka.IoT.CreateMitigationAction
import Amazonka.IoT.CreateOTAUpdate
import Amazonka.IoT.CreatePackage
import Amazonka.IoT.CreatePackageVersion
import Amazonka.IoT.CreatePolicy
import Amazonka.IoT.CreatePolicyVersion
import Amazonka.IoT.CreateProvisioningClaim
import Amazonka.IoT.CreateProvisioningTemplate
import Amazonka.IoT.CreateProvisioningTemplateVersion
import Amazonka.IoT.CreateRoleAlias
import Amazonka.IoT.CreateScheduledAudit
import Amazonka.IoT.CreateSecurityProfile
import Amazonka.IoT.CreateStream
import Amazonka.IoT.CreateThing
import Amazonka.IoT.CreateThingGroup
import Amazonka.IoT.CreateThingType
import Amazonka.IoT.CreateTopicRule
import Amazonka.IoT.CreateTopicRuleDestination
import Amazonka.IoT.DeleteAccountAuditConfiguration
import Amazonka.IoT.DeleteAuditSuppression
import Amazonka.IoT.DeleteAuthorizer
import Amazonka.IoT.DeleteBillingGroup
import Amazonka.IoT.DeleteCACertificate
import Amazonka.IoT.DeleteCertificate
import Amazonka.IoT.DeleteCustomMetric
import Amazonka.IoT.DeleteDimension
import Amazonka.IoT.DeleteDomainConfiguration
import Amazonka.IoT.DeleteDynamicThingGroup
import Amazonka.IoT.DeleteFleetMetric
import Amazonka.IoT.DeleteJob
import Amazonka.IoT.DeleteJobExecution
import Amazonka.IoT.DeleteJobTemplate
import Amazonka.IoT.DeleteMitigationAction
import Amazonka.IoT.DeleteOTAUpdate
import Amazonka.IoT.DeletePackage
import Amazonka.IoT.DeletePackageVersion
import Amazonka.IoT.DeletePolicy
import Amazonka.IoT.DeletePolicyVersion
import Amazonka.IoT.DeleteProvisioningTemplate
import Amazonka.IoT.DeleteProvisioningTemplateVersion
import Amazonka.IoT.DeleteRegistrationCode
import Amazonka.IoT.DeleteRoleAlias
import Amazonka.IoT.DeleteScheduledAudit
import Amazonka.IoT.DeleteSecurityProfile
import Amazonka.IoT.DeleteStream
import Amazonka.IoT.DeleteThing
import Amazonka.IoT.DeleteThingGroup
import Amazonka.IoT.DeleteThingType
import Amazonka.IoT.DeleteTopicRule
import Amazonka.IoT.DeleteTopicRuleDestination
import Amazonka.IoT.DeleteV2LoggingLevel
import Amazonka.IoT.DeprecateThingType
import Amazonka.IoT.DescribeAccountAuditConfiguration
import Amazonka.IoT.DescribeAuditFinding
import Amazonka.IoT.DescribeAuditMitigationActionsTask
import Amazonka.IoT.DescribeAuditSuppression
import Amazonka.IoT.DescribeAuditTask
import Amazonka.IoT.DescribeAuthorizer
import Amazonka.IoT.DescribeBillingGroup
import Amazonka.IoT.DescribeCACertificate
import Amazonka.IoT.DescribeCertificate
import Amazonka.IoT.DescribeCustomMetric
import Amazonka.IoT.DescribeDefaultAuthorizer
import Amazonka.IoT.DescribeDetectMitigationActionsTask
import Amazonka.IoT.DescribeDimension
import Amazonka.IoT.DescribeDomainConfiguration
import Amazonka.IoT.DescribeEndpoint
import Amazonka.IoT.DescribeEventConfigurations
import Amazonka.IoT.DescribeFleetMetric
import Amazonka.IoT.DescribeIndex
import Amazonka.IoT.DescribeJob
import Amazonka.IoT.DescribeJobExecution
import Amazonka.IoT.DescribeJobTemplate
import Amazonka.IoT.DescribeManagedJobTemplate
import Amazonka.IoT.DescribeMitigationAction
import Amazonka.IoT.DescribeProvisioningTemplate
import Amazonka.IoT.DescribeProvisioningTemplateVersion
import Amazonka.IoT.DescribeRoleAlias
import Amazonka.IoT.DescribeScheduledAudit
import Amazonka.IoT.DescribeSecurityProfile
import Amazonka.IoT.DescribeStream
import Amazonka.IoT.DescribeThing
import Amazonka.IoT.DescribeThingGroup
import Amazonka.IoT.DescribeThingRegistrationTask
import Amazonka.IoT.DescribeThingType
import Amazonka.IoT.DetachPolicy
import Amazonka.IoT.DetachSecurityProfile
import Amazonka.IoT.DetachThingPrincipal
import Amazonka.IoT.DisableTopicRule
import Amazonka.IoT.EnableTopicRule
import Amazonka.IoT.GetBehaviorModelTrainingSummaries
import Amazonka.IoT.GetBucketsAggregation
import Amazonka.IoT.GetCardinality
import Amazonka.IoT.GetEffectivePolicies
import Amazonka.IoT.GetIndexingConfiguration
import Amazonka.IoT.GetJobDocument
import Amazonka.IoT.GetLoggingOptions
import Amazonka.IoT.GetOTAUpdate
import Amazonka.IoT.GetPackage
import Amazonka.IoT.GetPackageConfiguration
import Amazonka.IoT.GetPackageVersion
import Amazonka.IoT.GetPercentiles
import Amazonka.IoT.GetPolicy
import Amazonka.IoT.GetPolicyVersion
import Amazonka.IoT.GetRegistrationCode
import Amazonka.IoT.GetStatistics
import Amazonka.IoT.GetTopicRule
import Amazonka.IoT.GetTopicRuleDestination
import Amazonka.IoT.GetV2LoggingOptions
import Amazonka.IoT.Lens
import Amazonka.IoT.ListActiveViolations
import Amazonka.IoT.ListAttachedPolicies
import Amazonka.IoT.ListAuditFindings
import Amazonka.IoT.ListAuditMitigationActionsExecutions
import Amazonka.IoT.ListAuditMitigationActionsTasks
import Amazonka.IoT.ListAuditSuppressions
import Amazonka.IoT.ListAuditTasks
import Amazonka.IoT.ListAuthorizers
import Amazonka.IoT.ListBillingGroups
import Amazonka.IoT.ListCACertificates
import Amazonka.IoT.ListCertificates
import Amazonka.IoT.ListCertificatesByCA
import Amazonka.IoT.ListCustomMetrics
import Amazonka.IoT.ListDetectMitigationActionsExecutions
import Amazonka.IoT.ListDetectMitigationActionsTasks
import Amazonka.IoT.ListDimensions
import Amazonka.IoT.ListDomainConfigurations
import Amazonka.IoT.ListFleetMetrics
import Amazonka.IoT.ListIndices
import Amazonka.IoT.ListJobExecutionsForJob
import Amazonka.IoT.ListJobExecutionsForThing
import Amazonka.IoT.ListJobTemplates
import Amazonka.IoT.ListJobs
import Amazonka.IoT.ListManagedJobTemplates
import Amazonka.IoT.ListMetricValues
import Amazonka.IoT.ListMitigationActions
import Amazonka.IoT.ListOTAUpdates
import Amazonka.IoT.ListOutgoingCertificates
import Amazonka.IoT.ListPackageVersions
import Amazonka.IoT.ListPackages
import Amazonka.IoT.ListPolicies
import Amazonka.IoT.ListPolicyVersions
import Amazonka.IoT.ListPrincipalThings
import Amazonka.IoT.ListProvisioningTemplateVersions
import Amazonka.IoT.ListProvisioningTemplates
import Amazonka.IoT.ListRelatedResourcesForAuditFinding
import Amazonka.IoT.ListRoleAliases
import Amazonka.IoT.ListScheduledAudits
import Amazonka.IoT.ListSecurityProfiles
import Amazonka.IoT.ListSecurityProfilesForTarget
import Amazonka.IoT.ListStreams
import Amazonka.IoT.ListTagsForResource
import Amazonka.IoT.ListTargetsForPolicy
import Amazonka.IoT.ListTargetsForSecurityProfile
import Amazonka.IoT.ListThingGroups
import Amazonka.IoT.ListThingGroupsForThing
import Amazonka.IoT.ListThingPrincipals
import Amazonka.IoT.ListThingRegistrationTaskReports
import Amazonka.IoT.ListThingRegistrationTasks
import Amazonka.IoT.ListThingTypes
import Amazonka.IoT.ListThings
import Amazonka.IoT.ListThingsInBillingGroup
import Amazonka.IoT.ListThingsInThingGroup
import Amazonka.IoT.ListTopicRuleDestinations
import Amazonka.IoT.ListTopicRules
import Amazonka.IoT.ListV2LoggingLevels
import Amazonka.IoT.ListViolationEvents
import Amazonka.IoT.PutVerificationStateOnViolation
import Amazonka.IoT.RegisterCACertificate
import Amazonka.IoT.RegisterCertificate
import Amazonka.IoT.RegisterCertificateWithoutCA
import Amazonka.IoT.RegisterThing
import Amazonka.IoT.RejectCertificateTransfer
import Amazonka.IoT.RemoveThingFromBillingGroup
import Amazonka.IoT.RemoveThingFromThingGroup
import Amazonka.IoT.ReplaceTopicRule
import Amazonka.IoT.SearchIndex
import Amazonka.IoT.SetDefaultAuthorizer
import Amazonka.IoT.SetDefaultPolicyVersion
import Amazonka.IoT.SetLoggingOptions
import Amazonka.IoT.SetV2LoggingLevel
import Amazonka.IoT.SetV2LoggingOptions
import Amazonka.IoT.StartAuditMitigationActionsTask
import Amazonka.IoT.StartDetectMitigationActionsTask
import Amazonka.IoT.StartOnDemandAuditTask
import Amazonka.IoT.StartThingRegistrationTask
import Amazonka.IoT.StopThingRegistrationTask
import Amazonka.IoT.TagResource
import Amazonka.IoT.TestAuthorization
import Amazonka.IoT.TestInvokeAuthorizer
import Amazonka.IoT.TransferCertificate
import Amazonka.IoT.Types
import Amazonka.IoT.UntagResource
import Amazonka.IoT.UpdateAccountAuditConfiguration
import Amazonka.IoT.UpdateAuditSuppression
import Amazonka.IoT.UpdateAuthorizer
import Amazonka.IoT.UpdateBillingGroup
import Amazonka.IoT.UpdateCACertificate
import Amazonka.IoT.UpdateCertificate
import Amazonka.IoT.UpdateCustomMetric
import Amazonka.IoT.UpdateDimension
import Amazonka.IoT.UpdateDomainConfiguration
import Amazonka.IoT.UpdateDynamicThingGroup
import Amazonka.IoT.UpdateEventConfigurations
import Amazonka.IoT.UpdateFleetMetric
import Amazonka.IoT.UpdateIndexingConfiguration
import Amazonka.IoT.UpdateJob
import Amazonka.IoT.UpdateMitigationAction
import Amazonka.IoT.UpdatePackage
import Amazonka.IoT.UpdatePackageConfiguration
import Amazonka.IoT.UpdatePackageVersion
import Amazonka.IoT.UpdateProvisioningTemplate
import Amazonka.IoT.UpdateRoleAlias
import Amazonka.IoT.UpdateScheduledAudit
import Amazonka.IoT.UpdateSecurityProfile
import Amazonka.IoT.UpdateStream
import Amazonka.IoT.UpdateThing
import Amazonka.IoT.UpdateThingGroup
import Amazonka.IoT.UpdateThingGroupsForThing
import Amazonka.IoT.UpdateTopicRuleDestination
import Amazonka.IoT.ValidateSecurityProfileBehaviors
import Amazonka.IoT.Waiters

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

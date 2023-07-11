{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Lens
  ( -- * Operations

    -- ** AcceptCertificateTransfer
    acceptCertificateTransfer_setAsActive,
    acceptCertificateTransfer_certificateId,

    -- ** AddThingToBillingGroup
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_billingGroupName,
    addThingToBillingGroup_thingArn,
    addThingToBillingGroup_thingName,
    addThingToBillingGroupResponse_httpStatus,

    -- ** AddThingToThingGroup
    addThingToThingGroup_overrideDynamicGroups,
    addThingToThingGroup_thingArn,
    addThingToThingGroup_thingGroupArn,
    addThingToThingGroup_thingGroupName,
    addThingToThingGroup_thingName,
    addThingToThingGroupResponse_httpStatus,

    -- ** AssociateTargetsWithJob
    associateTargetsWithJob_comment,
    associateTargetsWithJob_namespaceId,
    associateTargetsWithJob_targets,
    associateTargetsWithJob_jobId,
    associateTargetsWithJobResponse_description,
    associateTargetsWithJobResponse_jobArn,
    associateTargetsWithJobResponse_jobId,
    associateTargetsWithJobResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_policyName,
    attachPolicy_target,

    -- ** AttachSecurityProfile
    attachSecurityProfile_securityProfileName,
    attachSecurityProfile_securityProfileTargetArn,
    attachSecurityProfileResponse_httpStatus,

    -- ** AttachThingPrincipal
    attachThingPrincipal_thingName,
    attachThingPrincipal_principal,
    attachThingPrincipalResponse_httpStatus,

    -- ** CancelAuditMitigationActionsTask
    cancelAuditMitigationActionsTask_taskId,
    cancelAuditMitigationActionsTaskResponse_httpStatus,

    -- ** CancelAuditTask
    cancelAuditTask_taskId,
    cancelAuditTaskResponse_httpStatus,

    -- ** CancelCertificateTransfer
    cancelCertificateTransfer_certificateId,

    -- ** CancelDetectMitigationActionsTask
    cancelDetectMitigationActionsTask_taskId,
    cancelDetectMitigationActionsTaskResponse_httpStatus,

    -- ** CancelJob
    cancelJob_comment,
    cancelJob_force,
    cancelJob_reasonCode,
    cancelJob_jobId,
    cancelJobResponse_description,
    cancelJobResponse_jobArn,
    cancelJobResponse_jobId,
    cancelJobResponse_httpStatus,

    -- ** CancelJobExecution
    cancelJobExecution_expectedVersion,
    cancelJobExecution_force,
    cancelJobExecution_statusDetails,
    cancelJobExecution_jobId,
    cancelJobExecution_thingName,

    -- ** ClearDefaultAuthorizer
    clearDefaultAuthorizerResponse_httpStatus,

    -- ** ConfirmTopicRuleDestination
    confirmTopicRuleDestination_confirmationToken,
    confirmTopicRuleDestinationResponse_httpStatus,

    -- ** CreateAuditSuppression
    createAuditSuppression_description,
    createAuditSuppression_expirationDate,
    createAuditSuppression_suppressIndefinitely,
    createAuditSuppression_checkName,
    createAuditSuppression_resourceIdentifier,
    createAuditSuppression_clientRequestToken,
    createAuditSuppressionResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_enableCachingForHttp,
    createAuthorizer_signingDisabled,
    createAuthorizer_status,
    createAuthorizer_tags,
    createAuthorizer_tokenKeyName,
    createAuthorizer_tokenSigningPublicKeys,
    createAuthorizer_authorizerName,
    createAuthorizer_authorizerFunctionArn,
    createAuthorizerResponse_authorizerArn,
    createAuthorizerResponse_authorizerName,
    createAuthorizerResponse_httpStatus,

    -- ** CreateBillingGroup
    createBillingGroup_billingGroupProperties,
    createBillingGroup_tags,
    createBillingGroup_billingGroupName,
    createBillingGroupResponse_billingGroupArn,
    createBillingGroupResponse_billingGroupId,
    createBillingGroupResponse_billingGroupName,
    createBillingGroupResponse_httpStatus,

    -- ** CreateCertificateFromCsr
    createCertificateFromCsr_setAsActive,
    createCertificateFromCsr_certificateSigningRequest,
    createCertificateFromCsrResponse_certificateArn,
    createCertificateFromCsrResponse_certificateId,
    createCertificateFromCsrResponse_certificatePem,
    createCertificateFromCsrResponse_httpStatus,

    -- ** CreateCustomMetric
    createCustomMetric_displayName,
    createCustomMetric_tags,
    createCustomMetric_metricName,
    createCustomMetric_metricType,
    createCustomMetric_clientRequestToken,
    createCustomMetricResponse_metricArn,
    createCustomMetricResponse_metricName,
    createCustomMetricResponse_httpStatus,

    -- ** CreateDimension
    createDimension_tags,
    createDimension_name,
    createDimension_type,
    createDimension_stringValues,
    createDimension_clientRequestToken,
    createDimensionResponse_arn,
    createDimensionResponse_name,
    createDimensionResponse_httpStatus,

    -- ** CreateDomainConfiguration
    createDomainConfiguration_authorizerConfig,
    createDomainConfiguration_domainName,
    createDomainConfiguration_serverCertificateArns,
    createDomainConfiguration_serviceType,
    createDomainConfiguration_tags,
    createDomainConfiguration_validationCertificateArn,
    createDomainConfiguration_domainConfigurationName,
    createDomainConfigurationResponse_domainConfigurationArn,
    createDomainConfigurationResponse_domainConfigurationName,
    createDomainConfigurationResponse_httpStatus,

    -- ** CreateDynamicThingGroup
    createDynamicThingGroup_indexName,
    createDynamicThingGroup_queryVersion,
    createDynamicThingGroup_tags,
    createDynamicThingGroup_thingGroupProperties,
    createDynamicThingGroup_thingGroupName,
    createDynamicThingGroup_queryString,
    createDynamicThingGroupResponse_indexName,
    createDynamicThingGroupResponse_queryString,
    createDynamicThingGroupResponse_queryVersion,
    createDynamicThingGroupResponse_thingGroupArn,
    createDynamicThingGroupResponse_thingGroupId,
    createDynamicThingGroupResponse_thingGroupName,
    createDynamicThingGroupResponse_httpStatus,

    -- ** CreateFleetMetric
    createFleetMetric_description,
    createFleetMetric_indexName,
    createFleetMetric_queryVersion,
    createFleetMetric_tags,
    createFleetMetric_unit,
    createFleetMetric_metricName,
    createFleetMetric_queryString,
    createFleetMetric_aggregationType,
    createFleetMetric_period,
    createFleetMetric_aggregationField,
    createFleetMetricResponse_metricArn,
    createFleetMetricResponse_metricName,
    createFleetMetricResponse_httpStatus,

    -- ** CreateJob
    createJob_abortConfig,
    createJob_description,
    createJob_document,
    createJob_documentParameters,
    createJob_documentSource,
    createJob_jobExecutionsRetryConfig,
    createJob_jobExecutionsRolloutConfig,
    createJob_jobTemplateArn,
    createJob_namespaceId,
    createJob_presignedUrlConfig,
    createJob_schedulingConfig,
    createJob_tags,
    createJob_targetSelection,
    createJob_timeoutConfig,
    createJob_jobId,
    createJob_targets,
    createJobResponse_description,
    createJobResponse_jobArn,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- ** CreateJobTemplate
    createJobTemplate_abortConfig,
    createJobTemplate_document,
    createJobTemplate_documentSource,
    createJobTemplate_jobArn,
    createJobTemplate_jobExecutionsRetryConfig,
    createJobTemplate_jobExecutionsRolloutConfig,
    createJobTemplate_presignedUrlConfig,
    createJobTemplate_tags,
    createJobTemplate_timeoutConfig,
    createJobTemplate_jobTemplateId,
    createJobTemplate_description,
    createJobTemplateResponse_jobTemplateArn,
    createJobTemplateResponse_jobTemplateId,
    createJobTemplateResponse_httpStatus,

    -- ** CreateKeysAndCertificate
    createKeysAndCertificate_setAsActive,
    createKeysAndCertificateResponse_certificateArn,
    createKeysAndCertificateResponse_certificateId,
    createKeysAndCertificateResponse_certificatePem,
    createKeysAndCertificateResponse_keyPair,
    createKeysAndCertificateResponse_httpStatus,

    -- ** CreateMitigationAction
    createMitigationAction_tags,
    createMitigationAction_actionName,
    createMitigationAction_roleArn,
    createMitigationAction_actionParams,
    createMitigationActionResponse_actionArn,
    createMitigationActionResponse_actionId,
    createMitigationActionResponse_httpStatus,

    -- ** CreateOTAUpdate
    createOTAUpdate_additionalParameters,
    createOTAUpdate_awsJobAbortConfig,
    createOTAUpdate_awsJobExecutionsRolloutConfig,
    createOTAUpdate_awsJobPresignedUrlConfig,
    createOTAUpdate_awsJobTimeoutConfig,
    createOTAUpdate_description,
    createOTAUpdate_protocols,
    createOTAUpdate_tags,
    createOTAUpdate_targetSelection,
    createOTAUpdate_otaUpdateId,
    createOTAUpdate_targets,
    createOTAUpdate_files,
    createOTAUpdate_roleArn,
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_otaUpdateArn,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_otaUpdateStatus,
    createOTAUpdateResponse_httpStatus,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,
    createPolicyResponse_policyArn,
    createPolicyResponse_policyDocument,
    createPolicyResponse_policyName,
    createPolicyResponse_policyVersionId,
    createPolicyResponse_httpStatus,

    -- ** CreatePolicyVersion
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyName,
    createPolicyVersion_policyDocument,
    createPolicyVersionResponse_isDefaultVersion,
    createPolicyVersionResponse_policyArn,
    createPolicyVersionResponse_policyDocument,
    createPolicyVersionResponse_policyVersionId,
    createPolicyVersionResponse_httpStatus,

    -- ** CreateProvisioningClaim
    createProvisioningClaim_templateName,
    createProvisioningClaimResponse_certificateId,
    createProvisioningClaimResponse_certificatePem,
    createProvisioningClaimResponse_expiration,
    createProvisioningClaimResponse_keyPair,
    createProvisioningClaimResponse_httpStatus,

    -- ** CreateProvisioningTemplate
    createProvisioningTemplate_description,
    createProvisioningTemplate_enabled,
    createProvisioningTemplate_preProvisioningHook,
    createProvisioningTemplate_tags,
    createProvisioningTemplate_type,
    createProvisioningTemplate_templateName,
    createProvisioningTemplate_templateBody,
    createProvisioningTemplate_provisioningRoleArn,
    createProvisioningTemplateResponse_defaultVersionId,
    createProvisioningTemplateResponse_templateArn,
    createProvisioningTemplateResponse_templateName,
    createProvisioningTemplateResponse_httpStatus,

    -- ** CreateProvisioningTemplateVersion
    createProvisioningTemplateVersion_setAsDefault,
    createProvisioningTemplateVersion_templateName,
    createProvisioningTemplateVersion_templateBody,
    createProvisioningTemplateVersionResponse_isDefaultVersion,
    createProvisioningTemplateVersionResponse_templateArn,
    createProvisioningTemplateVersionResponse_templateName,
    createProvisioningTemplateVersionResponse_versionId,
    createProvisioningTemplateVersionResponse_httpStatus,

    -- ** CreateRoleAlias
    createRoleAlias_credentialDurationSeconds,
    createRoleAlias_tags,
    createRoleAlias_roleAlias,
    createRoleAlias_roleArn,
    createRoleAliasResponse_roleAlias,
    createRoleAliasResponse_roleAliasArn,
    createRoleAliasResponse_httpStatus,

    -- ** CreateScheduledAudit
    createScheduledAudit_dayOfMonth,
    createScheduledAudit_dayOfWeek,
    createScheduledAudit_tags,
    createScheduledAudit_frequency,
    createScheduledAudit_targetCheckNames,
    createScheduledAudit_scheduledAuditName,
    createScheduledAuditResponse_scheduledAuditArn,
    createScheduledAuditResponse_httpStatus,

    -- ** CreateSecurityProfile
    createSecurityProfile_additionalMetricsToRetain,
    createSecurityProfile_additionalMetricsToRetainV2,
    createSecurityProfile_alertTargets,
    createSecurityProfile_behaviors,
    createSecurityProfile_securityProfileDescription,
    createSecurityProfile_tags,
    createSecurityProfile_securityProfileName,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_securityProfileName,
    createSecurityProfileResponse_httpStatus,

    -- ** CreateStream
    createStream_description,
    createStream_tags,
    createStream_streamId,
    createStream_files,
    createStream_roleArn,
    createStreamResponse_description,
    createStreamResponse_streamArn,
    createStreamResponse_streamId,
    createStreamResponse_streamVersion,
    createStreamResponse_httpStatus,

    -- ** CreateThing
    createThing_attributePayload,
    createThing_billingGroupName,
    createThing_thingTypeName,
    createThing_thingName,
    createThingResponse_thingArn,
    createThingResponse_thingId,
    createThingResponse_thingName,
    createThingResponse_httpStatus,

    -- ** CreateThingGroup
    createThingGroup_parentGroupName,
    createThingGroup_tags,
    createThingGroup_thingGroupProperties,
    createThingGroup_thingGroupName,
    createThingGroupResponse_thingGroupArn,
    createThingGroupResponse_thingGroupId,
    createThingGroupResponse_thingGroupName,
    createThingGroupResponse_httpStatus,

    -- ** CreateThingType
    createThingType_tags,
    createThingType_thingTypeProperties,
    createThingType_thingTypeName,
    createThingTypeResponse_thingTypeArn,
    createThingTypeResponse_thingTypeId,
    createThingTypeResponse_thingTypeName,
    createThingTypeResponse_httpStatus,

    -- ** CreateTopicRule
    createTopicRule_tags,
    createTopicRule_ruleName,
    createTopicRule_topicRulePayload,

    -- ** CreateTopicRuleDestination
    createTopicRuleDestination_destinationConfiguration,
    createTopicRuleDestinationResponse_topicRuleDestination,
    createTopicRuleDestinationResponse_httpStatus,

    -- ** DeleteAccountAuditConfiguration
    deleteAccountAuditConfiguration_deleteScheduledAudits,
    deleteAccountAuditConfigurationResponse_httpStatus,

    -- ** DeleteAuditSuppression
    deleteAuditSuppression_checkName,
    deleteAuditSuppression_resourceIdentifier,
    deleteAuditSuppressionResponse_httpStatus,

    -- ** DeleteAuthorizer
    deleteAuthorizer_authorizerName,
    deleteAuthorizerResponse_httpStatus,

    -- ** DeleteBillingGroup
    deleteBillingGroup_expectedVersion,
    deleteBillingGroup_billingGroupName,
    deleteBillingGroupResponse_httpStatus,

    -- ** DeleteCACertificate
    deleteCACertificate_certificateId,
    deleteCACertificateResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_forceDelete,
    deleteCertificate_certificateId,

    -- ** DeleteCustomMetric
    deleteCustomMetric_metricName,
    deleteCustomMetricResponse_httpStatus,

    -- ** DeleteDimension
    deleteDimension_name,
    deleteDimensionResponse_httpStatus,

    -- ** DeleteDomainConfiguration
    deleteDomainConfiguration_domainConfigurationName,
    deleteDomainConfigurationResponse_httpStatus,

    -- ** DeleteDynamicThingGroup
    deleteDynamicThingGroup_expectedVersion,
    deleteDynamicThingGroup_thingGroupName,
    deleteDynamicThingGroupResponse_httpStatus,

    -- ** DeleteFleetMetric
    deleteFleetMetric_expectedVersion,
    deleteFleetMetric_metricName,

    -- ** DeleteJob
    deleteJob_force,
    deleteJob_namespaceId,
    deleteJob_jobId,

    -- ** DeleteJobExecution
    deleteJobExecution_force,
    deleteJobExecution_namespaceId,
    deleteJobExecution_jobId,
    deleteJobExecution_thingName,
    deleteJobExecution_executionNumber,

    -- ** DeleteJobTemplate
    deleteJobTemplate_jobTemplateId,

    -- ** DeleteMitigationAction
    deleteMitigationAction_actionName,
    deleteMitigationActionResponse_httpStatus,

    -- ** DeleteOTAUpdate
    deleteOTAUpdate_deleteStream,
    deleteOTAUpdate_forceDeleteAWSJob,
    deleteOTAUpdate_otaUpdateId,
    deleteOTAUpdateResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyName,

    -- ** DeletePolicyVersion
    deletePolicyVersion_policyName,
    deletePolicyVersion_policyVersionId,

    -- ** DeleteProvisioningTemplate
    deleteProvisioningTemplate_templateName,
    deleteProvisioningTemplateResponse_httpStatus,

    -- ** DeleteProvisioningTemplateVersion
    deleteProvisioningTemplateVersion_templateName,
    deleteProvisioningTemplateVersion_versionId,
    deleteProvisioningTemplateVersionResponse_httpStatus,

    -- ** DeleteRegistrationCode
    deleteRegistrationCodeResponse_httpStatus,

    -- ** DeleteRoleAlias
    deleteRoleAlias_roleAlias,
    deleteRoleAliasResponse_httpStatus,

    -- ** DeleteScheduledAudit
    deleteScheduledAudit_scheduledAuditName,
    deleteScheduledAuditResponse_httpStatus,

    -- ** DeleteSecurityProfile
    deleteSecurityProfile_expectedVersion,
    deleteSecurityProfile_securityProfileName,
    deleteSecurityProfileResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_streamId,
    deleteStreamResponse_httpStatus,

    -- ** DeleteThing
    deleteThing_expectedVersion,
    deleteThing_thingName,
    deleteThingResponse_httpStatus,

    -- ** DeleteThingGroup
    deleteThingGroup_expectedVersion,
    deleteThingGroup_thingGroupName,
    deleteThingGroupResponse_httpStatus,

    -- ** DeleteThingType
    deleteThingType_thingTypeName,
    deleteThingTypeResponse_httpStatus,

    -- ** DeleteTopicRule
    deleteTopicRule_ruleName,

    -- ** DeleteTopicRuleDestination
    deleteTopicRuleDestination_arn,
    deleteTopicRuleDestinationResponse_httpStatus,

    -- ** DeleteV2LoggingLevel
    deleteV2LoggingLevel_targetType,
    deleteV2LoggingLevel_targetName,

    -- ** DeprecateThingType
    deprecateThingType_undoDeprecate,
    deprecateThingType_thingTypeName,
    deprecateThingTypeResponse_httpStatus,

    -- ** DescribeAccountAuditConfiguration
    describeAccountAuditConfigurationResponse_auditCheckConfigurations,
    describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations,
    describeAccountAuditConfigurationResponse_roleArn,
    describeAccountAuditConfigurationResponse_httpStatus,

    -- ** DescribeAuditFinding
    describeAuditFinding_findingId,
    describeAuditFindingResponse_finding,
    describeAuditFindingResponse_httpStatus,

    -- ** DescribeAuditMitigationActionsTask
    describeAuditMitigationActionsTask_taskId,
    describeAuditMitigationActionsTaskResponse_actionsDefinition,
    describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping,
    describeAuditMitigationActionsTaskResponse_endTime,
    describeAuditMitigationActionsTaskResponse_startTime,
    describeAuditMitigationActionsTaskResponse_target,
    describeAuditMitigationActionsTaskResponse_taskStatistics,
    describeAuditMitigationActionsTaskResponse_taskStatus,
    describeAuditMitigationActionsTaskResponse_httpStatus,

    -- ** DescribeAuditSuppression
    describeAuditSuppression_checkName,
    describeAuditSuppression_resourceIdentifier,
    describeAuditSuppressionResponse_checkName,
    describeAuditSuppressionResponse_description,
    describeAuditSuppressionResponse_expirationDate,
    describeAuditSuppressionResponse_resourceIdentifier,
    describeAuditSuppressionResponse_suppressIndefinitely,
    describeAuditSuppressionResponse_httpStatus,

    -- ** DescribeAuditTask
    describeAuditTask_taskId,
    describeAuditTaskResponse_auditDetails,
    describeAuditTaskResponse_scheduledAuditName,
    describeAuditTaskResponse_taskStartTime,
    describeAuditTaskResponse_taskStatistics,
    describeAuditTaskResponse_taskStatus,
    describeAuditTaskResponse_taskType,
    describeAuditTaskResponse_httpStatus,

    -- ** DescribeAuthorizer
    describeAuthorizer_authorizerName,
    describeAuthorizerResponse_authorizerDescription,
    describeAuthorizerResponse_httpStatus,

    -- ** DescribeBillingGroup
    describeBillingGroup_billingGroupName,
    describeBillingGroupResponse_billingGroupArn,
    describeBillingGroupResponse_billingGroupId,
    describeBillingGroupResponse_billingGroupMetadata,
    describeBillingGroupResponse_billingGroupName,
    describeBillingGroupResponse_billingGroupProperties,
    describeBillingGroupResponse_version,
    describeBillingGroupResponse_httpStatus,

    -- ** DescribeCACertificate
    describeCACertificate_certificateId,
    describeCACertificateResponse_certificateDescription,
    describeCACertificateResponse_registrationConfig,
    describeCACertificateResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_certificateId,
    describeCertificateResponse_certificateDescription,
    describeCertificateResponse_httpStatus,

    -- ** DescribeCustomMetric
    describeCustomMetric_metricName,
    describeCustomMetricResponse_creationDate,
    describeCustomMetricResponse_displayName,
    describeCustomMetricResponse_lastModifiedDate,
    describeCustomMetricResponse_metricArn,
    describeCustomMetricResponse_metricName,
    describeCustomMetricResponse_metricType,
    describeCustomMetricResponse_httpStatus,

    -- ** DescribeDefaultAuthorizer
    describeDefaultAuthorizerResponse_authorizerDescription,
    describeDefaultAuthorizerResponse_httpStatus,

    -- ** DescribeDetectMitigationActionsTask
    describeDetectMitigationActionsTask_taskId,
    describeDetectMitigationActionsTaskResponse_taskSummary,
    describeDetectMitigationActionsTaskResponse_httpStatus,

    -- ** DescribeDimension
    describeDimension_name,
    describeDimensionResponse_arn,
    describeDimensionResponse_creationDate,
    describeDimensionResponse_lastModifiedDate,
    describeDimensionResponse_name,
    describeDimensionResponse_stringValues,
    describeDimensionResponse_type,
    describeDimensionResponse_httpStatus,

    -- ** DescribeDomainConfiguration
    describeDomainConfiguration_domainConfigurationName,
    describeDomainConfigurationResponse_authorizerConfig,
    describeDomainConfigurationResponse_domainConfigurationArn,
    describeDomainConfigurationResponse_domainConfigurationName,
    describeDomainConfigurationResponse_domainConfigurationStatus,
    describeDomainConfigurationResponse_domainName,
    describeDomainConfigurationResponse_domainType,
    describeDomainConfigurationResponse_lastStatusChangeDate,
    describeDomainConfigurationResponse_serverCertificates,
    describeDomainConfigurationResponse_serviceType,
    describeDomainConfigurationResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointType,
    describeEndpointResponse_endpointAddress,
    describeEndpointResponse_httpStatus,

    -- ** DescribeEventConfigurations
    describeEventConfigurationsResponse_creationDate,
    describeEventConfigurationsResponse_eventConfigurations,
    describeEventConfigurationsResponse_lastModifiedDate,
    describeEventConfigurationsResponse_httpStatus,

    -- ** DescribeFleetMetric
    describeFleetMetric_metricName,
    describeFleetMetricResponse_aggregationField,
    describeFleetMetricResponse_aggregationType,
    describeFleetMetricResponse_creationDate,
    describeFleetMetricResponse_description,
    describeFleetMetricResponse_indexName,
    describeFleetMetricResponse_lastModifiedDate,
    describeFleetMetricResponse_metricArn,
    describeFleetMetricResponse_metricName,
    describeFleetMetricResponse_period,
    describeFleetMetricResponse_queryString,
    describeFleetMetricResponse_queryVersion,
    describeFleetMetricResponse_unit,
    describeFleetMetricResponse_version,
    describeFleetMetricResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_indexName,
    describeIndexResponse_indexName,
    describeIndexResponse_indexStatus,
    describeIndexResponse_schema,
    describeIndexResponse_httpStatus,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_documentSource,
    describeJobResponse_job,
    describeJobResponse_httpStatus,

    -- ** DescribeJobExecution
    describeJobExecution_executionNumber,
    describeJobExecution_jobId,
    describeJobExecution_thingName,
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,

    -- ** DescribeJobTemplate
    describeJobTemplate_jobTemplateId,
    describeJobTemplateResponse_abortConfig,
    describeJobTemplateResponse_createdAt,
    describeJobTemplateResponse_description,
    describeJobTemplateResponse_document,
    describeJobTemplateResponse_documentSource,
    describeJobTemplateResponse_jobExecutionsRetryConfig,
    describeJobTemplateResponse_jobExecutionsRolloutConfig,
    describeJobTemplateResponse_jobTemplateArn,
    describeJobTemplateResponse_jobTemplateId,
    describeJobTemplateResponse_presignedUrlConfig,
    describeJobTemplateResponse_timeoutConfig,
    describeJobTemplateResponse_httpStatus,

    -- ** DescribeManagedJobTemplate
    describeManagedJobTemplate_templateVersion,
    describeManagedJobTemplate_templateName,
    describeManagedJobTemplateResponse_description,
    describeManagedJobTemplateResponse_document,
    describeManagedJobTemplateResponse_documentParameters,
    describeManagedJobTemplateResponse_environments,
    describeManagedJobTemplateResponse_templateArn,
    describeManagedJobTemplateResponse_templateName,
    describeManagedJobTemplateResponse_templateVersion,
    describeManagedJobTemplateResponse_httpStatus,

    -- ** DescribeMitigationAction
    describeMitigationAction_actionName,
    describeMitigationActionResponse_actionArn,
    describeMitigationActionResponse_actionId,
    describeMitigationActionResponse_actionName,
    describeMitigationActionResponse_actionParams,
    describeMitigationActionResponse_actionType,
    describeMitigationActionResponse_creationDate,
    describeMitigationActionResponse_lastModifiedDate,
    describeMitigationActionResponse_roleArn,
    describeMitigationActionResponse_httpStatus,

    -- ** DescribeProvisioningTemplate
    describeProvisioningTemplate_templateName,
    describeProvisioningTemplateResponse_creationDate,
    describeProvisioningTemplateResponse_defaultVersionId,
    describeProvisioningTemplateResponse_description,
    describeProvisioningTemplateResponse_enabled,
    describeProvisioningTemplateResponse_lastModifiedDate,
    describeProvisioningTemplateResponse_preProvisioningHook,
    describeProvisioningTemplateResponse_provisioningRoleArn,
    describeProvisioningTemplateResponse_templateArn,
    describeProvisioningTemplateResponse_templateBody,
    describeProvisioningTemplateResponse_templateName,
    describeProvisioningTemplateResponse_type,
    describeProvisioningTemplateResponse_httpStatus,

    -- ** DescribeProvisioningTemplateVersion
    describeProvisioningTemplateVersion_templateName,
    describeProvisioningTemplateVersion_versionId,
    describeProvisioningTemplateVersionResponse_creationDate,
    describeProvisioningTemplateVersionResponse_isDefaultVersion,
    describeProvisioningTemplateVersionResponse_templateBody,
    describeProvisioningTemplateVersionResponse_versionId,
    describeProvisioningTemplateVersionResponse_httpStatus,

    -- ** DescribeRoleAlias
    describeRoleAlias_roleAlias,
    describeRoleAliasResponse_roleAliasDescription,
    describeRoleAliasResponse_httpStatus,

    -- ** DescribeScheduledAudit
    describeScheduledAudit_scheduledAuditName,
    describeScheduledAuditResponse_dayOfMonth,
    describeScheduledAuditResponse_dayOfWeek,
    describeScheduledAuditResponse_frequency,
    describeScheduledAuditResponse_scheduledAuditArn,
    describeScheduledAuditResponse_scheduledAuditName,
    describeScheduledAuditResponse_targetCheckNames,
    describeScheduledAuditResponse_httpStatus,

    -- ** DescribeSecurityProfile
    describeSecurityProfile_securityProfileName,
    describeSecurityProfileResponse_additionalMetricsToRetain,
    describeSecurityProfileResponse_additionalMetricsToRetainV2,
    describeSecurityProfileResponse_alertTargets,
    describeSecurityProfileResponse_behaviors,
    describeSecurityProfileResponse_creationDate,
    describeSecurityProfileResponse_lastModifiedDate,
    describeSecurityProfileResponse_securityProfileArn,
    describeSecurityProfileResponse_securityProfileDescription,
    describeSecurityProfileResponse_securityProfileName,
    describeSecurityProfileResponse_version,
    describeSecurityProfileResponse_httpStatus,

    -- ** DescribeStream
    describeStream_streamId,
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,

    -- ** DescribeThing
    describeThing_thingName,
    describeThingResponse_attributes,
    describeThingResponse_billingGroupName,
    describeThingResponse_defaultClientId,
    describeThingResponse_thingArn,
    describeThingResponse_thingId,
    describeThingResponse_thingName,
    describeThingResponse_thingTypeName,
    describeThingResponse_version,
    describeThingResponse_httpStatus,

    -- ** DescribeThingGroup
    describeThingGroup_thingGroupName,
    describeThingGroupResponse_indexName,
    describeThingGroupResponse_queryString,
    describeThingGroupResponse_queryVersion,
    describeThingGroupResponse_status,
    describeThingGroupResponse_thingGroupArn,
    describeThingGroupResponse_thingGroupId,
    describeThingGroupResponse_thingGroupMetadata,
    describeThingGroupResponse_thingGroupName,
    describeThingGroupResponse_thingGroupProperties,
    describeThingGroupResponse_version,
    describeThingGroupResponse_httpStatus,

    -- ** DescribeThingRegistrationTask
    describeThingRegistrationTask_taskId,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_httpStatus,

    -- ** DescribeThingType
    describeThingType_thingTypeName,
    describeThingTypeResponse_thingTypeArn,
    describeThingTypeResponse_thingTypeId,
    describeThingTypeResponse_thingTypeMetadata,
    describeThingTypeResponse_thingTypeName,
    describeThingTypeResponse_thingTypeProperties,
    describeThingTypeResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyName,
    detachPolicy_target,

    -- ** DetachSecurityProfile
    detachSecurityProfile_securityProfileName,
    detachSecurityProfile_securityProfileTargetArn,
    detachSecurityProfileResponse_httpStatus,

    -- ** DetachThingPrincipal
    detachThingPrincipal_thingName,
    detachThingPrincipal_principal,
    detachThingPrincipalResponse_httpStatus,

    -- ** DisableTopicRule
    disableTopicRule_ruleName,

    -- ** EnableTopicRule
    enableTopicRule_ruleName,

    -- ** GetBehaviorModelTrainingSummaries
    getBehaviorModelTrainingSummaries_maxResults,
    getBehaviorModelTrainingSummaries_nextToken,
    getBehaviorModelTrainingSummaries_securityProfileName,
    getBehaviorModelTrainingSummariesResponse_nextToken,
    getBehaviorModelTrainingSummariesResponse_summaries,
    getBehaviorModelTrainingSummariesResponse_httpStatus,

    -- ** GetBucketsAggregation
    getBucketsAggregation_indexName,
    getBucketsAggregation_queryVersion,
    getBucketsAggregation_queryString,
    getBucketsAggregation_aggregationField,
    getBucketsAggregation_bucketsAggregationType,
    getBucketsAggregationResponse_buckets,
    getBucketsAggregationResponse_totalCount,
    getBucketsAggregationResponse_httpStatus,

    -- ** GetCardinality
    getCardinality_aggregationField,
    getCardinality_indexName,
    getCardinality_queryVersion,
    getCardinality_queryString,
    getCardinalityResponse_cardinality,
    getCardinalityResponse_httpStatus,

    -- ** GetEffectivePolicies
    getEffectivePolicies_cognitoIdentityPoolId,
    getEffectivePolicies_principal,
    getEffectivePolicies_thingName,
    getEffectivePoliciesResponse_effectivePolicies,
    getEffectivePoliciesResponse_httpStatus,

    -- ** GetIndexingConfiguration
    getIndexingConfigurationResponse_thingGroupIndexingConfiguration,
    getIndexingConfigurationResponse_thingIndexingConfiguration,
    getIndexingConfigurationResponse_httpStatus,

    -- ** GetJobDocument
    getJobDocument_jobId,
    getJobDocumentResponse_document,
    getJobDocumentResponse_httpStatus,

    -- ** GetLoggingOptions
    getLoggingOptionsResponse_logLevel,
    getLoggingOptionsResponse_roleArn,
    getLoggingOptionsResponse_httpStatus,

    -- ** GetOTAUpdate
    getOTAUpdate_otaUpdateId,
    getOTAUpdateResponse_otaUpdateInfo,
    getOTAUpdateResponse_httpStatus,

    -- ** GetPercentiles
    getPercentiles_aggregationField,
    getPercentiles_indexName,
    getPercentiles_percents,
    getPercentiles_queryVersion,
    getPercentiles_queryString,
    getPercentilesResponse_percentiles,
    getPercentilesResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyName,
    getPolicyResponse_creationDate,
    getPolicyResponse_defaultVersionId,
    getPolicyResponse_generationId,
    getPolicyResponse_lastModifiedDate,
    getPolicyResponse_policyArn,
    getPolicyResponse_policyDocument,
    getPolicyResponse_policyName,
    getPolicyResponse_httpStatus,

    -- ** GetPolicyVersion
    getPolicyVersion_policyName,
    getPolicyVersion_policyVersionId,
    getPolicyVersionResponse_creationDate,
    getPolicyVersionResponse_generationId,
    getPolicyVersionResponse_isDefaultVersion,
    getPolicyVersionResponse_lastModifiedDate,
    getPolicyVersionResponse_policyArn,
    getPolicyVersionResponse_policyDocument,
    getPolicyVersionResponse_policyName,
    getPolicyVersionResponse_policyVersionId,
    getPolicyVersionResponse_httpStatus,

    -- ** GetRegistrationCode
    getRegistrationCodeResponse_registrationCode,
    getRegistrationCodeResponse_httpStatus,

    -- ** GetStatistics
    getStatistics_aggregationField,
    getStatistics_indexName,
    getStatistics_queryVersion,
    getStatistics_queryString,
    getStatisticsResponse_statistics,
    getStatisticsResponse_httpStatus,

    -- ** GetTopicRule
    getTopicRule_ruleName,
    getTopicRuleResponse_rule,
    getTopicRuleResponse_ruleArn,
    getTopicRuleResponse_httpStatus,

    -- ** GetTopicRuleDestination
    getTopicRuleDestination_arn,
    getTopicRuleDestinationResponse_topicRuleDestination,
    getTopicRuleDestinationResponse_httpStatus,

    -- ** GetV2LoggingOptions
    getV2LoggingOptionsResponse_defaultLogLevel,
    getV2LoggingOptionsResponse_disableAllLogs,
    getV2LoggingOptionsResponse_roleArn,
    getV2LoggingOptionsResponse_httpStatus,

    -- ** ListActiveViolations
    listActiveViolations_behaviorCriteriaType,
    listActiveViolations_listSuppressedAlerts,
    listActiveViolations_maxResults,
    listActiveViolations_nextToken,
    listActiveViolations_securityProfileName,
    listActiveViolations_thingName,
    listActiveViolations_verificationState,
    listActiveViolationsResponse_activeViolations,
    listActiveViolationsResponse_nextToken,
    listActiveViolationsResponse_httpStatus,

    -- ** ListAttachedPolicies
    listAttachedPolicies_marker,
    listAttachedPolicies_pageSize,
    listAttachedPolicies_recursive,
    listAttachedPolicies_target,
    listAttachedPoliciesResponse_nextMarker,
    listAttachedPoliciesResponse_policies,
    listAttachedPoliciesResponse_httpStatus,

    -- ** ListAuditFindings
    listAuditFindings_checkName,
    listAuditFindings_endTime,
    listAuditFindings_listSuppressedFindings,
    listAuditFindings_maxResults,
    listAuditFindings_nextToken,
    listAuditFindings_resourceIdentifier,
    listAuditFindings_startTime,
    listAuditFindings_taskId,
    listAuditFindingsResponse_findings,
    listAuditFindingsResponse_nextToken,
    listAuditFindingsResponse_httpStatus,

    -- ** ListAuditMitigationActionsExecutions
    listAuditMitigationActionsExecutions_actionStatus,
    listAuditMitigationActionsExecutions_maxResults,
    listAuditMitigationActionsExecutions_nextToken,
    listAuditMitigationActionsExecutions_taskId,
    listAuditMitigationActionsExecutions_findingId,
    listAuditMitigationActionsExecutionsResponse_actionsExecutions,
    listAuditMitigationActionsExecutionsResponse_nextToken,
    listAuditMitigationActionsExecutionsResponse_httpStatus,

    -- ** ListAuditMitigationActionsTasks
    listAuditMitigationActionsTasks_auditTaskId,
    listAuditMitigationActionsTasks_findingId,
    listAuditMitigationActionsTasks_maxResults,
    listAuditMitigationActionsTasks_nextToken,
    listAuditMitigationActionsTasks_taskStatus,
    listAuditMitigationActionsTasks_startTime,
    listAuditMitigationActionsTasks_endTime,
    listAuditMitigationActionsTasksResponse_nextToken,
    listAuditMitigationActionsTasksResponse_tasks,
    listAuditMitigationActionsTasksResponse_httpStatus,

    -- ** ListAuditSuppressions
    listAuditSuppressions_ascendingOrder,
    listAuditSuppressions_checkName,
    listAuditSuppressions_maxResults,
    listAuditSuppressions_nextToken,
    listAuditSuppressions_resourceIdentifier,
    listAuditSuppressionsResponse_nextToken,
    listAuditSuppressionsResponse_suppressions,
    listAuditSuppressionsResponse_httpStatus,

    -- ** ListAuditTasks
    listAuditTasks_maxResults,
    listAuditTasks_nextToken,
    listAuditTasks_taskStatus,
    listAuditTasks_taskType,
    listAuditTasks_startTime,
    listAuditTasks_endTime,
    listAuditTasksResponse_nextToken,
    listAuditTasksResponse_tasks,
    listAuditTasksResponse_httpStatus,

    -- ** ListAuthorizers
    listAuthorizers_ascendingOrder,
    listAuthorizers_marker,
    listAuthorizers_pageSize,
    listAuthorizers_status,
    listAuthorizersResponse_authorizers,
    listAuthorizersResponse_nextMarker,
    listAuthorizersResponse_httpStatus,

    -- ** ListBillingGroups
    listBillingGroups_maxResults,
    listBillingGroups_namePrefixFilter,
    listBillingGroups_nextToken,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_httpStatus,

    -- ** ListCACertificates
    listCACertificates_ascendingOrder,
    listCACertificates_marker,
    listCACertificates_pageSize,
    listCACertificates_templateName,
    listCACertificatesResponse_certificates,
    listCACertificatesResponse_nextMarker,
    listCACertificatesResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_ascendingOrder,
    listCertificates_marker,
    listCertificates_pageSize,
    listCertificatesResponse_certificates,
    listCertificatesResponse_nextMarker,
    listCertificatesResponse_httpStatus,

    -- ** ListCertificatesByCA
    listCertificatesByCA_ascendingOrder,
    listCertificatesByCA_marker,
    listCertificatesByCA_pageSize,
    listCertificatesByCA_caCertificateId,
    listCertificatesByCAResponse_certificates,
    listCertificatesByCAResponse_nextMarker,
    listCertificatesByCAResponse_httpStatus,

    -- ** ListCustomMetrics
    listCustomMetrics_maxResults,
    listCustomMetrics_nextToken,
    listCustomMetricsResponse_metricNames,
    listCustomMetricsResponse_nextToken,
    listCustomMetricsResponse_httpStatus,

    -- ** ListDetectMitigationActionsExecutions
    listDetectMitigationActionsExecutions_endTime,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_startTime,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_violationId,
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_httpStatus,

    -- ** ListDetectMitigationActionsTasks
    listDetectMitigationActionsTasks_maxResults,
    listDetectMitigationActionsTasks_nextToken,
    listDetectMitigationActionsTasks_startTime,
    listDetectMitigationActionsTasks_endTime,
    listDetectMitigationActionsTasksResponse_nextToken,
    listDetectMitigationActionsTasksResponse_tasks,
    listDetectMitigationActionsTasksResponse_httpStatus,

    -- ** ListDimensions
    listDimensions_maxResults,
    listDimensions_nextToken,
    listDimensionsResponse_dimensionNames,
    listDimensionsResponse_nextToken,
    listDimensionsResponse_httpStatus,

    -- ** ListDomainConfigurations
    listDomainConfigurations_marker,
    listDomainConfigurations_pageSize,
    listDomainConfigurations_serviceType,
    listDomainConfigurationsResponse_domainConfigurations,
    listDomainConfigurationsResponse_nextMarker,
    listDomainConfigurationsResponse_httpStatus,

    -- ** ListFleetMetrics
    listFleetMetrics_maxResults,
    listFleetMetrics_nextToken,
    listFleetMetricsResponse_fleetMetrics,
    listFleetMetricsResponse_nextToken,
    listFleetMetricsResponse_httpStatus,

    -- ** ListIndices
    listIndices_maxResults,
    listIndices_nextToken,
    listIndicesResponse_indexNames,
    listIndicesResponse_nextToken,
    listIndicesResponse_httpStatus,

    -- ** ListJobExecutionsForJob
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_jobId,
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_httpStatus,

    -- ** ListJobExecutionsForThing
    listJobExecutionsForThing_jobId,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_thingName,
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_maxResults,
    listJobTemplates_nextToken,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_httpStatus,

    -- ** ListJobs
    listJobs_maxResults,
    listJobs_namespaceId,
    listJobs_nextToken,
    listJobs_status,
    listJobs_targetSelection,
    listJobs_thingGroupId,
    listJobs_thingGroupName,
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListManagedJobTemplates
    listManagedJobTemplates_maxResults,
    listManagedJobTemplates_nextToken,
    listManagedJobTemplates_templateName,
    listManagedJobTemplatesResponse_managedJobTemplates,
    listManagedJobTemplatesResponse_nextToken,
    listManagedJobTemplatesResponse_httpStatus,

    -- ** ListMetricValues
    listMetricValues_dimensionName,
    listMetricValues_dimensionValueOperator,
    listMetricValues_maxResults,
    listMetricValues_nextToken,
    listMetricValues_thingName,
    listMetricValues_metricName,
    listMetricValues_startTime,
    listMetricValues_endTime,
    listMetricValuesResponse_metricDatumList,
    listMetricValuesResponse_nextToken,
    listMetricValuesResponse_httpStatus,

    -- ** ListMitigationActions
    listMitigationActions_actionType,
    listMitigationActions_maxResults,
    listMitigationActions_nextToken,
    listMitigationActionsResponse_actionIdentifiers,
    listMitigationActionsResponse_nextToken,
    listMitigationActionsResponse_httpStatus,

    -- ** ListOTAUpdates
    listOTAUpdates_maxResults,
    listOTAUpdates_nextToken,
    listOTAUpdates_otaUpdateStatus,
    listOTAUpdatesResponse_nextToken,
    listOTAUpdatesResponse_otaUpdates,
    listOTAUpdatesResponse_httpStatus,

    -- ** ListOutgoingCertificates
    listOutgoingCertificates_ascendingOrder,
    listOutgoingCertificates_marker,
    listOutgoingCertificates_pageSize,
    listOutgoingCertificatesResponse_nextMarker,
    listOutgoingCertificatesResponse_outgoingCertificates,
    listOutgoingCertificatesResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_ascendingOrder,
    listPolicies_marker,
    listPolicies_pageSize,
    listPoliciesResponse_nextMarker,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** ListPolicyVersions
    listPolicyVersions_policyName,
    listPolicyVersionsResponse_policyVersions,
    listPolicyVersionsResponse_httpStatus,

    -- ** ListPrincipalThings
    listPrincipalThings_maxResults,
    listPrincipalThings_nextToken,
    listPrincipalThings_principal,
    listPrincipalThingsResponse_nextToken,
    listPrincipalThingsResponse_things,
    listPrincipalThingsResponse_httpStatus,

    -- ** ListProvisioningTemplateVersions
    listProvisioningTemplateVersions_maxResults,
    listProvisioningTemplateVersions_nextToken,
    listProvisioningTemplateVersions_templateName,
    listProvisioningTemplateVersionsResponse_nextToken,
    listProvisioningTemplateVersionsResponse_versions,
    listProvisioningTemplateVersionsResponse_httpStatus,

    -- ** ListProvisioningTemplates
    listProvisioningTemplates_maxResults,
    listProvisioningTemplates_nextToken,
    listProvisioningTemplatesResponse_nextToken,
    listProvisioningTemplatesResponse_templates,
    listProvisioningTemplatesResponse_httpStatus,

    -- ** ListRelatedResourcesForAuditFinding
    listRelatedResourcesForAuditFinding_maxResults,
    listRelatedResourcesForAuditFinding_nextToken,
    listRelatedResourcesForAuditFinding_findingId,
    listRelatedResourcesForAuditFindingResponse_nextToken,
    listRelatedResourcesForAuditFindingResponse_relatedResources,
    listRelatedResourcesForAuditFindingResponse_httpStatus,

    -- ** ListRoleAliases
    listRoleAliases_ascendingOrder,
    listRoleAliases_marker,
    listRoleAliases_pageSize,
    listRoleAliasesResponse_nextMarker,
    listRoleAliasesResponse_roleAliases,
    listRoleAliasesResponse_httpStatus,

    -- ** ListScheduledAudits
    listScheduledAudits_maxResults,
    listScheduledAudits_nextToken,
    listScheduledAuditsResponse_nextToken,
    listScheduledAuditsResponse_scheduledAudits,
    listScheduledAuditsResponse_httpStatus,

    -- ** ListSecurityProfiles
    listSecurityProfiles_dimensionName,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_metricName,
    listSecurityProfiles_nextToken,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileIdentifiers,
    listSecurityProfilesResponse_httpStatus,

    -- ** ListSecurityProfilesForTarget
    listSecurityProfilesForTarget_maxResults,
    listSecurityProfilesForTarget_nextToken,
    listSecurityProfilesForTarget_recursive,
    listSecurityProfilesForTarget_securityProfileTargetArn,
    listSecurityProfilesForTargetResponse_nextToken,
    listSecurityProfilesForTargetResponse_securityProfileTargetMappings,
    listSecurityProfilesForTargetResponse_httpStatus,

    -- ** ListStreams
    listStreams_ascendingOrder,
    listStreams_maxResults,
    listStreams_nextToken,
    listStreamsResponse_nextToken,
    listStreamsResponse_streams,
    listStreamsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_marker,
    listTargetsForPolicy_pageSize,
    listTargetsForPolicy_policyName,
    listTargetsForPolicyResponse_nextMarker,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** ListTargetsForSecurityProfile
    listTargetsForSecurityProfile_maxResults,
    listTargetsForSecurityProfile_nextToken,
    listTargetsForSecurityProfile_securityProfileName,
    listTargetsForSecurityProfileResponse_nextToken,
    listTargetsForSecurityProfileResponse_securityProfileTargets,
    listTargetsForSecurityProfileResponse_httpStatus,

    -- ** ListThingGroups
    listThingGroups_maxResults,
    listThingGroups_namePrefixFilter,
    listThingGroups_nextToken,
    listThingGroups_parentGroup,
    listThingGroups_recursive,
    listThingGroupsResponse_nextToken,
    listThingGroupsResponse_thingGroups,
    listThingGroupsResponse_httpStatus,

    -- ** ListThingGroupsForThing
    listThingGroupsForThing_maxResults,
    listThingGroupsForThing_nextToken,
    listThingGroupsForThing_thingName,
    listThingGroupsForThingResponse_nextToken,
    listThingGroupsForThingResponse_thingGroups,
    listThingGroupsForThingResponse_httpStatus,

    -- ** ListThingPrincipals
    listThingPrincipals_maxResults,
    listThingPrincipals_nextToken,
    listThingPrincipals_thingName,
    listThingPrincipalsResponse_nextToken,
    listThingPrincipalsResponse_principals,
    listThingPrincipalsResponse_httpStatus,

    -- ** ListThingRegistrationTaskReports
    listThingRegistrationTaskReports_maxResults,
    listThingRegistrationTaskReports_nextToken,
    listThingRegistrationTaskReports_taskId,
    listThingRegistrationTaskReports_reportType,
    listThingRegistrationTaskReportsResponse_nextToken,
    listThingRegistrationTaskReportsResponse_reportType,
    listThingRegistrationTaskReportsResponse_resourceLinks,
    listThingRegistrationTaskReportsResponse_httpStatus,

    -- ** ListThingRegistrationTasks
    listThingRegistrationTasks_maxResults,
    listThingRegistrationTasks_nextToken,
    listThingRegistrationTasks_status,
    listThingRegistrationTasksResponse_nextToken,
    listThingRegistrationTasksResponse_taskIds,
    listThingRegistrationTasksResponse_httpStatus,

    -- ** ListThingTypes
    listThingTypes_maxResults,
    listThingTypes_nextToken,
    listThingTypes_thingTypeName,
    listThingTypesResponse_nextToken,
    listThingTypesResponse_thingTypes,
    listThingTypesResponse_httpStatus,

    -- ** ListThings
    listThings_attributeName,
    listThings_attributeValue,
    listThings_maxResults,
    listThings_nextToken,
    listThings_thingTypeName,
    listThings_usePrefixAttributeValue,
    listThingsResponse_nextToken,
    listThingsResponse_things,
    listThingsResponse_httpStatus,

    -- ** ListThingsInBillingGroup
    listThingsInBillingGroup_maxResults,
    listThingsInBillingGroup_nextToken,
    listThingsInBillingGroup_billingGroupName,
    listThingsInBillingGroupResponse_nextToken,
    listThingsInBillingGroupResponse_things,
    listThingsInBillingGroupResponse_httpStatus,

    -- ** ListThingsInThingGroup
    listThingsInThingGroup_maxResults,
    listThingsInThingGroup_nextToken,
    listThingsInThingGroup_recursive,
    listThingsInThingGroup_thingGroupName,
    listThingsInThingGroupResponse_nextToken,
    listThingsInThingGroupResponse_things,
    listThingsInThingGroupResponse_httpStatus,

    -- ** ListTopicRuleDestinations
    listTopicRuleDestinations_maxResults,
    listTopicRuleDestinations_nextToken,
    listTopicRuleDestinationsResponse_destinationSummaries,
    listTopicRuleDestinationsResponse_nextToken,
    listTopicRuleDestinationsResponse_httpStatus,

    -- ** ListTopicRules
    listTopicRules_maxResults,
    listTopicRules_nextToken,
    listTopicRules_ruleDisabled,
    listTopicRules_topic,
    listTopicRulesResponse_nextToken,
    listTopicRulesResponse_rules,
    listTopicRulesResponse_httpStatus,

    -- ** ListV2LoggingLevels
    listV2LoggingLevels_maxResults,
    listV2LoggingLevels_nextToken,
    listV2LoggingLevels_targetType,
    listV2LoggingLevelsResponse_logTargetConfigurations,
    listV2LoggingLevelsResponse_nextToken,
    listV2LoggingLevelsResponse_httpStatus,

    -- ** ListViolationEvents
    listViolationEvents_behaviorCriteriaType,
    listViolationEvents_listSuppressedAlerts,
    listViolationEvents_maxResults,
    listViolationEvents_nextToken,
    listViolationEvents_securityProfileName,
    listViolationEvents_thingName,
    listViolationEvents_verificationState,
    listViolationEvents_startTime,
    listViolationEvents_endTime,
    listViolationEventsResponse_nextToken,
    listViolationEventsResponse_violationEvents,
    listViolationEventsResponse_httpStatus,

    -- ** PutVerificationStateOnViolation
    putVerificationStateOnViolation_verificationStateDescription,
    putVerificationStateOnViolation_violationId,
    putVerificationStateOnViolation_verificationState,
    putVerificationStateOnViolationResponse_httpStatus,

    -- ** RegisterCACertificate
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_certificateMode,
    registerCACertificate_registrationConfig,
    registerCACertificate_setAsActive,
    registerCACertificate_tags,
    registerCACertificate_verificationCertificate,
    registerCACertificate_caCertificate,
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_caCertificatePem,
    registerCertificate_setAsActive,
    registerCertificate_status,
    registerCertificate_certificatePem,
    registerCertificateResponse_certificateArn,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** RegisterCertificateWithoutCA
    registerCertificateWithoutCA_status,
    registerCertificateWithoutCA_certificatePem,
    registerCertificateWithoutCAResponse_certificateArn,
    registerCertificateWithoutCAResponse_certificateId,
    registerCertificateWithoutCAResponse_httpStatus,

    -- ** RegisterThing
    registerThing_parameters,
    registerThing_templateBody,
    registerThingResponse_certificatePem,
    registerThingResponse_resourceArns,
    registerThingResponse_httpStatus,

    -- ** RejectCertificateTransfer
    rejectCertificateTransfer_rejectReason,
    rejectCertificateTransfer_certificateId,

    -- ** RemoveThingFromBillingGroup
    removeThingFromBillingGroup_billingGroupArn,
    removeThingFromBillingGroup_billingGroupName,
    removeThingFromBillingGroup_thingArn,
    removeThingFromBillingGroup_thingName,
    removeThingFromBillingGroupResponse_httpStatus,

    -- ** RemoveThingFromThingGroup
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingGroupName,
    removeThingFromThingGroup_thingName,
    removeThingFromThingGroupResponse_httpStatus,

    -- ** ReplaceTopicRule
    replaceTopicRule_ruleName,
    replaceTopicRule_topicRulePayload,

    -- ** SearchIndex
    searchIndex_indexName,
    searchIndex_maxResults,
    searchIndex_nextToken,
    searchIndex_queryVersion,
    searchIndex_queryString,
    searchIndexResponse_nextToken,
    searchIndexResponse_thingGroups,
    searchIndexResponse_things,
    searchIndexResponse_httpStatus,

    -- ** SetDefaultAuthorizer
    setDefaultAuthorizer_authorizerName,
    setDefaultAuthorizerResponse_authorizerArn,
    setDefaultAuthorizerResponse_authorizerName,
    setDefaultAuthorizerResponse_httpStatus,

    -- ** SetDefaultPolicyVersion
    setDefaultPolicyVersion_policyName,
    setDefaultPolicyVersion_policyVersionId,

    -- ** SetLoggingOptions
    setLoggingOptions_loggingOptionsPayload,

    -- ** SetV2LoggingLevel
    setV2LoggingLevel_logTarget,
    setV2LoggingLevel_logLevel,

    -- ** SetV2LoggingOptions
    setV2LoggingOptions_defaultLogLevel,
    setV2LoggingOptions_disableAllLogs,
    setV2LoggingOptions_roleArn,

    -- ** StartAuditMitigationActionsTask
    startAuditMitigationActionsTask_taskId,
    startAuditMitigationActionsTask_target,
    startAuditMitigationActionsTask_auditCheckToActionsMapping,
    startAuditMitigationActionsTask_clientRequestToken,
    startAuditMitigationActionsTaskResponse_taskId,
    startAuditMitigationActionsTaskResponse_httpStatus,

    -- ** StartDetectMitigationActionsTask
    startDetectMitigationActionsTask_includeOnlyActiveViolations,
    startDetectMitigationActionsTask_includeSuppressedAlerts,
    startDetectMitigationActionsTask_violationEventOccurrenceRange,
    startDetectMitigationActionsTask_taskId,
    startDetectMitigationActionsTask_target,
    startDetectMitigationActionsTask_actions,
    startDetectMitigationActionsTask_clientRequestToken,
    startDetectMitigationActionsTaskResponse_taskId,
    startDetectMitigationActionsTaskResponse_httpStatus,

    -- ** StartOnDemandAuditTask
    startOnDemandAuditTask_targetCheckNames,
    startOnDemandAuditTaskResponse_taskId,
    startOnDemandAuditTaskResponse_httpStatus,

    -- ** StartThingRegistrationTask
    startThingRegistrationTask_templateBody,
    startThingRegistrationTask_inputFileBucket,
    startThingRegistrationTask_inputFileKey,
    startThingRegistrationTask_roleArn,
    startThingRegistrationTaskResponse_taskId,
    startThingRegistrationTaskResponse_httpStatus,

    -- ** StopThingRegistrationTask
    stopThingRegistrationTask_taskId,
    stopThingRegistrationTaskResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestAuthorization
    testAuthorization_clientId,
    testAuthorization_cognitoIdentityPoolId,
    testAuthorization_policyNamesToAdd,
    testAuthorization_policyNamesToSkip,
    testAuthorization_principal,
    testAuthorization_authInfos,
    testAuthorizationResponse_authResults,
    testAuthorizationResponse_httpStatus,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_httpContext,
    testInvokeAuthorizer_mqttContext,
    testInvokeAuthorizer_tlsContext,
    testInvokeAuthorizer_token,
    testInvokeAuthorizer_tokenSignature,
    testInvokeAuthorizer_authorizerName,
    testInvokeAuthorizerResponse_disconnectAfterInSeconds,
    testInvokeAuthorizerResponse_isAuthenticated,
    testInvokeAuthorizerResponse_policyDocuments,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_refreshAfterInSeconds,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** TransferCertificate
    transferCertificate_transferMessage,
    transferCertificate_certificateId,
    transferCertificate_targetAwsAccount,
    transferCertificateResponse_transferredCertificateArn,
    transferCertificateResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccountAuditConfiguration
    updateAccountAuditConfiguration_auditCheckConfigurations,
    updateAccountAuditConfiguration_auditNotificationTargetConfigurations,
    updateAccountAuditConfiguration_roleArn,
    updateAccountAuditConfigurationResponse_httpStatus,

    -- ** UpdateAuditSuppression
    updateAuditSuppression_description,
    updateAuditSuppression_expirationDate,
    updateAuditSuppression_suppressIndefinitely,
    updateAuditSuppression_checkName,
    updateAuditSuppression_resourceIdentifier,
    updateAuditSuppressionResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_authorizerFunctionArn,
    updateAuthorizer_enableCachingForHttp,
    updateAuthorizer_status,
    updateAuthorizer_tokenKeyName,
    updateAuthorizer_tokenSigningPublicKeys,
    updateAuthorizer_authorizerName,
    updateAuthorizerResponse_authorizerArn,
    updateAuthorizerResponse_authorizerName,
    updateAuthorizerResponse_httpStatus,

    -- ** UpdateBillingGroup
    updateBillingGroup_expectedVersion,
    updateBillingGroup_billingGroupName,
    updateBillingGroup_billingGroupProperties,
    updateBillingGroupResponse_version,
    updateBillingGroupResponse_httpStatus,

    -- ** UpdateCACertificate
    updateCACertificate_newAutoRegistrationStatus,
    updateCACertificate_newStatus,
    updateCACertificate_registrationConfig,
    updateCACertificate_removeAutoRegistration,
    updateCACertificate_certificateId,

    -- ** UpdateCertificate
    updateCertificate_certificateId,
    updateCertificate_newStatus,

    -- ** UpdateCustomMetric
    updateCustomMetric_metricName,
    updateCustomMetric_displayName,
    updateCustomMetricResponse_creationDate,
    updateCustomMetricResponse_displayName,
    updateCustomMetricResponse_lastModifiedDate,
    updateCustomMetricResponse_metricArn,
    updateCustomMetricResponse_metricName,
    updateCustomMetricResponse_metricType,
    updateCustomMetricResponse_httpStatus,

    -- ** UpdateDimension
    updateDimension_name,
    updateDimension_stringValues,
    updateDimensionResponse_arn,
    updateDimensionResponse_creationDate,
    updateDimensionResponse_lastModifiedDate,
    updateDimensionResponse_name,
    updateDimensionResponse_stringValues,
    updateDimensionResponse_type,
    updateDimensionResponse_httpStatus,

    -- ** UpdateDomainConfiguration
    updateDomainConfiguration_authorizerConfig,
    updateDomainConfiguration_domainConfigurationStatus,
    updateDomainConfiguration_removeAuthorizerConfig,
    updateDomainConfiguration_domainConfigurationName,
    updateDomainConfigurationResponse_domainConfigurationArn,
    updateDomainConfigurationResponse_domainConfigurationName,
    updateDomainConfigurationResponse_httpStatus,

    -- ** UpdateDynamicThingGroup
    updateDynamicThingGroup_expectedVersion,
    updateDynamicThingGroup_indexName,
    updateDynamicThingGroup_queryString,
    updateDynamicThingGroup_queryVersion,
    updateDynamicThingGroup_thingGroupName,
    updateDynamicThingGroup_thingGroupProperties,
    updateDynamicThingGroupResponse_version,
    updateDynamicThingGroupResponse_httpStatus,

    -- ** UpdateEventConfigurations
    updateEventConfigurations_eventConfigurations,
    updateEventConfigurationsResponse_httpStatus,

    -- ** UpdateFleetMetric
    updateFleetMetric_aggregationField,
    updateFleetMetric_aggregationType,
    updateFleetMetric_description,
    updateFleetMetric_expectedVersion,
    updateFleetMetric_period,
    updateFleetMetric_queryString,
    updateFleetMetric_queryVersion,
    updateFleetMetric_unit,
    updateFleetMetric_metricName,
    updateFleetMetric_indexName,

    -- ** UpdateIndexingConfiguration
    updateIndexingConfiguration_thingGroupIndexingConfiguration,
    updateIndexingConfiguration_thingIndexingConfiguration,
    updateIndexingConfigurationResponse_httpStatus,

    -- ** UpdateJob
    updateJob_abortConfig,
    updateJob_description,
    updateJob_jobExecutionsRetryConfig,
    updateJob_jobExecutionsRolloutConfig,
    updateJob_namespaceId,
    updateJob_presignedUrlConfig,
    updateJob_timeoutConfig,
    updateJob_jobId,

    -- ** UpdateMitigationAction
    updateMitigationAction_actionParams,
    updateMitigationAction_roleArn,
    updateMitigationAction_actionName,
    updateMitigationActionResponse_actionArn,
    updateMitigationActionResponse_actionId,
    updateMitigationActionResponse_httpStatus,

    -- ** UpdateProvisioningTemplate
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_templateName,
    updateProvisioningTemplateResponse_httpStatus,

    -- ** UpdateRoleAlias
    updateRoleAlias_credentialDurationSeconds,
    updateRoleAlias_roleArn,
    updateRoleAlias_roleAlias,
    updateRoleAliasResponse_roleAlias,
    updateRoleAliasResponse_roleAliasArn,
    updateRoleAliasResponse_httpStatus,

    -- ** UpdateScheduledAudit
    updateScheduledAudit_dayOfMonth,
    updateScheduledAudit_dayOfWeek,
    updateScheduledAudit_frequency,
    updateScheduledAudit_targetCheckNames,
    updateScheduledAudit_scheduledAuditName,
    updateScheduledAuditResponse_scheduledAuditArn,
    updateScheduledAuditResponse_httpStatus,

    -- ** UpdateSecurityProfile
    updateSecurityProfile_additionalMetricsToRetain,
    updateSecurityProfile_additionalMetricsToRetainV2,
    updateSecurityProfile_alertTargets,
    updateSecurityProfile_behaviors,
    updateSecurityProfile_deleteAdditionalMetricsToRetain,
    updateSecurityProfile_deleteAlertTargets,
    updateSecurityProfile_deleteBehaviors,
    updateSecurityProfile_expectedVersion,
    updateSecurityProfile_securityProfileDescription,
    updateSecurityProfile_securityProfileName,
    updateSecurityProfileResponse_additionalMetricsToRetain,
    updateSecurityProfileResponse_additionalMetricsToRetainV2,
    updateSecurityProfileResponse_alertTargets,
    updateSecurityProfileResponse_behaviors,
    updateSecurityProfileResponse_creationDate,
    updateSecurityProfileResponse_lastModifiedDate,
    updateSecurityProfileResponse_securityProfileArn,
    updateSecurityProfileResponse_securityProfileDescription,
    updateSecurityProfileResponse_securityProfileName,
    updateSecurityProfileResponse_version,
    updateSecurityProfileResponse_httpStatus,

    -- ** UpdateStream
    updateStream_description,
    updateStream_files,
    updateStream_roleArn,
    updateStream_streamId,
    updateStreamResponse_description,
    updateStreamResponse_streamArn,
    updateStreamResponse_streamId,
    updateStreamResponse_streamVersion,
    updateStreamResponse_httpStatus,

    -- ** UpdateThing
    updateThing_attributePayload,
    updateThing_expectedVersion,
    updateThing_removeThingType,
    updateThing_thingTypeName,
    updateThing_thingName,
    updateThingResponse_httpStatus,

    -- ** UpdateThingGroup
    updateThingGroup_expectedVersion,
    updateThingGroup_thingGroupName,
    updateThingGroup_thingGroupProperties,
    updateThingGroupResponse_version,
    updateThingGroupResponse_httpStatus,

    -- ** UpdateThingGroupsForThing
    updateThingGroupsForThing_overrideDynamicGroups,
    updateThingGroupsForThing_thingGroupsToAdd,
    updateThingGroupsForThing_thingGroupsToRemove,
    updateThingGroupsForThing_thingName,
    updateThingGroupsForThingResponse_httpStatus,

    -- ** UpdateTopicRuleDestination
    updateTopicRuleDestination_arn,
    updateTopicRuleDestination_status,
    updateTopicRuleDestinationResponse_httpStatus,

    -- ** ValidateSecurityProfileBehaviors
    validateSecurityProfileBehaviors_behaviors,
    validateSecurityProfileBehaviorsResponse_valid,
    validateSecurityProfileBehaviorsResponse_validationErrors,
    validateSecurityProfileBehaviorsResponse_httpStatus,

    -- * Types

    -- ** AbortConfig
    abortConfig_criteriaList,

    -- ** AbortCriteria
    abortCriteria_failureType,
    abortCriteria_action,
    abortCriteria_thresholdPercentage,
    abortCriteria_minNumberOfExecutedThings,

    -- ** Action
    action_cloudwatchAlarm,
    action_cloudwatchLogs,
    action_cloudwatchMetric,
    action_dynamoDB,
    action_dynamoDBv2,
    action_elasticsearch,
    action_firehose,
    action_http,
    action_iotAnalytics,
    action_iotEvents,
    action_iotSiteWise,
    action_kafka,
    action_kinesis,
    action_lambda,
    action_location,
    action_openSearch,
    action_republish,
    action_s3,
    action_salesforce,
    action_sns,
    action_sqs,
    action_stepFunctions,
    action_timestream,

    -- ** ActiveViolation
    activeViolation_behavior,
    activeViolation_lastViolationTime,
    activeViolation_lastViolationValue,
    activeViolation_securityProfileName,
    activeViolation_thingName,
    activeViolation_verificationState,
    activeViolation_verificationStateDescription,
    activeViolation_violationEventAdditionalInfo,
    activeViolation_violationId,
    activeViolation_violationStartTime,

    -- ** AddThingsToThingGroupParams
    addThingsToThingGroupParams_overrideDynamicGroups,
    addThingsToThingGroupParams_thingGroupNames,

    -- ** AggregationType
    aggregationType_values,
    aggregationType_name,

    -- ** AlertTarget
    alertTarget_alertTargetArn,
    alertTarget_roleArn,

    -- ** Allowed
    allowed_policies,

    -- ** AssetPropertyTimestamp
    assetPropertyTimestamp_offsetInNanos,
    assetPropertyTimestamp_timeInSeconds,

    -- ** AssetPropertyValue
    assetPropertyValue_quality,
    assetPropertyValue_value,
    assetPropertyValue_timestamp,

    -- ** AssetPropertyVariant
    assetPropertyVariant_booleanValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_integerValue,
    assetPropertyVariant_stringValue,

    -- ** AttributePayload
    attributePayload_attributes,
    attributePayload_merge,

    -- ** AuditCheckConfiguration
    auditCheckConfiguration_enabled,

    -- ** AuditCheckDetails
    auditCheckDetails_checkCompliant,
    auditCheckDetails_checkRunStatus,
    auditCheckDetails_errorCode,
    auditCheckDetails_message,
    auditCheckDetails_nonCompliantResourcesCount,
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_totalResourcesCount,

    -- ** AuditFinding
    auditFinding_checkName,
    auditFinding_findingId,
    auditFinding_findingTime,
    auditFinding_isSuppressed,
    auditFinding_nonCompliantResource,
    auditFinding_reasonForNonCompliance,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_relatedResources,
    auditFinding_severity,
    auditFinding_taskId,
    auditFinding_taskStartTime,

    -- ** AuditMitigationActionExecutionMetadata
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_message,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_taskId,

    -- ** AuditMitigationActionsTaskMetadata
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- ** AuditMitigationActionsTaskTarget
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_findingIds,

    -- ** AuditNotificationTarget
    auditNotificationTarget_enabled,
    auditNotificationTarget_roleArn,
    auditNotificationTarget_targetArn,

    -- ** AuditSuppression
    auditSuppression_description,
    auditSuppression_expirationDate,
    auditSuppression_suppressIndefinitely,
    auditSuppression_checkName,
    auditSuppression_resourceIdentifier,

    -- ** AuditTaskMetadata
    auditTaskMetadata_taskId,
    auditTaskMetadata_taskStatus,
    auditTaskMetadata_taskType,

    -- ** AuthInfo
    authInfo_actionType,
    authInfo_resources,

    -- ** AuthResult
    authResult_allowed,
    authResult_authDecision,
    authResult_authInfo,
    authResult_denied,
    authResult_missingContextValues,

    -- ** AuthorizerConfig
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- ** AuthorizerDescription
    authorizerDescription_authorizerArn,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_authorizerName,
    authorizerDescription_creationDate,
    authorizerDescription_enableCachingForHttp,
    authorizerDescription_lastModifiedDate,
    authorizerDescription_signingDisabled,
    authorizerDescription_status,
    authorizerDescription_tokenKeyName,
    authorizerDescription_tokenSigningPublicKeys,

    -- ** AuthorizerSummary
    authorizerSummary_authorizerArn,
    authorizerSummary_authorizerName,

    -- ** AwsJobAbortConfig
    awsJobAbortConfig_abortCriteriaList,

    -- ** AwsJobAbortCriteria
    awsJobAbortCriteria_failureType,
    awsJobAbortCriteria_action,
    awsJobAbortCriteria_thresholdPercentage,
    awsJobAbortCriteria_minNumberOfExecutedThings,

    -- ** AwsJobExecutionsRolloutConfig
    awsJobExecutionsRolloutConfig_exponentialRate,
    awsJobExecutionsRolloutConfig_maximumPerMinute,

    -- ** AwsJobExponentialRolloutRate
    awsJobExponentialRolloutRate_baseRatePerMinute,
    awsJobExponentialRolloutRate_incrementFactor,
    awsJobExponentialRolloutRate_rateIncreaseCriteria,

    -- ** AwsJobPresignedUrlConfig
    awsJobPresignedUrlConfig_expiresInSec,

    -- ** AwsJobRateIncreaseCriteria
    awsJobRateIncreaseCriteria_numberOfNotifiedThings,
    awsJobRateIncreaseCriteria_numberOfSucceededThings,

    -- ** AwsJobTimeoutConfig
    awsJobTimeoutConfig_inProgressTimeoutInMinutes,

    -- ** Behavior
    behavior_criteria,
    behavior_metric,
    behavior_metricDimension,
    behavior_suppressAlerts,
    behavior_name,

    -- ** BehaviorCriteria
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_consecutiveDatapointsToClear,
    behaviorCriteria_durationSeconds,
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_value,

    -- ** BehaviorModelTrainingSummary
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_modelStatus,
    behaviorModelTrainingSummary_securityProfileName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,

    -- ** BillingGroupMetadata
    billingGroupMetadata_creationDate,

    -- ** BillingGroupProperties
    billingGroupProperties_billingGroupDescription,

    -- ** Bucket
    bucket_count,
    bucket_keyValue,

    -- ** BucketsAggregationType
    bucketsAggregationType_termsAggregation,

    -- ** CACertificate
    cACertificate_certificateArn,
    cACertificate_certificateId,
    cACertificate_creationDate,
    cACertificate_status,

    -- ** CACertificateDescription
    cACertificateDescription_autoRegistrationStatus,
    cACertificateDescription_certificateArn,
    cACertificateDescription_certificateId,
    cACertificateDescription_certificateMode,
    cACertificateDescription_certificatePem,
    cACertificateDescription_creationDate,
    cACertificateDescription_customerVersion,
    cACertificateDescription_generationId,
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_ownedBy,
    cACertificateDescription_status,
    cACertificateDescription_validity,

    -- ** Certificate
    certificate_certificateArn,
    certificate_certificateId,
    certificate_certificateMode,
    certificate_creationDate,
    certificate_status,

    -- ** CertificateDescription
    certificateDescription_caCertificateId,
    certificateDescription_certificateArn,
    certificateDescription_certificateId,
    certificateDescription_certificateMode,
    certificateDescription_certificatePem,
    certificateDescription_creationDate,
    certificateDescription_customerVersion,
    certificateDescription_generationId,
    certificateDescription_lastModifiedDate,
    certificateDescription_ownedBy,
    certificateDescription_previousOwnedBy,
    certificateDescription_status,
    certificateDescription_transferData,
    certificateDescription_validity,

    -- ** CertificateValidity
    certificateValidity_notAfter,
    certificateValidity_notBefore,

    -- ** CloudwatchAlarmAction
    cloudwatchAlarmAction_roleArn,
    cloudwatchAlarmAction_alarmName,
    cloudwatchAlarmAction_stateReason,
    cloudwatchAlarmAction_stateValue,

    -- ** CloudwatchLogsAction
    cloudwatchLogsAction_roleArn,
    cloudwatchLogsAction_logGroupName,

    -- ** CloudwatchMetricAction
    cloudwatchMetricAction_metricTimestamp,
    cloudwatchMetricAction_roleArn,
    cloudwatchMetricAction_metricNamespace,
    cloudwatchMetricAction_metricName,
    cloudwatchMetricAction_metricValue,
    cloudwatchMetricAction_metricUnit,

    -- ** CodeSigning
    codeSigning_awsSignerJobId,
    codeSigning_customCodeSigning,
    codeSigning_startSigningJobParameter,

    -- ** CodeSigningCertificateChain
    codeSigningCertificateChain_certificateName,
    codeSigningCertificateChain_inlineDocument,

    -- ** CodeSigningSignature
    codeSigningSignature_inlineDocument,

    -- ** Configuration
    configuration_enabled,

    -- ** CustomCodeSigning
    customCodeSigning_certificateChain,
    customCodeSigning_hashAlgorithm,
    customCodeSigning_signature,
    customCodeSigning_signatureAlgorithm,

    -- ** Denied
    denied_explicitDeny,
    denied_implicitDeny,

    -- ** Destination
    destination_s3Destination,

    -- ** DetectMitigationActionExecution
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_errorCode,
    detectMitigationActionExecution_executionEndDate,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_violationId,

    -- ** DetectMitigationActionsTaskStatistics
    detectMitigationActionsTaskStatistics_actionsExecuted,
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,

    -- ** DetectMitigationActionsTaskSummary
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_taskStatus,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,

    -- ** DetectMitigationActionsTaskTarget
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,
    detectMitigationActionsTaskTarget_violationIds,

    -- ** DocumentParameter
    documentParameter_description,
    documentParameter_example,
    documentParameter_key,
    documentParameter_optional,
    documentParameter_regex,

    -- ** DomainConfigurationSummary
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_serviceType,

    -- ** DynamoDBAction
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_rangeKeyValue,
    dynamoDBAction_tableName,
    dynamoDBAction_roleArn,
    dynamoDBAction_hashKeyField,
    dynamoDBAction_hashKeyValue,

    -- ** DynamoDBv2Action
    dynamoDBv2Action_roleArn,
    dynamoDBv2Action_putItem,

    -- ** EffectivePolicy
    effectivePolicy_policyArn,
    effectivePolicy_policyDocument,
    effectivePolicy_policyName,

    -- ** ElasticsearchAction
    elasticsearchAction_roleArn,
    elasticsearchAction_endpoint,
    elasticsearchAction_index,
    elasticsearchAction_type,
    elasticsearchAction_id,

    -- ** EnableIoTLoggingParams
    enableIoTLoggingParams_roleArnForLogging,
    enableIoTLoggingParams_logLevel,

    -- ** ErrorInfo
    errorInfo_code,
    errorInfo_message,

    -- ** ExplicitDeny
    explicitDeny_policies,

    -- ** ExponentialRolloutRate
    exponentialRolloutRate_baseRatePerMinute,
    exponentialRolloutRate_incrementFactor,
    exponentialRolloutRate_rateIncreaseCriteria,

    -- ** Field
    field_name,
    field_type,

    -- ** FileLocation
    fileLocation_s3Location,
    fileLocation_stream,

    -- ** FirehoseAction
    firehoseAction_batchMode,
    firehoseAction_separator,
    firehoseAction_roleArn,
    firehoseAction_deliveryStreamName,

    -- ** FleetMetricNameAndArn
    fleetMetricNameAndArn_metricArn,
    fleetMetricNameAndArn_metricName,

    -- ** GroupNameAndArn
    groupNameAndArn_groupArn,
    groupNameAndArn_groupName,

    -- ** HttpAction
    httpAction_auth,
    httpAction_confirmationUrl,
    httpAction_headers,
    httpAction_url,

    -- ** HttpActionHeader
    httpActionHeader_key,
    httpActionHeader_value,

    -- ** HttpAuthorization
    httpAuthorization_sigv4,

    -- ** HttpContext
    httpContext_headers,
    httpContext_queryString,

    -- ** HttpUrlDestinationConfiguration
    httpUrlDestinationConfiguration_confirmationUrl,

    -- ** HttpUrlDestinationProperties
    httpUrlDestinationProperties_confirmationUrl,

    -- ** HttpUrlDestinationSummary
    httpUrlDestinationSummary_confirmationUrl,

    -- ** ImplicitDeny
    implicitDeny_policies,

    -- ** IndexingFilter
    indexingFilter_namedShadowNames,

    -- ** IotAnalyticsAction
    iotAnalyticsAction_batchMode,
    iotAnalyticsAction_channelArn,
    iotAnalyticsAction_channelName,
    iotAnalyticsAction_roleArn,

    -- ** IotEventsAction
    iotEventsAction_batchMode,
    iotEventsAction_messageId,
    iotEventsAction_inputName,
    iotEventsAction_roleArn,

    -- ** IotSiteWiseAction
    iotSiteWiseAction_putAssetPropertyValueEntries,
    iotSiteWiseAction_roleArn,

    -- ** IssuerCertificateIdentifier
    issuerCertificateIdentifier_issuerCertificateSerialNumber,
    issuerCertificateIdentifier_issuerCertificateSubject,
    issuerCertificateIdentifier_issuerId,

    -- ** Job
    job_abortConfig,
    job_comment,
    job_completedAt,
    job_createdAt,
    job_description,
    job_documentParameters,
    job_forceCanceled,
    job_isConcurrent,
    job_jobArn,
    job_jobExecutionsRetryConfig,
    job_jobExecutionsRolloutConfig,
    job_jobId,
    job_jobProcessDetails,
    job_jobTemplateArn,
    job_lastUpdatedAt,
    job_namespaceId,
    job_presignedUrlConfig,
    job_reasonCode,
    job_schedulingConfig,
    job_status,
    job_targetSelection,
    job_targets,
    job_timeoutConfig,

    -- ** JobExecution
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_executionNumber,
    jobExecution_forceCanceled,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_queuedAt,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingArn,
    jobExecution_versionNumber,

    -- ** JobExecutionStatusDetails
    jobExecutionStatusDetails_detailsMap,

    -- ** JobExecutionSummary
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_retryAttempt,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_status,

    -- ** JobExecutionSummaryForJob
    jobExecutionSummaryForJob_jobExecutionSummary,
    jobExecutionSummaryForJob_thingArn,

    -- ** JobExecutionSummaryForThing
    jobExecutionSummaryForThing_jobExecutionSummary,
    jobExecutionSummaryForThing_jobId,

    -- ** JobExecutionsRetryConfig
    jobExecutionsRetryConfig_criteriaList,

    -- ** JobExecutionsRolloutConfig
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- ** JobProcessDetails
    jobProcessDetails_numberOfCanceledThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfTimedOutThings,
    jobProcessDetails_processingTargets,

    -- ** JobSummary
    jobSummary_completedAt,
    jobSummary_createdAt,
    jobSummary_isConcurrent,
    jobSummary_jobArn,
    jobSummary_jobId,
    jobSummary_lastUpdatedAt,
    jobSummary_status,
    jobSummary_targetSelection,
    jobSummary_thingGroupId,

    -- ** JobTemplateSummary
    jobTemplateSummary_createdAt,
    jobTemplateSummary_description,
    jobTemplateSummary_jobTemplateArn,
    jobTemplateSummary_jobTemplateId,

    -- ** KafkaAction
    kafkaAction_key,
    kafkaAction_partition,
    kafkaAction_destinationArn,
    kafkaAction_topic,
    kafkaAction_clientProperties,

    -- ** KeyPair
    keyPair_privateKey,
    keyPair_publicKey,

    -- ** KinesisAction
    kinesisAction_partitionKey,
    kinesisAction_roleArn,
    kinesisAction_streamName,

    -- ** LambdaAction
    lambdaAction_functionArn,

    -- ** LocationAction
    locationAction_timestamp,
    locationAction_roleArn,
    locationAction_trackerName,
    locationAction_deviceId,
    locationAction_latitude,
    locationAction_longitude,

    -- ** LocationTimestamp
    locationTimestamp_unit,
    locationTimestamp_value,

    -- ** LogTarget
    logTarget_targetName,
    logTarget_targetType,

    -- ** LogTargetConfiguration
    logTargetConfiguration_logLevel,
    logTargetConfiguration_logTarget,

    -- ** LoggingOptionsPayload
    loggingOptionsPayload_logLevel,
    loggingOptionsPayload_roleArn,

    -- ** MachineLearningDetectionConfig
    machineLearningDetectionConfig_confidenceLevel,

    -- ** ManagedJobTemplateSummary
    managedJobTemplateSummary_description,
    managedJobTemplateSummary_environments,
    managedJobTemplateSummary_templateArn,
    managedJobTemplateSummary_templateName,
    managedJobTemplateSummary_templateVersion,

    -- ** MetricDatum
    metricDatum_timestamp,
    metricDatum_value,

    -- ** MetricDimension
    metricDimension_operator,
    metricDimension_dimensionName,

    -- ** MetricToRetain
    metricToRetain_metricDimension,
    metricToRetain_metric,

    -- ** MetricValue
    metricValue_cidrs,
    metricValue_count,
    metricValue_number,
    metricValue_numbers,
    metricValue_ports,
    metricValue_strings,

    -- ** MitigationAction
    mitigationAction_actionParams,
    mitigationAction_id,
    mitigationAction_name,
    mitigationAction_roleArn,

    -- ** MitigationActionIdentifier
    mitigationActionIdentifier_actionArn,
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_creationDate,

    -- ** MitigationActionParams
    mitigationActionParams_addThingsToThingGroupParams,
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_publishFindingToSnsParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_updateCACertificateParams,
    mitigationActionParams_updateDeviceCertificateParams,

    -- ** MqttContext
    mqttContext_clientId,
    mqttContext_password,
    mqttContext_username,

    -- ** MqttHeaders
    mqttHeaders_contentType,
    mqttHeaders_correlationData,
    mqttHeaders_messageExpiry,
    mqttHeaders_payloadFormatIndicator,
    mqttHeaders_responseTopic,
    mqttHeaders_userProperties,

    -- ** NonCompliantResource
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceIdentifier,
    nonCompliantResource_resourceType,

    -- ** OTAUpdateFile
    oTAUpdateFile_attributes,
    oTAUpdateFile_codeSigning,
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_fileName,
    oTAUpdateFile_fileType,
    oTAUpdateFile_fileVersion,

    -- ** OTAUpdateInfo
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_description,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_targetSelection,
    oTAUpdateInfo_targets,

    -- ** OTAUpdateSummary
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateArn,
    oTAUpdateSummary_otaUpdateId,

    -- ** OpenSearchAction
    openSearchAction_roleArn,
    openSearchAction_endpoint,
    openSearchAction_index,
    openSearchAction_type,
    openSearchAction_id,

    -- ** OutgoingCertificate
    outgoingCertificate_certificateArn,
    outgoingCertificate_certificateId,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferDate,
    outgoingCertificate_transferMessage,
    outgoingCertificate_transferredTo,

    -- ** PercentPair
    percentPair_percent,
    percentPair_value,

    -- ** Policy
    policy_policyArn,
    policy_policyName,

    -- ** PolicyVersion
    policyVersion_createDate,
    policyVersion_isDefaultVersion,
    policyVersion_versionId,

    -- ** PolicyVersionIdentifier
    policyVersionIdentifier_policyName,
    policyVersionIdentifier_policyVersionId,

    -- ** PresignedUrlConfig
    presignedUrlConfig_expiresInSec,
    presignedUrlConfig_roleArn,

    -- ** ProvisioningHook
    provisioningHook_payloadVersion,
    provisioningHook_targetArn,

    -- ** ProvisioningTemplateSummary
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_description,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_templateArn,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_type,

    -- ** ProvisioningTemplateVersionSummary
    provisioningTemplateVersionSummary_creationDate,
    provisioningTemplateVersionSummary_isDefaultVersion,
    provisioningTemplateVersionSummary_versionId,

    -- ** PublishFindingToSnsParams
    publishFindingToSnsParams_topicArn,

    -- ** PutAssetPropertyValueEntry
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_propertyValues,

    -- ** PutItemInput
    putItemInput_tableName,

    -- ** RateIncreaseCriteria
    rateIncreaseCriteria_numberOfNotifiedThings,
    rateIncreaseCriteria_numberOfSucceededThings,

    -- ** RegistrationConfig
    registrationConfig_roleArn,
    registrationConfig_templateBody,
    registrationConfig_templateName,

    -- ** RelatedResource
    relatedResource_additionalInfo,
    relatedResource_resourceIdentifier,
    relatedResource_resourceType,

    -- ** ReplaceDefaultPolicyVersionParams
    replaceDefaultPolicyVersionParams_templateName,

    -- ** RepublishAction
    republishAction_headers,
    republishAction_qos,
    republishAction_roleArn,
    republishAction_topic,

    -- ** ResourceIdentifier
    resourceIdentifier_account,
    resourceIdentifier_caCertificateId,
    resourceIdentifier_clientId,
    resourceIdentifier_cognitoIdentityPoolId,
    resourceIdentifier_deviceCertificateArn,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_issuerCertificateIdentifier,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_roleAliasArn,

    -- ** RetryCriteria
    retryCriteria_failureType,
    retryCriteria_numberOfRetries,

    -- ** RoleAliasDescription
    roleAliasDescription_creationDate,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_owner,
    roleAliasDescription_roleAlias,
    roleAliasDescription_roleAliasArn,
    roleAliasDescription_roleArn,

    -- ** S3Action
    s3Action_cannedAcl,
    s3Action_roleArn,
    s3Action_bucketName,
    s3Action_key,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_prefix,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,
    s3Location_version,

    -- ** SalesforceAction
    salesforceAction_token,
    salesforceAction_url,

    -- ** ScheduledAuditMetadata
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_frequency,
    scheduledAuditMetadata_scheduledAuditArn,
    scheduledAuditMetadata_scheduledAuditName,

    -- ** SchedulingConfig
    schedulingConfig_endBehavior,
    schedulingConfig_endTime,
    schedulingConfig_startTime,

    -- ** SecurityProfileIdentifier
    securityProfileIdentifier_name,
    securityProfileIdentifier_arn,

    -- ** SecurityProfileTarget
    securityProfileTarget_arn,

    -- ** SecurityProfileTargetMapping
    securityProfileTargetMapping_securityProfileIdentifier,
    securityProfileTargetMapping_target,

    -- ** ServerCertificateSummary
    serverCertificateSummary_serverCertificateArn,
    serverCertificateSummary_serverCertificateStatus,
    serverCertificateSummary_serverCertificateStatusDetail,

    -- ** SigV4Authorization
    sigV4Authorization_signingRegion,
    sigV4Authorization_serviceName,
    sigV4Authorization_roleArn,

    -- ** SigningProfileParameter
    signingProfileParameter_certificateArn,
    signingProfileParameter_certificatePathOnDevice,
    signingProfileParameter_platform,

    -- ** SnsAction
    snsAction_messageFormat,
    snsAction_targetArn,
    snsAction_roleArn,

    -- ** SqsAction
    sqsAction_useBase64,
    sqsAction_roleArn,
    sqsAction_queueUrl,

    -- ** StartSigningJobParameter
    startSigningJobParameter_destination,
    startSigningJobParameter_signingProfileName,
    startSigningJobParameter_signingProfileParameter,

    -- ** StatisticalThreshold
    statisticalThreshold_statistic,

    -- ** Statistics
    statistics_average,
    statistics_count,
    statistics_maximum,
    statistics_minimum,
    statistics_stdDeviation,
    statistics_sum,
    statistics_sumOfSquares,
    statistics_variance,

    -- ** StepFunctionsAction
    stepFunctionsAction_executionNamePrefix,
    stepFunctionsAction_stateMachineName,
    stepFunctionsAction_roleArn,

    -- ** Stream
    stream_fileId,
    stream_streamId,

    -- ** StreamFile
    streamFile_fileId,
    streamFile_s3Location,

    -- ** StreamInfo
    streamInfo_createdAt,
    streamInfo_description,
    streamInfo_files,
    streamInfo_lastUpdatedAt,
    streamInfo_roleArn,
    streamInfo_streamArn,
    streamInfo_streamId,
    streamInfo_streamVersion,

    -- ** StreamSummary
    streamSummary_description,
    streamSummary_streamArn,
    streamSummary_streamId,
    streamSummary_streamVersion,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TaskStatistics
    taskStatistics_canceledChecks,
    taskStatistics_compliantChecks,
    taskStatistics_failedChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_nonCompliantChecks,
    taskStatistics_totalChecks,
    taskStatistics_waitingForDataCollectionChecks,

    -- ** TaskStatisticsForAuditCheck
    taskStatisticsForAuditCheck_canceledFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_succeededFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,

    -- ** TermsAggregation
    termsAggregation_maxBuckets,

    -- ** ThingAttribute
    thingAttribute_attributes,
    thingAttribute_thingArn,
    thingAttribute_thingName,
    thingAttribute_thingTypeName,
    thingAttribute_version,

    -- ** ThingConnectivity
    thingConnectivity_connected,
    thingConnectivity_disconnectReason,
    thingConnectivity_timestamp,

    -- ** ThingDocument
    thingDocument_attributes,
    thingDocument_connectivity,
    thingDocument_deviceDefender,
    thingDocument_shadow,
    thingDocument_thingGroupNames,
    thingDocument_thingId,
    thingDocument_thingName,
    thingDocument_thingTypeName,

    -- ** ThingGroupDocument
    thingGroupDocument_attributes,
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_thingGroupDescription,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupName,

    -- ** ThingGroupIndexingConfiguration
    thingGroupIndexingConfiguration_customFields,
    thingGroupIndexingConfiguration_managedFields,
    thingGroupIndexingConfiguration_thingGroupIndexingMode,

    -- ** ThingGroupMetadata
    thingGroupMetadata_creationDate,
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_rootToParentThingGroups,

    -- ** ThingGroupProperties
    thingGroupProperties_attributePayload,
    thingGroupProperties_thingGroupDescription,

    -- ** ThingIndexingConfiguration
    thingIndexingConfiguration_customFields,
    thingIndexingConfiguration_deviceDefenderIndexingMode,
    thingIndexingConfiguration_filter,
    thingIndexingConfiguration_managedFields,
    thingIndexingConfiguration_namedShadowIndexingMode,
    thingIndexingConfiguration_thingConnectivityIndexingMode,
    thingIndexingConfiguration_thingIndexingMode,

    -- ** ThingTypeDefinition
    thingTypeDefinition_thingTypeArn,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeName,
    thingTypeDefinition_thingTypeProperties,

    -- ** ThingTypeMetadata
    thingTypeMetadata_creationDate,
    thingTypeMetadata_deprecated,
    thingTypeMetadata_deprecationDate,

    -- ** ThingTypeProperties
    thingTypeProperties_searchableAttributes,
    thingTypeProperties_thingTypeDescription,

    -- ** TimeoutConfig
    timeoutConfig_inProgressTimeoutInMinutes,

    -- ** TimestreamAction
    timestreamAction_timestamp,
    timestreamAction_roleArn,
    timestreamAction_databaseName,
    timestreamAction_tableName,
    timestreamAction_dimensions,

    -- ** TimestreamDimension
    timestreamDimension_name,
    timestreamDimension_value,

    -- ** TimestreamTimestamp
    timestreamTimestamp_value,
    timestreamTimestamp_unit,

    -- ** TlsContext
    tlsContext_serverName,

    -- ** TopicRule
    topicRule_actions,
    topicRule_awsIotSqlVersion,
    topicRule_createdAt,
    topicRule_description,
    topicRule_errorAction,
    topicRule_ruleDisabled,
    topicRule_ruleName,
    topicRule_sql,

    -- ** TopicRuleDestination
    topicRuleDestination_arn,
    topicRuleDestination_createdAt,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_lastUpdatedAt,
    topicRuleDestination_status,
    topicRuleDestination_statusReason,
    topicRuleDestination_vpcProperties,

    -- ** TopicRuleDestinationConfiguration
    topicRuleDestinationConfiguration_httpUrlConfiguration,
    topicRuleDestinationConfiguration_vpcConfiguration,

    -- ** TopicRuleDestinationSummary
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_lastUpdatedAt,
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_statusReason,
    topicRuleDestinationSummary_vpcDestinationSummary,

    -- ** TopicRuleListItem
    topicRuleListItem_createdAt,
    topicRuleListItem_ruleArn,
    topicRuleListItem_ruleDisabled,
    topicRuleListItem_ruleName,
    topicRuleListItem_topicPattern,

    -- ** TopicRulePayload
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_description,
    topicRulePayload_errorAction,
    topicRulePayload_ruleDisabled,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- ** TransferData
    transferData_acceptDate,
    transferData_rejectDate,
    transferData_rejectReason,
    transferData_transferDate,
    transferData_transferMessage,

    -- ** UpdateCACertificateParams
    updateCACertificateParams_action,

    -- ** UpdateDeviceCertificateParams
    updateDeviceCertificateParams_action,

    -- ** UserProperty
    userProperty_key,
    userProperty_value,

    -- ** ValidationError
    validationError_errorMessage,

    -- ** ViolationEvent
    violationEvent_behavior,
    violationEvent_metricValue,
    violationEvent_securityProfileName,
    violationEvent_thingName,
    violationEvent_verificationState,
    violationEvent_verificationStateDescription,
    violationEvent_violationEventAdditionalInfo,
    violationEvent_violationEventTime,
    violationEvent_violationEventType,
    violationEvent_violationId,

    -- ** ViolationEventAdditionalInfo
    violationEventAdditionalInfo_confidenceLevel,

    -- ** ViolationEventOccurrenceRange
    violationEventOccurrenceRange_startTime,
    violationEventOccurrenceRange_endTime,

    -- ** VpcDestinationConfiguration
    vpcDestinationConfiguration_securityGroups,
    vpcDestinationConfiguration_subnetIds,
    vpcDestinationConfiguration_vpcId,
    vpcDestinationConfiguration_roleArn,

    -- ** VpcDestinationProperties
    vpcDestinationProperties_roleArn,
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_vpcId,

    -- ** VpcDestinationSummary
    vpcDestinationSummary_roleArn,
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_vpcId,
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
import Amazonka.IoT.GetPercentiles
import Amazonka.IoT.GetPolicy
import Amazonka.IoT.GetPolicyVersion
import Amazonka.IoT.GetRegistrationCode
import Amazonka.IoT.GetStatistics
import Amazonka.IoT.GetTopicRule
import Amazonka.IoT.GetTopicRuleDestination
import Amazonka.IoT.GetV2LoggingOptions
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
import Amazonka.IoT.Types.AbortConfig
import Amazonka.IoT.Types.AbortCriteria
import Amazonka.IoT.Types.Action
import Amazonka.IoT.Types.ActiveViolation
import Amazonka.IoT.Types.AddThingsToThingGroupParams
import Amazonka.IoT.Types.AggregationType
import Amazonka.IoT.Types.AlertTarget
import Amazonka.IoT.Types.Allowed
import Amazonka.IoT.Types.AssetPropertyTimestamp
import Amazonka.IoT.Types.AssetPropertyValue
import Amazonka.IoT.Types.AssetPropertyVariant
import Amazonka.IoT.Types.AttributePayload
import Amazonka.IoT.Types.AuditCheckConfiguration
import Amazonka.IoT.Types.AuditCheckDetails
import Amazonka.IoT.Types.AuditFinding
import Amazonka.IoT.Types.AuditMitigationActionExecutionMetadata
import Amazonka.IoT.Types.AuditMitigationActionsTaskMetadata
import Amazonka.IoT.Types.AuditMitigationActionsTaskTarget
import Amazonka.IoT.Types.AuditNotificationTarget
import Amazonka.IoT.Types.AuditSuppression
import Amazonka.IoT.Types.AuditTaskMetadata
import Amazonka.IoT.Types.AuthInfo
import Amazonka.IoT.Types.AuthResult
import Amazonka.IoT.Types.AuthorizerConfig
import Amazonka.IoT.Types.AuthorizerDescription
import Amazonka.IoT.Types.AuthorizerSummary
import Amazonka.IoT.Types.AwsJobAbortConfig
import Amazonka.IoT.Types.AwsJobAbortCriteria
import Amazonka.IoT.Types.AwsJobExecutionsRolloutConfig
import Amazonka.IoT.Types.AwsJobExponentialRolloutRate
import Amazonka.IoT.Types.AwsJobPresignedUrlConfig
import Amazonka.IoT.Types.AwsJobRateIncreaseCriteria
import Amazonka.IoT.Types.AwsJobTimeoutConfig
import Amazonka.IoT.Types.Behavior
import Amazonka.IoT.Types.BehaviorCriteria
import Amazonka.IoT.Types.BehaviorModelTrainingSummary
import Amazonka.IoT.Types.BillingGroupMetadata
import Amazonka.IoT.Types.BillingGroupProperties
import Amazonka.IoT.Types.Bucket
import Amazonka.IoT.Types.BucketsAggregationType
import Amazonka.IoT.Types.CACertificate
import Amazonka.IoT.Types.CACertificateDescription
import Amazonka.IoT.Types.Certificate
import Amazonka.IoT.Types.CertificateDescription
import Amazonka.IoT.Types.CertificateValidity
import Amazonka.IoT.Types.CloudwatchAlarmAction
import Amazonka.IoT.Types.CloudwatchLogsAction
import Amazonka.IoT.Types.CloudwatchMetricAction
import Amazonka.IoT.Types.CodeSigning
import Amazonka.IoT.Types.CodeSigningCertificateChain
import Amazonka.IoT.Types.CodeSigningSignature
import Amazonka.IoT.Types.Configuration
import Amazonka.IoT.Types.CustomCodeSigning
import Amazonka.IoT.Types.Denied
import Amazonka.IoT.Types.Destination
import Amazonka.IoT.Types.DetectMitigationActionExecution
import Amazonka.IoT.Types.DetectMitigationActionsTaskStatistics
import Amazonka.IoT.Types.DetectMitigationActionsTaskSummary
import Amazonka.IoT.Types.DetectMitigationActionsTaskTarget
import Amazonka.IoT.Types.DocumentParameter
import Amazonka.IoT.Types.DomainConfigurationSummary
import Amazonka.IoT.Types.DynamoDBAction
import Amazonka.IoT.Types.DynamoDBv2Action
import Amazonka.IoT.Types.EffectivePolicy
import Amazonka.IoT.Types.ElasticsearchAction
import Amazonka.IoT.Types.EnableIoTLoggingParams
import Amazonka.IoT.Types.ErrorInfo
import Amazonka.IoT.Types.ExplicitDeny
import Amazonka.IoT.Types.ExponentialRolloutRate
import Amazonka.IoT.Types.Field
import Amazonka.IoT.Types.FileLocation
import Amazonka.IoT.Types.FirehoseAction
import Amazonka.IoT.Types.FleetMetricNameAndArn
import Amazonka.IoT.Types.GroupNameAndArn
import Amazonka.IoT.Types.HttpAction
import Amazonka.IoT.Types.HttpActionHeader
import Amazonka.IoT.Types.HttpAuthorization
import Amazonka.IoT.Types.HttpContext
import Amazonka.IoT.Types.HttpUrlDestinationConfiguration
import Amazonka.IoT.Types.HttpUrlDestinationProperties
import Amazonka.IoT.Types.HttpUrlDestinationSummary
import Amazonka.IoT.Types.ImplicitDeny
import Amazonka.IoT.Types.IndexingFilter
import Amazonka.IoT.Types.IotAnalyticsAction
import Amazonka.IoT.Types.IotEventsAction
import Amazonka.IoT.Types.IotSiteWiseAction
import Amazonka.IoT.Types.IssuerCertificateIdentifier
import Amazonka.IoT.Types.Job
import Amazonka.IoT.Types.JobExecution
import Amazonka.IoT.Types.JobExecutionStatusDetails
import Amazonka.IoT.Types.JobExecutionSummary
import Amazonka.IoT.Types.JobExecutionSummaryForJob
import Amazonka.IoT.Types.JobExecutionSummaryForThing
import Amazonka.IoT.Types.JobExecutionsRetryConfig
import Amazonka.IoT.Types.JobExecutionsRolloutConfig
import Amazonka.IoT.Types.JobProcessDetails
import Amazonka.IoT.Types.JobSummary
import Amazonka.IoT.Types.JobTemplateSummary
import Amazonka.IoT.Types.KafkaAction
import Amazonka.IoT.Types.KeyPair
import Amazonka.IoT.Types.KinesisAction
import Amazonka.IoT.Types.LambdaAction
import Amazonka.IoT.Types.LocationAction
import Amazonka.IoT.Types.LocationTimestamp
import Amazonka.IoT.Types.LogTarget
import Amazonka.IoT.Types.LogTargetConfiguration
import Amazonka.IoT.Types.LoggingOptionsPayload
import Amazonka.IoT.Types.MachineLearningDetectionConfig
import Amazonka.IoT.Types.ManagedJobTemplateSummary
import Amazonka.IoT.Types.MetricDatum
import Amazonka.IoT.Types.MetricDimension
import Amazonka.IoT.Types.MetricToRetain
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.MitigationAction
import Amazonka.IoT.Types.MitigationActionIdentifier
import Amazonka.IoT.Types.MitigationActionParams
import Amazonka.IoT.Types.MqttContext
import Amazonka.IoT.Types.MqttHeaders
import Amazonka.IoT.Types.NonCompliantResource
import Amazonka.IoT.Types.OTAUpdateFile
import Amazonka.IoT.Types.OTAUpdateInfo
import Amazonka.IoT.Types.OTAUpdateSummary
import Amazonka.IoT.Types.OpenSearchAction
import Amazonka.IoT.Types.OutgoingCertificate
import Amazonka.IoT.Types.PercentPair
import Amazonka.IoT.Types.Policy
import Amazonka.IoT.Types.PolicyVersion
import Amazonka.IoT.Types.PolicyVersionIdentifier
import Amazonka.IoT.Types.PresignedUrlConfig
import Amazonka.IoT.Types.ProvisioningHook
import Amazonka.IoT.Types.ProvisioningTemplateSummary
import Amazonka.IoT.Types.ProvisioningTemplateVersionSummary
import Amazonka.IoT.Types.PublishFindingToSnsParams
import Amazonka.IoT.Types.PutAssetPropertyValueEntry
import Amazonka.IoT.Types.PutItemInput
import Amazonka.IoT.Types.RateIncreaseCriteria
import Amazonka.IoT.Types.RegistrationConfig
import Amazonka.IoT.Types.RelatedResource
import Amazonka.IoT.Types.ReplaceDefaultPolicyVersionParams
import Amazonka.IoT.Types.RepublishAction
import Amazonka.IoT.Types.ResourceIdentifier
import Amazonka.IoT.Types.RetryCriteria
import Amazonka.IoT.Types.RoleAliasDescription
import Amazonka.IoT.Types.S3Action
import Amazonka.IoT.Types.S3Destination
import Amazonka.IoT.Types.S3Location
import Amazonka.IoT.Types.SalesforceAction
import Amazonka.IoT.Types.ScheduledAuditMetadata
import Amazonka.IoT.Types.SchedulingConfig
import Amazonka.IoT.Types.SecurityProfileIdentifier
import Amazonka.IoT.Types.SecurityProfileTarget
import Amazonka.IoT.Types.SecurityProfileTargetMapping
import Amazonka.IoT.Types.ServerCertificateSummary
import Amazonka.IoT.Types.SigV4Authorization
import Amazonka.IoT.Types.SigningProfileParameter
import Amazonka.IoT.Types.SnsAction
import Amazonka.IoT.Types.SqsAction
import Amazonka.IoT.Types.StartSigningJobParameter
import Amazonka.IoT.Types.StatisticalThreshold
import Amazonka.IoT.Types.Statistics
import Amazonka.IoT.Types.StepFunctionsAction
import Amazonka.IoT.Types.Stream
import Amazonka.IoT.Types.StreamFile
import Amazonka.IoT.Types.StreamInfo
import Amazonka.IoT.Types.StreamSummary
import Amazonka.IoT.Types.Tag
import Amazonka.IoT.Types.TaskStatistics
import Amazonka.IoT.Types.TaskStatisticsForAuditCheck
import Amazonka.IoT.Types.TermsAggregation
import Amazonka.IoT.Types.ThingAttribute
import Amazonka.IoT.Types.ThingConnectivity
import Amazonka.IoT.Types.ThingDocument
import Amazonka.IoT.Types.ThingGroupDocument
import Amazonka.IoT.Types.ThingGroupIndexingConfiguration
import Amazonka.IoT.Types.ThingGroupMetadata
import Amazonka.IoT.Types.ThingGroupProperties
import Amazonka.IoT.Types.ThingIndexingConfiguration
import Amazonka.IoT.Types.ThingTypeDefinition
import Amazonka.IoT.Types.ThingTypeMetadata
import Amazonka.IoT.Types.ThingTypeProperties
import Amazonka.IoT.Types.TimeoutConfig
import Amazonka.IoT.Types.TimestreamAction
import Amazonka.IoT.Types.TimestreamDimension
import Amazonka.IoT.Types.TimestreamTimestamp
import Amazonka.IoT.Types.TlsContext
import Amazonka.IoT.Types.TopicRule
import Amazonka.IoT.Types.TopicRuleDestination
import Amazonka.IoT.Types.TopicRuleDestinationConfiguration
import Amazonka.IoT.Types.TopicRuleDestinationSummary
import Amazonka.IoT.Types.TopicRuleListItem
import Amazonka.IoT.Types.TopicRulePayload
import Amazonka.IoT.Types.TransferData
import Amazonka.IoT.Types.UpdateCACertificateParams
import Amazonka.IoT.Types.UpdateDeviceCertificateParams
import Amazonka.IoT.Types.UserProperty
import Amazonka.IoT.Types.ValidationError
import Amazonka.IoT.Types.ViolationEvent
import Amazonka.IoT.Types.ViolationEventAdditionalInfo
import Amazonka.IoT.Types.ViolationEventOccurrenceRange
import Amazonka.IoT.Types.VpcDestinationConfiguration
import Amazonka.IoT.Types.VpcDestinationProperties
import Amazonka.IoT.Types.VpcDestinationSummary
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

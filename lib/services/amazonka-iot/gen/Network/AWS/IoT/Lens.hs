{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Lens
  ( -- * Operations

    -- ** GetBucketsAggregation
    getBucketsAggregation_indexName,
    getBucketsAggregation_queryVersion,
    getBucketsAggregation_queryString,
    getBucketsAggregation_aggregationField,
    getBucketsAggregation_bucketsAggregationType,
    getBucketsAggregationResponse_buckets,
    getBucketsAggregationResponse_totalCount,
    getBucketsAggregationResponse_httpStatus,

    -- ** UpdateIndexingConfiguration
    updateIndexingConfiguration_thingGroupIndexingConfiguration,
    updateIndexingConfiguration_thingIndexingConfiguration,
    updateIndexingConfigurationResponse_httpStatus,

    -- ** PutVerificationStateOnViolation
    putVerificationStateOnViolation_verificationStateDescription,
    putVerificationStateOnViolation_violationId,
    putVerificationStateOnViolation_verificationState,
    putVerificationStateOnViolationResponse_httpStatus,

    -- ** ListThingRegistrationTaskReports
    listThingRegistrationTaskReports_nextToken,
    listThingRegistrationTaskReports_maxResults,
    listThingRegistrationTaskReports_taskId,
    listThingRegistrationTaskReports_reportType,
    listThingRegistrationTaskReportsResponse_nextToken,
    listThingRegistrationTaskReportsResponse_reportType,
    listThingRegistrationTaskReportsResponse_resourceLinks,
    listThingRegistrationTaskReportsResponse_httpStatus,

    -- ** DescribeFleetMetric
    describeFleetMetric_metricName,
    describeFleetMetricResponse_lastModifiedDate,
    describeFleetMetricResponse_queryString,
    describeFleetMetricResponse_indexName,
    describeFleetMetricResponse_metricArn,
    describeFleetMetricResponse_unit,
    describeFleetMetricResponse_metricName,
    describeFleetMetricResponse_queryVersion,
    describeFleetMetricResponse_creationDate,
    describeFleetMetricResponse_version,
    describeFleetMetricResponse_aggregationType,
    describeFleetMetricResponse_aggregationField,
    describeFleetMetricResponse_description,
    describeFleetMetricResponse_period,
    describeFleetMetricResponse_httpStatus,

    -- ** CreateProvisioningClaim
    createProvisioningClaim_templateName,
    createProvisioningClaimResponse_expiration,
    createProvisioningClaimResponse_keyPair,
    createProvisioningClaimResponse_certificateId,
    createProvisioningClaimResponse_certificatePem,
    createProvisioningClaimResponse_httpStatus,

    -- ** UpdateCertificate
    updateCertificate_certificateId,
    updateCertificate_newStatus,

    -- ** ListMitigationActions
    listMitigationActions_nextToken,
    listMitigationActions_actionType,
    listMitigationActions_maxResults,
    listMitigationActionsResponse_nextToken,
    listMitigationActionsResponse_actionIdentifiers,
    listMitigationActionsResponse_httpStatus,

    -- ** DescribeProvisioningTemplate
    describeProvisioningTemplate_templateName,
    describeProvisioningTemplateResponse_lastModifiedDate,
    describeProvisioningTemplateResponse_templateName,
    describeProvisioningTemplateResponse_preProvisioningHook,
    describeProvisioningTemplateResponse_creationDate,
    describeProvisioningTemplateResponse_enabled,
    describeProvisioningTemplateResponse_defaultVersionId,
    describeProvisioningTemplateResponse_description,
    describeProvisioningTemplateResponse_provisioningRoleArn,
    describeProvisioningTemplateResponse_templateBody,
    describeProvisioningTemplateResponse_templateArn,
    describeProvisioningTemplateResponse_httpStatus,

    -- ** DeleteJobExecution
    deleteJobExecution_namespaceId,
    deleteJobExecution_force,
    deleteJobExecution_jobId,
    deleteJobExecution_thingName,
    deleteJobExecution_executionNumber,

    -- ** ListSecurityProfiles
    listSecurityProfiles_dimensionName,
    listSecurityProfiles_nextToken,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_metricName,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileIdentifiers,
    listSecurityProfilesResponse_httpStatus,

    -- ** UpdateMitigationAction
    updateMitigationAction_roleArn,
    updateMitigationAction_actionParams,
    updateMitigationAction_actionName,
    updateMitigationActionResponse_actionArn,
    updateMitigationActionResponse_actionId,
    updateMitigationActionResponse_httpStatus,

    -- ** DeleteMitigationAction
    deleteMitigationAction_actionName,
    deleteMitigationActionResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_pageSize,
    listPolicies_ascendingOrder,
    listPolicies_marker,
    listPoliciesResponse_policies,
    listPoliciesResponse_nextMarker,
    listPoliciesResponse_httpStatus,

    -- ** StartDetectMitigationActionsTask
    startDetectMitigationActionsTask_includeSuppressedAlerts,
    startDetectMitigationActionsTask_includeOnlyActiveViolations,
    startDetectMitigationActionsTask_violationEventOccurrenceRange,
    startDetectMitigationActionsTask_taskId,
    startDetectMitigationActionsTask_target,
    startDetectMitigationActionsTask_actions,
    startDetectMitigationActionsTask_clientRequestToken,
    startDetectMitigationActionsTaskResponse_taskId,
    startDetectMitigationActionsTaskResponse_httpStatus,

    -- ** GetCardinality
    getCardinality_indexName,
    getCardinality_queryVersion,
    getCardinality_aggregationField,
    getCardinality_queryString,
    getCardinalityResponse_cardinality,
    getCardinalityResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_forceDelete,
    deleteCertificate_certificateId,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,
    createPolicyResponse_policyDocument,
    createPolicyResponse_policyVersionId,
    createPolicyResponse_policyName,
    createPolicyResponse_policyArn,
    createPolicyResponse_httpStatus,

    -- ** ListViolationEvents
    listViolationEvents_nextToken,
    listViolationEvents_maxResults,
    listViolationEvents_thingName,
    listViolationEvents_securityProfileName,
    listViolationEvents_listSuppressedAlerts,
    listViolationEvents_behaviorCriteriaType,
    listViolationEvents_verificationState,
    listViolationEvents_startTime,
    listViolationEvents_endTime,
    listViolationEventsResponse_nextToken,
    listViolationEventsResponse_violationEvents,
    listViolationEventsResponse_httpStatus,

    -- ** CreateDimension
    createDimension_tags,
    createDimension_name,
    createDimension_type,
    createDimension_stringValues,
    createDimension_clientRequestToken,
    createDimensionResponse_arn,
    createDimensionResponse_name,
    createDimensionResponse_httpStatus,

    -- ** ListAuditTasks
    listAuditTasks_nextToken,
    listAuditTasks_maxResults,
    listAuditTasks_taskStatus,
    listAuditTasks_taskType,
    listAuditTasks_startTime,
    listAuditTasks_endTime,
    listAuditTasksResponse_nextToken,
    listAuditTasksResponse_tasks,
    listAuditTasksResponse_httpStatus,

    -- ** DescribeThingType
    describeThingType_thingTypeName,
    describeThingTypeResponse_thingTypeProperties,
    describeThingTypeResponse_thingTypeMetadata,
    describeThingTypeResponse_thingTypeId,
    describeThingTypeResponse_thingTypeArn,
    describeThingTypeResponse_thingTypeName,
    describeThingTypeResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyName,

    -- ** CreateMitigationAction
    createMitigationAction_tags,
    createMitigationAction_actionName,
    createMitigationAction_roleArn,
    createMitigationAction_actionParams,
    createMitigationActionResponse_actionArn,
    createMitigationActionResponse_actionId,
    createMitigationActionResponse_httpStatus,

    -- ** DeleteDomainConfiguration
    deleteDomainConfiguration_domainConfigurationName,
    deleteDomainConfigurationResponse_httpStatus,

    -- ** GetTopicRule
    getTopicRule_ruleName,
    getTopicRuleResponse_ruleArn,
    getTopicRuleResponse_rule,
    getTopicRuleResponse_httpStatus,

    -- ** UpdateDomainConfiguration
    updateDomainConfiguration_domainConfigurationStatus,
    updateDomainConfiguration_authorizerConfig,
    updateDomainConfiguration_removeAuthorizerConfig,
    updateDomainConfiguration_domainConfigurationName,
    updateDomainConfigurationResponse_domainConfigurationArn,
    updateDomainConfigurationResponse_domainConfigurationName,
    updateDomainConfigurationResponse_httpStatus,

    -- ** RejectCertificateTransfer
    rejectCertificateTransfer_rejectReason,
    rejectCertificateTransfer_certificateId,

    -- ** SetLoggingOptions
    setLoggingOptions_loggingOptionsPayload,

    -- ** CreateSecurityProfile
    createSecurityProfile_alertTargets,
    createSecurityProfile_additionalMetricsToRetain,
    createSecurityProfile_behaviors,
    createSecurityProfile_additionalMetricsToRetainV2,
    createSecurityProfile_tags,
    createSecurityProfile_securityProfileDescription,
    createSecurityProfile_securityProfileName,
    createSecurityProfileResponse_securityProfileName,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_httpStatus,

    -- ** CancelJob
    cancelJob_reasonCode,
    cancelJob_comment,
    cancelJob_force,
    cancelJob_jobId,
    cancelJobResponse_jobArn,
    cancelJobResponse_description,
    cancelJobResponse_jobId,
    cancelJobResponse_httpStatus,

    -- ** ListDomainConfigurations
    listDomainConfigurations_pageSize,
    listDomainConfigurations_serviceType,
    listDomainConfigurations_marker,
    listDomainConfigurationsResponse_domainConfigurations,
    listDomainConfigurationsResponse_nextMarker,
    listDomainConfigurationsResponse_httpStatus,

    -- ** DescribeScheduledAudit
    describeScheduledAudit_scheduledAuditName,
    describeScheduledAuditResponse_dayOfWeek,
    describeScheduledAuditResponse_scheduledAuditArn,
    describeScheduledAuditResponse_scheduledAuditName,
    describeScheduledAuditResponse_dayOfMonth,
    describeScheduledAuditResponse_frequency,
    describeScheduledAuditResponse_targetCheckNames,
    describeScheduledAuditResponse_httpStatus,

    -- ** GetV2LoggingOptions
    getV2LoggingOptionsResponse_roleArn,
    getV2LoggingOptionsResponse_disableAllLogs,
    getV2LoggingOptionsResponse_defaultLogLevel,
    getV2LoggingOptionsResponse_httpStatus,

    -- ** ListThingsInThingGroup
    listThingsInThingGroup_nextToken,
    listThingsInThingGroup_maxResults,
    listThingsInThingGroup_recursive,
    listThingsInThingGroup_thingGroupName,
    listThingsInThingGroupResponse_nextToken,
    listThingsInThingGroupResponse_things,
    listThingsInThingGroupResponse_httpStatus,

    -- ** ListCustomMetrics
    listCustomMetrics_nextToken,
    listCustomMetrics_maxResults,
    listCustomMetricsResponse_nextToken,
    listCustomMetricsResponse_metricNames,
    listCustomMetricsResponse_httpStatus,

    -- ** AttachSecurityProfile
    attachSecurityProfile_securityProfileName,
    attachSecurityProfile_securityProfileTargetArn,
    attachSecurityProfileResponse_httpStatus,

    -- ** UpdateJob
    updateJob_jobExecutionsRolloutConfig,
    updateJob_timeoutConfig,
    updateJob_namespaceId,
    updateJob_presignedUrlConfig,
    updateJob_description,
    updateJob_abortConfig,
    updateJob_jobId,

    -- ** DeleteJob
    deleteJob_namespaceId,
    deleteJob_force,
    deleteJob_jobId,

    -- ** DeleteCACertificate
    deleteCACertificate_certificateId,
    deleteCACertificateResponse_httpStatus,

    -- ** DeleteCustomMetric
    deleteCustomMetric_metricName,
    deleteCustomMetricResponse_httpStatus,

    -- ** CreatePolicyVersion
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyName,
    createPolicyVersion_policyDocument,
    createPolicyVersionResponse_policyDocument,
    createPolicyVersionResponse_policyVersionId,
    createPolicyVersionResponse_isDefaultVersion,
    createPolicyVersionResponse_policyArn,
    createPolicyVersionResponse_httpStatus,

    -- ** ListRoleAliases
    listRoleAliases_pageSize,
    listRoleAliases_ascendingOrder,
    listRoleAliases_marker,
    listRoleAliasesResponse_roleAliases,
    listRoleAliasesResponse_nextMarker,
    listRoleAliasesResponse_httpStatus,

    -- ** CancelAuditTask
    cancelAuditTask_taskId,
    cancelAuditTaskResponse_httpStatus,

    -- ** CreateRoleAlias
    createRoleAlias_tags,
    createRoleAlias_credentialDurationSeconds,
    createRoleAlias_roleAlias,
    createRoleAlias_roleArn,
    createRoleAliasResponse_roleAliasArn,
    createRoleAliasResponse_roleAlias,
    createRoleAliasResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_nextToken,
    listJobTemplates_maxResults,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_httpStatus,

    -- ** DescribeProvisioningTemplateVersion
    describeProvisioningTemplateVersion_templateName,
    describeProvisioningTemplateVersion_versionId,
    describeProvisioningTemplateVersionResponse_creationDate,
    describeProvisioningTemplateVersionResponse_versionId,
    describeProvisioningTemplateVersionResponse_isDefaultVersion,
    describeProvisioningTemplateVersionResponse_templateBody,
    describeProvisioningTemplateVersionResponse_httpStatus,

    -- ** CreateKeysAndCertificate
    createKeysAndCertificate_setAsActive,
    createKeysAndCertificateResponse_certificateArn,
    createKeysAndCertificateResponse_keyPair,
    createKeysAndCertificateResponse_certificateId,
    createKeysAndCertificateResponse_certificatePem,
    createKeysAndCertificateResponse_httpStatus,

    -- ** UpdateCACertificate
    updateCACertificate_newStatus,
    updateCACertificate_removeAutoRegistration,
    updateCACertificate_newAutoRegistrationStatus,
    updateCACertificate_registrationConfig,
    updateCACertificate_certificateId,

    -- ** UpdateCustomMetric
    updateCustomMetric_metricName,
    updateCustomMetric_displayName,
    updateCustomMetricResponse_lastModifiedDate,
    updateCustomMetricResponse_metricType,
    updateCustomMetricResponse_metricArn,
    updateCustomMetricResponse_metricName,
    updateCustomMetricResponse_creationDate,
    updateCustomMetricResponse_displayName,
    updateCustomMetricResponse_httpStatus,

    -- ** StartAuditMitigationActionsTask
    startAuditMitigationActionsTask_taskId,
    startAuditMitigationActionsTask_target,
    startAuditMitigationActionsTask_auditCheckToActionsMapping,
    startAuditMitigationActionsTask_clientRequestToken,
    startAuditMitigationActionsTaskResponse_taskId,
    startAuditMitigationActionsTaskResponse_httpStatus,

    -- ** TransferCertificate
    transferCertificate_transferMessage,
    transferCertificate_certificateId,
    transferCertificate_targetAwsAccount,
    transferCertificateResponse_transferredCertificateArn,
    transferCertificateResponse_httpStatus,

    -- ** GetPercentiles
    getPercentiles_indexName,
    getPercentiles_percents,
    getPercentiles_queryVersion,
    getPercentiles_aggregationField,
    getPercentiles_queryString,
    getPercentilesResponse_percentiles,
    getPercentilesResponse_httpStatus,

    -- ** DeleteTopicRule
    deleteTopicRule_ruleName,

    -- ** ListTargetsForSecurityProfile
    listTargetsForSecurityProfile_nextToken,
    listTargetsForSecurityProfile_maxResults,
    listTargetsForSecurityProfile_securityProfileName,
    listTargetsForSecurityProfileResponse_securityProfileTargets,
    listTargetsForSecurityProfileResponse_nextToken,
    listTargetsForSecurityProfileResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointType,
    describeEndpointResponse_endpointAddress,
    describeEndpointResponse_httpStatus,

    -- ** ListThingsInBillingGroup
    listThingsInBillingGroup_nextToken,
    listThingsInBillingGroup_maxResults,
    listThingsInBillingGroup_billingGroupName,
    listThingsInBillingGroupResponse_nextToken,
    listThingsInBillingGroupResponse_things,
    listThingsInBillingGroupResponse_httpStatus,

    -- ** SetV2LoggingLevel
    setV2LoggingLevel_logTarget,
    setV2LoggingLevel_logLevel,

    -- ** CreateJobTemplate
    createJobTemplate_jobExecutionsRolloutConfig,
    createJobTemplate_timeoutConfig,
    createJobTemplate_jobArn,
    createJobTemplate_documentSource,
    createJobTemplate_document,
    createJobTemplate_presignedUrlConfig,
    createJobTemplate_tags,
    createJobTemplate_abortConfig,
    createJobTemplate_jobTemplateId,
    createJobTemplate_description,
    createJobTemplateResponse_jobTemplateId,
    createJobTemplateResponse_jobTemplateArn,
    createJobTemplateResponse_httpStatus,

    -- ** SetDefaultPolicyVersion
    setDefaultPolicyVersion_policyName,
    setDefaultPolicyVersion_policyVersionId,

    -- ** CreateCustomMetric
    createCustomMetric_tags,
    createCustomMetric_displayName,
    createCustomMetric_metricName,
    createCustomMetric_metricType,
    createCustomMetric_clientRequestToken,
    createCustomMetricResponse_metricArn,
    createCustomMetricResponse_metricName,
    createCustomMetricResponse_httpStatus,

    -- ** ListFleetMetrics
    listFleetMetrics_nextToken,
    listFleetMetrics_maxResults,
    listFleetMetricsResponse_nextToken,
    listFleetMetricsResponse_fleetMetrics,
    listFleetMetricsResponse_httpStatus,

    -- ** UpdateFleetMetric
    updateFleetMetric_queryString,
    updateFleetMetric_expectedVersion,
    updateFleetMetric_unit,
    updateFleetMetric_queryVersion,
    updateFleetMetric_aggregationType,
    updateFleetMetric_aggregationField,
    updateFleetMetric_description,
    updateFleetMetric_period,
    updateFleetMetric_metricName,
    updateFleetMetric_indexName,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListJobExecutionsForThing
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_thingName,
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_httpStatus,

    -- ** DeleteFleetMetric
    deleteFleetMetric_expectedVersion,
    deleteFleetMetric_metricName,

    -- ** DisableTopicRule
    disableTopicRule_ruleName,

    -- ** DescribeAuditMitigationActionsTask
    describeAuditMitigationActionsTask_taskId,
    describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping,
    describeAuditMitigationActionsTaskResponse_taskStatistics,
    describeAuditMitigationActionsTaskResponse_startTime,
    describeAuditMitigationActionsTaskResponse_endTime,
    describeAuditMitigationActionsTaskResponse_target,
    describeAuditMitigationActionsTaskResponse_taskStatus,
    describeAuditMitigationActionsTaskResponse_actionsDefinition,
    describeAuditMitigationActionsTaskResponse_httpStatus,

    -- ** CreateThing
    createThing_thingTypeName,
    createThing_billingGroupName,
    createThing_attributePayload,
    createThing_thingName,
    createThingResponse_thingId,
    createThingResponse_thingArn,
    createThingResponse_thingName,
    createThingResponse_httpStatus,

    -- ** ListV2LoggingLevels
    listV2LoggingLevels_nextToken,
    listV2LoggingLevels_targetType,
    listV2LoggingLevels_maxResults,
    listV2LoggingLevelsResponse_nextToken,
    listV2LoggingLevelsResponse_logTargetConfigurations,
    listV2LoggingLevelsResponse_httpStatus,

    -- ** ListAuditMitigationActionsExecutions
    listAuditMitigationActionsExecutions_nextToken,
    listAuditMitigationActionsExecutions_maxResults,
    listAuditMitigationActionsExecutions_actionStatus,
    listAuditMitigationActionsExecutions_taskId,
    listAuditMitigationActionsExecutions_findingId,
    listAuditMitigationActionsExecutionsResponse_nextToken,
    listAuditMitigationActionsExecutionsResponse_actionsExecutions,
    listAuditMitigationActionsExecutionsResponse_httpStatus,

    -- ** ListProvisioningTemplates
    listProvisioningTemplates_nextToken,
    listProvisioningTemplates_maxResults,
    listProvisioningTemplatesResponse_nextToken,
    listProvisioningTemplatesResponse_templates,
    listProvisioningTemplatesResponse_httpStatus,

    -- ** StartThingRegistrationTask
    startThingRegistrationTask_templateBody,
    startThingRegistrationTask_inputFileBucket,
    startThingRegistrationTask_inputFileKey,
    startThingRegistrationTask_roleArn,
    startThingRegistrationTaskResponse_taskId,
    startThingRegistrationTaskResponse_httpStatus,

    -- ** DescribeJobExecution
    describeJobExecution_executionNumber,
    describeJobExecution_jobId,
    describeJobExecution_thingName,
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_certificateId,
    describeCertificateResponse_certificateDescription,
    describeCertificateResponse_httpStatus,

    -- ** DeleteProvisioningTemplate
    deleteProvisioningTemplate_templateName,
    deleteProvisioningTemplateResponse_httpStatus,

    -- ** UpdateProvisioningTemplate
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_templateName,
    updateProvisioningTemplateResponse_httpStatus,

    -- ** GetIndexingConfiguration
    getIndexingConfigurationResponse_thingGroupIndexingConfiguration,
    getIndexingConfigurationResponse_thingIndexingConfiguration,
    getIndexingConfigurationResponse_httpStatus,

    -- ** StopThingRegistrationTask
    stopThingRegistrationTask_taskId,
    stopThingRegistrationTaskResponse_httpStatus,

    -- ** CreateScheduledAudit
    createScheduledAudit_dayOfWeek,
    createScheduledAudit_dayOfMonth,
    createScheduledAudit_tags,
    createScheduledAudit_frequency,
    createScheduledAudit_targetCheckNames,
    createScheduledAudit_scheduledAuditName,
    createScheduledAuditResponse_scheduledAuditArn,
    createScheduledAuditResponse_httpStatus,

    -- ** SetDefaultAuthorizer
    setDefaultAuthorizer_authorizerName,
    setDefaultAuthorizerResponse_authorizerArn,
    setDefaultAuthorizerResponse_authorizerName,
    setDefaultAuthorizerResponse_httpStatus,

    -- ** DeleteV2LoggingLevel
    deleteV2LoggingLevel_targetType,
    deleteV2LoggingLevel_targetName,

    -- ** DescribeMitigationAction
    describeMitigationAction_actionName,
    describeMitigationActionResponse_lastModifiedDate,
    describeMitigationActionResponse_actionName,
    describeMitigationActionResponse_roleArn,
    describeMitigationActionResponse_actionType,
    describeMitigationActionResponse_actionArn,
    describeMitigationActionResponse_actionId,
    describeMitigationActionResponse_creationDate,
    describeMitigationActionResponse_actionParams,
    describeMitigationActionResponse_httpStatus,

    -- ** DescribeDimension
    describeDimension_name,
    describeDimensionResponse_lastModifiedDate,
    describeDimensionResponse_creationDate,
    describeDimensionResponse_arn,
    describeDimensionResponse_name,
    describeDimensionResponse_type,
    describeDimensionResponse_stringValues,
    describeDimensionResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_status,
    createAuthorizer_tokenSigningPublicKeys,
    createAuthorizer_tags,
    createAuthorizer_tokenKeyName,
    createAuthorizer_signingDisabled,
    createAuthorizer_authorizerName,
    createAuthorizer_authorizerFunctionArn,
    createAuthorizerResponse_authorizerArn,
    createAuthorizerResponse_authorizerName,
    createAuthorizerResponse_httpStatus,

    -- ** SetV2LoggingOptions
    setV2LoggingOptions_roleArn,
    setV2LoggingOptions_disableAllLogs,
    setV2LoggingOptions_defaultLogLevel,

    -- ** DeleteStream
    deleteStream_streamId,
    deleteStreamResponse_httpStatus,

    -- ** ListDetectMitigationActionsTasks
    listDetectMitigationActionsTasks_nextToken,
    listDetectMitigationActionsTasks_maxResults,
    listDetectMitigationActionsTasks_startTime,
    listDetectMitigationActionsTasks_endTime,
    listDetectMitigationActionsTasksResponse_nextToken,
    listDetectMitigationActionsTasksResponse_tasks,
    listDetectMitigationActionsTasksResponse_httpStatus,

    -- ** UpdateAuditSuppression
    updateAuditSuppression_expirationDate,
    updateAuditSuppression_description,
    updateAuditSuppression_suppressIndefinitely,
    updateAuditSuppression_checkName,
    updateAuditSuppression_resourceIdentifier,
    updateAuditSuppressionResponse_httpStatus,

    -- ** UpdateStream
    updateStream_roleArn,
    updateStream_description,
    updateStream_files,
    updateStream_streamId,
    updateStreamResponse_streamVersion,
    updateStreamResponse_streamArn,
    updateStreamResponse_streamId,
    updateStreamResponse_description,
    updateStreamResponse_httpStatus,

    -- ** UpdateThing
    updateThing_expectedVersion,
    updateThing_thingTypeName,
    updateThing_attributePayload,
    updateThing_removeThingType,
    updateThing_thingName,
    updateThingResponse_httpStatus,

    -- ** DescribeThingRegistrationTask
    describeThingRegistrationTask_taskId,
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_httpStatus,

    -- ** DeleteAccountAuditConfiguration
    deleteAccountAuditConfiguration_deleteScheduledAudits,
    deleteAccountAuditConfigurationResponse_httpStatus,

    -- ** ListThingGroupsForThing
    listThingGroupsForThing_nextToken,
    listThingGroupsForThing_maxResults,
    listThingGroupsForThing_thingName,
    listThingGroupsForThingResponse_nextToken,
    listThingGroupsForThingResponse_thingGroups,
    listThingGroupsForThingResponse_httpStatus,

    -- ** ListCertificatesByCA
    listCertificatesByCA_pageSize,
    listCertificatesByCA_ascendingOrder,
    listCertificatesByCA_marker,
    listCertificatesByCA_caCertificateId,
    listCertificatesByCAResponse_nextMarker,
    listCertificatesByCAResponse_certificates,
    listCertificatesByCAResponse_httpStatus,

    -- ** ListThings
    listThings_attributeValue,
    listThings_nextToken,
    listThings_maxResults,
    listThings_attributeName,
    listThings_usePrefixAttributeValue,
    listThings_thingTypeName,
    listThingsResponse_nextToken,
    listThingsResponse_things,
    listThingsResponse_httpStatus,

    -- ** AddThingToThingGroup
    addThingToThingGroup_thingArn,
    addThingToThingGroup_thingGroupArn,
    addThingToThingGroup_thingName,
    addThingToThingGroup_thingGroupName,
    addThingToThingGroup_overrideDynamicGroups,
    addThingToThingGroupResponse_httpStatus,

    -- ** DeleteBillingGroup
    deleteBillingGroup_expectedVersion,
    deleteBillingGroup_billingGroupName,
    deleteBillingGroupResponse_httpStatus,

    -- ** ListScheduledAudits
    listScheduledAudits_nextToken,
    listScheduledAudits_maxResults,
    listScheduledAuditsResponse_nextToken,
    listScheduledAuditsResponse_scheduledAudits,
    listScheduledAuditsResponse_httpStatus,

    -- ** DeleteRegistrationCode
    deleteRegistrationCodeResponse_httpStatus,

    -- ** DeleteThing
    deleteThing_expectedVersion,
    deleteThing_thingName,
    deleteThingResponse_httpStatus,

    -- ** AttachThingPrincipal
    attachThingPrincipal_thingName,
    attachThingPrincipal_principal,
    attachThingPrincipalResponse_httpStatus,

    -- ** UpdateBillingGroup
    updateBillingGroup_expectedVersion,
    updateBillingGroup_billingGroupName,
    updateBillingGroup_billingGroupProperties,
    updateBillingGroupResponse_version,
    updateBillingGroupResponse_httpStatus,

    -- ** UpdateAccountAuditConfiguration
    updateAccountAuditConfiguration_roleArn,
    updateAccountAuditConfiguration_auditCheckConfigurations,
    updateAccountAuditConfiguration_auditNotificationTargetConfigurations,
    updateAccountAuditConfigurationResponse_httpStatus,

    -- ** GetLoggingOptions
    getLoggingOptionsResponse_roleArn,
    getLoggingOptionsResponse_logLevel,
    getLoggingOptionsResponse_httpStatus,

    -- ** DeleteAuditSuppression
    deleteAuditSuppression_checkName,
    deleteAuditSuppression_resourceIdentifier,
    deleteAuditSuppressionResponse_httpStatus,

    -- ** DescribeCustomMetric
    describeCustomMetric_metricName,
    describeCustomMetricResponse_lastModifiedDate,
    describeCustomMetricResponse_metricType,
    describeCustomMetricResponse_metricArn,
    describeCustomMetricResponse_metricName,
    describeCustomMetricResponse_creationDate,
    describeCustomMetricResponse_displayName,
    describeCustomMetricResponse_httpStatus,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_job,
    describeJobResponse_documentSource,
    describeJobResponse_httpStatus,

    -- ** DeleteOTAUpdate
    deleteOTAUpdate_forceDeleteAWSJob,
    deleteOTAUpdate_deleteStream,
    deleteOTAUpdate_otaUpdateId,
    deleteOTAUpdateResponse_httpStatus,

    -- ** GetRegistrationCode
    getRegistrationCodeResponse_registrationCode,
    getRegistrationCodeResponse_httpStatus,

    -- ** ListDetectMitigationActionsExecutions
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_violationId,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_startTime,
    listDetectMitigationActionsExecutions_endTime,
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_httpStatus,

    -- ** RegisterCertificateWithoutCA
    registerCertificateWithoutCA_status,
    registerCertificateWithoutCA_certificatePem,
    registerCertificateWithoutCAResponse_certificateArn,
    registerCertificateWithoutCAResponse_certificateId,
    registerCertificateWithoutCAResponse_httpStatus,

    -- ** CreateDynamicThingGroup
    createDynamicThingGroup_indexName,
    createDynamicThingGroup_queryVersion,
    createDynamicThingGroup_tags,
    createDynamicThingGroup_thingGroupProperties,
    createDynamicThingGroup_thingGroupName,
    createDynamicThingGroup_queryString,
    createDynamicThingGroupResponse_queryString,
    createDynamicThingGroupResponse_indexName,
    createDynamicThingGroupResponse_thingGroupArn,
    createDynamicThingGroupResponse_queryVersion,
    createDynamicThingGroupResponse_thingGroupName,
    createDynamicThingGroupResponse_thingGroupId,
    createDynamicThingGroupResponse_httpStatus,

    -- ** DescribeCACertificate
    describeCACertificate_certificateId,
    describeCACertificateResponse_certificateDescription,
    describeCACertificateResponse_registrationConfig,
    describeCACertificateResponse_httpStatus,

    -- ** DeleteProvisioningTemplateVersion
    deleteProvisioningTemplateVersion_templateName,
    deleteProvisioningTemplateVersion_versionId,
    deleteProvisioningTemplateVersionResponse_httpStatus,

    -- ** DetachSecurityProfile
    detachSecurityProfile_securityProfileName,
    detachSecurityProfile_securityProfileTargetArn,
    detachSecurityProfileResponse_httpStatus,

    -- ** ListThingPrincipals
    listThingPrincipals_nextToken,
    listThingPrincipals_maxResults,
    listThingPrincipals_thingName,
    listThingPrincipalsResponse_nextToken,
    listThingPrincipalsResponse_principals,
    listThingPrincipalsResponse_httpStatus,

    -- ** RemoveThingFromThingGroup
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingName,
    removeThingFromThingGroup_thingGroupName,
    removeThingFromThingGroupResponse_httpStatus,

    -- ** GetBehaviorModelTrainingSummaries
    getBehaviorModelTrainingSummaries_nextToken,
    getBehaviorModelTrainingSummaries_maxResults,
    getBehaviorModelTrainingSummaries_securityProfileName,
    getBehaviorModelTrainingSummariesResponse_nextToken,
    getBehaviorModelTrainingSummariesResponse_summaries,
    getBehaviorModelTrainingSummariesResponse_httpStatus,

    -- ** UpdateDynamicThingGroup
    updateDynamicThingGroup_queryString,
    updateDynamicThingGroup_expectedVersion,
    updateDynamicThingGroup_indexName,
    updateDynamicThingGroup_queryVersion,
    updateDynamicThingGroup_thingGroupName,
    updateDynamicThingGroup_thingGroupProperties,
    updateDynamicThingGroupResponse_version,
    updateDynamicThingGroupResponse_httpStatus,

    -- ** CreateTopicRuleDestination
    createTopicRuleDestination_destinationConfiguration,
    createTopicRuleDestinationResponse_topicRuleDestination,
    createTopicRuleDestinationResponse_httpStatus,

    -- ** ListPrincipalThings
    listPrincipalThings_nextToken,
    listPrincipalThings_maxResults,
    listPrincipalThings_principal,
    listPrincipalThingsResponse_nextToken,
    listPrincipalThingsResponse_things,
    listPrincipalThingsResponse_httpStatus,

    -- ** CreateProvisioningTemplateVersion
    createProvisioningTemplateVersion_setAsDefault,
    createProvisioningTemplateVersion_templateName,
    createProvisioningTemplateVersion_templateBody,
    createProvisioningTemplateVersionResponse_templateName,
    createProvisioningTemplateVersionResponse_versionId,
    createProvisioningTemplateVersionResponse_isDefaultVersion,
    createProvisioningTemplateVersionResponse_templateArn,
    createProvisioningTemplateVersionResponse_httpStatus,

    -- ** DescribeRoleAlias
    describeRoleAlias_roleAlias,
    describeRoleAliasResponse_roleAliasDescription,
    describeRoleAliasResponse_httpStatus,

    -- ** DeleteDynamicThingGroup
    deleteDynamicThingGroup_expectedVersion,
    deleteDynamicThingGroup_thingGroupName,
    deleteDynamicThingGroupResponse_httpStatus,

    -- ** CreateThingGroup
    createThingGroup_parentGroupName,
    createThingGroup_tags,
    createThingGroup_thingGroupProperties,
    createThingGroup_thingGroupName,
    createThingGroupResponse_thingGroupArn,
    createThingGroupResponse_thingGroupName,
    createThingGroupResponse_thingGroupId,
    createThingGroupResponse_httpStatus,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_httpContext,
    testInvokeAuthorizer_mqttContext,
    testInvokeAuthorizer_tokenSignature,
    testInvokeAuthorizer_tlsContext,
    testInvokeAuthorizer_token,
    testInvokeAuthorizer_authorizerName,
    testInvokeAuthorizerResponse_disconnectAfterInSeconds,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_policyDocuments,
    testInvokeAuthorizerResponse_isAuthenticated,
    testInvokeAuthorizerResponse_refreshAfterInSeconds,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** CreateOTAUpdate
    createOTAUpdate_targetSelection,
    createOTAUpdate_awsJobTimeoutConfig,
    createOTAUpdate_protocols,
    createOTAUpdate_tags,
    createOTAUpdate_awsJobPresignedUrlConfig,
    createOTAUpdate_description,
    createOTAUpdate_additionalParameters,
    createOTAUpdate_awsJobExecutionsRolloutConfig,
    createOTAUpdate_awsJobAbortConfig,
    createOTAUpdate_otaUpdateId,
    createOTAUpdate_targets,
    createOTAUpdate_files,
    createOTAUpdate_roleArn,
    createOTAUpdateResponse_otaUpdateStatus,
    createOTAUpdateResponse_otaUpdateArn,
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_httpStatus,

    -- ** DescribeDefaultAuthorizer
    describeDefaultAuthorizerResponse_authorizerDescription,
    describeDefaultAuthorizerResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyName,
    detachPolicy_target,

    -- ** ListAuditMitigationActionsTasks
    listAuditMitigationActionsTasks_nextToken,
    listAuditMitigationActionsTasks_maxResults,
    listAuditMitigationActionsTasks_findingId,
    listAuditMitigationActionsTasks_auditTaskId,
    listAuditMitigationActionsTasks_taskStatus,
    listAuditMitigationActionsTasks_startTime,
    listAuditMitigationActionsTasks_endTime,
    listAuditMitigationActionsTasksResponse_nextToken,
    listAuditMitigationActionsTasksResponse_tasks,
    listAuditMitigationActionsTasksResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_caCertificatePem,
    registerCertificate_setAsActive,
    registerCertificate_status,
    registerCertificate_certificatePem,
    registerCertificateResponse_certificateArn,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** UpdateSecurityProfile
    updateSecurityProfile_expectedVersion,
    updateSecurityProfile_alertTargets,
    updateSecurityProfile_deleteAdditionalMetricsToRetain,
    updateSecurityProfile_additionalMetricsToRetain,
    updateSecurityProfile_deleteAlertTargets,
    updateSecurityProfile_behaviors,
    updateSecurityProfile_additionalMetricsToRetainV2,
    updateSecurityProfile_deleteBehaviors,
    updateSecurityProfile_securityProfileDescription,
    updateSecurityProfile_securityProfileName,
    updateSecurityProfileResponse_lastModifiedDate,
    updateSecurityProfileResponse_alertTargets,
    updateSecurityProfileResponse_additionalMetricsToRetain,
    updateSecurityProfileResponse_creationDate,
    updateSecurityProfileResponse_securityProfileName,
    updateSecurityProfileResponse_version,
    updateSecurityProfileResponse_behaviors,
    updateSecurityProfileResponse_additionalMetricsToRetainV2,
    updateSecurityProfileResponse_securityProfileDescription,
    updateSecurityProfileResponse_securityProfileArn,
    updateSecurityProfileResponse_httpStatus,

    -- ** DescribeAuthorizer
    describeAuthorizer_authorizerName,
    describeAuthorizerResponse_authorizerDescription,
    describeAuthorizerResponse_httpStatus,

    -- ** GetPolicyVersion
    getPolicyVersion_policyName,
    getPolicyVersion_policyVersionId,
    getPolicyVersionResponse_lastModifiedDate,
    getPolicyVersionResponse_policyDocument,
    getPolicyVersionResponse_policyVersionId,
    getPolicyVersionResponse_policyName,
    getPolicyVersionResponse_creationDate,
    getPolicyVersionResponse_generationId,
    getPolicyVersionResponse_isDefaultVersion,
    getPolicyVersionResponse_policyArn,
    getPolicyVersionResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_pageSize,
    listCertificates_ascendingOrder,
    listCertificates_marker,
    listCertificatesResponse_nextMarker,
    listCertificatesResponse_certificates,
    listCertificatesResponse_httpStatus,

    -- ** DeleteSecurityProfile
    deleteSecurityProfile_expectedVersion,
    deleteSecurityProfile_securityProfileName,
    deleteSecurityProfileResponse_httpStatus,

    -- ** ValidateSecurityProfileBehaviors
    validateSecurityProfileBehaviors_behaviors,
    validateSecurityProfileBehaviorsResponse_validationErrors,
    validateSecurityProfileBehaviorsResponse_valid,
    validateSecurityProfileBehaviorsResponse_httpStatus,

    -- ** CreateDomainConfiguration
    createDomainConfiguration_serverCertificateArns,
    createDomainConfiguration_authorizerConfig,
    createDomainConfiguration_domainName,
    createDomainConfiguration_tags,
    createDomainConfiguration_validationCertificateArn,
    createDomainConfiguration_serviceType,
    createDomainConfiguration_domainConfigurationName,
    createDomainConfigurationResponse_domainConfigurationArn,
    createDomainConfigurationResponse_domainConfigurationName,
    createDomainConfigurationResponse_httpStatus,

    -- ** ListActiveViolations
    listActiveViolations_nextToken,
    listActiveViolations_maxResults,
    listActiveViolations_thingName,
    listActiveViolations_securityProfileName,
    listActiveViolations_listSuppressedAlerts,
    listActiveViolations_behaviorCriteriaType,
    listActiveViolations_verificationState,
    listActiveViolationsResponse_nextToken,
    listActiveViolationsResponse_activeViolations,
    listActiveViolationsResponse_httpStatus,

    -- ** DescribeBillingGroup
    describeBillingGroup_billingGroupName,
    describeBillingGroupResponse_billingGroupProperties,
    describeBillingGroupResponse_version,
    describeBillingGroupResponse_billingGroupId,
    describeBillingGroupResponse_billingGroupMetadata,
    describeBillingGroupResponse_billingGroupArn,
    describeBillingGroupResponse_billingGroupName,
    describeBillingGroupResponse_httpStatus,

    -- ** ListThingRegistrationTasks
    listThingRegistrationTasks_nextToken,
    listThingRegistrationTasks_status,
    listThingRegistrationTasks_maxResults,
    listThingRegistrationTasksResponse_nextToken,
    listThingRegistrationTasksResponse_taskIds,
    listThingRegistrationTasksResponse_httpStatus,

    -- ** UpdateDimension
    updateDimension_name,
    updateDimension_stringValues,
    updateDimensionResponse_lastModifiedDate,
    updateDimensionResponse_creationDate,
    updateDimensionResponse_arn,
    updateDimensionResponse_name,
    updateDimensionResponse_type,
    updateDimensionResponse_stringValues,
    updateDimensionResponse_httpStatus,

    -- ** DescribeAuditSuppression
    describeAuditSuppression_checkName,
    describeAuditSuppression_resourceIdentifier,
    describeAuditSuppressionResponse_expirationDate,
    describeAuditSuppressionResponse_resourceIdentifier,
    describeAuditSuppressionResponse_checkName,
    describeAuditSuppressionResponse_description,
    describeAuditSuppressionResponse_suppressIndefinitely,
    describeAuditSuppressionResponse_httpStatus,

    -- ** DescribeAccountAuditConfiguration
    describeAccountAuditConfigurationResponse_roleArn,
    describeAccountAuditConfigurationResponse_auditCheckConfigurations,
    describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations,
    describeAccountAuditConfigurationResponse_httpStatus,

    -- ** DeprecateThingType
    deprecateThingType_undoDeprecate,
    deprecateThingType_thingTypeName,
    deprecateThingTypeResponse_httpStatus,

    -- ** DescribeDetectMitigationActionsTask
    describeDetectMitigationActionsTask_taskId,
    describeDetectMitigationActionsTaskResponse_taskSummary,
    describeDetectMitigationActionsTaskResponse_httpStatus,

    -- ** DeleteDimension
    deleteDimension_name,
    deleteDimensionResponse_httpStatus,

    -- ** ListAuditFindings
    listAuditFindings_nextToken,
    listAuditFindings_maxResults,
    listAuditFindings_taskId,
    listAuditFindings_startTime,
    listAuditFindings_endTime,
    listAuditFindings_listSuppressedFindings,
    listAuditFindings_resourceIdentifier,
    listAuditFindings_checkName,
    listAuditFindingsResponse_nextToken,
    listAuditFindingsResponse_findings,
    listAuditFindingsResponse_httpStatus,

    -- ** DescribeThing
    describeThing_thingName,
    describeThingResponse_thingId,
    describeThingResponse_thingArn,
    describeThingResponse_thingName,
    describeThingResponse_version,
    describeThingResponse_defaultClientId,
    describeThingResponse_attributes,
    describeThingResponse_thingTypeName,
    describeThingResponse_billingGroupName,
    describeThingResponse_httpStatus,

    -- ** ListDimensions
    listDimensions_nextToken,
    listDimensions_maxResults,
    listDimensionsResponse_nextToken,
    listDimensionsResponse_dimensionNames,
    listDimensionsResponse_httpStatus,

    -- ** DetachThingPrincipal
    detachThingPrincipal_thingName,
    detachThingPrincipal_principal,
    detachThingPrincipalResponse_httpStatus,

    -- ** DescribeStream
    describeStream_streamId,
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,

    -- ** ConfirmTopicRuleDestination
    confirmTopicRuleDestination_confirmationToken,
    confirmTopicRuleDestinationResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyName,
    getPolicyResponse_lastModifiedDate,
    getPolicyResponse_policyDocument,
    getPolicyResponse_policyName,
    getPolicyResponse_creationDate,
    getPolicyResponse_defaultVersionId,
    getPolicyResponse_generationId,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,

    -- ** ListTopicRules
    listTopicRules_nextToken,
    listTopicRules_maxResults,
    listTopicRules_topic,
    listTopicRules_ruleDisabled,
    listTopicRulesResponse_nextToken,
    listTopicRulesResponse_rules,
    listTopicRulesResponse_httpStatus,

    -- ** ListCACertificates
    listCACertificates_pageSize,
    listCACertificates_ascendingOrder,
    listCACertificates_marker,
    listCACertificatesResponse_nextMarker,
    listCACertificatesResponse_certificates,
    listCACertificatesResponse_httpStatus,

    -- ** StartOnDemandAuditTask
    startOnDemandAuditTask_targetCheckNames,
    startOnDemandAuditTaskResponse_taskId,
    startOnDemandAuditTaskResponse_httpStatus,

    -- ** UpdateEventConfigurations
    updateEventConfigurations_eventConfigurations,
    updateEventConfigurationsResponse_httpStatus,

    -- ** UpdateThingGroupsForThing
    updateThingGroupsForThing_thingName,
    updateThingGroupsForThing_overrideDynamicGroups,
    updateThingGroupsForThing_thingGroupsToRemove,
    updateThingGroupsForThing_thingGroupsToAdd,
    updateThingGroupsForThingResponse_httpStatus,

    -- ** ListSecurityProfilesForTarget
    listSecurityProfilesForTarget_nextToken,
    listSecurityProfilesForTarget_maxResults,
    listSecurityProfilesForTarget_recursive,
    listSecurityProfilesForTarget_securityProfileTargetArn,
    listSecurityProfilesForTargetResponse_nextToken,
    listSecurityProfilesForTargetResponse_securityProfileTargetMappings,
    listSecurityProfilesForTargetResponse_httpStatus,

    -- ** DeleteJobTemplate
    deleteJobTemplate_jobTemplateId,

    -- ** EnableTopicRule
    enableTopicRule_ruleName,

    -- ** AcceptCertificateTransfer
    acceptCertificateTransfer_setAsActive,
    acceptCertificateTransfer_certificateId,

    -- ** GetJobDocument
    getJobDocument_jobId,
    getJobDocumentResponse_document,
    getJobDocumentResponse_httpStatus,

    -- ** ListAttachedPolicies
    listAttachedPolicies_pageSize,
    listAttachedPolicies_recursive,
    listAttachedPolicies_marker,
    listAttachedPolicies_target,
    listAttachedPoliciesResponse_policies,
    listAttachedPoliciesResponse_nextMarker,
    listAttachedPoliciesResponse_httpStatus,

    -- ** DescribeThingGroup
    describeThingGroup_thingGroupName,
    describeThingGroupResponse_queryString,
    describeThingGroupResponse_status,
    describeThingGroupResponse_indexName,
    describeThingGroupResponse_thingGroupArn,
    describeThingGroupResponse_queryVersion,
    describeThingGroupResponse_version,
    describeThingGroupResponse_thingGroupName,
    describeThingGroupResponse_thingGroupId,
    describeThingGroupResponse_thingGroupMetadata,
    describeThingGroupResponse_thingGroupProperties,
    describeThingGroupResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_status,
    listJobs_targetSelection,
    listJobs_namespaceId,
    listJobs_maxResults,
    listJobs_thingGroupName,
    listJobs_thingGroupId,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** CreateFleetMetric
    createFleetMetric_indexName,
    createFleetMetric_unit,
    createFleetMetric_queryVersion,
    createFleetMetric_tags,
    createFleetMetric_description,
    createFleetMetric_metricName,
    createFleetMetric_queryString,
    createFleetMetric_aggregationType,
    createFleetMetric_period,
    createFleetMetric_aggregationField,
    createFleetMetricResponse_metricArn,
    createFleetMetricResponse_metricName,
    createFleetMetricResponse_httpStatus,

    -- ** RegisterCACertificate
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_setAsActive,
    registerCACertificate_tags,
    registerCACertificate_registrationConfig,
    registerCACertificate_caCertificate,
    registerCACertificate_verificationCertificate,
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,

    -- ** ReplaceTopicRule
    replaceTopicRule_ruleName,
    replaceTopicRule_topicRulePayload,

    -- ** GetStatistics
    getStatistics_indexName,
    getStatistics_queryVersion,
    getStatistics_aggregationField,
    getStatistics_queryString,
    getStatisticsResponse_statistics,
    getStatisticsResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_indexName,
    describeIndexResponse_schema,
    describeIndexResponse_indexName,
    describeIndexResponse_indexStatus,
    describeIndexResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_policyName,
    attachPolicy_target,

    -- ** UpdateRoleAlias
    updateRoleAlias_roleArn,
    updateRoleAlias_credentialDurationSeconds,
    updateRoleAlias_roleAlias,
    updateRoleAliasResponse_roleAliasArn,
    updateRoleAliasResponse_roleAlias,
    updateRoleAliasResponse_httpStatus,

    -- ** ClearDefaultAuthorizer
    clearDefaultAuthorizerResponse_httpStatus,

    -- ** CreateTopicRule
    createTopicRule_tags,
    createTopicRule_ruleName,
    createTopicRule_topicRulePayload,

    -- ** CancelJobExecution
    cancelJobExecution_expectedVersion,
    cancelJobExecution_statusDetails,
    cancelJobExecution_force,
    cancelJobExecution_jobId,
    cancelJobExecution_thingName,

    -- ** CreateJob
    createJob_jobExecutionsRolloutConfig,
    createJob_timeoutConfig,
    createJob_targetSelection,
    createJob_namespaceId,
    createJob_documentSource,
    createJob_document,
    createJob_presignedUrlConfig,
    createJob_tags,
    createJob_description,
    createJob_abortConfig,
    createJob_jobTemplateArn,
    createJob_jobId,
    createJob_targets,
    createJobResponse_jobArn,
    createJobResponse_description,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- ** AssociateTargetsWithJob
    associateTargetsWithJob_namespaceId,
    associateTargetsWithJob_comment,
    associateTargetsWithJob_targets,
    associateTargetsWithJob_jobId,
    associateTargetsWithJobResponse_jobArn,
    associateTargetsWithJobResponse_description,
    associateTargetsWithJobResponse_jobId,
    associateTargetsWithJobResponse_httpStatus,

    -- ** DeletePolicyVersion
    deletePolicyVersion_policyName,
    deletePolicyVersion_policyVersionId,

    -- ** DeleteRoleAlias
    deleteRoleAlias_roleAlias,
    deleteRoleAliasResponse_httpStatus,

    -- ** ListPolicyVersions
    listPolicyVersions_policyName,
    listPolicyVersionsResponse_policyVersions,
    listPolicyVersionsResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_pageSize,
    listTargetsForPolicy_marker,
    listTargetsForPolicy_policyName,
    listTargetsForPolicyResponse_nextMarker,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,

    -- ** CancelCertificateTransfer
    cancelCertificateTransfer_certificateId,

    -- ** ListAuthorizers
    listAuthorizers_status,
    listAuthorizers_pageSize,
    listAuthorizers_ascendingOrder,
    listAuthorizers_marker,
    listAuthorizersResponse_nextMarker,
    listAuthorizersResponse_authorizers,
    listAuthorizersResponse_httpStatus,

    -- ** CreateThingType
    createThingType_thingTypeProperties,
    createThingType_tags,
    createThingType_thingTypeName,
    createThingTypeResponse_thingTypeId,
    createThingTypeResponse_thingTypeArn,
    createThingTypeResponse_thingTypeName,
    createThingTypeResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_status,
    updateAuthorizer_authorizerFunctionArn,
    updateAuthorizer_tokenSigningPublicKeys,
    updateAuthorizer_tokenKeyName,
    updateAuthorizer_authorizerName,
    updateAuthorizerResponse_authorizerArn,
    updateAuthorizerResponse_authorizerName,
    updateAuthorizerResponse_httpStatus,

    -- ** CreateAuditSuppression
    createAuditSuppression_expirationDate,
    createAuditSuppression_description,
    createAuditSuppression_suppressIndefinitely,
    createAuditSuppression_checkName,
    createAuditSuppression_resourceIdentifier,
    createAuditSuppression_clientRequestToken,
    createAuditSuppressionResponse_httpStatus,

    -- ** ListJobExecutionsForJob
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_jobId,
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_httpStatus,

    -- ** DescribeSecurityProfile
    describeSecurityProfile_securityProfileName,
    describeSecurityProfileResponse_lastModifiedDate,
    describeSecurityProfileResponse_alertTargets,
    describeSecurityProfileResponse_additionalMetricsToRetain,
    describeSecurityProfileResponse_creationDate,
    describeSecurityProfileResponse_securityProfileName,
    describeSecurityProfileResponse_version,
    describeSecurityProfileResponse_behaviors,
    describeSecurityProfileResponse_additionalMetricsToRetainV2,
    describeSecurityProfileResponse_securityProfileDescription,
    describeSecurityProfileResponse_securityProfileArn,
    describeSecurityProfileResponse_httpStatus,

    -- ** RemoveThingFromBillingGroup
    removeThingFromBillingGroup_thingArn,
    removeThingFromBillingGroup_thingName,
    removeThingFromBillingGroup_billingGroupArn,
    removeThingFromBillingGroup_billingGroupName,
    removeThingFromBillingGroupResponse_httpStatus,

    -- ** CreateStream
    createStream_tags,
    createStream_description,
    createStream_streamId,
    createStream_files,
    createStream_roleArn,
    createStreamResponse_streamVersion,
    createStreamResponse_streamArn,
    createStreamResponse_streamId,
    createStreamResponse_description,
    createStreamResponse_httpStatus,

    -- ** SearchIndex
    searchIndex_nextToken,
    searchIndex_indexName,
    searchIndex_maxResults,
    searchIndex_queryVersion,
    searchIndex_queryString,
    searchIndexResponse_nextToken,
    searchIndexResponse_things,
    searchIndexResponse_thingGroups,
    searchIndexResponse_httpStatus,

    -- ** CancelAuditMitigationActionsTask
    cancelAuditMitigationActionsTask_taskId,
    cancelAuditMitigationActionsTaskResponse_httpStatus,

    -- ** DeleteAuthorizer
    deleteAuthorizer_authorizerName,
    deleteAuthorizerResponse_httpStatus,

    -- ** CreateBillingGroup
    createBillingGroup_billingGroupProperties,
    createBillingGroup_tags,
    createBillingGroup_billingGroupName,
    createBillingGroupResponse_billingGroupId,
    createBillingGroupResponse_billingGroupArn,
    createBillingGroupResponse_billingGroupName,
    createBillingGroupResponse_httpStatus,

    -- ** DescribeAuditFinding
    describeAuditFinding_findingId,
    describeAuditFindingResponse_finding,
    describeAuditFindingResponse_httpStatus,

    -- ** DeleteScheduledAudit
    deleteScheduledAudit_scheduledAuditName,
    deleteScheduledAuditResponse_httpStatus,

    -- ** GetEffectivePolicies
    getEffectivePolicies_thingName,
    getEffectivePolicies_cognitoIdentityPoolId,
    getEffectivePolicies_principal,
    getEffectivePoliciesResponse_effectivePolicies,
    getEffectivePoliciesResponse_httpStatus,

    -- ** GetOTAUpdate
    getOTAUpdate_otaUpdateId,
    getOTAUpdateResponse_otaUpdateInfo,
    getOTAUpdateResponse_httpStatus,

    -- ** CreateProvisioningTemplate
    createProvisioningTemplate_preProvisioningHook,
    createProvisioningTemplate_enabled,
    createProvisioningTemplate_tags,
    createProvisioningTemplate_description,
    createProvisioningTemplate_templateName,
    createProvisioningTemplate_templateBody,
    createProvisioningTemplate_provisioningRoleArn,
    createProvisioningTemplateResponse_templateName,
    createProvisioningTemplateResponse_defaultVersionId,
    createProvisioningTemplateResponse_templateArn,
    createProvisioningTemplateResponse_httpStatus,

    -- ** ListThingTypes
    listThingTypes_nextToken,
    listThingTypes_maxResults,
    listThingTypes_thingTypeName,
    listThingTypesResponse_thingTypes,
    listThingTypesResponse_nextToken,
    listThingTypesResponse_httpStatus,

    -- ** DeleteThingType
    deleteThingType_thingTypeName,
    deleteThingTypeResponse_httpStatus,

    -- ** RegisterThing
    registerThing_parameters,
    registerThing_templateBody,
    registerThingResponse_resourceArns,
    registerThingResponse_certificatePem,
    registerThingResponse_httpStatus,

    -- ** ListBillingGroups
    listBillingGroups_namePrefixFilter,
    listBillingGroups_nextToken,
    listBillingGroups_maxResults,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_httpStatus,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_maxResults,
    listStreams_ascendingOrder,
    listStreamsResponse_streams,
    listStreamsResponse_nextToken,
    listStreamsResponse_httpStatus,

    -- ** TestAuthorization
    testAuthorization_clientId,
    testAuthorization_cognitoIdentityPoolId,
    testAuthorization_principal,
    testAuthorization_policyNamesToSkip,
    testAuthorization_policyNamesToAdd,
    testAuthorization_authInfos,
    testAuthorizationResponse_authResults,
    testAuthorizationResponse_httpStatus,

    -- ** ListIndices
    listIndices_nextToken,
    listIndices_maxResults,
    listIndicesResponse_nextToken,
    listIndicesResponse_indexNames,
    listIndicesResponse_httpStatus,

    -- ** DescribeAuditTask
    describeAuditTask_taskId,
    describeAuditTaskResponse_auditDetails,
    describeAuditTaskResponse_scheduledAuditName,
    describeAuditTaskResponse_taskStatistics,
    describeAuditTaskResponse_taskStatus,
    describeAuditTaskResponse_taskStartTime,
    describeAuditTaskResponse_taskType,
    describeAuditTaskResponse_httpStatus,

    -- ** CreateCertificateFromCsr
    createCertificateFromCsr_setAsActive,
    createCertificateFromCsr_certificateSigningRequest,
    createCertificateFromCsrResponse_certificateArn,
    createCertificateFromCsrResponse_certificateId,
    createCertificateFromCsrResponse_certificatePem,
    createCertificateFromCsrResponse_httpStatus,

    -- ** ListAuditSuppressions
    listAuditSuppressions_nextToken,
    listAuditSuppressions_maxResults,
    listAuditSuppressions_resourceIdentifier,
    listAuditSuppressions_checkName,
    listAuditSuppressions_ascendingOrder,
    listAuditSuppressionsResponse_nextToken,
    listAuditSuppressionsResponse_suppressions,
    listAuditSuppressionsResponse_httpStatus,

    -- ** DescribeDomainConfiguration
    describeDomainConfiguration_domainConfigurationName,
    describeDomainConfigurationResponse_domainConfigurationStatus,
    describeDomainConfigurationResponse_authorizerConfig,
    describeDomainConfigurationResponse_serverCertificates,
    describeDomainConfigurationResponse_domainConfigurationArn,
    describeDomainConfigurationResponse_domainName,
    describeDomainConfigurationResponse_domainConfigurationName,
    describeDomainConfigurationResponse_lastStatusChangeDate,
    describeDomainConfigurationResponse_domainType,
    describeDomainConfigurationResponse_serviceType,
    describeDomainConfigurationResponse_httpStatus,

    -- ** UpdateScheduledAudit
    updateScheduledAudit_dayOfWeek,
    updateScheduledAudit_dayOfMonth,
    updateScheduledAudit_frequency,
    updateScheduledAudit_targetCheckNames,
    updateScheduledAudit_scheduledAuditName,
    updateScheduledAuditResponse_scheduledAuditArn,
    updateScheduledAuditResponse_httpStatus,

    -- ** GetTopicRuleDestination
    getTopicRuleDestination_arn,
    getTopicRuleDestinationResponse_topicRuleDestination,
    getTopicRuleDestinationResponse_httpStatus,

    -- ** DeleteTopicRuleDestination
    deleteTopicRuleDestination_arn,
    deleteTopicRuleDestinationResponse_httpStatus,

    -- ** ListOutgoingCertificates
    listOutgoingCertificates_pageSize,
    listOutgoingCertificates_ascendingOrder,
    listOutgoingCertificates_marker,
    listOutgoingCertificatesResponse_nextMarker,
    listOutgoingCertificatesResponse_outgoingCertificates,
    listOutgoingCertificatesResponse_httpStatus,

    -- ** DescribeJobTemplate
    describeJobTemplate_jobTemplateId,
    describeJobTemplateResponse_jobExecutionsRolloutConfig,
    describeJobTemplateResponse_timeoutConfig,
    describeJobTemplateResponse_createdAt,
    describeJobTemplateResponse_jobTemplateId,
    describeJobTemplateResponse_documentSource,
    describeJobTemplateResponse_document,
    describeJobTemplateResponse_presignedUrlConfig,
    describeJobTemplateResponse_description,
    describeJobTemplateResponse_abortConfig,
    describeJobTemplateResponse_jobTemplateArn,
    describeJobTemplateResponse_httpStatus,

    -- ** AddThingToBillingGroup
    addThingToBillingGroup_thingArn,
    addThingToBillingGroup_thingName,
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_billingGroupName,
    addThingToBillingGroupResponse_httpStatus,

    -- ** ListOTAUpdates
    listOTAUpdates_otaUpdateStatus,
    listOTAUpdates_nextToken,
    listOTAUpdates_maxResults,
    listOTAUpdatesResponse_nextToken,
    listOTAUpdatesResponse_otaUpdates,
    listOTAUpdatesResponse_httpStatus,

    -- ** UpdateThingGroup
    updateThingGroup_expectedVersion,
    updateThingGroup_thingGroupName,
    updateThingGroup_thingGroupProperties,
    updateThingGroupResponse_version,
    updateThingGroupResponse_httpStatus,

    -- ** DeleteThingGroup
    deleteThingGroup_expectedVersion,
    deleteThingGroup_thingGroupName,
    deleteThingGroupResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeEventConfigurations
    describeEventConfigurationsResponse_lastModifiedDate,
    describeEventConfigurationsResponse_creationDate,
    describeEventConfigurationsResponse_eventConfigurations,
    describeEventConfigurationsResponse_httpStatus,

    -- ** ListTopicRuleDestinations
    listTopicRuleDestinations_nextToken,
    listTopicRuleDestinations_maxResults,
    listTopicRuleDestinationsResponse_nextToken,
    listTopicRuleDestinationsResponse_destinationSummaries,
    listTopicRuleDestinationsResponse_httpStatus,

    -- ** ListProvisioningTemplateVersions
    listProvisioningTemplateVersions_nextToken,
    listProvisioningTemplateVersions_maxResults,
    listProvisioningTemplateVersions_templateName,
    listProvisioningTemplateVersionsResponse_nextToken,
    listProvisioningTemplateVersionsResponse_versions,
    listProvisioningTemplateVersionsResponse_httpStatus,

    -- ** UpdateTopicRuleDestination
    updateTopicRuleDestination_arn,
    updateTopicRuleDestination_status,
    updateTopicRuleDestinationResponse_httpStatus,

    -- ** ListThingGroups
    listThingGroups_namePrefixFilter,
    listThingGroups_nextToken,
    listThingGroups_maxResults,
    listThingGroups_recursive,
    listThingGroups_parentGroup,
    listThingGroupsResponse_nextToken,
    listThingGroupsResponse_thingGroups,
    listThingGroupsResponse_httpStatus,

    -- ** CancelDetectMitigationActionsTask
    cancelDetectMitigationActionsTask_taskId,
    cancelDetectMitigationActionsTaskResponse_httpStatus,

    -- * Types

    -- ** AbortConfig
    abortConfig_criteriaList,

    -- ** AbortCriteria
    abortCriteria_failureType,
    abortCriteria_action,
    abortCriteria_thresholdPercentage,
    abortCriteria_minNumberOfExecutedThings,

    -- ** Action
    action_cloudwatchLogs,
    action_cloudwatchMetric,
    action_sqs,
    action_firehose,
    action_timestream,
    action_sns,
    action_elasticsearch,
    action_kinesis,
    action_salesforce,
    action_dynamoDBv2,
    action_lambda,
    action_iotAnalytics,
    action_iotSiteWise,
    action_republish,
    action_kafka,
    action_dynamoDB,
    action_stepFunctions,
    action_cloudwatchAlarm,
    action_s3,
    action_http,
    action_iotEvents,
    action_openSearch,

    -- ** ActiveViolation
    activeViolation_violationId,
    activeViolation_lastViolationTime,
    activeViolation_thingName,
    activeViolation_lastViolationValue,
    activeViolation_verificationStateDescription,
    activeViolation_securityProfileName,
    activeViolation_behavior,
    activeViolation_violationStartTime,
    activeViolation_verificationState,
    activeViolation_violationEventAdditionalInfo,

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
    assetPropertyVariant_stringValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_booleanValue,
    assetPropertyVariant_integerValue,

    -- ** AttributePayload
    attributePayload_merge,
    attributePayload_attributes,

    -- ** AuditCheckConfiguration
    auditCheckConfiguration_enabled,

    -- ** AuditCheckDetails
    auditCheckDetails_checkCompliant,
    auditCheckDetails_message,
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_checkRunStatus,
    auditCheckDetails_totalResourcesCount,
    auditCheckDetails_errorCode,
    auditCheckDetails_nonCompliantResourcesCount,

    -- ** AuditFinding
    auditFinding_severity,
    auditFinding_findingId,
    auditFinding_taskId,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_reasonForNonCompliance,
    auditFinding_isSuppressed,
    auditFinding_checkName,
    auditFinding_relatedResources,
    auditFinding_findingTime,
    auditFinding_taskStartTime,
    auditFinding_nonCompliantResource,

    -- ** AuditMitigationActionExecutionMetadata
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_message,
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_taskId,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,

    -- ** AuditMitigationActionsTaskMetadata
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- ** AuditMitigationActionsTaskTarget
    auditMitigationActionsTaskTarget_findingIds,
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,

    -- ** AuditNotificationTarget
    auditNotificationTarget_roleArn,
    auditNotificationTarget_enabled,
    auditNotificationTarget_targetArn,

    -- ** AuditSuppression
    auditSuppression_expirationDate,
    auditSuppression_description,
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
    authResult_authInfo,
    authResult_allowed,
    authResult_denied,
    authResult_missingContextValues,
    authResult_authDecision,

    -- ** AuthorizerConfig
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- ** AuthorizerDescription
    authorizerDescription_lastModifiedDate,
    authorizerDescription_authorizerArn,
    authorizerDescription_status,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_creationDate,
    authorizerDescription_tokenSigningPublicKeys,
    authorizerDescription_authorizerName,
    authorizerDescription_tokenKeyName,
    authorizerDescription_signingDisabled,

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
    behavior_metricDimension,
    behavior_suppressAlerts,
    behavior_metric,
    behavior_criteria,
    behavior_name,

    -- ** BehaviorCriteria
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_consecutiveDatapointsToClear,
    behaviorCriteria_value,
    behaviorCriteria_durationSeconds,

    -- ** BehaviorModelTrainingSummary
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_modelStatus,
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_securityProfileName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,

    -- ** BillingGroupMetadata
    billingGroupMetadata_creationDate,

    -- ** BillingGroupProperties
    billingGroupProperties_billingGroupDescription,

    -- ** Bucket
    bucket_keyValue,
    bucket_count,

    -- ** BucketsAggregationType
    bucketsAggregationType_termsAggregation,

    -- ** CACertificate
    cACertificate_status,
    cACertificate_certificateArn,
    cACertificate_creationDate,
    cACertificate_certificateId,

    -- ** CACertificateDescription
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_status,
    cACertificateDescription_certificateArn,
    cACertificateDescription_creationDate,
    cACertificateDescription_ownedBy,
    cACertificateDescription_customerVersion,
    cACertificateDescription_generationId,
    cACertificateDescription_certificateId,
    cACertificateDescription_certificatePem,
    cACertificateDescription_validity,
    cACertificateDescription_autoRegistrationStatus,

    -- ** Certificate
    certificate_status,
    certificate_certificateMode,
    certificate_certificateArn,
    certificate_creationDate,
    certificate_certificateId,

    -- ** CertificateDescription
    certificateDescription_lastModifiedDate,
    certificateDescription_status,
    certificateDescription_certificateMode,
    certificateDescription_certificateArn,
    certificateDescription_creationDate,
    certificateDescription_previousOwnedBy,
    certificateDescription_ownedBy,
    certificateDescription_customerVersion,
    certificateDescription_generationId,
    certificateDescription_transferData,
    certificateDescription_certificateId,
    certificateDescription_certificatePem,
    certificateDescription_caCertificateId,
    certificateDescription_validity,

    -- ** CertificateValidity
    certificateValidity_notBefore,
    certificateValidity_notAfter,

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
    codeSigning_startSigningJobParameter,
    codeSigning_awsSignerJobId,
    codeSigning_customCodeSigning,

    -- ** CodeSigningCertificateChain
    codeSigningCertificateChain_inlineDocument,
    codeSigningCertificateChain_certificateName,

    -- ** CodeSigningSignature
    codeSigningSignature_inlineDocument,

    -- ** Configuration
    configuration_enabled,

    -- ** CustomCodeSigning
    customCodeSigning_signature,
    customCodeSigning_signatureAlgorithm,
    customCodeSigning_certificateChain,
    customCodeSigning_hashAlgorithm,

    -- ** Denied
    denied_implicitDeny,
    denied_explicitDeny,

    -- ** Destination
    destination_s3Destination,

    -- ** DetectMitigationActionExecution
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_violationId,
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_executionEndDate,
    detectMitigationActionExecution_errorCode,

    -- ** DetectMitigationActionsTaskStatistics
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,
    detectMitigationActionsTaskStatistics_actionsExecuted,

    -- ** DetectMitigationActionsTaskSummary
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskStatus,
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,

    -- ** DetectMitigationActionsTaskTarget
    detectMitigationActionsTaskTarget_violationIds,
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,

    -- ** DomainConfigurationSummary
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_serviceType,

    -- ** DynamoDBAction
    dynamoDBAction_rangeKeyValue,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_payloadField,
    dynamoDBAction_tableName,
    dynamoDBAction_roleArn,
    dynamoDBAction_hashKeyField,
    dynamoDBAction_hashKeyValue,

    -- ** DynamoDBv2Action
    dynamoDBv2Action_roleArn,
    dynamoDBv2Action_putItem,

    -- ** EffectivePolicy
    effectivePolicy_policyDocument,
    effectivePolicy_policyName,
    effectivePolicy_policyArn,

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
    errorInfo_message,
    errorInfo_code,

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
    fileLocation_stream,
    fileLocation_s3Location,

    -- ** FirehoseAction
    firehoseAction_separator,
    firehoseAction_batchMode,
    firehoseAction_roleArn,
    firehoseAction_deliveryStreamName,

    -- ** FleetMetricNameAndArn
    fleetMetricNameAndArn_metricArn,
    fleetMetricNameAndArn_metricName,

    -- ** GroupNameAndArn
    groupNameAndArn_groupName,
    groupNameAndArn_groupArn,

    -- ** HttpAction
    httpAction_headers,
    httpAction_auth,
    httpAction_confirmationUrl,
    httpAction_url,

    -- ** HttpActionHeader
    httpActionHeader_key,
    httpActionHeader_value,

    -- ** HttpAuthorization
    httpAuthorization_sigv4,

    -- ** HttpContext
    httpContext_queryString,
    httpContext_headers,

    -- ** HttpUrlDestinationConfiguration
    httpUrlDestinationConfiguration_confirmationUrl,

    -- ** HttpUrlDestinationProperties
    httpUrlDestinationProperties_confirmationUrl,

    -- ** HttpUrlDestinationSummary
    httpUrlDestinationSummary_confirmationUrl,

    -- ** ImplicitDeny
    implicitDeny_policies,

    -- ** IotAnalyticsAction
    iotAnalyticsAction_channelName,
    iotAnalyticsAction_roleArn,
    iotAnalyticsAction_batchMode,
    iotAnalyticsAction_channelArn,

    -- ** IotEventsAction
    iotEventsAction_batchMode,
    iotEventsAction_messageId,
    iotEventsAction_inputName,
    iotEventsAction_roleArn,

    -- ** IotSiteWiseAction
    iotSiteWiseAction_putAssetPropertyValueEntries,
    iotSiteWiseAction_roleArn,

    -- ** Job
    job_jobExecutionsRolloutConfig,
    job_status,
    job_reasonCode,
    job_timeoutConfig,
    job_targetSelection,
    job_namespaceId,
    job_jobProcessDetails,
    job_comment,
    job_completedAt,
    job_createdAt,
    job_forceCanceled,
    job_jobArn,
    job_targets,
    job_presignedUrlConfig,
    job_description,
    job_abortConfig,
    job_jobTemplateArn,
    job_jobId,
    job_lastUpdatedAt,

    -- ** JobExecution
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_thingArn,
    jobExecution_statusDetails,
    jobExecution_queuedAt,
    jobExecution_forceCanceled,
    jobExecution_executionNumber,
    jobExecution_versionNumber,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,

    -- ** JobExecutionStatusDetails
    jobExecutionStatusDetails_detailsMap,

    -- ** JobExecutionSummary
    jobExecutionSummary_startedAt,
    jobExecutionSummary_status,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,

    -- ** JobExecutionSummaryForJob
    jobExecutionSummaryForJob_thingArn,
    jobExecutionSummaryForJob_jobExecutionSummary,

    -- ** JobExecutionSummaryForThing
    jobExecutionSummaryForThing_jobExecutionSummary,
    jobExecutionSummaryForThing_jobId,

    -- ** JobExecutionsRolloutConfig
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- ** JobProcessDetails
    jobProcessDetails_processingTargets,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_numberOfTimedOutThings,
    jobProcessDetails_numberOfCanceledThings,

    -- ** JobSummary
    jobSummary_status,
    jobSummary_targetSelection,
    jobSummary_completedAt,
    jobSummary_createdAt,
    jobSummary_jobArn,
    jobSummary_thingGroupId,
    jobSummary_jobId,
    jobSummary_lastUpdatedAt,

    -- ** JobTemplateSummary
    jobTemplateSummary_createdAt,
    jobTemplateSummary_jobTemplateId,
    jobTemplateSummary_description,
    jobTemplateSummary_jobTemplateArn,

    -- ** KafkaAction
    kafkaAction_key,
    kafkaAction_partition,
    kafkaAction_destinationArn,
    kafkaAction_topic,
    kafkaAction_clientProperties,

    -- ** KeyPair
    keyPair_publicKey,
    keyPair_privateKey,

    -- ** KinesisAction
    kinesisAction_partitionKey,
    kinesisAction_roleArn,
    kinesisAction_streamName,

    -- ** LambdaAction
    lambdaAction_functionArn,

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

    -- ** MetricDimension
    metricDimension_operator,
    metricDimension_dimensionName,

    -- ** MetricToRetain
    metricToRetain_metricDimension,
    metricToRetain_metric,

    -- ** MetricValue
    metricValue_numbers,
    metricValue_ports,
    metricValue_cidrs,
    metricValue_strings,
    metricValue_number,
    metricValue_count,

    -- ** MitigationAction
    mitigationAction_roleArn,
    mitigationAction_id,
    mitigationAction_actionParams,
    mitigationAction_name,

    -- ** MitigationActionIdentifier
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_actionArn,
    mitigationActionIdentifier_creationDate,

    -- ** MitigationActionParams
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_updateDeviceCertificateParams,
    mitigationActionParams_publishFindingToSnsParams,
    mitigationActionParams_updateCACertificateParams,
    mitigationActionParams_addThingsToThingGroupParams,

    -- ** MqttContext
    mqttContext_clientId,
    mqttContext_password,
    mqttContext_username,

    -- ** NonCompliantResource
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceType,
    nonCompliantResource_resourceIdentifier,

    -- ** OTAUpdateFile
    oTAUpdateFile_fileVersion,
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_attributes,
    oTAUpdateFile_fileName,
    oTAUpdateFile_codeSigning,
    oTAUpdateFile_fileType,

    -- ** OTAUpdateInfo
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_targetSelection,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_targets,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_description,
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,

    -- ** OTAUpdateSummary
    oTAUpdateSummary_otaUpdateArn,
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateId,

    -- ** OpenSearchAction
    openSearchAction_roleArn,
    openSearchAction_endpoint,
    openSearchAction_index,
    openSearchAction_type,
    openSearchAction_id,

    -- ** OutgoingCertificate
    outgoingCertificate_transferDate,
    outgoingCertificate_certificateArn,
    outgoingCertificate_transferMessage,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferredTo,
    outgoingCertificate_certificateId,

    -- ** PercentPair
    percentPair_percent,
    percentPair_value,

    -- ** Policy
    policy_policyName,
    policy_policyArn,

    -- ** PolicyVersion
    policyVersion_createDate,
    policyVersion_versionId,
    policyVersion_isDefaultVersion,

    -- ** PolicyVersionIdentifier
    policyVersionIdentifier_policyVersionId,
    policyVersionIdentifier_policyName,

    -- ** PresignedUrlConfig
    presignedUrlConfig_roleArn,
    presignedUrlConfig_expiresInSec,

    -- ** ProvisioningHook
    provisioningHook_payloadVersion,
    provisioningHook_targetArn,

    -- ** ProvisioningTemplateSummary
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_description,
    provisioningTemplateSummary_templateArn,

    -- ** ProvisioningTemplateVersionSummary
    provisioningTemplateVersionSummary_creationDate,
    provisioningTemplateVersionSummary_versionId,
    provisioningTemplateVersionSummary_isDefaultVersion,

    -- ** PublishFindingToSnsParams
    publishFindingToSnsParams_topicArn,

    -- ** PutAssetPropertyValueEntry
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_assetId,
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

    -- ** RelatedResource
    relatedResource_additionalInfo,
    relatedResource_resourceType,
    relatedResource_resourceIdentifier,

    -- ** ReplaceDefaultPolicyVersionParams
    replaceDefaultPolicyVersionParams_templateName,

    -- ** RepublishAction
    republishAction_qos,
    republishAction_roleArn,
    republishAction_topic,

    -- ** ResourceIdentifier
    resourceIdentifier_roleAliasArn,
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_clientId,
    resourceIdentifier_cognitoIdentityPoolId,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_account,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_caCertificateId,

    -- ** RoleAliasDescription
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_roleAliasArn,
    roleAliasDescription_roleArn,
    roleAliasDescription_creationDate,
    roleAliasDescription_owner,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_roleAlias,

    -- ** S3Action
    s3Action_cannedAcl,
    s3Action_roleArn,
    s3Action_bucketName,
    s3Action_key,

    -- ** S3Destination
    s3Destination_prefix,
    s3Destination_bucket,

    -- ** S3Location
    s3Location_key,
    s3Location_version,
    s3Location_bucket,

    -- ** SalesforceAction
    salesforceAction_token,
    salesforceAction_url,

    -- ** ScheduledAuditMetadata
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_scheduledAuditArn,
    scheduledAuditMetadata_scheduledAuditName,
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_frequency,

    -- ** SecurityProfileIdentifier
    securityProfileIdentifier_name,
    securityProfileIdentifier_arn,

    -- ** SecurityProfileTarget
    securityProfileTarget_arn,

    -- ** SecurityProfileTargetMapping
    securityProfileTargetMapping_target,
    securityProfileTargetMapping_securityProfileIdentifier,

    -- ** ServerCertificateSummary
    serverCertificateSummary_serverCertificateStatus,
    serverCertificateSummary_serverCertificateStatusDetail,
    serverCertificateSummary_serverCertificateArn,

    -- ** SigV4Authorization
    sigV4Authorization_signingRegion,
    sigV4Authorization_serviceName,
    sigV4Authorization_roleArn,

    -- ** SigningProfileParameter
    signingProfileParameter_platform,
    signingProfileParameter_certificateArn,
    signingProfileParameter_certificatePathOnDevice,

    -- ** SnsAction
    snsAction_messageFormat,
    snsAction_targetArn,
    snsAction_roleArn,

    -- ** SqsAction
    sqsAction_useBase64,
    sqsAction_roleArn,
    sqsAction_queueUrl,

    -- ** StartSigningJobParameter
    startSigningJobParameter_signingProfileName,
    startSigningJobParameter_destination,
    startSigningJobParameter_signingProfileParameter,

    -- ** StatisticalThreshold
    statisticalThreshold_statistic,

    -- ** Statistics
    statistics_minimum,
    statistics_sum,
    statistics_stdDeviation,
    statistics_variance,
    statistics_average,
    statistics_count,
    statistics_sumOfSquares,
    statistics_maximum,

    -- ** StepFunctionsAction
    stepFunctionsAction_executionNamePrefix,
    stepFunctionsAction_stateMachineName,
    stepFunctionsAction_roleArn,

    -- ** Stream
    stream_streamId,
    stream_fileId,

    -- ** StreamFile
    streamFile_s3Location,
    streamFile_fileId,

    -- ** StreamInfo
    streamInfo_roleArn,
    streamInfo_streamVersion,
    streamInfo_createdAt,
    streamInfo_streamArn,
    streamInfo_streamId,
    streamInfo_description,
    streamInfo_files,
    streamInfo_lastUpdatedAt,

    -- ** StreamSummary
    streamSummary_streamVersion,
    streamSummary_streamArn,
    streamSummary_streamId,
    streamSummary_description,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TaskStatistics
    taskStatistics_totalChecks,
    taskStatistics_waitingForDataCollectionChecks,
    taskStatistics_compliantChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_failedChecks,
    taskStatistics_nonCompliantChecks,
    taskStatistics_canceledChecks,

    -- ** TaskStatisticsForAuditCheck
    taskStatisticsForAuditCheck_succeededFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_canceledFindingsCount,

    -- ** TermsAggregation
    termsAggregation_maxBuckets,

    -- ** ThingAttribute
    thingAttribute_thingArn,
    thingAttribute_thingName,
    thingAttribute_version,
    thingAttribute_attributes,
    thingAttribute_thingTypeName,

    -- ** ThingConnectivity
    thingConnectivity_disconnectReason,
    thingConnectivity_connected,
    thingConnectivity_timestamp,

    -- ** ThingDocument
    thingDocument_thingId,
    thingDocument_thingName,
    thingDocument_connectivity,
    thingDocument_attributes,
    thingDocument_thingGroupNames,
    thingDocument_shadow,
    thingDocument_thingTypeName,

    -- ** ThingGroupDocument
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_attributes,
    thingGroupDocument_thingGroupName,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupDescription,

    -- ** ThingGroupIndexingConfiguration
    thingGroupIndexingConfiguration_managedFields,
    thingGroupIndexingConfiguration_customFields,
    thingGroupIndexingConfiguration_thingGroupIndexingMode,

    -- ** ThingGroupMetadata
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_creationDate,
    thingGroupMetadata_rootToParentThingGroups,

    -- ** ThingGroupProperties
    thingGroupProperties_thingGroupDescription,
    thingGroupProperties_attributePayload,

    -- ** ThingIndexingConfiguration
    thingIndexingConfiguration_thingConnectivityIndexingMode,
    thingIndexingConfiguration_managedFields,
    thingIndexingConfiguration_customFields,
    thingIndexingConfiguration_thingIndexingMode,

    -- ** ThingTypeDefinition
    thingTypeDefinition_thingTypeProperties,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeArn,
    thingTypeDefinition_thingTypeName,

    -- ** ThingTypeMetadata
    thingTypeMetadata_deprecationDate,
    thingTypeMetadata_creationDate,
    thingTypeMetadata_deprecated,

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
    topicRule_ruleName,
    topicRule_errorAction,
    topicRule_awsIotSqlVersion,
    topicRule_createdAt,
    topicRule_actions,
    topicRule_ruleDisabled,
    topicRule_description,
    topicRule_sql,

    -- ** TopicRuleDestination
    topicRuleDestination_status,
    topicRuleDestination_createdAt,
    topicRuleDestination_arn,
    topicRuleDestination_vpcProperties,
    topicRuleDestination_statusReason,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_lastUpdatedAt,

    -- ** TopicRuleDestinationConfiguration
    topicRuleDestinationConfiguration_vpcConfiguration,
    topicRuleDestinationConfiguration_httpUrlConfiguration,

    -- ** TopicRuleDestinationSummary
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_statusReason,
    topicRuleDestinationSummary_vpcDestinationSummary,
    topicRuleDestinationSummary_lastUpdatedAt,

    -- ** TopicRuleListItem
    topicRuleListItem_ruleName,
    topicRuleListItem_ruleArn,
    topicRuleListItem_createdAt,
    topicRuleListItem_topicPattern,
    topicRuleListItem_ruleDisabled,

    -- ** TopicRulePayload
    topicRulePayload_errorAction,
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_ruleDisabled,
    topicRulePayload_description,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- ** TransferData
    transferData_transferDate,
    transferData_transferMessage,
    transferData_acceptDate,
    transferData_rejectReason,
    transferData_rejectDate,

    -- ** UpdateCACertificateParams
    updateCACertificateParams_action,

    -- ** UpdateDeviceCertificateParams
    updateDeviceCertificateParams_action,

    -- ** ValidationError
    validationError_errorMessage,

    -- ** ViolationEvent
    violationEvent_metricValue,
    violationEvent_violationId,
    violationEvent_thingName,
    violationEvent_verificationStateDescription,
    violationEvent_securityProfileName,
    violationEvent_behavior,
    violationEvent_verificationState,
    violationEvent_violationEventTime,
    violationEvent_violationEventType,
    violationEvent_violationEventAdditionalInfo,

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
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_vpcId,

    -- ** VpcDestinationSummary
    vpcDestinationSummary_roleArn,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_vpcId,
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
import Network.AWS.IoT.Types.AbortConfig
import Network.AWS.IoT.Types.AbortCriteria
import Network.AWS.IoT.Types.Action
import Network.AWS.IoT.Types.ActiveViolation
import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.AggregationType
import Network.AWS.IoT.Types.AlertTarget
import Network.AWS.IoT.Types.Allowed
import Network.AWS.IoT.Types.AssetPropertyTimestamp
import Network.AWS.IoT.Types.AssetPropertyValue
import Network.AWS.IoT.Types.AssetPropertyVariant
import Network.AWS.IoT.Types.AttributePayload
import Network.AWS.IoT.Types.AuditCheckConfiguration
import Network.AWS.IoT.Types.AuditCheckDetails
import Network.AWS.IoT.Types.AuditFinding
import Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
import Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
import Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
import Network.AWS.IoT.Types.AuditNotificationTarget
import Network.AWS.IoT.Types.AuditSuppression
import Network.AWS.IoT.Types.AuditTaskMetadata
import Network.AWS.IoT.Types.AuthInfo
import Network.AWS.IoT.Types.AuthResult
import Network.AWS.IoT.Types.AuthorizerConfig
import Network.AWS.IoT.Types.AuthorizerDescription
import Network.AWS.IoT.Types.AuthorizerSummary
import Network.AWS.IoT.Types.AwsJobAbortConfig
import Network.AWS.IoT.Types.AwsJobAbortCriteria
import Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
import Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
import Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
import Network.AWS.IoT.Types.AwsJobTimeoutConfig
import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.BehaviorModelTrainingSummary
import Network.AWS.IoT.Types.BillingGroupMetadata
import Network.AWS.IoT.Types.BillingGroupProperties
import Network.AWS.IoT.Types.Bucket
import Network.AWS.IoT.Types.BucketsAggregationType
import Network.AWS.IoT.Types.CACertificate
import Network.AWS.IoT.Types.CACertificateDescription
import Network.AWS.IoT.Types.Certificate
import Network.AWS.IoT.Types.CertificateDescription
import Network.AWS.IoT.Types.CertificateValidity
import Network.AWS.IoT.Types.CloudwatchAlarmAction
import Network.AWS.IoT.Types.CloudwatchLogsAction
import Network.AWS.IoT.Types.CloudwatchMetricAction
import Network.AWS.IoT.Types.CodeSigning
import Network.AWS.IoT.Types.CodeSigningCertificateChain
import Network.AWS.IoT.Types.CodeSigningSignature
import Network.AWS.IoT.Types.Configuration
import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.Denied
import Network.AWS.IoT.Types.Destination
import Network.AWS.IoT.Types.DetectMitigationActionExecution
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics
import Network.AWS.IoT.Types.DetectMitigationActionsTaskSummary
import Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget
import Network.AWS.IoT.Types.DomainConfigurationSummary
import Network.AWS.IoT.Types.DynamoDBAction
import Network.AWS.IoT.Types.DynamoDBv2Action
import Network.AWS.IoT.Types.EffectivePolicy
import Network.AWS.IoT.Types.ElasticsearchAction
import Network.AWS.IoT.Types.EnableIoTLoggingParams
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ExponentialRolloutRate
import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.FileLocation
import Network.AWS.IoT.Types.FirehoseAction
import Network.AWS.IoT.Types.FleetMetricNameAndArn
import Network.AWS.IoT.Types.GroupNameAndArn
import Network.AWS.IoT.Types.HttpAction
import Network.AWS.IoT.Types.HttpActionHeader
import Network.AWS.IoT.Types.HttpAuthorization
import Network.AWS.IoT.Types.HttpContext
import Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
import Network.AWS.IoT.Types.HttpUrlDestinationProperties
import Network.AWS.IoT.Types.HttpUrlDestinationSummary
import Network.AWS.IoT.Types.ImplicitDeny
import Network.AWS.IoT.Types.IotAnalyticsAction
import Network.AWS.IoT.Types.IotEventsAction
import Network.AWS.IoT.Types.IotSiteWiseAction
import Network.AWS.IoT.Types.Job
import Network.AWS.IoT.Types.JobExecution
import Network.AWS.IoT.Types.JobExecutionStatusDetails
import Network.AWS.IoT.Types.JobExecutionSummary
import Network.AWS.IoT.Types.JobExecutionSummaryForJob
import Network.AWS.IoT.Types.JobExecutionSummaryForThing
import Network.AWS.IoT.Types.JobExecutionsRolloutConfig
import Network.AWS.IoT.Types.JobProcessDetails
import Network.AWS.IoT.Types.JobSummary
import Network.AWS.IoT.Types.JobTemplateSummary
import Network.AWS.IoT.Types.KafkaAction
import Network.AWS.IoT.Types.KeyPair
import Network.AWS.IoT.Types.KinesisAction
import Network.AWS.IoT.Types.LambdaAction
import Network.AWS.IoT.Types.LogTarget
import Network.AWS.IoT.Types.LogTargetConfiguration
import Network.AWS.IoT.Types.LoggingOptionsPayload
import Network.AWS.IoT.Types.MachineLearningDetectionConfig
import Network.AWS.IoT.Types.MetricDimension
import Network.AWS.IoT.Types.MetricToRetain
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.MitigationAction
import Network.AWS.IoT.Types.MitigationActionIdentifier
import Network.AWS.IoT.Types.MitigationActionParams
import Network.AWS.IoT.Types.MqttContext
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateInfo
import Network.AWS.IoT.Types.OTAUpdateSummary
import Network.AWS.IoT.Types.OpenSearchAction
import Network.AWS.IoT.Types.OutgoingCertificate
import Network.AWS.IoT.Types.PercentPair
import Network.AWS.IoT.Types.Policy
import Network.AWS.IoT.Types.PolicyVersion
import Network.AWS.IoT.Types.PolicyVersionIdentifier
import Network.AWS.IoT.Types.PresignedUrlConfig
import Network.AWS.IoT.Types.ProvisioningHook
import Network.AWS.IoT.Types.ProvisioningTemplateSummary
import Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
import Network.AWS.IoT.Types.PublishFindingToSnsParams
import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
import Network.AWS.IoT.Types.PutItemInput
import Network.AWS.IoT.Types.RateIncreaseCriteria
import Network.AWS.IoT.Types.RegistrationConfig
import Network.AWS.IoT.Types.RelatedResource
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
import Network.AWS.IoT.Types.RepublishAction
import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.RoleAliasDescription
import Network.AWS.IoT.Types.S3Action
import Network.AWS.IoT.Types.S3Destination
import Network.AWS.IoT.Types.S3Location
import Network.AWS.IoT.Types.SalesforceAction
import Network.AWS.IoT.Types.ScheduledAuditMetadata
import Network.AWS.IoT.Types.SecurityProfileIdentifier
import Network.AWS.IoT.Types.SecurityProfileTarget
import Network.AWS.IoT.Types.SecurityProfileTargetMapping
import Network.AWS.IoT.Types.ServerCertificateSummary
import Network.AWS.IoT.Types.SigV4Authorization
import Network.AWS.IoT.Types.SigningProfileParameter
import Network.AWS.IoT.Types.SnsAction
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StartSigningJobParameter
import Network.AWS.IoT.Types.StatisticalThreshold
import Network.AWS.IoT.Types.Statistics
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.Stream
import Network.AWS.IoT.Types.StreamFile
import Network.AWS.IoT.Types.StreamInfo
import Network.AWS.IoT.Types.StreamSummary
import Network.AWS.IoT.Types.Tag
import Network.AWS.IoT.Types.TaskStatistics
import Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
import Network.AWS.IoT.Types.TermsAggregation
import Network.AWS.IoT.Types.ThingAttribute
import Network.AWS.IoT.Types.ThingConnectivity
import Network.AWS.IoT.Types.ThingDocument
import Network.AWS.IoT.Types.ThingGroupDocument
import Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
import Network.AWS.IoT.Types.ThingGroupMetadata
import Network.AWS.IoT.Types.ThingGroupProperties
import Network.AWS.IoT.Types.ThingIndexingConfiguration
import Network.AWS.IoT.Types.ThingTypeDefinition
import Network.AWS.IoT.Types.ThingTypeMetadata
import Network.AWS.IoT.Types.ThingTypeProperties
import Network.AWS.IoT.Types.TimeoutConfig
import Network.AWS.IoT.Types.TimestreamAction
import Network.AWS.IoT.Types.TimestreamDimension
import Network.AWS.IoT.Types.TimestreamTimestamp
import Network.AWS.IoT.Types.TlsContext
import Network.AWS.IoT.Types.TopicRule
import Network.AWS.IoT.Types.TopicRuleDestination
import Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
import Network.AWS.IoT.Types.TopicRuleDestinationSummary
import Network.AWS.IoT.Types.TopicRuleListItem
import Network.AWS.IoT.Types.TopicRulePayload
import Network.AWS.IoT.Types.TransferData
import Network.AWS.IoT.Types.UpdateCACertificateParams
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
import Network.AWS.IoT.Types.ValidationError
import Network.AWS.IoT.Types.ViolationEvent
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import Network.AWS.IoT.Types.ViolationEventOccurrenceRange
import Network.AWS.IoT.Types.VpcDestinationConfiguration
import Network.AWS.IoT.Types.VpcDestinationProperties
import Network.AWS.IoT.Types.VpcDestinationSummary
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

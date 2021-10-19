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

    -- ** GetCardinality
    getCardinality_queryVersion,
    getCardinality_aggregationField,
    getCardinality_indexName,
    getCardinality_queryString,
    getCardinalityResponse_cardinality,
    getCardinalityResponse_httpStatus,

    -- ** CreateDomainConfiguration
    createDomainConfiguration_authorizerConfig,
    createDomainConfiguration_serverCertificateArns,
    createDomainConfiguration_domainName,
    createDomainConfiguration_serviceType,
    createDomainConfiguration_validationCertificateArn,
    createDomainConfiguration_tags,
    createDomainConfiguration_domainConfigurationName,
    createDomainConfigurationResponse_domainConfigurationName,
    createDomainConfigurationResponse_domainConfigurationArn,
    createDomainConfigurationResponse_httpStatus,

    -- ** StartDetectMitigationActionsTask
    startDetectMitigationActionsTask_violationEventOccurrenceRange,
    startDetectMitigationActionsTask_includeOnlyActiveViolations,
    startDetectMitigationActionsTask_includeSuppressedAlerts,
    startDetectMitigationActionsTask_taskId,
    startDetectMitigationActionsTask_target,
    startDetectMitigationActionsTask_actions,
    startDetectMitigationActionsTask_clientRequestToken,
    startDetectMitigationActionsTaskResponse_taskId,
    startDetectMitigationActionsTaskResponse_httpStatus,

    -- ** DeleteSecurityProfile
    deleteSecurityProfile_expectedVersion,
    deleteSecurityProfile_securityProfileName,
    deleteSecurityProfileResponse_httpStatus,

    -- ** UpdateSecurityProfile
    updateSecurityProfile_alertTargets,
    updateSecurityProfile_additionalMetricsToRetainV2,
    updateSecurityProfile_behaviors,
    updateSecurityProfile_expectedVersion,
    updateSecurityProfile_deleteAlertTargets,
    updateSecurityProfile_additionalMetricsToRetain,
    updateSecurityProfile_securityProfileDescription,
    updateSecurityProfile_deleteBehaviors,
    updateSecurityProfile_deleteAdditionalMetricsToRetain,
    updateSecurityProfile_securityProfileName,
    updateSecurityProfileResponse_alertTargets,
    updateSecurityProfileResponse_additionalMetricsToRetainV2,
    updateSecurityProfileResponse_behaviors,
    updateSecurityProfileResponse_lastModifiedDate,
    updateSecurityProfileResponse_version,
    updateSecurityProfileResponse_securityProfileName,
    updateSecurityProfileResponse_creationDate,
    updateSecurityProfileResponse_additionalMetricsToRetain,
    updateSecurityProfileResponse_securityProfileArn,
    updateSecurityProfileResponse_securityProfileDescription,
    updateSecurityProfileResponse_httpStatus,

    -- ** ListSecurityProfiles
    listSecurityProfiles_metricName,
    listSecurityProfiles_nextToken,
    listSecurityProfiles_dimensionName,
    listSecurityProfiles_maxResults,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileIdentifiers,
    listSecurityProfilesResponse_httpStatus,

    -- ** ListPolicies
    listPolicies_marker,
    listPolicies_ascendingOrder,
    listPolicies_pageSize,
    listPoliciesResponse_nextMarker,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,

    -- ** DescribeProvisioningTemplate
    describeProvisioningTemplate_templateName,
    describeProvisioningTemplateResponse_lastModifiedDate,
    describeProvisioningTemplateResponse_templateName,
    describeProvisioningTemplateResponse_preProvisioningHook,
    describeProvisioningTemplateResponse_enabled,
    describeProvisioningTemplateResponse_provisioningRoleArn,
    describeProvisioningTemplateResponse_defaultVersionId,
    describeProvisioningTemplateResponse_creationDate,
    describeProvisioningTemplateResponse_templateArn,
    describeProvisioningTemplateResponse_templateBody,
    describeProvisioningTemplateResponse_description,
    describeProvisioningTemplateResponse_httpStatus,

    -- ** UpdateMitigationAction
    updateMitigationAction_actionParams,
    updateMitigationAction_roleArn,
    updateMitigationAction_actionName,
    updateMitigationActionResponse_actionId,
    updateMitigationActionResponse_actionArn,
    updateMitigationActionResponse_httpStatus,

    -- ** DeleteMitigationAction
    deleteMitigationAction_actionName,
    deleteMitigationActionResponse_httpStatus,

    -- ** DeleteJobExecution
    deleteJobExecution_force,
    deleteJobExecution_namespaceId,
    deleteJobExecution_jobId,
    deleteJobExecution_thingName,
    deleteJobExecution_executionNumber,

    -- ** CreatePolicy
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,
    createPolicyResponse_policyName,
    createPolicyResponse_policyDocument,
    createPolicyResponse_policyVersionId,
    createPolicyResponse_policyArn,
    createPolicyResponse_httpStatus,

    -- ** RegisterCertificate
    registerCertificate_status,
    registerCertificate_caCertificatePem,
    registerCertificate_setAsActive,
    registerCertificate_certificatePem,
    registerCertificateResponse_certificateArn,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,

    -- ** DeleteDynamicThingGroup
    deleteDynamicThingGroup_expectedVersion,
    deleteDynamicThingGroup_thingGroupName,
    deleteDynamicThingGroupResponse_httpStatus,

    -- ** ListThingPrincipals
    listThingPrincipals_nextToken,
    listThingPrincipals_maxResults,
    listThingPrincipals_thingName,
    listThingPrincipalsResponse_principals,
    listThingPrincipalsResponse_nextToken,
    listThingPrincipalsResponse_httpStatus,

    -- ** UpdateDynamicThingGroup
    updateDynamicThingGroup_queryVersion,
    updateDynamicThingGroup_expectedVersion,
    updateDynamicThingGroup_queryString,
    updateDynamicThingGroup_indexName,
    updateDynamicThingGroup_thingGroupName,
    updateDynamicThingGroup_thingGroupProperties,
    updateDynamicThingGroupResponse_version,
    updateDynamicThingGroupResponse_httpStatus,

    -- ** DescribeRoleAlias
    describeRoleAlias_roleAlias,
    describeRoleAliasResponse_roleAliasDescription,
    describeRoleAliasResponse_httpStatus,

    -- ** CreateProvisioningTemplateVersion
    createProvisioningTemplateVersion_setAsDefault,
    createProvisioningTemplateVersion_templateName,
    createProvisioningTemplateVersion_templateBody,
    createProvisioningTemplateVersionResponse_versionId,
    createProvisioningTemplateVersionResponse_templateName,
    createProvisioningTemplateVersionResponse_templateArn,
    createProvisioningTemplateVersionResponse_isDefaultVersion,
    createProvisioningTemplateVersionResponse_httpStatus,

    -- ** CreateOTAUpdate
    createOTAUpdate_awsJobAbortConfig,
    createOTAUpdate_awsJobExecutionsRolloutConfig,
    createOTAUpdate_protocols,
    createOTAUpdate_awsJobPresignedUrlConfig,
    createOTAUpdate_additionalParameters,
    createOTAUpdate_awsJobTimeoutConfig,
    createOTAUpdate_description,
    createOTAUpdate_targetSelection,
    createOTAUpdate_tags,
    createOTAUpdate_otaUpdateId,
    createOTAUpdate_targets,
    createOTAUpdate_files,
    createOTAUpdate_roleArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_otaUpdateStatus,
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_otaUpdateArn,
    createOTAUpdateResponse_httpStatus,

    -- ** DescribeDefaultAuthorizer
    describeDefaultAuthorizerResponse_authorizerDescription,
    describeDefaultAuthorizerResponse_httpStatus,

    -- ** ListAuditMitigationActionsTasks
    listAuditMitigationActionsTasks_auditTaskId,
    listAuditMitigationActionsTasks_nextToken,
    listAuditMitigationActionsTasks_findingId,
    listAuditMitigationActionsTasks_maxResults,
    listAuditMitigationActionsTasks_taskStatus,
    listAuditMitigationActionsTasks_startTime,
    listAuditMitigationActionsTasks_endTime,
    listAuditMitigationActionsTasksResponse_tasks,
    listAuditMitigationActionsTasksResponse_nextToken,
    listAuditMitigationActionsTasksResponse_httpStatus,

    -- ** ListThingRegistrationTaskReports
    listThingRegistrationTaskReports_nextToken,
    listThingRegistrationTaskReports_maxResults,
    listThingRegistrationTaskReports_taskId,
    listThingRegistrationTaskReports_reportType,
    listThingRegistrationTaskReportsResponse_resourceLinks,
    listThingRegistrationTaskReportsResponse_nextToken,
    listThingRegistrationTaskReportsResponse_reportType,
    listThingRegistrationTaskReportsResponse_httpStatus,

    -- ** GetBehaviorModelTrainingSummaries
    getBehaviorModelTrainingSummaries_nextToken,
    getBehaviorModelTrainingSummaries_securityProfileName,
    getBehaviorModelTrainingSummaries_maxResults,
    getBehaviorModelTrainingSummariesResponse_nextToken,
    getBehaviorModelTrainingSummariesResponse_summaries,
    getBehaviorModelTrainingSummariesResponse_httpStatus,

    -- ** ListPrincipalThings
    listPrincipalThings_nextToken,
    listPrincipalThings_maxResults,
    listPrincipalThings_principal,
    listPrincipalThingsResponse_nextToken,
    listPrincipalThingsResponse_things,
    listPrincipalThingsResponse_httpStatus,

    -- ** RemoveThingFromThingGroup
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupName,
    removeThingFromThingGroup_thingName,
    removeThingFromThingGroupResponse_httpStatus,

    -- ** DescribeEventConfigurations
    describeEventConfigurationsResponse_lastModifiedDate,
    describeEventConfigurationsResponse_eventConfigurations,
    describeEventConfigurationsResponse_creationDate,
    describeEventConfigurationsResponse_httpStatus,

    -- ** CancelDetectMitigationActionsTask
    cancelDetectMitigationActionsTask_taskId,
    cancelDetectMitigationActionsTaskResponse_httpStatus,

    -- ** ListTopicRuleDestinations
    listTopicRuleDestinations_nextToken,
    listTopicRuleDestinations_maxResults,
    listTopicRuleDestinationsResponse_destinationSummaries,
    listTopicRuleDestinationsResponse_nextToken,
    listTopicRuleDestinationsResponse_httpStatus,

    -- ** RegisterCertificateWithoutCA
    registerCertificateWithoutCA_status,
    registerCertificateWithoutCA_certificatePem,
    registerCertificateWithoutCAResponse_certificateArn,
    registerCertificateWithoutCAResponse_certificateId,
    registerCertificateWithoutCAResponse_httpStatus,

    -- ** DescribeCustomMetric
    describeCustomMetric_metricName,
    describeCustomMetricResponse_metricType,
    describeCustomMetricResponse_lastModifiedDate,
    describeCustomMetricResponse_metricName,
    describeCustomMetricResponse_displayName,
    describeCustomMetricResponse_creationDate,
    describeCustomMetricResponse_metricArn,
    describeCustomMetricResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThingGroups
    listThingGroups_namePrefixFilter,
    listThingGroups_parentGroup,
    listThingGroups_nextToken,
    listThingGroups_recursive,
    listThingGroups_maxResults,
    listThingGroupsResponse_thingGroups,
    listThingGroupsResponse_nextToken,
    listThingGroupsResponse_httpStatus,

    -- ** DescribeJobTemplate
    describeJobTemplate_jobTemplateId,
    describeJobTemplateResponse_jobExecutionsRolloutConfig,
    describeJobTemplateResponse_documentSource,
    describeJobTemplateResponse_createdAt,
    describeJobTemplateResponse_abortConfig,
    describeJobTemplateResponse_presignedUrlConfig,
    describeJobTemplateResponse_document,
    describeJobTemplateResponse_jobTemplateId,
    describeJobTemplateResponse_jobTemplateArn,
    describeJobTemplateResponse_description,
    describeJobTemplateResponse_timeoutConfig,
    describeJobTemplateResponse_httpStatus,

    -- ** ListScheduledAudits
    listScheduledAudits_nextToken,
    listScheduledAudits_maxResults,
    listScheduledAuditsResponse_scheduledAudits,
    listScheduledAuditsResponse_nextToken,
    listScheduledAuditsResponse_httpStatus,

    -- ** DescribeThingRegistrationTask
    describeThingRegistrationTask_taskId,
    describeThingRegistrationTaskResponse_status,
    describeThingRegistrationTaskResponse_lastModifiedDate,
    describeThingRegistrationTaskResponse_inputFileKey,
    describeThingRegistrationTaskResponse_taskId,
    describeThingRegistrationTaskResponse_creationDate,
    describeThingRegistrationTaskResponse_percentageProgress,
    describeThingRegistrationTaskResponse_templateBody,
    describeThingRegistrationTaskResponse_successCount,
    describeThingRegistrationTaskResponse_message,
    describeThingRegistrationTaskResponse_failureCount,
    describeThingRegistrationTaskResponse_inputFileBucket,
    describeThingRegistrationTaskResponse_roleArn,
    describeThingRegistrationTaskResponse_httpStatus,

    -- ** UpdateScheduledAudit
    updateScheduledAudit_frequency,
    updateScheduledAudit_dayOfMonth,
    updateScheduledAudit_targetCheckNames,
    updateScheduledAudit_dayOfWeek,
    updateScheduledAudit_scheduledAuditName,
    updateScheduledAuditResponse_scheduledAuditArn,
    updateScheduledAuditResponse_httpStatus,

    -- ** DeleteScheduledAudit
    deleteScheduledAudit_scheduledAuditName,
    deleteScheduledAuditResponse_httpStatus,

    -- ** DescribeAuditFinding
    describeAuditFinding_findingId,
    describeAuditFindingResponse_finding,
    describeAuditFindingResponse_httpStatus,

    -- ** DescribeDimension
    describeDimension_name,
    describeDimensionResponse_lastModifiedDate,
    describeDimensionResponse_arn,
    describeDimensionResponse_stringValues,
    describeDimensionResponse_name,
    describeDimensionResponse_creationDate,
    describeDimensionResponse_type,
    describeDimensionResponse_httpStatus,

    -- ** GetLoggingOptions
    getLoggingOptionsResponse_logLevel,
    getLoggingOptionsResponse_roleArn,
    getLoggingOptionsResponse_httpStatus,

    -- ** DeleteAccountAuditConfiguration
    deleteAccountAuditConfiguration_deleteScheduledAudits,
    deleteAccountAuditConfigurationResponse_httpStatus,

    -- ** UpdateAccountAuditConfiguration
    updateAccountAuditConfiguration_auditCheckConfigurations,
    updateAccountAuditConfiguration_auditNotificationTargetConfigurations,
    updateAccountAuditConfiguration_roleArn,
    updateAccountAuditConfigurationResponse_httpStatus,

    -- ** GetOTAUpdate
    getOTAUpdate_otaUpdateId,
    getOTAUpdateResponse_otaUpdateInfo,
    getOTAUpdateResponse_httpStatus,

    -- ** GetEffectivePolicies
    getEffectivePolicies_principal,
    getEffectivePolicies_cognitoIdentityPoolId,
    getEffectivePolicies_thingName,
    getEffectivePoliciesResponse_effectivePolicies,
    getEffectivePoliciesResponse_httpStatus,

    -- ** ListThingTypes
    listThingTypes_thingTypeName,
    listThingTypes_nextToken,
    listThingTypes_maxResults,
    listThingTypesResponse_thingTypes,
    listThingTypesResponse_nextToken,
    listThingTypesResponse_httpStatus,

    -- ** SetV2LoggingOptions
    setV2LoggingOptions_disableAllLogs,
    setV2LoggingOptions_defaultLogLevel,
    setV2LoggingOptions_roleArn,

    -- ** CreateProvisioningTemplate
    createProvisioningTemplate_preProvisioningHook,
    createProvisioningTemplate_enabled,
    createProvisioningTemplate_description,
    createProvisioningTemplate_tags,
    createProvisioningTemplate_templateName,
    createProvisioningTemplate_templateBody,
    createProvisioningTemplate_provisioningRoleArn,
    createProvisioningTemplateResponse_templateName,
    createProvisioningTemplateResponse_defaultVersionId,
    createProvisioningTemplateResponse_templateArn,
    createProvisioningTemplateResponse_httpStatus,

    -- ** ListThingGroupsForThing
    listThingGroupsForThing_nextToken,
    listThingGroupsForThing_maxResults,
    listThingGroupsForThing_thingName,
    listThingGroupsForThingResponse_thingGroups,
    listThingGroupsForThingResponse_nextToken,
    listThingGroupsForThingResponse_httpStatus,

    -- ** CreateCertificateFromCsr
    createCertificateFromCsr_setAsActive,
    createCertificateFromCsr_certificateSigningRequest,
    createCertificateFromCsrResponse_certificatePem,
    createCertificateFromCsrResponse_certificateArn,
    createCertificateFromCsrResponse_certificateId,
    createCertificateFromCsrResponse_httpStatus,

    -- ** DeleteThing
    deleteThing_expectedVersion,
    deleteThing_thingName,
    deleteThingResponse_httpStatus,

    -- ** UpdateThing
    updateThing_removeThingType,
    updateThing_thingTypeName,
    updateThing_expectedVersion,
    updateThing_attributePayload,
    updateThing_thingName,
    updateThingResponse_httpStatus,

    -- ** DeleteProvisioningTemplate
    deleteProvisioningTemplate_templateName,
    deleteProvisioningTemplateResponse_httpStatus,

    -- ** UpdateProvisioningTemplate
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_templateName,
    updateProvisioningTemplateResponse_httpStatus,

    -- ** DescribeMitigationAction
    describeMitigationAction_actionName,
    describeMitigationActionResponse_lastModifiedDate,
    describeMitigationActionResponse_actionParams,
    describeMitigationActionResponse_actionId,
    describeMitigationActionResponse_actionName,
    describeMitigationActionResponse_creationDate,
    describeMitigationActionResponse_actionArn,
    describeMitigationActionResponse_actionType,
    describeMitigationActionResponse_roleArn,
    describeMitigationActionResponse_httpStatus,

    -- ** StartThingRegistrationTask
    startThingRegistrationTask_templateBody,
    startThingRegistrationTask_inputFileBucket,
    startThingRegistrationTask_inputFileKey,
    startThingRegistrationTask_roleArn,
    startThingRegistrationTaskResponse_taskId,
    startThingRegistrationTaskResponse_httpStatus,

    -- ** CreateScheduledAudit
    createScheduledAudit_dayOfMonth,
    createScheduledAudit_dayOfWeek,
    createScheduledAudit_tags,
    createScheduledAudit_frequency,
    createScheduledAudit_targetCheckNames,
    createScheduledAudit_scheduledAuditName,
    createScheduledAuditResponse_scheduledAuditArn,
    createScheduledAuditResponse_httpStatus,

    -- ** ListAuthorizers
    listAuthorizers_status,
    listAuthorizers_marker,
    listAuthorizers_ascendingOrder,
    listAuthorizers_pageSize,
    listAuthorizersResponse_authorizers,
    listAuthorizersResponse_nextMarker,
    listAuthorizersResponse_httpStatus,

    -- ** ListJobExecutionsForJob
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_jobId,
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_httpStatus,

    -- ** RemoveThingFromBillingGroup
    removeThingFromBillingGroup_thingArn,
    removeThingFromBillingGroup_billingGroupArn,
    removeThingFromBillingGroup_thingName,
    removeThingFromBillingGroup_billingGroupName,
    removeThingFromBillingGroupResponse_httpStatus,

    -- ** SearchIndex
    searchIndex_queryVersion,
    searchIndex_nextToken,
    searchIndex_maxResults,
    searchIndex_indexName,
    searchIndex_queryString,
    searchIndexResponse_thingGroups,
    searchIndexResponse_nextToken,
    searchIndexResponse_things,
    searchIndexResponse_httpStatus,

    -- ** CreateThingType
    createThingType_thingTypeProperties,
    createThingType_tags,
    createThingType_thingTypeName,
    createThingTypeResponse_thingTypeName,
    createThingTypeResponse_thingTypeId,
    createThingTypeResponse_thingTypeArn,
    createThingTypeResponse_httpStatus,

    -- ** DescribeSecurityProfile
    describeSecurityProfile_securityProfileName,
    describeSecurityProfileResponse_alertTargets,
    describeSecurityProfileResponse_additionalMetricsToRetainV2,
    describeSecurityProfileResponse_behaviors,
    describeSecurityProfileResponse_lastModifiedDate,
    describeSecurityProfileResponse_version,
    describeSecurityProfileResponse_securityProfileName,
    describeSecurityProfileResponse_creationDate,
    describeSecurityProfileResponse_additionalMetricsToRetain,
    describeSecurityProfileResponse_securityProfileArn,
    describeSecurityProfileResponse_securityProfileDescription,
    describeSecurityProfileResponse_httpStatus,

    -- ** DeleteV2LoggingLevel
    deleteV2LoggingLevel_targetType,
    deleteV2LoggingLevel_targetName,

    -- ** SetDefaultAuthorizer
    setDefaultAuthorizer_authorizerName,
    setDefaultAuthorizerResponse_authorizerName,
    setDefaultAuthorizerResponse_authorizerArn,
    setDefaultAuthorizerResponse_httpStatus,

    -- ** DescribeJobExecution
    describeJobExecution_executionNumber,
    describeJobExecution_jobId,
    describeJobExecution_thingName,
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,

    -- ** CancelCertificateTransfer
    cancelCertificateTransfer_certificateId,

    -- ** GetIndexingConfiguration
    getIndexingConfigurationResponse_thingGroupIndexingConfiguration,
    getIndexingConfigurationResponse_thingIndexingConfiguration,
    getIndexingConfigurationResponse_httpStatus,

    -- ** ListAuditMitigationActionsExecutions
    listAuditMitigationActionsExecutions_nextToken,
    listAuditMitigationActionsExecutions_actionStatus,
    listAuditMitigationActionsExecutions_maxResults,
    listAuditMitigationActionsExecutions_taskId,
    listAuditMitigationActionsExecutions_findingId,
    listAuditMitigationActionsExecutionsResponse_actionsExecutions,
    listAuditMitigationActionsExecutionsResponse_nextToken,
    listAuditMitigationActionsExecutionsResponse_httpStatus,

    -- ** CreateCustomMetric
    createCustomMetric_displayName,
    createCustomMetric_tags,
    createCustomMetric_metricName,
    createCustomMetric_metricType,
    createCustomMetric_clientRequestToken,
    createCustomMetricResponse_metricName,
    createCustomMetricResponse_metricArn,
    createCustomMetricResponse_httpStatus,

    -- ** DescribeAuditMitigationActionsTask
    describeAuditMitigationActionsTask_taskId,
    describeAuditMitigationActionsTaskResponse_startTime,
    describeAuditMitigationActionsTaskResponse_taskStatistics,
    describeAuditMitigationActionsTaskResponse_actionsDefinition,
    describeAuditMitigationActionsTaskResponse_auditCheckToActionsMapping,
    describeAuditMitigationActionsTaskResponse_endTime,
    describeAuditMitigationActionsTaskResponse_target,
    describeAuditMitigationActionsTaskResponse_taskStatus,
    describeAuditMitigationActionsTaskResponse_httpStatus,

    -- ** GetStatistics
    getStatistics_queryVersion,
    getStatistics_aggregationField,
    getStatistics_indexName,
    getStatistics_queryString,
    getStatisticsResponse_statistics,
    getStatisticsResponse_httpStatus,

    -- ** DeleteRoleAlias
    deleteRoleAlias_roleAlias,
    deleteRoleAliasResponse_httpStatus,

    -- ** UpdateRoleAlias
    updateRoleAlias_credentialDurationSeconds,
    updateRoleAlias_roleArn,
    updateRoleAlias_roleAlias,
    updateRoleAliasResponse_roleAliasArn,
    updateRoleAliasResponse_roleAlias,
    updateRoleAliasResponse_httpStatus,

    -- ** ListFleetMetrics
    listFleetMetrics_nextToken,
    listFleetMetrics_maxResults,
    listFleetMetricsResponse_fleetMetrics,
    listFleetMetricsResponse_nextToken,
    listFleetMetricsResponse_httpStatus,

    -- ** DeletePolicyVersion
    deletePolicyVersion_policyName,
    deletePolicyVersion_policyVersionId,

    -- ** DisableTopicRule
    disableTopicRule_ruleName,

    -- ** CreateTopicRule
    createTopicRule_tags,
    createTopicRule_ruleName,
    createTopicRule_topicRulePayload,

    -- ** CreateJob
    createJob_jobExecutionsRolloutConfig,
    createJob_documentSource,
    createJob_abortConfig,
    createJob_namespaceId,
    createJob_presignedUrlConfig,
    createJob_document,
    createJob_jobTemplateArn,
    createJob_description,
    createJob_targetSelection,
    createJob_timeoutConfig,
    createJob_tags,
    createJob_jobId,
    createJob_targets,
    createJobResponse_jobId,
    createJobResponse_jobArn,
    createJobResponse_description,
    createJobResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_indexName,
    describeIndexResponse_indexStatus,
    describeIndexResponse_schema,
    describeIndexResponse_indexName,
    describeIndexResponse_httpStatus,

    -- ** AssociateTargetsWithJob
    associateTargetsWithJob_namespaceId,
    associateTargetsWithJob_comment,
    associateTargetsWithJob_targets,
    associateTargetsWithJob_jobId,
    associateTargetsWithJobResponse_jobId,
    associateTargetsWithJobResponse_jobArn,
    associateTargetsWithJobResponse_description,
    associateTargetsWithJobResponse_httpStatus,

    -- ** AttachSecurityProfile
    attachSecurityProfile_securityProfileName,
    attachSecurityProfile_securityProfileTargetArn,
    attachSecurityProfileResponse_httpStatus,

    -- ** ListAttachedPolicies
    listAttachedPolicies_marker,
    listAttachedPolicies_recursive,
    listAttachedPolicies_pageSize,
    listAttachedPolicies_target,
    listAttachedPoliciesResponse_nextMarker,
    listAttachedPoliciesResponse_policies,
    listAttachedPoliciesResponse_httpStatus,

    -- ** CreatePolicyVersion
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyName,
    createPolicyVersion_policyDocument,
    createPolicyVersionResponse_policyDocument,
    createPolicyVersionResponse_policyVersionId,
    createPolicyVersionResponse_policyArn,
    createPolicyVersionResponse_isDefaultVersion,
    createPolicyVersionResponse_httpStatus,

    -- ** ListCACertificates
    listCACertificates_marker,
    listCACertificates_ascendingOrder,
    listCACertificates_pageSize,
    listCACertificatesResponse_certificates,
    listCACertificatesResponse_nextMarker,
    listCACertificatesResponse_httpStatus,

    -- ** DeleteTopicRule
    deleteTopicRule_ruleName,

    -- ** GetJobDocument
    getJobDocument_jobId,
    getJobDocumentResponse_document,
    getJobDocumentResponse_httpStatus,

    -- ** DescribeProvisioningTemplateVersion
    describeProvisioningTemplateVersion_templateName,
    describeProvisioningTemplateVersion_versionId,
    describeProvisioningTemplateVersionResponse_versionId,
    describeProvisioningTemplateVersionResponse_creationDate,
    describeProvisioningTemplateVersionResponse_templateBody,
    describeProvisioningTemplateVersionResponse_isDefaultVersion,
    describeProvisioningTemplateVersionResponse_httpStatus,

    -- ** ListCustomMetrics
    listCustomMetrics_nextToken,
    listCustomMetrics_maxResults,
    listCustomMetricsResponse_metricNames,
    listCustomMetricsResponse_nextToken,
    listCustomMetricsResponse_httpStatus,

    -- ** CancelAuditTask
    cancelAuditTask_taskId,
    cancelAuditTaskResponse_httpStatus,

    -- ** CreateRoleAlias
    createRoleAlias_credentialDurationSeconds,
    createRoleAlias_tags,
    createRoleAlias_roleAlias,
    createRoleAlias_roleArn,
    createRoleAliasResponse_roleAliasArn,
    createRoleAliasResponse_roleAlias,
    createRoleAliasResponse_httpStatus,

    -- ** DeleteCACertificate
    deleteCACertificate_certificateId,
    deleteCACertificateResponse_httpStatus,

    -- ** UpdateCACertificate
    updateCACertificate_removeAutoRegistration,
    updateCACertificate_newStatus,
    updateCACertificate_registrationConfig,
    updateCACertificate_newAutoRegistrationStatus,
    updateCACertificate_certificateId,

    -- ** ListTopicRules
    listTopicRules_ruleDisabled,
    listTopicRules_topic,
    listTopicRules_nextToken,
    listTopicRules_maxResults,
    listTopicRulesResponse_rules,
    listTopicRulesResponse_nextToken,
    listTopicRulesResponse_httpStatus,

    -- ** TransferCertificate
    transferCertificate_transferMessage,
    transferCertificate_certificateId,
    transferCertificate_targetAwsAccount,
    transferCertificateResponse_transferredCertificateArn,
    transferCertificateResponse_httpStatus,

    -- ** ListJobs
    listJobs_status,
    listJobs_thingGroupId,
    listJobs_namespaceId,
    listJobs_nextToken,
    listJobs_thingGroupName,
    listJobs_maxResults,
    listJobs_targetSelection,
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListRoleAliases
    listRoleAliases_marker,
    listRoleAliases_ascendingOrder,
    listRoleAliases_pageSize,
    listRoleAliasesResponse_roleAliases,
    listRoleAliasesResponse_nextMarker,
    listRoleAliasesResponse_httpStatus,

    -- ** StartOnDemandAuditTask
    startOnDemandAuditTask_targetCheckNames,
    startOnDemandAuditTaskResponse_taskId,
    startOnDemandAuditTaskResponse_httpStatus,

    -- ** DescribeThingGroup
    describeThingGroup_thingGroupName,
    describeThingGroupResponse_status,
    describeThingGroupResponse_queryVersion,
    describeThingGroupResponse_thingGroupArn,
    describeThingGroupResponse_thingGroupId,
    describeThingGroupResponse_thingGroupMetadata,
    describeThingGroupResponse_thingGroupName,
    describeThingGroupResponse_queryString,
    describeThingGroupResponse_version,
    describeThingGroupResponse_thingGroupProperties,
    describeThingGroupResponse_indexName,
    describeThingGroupResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_force,
    deleteJob_namespaceId,
    deleteJob_jobId,

    -- ** ListTargetsForSecurityProfile
    listTargetsForSecurityProfile_nextToken,
    listTargetsForSecurityProfile_maxResults,
    listTargetsForSecurityProfile_securityProfileName,
    listTargetsForSecurityProfileResponse_securityProfileTargets,
    listTargetsForSecurityProfileResponse_nextToken,
    listTargetsForSecurityProfileResponse_httpStatus,

    -- ** UpdateJob
    updateJob_jobExecutionsRolloutConfig,
    updateJob_abortConfig,
    updateJob_namespaceId,
    updateJob_presignedUrlConfig,
    updateJob_description,
    updateJob_timeoutConfig,
    updateJob_jobId,

    -- ** StartAuditMitigationActionsTask
    startAuditMitigationActionsTask_taskId,
    startAuditMitigationActionsTask_target,
    startAuditMitigationActionsTask_auditCheckToActionsMapping,
    startAuditMitigationActionsTask_clientRequestToken,
    startAuditMitigationActionsTaskResponse_taskId,
    startAuditMitigationActionsTaskResponse_httpStatus,

    -- ** DescribeDetectMitigationActionsTask
    describeDetectMitigationActionsTask_taskId,
    describeDetectMitigationActionsTaskResponse_taskSummary,
    describeDetectMitigationActionsTaskResponse_httpStatus,

    -- ** GetTopicRule
    getTopicRule_ruleName,
    getTopicRuleResponse_rule,
    getTopicRuleResponse_ruleArn,
    getTopicRuleResponse_httpStatus,

    -- ** DescribeThing
    describeThing_thingName,
    describeThingResponse_defaultClientId,
    describeThingResponse_thingTypeName,
    describeThingResponse_thingArn,
    describeThingResponse_attributes,
    describeThingResponse_version,
    describeThingResponse_thingName,
    describeThingResponse_billingGroupName,
    describeThingResponse_thingId,
    describeThingResponse_httpStatus,

    -- ** ListDomainConfigurations
    listDomainConfigurations_marker,
    listDomainConfigurations_serviceType,
    listDomainConfigurations_pageSize,
    listDomainConfigurationsResponse_domainConfigurations,
    listDomainConfigurationsResponse_nextMarker,
    listDomainConfigurationsResponse_httpStatus,

    -- ** ListAuditTasks
    listAuditTasks_taskType,
    listAuditTasks_nextToken,
    listAuditTasks_maxResults,
    listAuditTasks_taskStatus,
    listAuditTasks_startTime,
    listAuditTasks_endTime,
    listAuditTasksResponse_tasks,
    listAuditTasksResponse_nextToken,
    listAuditTasksResponse_httpStatus,

    -- ** DescribeAccountAuditConfiguration
    describeAccountAuditConfigurationResponse_auditCheckConfigurations,
    describeAccountAuditConfigurationResponse_auditNotificationTargetConfigurations,
    describeAccountAuditConfigurationResponse_roleArn,
    describeAccountAuditConfigurationResponse_httpStatus,

    -- ** DeleteDimension
    deleteDimension_name,
    deleteDimensionResponse_httpStatus,

    -- ** UpdateDimension
    updateDimension_name,
    updateDimension_stringValues,
    updateDimensionResponse_lastModifiedDate,
    updateDimensionResponse_arn,
    updateDimensionResponse_stringValues,
    updateDimensionResponse_name,
    updateDimensionResponse_creationDate,
    updateDimensionResponse_type,
    updateDimensionResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyName,

    -- ** ListThingsInThingGroup
    listThingsInThingGroup_nextToken,
    listThingsInThingGroup_recursive,
    listThingsInThingGroup_maxResults,
    listThingsInThingGroup_thingGroupName,
    listThingsInThingGroupResponse_nextToken,
    listThingsInThingGroupResponse_things,
    listThingsInThingGroupResponse_httpStatus,

    -- ** ListAuditFindings
    listAuditFindings_startTime,
    listAuditFindings_taskId,
    listAuditFindings_checkName,
    listAuditFindings_listSuppressedFindings,
    listAuditFindings_nextToken,
    listAuditFindings_endTime,
    listAuditFindings_maxResults,
    listAuditFindings_resourceIdentifier,
    listAuditFindingsResponse_nextToken,
    listAuditFindingsResponse_findings,
    listAuditFindingsResponse_httpStatus,

    -- ** DescribeScheduledAudit
    describeScheduledAudit_scheduledAuditName,
    describeScheduledAuditResponse_frequency,
    describeScheduledAuditResponse_scheduledAuditName,
    describeScheduledAuditResponse_dayOfMonth,
    describeScheduledAuditResponse_targetCheckNames,
    describeScheduledAuditResponse_dayOfWeek,
    describeScheduledAuditResponse_scheduledAuditArn,
    describeScheduledAuditResponse_httpStatus,

    -- ** CreateMitigationAction
    createMitigationAction_tags,
    createMitigationAction_actionName,
    createMitigationAction_roleArn,
    createMitigationAction_actionParams,
    createMitigationActionResponse_actionId,
    createMitigationActionResponse_actionArn,
    createMitigationActionResponse_httpStatus,

    -- ** ConfirmTopicRuleDestination
    confirmTopicRuleDestination_confirmationToken,
    confirmTopicRuleDestinationResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_marker,
    listCertificates_ascendingOrder,
    listCertificates_pageSize,
    listCertificatesResponse_certificates,
    listCertificatesResponse_nextMarker,
    listCertificatesResponse_httpStatus,

    -- ** ListMitigationActions
    listMitigationActions_nextToken,
    listMitigationActions_actionType,
    listMitigationActions_maxResults,
    listMitigationActionsResponse_actionIdentifiers,
    listMitigationActionsResponse_nextToken,
    listMitigationActionsResponse_httpStatus,

    -- ** DescribeAuthorizer
    describeAuthorizer_authorizerName,
    describeAuthorizerResponse_authorizerDescription,
    describeAuthorizerResponse_httpStatus,

    -- ** GetPolicyVersion
    getPolicyVersion_policyName,
    getPolicyVersion_policyVersionId,
    getPolicyVersionResponse_lastModifiedDate,
    getPolicyVersionResponse_policyName,
    getPolicyVersionResponse_policyDocument,
    getPolicyVersionResponse_policyVersionId,
    getPolicyVersionResponse_policyArn,
    getPolicyVersionResponse_creationDate,
    getPolicyVersionResponse_generationId,
    getPolicyVersionResponse_isDefaultVersion,
    getPolicyVersionResponse_httpStatus,

    -- ** ListActiveViolations
    listActiveViolations_nextToken,
    listActiveViolations_listSuppressedAlerts,
    listActiveViolations_behaviorCriteriaType,
    listActiveViolations_securityProfileName,
    listActiveViolations_thingName,
    listActiveViolations_verificationState,
    listActiveViolations_maxResults,
    listActiveViolationsResponse_activeViolations,
    listActiveViolationsResponse_nextToken,
    listActiveViolationsResponse_httpStatus,

    -- ** ValidateSecurityProfileBehaviors
    validateSecurityProfileBehaviors_behaviors,
    validateSecurityProfileBehaviorsResponse_validationErrors,
    validateSecurityProfileBehaviorsResponse_valid,
    validateSecurityProfileBehaviorsResponse_httpStatus,

    -- ** ListViolationEvents
    listViolationEvents_nextToken,
    listViolationEvents_listSuppressedAlerts,
    listViolationEvents_behaviorCriteriaType,
    listViolationEvents_securityProfileName,
    listViolationEvents_thingName,
    listViolationEvents_verificationState,
    listViolationEvents_maxResults,
    listViolationEvents_startTime,
    listViolationEvents_endTime,
    listViolationEventsResponse_violationEvents,
    listViolationEventsResponse_nextToken,
    listViolationEventsResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_forceDelete,
    deleteCertificate_certificateId,

    -- ** UpdateCertificate
    updateCertificate_certificateId,
    updateCertificate_newStatus,

    -- ** CreateDimension
    createDimension_tags,
    createDimension_name,
    createDimension_type,
    createDimension_stringValues,
    createDimension_clientRequestToken,
    createDimensionResponse_arn,
    createDimensionResponse_name,
    createDimensionResponse_httpStatus,

    -- ** UpdateIndexingConfiguration
    updateIndexingConfiguration_thingGroupIndexingConfiguration,
    updateIndexingConfiguration_thingIndexingConfiguration,
    updateIndexingConfigurationResponse_httpStatus,

    -- ** GetBucketsAggregation
    getBucketsAggregation_queryVersion,
    getBucketsAggregation_indexName,
    getBucketsAggregation_queryString,
    getBucketsAggregation_aggregationField,
    getBucketsAggregation_bucketsAggregationType,
    getBucketsAggregationResponse_buckets,
    getBucketsAggregationResponse_totalCount,
    getBucketsAggregationResponse_httpStatus,

    -- ** CreateProvisioningClaim
    createProvisioningClaim_templateName,
    createProvisioningClaimResponse_keyPair,
    createProvisioningClaimResponse_certificatePem,
    createProvisioningClaimResponse_certificateId,
    createProvisioningClaimResponse_expiration,
    createProvisioningClaimResponse_httpStatus,

    -- ** TestInvokeAuthorizer
    testInvokeAuthorizer_token,
    testInvokeAuthorizer_tlsContext,
    testInvokeAuthorizer_tokenSignature,
    testInvokeAuthorizer_httpContext,
    testInvokeAuthorizer_mqttContext,
    testInvokeAuthorizer_authorizerName,
    testInvokeAuthorizerResponse_policyDocuments,
    testInvokeAuthorizerResponse_principalId,
    testInvokeAuthorizerResponse_disconnectAfterInSeconds,
    testInvokeAuthorizerResponse_isAuthenticated,
    testInvokeAuthorizerResponse_refreshAfterInSeconds,
    testInvokeAuthorizerResponse_httpStatus,

    -- ** PutVerificationStateOnViolation
    putVerificationStateOnViolation_verificationStateDescription,
    putVerificationStateOnViolation_violationId,
    putVerificationStateOnViolation_verificationState,
    putVerificationStateOnViolationResponse_httpStatus,

    -- ** CreateThingGroup
    createThingGroup_parentGroupName,
    createThingGroup_thingGroupProperties,
    createThingGroup_tags,
    createThingGroup_thingGroupName,
    createThingGroupResponse_thingGroupArn,
    createThingGroupResponse_thingGroupId,
    createThingGroupResponse_thingGroupName,
    createThingGroupResponse_httpStatus,

    -- ** DescribeFleetMetric
    describeFleetMetric_metricName,
    describeFleetMetricResponse_aggregationType,
    describeFleetMetricResponse_lastModifiedDate,
    describeFleetMetricResponse_period,
    describeFleetMetricResponse_queryVersion,
    describeFleetMetricResponse_metricName,
    describeFleetMetricResponse_aggregationField,
    describeFleetMetricResponse_queryString,
    describeFleetMetricResponse_version,
    describeFleetMetricResponse_creationDate,
    describeFleetMetricResponse_description,
    describeFleetMetricResponse_unit,
    describeFleetMetricResponse_indexName,
    describeFleetMetricResponse_metricArn,
    describeFleetMetricResponse_httpStatus,

    -- ** CreateTopicRuleDestination
    createTopicRuleDestination_destinationConfiguration,
    createTopicRuleDestinationResponse_topicRuleDestination,
    createTopicRuleDestinationResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_policyName,
    detachPolicy_target,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_documentSource,
    describeJobResponse_job,
    describeJobResponse_httpStatus,

    -- ** AddThingToBillingGroup
    addThingToBillingGroup_thingArn,
    addThingToBillingGroup_billingGroupArn,
    addThingToBillingGroup_thingName,
    addThingToBillingGroup_billingGroupName,
    addThingToBillingGroupResponse_httpStatus,

    -- ** UpdateTopicRuleDestination
    updateTopicRuleDestination_arn,
    updateTopicRuleDestination_status,
    updateTopicRuleDestinationResponse_httpStatus,

    -- ** DeleteTopicRuleDestination
    deleteTopicRuleDestination_arn,
    deleteTopicRuleDestinationResponse_httpStatus,

    -- ** DeleteThingGroup
    deleteThingGroup_expectedVersion,
    deleteThingGroup_thingGroupName,
    deleteThingGroupResponse_httpStatus,

    -- ** UpdateThingGroup
    updateThingGroup_expectedVersion,
    updateThingGroup_thingGroupName,
    updateThingGroup_thingGroupProperties,
    updateThingGroupResponse_version,
    updateThingGroupResponse_httpStatus,

    -- ** ListOTAUpdates
    listOTAUpdates_nextToken,
    listOTAUpdates_otaUpdateStatus,
    listOTAUpdates_maxResults,
    listOTAUpdatesResponse_nextToken,
    listOTAUpdatesResponse_otaUpdates,
    listOTAUpdatesResponse_httpStatus,

    -- ** DeleteOTAUpdate
    deleteOTAUpdate_forceDeleteAWSJob,
    deleteOTAUpdate_deleteStream,
    deleteOTAUpdate_otaUpdateId,
    deleteOTAUpdateResponse_httpStatus,

    -- ** CreateDynamicThingGroup
    createDynamicThingGroup_queryVersion,
    createDynamicThingGroup_thingGroupProperties,
    createDynamicThingGroup_indexName,
    createDynamicThingGroup_tags,
    createDynamicThingGroup_thingGroupName,
    createDynamicThingGroup_queryString,
    createDynamicThingGroupResponse_queryVersion,
    createDynamicThingGroupResponse_thingGroupArn,
    createDynamicThingGroupResponse_thingGroupId,
    createDynamicThingGroupResponse_thingGroupName,
    createDynamicThingGroupResponse_queryString,
    createDynamicThingGroupResponse_indexName,
    createDynamicThingGroupResponse_httpStatus,

    -- ** DetachSecurityProfile
    detachSecurityProfile_securityProfileName,
    detachSecurityProfile_securityProfileTargetArn,
    detachSecurityProfileResponse_httpStatus,

    -- ** ListOutgoingCertificates
    listOutgoingCertificates_marker,
    listOutgoingCertificates_ascendingOrder,
    listOutgoingCertificates_pageSize,
    listOutgoingCertificatesResponse_nextMarker,
    listOutgoingCertificatesResponse_outgoingCertificates,
    listOutgoingCertificatesResponse_httpStatus,

    -- ** DeleteProvisioningTemplateVersion
    deleteProvisioningTemplateVersion_templateName,
    deleteProvisioningTemplateVersion_versionId,
    deleteProvisioningTemplateVersionResponse_httpStatus,

    -- ** DescribeCACertificate
    describeCACertificate_certificateId,
    describeCACertificateResponse_certificateDescription,
    describeCACertificateResponse_registrationConfig,
    describeCACertificateResponse_httpStatus,

    -- ** ListProvisioningTemplateVersions
    listProvisioningTemplateVersions_nextToken,
    listProvisioningTemplateVersions_maxResults,
    listProvisioningTemplateVersions_templateName,
    listProvisioningTemplateVersionsResponse_versions,
    listProvisioningTemplateVersionsResponse_nextToken,
    listProvisioningTemplateVersionsResponse_httpStatus,

    -- ** GetRegistrationCode
    getRegistrationCodeResponse_registrationCode,
    getRegistrationCodeResponse_httpStatus,

    -- ** ListDetectMitigationActionsExecutions
    listDetectMitigationActionsExecutions_startTime,
    listDetectMitigationActionsExecutions_taskId,
    listDetectMitigationActionsExecutions_violationId,
    listDetectMitigationActionsExecutions_nextToken,
    listDetectMitigationActionsExecutions_endTime,
    listDetectMitigationActionsExecutions_thingName,
    listDetectMitigationActionsExecutions_maxResults,
    listDetectMitigationActionsExecutionsResponse_actionsExecutions,
    listDetectMitigationActionsExecutionsResponse_nextToken,
    listDetectMitigationActionsExecutionsResponse_httpStatus,

    -- ** ListBillingGroups
    listBillingGroups_namePrefixFilter,
    listBillingGroups_nextToken,
    listBillingGroups_maxResults,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_httpStatus,

    -- ** DeleteThingType
    deleteThingType_thingTypeName,
    deleteThingTypeResponse_httpStatus,

    -- ** DeleteBillingGroup
    deleteBillingGroup_expectedVersion,
    deleteBillingGroup_billingGroupName,
    deleteBillingGroupResponse_httpStatus,

    -- ** AddThingToThingGroup
    addThingToThingGroup_thingGroupArn,
    addThingToThingGroup_thingArn,
    addThingToThingGroup_thingGroupName,
    addThingToThingGroup_overrideDynamicGroups,
    addThingToThingGroup_thingName,
    addThingToThingGroupResponse_httpStatus,

    -- ** UpdateBillingGroup
    updateBillingGroup_expectedVersion,
    updateBillingGroup_billingGroupName,
    updateBillingGroup_billingGroupProperties,
    updateBillingGroupResponse_version,
    updateBillingGroupResponse_httpStatus,

    -- ** GetTopicRuleDestination
    getTopicRuleDestination_arn,
    getTopicRuleDestinationResponse_topicRuleDestination,
    getTopicRuleDestinationResponse_httpStatus,

    -- ** ListCertificatesByCA
    listCertificatesByCA_marker,
    listCertificatesByCA_ascendingOrder,
    listCertificatesByCA_pageSize,
    listCertificatesByCA_caCertificateId,
    listCertificatesByCAResponse_certificates,
    listCertificatesByCAResponse_nextMarker,
    listCertificatesByCAResponse_httpStatus,

    -- ** UpdateAuditSuppression
    updateAuditSuppression_expirationDate,
    updateAuditSuppression_suppressIndefinitely,
    updateAuditSuppression_description,
    updateAuditSuppression_checkName,
    updateAuditSuppression_resourceIdentifier,
    updateAuditSuppressionResponse_httpStatus,

    -- ** AttachThingPrincipal
    attachThingPrincipal_thingName,
    attachThingPrincipal_principal,
    attachThingPrincipalResponse_httpStatus,

    -- ** ListThings
    listThings_usePrefixAttributeValue,
    listThings_attributeValue,
    listThings_thingTypeName,
    listThings_nextToken,
    listThings_attributeName,
    listThings_maxResults,
    listThingsResponse_nextToken,
    listThingsResponse_things,
    listThingsResponse_httpStatus,

    -- ** DeleteAuditSuppression
    deleteAuditSuppression_checkName,
    deleteAuditSuppression_resourceIdentifier,
    deleteAuditSuppressionResponse_httpStatus,

    -- ** ListDetectMitigationActionsTasks
    listDetectMitigationActionsTasks_nextToken,
    listDetectMitigationActionsTasks_maxResults,
    listDetectMitigationActionsTasks_startTime,
    listDetectMitigationActionsTasks_endTime,
    listDetectMitigationActionsTasksResponse_tasks,
    listDetectMitigationActionsTasksResponse_nextToken,
    listDetectMitigationActionsTasksResponse_httpStatus,

    -- ** RegisterThing
    registerThing_parameters,
    registerThing_templateBody,
    registerThingResponse_certificatePem,
    registerThingResponse_resourceArns,
    registerThingResponse_httpStatus,

    -- ** ListAuditSuppressions
    listAuditSuppressions_checkName,
    listAuditSuppressions_nextToken,
    listAuditSuppressions_ascendingOrder,
    listAuditSuppressions_maxResults,
    listAuditSuppressions_resourceIdentifier,
    listAuditSuppressionsResponse_nextToken,
    listAuditSuppressionsResponse_suppressions,
    listAuditSuppressionsResponse_httpStatus,

    -- ** DescribeDomainConfiguration
    describeDomainConfiguration_domainConfigurationName,
    describeDomainConfigurationResponse_domainConfigurationName,
    describeDomainConfigurationResponse_serverCertificates,
    describeDomainConfigurationResponse_authorizerConfig,
    describeDomainConfigurationResponse_lastStatusChangeDate,
    describeDomainConfigurationResponse_domainConfigurationStatus,
    describeDomainConfigurationResponse_domainName,
    describeDomainConfigurationResponse_domainConfigurationArn,
    describeDomainConfigurationResponse_serviceType,
    describeDomainConfigurationResponse_domainType,
    describeDomainConfigurationResponse_httpStatus,

    -- ** DescribeAuditTask
    describeAuditTask_taskId,
    describeAuditTaskResponse_auditDetails,
    describeAuditTaskResponse_taskType,
    describeAuditTaskResponse_taskStartTime,
    describeAuditTaskResponse_taskStatistics,
    describeAuditTaskResponse_scheduledAuditName,
    describeAuditTaskResponse_taskStatus,
    describeAuditTaskResponse_httpStatus,

    -- ** DeleteRegistrationCode
    deleteRegistrationCodeResponse_httpStatus,

    -- ** UpdateStream
    updateStream_files,
    updateStream_description,
    updateStream_roleArn,
    updateStream_streamId,
    updateStreamResponse_streamVersion,
    updateStreamResponse_streamArn,
    updateStreamResponse_description,
    updateStreamResponse_streamId,
    updateStreamResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_streamId,
    deleteStreamResponse_httpStatus,

    -- ** ListStreams
    listStreams_nextToken,
    listStreams_ascendingOrder,
    listStreams_maxResults,
    listStreamsResponse_nextToken,
    listStreamsResponse_streams,
    listStreamsResponse_httpStatus,

    -- ** CreateAuthorizer
    createAuthorizer_status,
    createAuthorizer_signingDisabled,
    createAuthorizer_tokenSigningPublicKeys,
    createAuthorizer_tokenKeyName,
    createAuthorizer_tags,
    createAuthorizer_authorizerName,
    createAuthorizer_authorizerFunctionArn,
    createAuthorizerResponse_authorizerName,
    createAuthorizerResponse_authorizerArn,
    createAuthorizerResponse_httpStatus,

    -- ** TestAuthorization
    testAuthorization_clientId,
    testAuthorization_policyNamesToAdd,
    testAuthorization_principal,
    testAuthorization_cognitoIdentityPoolId,
    testAuthorization_policyNamesToSkip,
    testAuthorization_authInfos,
    testAuthorizationResponse_authResults,
    testAuthorizationResponse_httpStatus,

    -- ** ListIndices
    listIndices_nextToken,
    listIndices_maxResults,
    listIndicesResponse_nextToken,
    listIndicesResponse_indexNames,
    listIndicesResponse_httpStatus,

    -- ** UpdateAuthorizer
    updateAuthorizer_status,
    updateAuthorizer_authorizerFunctionArn,
    updateAuthorizer_tokenSigningPublicKeys,
    updateAuthorizer_tokenKeyName,
    updateAuthorizer_authorizerName,
    updateAuthorizerResponse_authorizerName,
    updateAuthorizerResponse_authorizerArn,
    updateAuthorizerResponse_httpStatus,

    -- ** DeleteAuthorizer
    deleteAuthorizer_authorizerName,
    deleteAuthorizerResponse_httpStatus,

    -- ** CreateThing
    createThing_thingTypeName,
    createThing_attributePayload,
    createThing_billingGroupName,
    createThing_thingName,
    createThingResponse_thingArn,
    createThingResponse_thingName,
    createThingResponse_thingId,
    createThingResponse_httpStatus,

    -- ** CreateStream
    createStream_description,
    createStream_tags,
    createStream_streamId,
    createStream_files,
    createStream_roleArn,
    createStreamResponse_streamVersion,
    createStreamResponse_streamArn,
    createStreamResponse_description,
    createStreamResponse_streamId,
    createStreamResponse_httpStatus,

    -- ** CancelAuditMitigationActionsTask
    cancelAuditMitigationActionsTask_taskId,
    cancelAuditMitigationActionsTaskResponse_httpStatus,

    -- ** CreateAuditSuppression
    createAuditSuppression_expirationDate,
    createAuditSuppression_suppressIndefinitely,
    createAuditSuppression_description,
    createAuditSuppression_checkName,
    createAuditSuppression_resourceIdentifier,
    createAuditSuppression_clientRequestToken,
    createAuditSuppressionResponse_httpStatus,

    -- ** CreateBillingGroup
    createBillingGroup_billingGroupProperties,
    createBillingGroup_tags,
    createBillingGroup_billingGroupName,
    createBillingGroupResponse_billingGroupArn,
    createBillingGroupResponse_billingGroupName,
    createBillingGroupResponse_billingGroupId,
    createBillingGroupResponse_httpStatus,

    -- ** ListProvisioningTemplates
    listProvisioningTemplates_nextToken,
    listProvisioningTemplates_maxResults,
    listProvisioningTemplatesResponse_templates,
    listProvisioningTemplatesResponse_nextToken,
    listProvisioningTemplatesResponse_httpStatus,

    -- ** ListV2LoggingLevels
    listV2LoggingLevels_targetType,
    listV2LoggingLevels_nextToken,
    listV2LoggingLevels_maxResults,
    listV2LoggingLevelsResponse_logTargetConfigurations,
    listV2LoggingLevelsResponse_nextToken,
    listV2LoggingLevelsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** StopThingRegistrationTask
    stopThingRegistrationTask_taskId,
    stopThingRegistrationTaskResponse_httpStatus,

    -- ** DescribeCertificate
    describeCertificate_certificateId,
    describeCertificateResponse_certificateDescription,
    describeCertificateResponse_httpStatus,

    -- ** ListTargetsForPolicy
    listTargetsForPolicy_marker,
    listTargetsForPolicy_pageSize,
    listTargetsForPolicy_policyName,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_nextMarker,
    listTargetsForPolicyResponse_httpStatus,

    -- ** CreateJobTemplate
    createJobTemplate_jobExecutionsRolloutConfig,
    createJobTemplate_jobArn,
    createJobTemplate_documentSource,
    createJobTemplate_abortConfig,
    createJobTemplate_presignedUrlConfig,
    createJobTemplate_document,
    createJobTemplate_timeoutConfig,
    createJobTemplate_tags,
    createJobTemplate_jobTemplateId,
    createJobTemplate_description,
    createJobTemplateResponse_jobTemplateId,
    createJobTemplateResponse_jobTemplateArn,
    createJobTemplateResponse_httpStatus,

    -- ** ClearDefaultAuthorizer
    clearDefaultAuthorizerResponse_httpStatus,

    -- ** ReplaceTopicRule
    replaceTopicRule_ruleName,
    replaceTopicRule_topicRulePayload,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteFleetMetric
    deleteFleetMetric_expectedVersion,
    deleteFleetMetric_metricName,

    -- ** UpdateFleetMetric
    updateFleetMetric_aggregationType,
    updateFleetMetric_period,
    updateFleetMetric_queryVersion,
    updateFleetMetric_aggregationField,
    updateFleetMetric_expectedVersion,
    updateFleetMetric_queryString,
    updateFleetMetric_description,
    updateFleetMetric_unit,
    updateFleetMetric_metricName,
    updateFleetMetric_indexName,

    -- ** SetDefaultPolicyVersion
    setDefaultPolicyVersion_policyName,
    setDefaultPolicyVersion_policyVersionId,

    -- ** CancelJobExecution
    cancelJobExecution_force,
    cancelJobExecution_statusDetails,
    cancelJobExecution_expectedVersion,
    cancelJobExecution_jobId,
    cancelJobExecution_thingName,

    -- ** ListPolicyVersions
    listPolicyVersions_policyName,
    listPolicyVersionsResponse_policyVersions,
    listPolicyVersionsResponse_httpStatus,

    -- ** SetV2LoggingLevel
    setV2LoggingLevel_logTarget,
    setV2LoggingLevel_logLevel,

    -- ** ListJobExecutionsForThing
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_thingName,
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_policyName,
    attachPolicy_target,

    -- ** CreateKeysAndCertificate
    createKeysAndCertificate_setAsActive,
    createKeysAndCertificateResponse_keyPair,
    createKeysAndCertificateResponse_certificatePem,
    createKeysAndCertificateResponse_certificateArn,
    createKeysAndCertificateResponse_certificateId,
    createKeysAndCertificateResponse_httpStatus,

    -- ** ListThingsInBillingGroup
    listThingsInBillingGroup_nextToken,
    listThingsInBillingGroup_maxResults,
    listThingsInBillingGroup_billingGroupName,
    listThingsInBillingGroupResponse_nextToken,
    listThingsInBillingGroupResponse_things,
    listThingsInBillingGroupResponse_httpStatus,

    -- ** UpdateThingGroupsForThing
    updateThingGroupsForThing_thingGroupsToAdd,
    updateThingGroupsForThing_thingGroupsToRemove,
    updateThingGroupsForThing_overrideDynamicGroups,
    updateThingGroupsForThing_thingName,
    updateThingGroupsForThingResponse_httpStatus,

    -- ** CreateFleetMetric
    createFleetMetric_queryVersion,
    createFleetMetric_description,
    createFleetMetric_unit,
    createFleetMetric_indexName,
    createFleetMetric_tags,
    createFleetMetric_metricName,
    createFleetMetric_queryString,
    createFleetMetric_aggregationType,
    createFleetMetric_period,
    createFleetMetric_aggregationField,
    createFleetMetricResponse_metricName,
    createFleetMetricResponse_metricArn,
    createFleetMetricResponse_httpStatus,

    -- ** EnableTopicRule
    enableTopicRule_ruleName,

    -- ** DeleteJobTemplate
    deleteJobTemplate_jobTemplateId,

    -- ** AcceptCertificateTransfer
    acceptCertificateTransfer_setAsActive,
    acceptCertificateTransfer_certificateId,

    -- ** GetPercentiles
    getPercentiles_percents,
    getPercentiles_queryVersion,
    getPercentiles_aggregationField,
    getPercentiles_indexName,
    getPercentiles_queryString,
    getPercentilesResponse_percentiles,
    getPercentilesResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_policyName,
    getPolicyResponse_lastModifiedDate,
    getPolicyResponse_policyName,
    getPolicyResponse_policyDocument,
    getPolicyResponse_defaultVersionId,
    getPolicyResponse_policyArn,
    getPolicyResponse_creationDate,
    getPolicyResponse_generationId,
    getPolicyResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_nextToken,
    listJobTemplates_maxResults,
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointType,
    describeEndpointResponse_endpointAddress,
    describeEndpointResponse_httpStatus,

    -- ** ListSecurityProfilesForTarget
    listSecurityProfilesForTarget_nextToken,
    listSecurityProfilesForTarget_recursive,
    listSecurityProfilesForTarget_maxResults,
    listSecurityProfilesForTarget_securityProfileTargetArn,
    listSecurityProfilesForTargetResponse_nextToken,
    listSecurityProfilesForTargetResponse_securityProfileTargetMappings,
    listSecurityProfilesForTargetResponse_httpStatus,

    -- ** UpdateEventConfigurations
    updateEventConfigurations_eventConfigurations,
    updateEventConfigurationsResponse_httpStatus,

    -- ** UpdateCustomMetric
    updateCustomMetric_metricName,
    updateCustomMetric_displayName,
    updateCustomMetricResponse_metricType,
    updateCustomMetricResponse_lastModifiedDate,
    updateCustomMetricResponse_metricName,
    updateCustomMetricResponse_displayName,
    updateCustomMetricResponse_creationDate,
    updateCustomMetricResponse_metricArn,
    updateCustomMetricResponse_httpStatus,

    -- ** DeleteCustomMetric
    deleteCustomMetric_metricName,
    deleteCustomMetricResponse_httpStatus,

    -- ** RegisterCACertificate
    registerCACertificate_setAsActive,
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_registrationConfig,
    registerCACertificate_tags,
    registerCACertificate_caCertificate,
    registerCACertificate_verificationCertificate,
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,

    -- ** DeleteDomainConfiguration
    deleteDomainConfiguration_domainConfigurationName,
    deleteDomainConfigurationResponse_httpStatus,

    -- ** UpdateDomainConfiguration
    updateDomainConfiguration_authorizerConfig,
    updateDomainConfiguration_domainConfigurationStatus,
    updateDomainConfiguration_removeAuthorizerConfig,
    updateDomainConfiguration_domainConfigurationName,
    updateDomainConfigurationResponse_domainConfigurationName,
    updateDomainConfigurationResponse_domainConfigurationArn,
    updateDomainConfigurationResponse_httpStatus,

    -- ** SetLoggingOptions
    setLoggingOptions_loggingOptionsPayload,

    -- ** DescribeThingType
    describeThingType_thingTypeName,
    describeThingTypeResponse_thingTypeProperties,
    describeThingTypeResponse_thingTypeName,
    describeThingTypeResponse_thingTypeId,
    describeThingTypeResponse_thingTypeMetadata,
    describeThingTypeResponse_thingTypeArn,
    describeThingTypeResponse_httpStatus,

    -- ** ListDimensions
    listDimensions_nextToken,
    listDimensions_maxResults,
    listDimensionsResponse_nextToken,
    listDimensionsResponse_dimensionNames,
    listDimensionsResponse_httpStatus,

    -- ** GetV2LoggingOptions
    getV2LoggingOptionsResponse_disableAllLogs,
    getV2LoggingOptionsResponse_defaultLogLevel,
    getV2LoggingOptionsResponse_roleArn,
    getV2LoggingOptionsResponse_httpStatus,

    -- ** ListThingRegistrationTasks
    listThingRegistrationTasks_status,
    listThingRegistrationTasks_nextToken,
    listThingRegistrationTasks_maxResults,
    listThingRegistrationTasksResponse_nextToken,
    listThingRegistrationTasksResponse_taskIds,
    listThingRegistrationTasksResponse_httpStatus,

    -- ** RejectCertificateTransfer
    rejectCertificateTransfer_rejectReason,
    rejectCertificateTransfer_certificateId,

    -- ** DescribeAuditSuppression
    describeAuditSuppression_checkName,
    describeAuditSuppression_resourceIdentifier,
    describeAuditSuppressionResponse_checkName,
    describeAuditSuppressionResponse_expirationDate,
    describeAuditSuppressionResponse_suppressIndefinitely,
    describeAuditSuppressionResponse_description,
    describeAuditSuppressionResponse_resourceIdentifier,
    describeAuditSuppressionResponse_httpStatus,

    -- ** DescribeStream
    describeStream_streamId,
    describeStreamResponse_streamInfo,
    describeStreamResponse_httpStatus,

    -- ** CreateSecurityProfile
    createSecurityProfile_alertTargets,
    createSecurityProfile_additionalMetricsToRetainV2,
    createSecurityProfile_behaviors,
    createSecurityProfile_additionalMetricsToRetain,
    createSecurityProfile_securityProfileDescription,
    createSecurityProfile_tags,
    createSecurityProfile_securityProfileName,
    createSecurityProfileResponse_securityProfileName,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_httpStatus,

    -- ** DescribeBillingGroup
    describeBillingGroup_billingGroupName,
    describeBillingGroupResponse_billingGroupArn,
    describeBillingGroupResponse_version,
    describeBillingGroupResponse_billingGroupProperties,
    describeBillingGroupResponse_billingGroupName,
    describeBillingGroupResponse_billingGroupId,
    describeBillingGroupResponse_billingGroupMetadata,
    describeBillingGroupResponse_httpStatus,

    -- ** DetachThingPrincipal
    detachThingPrincipal_thingName,
    detachThingPrincipal_principal,
    detachThingPrincipalResponse_httpStatus,

    -- ** CancelJob
    cancelJob_force,
    cancelJob_reasonCode,
    cancelJob_comment,
    cancelJob_jobId,
    cancelJobResponse_jobId,
    cancelJobResponse_jobArn,
    cancelJobResponse_description,
    cancelJobResponse_httpStatus,

    -- ** DeprecateThingType
    deprecateThingType_undoDeprecate,
    deprecateThingType_thingTypeName,
    deprecateThingTypeResponse_httpStatus,

    -- * Types

    -- ** AbortConfig
    abortConfig_criteriaList,

    -- ** AbortCriteria
    abortCriteria_failureType,
    abortCriteria_action,
    abortCriteria_thresholdPercentage,
    abortCriteria_minNumberOfExecutedThings,

    -- ** Action
    action_cloudwatchMetric,
    action_cloudwatchLogs,
    action_dynamoDBv2,
    action_stepFunctions,
    action_cloudwatchAlarm,
    action_sns,
    action_dynamoDB,
    action_firehose,
    action_timestream,
    action_iotSiteWise,
    action_iotAnalytics,
    action_lambda,
    action_openSearch,
    action_iotEvents,
    action_salesforce,
    action_kinesis,
    action_s3,
    action_http,
    action_elasticsearch,
    action_kafka,
    action_republish,
    action_sqs,

    -- ** ActiveViolation
    activeViolation_verificationStateDescription,
    activeViolation_lastViolationValue,
    activeViolation_lastViolationTime,
    activeViolation_violationStartTime,
    activeViolation_violationId,
    activeViolation_behavior,
    activeViolation_securityProfileName,
    activeViolation_violationEventAdditionalInfo,
    activeViolation_thingName,
    activeViolation_verificationState,

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
    assetPropertyVariant_integerValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_stringValue,
    assetPropertyVariant_booleanValue,

    -- ** AttributePayload
    attributePayload_attributes,
    attributePayload_merge,

    -- ** AuditCheckConfiguration
    auditCheckConfiguration_enabled,

    -- ** AuditCheckDetails
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_totalResourcesCount,
    auditCheckDetails_checkCompliant,
    auditCheckDetails_nonCompliantResourcesCount,
    auditCheckDetails_errorCode,
    auditCheckDetails_message,
    auditCheckDetails_checkRunStatus,

    -- ** AuditFinding
    auditFinding_isSuppressed,
    auditFinding_taskId,
    auditFinding_findingTime,
    auditFinding_taskStartTime,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_severity,
    auditFinding_relatedResources,
    auditFinding_checkName,
    auditFinding_nonCompliantResource,
    auditFinding_reasonForNonCompliance,
    auditFinding_findingId,

    -- ** AuditMitigationActionExecutionMetadata
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_taskId,
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_message,

    -- ** AuditMitigationActionsTaskMetadata
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- ** AuditMitigationActionsTaskTarget
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_findingIds,
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,

    -- ** AuditNotificationTarget
    auditNotificationTarget_targetArn,
    auditNotificationTarget_enabled,
    auditNotificationTarget_roleArn,

    -- ** AuditSuppression
    auditSuppression_expirationDate,
    auditSuppression_suppressIndefinitely,
    auditSuppression_description,
    auditSuppression_checkName,
    auditSuppression_resourceIdentifier,

    -- ** AuditTaskMetadata
    auditTaskMetadata_taskType,
    auditTaskMetadata_taskId,
    auditTaskMetadata_taskStatus,

    -- ** AuthInfo
    authInfo_actionType,
    authInfo_resources,

    -- ** AuthResult
    authResult_denied,
    authResult_authDecision,
    authResult_allowed,
    authResult_missingContextValues,
    authResult_authInfo,

    -- ** AuthorizerConfig
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- ** AuthorizerDescription
    authorizerDescription_status,
    authorizerDescription_lastModifiedDate,
    authorizerDescription_signingDisabled,
    authorizerDescription_authorizerName,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_authorizerArn,
    authorizerDescription_creationDate,
    authorizerDescription_tokenSigningPublicKeys,
    authorizerDescription_tokenKeyName,

    -- ** AuthorizerSummary
    authorizerSummary_authorizerName,
    authorizerSummary_authorizerArn,

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
    behavior_suppressAlerts,
    behavior_metricDimension,
    behavior_metric,
    behavior_criteria,
    behavior_name,

    -- ** BehaviorCriteria
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_value,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_durationSeconds,
    behaviorCriteria_consecutiveDatapointsToClear,

    -- ** BehaviorModelTrainingSummary
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_securityProfileName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,
    behaviorModelTrainingSummary_modelStatus,

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
    cACertificate_certificateId,
    cACertificate_creationDate,

    -- ** CACertificateDescription
    cACertificateDescription_status,
    cACertificateDescription_ownedBy,
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_certificatePem,
    cACertificateDescription_certificateArn,
    cACertificateDescription_certificateId,
    cACertificateDescription_validity,
    cACertificateDescription_autoRegistrationStatus,
    cACertificateDescription_creationDate,
    cACertificateDescription_generationId,
    cACertificateDescription_customerVersion,

    -- ** Certificate
    certificate_status,
    certificate_certificateArn,
    certificate_certificateId,
    certificate_certificateMode,
    certificate_creationDate,

    -- ** CertificateDescription
    certificateDescription_status,
    certificateDescription_ownedBy,
    certificateDescription_lastModifiedDate,
    certificateDescription_caCertificateId,
    certificateDescription_previousOwnedBy,
    certificateDescription_certificatePem,
    certificateDescription_certificateArn,
    certificateDescription_certificateId,
    certificateDescription_certificateMode,
    certificateDescription_validity,
    certificateDescription_creationDate,
    certificateDescription_generationId,
    certificateDescription_transferData,
    certificateDescription_customerVersion,

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
    codeSigning_customCodeSigning,
    codeSigning_startSigningJobParameter,
    codeSigning_awsSignerJobId,

    -- ** CodeSigningCertificateChain
    codeSigningCertificateChain_certificateName,
    codeSigningCertificateChain_inlineDocument,

    -- ** CodeSigningSignature
    codeSigningSignature_inlineDocument,

    -- ** Configuration
    configuration_enabled,

    -- ** CustomCodeSigning
    customCodeSigning_signature,
    customCodeSigning_hashAlgorithm,
    customCodeSigning_certificateChain,
    customCodeSigning_signatureAlgorithm,

    -- ** Denied
    denied_implicitDeny,
    denied_explicitDeny,

    -- ** Destination
    destination_s3Destination,

    -- ** DetectMitigationActionExecution
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_violationId,
    detectMitigationActionExecution_errorCode,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_executionEndDate,

    -- ** DetectMitigationActionsTaskStatistics
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,
    detectMitigationActionsTaskStatistics_actionsExecuted,

    -- ** DetectMitigationActionsTaskSummary
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskStatus,

    -- ** DetectMitigationActionsTaskTarget
    detectMitigationActionsTaskTarget_violationIds,
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,

    -- ** DomainConfigurationSummary
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_serviceType,

    -- ** DynamoDBAction
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyValue,
    dynamoDBAction_tableName,
    dynamoDBAction_roleArn,
    dynamoDBAction_hashKeyField,
    dynamoDBAction_hashKeyValue,

    -- ** DynamoDBv2Action
    dynamoDBv2Action_roleArn,
    dynamoDBv2Action_putItem,

    -- ** EffectivePolicy
    effectivePolicy_policyName,
    effectivePolicy_policyDocument,
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
    fileLocation_stream,
    fileLocation_s3Location,

    -- ** FirehoseAction
    firehoseAction_batchMode,
    firehoseAction_separator,
    firehoseAction_roleArn,
    firehoseAction_deliveryStreamName,

    -- ** FleetMetricNameAndArn
    fleetMetricNameAndArn_metricName,
    fleetMetricNameAndArn_metricArn,

    -- ** GroupNameAndArn
    groupNameAndArn_groupArn,
    groupNameAndArn_groupName,

    -- ** HttpAction
    httpAction_confirmationUrl,
    httpAction_auth,
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

    -- ** Job
    job_status,
    job_jobExecutionsRolloutConfig,
    job_jobId,
    job_lastUpdatedAt,
    job_jobArn,
    job_createdAt,
    job_abortConfig,
    job_jobProcessDetails,
    job_namespaceId,
    job_reasonCode,
    job_presignedUrlConfig,
    job_forceCanceled,
    job_jobTemplateArn,
    job_targets,
    job_completedAt,
    job_comment,
    job_description,
    job_targetSelection,
    job_timeoutConfig,

    -- ** JobExecution
    jobExecution_status,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_queuedAt,
    jobExecution_statusDetails,
    jobExecution_thingArn,
    jobExecution_executionNumber,
    jobExecution_versionNumber,
    jobExecution_startedAt,
    jobExecution_forceCanceled,

    -- ** JobExecutionStatusDetails
    jobExecutionStatusDetails_detailsMap,

    -- ** JobExecutionSummary
    jobExecutionSummary_status,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_startedAt,

    -- ** JobExecutionSummaryForJob
    jobExecutionSummaryForJob_jobExecutionSummary,
    jobExecutionSummaryForJob_thingArn,

    -- ** JobExecutionSummaryForThing
    jobExecutionSummaryForThing_jobId,
    jobExecutionSummaryForThing_jobExecutionSummary,

    -- ** JobExecutionsRolloutConfig
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- ** JobProcessDetails
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfCanceledThings,
    jobProcessDetails_numberOfTimedOutThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_processingTargets,

    -- ** JobSummary
    jobSummary_status,
    jobSummary_jobId,
    jobSummary_lastUpdatedAt,
    jobSummary_jobArn,
    jobSummary_createdAt,
    jobSummary_thingGroupId,
    jobSummary_completedAt,
    jobSummary_targetSelection,

    -- ** JobTemplateSummary
    jobTemplateSummary_createdAt,
    jobTemplateSummary_jobTemplateId,
    jobTemplateSummary_jobTemplateArn,
    jobTemplateSummary_description,

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
    metricValue_cidrs,
    metricValue_count,
    metricValue_ports,
    metricValue_numbers,
    metricValue_number,
    metricValue_strings,

    -- ** MitigationAction
    mitigationAction_actionParams,
    mitigationAction_name,
    mitigationAction_id,
    mitigationAction_roleArn,

    -- ** MitigationActionIdentifier
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_creationDate,
    mitigationActionIdentifier_actionArn,

    -- ** MitigationActionParams
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_addThingsToThingGroupParams,
    mitigationActionParams_updateCACertificateParams,
    mitigationActionParams_updateDeviceCertificateParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_publishFindingToSnsParams,

    -- ** MqttContext
    mqttContext_clientId,
    mqttContext_username,
    mqttContext_password,

    -- ** NonCompliantResource
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceType,
    nonCompliantResource_resourceIdentifier,

    -- ** OTAUpdateFile
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_fileType,
    oTAUpdateFile_fileVersion,
    oTAUpdateFile_attributes,
    oTAUpdateFile_codeSigning,
    oTAUpdateFile_fileName,

    -- ** OTAUpdateInfo
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_targets,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_description,
    oTAUpdateInfo_targetSelection,

    -- ** OTAUpdateSummary
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateId,
    oTAUpdateSummary_otaUpdateArn,

    -- ** OpenSearchAction
    openSearchAction_roleArn,
    openSearchAction_endpoint,
    openSearchAction_index,
    openSearchAction_type,
    openSearchAction_id,

    -- ** OutgoingCertificate
    outgoingCertificate_transferDate,
    outgoingCertificate_certificateArn,
    outgoingCertificate_certificateId,
    outgoingCertificate_transferredTo,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferMessage,

    -- ** PercentPair
    percentPair_value,
    percentPair_percent,

    -- ** Policy
    policy_policyName,
    policy_policyArn,

    -- ** PolicyVersion
    policyVersion_versionId,
    policyVersion_createDate,
    policyVersion_isDefaultVersion,

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
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_templateArn,
    provisioningTemplateSummary_description,

    -- ** ProvisioningTemplateVersionSummary
    provisioningTemplateVersionSummary_versionId,
    provisioningTemplateVersionSummary_creationDate,
    provisioningTemplateVersionSummary_isDefaultVersion,

    -- ** PublishFindingToSnsParams
    publishFindingToSnsParams_topicArn,

    -- ** PutAssetPropertyValueEntry
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_propertyValues,

    -- ** PutItemInput
    putItemInput_tableName,

    -- ** RateIncreaseCriteria
    rateIncreaseCriteria_numberOfNotifiedThings,
    rateIncreaseCriteria_numberOfSucceededThings,

    -- ** RegistrationConfig
    registrationConfig_templateBody,
    registrationConfig_roleArn,

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
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_clientId,
    resourceIdentifier_roleAliasArn,
    resourceIdentifier_caCertificateId,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_account,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_cognitoIdentityPoolId,

    -- ** RoleAliasDescription
    roleAliasDescription_roleAliasArn,
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_roleAlias,
    roleAliasDescription_owner,
    roleAliasDescription_creationDate,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_roleArn,

    -- ** S3Action
    s3Action_cannedAcl,
    s3Action_roleArn,
    s3Action_bucketName,
    s3Action_key,

    -- ** S3Destination
    s3Destination_prefix,
    s3Destination_bucket,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,
    s3Location_version,

    -- ** SalesforceAction
    salesforceAction_token,
    salesforceAction_url,

    -- ** ScheduledAuditMetadata
    scheduledAuditMetadata_frequency,
    scheduledAuditMetadata_scheduledAuditName,
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_scheduledAuditArn,

    -- ** SecurityProfileIdentifier
    securityProfileIdentifier_name,
    securityProfileIdentifier_arn,

    -- ** SecurityProfileTarget
    securityProfileTarget_arn,

    -- ** SecurityProfileTargetMapping
    securityProfileTargetMapping_securityProfileIdentifier,
    securityProfileTargetMapping_target,

    -- ** ServerCertificateSummary
    serverCertificateSummary_serverCertificateStatusDetail,
    serverCertificateSummary_serverCertificateStatus,
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
    startSigningJobParameter_destination,
    startSigningJobParameter_signingProfileName,
    startSigningJobParameter_signingProfileParameter,

    -- ** StatisticalThreshold
    statisticalThreshold_statistic,

    -- ** Statistics
    statistics_stdDeviation,
    statistics_maximum,
    statistics_average,
    statistics_count,
    statistics_minimum,
    statistics_variance,
    statistics_sumOfSquares,
    statistics_sum,

    -- ** StepFunctionsAction
    stepFunctionsAction_executionNamePrefix,
    stepFunctionsAction_stateMachineName,
    stepFunctionsAction_roleArn,

    -- ** Stream
    stream_fileId,
    stream_streamId,

    -- ** StreamFile
    streamFile_s3Location,
    streamFile_fileId,

    -- ** StreamInfo
    streamInfo_lastUpdatedAt,
    streamInfo_createdAt,
    streamInfo_streamVersion,
    streamInfo_streamArn,
    streamInfo_files,
    streamInfo_description,
    streamInfo_streamId,
    streamInfo_roleArn,

    -- ** StreamSummary
    streamSummary_streamVersion,
    streamSummary_streamArn,
    streamSummary_description,
    streamSummary_streamId,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TaskStatistics
    taskStatistics_nonCompliantChecks,
    taskStatistics_waitingForDataCollectionChecks,
    taskStatistics_failedChecks,
    taskStatistics_totalChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_compliantChecks,
    taskStatistics_canceledChecks,

    -- ** TaskStatisticsForAuditCheck
    taskStatisticsForAuditCheck_canceledFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_succeededFindingsCount,

    -- ** TermsAggregation
    termsAggregation_maxBuckets,

    -- ** ThingAttribute
    thingAttribute_thingTypeName,
    thingAttribute_thingArn,
    thingAttribute_attributes,
    thingAttribute_version,
    thingAttribute_thingName,

    -- ** ThingConnectivity
    thingConnectivity_disconnectReason,
    thingConnectivity_connected,
    thingConnectivity_timestamp,

    -- ** ThingDocument
    thingDocument_thingGroupNames,
    thingDocument_thingTypeName,
    thingDocument_shadow,
    thingDocument_attributes,
    thingDocument_connectivity,
    thingDocument_thingName,
    thingDocument_thingId,

    -- ** ThingGroupDocument
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupName,
    thingGroupDocument_attributes,
    thingGroupDocument_thingGroupDescription,

    -- ** ThingGroupIndexingConfiguration
    thingGroupIndexingConfiguration_managedFields,
    thingGroupIndexingConfiguration_customFields,
    thingGroupIndexingConfiguration_thingGroupIndexingMode,

    -- ** ThingGroupMetadata
    thingGroupMetadata_rootToParentThingGroups,
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_creationDate,

    -- ** ThingGroupProperties
    thingGroupProperties_attributePayload,
    thingGroupProperties_thingGroupDescription,

    -- ** ThingIndexingConfiguration
    thingIndexingConfiguration_managedFields,
    thingIndexingConfiguration_thingConnectivityIndexingMode,
    thingIndexingConfiguration_customFields,
    thingIndexingConfiguration_thingIndexingMode,

    -- ** ThingTypeDefinition
    thingTypeDefinition_thingTypeProperties,
    thingTypeDefinition_thingTypeName,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeArn,

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
    topicRule_createdAt,
    topicRule_actions,
    topicRule_awsIotSqlVersion,
    topicRule_errorAction,
    topicRule_ruleDisabled,
    topicRule_ruleName,
    topicRule_sql,
    topicRule_description,

    -- ** TopicRuleDestination
    topicRuleDestination_vpcProperties,
    topicRuleDestination_status,
    topicRuleDestination_lastUpdatedAt,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_arn,
    topicRuleDestination_createdAt,
    topicRuleDestination_statusReason,

    -- ** TopicRuleDestinationConfiguration
    topicRuleDestinationConfiguration_vpcConfiguration,
    topicRuleDestinationConfiguration_httpUrlConfiguration,

    -- ** TopicRuleDestinationSummary
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_lastUpdatedAt,
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_vpcDestinationSummary,
    topicRuleDestinationSummary_statusReason,

    -- ** TopicRuleListItem
    topicRuleListItem_createdAt,
    topicRuleListItem_ruleDisabled,
    topicRuleListItem_ruleName,
    topicRuleListItem_ruleArn,
    topicRuleListItem_topicPattern,

    -- ** TopicRulePayload
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_errorAction,
    topicRulePayload_ruleDisabled,
    topicRulePayload_description,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- ** TransferData
    transferData_transferDate,
    transferData_acceptDate,
    transferData_transferMessage,
    transferData_rejectDate,
    transferData_rejectReason,

    -- ** UpdateCACertificateParams
    updateCACertificateParams_action,

    -- ** UpdateDeviceCertificateParams
    updateDeviceCertificateParams_action,

    -- ** ValidationError
    validationError_errorMessage,

    -- ** ViolationEvent
    violationEvent_violationEventType,
    violationEvent_verificationStateDescription,
    violationEvent_violationId,
    violationEvent_behavior,
    violationEvent_metricValue,
    violationEvent_securityProfileName,
    violationEvent_violationEventAdditionalInfo,
    violationEvent_violationEventTime,
    violationEvent_thingName,
    violationEvent_verificationState,

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
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_vpcId,
    vpcDestinationProperties_roleArn,

    -- ** VpcDestinationSummary
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_vpcId,
    vpcDestinationSummary_roleArn,
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

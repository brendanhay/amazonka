{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Lens
  ( -- * Operations

    -- ** AcceptAdministratorInvitation
    acceptAdministratorInvitation_administratorId,
    acceptAdministratorInvitation_invitationId,
    acceptAdministratorInvitationResponse_httpStatus,

    -- ** BatchDisableStandards
    batchDisableStandards_standardsSubscriptionArns,
    batchDisableStandardsResponse_standardsSubscriptions,
    batchDisableStandardsResponse_httpStatus,

    -- ** BatchEnableStandards
    batchEnableStandards_standardsSubscriptionRequests,
    batchEnableStandardsResponse_standardsSubscriptions,
    batchEnableStandardsResponse_httpStatus,

    -- ** BatchImportFindings
    batchImportFindings_findings,
    batchImportFindingsResponse_failedFindings,
    batchImportFindingsResponse_httpStatus,
    batchImportFindingsResponse_failedCount,
    batchImportFindingsResponse_successCount,

    -- ** BatchUpdateFindings
    batchUpdateFindings_criticality,
    batchUpdateFindings_severity,
    batchUpdateFindings_relatedFindings,
    batchUpdateFindings_confidence,
    batchUpdateFindings_userDefinedFields,
    batchUpdateFindings_types,
    batchUpdateFindings_verificationState,
    batchUpdateFindings_workflow,
    batchUpdateFindings_note,
    batchUpdateFindings_findingIdentifiers,
    batchUpdateFindingsResponse_httpStatus,
    batchUpdateFindingsResponse_processedFindings,
    batchUpdateFindingsResponse_unprocessedFindings,

    -- ** CreateActionTarget
    createActionTarget_name,
    createActionTarget_description,
    createActionTarget_id,
    createActionTargetResponse_httpStatus,
    createActionTargetResponse_actionTargetArn,

    -- ** CreateFindingAggregator
    createFindingAggregator_regions,
    createFindingAggregator_regionLinkingMode,
    createFindingAggregatorResponse_regions,
    createFindingAggregatorResponse_findingAggregatorArn,
    createFindingAggregatorResponse_regionLinkingMode,
    createFindingAggregatorResponse_findingAggregationRegion,
    createFindingAggregatorResponse_httpStatus,

    -- ** CreateInsight
    createInsight_name,
    createInsight_filters,
    createInsight_groupByAttribute,
    createInsightResponse_httpStatus,
    createInsightResponse_insightArn,

    -- ** CreateMembers
    createMembers_accountDetails,
    createMembersResponse_unprocessedAccounts,
    createMembersResponse_httpStatus,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_unprocessedAccounts,
    declineInvitationsResponse_httpStatus,

    -- ** DeleteActionTarget
    deleteActionTarget_actionTargetArn,
    deleteActionTargetResponse_httpStatus,
    deleteActionTargetResponse_actionTargetArn,

    -- ** DeleteFindingAggregator
    deleteFindingAggregator_findingAggregatorArn,
    deleteFindingAggregatorResponse_httpStatus,

    -- ** DeleteInsight
    deleteInsight_insightArn,
    deleteInsightResponse_httpStatus,
    deleteInsightResponse_insightArn,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_unprocessedAccounts,
    deleteInvitationsResponse_httpStatus,

    -- ** DeleteMembers
    deleteMembers_accountIds,
    deleteMembersResponse_unprocessedAccounts,
    deleteMembersResponse_httpStatus,

    -- ** DescribeActionTargets
    describeActionTargets_nextToken,
    describeActionTargets_actionTargetArns,
    describeActionTargets_maxResults,
    describeActionTargetsResponse_nextToken,
    describeActionTargetsResponse_httpStatus,
    describeActionTargetsResponse_actionTargets,

    -- ** DescribeHub
    describeHub_hubArn,
    describeHubResponse_hubArn,
    describeHubResponse_autoEnableControls,
    describeHubResponse_subscribedAt,
    describeHubResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,
    describeOrganizationConfigurationResponse_autoEnableStandards,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** DescribeProducts
    describeProducts_nextToken,
    describeProducts_maxResults,
    describeProducts_productArn,
    describeProductsResponse_nextToken,
    describeProductsResponse_httpStatus,
    describeProductsResponse_products,

    -- ** DescribeStandards
    describeStandards_nextToken,
    describeStandards_maxResults,
    describeStandardsResponse_nextToken,
    describeStandardsResponse_standards,
    describeStandardsResponse_httpStatus,

    -- ** DescribeStandardsControls
    describeStandardsControls_nextToken,
    describeStandardsControls_maxResults,
    describeStandardsControls_standardsSubscriptionArn,
    describeStandardsControlsResponse_nextToken,
    describeStandardsControlsResponse_controls,
    describeStandardsControlsResponse_httpStatus,

    -- ** DisableImportFindingsForProduct
    disableImportFindingsForProduct_productSubscriptionArn,
    disableImportFindingsForProductResponse_httpStatus,

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- ** DisableSecurityHub
    disableSecurityHubResponse_httpStatus,

    -- ** DisassociateFromAdministratorAccount
    disassociateFromAdministratorAccountResponse_httpStatus,

    -- ** DisassociateMembers
    disassociateMembers_accountIds,
    disassociateMembersResponse_httpStatus,

    -- ** EnableImportFindingsForProduct
    enableImportFindingsForProduct_productArn,
    enableImportFindingsForProductResponse_productSubscriptionArn,
    enableImportFindingsForProductResponse_httpStatus,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** EnableSecurityHub
    enableSecurityHub_tags,
    enableSecurityHub_enableDefaultStandards,
    enableSecurityHubResponse_httpStatus,

    -- ** GetAdministratorAccount
    getAdministratorAccountResponse_administrator,
    getAdministratorAccountResponse_httpStatus,

    -- ** GetEnabledStandards
    getEnabledStandards_nextToken,
    getEnabledStandards_maxResults,
    getEnabledStandards_standardsSubscriptionArns,
    getEnabledStandardsResponse_standardsSubscriptions,
    getEnabledStandardsResponse_nextToken,
    getEnabledStandardsResponse_httpStatus,

    -- ** GetFindingAggregator
    getFindingAggregator_findingAggregatorArn,
    getFindingAggregatorResponse_regions,
    getFindingAggregatorResponse_findingAggregatorArn,
    getFindingAggregatorResponse_regionLinkingMode,
    getFindingAggregatorResponse_findingAggregationRegion,
    getFindingAggregatorResponse_httpStatus,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_nextToken,
    getFindings_filters,
    getFindings_maxResults,
    getFindingsResponse_nextToken,
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,

    -- ** GetInsightResults
    getInsightResults_insightArn,
    getInsightResultsResponse_httpStatus,
    getInsightResultsResponse_insightResults,

    -- ** GetInsights
    getInsights_nextToken,
    getInsights_maxResults,
    getInsights_insightArns,
    getInsightsResponse_nextToken,
    getInsightsResponse_httpStatus,
    getInsightsResponse_insights,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** GetMembers
    getMembers_accountIds,
    getMembersResponse_unprocessedAccounts,
    getMembersResponse_members,
    getMembersResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_accountIds,
    inviteMembersResponse_unprocessedAccounts,
    inviteMembersResponse_httpStatus,

    -- ** ListEnabledProductsForImport
    listEnabledProductsForImport_nextToken,
    listEnabledProductsForImport_maxResults,
    listEnabledProductsForImportResponse_nextToken,
    listEnabledProductsForImportResponse_productSubscriptions,
    listEnabledProductsForImportResponse_httpStatus,

    -- ** ListFindingAggregators
    listFindingAggregators_nextToken,
    listFindingAggregators_maxResults,
    listFindingAggregatorsResponse_findingAggregators,
    listFindingAggregatorsResponse_nextToken,
    listFindingAggregatorsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_maxResults,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateActionTarget
    updateActionTarget_name,
    updateActionTarget_description,
    updateActionTarget_actionTargetArn,
    updateActionTargetResponse_httpStatus,

    -- ** UpdateFindingAggregator
    updateFindingAggregator_regions,
    updateFindingAggregator_findingAggregatorArn,
    updateFindingAggregator_regionLinkingMode,
    updateFindingAggregatorResponse_regions,
    updateFindingAggregatorResponse_findingAggregatorArn,
    updateFindingAggregatorResponse_regionLinkingMode,
    updateFindingAggregatorResponse_findingAggregationRegion,
    updateFindingAggregatorResponse_httpStatus,

    -- ** UpdateFindings
    updateFindings_recordState,
    updateFindings_note,
    updateFindings_filters,
    updateFindingsResponse_httpStatus,

    -- ** UpdateInsight
    updateInsight_name,
    updateInsight_filters,
    updateInsight_groupByAttribute,
    updateInsight_insightArn,
    updateInsightResponse_httpStatus,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnableStandards,
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** UpdateSecurityHubConfiguration
    updateSecurityHubConfiguration_autoEnableControls,
    updateSecurityHubConfigurationResponse_httpStatus,

    -- ** UpdateStandardsControl
    updateStandardsControl_disabledReason,
    updateStandardsControl_controlStatus,
    updateStandardsControl_standardsControlArn,
    updateStandardsControlResponse_httpStatus,

    -- * Types

    -- ** AccountDetails
    accountDetails_email,
    accountDetails_accountId,

    -- ** Action
    action_actionType,
    action_networkConnectionAction,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_portProbeAction,

    -- ** ActionLocalIpDetails
    actionLocalIpDetails_ipAddressV4,

    -- ** ActionLocalPortDetails
    actionLocalPortDetails_port,
    actionLocalPortDetails_portName,

    -- ** ActionRemoteIpDetails
    actionRemoteIpDetails_country,
    actionRemoteIpDetails_ipAddressV4,
    actionRemoteIpDetails_city,
    actionRemoteIpDetails_organization,
    actionRemoteIpDetails_geoLocation,

    -- ** ActionRemotePortDetails
    actionRemotePortDetails_port,
    actionRemotePortDetails_portName,

    -- ** ActionTarget
    actionTarget_actionTargetArn,
    actionTarget_name,
    actionTarget_description,

    -- ** Adjustment
    adjustment_metric,
    adjustment_reason,

    -- ** AdminAccount
    adminAccount_status,
    adminAccount_accountId,

    -- ** AvailabilityZone
    availabilityZone_zoneName,
    availabilityZone_subnetId,

    -- ** AwsApiCallAction
    awsApiCallAction_affectedResources,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_lastSeen,
    awsApiCallAction_domainDetails,
    awsApiCallAction_api,
    awsApiCallAction_serviceName,
    awsApiCallAction_callerType,
    awsApiCallAction_firstSeen,

    -- ** AwsApiCallActionDomainDetails
    awsApiCallActionDomainDetails_domain,

    -- ** AwsApiGatewayAccessLogSettings
    awsApiGatewayAccessLogSettings_format,
    awsApiGatewayAccessLogSettings_destinationArn,

    -- ** AwsApiGatewayCanarySettings
    awsApiGatewayCanarySettings_deploymentId,
    awsApiGatewayCanarySettings_useStageCache,
    awsApiGatewayCanarySettings_stageVariableOverrides,
    awsApiGatewayCanarySettings_percentTraffic,

    -- ** AwsApiGatewayEndpointConfiguration
    awsApiGatewayEndpointConfiguration_types,

    -- ** AwsApiGatewayMethodSettings
    awsApiGatewayMethodSettings_throttlingRateLimit,
    awsApiGatewayMethodSettings_loggingLevel,
    awsApiGatewayMethodSettings_throttlingBurstLimit,
    awsApiGatewayMethodSettings_resourcePath,
    awsApiGatewayMethodSettings_metricsEnabled,
    awsApiGatewayMethodSettings_requireAuthorizationForCacheControl,
    awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy,
    awsApiGatewayMethodSettings_httpMethod,
    awsApiGatewayMethodSettings_cacheTtlInSeconds,
    awsApiGatewayMethodSettings_dataTraceEnabled,
    awsApiGatewayMethodSettings_cachingEnabled,
    awsApiGatewayMethodSettings_cacheDataEncrypted,

    -- ** AwsApiGatewayRestApiDetails
    awsApiGatewayRestApiDetails_name,
    awsApiGatewayRestApiDetails_id,
    awsApiGatewayRestApiDetails_description,
    awsApiGatewayRestApiDetails_binaryMediaTypes,
    awsApiGatewayRestApiDetails_endpointConfiguration,
    awsApiGatewayRestApiDetails_createdDate,
    awsApiGatewayRestApiDetails_apiKeySource,
    awsApiGatewayRestApiDetails_minimumCompressionSize,
    awsApiGatewayRestApiDetails_version,

    -- ** AwsApiGatewayStageDetails
    awsApiGatewayStageDetails_stageName,
    awsApiGatewayStageDetails_webAclArn,
    awsApiGatewayStageDetails_accessLogSettings,
    awsApiGatewayStageDetails_cacheClusterEnabled,
    awsApiGatewayStageDetails_cacheClusterStatus,
    awsApiGatewayStageDetails_deploymentId,
    awsApiGatewayStageDetails_lastUpdatedDate,
    awsApiGatewayStageDetails_methodSettings,
    awsApiGatewayStageDetails_tracingEnabled,
    awsApiGatewayStageDetails_description,
    awsApiGatewayStageDetails_clientCertificateId,
    awsApiGatewayStageDetails_cacheClusterSize,
    awsApiGatewayStageDetails_createdDate,
    awsApiGatewayStageDetails_canarySettings,
    awsApiGatewayStageDetails_documentationVersion,
    awsApiGatewayStageDetails_variables,

    -- ** AwsApiGatewayV2ApiDetails
    awsApiGatewayV2ApiDetails_name,
    awsApiGatewayV2ApiDetails_apiEndpoint,
    awsApiGatewayV2ApiDetails_apiId,
    awsApiGatewayV2ApiDetails_routeSelectionExpression,
    awsApiGatewayV2ApiDetails_description,
    awsApiGatewayV2ApiDetails_apiKeySelectionExpression,
    awsApiGatewayV2ApiDetails_protocolType,
    awsApiGatewayV2ApiDetails_createdDate,
    awsApiGatewayV2ApiDetails_corsConfiguration,
    awsApiGatewayV2ApiDetails_version,

    -- ** AwsApiGatewayV2RouteSettings
    awsApiGatewayV2RouteSettings_throttlingRateLimit,
    awsApiGatewayV2RouteSettings_loggingLevel,
    awsApiGatewayV2RouteSettings_throttlingBurstLimit,
    awsApiGatewayV2RouteSettings_detailedMetricsEnabled,
    awsApiGatewayV2RouteSettings_dataTraceEnabled,

    -- ** AwsApiGatewayV2StageDetails
    awsApiGatewayV2StageDetails_stageName,
    awsApiGatewayV2StageDetails_accessLogSettings,
    awsApiGatewayV2StageDetails_deploymentId,
    awsApiGatewayV2StageDetails_autoDeploy,
    awsApiGatewayV2StageDetails_lastUpdatedDate,
    awsApiGatewayV2StageDetails_stageVariables,
    awsApiGatewayV2StageDetails_description,
    awsApiGatewayV2StageDetails_clientCertificateId,
    awsApiGatewayV2StageDetails_lastDeploymentStatusMessage,
    awsApiGatewayV2StageDetails_defaultRouteSettings,
    awsApiGatewayV2StageDetails_createdDate,
    awsApiGatewayV2StageDetails_apiGatewayManaged,
    awsApiGatewayV2StageDetails_routeSettings,

    -- ** AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
    awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value,

    -- ** AwsAutoScalingAutoScalingGroupDetails
    awsAutoScalingAutoScalingGroupDetails_createdTime,
    awsAutoScalingAutoScalingGroupDetails_loadBalancerNames,
    awsAutoScalingAutoScalingGroupDetails_availabilityZones,
    awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod,
    awsAutoScalingAutoScalingGroupDetails_launchTemplate,
    awsAutoScalingAutoScalingGroupDetails_launchConfigurationName,
    awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy,
    awsAutoScalingAutoScalingGroupDetails_healthCheckType,
    awsAutoScalingAutoScalingGroupDetails_capacityRebalance,

    -- ** AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_ebs,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_deviceName,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_noDevice,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_virtualName,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops,

    -- ** AwsAutoScalingLaunchConfigurationDetails
    awsAutoScalingLaunchConfigurationDetails_ebsOptimized,
    awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile,
    awsAutoScalingLaunchConfigurationDetails_createdTime,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId,
    awsAutoScalingLaunchConfigurationDetails_userData,
    awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress,
    awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings,
    awsAutoScalingLaunchConfigurationDetails_launchConfigurationName,
    awsAutoScalingLaunchConfigurationDetails_instanceType,
    awsAutoScalingLaunchConfigurationDetails_placementTenancy,
    awsAutoScalingLaunchConfigurationDetails_securityGroups,
    awsAutoScalingLaunchConfigurationDetails_ramdiskId,
    awsAutoScalingLaunchConfigurationDetails_instanceMonitoring,
    awsAutoScalingLaunchConfigurationDetails_keyName,
    awsAutoScalingLaunchConfigurationDetails_kernelId,
    awsAutoScalingLaunchConfigurationDetails_spotPrice,
    awsAutoScalingLaunchConfigurationDetails_imageId,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups,
    awsAutoScalingLaunchConfigurationDetails_metadataOptions,

    -- ** AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
    awsAutoScalingLaunchConfigurationInstanceMonitoringDetails_enabled,

    -- ** AwsAutoScalingLaunchConfigurationMetadataOptions
    awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint,

    -- ** AwsBackupBackupPlanAdvancedBackupSettingsDetails
    awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions,

    -- ** AwsBackupBackupPlanBackupPlanDetails
    awsBackupBackupPlanBackupPlanDetails_backupPlanName,
    awsBackupBackupPlanBackupPlanDetails_backupPlanRule,
    awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings,

    -- ** AwsBackupBackupPlanDetails
    awsBackupBackupPlanDetails_backupPlan,
    awsBackupBackupPlanDetails_backupPlanArn,
    awsBackupBackupPlanDetails_backupPlanId,
    awsBackupBackupPlanDetails_versionId,

    -- ** AwsBackupBackupPlanLifecycleDetails
    awsBackupBackupPlanLifecycleDetails_deleteAfterDays,
    awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays,

    -- ** AwsBackupBackupPlanRuleCopyActionsDetails
    awsBackupBackupPlanRuleCopyActionsDetails_lifecycle,
    awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn,

    -- ** AwsBackupBackupPlanRuleDetails
    awsBackupBackupPlanRuleDetails_startWindowMinutes,
    awsBackupBackupPlanRuleDetails_lifecycle,
    awsBackupBackupPlanRuleDetails_targetBackupVault,
    awsBackupBackupPlanRuleDetails_ruleId,
    awsBackupBackupPlanRuleDetails_copyActions,
    awsBackupBackupPlanRuleDetails_scheduleExpression,
    awsBackupBackupPlanRuleDetails_enableContinuousBackup,
    awsBackupBackupPlanRuleDetails_ruleName,
    awsBackupBackupPlanRuleDetails_completionWindowMinutes,

    -- ** AwsBackupBackupVaultDetails
    awsBackupBackupVaultDetails_notifications,
    awsBackupBackupVaultDetails_encryptionKeyArn,
    awsBackupBackupVaultDetails_backupVaultName,
    awsBackupBackupVaultDetails_backupVaultArn,
    awsBackupBackupVaultDetails_accessPolicy,

    -- ** AwsBackupBackupVaultNotificationsDetails
    awsBackupBackupVaultNotificationsDetails_backupVaultEvents,
    awsBackupBackupVaultNotificationsDetails_snsTopicArn,

    -- ** AwsBackupRecoveryPointCalculatedLifecycleDetails
    awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt,
    awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt,

    -- ** AwsBackupRecoveryPointCreatedByDetails
    awsBackupRecoveryPointCreatedByDetails_backupPlanVersion,
    awsBackupRecoveryPointCreatedByDetails_backupPlanArn,
    awsBackupRecoveryPointCreatedByDetails_backupPlanId,
    awsBackupRecoveryPointCreatedByDetails_backupRuleId,

    -- ** AwsBackupRecoveryPointDetails
    awsBackupRecoveryPointDetails_encryptionKeyArn,
    awsBackupRecoveryPointDetails_resourceType,
    awsBackupRecoveryPointDetails_lifecycle,
    awsBackupRecoveryPointDetails_recoveryPointArn,
    awsBackupRecoveryPointDetails_completionDate,
    awsBackupRecoveryPointDetails_backupVaultName,
    awsBackupRecoveryPointDetails_creationDate,
    awsBackupRecoveryPointDetails_backupSizeInBytes,
    awsBackupRecoveryPointDetails_status,
    awsBackupRecoveryPointDetails_backupVaultArn,
    awsBackupRecoveryPointDetails_isEncrypted,
    awsBackupRecoveryPointDetails_iamRoleArn,
    awsBackupRecoveryPointDetails_sourceBackupVaultArn,
    awsBackupRecoveryPointDetails_resourceArn,
    awsBackupRecoveryPointDetails_storageClass,
    awsBackupRecoveryPointDetails_statusMessage,
    awsBackupRecoveryPointDetails_createdBy,
    awsBackupRecoveryPointDetails_lastRestoreTime,
    awsBackupRecoveryPointDetails_calculatedLifecycle,

    -- ** AwsBackupRecoveryPointLifecycleDetails
    awsBackupRecoveryPointLifecycleDetails_deleteAfterDays,
    awsBackupRecoveryPointLifecycleDetails_moveToColdStorageAfterDays,

    -- ** AwsCertificateManagerCertificateDetails
    awsCertificateManagerCertificateDetails_issuer,
    awsCertificateManagerCertificateDetails_domainValidationOptions,
    awsCertificateManagerCertificateDetails_type,
    awsCertificateManagerCertificateDetails_certificateAuthorityArn,
    awsCertificateManagerCertificateDetails_domainName,
    awsCertificateManagerCertificateDetails_serial,
    awsCertificateManagerCertificateDetails_keyUsages,
    awsCertificateManagerCertificateDetails_renewalSummary,
    awsCertificateManagerCertificateDetails_keyAlgorithm,
    awsCertificateManagerCertificateDetails_extendedKeyUsages,
    awsCertificateManagerCertificateDetails_inUseBy,
    awsCertificateManagerCertificateDetails_status,
    awsCertificateManagerCertificateDetails_options,
    awsCertificateManagerCertificateDetails_importedAt,
    awsCertificateManagerCertificateDetails_notBefore,
    awsCertificateManagerCertificateDetails_signatureAlgorithm,
    awsCertificateManagerCertificateDetails_subject,
    awsCertificateManagerCertificateDetails_notAfter,
    awsCertificateManagerCertificateDetails_renewalEligibility,
    awsCertificateManagerCertificateDetails_createdAt,
    awsCertificateManagerCertificateDetails_subjectAlternativeNames,
    awsCertificateManagerCertificateDetails_failureReason,
    awsCertificateManagerCertificateDetails_issuedAt,

    -- ** AwsCertificateManagerCertificateDomainValidationOption
    awsCertificateManagerCertificateDomainValidationOption_domainName,
    awsCertificateManagerCertificateDomainValidationOption_validationStatus,
    awsCertificateManagerCertificateDomainValidationOption_validationDomain,
    awsCertificateManagerCertificateDomainValidationOption_resourceRecord,
    awsCertificateManagerCertificateDomainValidationOption_validationEmails,
    awsCertificateManagerCertificateDomainValidationOption_validationMethod,

    -- ** AwsCertificateManagerCertificateExtendedKeyUsage
    awsCertificateManagerCertificateExtendedKeyUsage_name,
    awsCertificateManagerCertificateExtendedKeyUsage_oId,

    -- ** AwsCertificateManagerCertificateKeyUsage
    awsCertificateManagerCertificateKeyUsage_name,

    -- ** AwsCertificateManagerCertificateOptions
    awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference,

    -- ** AwsCertificateManagerCertificateRenewalSummary
    awsCertificateManagerCertificateRenewalSummary_domainValidationOptions,
    awsCertificateManagerCertificateRenewalSummary_renewalStatusReason,
    awsCertificateManagerCertificateRenewalSummary_renewalStatus,
    awsCertificateManagerCertificateRenewalSummary_updatedAt,

    -- ** AwsCertificateManagerCertificateResourceRecord
    awsCertificateManagerCertificateResourceRecord_name,
    awsCertificateManagerCertificateResourceRecord_type,
    awsCertificateManagerCertificateResourceRecord_value,

    -- ** AwsCloudFormationStackDetails
    awsCloudFormationStackDetails_stackId,
    awsCloudFormationStackDetails_roleArn,
    awsCloudFormationStackDetails_timeoutInMinutes,
    awsCloudFormationStackDetails_enableTerminationProtection,
    awsCloudFormationStackDetails_notificationArns,
    awsCloudFormationStackDetails_stackStatusReason,
    awsCloudFormationStackDetails_disableRollback,
    awsCloudFormationStackDetails_lastUpdatedTime,
    awsCloudFormationStackDetails_description,
    awsCloudFormationStackDetails_stackName,
    awsCloudFormationStackDetails_stackStatus,
    awsCloudFormationStackDetails_outputs,
    awsCloudFormationStackDetails_capabilities,
    awsCloudFormationStackDetails_creationTime,
    awsCloudFormationStackDetails_driftInformation,

    -- ** AwsCloudFormationStackDriftInformationDetails
    awsCloudFormationStackDriftInformationDetails_stackDriftStatus,

    -- ** AwsCloudFormationStackOutputsDetails
    awsCloudFormationStackOutputsDetails_outputKey,
    awsCloudFormationStackOutputsDetails_description,
    awsCloudFormationStackOutputsDetails_outputValue,

    -- ** AwsCloudFrontDistributionCacheBehavior
    awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy,

    -- ** AwsCloudFrontDistributionCacheBehaviors
    awsCloudFrontDistributionCacheBehaviors_items,

    -- ** AwsCloudFrontDistributionDefaultCacheBehavior
    awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy,

    -- ** AwsCloudFrontDistributionDetails
    awsCloudFrontDistributionDetails_domainName,
    awsCloudFrontDistributionDetails_status,
    awsCloudFrontDistributionDetails_defaultRootObject,
    awsCloudFrontDistributionDetails_viewerCertificate,
    awsCloudFrontDistributionDetails_lastModifiedTime,
    awsCloudFrontDistributionDetails_logging,
    awsCloudFrontDistributionDetails_webAclId,
    awsCloudFrontDistributionDetails_originGroups,
    awsCloudFrontDistributionDetails_origins,
    awsCloudFrontDistributionDetails_cacheBehaviors,
    awsCloudFrontDistributionDetails_eTag,
    awsCloudFrontDistributionDetails_defaultCacheBehavior,

    -- ** AwsCloudFrontDistributionLogging
    awsCloudFrontDistributionLogging_bucket,
    awsCloudFrontDistributionLogging_enabled,
    awsCloudFrontDistributionLogging_includeCookies,
    awsCloudFrontDistributionLogging_prefix,

    -- ** AwsCloudFrontDistributionOriginCustomOriginConfig
    awsCloudFrontDistributionOriginCustomOriginConfig_httpsPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_httpPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_originKeepaliveTimeout,
    awsCloudFrontDistributionOriginCustomOriginConfig_originProtocolPolicy,
    awsCloudFrontDistributionOriginCustomOriginConfig_originReadTimeout,
    awsCloudFrontDistributionOriginCustomOriginConfig_originSslProtocols,

    -- ** AwsCloudFrontDistributionOriginGroup
    awsCloudFrontDistributionOriginGroup_failoverCriteria,

    -- ** AwsCloudFrontDistributionOriginGroupFailover
    awsCloudFrontDistributionOriginGroupFailover_statusCodes,

    -- ** AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
    awsCloudFrontDistributionOriginGroupFailoverStatusCodes_items,
    awsCloudFrontDistributionOriginGroupFailoverStatusCodes_quantity,

    -- ** AwsCloudFrontDistributionOriginGroups
    awsCloudFrontDistributionOriginGroups_items,

    -- ** AwsCloudFrontDistributionOriginItem
    awsCloudFrontDistributionOriginItem_domainName,
    awsCloudFrontDistributionOriginItem_id,
    awsCloudFrontDistributionOriginItem_s3OriginConfig,
    awsCloudFrontDistributionOriginItem_originPath,
    awsCloudFrontDistributionOriginItem_customOriginConfig,

    -- ** AwsCloudFrontDistributionOriginS3OriginConfig
    awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity,

    -- ** AwsCloudFrontDistributionOriginSslProtocols
    awsCloudFrontDistributionOriginSslProtocols_items,
    awsCloudFrontDistributionOriginSslProtocols_quantity,

    -- ** AwsCloudFrontDistributionOrigins
    awsCloudFrontDistributionOrigins_items,

    -- ** AwsCloudFrontDistributionViewerCertificate
    awsCloudFrontDistributionViewerCertificate_iamCertificateId,
    awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate,
    awsCloudFrontDistributionViewerCertificate_certificate,
    awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion,
    awsCloudFrontDistributionViewerCertificate_acmCertificateArn,
    awsCloudFrontDistributionViewerCertificate_sslSupportMethod,
    awsCloudFrontDistributionViewerCertificate_certificateSource,

    -- ** AwsCloudTrailTrailDetails
    awsCloudTrailTrailDetails_s3KeyPrefix,
    awsCloudTrailTrailDetails_name,
    awsCloudTrailTrailDetails_logFileValidationEnabled,
    awsCloudTrailTrailDetails_snsTopicName,
    awsCloudTrailTrailDetails_isOrganizationTrail,
    awsCloudTrailTrailDetails_includeGlobalServiceEvents,
    awsCloudTrailTrailDetails_s3BucketName,
    awsCloudTrailTrailDetails_hasCustomEventSelectors,
    awsCloudTrailTrailDetails_snsTopicArn,
    awsCloudTrailTrailDetails_isMultiRegionTrail,
    awsCloudTrailTrailDetails_kmsKeyId,
    awsCloudTrailTrailDetails_cloudWatchLogsRoleArn,
    awsCloudTrailTrailDetails_homeRegion,
    awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn,
    awsCloudTrailTrailDetails_trailArn,

    -- ** AwsCloudWatchAlarmDetails
    awsCloudWatchAlarmDetails_alarmActions,
    awsCloudWatchAlarmDetails_alarmDescription,
    awsCloudWatchAlarmDetails_extendedStatistic,
    awsCloudWatchAlarmDetails_actionsEnabled,
    awsCloudWatchAlarmDetails_period,
    awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile,
    awsCloudWatchAlarmDetails_dimensions,
    awsCloudWatchAlarmDetails_thresholdMetricId,
    awsCloudWatchAlarmDetails_treatMissingData,
    awsCloudWatchAlarmDetails_evaluationPeriods,
    awsCloudWatchAlarmDetails_datapointsToAlarm,
    awsCloudWatchAlarmDetails_insufficientDataActions,
    awsCloudWatchAlarmDetails_alarmArn,
    awsCloudWatchAlarmDetails_metricName,
    awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp,
    awsCloudWatchAlarmDetails_threshold,
    awsCloudWatchAlarmDetails_okActions,
    awsCloudWatchAlarmDetails_alarmName,
    awsCloudWatchAlarmDetails_comparisonOperator,
    awsCloudWatchAlarmDetails_namespace,
    awsCloudWatchAlarmDetails_statistic,
    awsCloudWatchAlarmDetails_unit,

    -- ** AwsCloudWatchAlarmDimensionsDetails
    awsCloudWatchAlarmDimensionsDetails_name,
    awsCloudWatchAlarmDimensionsDetails_value,

    -- ** AwsCodeBuildProjectArtifactsDetails
    awsCodeBuildProjectArtifactsDetails_encryptionDisabled,
    awsCodeBuildProjectArtifactsDetails_name,
    awsCodeBuildProjectArtifactsDetails_type,
    awsCodeBuildProjectArtifactsDetails_path,
    awsCodeBuildProjectArtifactsDetails_artifactIdentifier,
    awsCodeBuildProjectArtifactsDetails_packaging,
    awsCodeBuildProjectArtifactsDetails_location,
    awsCodeBuildProjectArtifactsDetails_overrideArtifactName,
    awsCodeBuildProjectArtifactsDetails_namespaceType,

    -- ** AwsCodeBuildProjectDetails
    awsCodeBuildProjectDetails_name,
    awsCodeBuildProjectDetails_environment,
    awsCodeBuildProjectDetails_vpcConfig,
    awsCodeBuildProjectDetails_secondaryArtifacts,
    awsCodeBuildProjectDetails_serviceRole,
    awsCodeBuildProjectDetails_source,
    awsCodeBuildProjectDetails_logsConfig,
    awsCodeBuildProjectDetails_encryptionKey,
    awsCodeBuildProjectDetails_artifacts,

    -- ** AwsCodeBuildProjectEnvironment
    awsCodeBuildProjectEnvironment_privilegedMode,
    awsCodeBuildProjectEnvironment_type,
    awsCodeBuildProjectEnvironment_imagePullCredentialsType,
    awsCodeBuildProjectEnvironment_certificate,
    awsCodeBuildProjectEnvironment_registryCredential,
    awsCodeBuildProjectEnvironment_environmentVariables,

    -- ** AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value,

    -- ** AwsCodeBuildProjectEnvironmentRegistryCredential
    awsCodeBuildProjectEnvironmentRegistryCredential_credential,
    awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider,

    -- ** AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName,

    -- ** AwsCodeBuildProjectLogsConfigDetails
    awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs,
    awsCodeBuildProjectLogsConfigDetails_s3Logs,

    -- ** AwsCodeBuildProjectLogsConfigS3LogsDetails
    awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled,
    awsCodeBuildProjectLogsConfigS3LogsDetails_status,
    awsCodeBuildProjectLogsConfigS3LogsDetails_location,

    -- ** AwsCodeBuildProjectSource
    awsCodeBuildProjectSource_insecureSsl,
    awsCodeBuildProjectSource_type,
    awsCodeBuildProjectSource_location,
    awsCodeBuildProjectSource_gitCloneDepth,

    -- ** AwsCodeBuildProjectVpcConfig
    awsCodeBuildProjectVpcConfig_securityGroupIds,
    awsCodeBuildProjectVpcConfig_subnets,
    awsCodeBuildProjectVpcConfig_vpcId,

    -- ** AwsCorsConfiguration
    awsCorsConfiguration_allowHeaders,
    awsCorsConfiguration_exposeHeaders,
    awsCorsConfiguration_allowCredentials,
    awsCorsConfiguration_allowMethods,
    awsCorsConfiguration_allowOrigins,
    awsCorsConfiguration_maxAge,

    -- ** AwsDynamoDbTableAttributeDefinition
    awsDynamoDbTableAttributeDefinition_attributeType,
    awsDynamoDbTableAttributeDefinition_attributeName,

    -- ** AwsDynamoDbTableBillingModeSummary
    awsDynamoDbTableBillingModeSummary_billingMode,
    awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- ** AwsDynamoDbTableDetails
    awsDynamoDbTableDetails_tableName,
    awsDynamoDbTableDetails_latestStreamLabel,
    awsDynamoDbTableDetails_billingModeSummary,
    awsDynamoDbTableDetails_tableStatus,
    awsDynamoDbTableDetails_localSecondaryIndexes,
    awsDynamoDbTableDetails_tableSizeBytes,
    awsDynamoDbTableDetails_creationDateTime,
    awsDynamoDbTableDetails_replicas,
    awsDynamoDbTableDetails_itemCount,
    awsDynamoDbTableDetails_provisionedThroughput,
    awsDynamoDbTableDetails_latestStreamArn,
    awsDynamoDbTableDetails_tableId,
    awsDynamoDbTableDetails_keySchema,
    awsDynamoDbTableDetails_restoreSummary,
    awsDynamoDbTableDetails_globalSecondaryIndexes,
    awsDynamoDbTableDetails_streamSpecification,
    awsDynamoDbTableDetails_globalTableVersion,
    awsDynamoDbTableDetails_sseDescription,
    awsDynamoDbTableDetails_attributeDefinitions,

    -- ** AwsDynamoDbTableGlobalSecondaryIndex
    awsDynamoDbTableGlobalSecondaryIndex_itemCount,
    awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput,
    awsDynamoDbTableGlobalSecondaryIndex_backfilling,
    awsDynamoDbTableGlobalSecondaryIndex_indexName,
    awsDynamoDbTableGlobalSecondaryIndex_indexArn,
    awsDynamoDbTableGlobalSecondaryIndex_indexStatus,
    awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes,
    awsDynamoDbTableGlobalSecondaryIndex_keySchema,
    awsDynamoDbTableGlobalSecondaryIndex_projection,

    -- ** AwsDynamoDbTableKeySchema
    awsDynamoDbTableKeySchema_keyType,
    awsDynamoDbTableKeySchema_attributeName,

    -- ** AwsDynamoDbTableLocalSecondaryIndex
    awsDynamoDbTableLocalSecondaryIndex_indexName,
    awsDynamoDbTableLocalSecondaryIndex_indexArn,
    awsDynamoDbTableLocalSecondaryIndex_keySchema,
    awsDynamoDbTableLocalSecondaryIndex_projection,

    -- ** AwsDynamoDbTableProjection
    awsDynamoDbTableProjection_projectionType,
    awsDynamoDbTableProjection_nonKeyAttributes,

    -- ** AwsDynamoDbTableProvisionedThroughput
    awsDynamoDbTableProvisionedThroughput_readCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday,
    awsDynamoDbTableProvisionedThroughput_writeCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime,

    -- ** AwsDynamoDbTableProvisionedThroughputOverride
    awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits,

    -- ** AwsDynamoDbTableReplica
    awsDynamoDbTableReplica_kmsMasterKeyId,
    awsDynamoDbTableReplica_provisionedThroughputOverride,
    awsDynamoDbTableReplica_regionName,
    awsDynamoDbTableReplica_replicaStatusDescription,
    awsDynamoDbTableReplica_globalSecondaryIndexes,
    awsDynamoDbTableReplica_replicaStatus,

    -- ** AwsDynamoDbTableReplicaGlobalSecondaryIndex
    awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName,

    -- ** AwsDynamoDbTableRestoreSummary
    awsDynamoDbTableRestoreSummary_restoreInProgress,
    awsDynamoDbTableRestoreSummary_sourceBackupArn,
    awsDynamoDbTableRestoreSummary_sourceTableArn,
    awsDynamoDbTableRestoreSummary_restoreDateTime,

    -- ** AwsDynamoDbTableSseDescription
    awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime,
    awsDynamoDbTableSseDescription_status,
    awsDynamoDbTableSseDescription_sseType,
    awsDynamoDbTableSseDescription_kmsMasterKeyArn,

    -- ** AwsDynamoDbTableStreamSpecification
    awsDynamoDbTableStreamSpecification_streamViewType,
    awsDynamoDbTableStreamSpecification_streamEnabled,

    -- ** AwsEc2EipDetails
    awsEc2EipDetails_allocationId,
    awsEc2EipDetails_networkBorderGroup,
    awsEc2EipDetails_domain,
    awsEc2EipDetails_networkInterfaceOwnerId,
    awsEc2EipDetails_publicIp,
    awsEc2EipDetails_instanceId,
    awsEc2EipDetails_networkInterfaceId,
    awsEc2EipDetails_privateIpAddress,
    awsEc2EipDetails_associationId,
    awsEc2EipDetails_publicIpv4Pool,

    -- ** AwsEc2InstanceDetails
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_virtualizationType,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_vpcId,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_imageId,
    awsEc2InstanceDetails_networkInterfaces,
    awsEc2InstanceDetails_metadataOptions,

    -- ** AwsEc2InstanceMetadataOptions
    awsEc2InstanceMetadataOptions_httpPutResponseHopLimit,
    awsEc2InstanceMetadataOptions_httpTokens,
    awsEc2InstanceMetadataOptions_httpEndpoint,
    awsEc2InstanceMetadataOptions_instanceMetadataTags,
    awsEc2InstanceMetadataOptions_httpProtocolIpv6,

    -- ** AwsEc2InstanceNetworkInterfacesDetails
    awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId,

    -- ** AwsEc2NetworkAclAssociation
    awsEc2NetworkAclAssociation_networkAclId,
    awsEc2NetworkAclAssociation_subnetId,
    awsEc2NetworkAclAssociation_networkAclAssociationId,

    -- ** AwsEc2NetworkAclDetails
    awsEc2NetworkAclDetails_ownerId,
    awsEc2NetworkAclDetails_networkAclId,
    awsEc2NetworkAclDetails_associations,
    awsEc2NetworkAclDetails_isDefault,
    awsEc2NetworkAclDetails_entries,
    awsEc2NetworkAclDetails_vpcId,

    -- ** AwsEc2NetworkAclEntry
    awsEc2NetworkAclEntry_icmpTypeCode,
    awsEc2NetworkAclEntry_egress,
    awsEc2NetworkAclEntry_portRange,
    awsEc2NetworkAclEntry_ruleNumber,
    awsEc2NetworkAclEntry_cidrBlock,
    awsEc2NetworkAclEntry_ruleAction,
    awsEc2NetworkAclEntry_protocol,
    awsEc2NetworkAclEntry_ipv6CidrBlock,

    -- ** AwsEc2NetworkInterfaceAttachment
    awsEc2NetworkInterfaceAttachment_deleteOnTermination,
    awsEc2NetworkInterfaceAttachment_status,
    awsEc2NetworkInterfaceAttachment_attachmentId,
    awsEc2NetworkInterfaceAttachment_instanceId,
    awsEc2NetworkInterfaceAttachment_attachTime,
    awsEc2NetworkInterfaceAttachment_deviceIndex,
    awsEc2NetworkInterfaceAttachment_instanceOwnerId,

    -- ** AwsEc2NetworkInterfaceDetails
    awsEc2NetworkInterfaceDetails_attachment,
    awsEc2NetworkInterfaceDetails_sourceDestCheck,
    awsEc2NetworkInterfaceDetails_privateIpAddresses,
    awsEc2NetworkInterfaceDetails_publicIp,
    awsEc2NetworkInterfaceDetails_networkInterfaceId,
    awsEc2NetworkInterfaceDetails_publicDnsName,
    awsEc2NetworkInterfaceDetails_securityGroups,
    awsEc2NetworkInterfaceDetails_ipV6Addresses,

    -- ** AwsEc2NetworkInterfaceIpV6AddressDetail
    awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address,

    -- ** AwsEc2NetworkInterfacePrivateIpAddressDetail
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName,

    -- ** AwsEc2NetworkInterfaceSecurityGroup
    awsEc2NetworkInterfaceSecurityGroup_groupName,
    awsEc2NetworkInterfaceSecurityGroup_groupId,

    -- ** AwsEc2SecurityGroupDetails
    awsEc2SecurityGroupDetails_ipPermissionsEgress,
    awsEc2SecurityGroupDetails_ownerId,
    awsEc2SecurityGroupDetails_ipPermissions,
    awsEc2SecurityGroupDetails_groupName,
    awsEc2SecurityGroupDetails_vpcId,
    awsEc2SecurityGroupDetails_groupId,

    -- ** AwsEc2SecurityGroupIpPermission
    awsEc2SecurityGroupIpPermission_toPort,
    awsEc2SecurityGroupIpPermission_ipv6Ranges,
    awsEc2SecurityGroupIpPermission_ipProtocol,
    awsEc2SecurityGroupIpPermission_prefixListIds,
    awsEc2SecurityGroupIpPermission_ipRanges,
    awsEc2SecurityGroupIpPermission_userIdGroupPairs,
    awsEc2SecurityGroupIpPermission_fromPort,

    -- ** AwsEc2SecurityGroupIpRange
    awsEc2SecurityGroupIpRange_cidrIp,

    -- ** AwsEc2SecurityGroupIpv6Range
    awsEc2SecurityGroupIpv6Range_cidrIpv6,

    -- ** AwsEc2SecurityGroupPrefixListId
    awsEc2SecurityGroupPrefixListId_prefixListId,

    -- ** AwsEc2SecurityGroupUserIdGroupPair
    awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId,
    awsEc2SecurityGroupUserIdGroupPair_groupName,
    awsEc2SecurityGroupUserIdGroupPair_peeringStatus,
    awsEc2SecurityGroupUserIdGroupPair_userId,
    awsEc2SecurityGroupUserIdGroupPair_vpcId,
    awsEc2SecurityGroupUserIdGroupPair_groupId,

    -- ** AwsEc2SubnetDetails
    awsEc2SubnetDetails_ownerId,
    awsEc2SubnetDetails_mapPublicIpOnLaunch,
    awsEc2SubnetDetails_defaultForAz,
    awsEc2SubnetDetails_subnetId,
    awsEc2SubnetDetails_state,
    awsEc2SubnetDetails_availabilityZone,
    awsEc2SubnetDetails_ipv6CidrBlockAssociationSet,
    awsEc2SubnetDetails_subnetArn,
    awsEc2SubnetDetails_cidrBlock,
    awsEc2SubnetDetails_assignIpv6AddressOnCreation,
    awsEc2SubnetDetails_vpcId,
    awsEc2SubnetDetails_availabilityZoneId,
    awsEc2SubnetDetails_availableIpAddressCount,

    -- ** AwsEc2TransitGatewayDetails
    awsEc2TransitGatewayDetails_associationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_dnsSupport,
    awsEc2TransitGatewayDetails_defaultRouteTableAssociation,
    awsEc2TransitGatewayDetails_propagationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_id,
    awsEc2TransitGatewayDetails_description,
    awsEc2TransitGatewayDetails_autoAcceptSharedAttachments,
    awsEc2TransitGatewayDetails_multicastSupport,
    awsEc2TransitGatewayDetails_amazonSideAsn,
    awsEc2TransitGatewayDetails_vpnEcmpSupport,
    awsEc2TransitGatewayDetails_defaultRouteTablePropagation,
    awsEc2TransitGatewayDetails_transitGatewayCidrBlocks,

    -- ** AwsEc2VolumeAttachment
    awsEc2VolumeAttachment_deleteOnTermination,
    awsEc2VolumeAttachment_status,
    awsEc2VolumeAttachment_instanceId,
    awsEc2VolumeAttachment_attachTime,

    -- ** AwsEc2VolumeDetails
    awsEc2VolumeDetails_deviceName,
    awsEc2VolumeDetails_snapshotId,
    awsEc2VolumeDetails_size,
    awsEc2VolumeDetails_volumeType,
    awsEc2VolumeDetails_status,
    awsEc2VolumeDetails_attachments,
    awsEc2VolumeDetails_volumeScanStatus,
    awsEc2VolumeDetails_encrypted,
    awsEc2VolumeDetails_kmsKeyId,
    awsEc2VolumeDetails_volumeId,
    awsEc2VolumeDetails_createTime,

    -- ** AwsEc2VpcDetails
    awsEc2VpcDetails_state,
    awsEc2VpcDetails_dhcpOptionsId,
    awsEc2VpcDetails_ipv6CidrBlockAssociationSet,
    awsEc2VpcDetails_cidrBlockAssociationSet,

    -- ** AwsEc2VpcEndpointServiceDetails
    awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_acceptanceRequired,
    awsEc2VpcEndpointServiceDetails_availabilityZones,
    awsEc2VpcEndpointServiceDetails_serviceType,
    awsEc2VpcEndpointServiceDetails_managesVpcEndpoints,
    awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames,
    awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_privateDnsName,
    awsEc2VpcEndpointServiceDetails_serviceState,
    awsEc2VpcEndpointServiceDetails_serviceName,
    awsEc2VpcEndpointServiceDetails_serviceId,

    -- ** AwsEc2VpcEndpointServiceServiceTypeDetails
    awsEc2VpcEndpointServiceServiceTypeDetails_serviceType,

    -- ** AwsEc2VpcPeeringConnectionDetails
    awsEc2VpcPeeringConnectionDetails_requesterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_expirationTime,
    awsEc2VpcPeeringConnectionDetails_accepterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId,
    awsEc2VpcPeeringConnectionDetails_status,

    -- ** AwsEc2VpcPeeringConnectionStatusDetails
    awsEc2VpcPeeringConnectionStatusDetails_message,
    awsEc2VpcPeeringConnectionStatusDetails_code,

    -- ** AwsEc2VpcPeeringConnectionVpcInfoDetails
    awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions,
    awsEc2VpcPeeringConnectionVpcInfoDetails_region,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock,
    awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId,

    -- ** AwsEc2VpnConnectionDetails
    awsEc2VpnConnectionDetails_type,
    awsEc2VpnConnectionDetails_transitGatewayId,
    awsEc2VpnConnectionDetails_customerGatewayConfiguration,
    awsEc2VpnConnectionDetails_vpnConnectionId,
    awsEc2VpnConnectionDetails_state,
    awsEc2VpnConnectionDetails_options,
    awsEc2VpnConnectionDetails_customerGatewayId,
    awsEc2VpnConnectionDetails_vpnGatewayId,
    awsEc2VpnConnectionDetails_category,
    awsEc2VpnConnectionDetails_vgwTelemetry,
    awsEc2VpnConnectionDetails_routes,

    -- ** AwsEc2VpnConnectionOptionsDetails
    awsEc2VpnConnectionOptionsDetails_tunnelOptions,
    awsEc2VpnConnectionOptionsDetails_staticRoutesOnly,

    -- ** AwsEc2VpnConnectionOptionsTunnelOptionsDetails
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize,

    -- ** AwsEc2VpnConnectionRoutesDetails
    awsEc2VpnConnectionRoutesDetails_state,
    awsEc2VpnConnectionRoutesDetails_destinationCidrBlock,

    -- ** AwsEc2VpnConnectionVgwTelemetryDetails
    awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount,
    awsEc2VpnConnectionVgwTelemetryDetails_status,
    awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange,
    awsEc2VpnConnectionVgwTelemetryDetails_certificateArn,
    awsEc2VpnConnectionVgwTelemetryDetails_statusMessage,
    awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress,

    -- ** AwsEcrContainerImageDetails
    awsEcrContainerImageDetails_repositoryName,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_imagePublishedAt,
    awsEcrContainerImageDetails_registryId,
    awsEcrContainerImageDetails_imageDigest,
    awsEcrContainerImageDetails_architecture,

    -- ** AwsEcrRepositoryDetails
    awsEcrRepositoryDetails_repositoryName,
    awsEcrRepositoryDetails_arn,
    awsEcrRepositoryDetails_repositoryPolicyText,
    awsEcrRepositoryDetails_lifecyclePolicy,
    awsEcrRepositoryDetails_imageTagMutability,
    awsEcrRepositoryDetails_imageScanningConfiguration,

    -- ** AwsEcrRepositoryImageScanningConfigurationDetails
    awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush,

    -- ** AwsEcrRepositoryLifecyclePolicyDetails
    awsEcrRepositoryLifecyclePolicyDetails_registryId,
    awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText,

    -- ** AwsEcsClusterClusterSettingsDetails
    awsEcsClusterClusterSettingsDetails_name,
    awsEcsClusterClusterSettingsDetails_value,

    -- ** AwsEcsClusterConfigurationDetails
    awsEcsClusterConfigurationDetails_executeCommandConfiguration,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName,

    -- ** AwsEcsClusterDefaultCapacityProviderStrategyDetails
    awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_base,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_weight,

    -- ** AwsEcsClusterDetails
    awsEcsClusterDetails_clusterArn,
    awsEcsClusterDetails_clusterSettings,
    awsEcsClusterDetails_configuration,
    awsEcsClusterDetails_registeredContainerInstancesCount,
    awsEcsClusterDetails_status,
    awsEcsClusterDetails_runningTasksCount,
    awsEcsClusterDetails_capacityProviders,
    awsEcsClusterDetails_activeServicesCount,
    awsEcsClusterDetails_defaultCapacityProviderStrategy,
    awsEcsClusterDetails_clusterName,

    -- ** AwsEcsContainerDetails
    awsEcsContainerDetails_name,
    awsEcsContainerDetails_privileged,
    awsEcsContainerDetails_mountPoints,
    awsEcsContainerDetails_image,

    -- ** AwsEcsServiceCapacityProviderStrategyDetails
    awsEcsServiceCapacityProviderStrategyDetails_capacityProvider,
    awsEcsServiceCapacityProviderStrategyDetails_base,
    awsEcsServiceCapacityProviderStrategyDetails_weight,

    -- ** AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback,

    -- ** AwsEcsServiceDeploymentConfigurationDetails
    awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent,
    awsEcsServiceDeploymentConfigurationDetails_maximumPercent,
    awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker,

    -- ** AwsEcsServiceDeploymentControllerDetails
    awsEcsServiceDeploymentControllerDetails_type,

    -- ** AwsEcsServiceDetails
    awsEcsServiceDetails_healthCheckGracePeriodSeconds,
    awsEcsServiceDetails_name,
    awsEcsServiceDetails_deploymentConfiguration,
    awsEcsServiceDetails_schedulingStrategy,
    awsEcsServiceDetails_serviceRegistries,
    awsEcsServiceDetails_cluster,
    awsEcsServiceDetails_placementStrategies,
    awsEcsServiceDetails_taskDefinition,
    awsEcsServiceDetails_networkConfiguration,
    awsEcsServiceDetails_desiredCount,
    awsEcsServiceDetails_enableExecuteCommand,
    awsEcsServiceDetails_capacityProviderStrategy,
    awsEcsServiceDetails_placementConstraints,
    awsEcsServiceDetails_propagateTags,
    awsEcsServiceDetails_deploymentController,
    awsEcsServiceDetails_loadBalancers,
    awsEcsServiceDetails_launchType,
    awsEcsServiceDetails_platformVersion,
    awsEcsServiceDetails_role,
    awsEcsServiceDetails_serviceName,
    awsEcsServiceDetails_enableEcsManagedTags,
    awsEcsServiceDetails_serviceArn,

    -- ** AwsEcsServiceLoadBalancersDetails
    awsEcsServiceLoadBalancersDetails_containerPort,
    awsEcsServiceLoadBalancersDetails_containerName,
    awsEcsServiceLoadBalancersDetails_loadBalancerName,
    awsEcsServiceLoadBalancersDetails_targetGroupArn,

    -- ** AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp,

    -- ** AwsEcsServiceNetworkConfigurationDetails
    awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration,

    -- ** AwsEcsServicePlacementConstraintsDetails
    awsEcsServicePlacementConstraintsDetails_type,
    awsEcsServicePlacementConstraintsDetails_expression,

    -- ** AwsEcsServicePlacementStrategiesDetails
    awsEcsServicePlacementStrategiesDetails_type,
    awsEcsServicePlacementStrategiesDetails_field,

    -- ** AwsEcsServiceServiceRegistriesDetails
    awsEcsServiceServiceRegistriesDetails_port,
    awsEcsServiceServiceRegistriesDetails_containerPort,
    awsEcsServiceServiceRegistriesDetails_containerName,
    awsEcsServiceServiceRegistriesDetails_registryArn,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDetails
    awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem,
    awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck,
    awsEcsTaskDefinitionContainerDefinitionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn,
    awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environment,
    awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements,
    awsEcsTaskDefinitionContainerDefinitionsDetails_cpu,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memory,
    awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers,
    awsEcsTaskDefinitionContainerDefinitionsDetails_user,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation,
    awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits,
    awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials,
    awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings,
    awsEcsTaskDefinitionContainerDefinitionsDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles,
    awsEcsTaskDefinitionContainerDefinitionsDetails_secrets,
    awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels,
    awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains,
    awsEcsTaskDefinitionContainerDefinitionsDetails_privileged,
    awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint,
    awsEcsTaskDefinitionContainerDefinitionsDetails_links,
    awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints,
    awsEcsTaskDefinitionContainerDefinitionsDetails_image,
    awsEcsTaskDefinitionContainerDefinitionsDetails_essential,
    awsEcsTaskDefinitionContainerDefinitionsDetails_interactive,
    awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions,
    awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal,
    awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls,
    awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_workingDirectory,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails_value,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails_value,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
    awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_ipAddress,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort,
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort,
    awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
    awsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails_credentialsParameter,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
    awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_type,
    awsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails_value,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
    awsEcsTaskDefinitionContainerDefinitionsSecretsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsSecretsDetails_valueFrom,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
    awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_namespace,
    awsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails_value,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer,

    -- ** AwsEcsTaskDefinitionDetails
    awsEcsTaskDefinitionDetails_proxyConfiguration,
    awsEcsTaskDefinitionDetails_pidMode,
    awsEcsTaskDefinitionDetails_cpu,
    awsEcsTaskDefinitionDetails_memory,
    awsEcsTaskDefinitionDetails_taskRoleArn,
    awsEcsTaskDefinitionDetails_inferenceAccelerators,
    awsEcsTaskDefinitionDetails_volumes,
    awsEcsTaskDefinitionDetails_requiresCompatibilities,
    awsEcsTaskDefinitionDetails_placementConstraints,
    awsEcsTaskDefinitionDetails_family,
    awsEcsTaskDefinitionDetails_containerDefinitions,
    awsEcsTaskDefinitionDetails_executionRoleArn,
    awsEcsTaskDefinitionDetails_networkMode,
    awsEcsTaskDefinitionDetails_ipcMode,

    -- ** AwsEcsTaskDefinitionInferenceAcceleratorsDetails
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType,

    -- ** AwsEcsTaskDefinitionPlacementConstraintsDetails
    awsEcsTaskDefinitionPlacementConstraintsDetails_type,
    awsEcsTaskDefinitionPlacementConstraintsDetails_expression,

    -- ** AwsEcsTaskDefinitionProxyConfigurationDetails
    awsEcsTaskDefinitionProxyConfigurationDetails_containerName,
    awsEcsTaskDefinitionProxyConfigurationDetails_type,
    awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties,

    -- ** AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value,

    -- ** AwsEcsTaskDefinitionVolumesDetails
    awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_name,
    awsEcsTaskDefinitionVolumesDetails_host,
    awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration,

    -- ** AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig,

    -- ** AwsEcsTaskDefinitionVolumesHostDetails
    awsEcsTaskDefinitionVolumesHostDetails_sourcePath,

    -- ** AwsEcsTaskDetails
    awsEcsTaskDetails_clusterArn,
    awsEcsTaskDetails_containers,
    awsEcsTaskDetails_taskDefinitionArn,
    awsEcsTaskDetails_startedBy,
    awsEcsTaskDetails_volumes,
    awsEcsTaskDetails_startedAt,
    awsEcsTaskDetails_group,
    awsEcsTaskDetails_createdAt,
    awsEcsTaskDetails_version,

    -- ** AwsEcsTaskVolumeDetails
    awsEcsTaskVolumeDetails_name,
    awsEcsTaskVolumeDetails_host,

    -- ** AwsEcsTaskVolumeHostDetails
    awsEcsTaskVolumeHostDetails_sourcePath,

    -- ** AwsEfsAccessPointDetails
    awsEfsAccessPointDetails_clientToken,
    awsEfsAccessPointDetails_posixUser,
    awsEfsAccessPointDetails_arn,
    awsEfsAccessPointDetails_fileSystemId,
    awsEfsAccessPointDetails_accessPointId,
    awsEfsAccessPointDetails_rootDirectory,

    -- ** AwsEfsAccessPointPosixUserDetails
    awsEfsAccessPointPosixUserDetails_gid,
    awsEfsAccessPointPosixUserDetails_secondaryGids,
    awsEfsAccessPointPosixUserDetails_uid,

    -- ** AwsEfsAccessPointRootDirectoryCreationInfoDetails
    awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid,

    -- ** AwsEfsAccessPointRootDirectoryDetails
    awsEfsAccessPointRootDirectoryDetails_creationInfo,
    awsEfsAccessPointRootDirectoryDetails_path,

    -- ** AwsEksClusterDetails
    awsEksClusterDetails_name,
    awsEksClusterDetails_roleArn,
    awsEksClusterDetails_arn,
    awsEksClusterDetails_clusterStatus,
    awsEksClusterDetails_logging,
    awsEksClusterDetails_endpoint,
    awsEksClusterDetails_resourcesVpcConfig,
    awsEksClusterDetails_certificateAuthorityData,
    awsEksClusterDetails_version,

    -- ** AwsEksClusterLoggingClusterLoggingDetails
    awsEksClusterLoggingClusterLoggingDetails_types,
    awsEksClusterLoggingClusterLoggingDetails_enabled,

    -- ** AwsEksClusterLoggingDetails
    awsEksClusterLoggingDetails_clusterLogging,

    -- ** AwsEksClusterResourcesVpcConfigDetails
    awsEksClusterResourcesVpcConfigDetails_securityGroupIds,
    awsEksClusterResourcesVpcConfigDetails_subnetIds,

    -- ** AwsElasticBeanstalkEnvironmentDetails
    awsElasticBeanstalkEnvironmentDetails_cname,
    awsElasticBeanstalkEnvironmentDetails_environmentName,
    awsElasticBeanstalkEnvironmentDetails_status,
    awsElasticBeanstalkEnvironmentDetails_description,
    awsElasticBeanstalkEnvironmentDetails_tier,
    awsElasticBeanstalkEnvironmentDetails_endpointUrl,
    awsElasticBeanstalkEnvironmentDetails_solutionStackName,
    awsElasticBeanstalkEnvironmentDetails_dateUpdated,
    awsElasticBeanstalkEnvironmentDetails_dateCreated,
    awsElasticBeanstalkEnvironmentDetails_environmentArn,
    awsElasticBeanstalkEnvironmentDetails_environmentLinks,
    awsElasticBeanstalkEnvironmentDetails_platformArn,
    awsElasticBeanstalkEnvironmentDetails_environmentId,
    awsElasticBeanstalkEnvironmentDetails_versionLabel,
    awsElasticBeanstalkEnvironmentDetails_applicationName,
    awsElasticBeanstalkEnvironmentDetails_optionSettings,

    -- ** AwsElasticBeanstalkEnvironmentEnvironmentLink
    awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName,
    awsElasticBeanstalkEnvironmentEnvironmentLink_linkName,

    -- ** AwsElasticBeanstalkEnvironmentOptionSetting
    awsElasticBeanstalkEnvironmentOptionSetting_resourceName,
    awsElasticBeanstalkEnvironmentOptionSetting_optionName,
    awsElasticBeanstalkEnvironmentOptionSetting_namespace,
    awsElasticBeanstalkEnvironmentOptionSetting_value,

    -- ** AwsElasticBeanstalkEnvironmentTier
    awsElasticBeanstalkEnvironmentTier_name,
    awsElasticBeanstalkEnvironmentTier_type,
    awsElasticBeanstalkEnvironmentTier_version,

    -- ** AwsElasticsearchDomainDetails
    awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions,
    awsElasticsearchDomainDetails_elasticsearchClusterConfig,
    awsElasticsearchDomainDetails_domainName,
    awsElasticsearchDomainDetails_encryptionAtRestOptions,
    awsElasticsearchDomainDetails_endpoints,
    awsElasticsearchDomainDetails_elasticsearchVersion,
    awsElasticsearchDomainDetails_accessPolicies,
    awsElasticsearchDomainDetails_vPCOptions,
    awsElasticsearchDomainDetails_domainEndpointOptions,
    awsElasticsearchDomainDetails_domainId,
    awsElasticsearchDomainDetails_endpoint,
    awsElasticsearchDomainDetails_serviceSoftwareOptions,
    awsElasticsearchDomainDetails_logPublishingOptions,

    -- ** AwsElasticsearchDomainDomainEndpointOptions
    awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy,
    awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigDetails
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
    awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- ** AwsElasticsearchDomainEncryptionAtRestOptions
    awsElasticsearchDomainEncryptionAtRestOptions_enabled,
    awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId,

    -- ** AwsElasticsearchDomainLogPublishingOptions
    awsElasticsearchDomainLogPublishingOptions_indexSlowLogs,
    awsElasticsearchDomainLogPublishingOptions_auditLogs,
    awsElasticsearchDomainLogPublishingOptions_searchSlowLogs,

    -- ** AwsElasticsearchDomainLogPublishingOptionsLogConfig
    awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn,

    -- ** AwsElasticsearchDomainNodeToNodeEncryptionOptions
    awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled,

    -- ** AwsElasticsearchDomainServiceSoftwareOptions
    awsElasticsearchDomainServiceSoftwareOptions_newVersion,
    awsElasticsearchDomainServiceSoftwareOptions_updateAvailable,
    awsElasticsearchDomainServiceSoftwareOptions_cancellable,
    awsElasticsearchDomainServiceSoftwareOptions_updateStatus,
    awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate,
    awsElasticsearchDomainServiceSoftwareOptions_description,
    awsElasticsearchDomainServiceSoftwareOptions_currentVersion,

    -- ** AwsElasticsearchDomainVPCOptions
    awsElasticsearchDomainVPCOptions_securityGroupIds,
    awsElasticsearchDomainVPCOptions_availabilityZones,
    awsElasticsearchDomainVPCOptions_vPCId,
    awsElasticsearchDomainVPCOptions_subnetIds,

    -- ** AwsElbAppCookieStickinessPolicy
    awsElbAppCookieStickinessPolicy_policyName,
    awsElbAppCookieStickinessPolicy_cookieName,

    -- ** AwsElbLbCookieStickinessPolicy
    awsElbLbCookieStickinessPolicy_policyName,
    awsElbLbCookieStickinessPolicy_cookieExpirationPeriod,

    -- ** AwsElbLoadBalancerAccessLog
    awsElbLoadBalancerAccessLog_s3BucketPrefix,
    awsElbLoadBalancerAccessLog_s3BucketName,
    awsElbLoadBalancerAccessLog_enabled,
    awsElbLoadBalancerAccessLog_emitInterval,

    -- ** AwsElbLoadBalancerAdditionalAttribute
    awsElbLoadBalancerAdditionalAttribute_key,
    awsElbLoadBalancerAdditionalAttribute_value,

    -- ** AwsElbLoadBalancerAttributes
    awsElbLoadBalancerAttributes_connectionSettings,
    awsElbLoadBalancerAttributes_connectionDraining,
    awsElbLoadBalancerAttributes_additionalAttributes,
    awsElbLoadBalancerAttributes_accessLog,
    awsElbLoadBalancerAttributes_crossZoneLoadBalancing,

    -- ** AwsElbLoadBalancerBackendServerDescription
    awsElbLoadBalancerBackendServerDescription_policyNames,
    awsElbLoadBalancerBackendServerDescription_instancePort,

    -- ** AwsElbLoadBalancerConnectionDraining
    awsElbLoadBalancerConnectionDraining_timeout,
    awsElbLoadBalancerConnectionDraining_enabled,

    -- ** AwsElbLoadBalancerConnectionSettings
    awsElbLoadBalancerConnectionSettings_idleTimeout,

    -- ** AwsElbLoadBalancerCrossZoneLoadBalancing
    awsElbLoadBalancerCrossZoneLoadBalancing_enabled,

    -- ** AwsElbLoadBalancerDetails
    awsElbLoadBalancerDetails_instances,
    awsElbLoadBalancerDetails_healthCheck,
    awsElbLoadBalancerDetails_scheme,
    awsElbLoadBalancerDetails_createdTime,
    awsElbLoadBalancerDetails_canonicalHostedZoneName,
    awsElbLoadBalancerDetails_canonicalHostedZoneNameID,
    awsElbLoadBalancerDetails_loadBalancerName,
    awsElbLoadBalancerDetails_subnets,
    awsElbLoadBalancerDetails_sourceSecurityGroup,
    awsElbLoadBalancerDetails_availabilityZones,
    awsElbLoadBalancerDetails_listenerDescriptions,
    awsElbLoadBalancerDetails_policies,
    awsElbLoadBalancerDetails_backendServerDescriptions,
    awsElbLoadBalancerDetails_securityGroups,
    awsElbLoadBalancerDetails_vpcId,
    awsElbLoadBalancerDetails_dnsName,
    awsElbLoadBalancerDetails_loadBalancerAttributes,

    -- ** AwsElbLoadBalancerHealthCheck
    awsElbLoadBalancerHealthCheck_timeout,
    awsElbLoadBalancerHealthCheck_interval,
    awsElbLoadBalancerHealthCheck_healthyThreshold,
    awsElbLoadBalancerHealthCheck_target,
    awsElbLoadBalancerHealthCheck_unhealthyThreshold,

    -- ** AwsElbLoadBalancerInstance
    awsElbLoadBalancerInstance_instanceId,

    -- ** AwsElbLoadBalancerListener
    awsElbLoadBalancerListener_sslCertificateId,
    awsElbLoadBalancerListener_instanceProtocol,
    awsElbLoadBalancerListener_instancePort,
    awsElbLoadBalancerListener_protocol,
    awsElbLoadBalancerListener_loadBalancerPort,

    -- ** AwsElbLoadBalancerListenerDescription
    awsElbLoadBalancerListenerDescription_listener,
    awsElbLoadBalancerListenerDescription_policyNames,

    -- ** AwsElbLoadBalancerPolicies
    awsElbLoadBalancerPolicies_otherPolicies,
    awsElbLoadBalancerPolicies_lbCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_appCookieStickinessPolicies,

    -- ** AwsElbLoadBalancerSourceSecurityGroup
    awsElbLoadBalancerSourceSecurityGroup_ownerAlias,
    awsElbLoadBalancerSourceSecurityGroup_groupName,

    -- ** AwsElbv2LoadBalancerAttribute
    awsElbv2LoadBalancerAttribute_key,
    awsElbv2LoadBalancerAttribute_value,

    -- ** AwsElbv2LoadBalancerDetails
    awsElbv2LoadBalancerDetails_scheme,
    awsElbv2LoadBalancerDetails_type,
    awsElbv2LoadBalancerDetails_createdTime,
    awsElbv2LoadBalancerDetails_availabilityZones,
    awsElbv2LoadBalancerDetails_state,
    awsElbv2LoadBalancerDetails_securityGroups,
    awsElbv2LoadBalancerDetails_dNSName,
    awsElbv2LoadBalancerDetails_vpcId,
    awsElbv2LoadBalancerDetails_loadBalancerAttributes,
    awsElbv2LoadBalancerDetails_canonicalHostedZoneId,
    awsElbv2LoadBalancerDetails_ipAddressType,

    -- ** AwsIamAccessKeyDetails
    awsIamAccessKeyDetails_principalId,
    awsIamAccessKeyDetails_principalName,
    awsIamAccessKeyDetails_userName,
    awsIamAccessKeyDetails_status,
    awsIamAccessKeyDetails_sessionContext,
    awsIamAccessKeyDetails_accountId,
    awsIamAccessKeyDetails_principalType,
    awsIamAccessKeyDetails_createdAt,
    awsIamAccessKeyDetails_accessKeyId,

    -- ** AwsIamAccessKeySessionContext
    awsIamAccessKeySessionContext_sessionIssuer,
    awsIamAccessKeySessionContext_attributes,

    -- ** AwsIamAccessKeySessionContextAttributes
    awsIamAccessKeySessionContextAttributes_mfaAuthenticated,
    awsIamAccessKeySessionContextAttributes_creationDate,

    -- ** AwsIamAccessKeySessionContextSessionIssuer
    awsIamAccessKeySessionContextSessionIssuer_principalId,
    awsIamAccessKeySessionContextSessionIssuer_type,
    awsIamAccessKeySessionContextSessionIssuer_userName,
    awsIamAccessKeySessionContextSessionIssuer_arn,
    awsIamAccessKeySessionContextSessionIssuer_accountId,

    -- ** AwsIamAttachedManagedPolicy
    awsIamAttachedManagedPolicy_policyName,
    awsIamAttachedManagedPolicy_policyArn,

    -- ** AwsIamGroupDetails
    awsIamGroupDetails_path,
    awsIamGroupDetails_groupName,
    awsIamGroupDetails_attachedManagedPolicies,
    awsIamGroupDetails_createDate,
    awsIamGroupDetails_groupId,
    awsIamGroupDetails_groupPolicyList,

    -- ** AwsIamGroupPolicy
    awsIamGroupPolicy_policyName,

    -- ** AwsIamInstanceProfile
    awsIamInstanceProfile_instanceProfileName,
    awsIamInstanceProfile_arn,
    awsIamInstanceProfile_path,
    awsIamInstanceProfile_createDate,
    awsIamInstanceProfile_roles,
    awsIamInstanceProfile_instanceProfileId,

    -- ** AwsIamInstanceProfileRole
    awsIamInstanceProfileRole_roleName,
    awsIamInstanceProfileRole_arn,
    awsIamInstanceProfileRole_path,
    awsIamInstanceProfileRole_assumeRolePolicyDocument,
    awsIamInstanceProfileRole_createDate,
    awsIamInstanceProfileRole_roleId,

    -- ** AwsIamPermissionsBoundary
    awsIamPermissionsBoundary_permissionsBoundaryType,
    awsIamPermissionsBoundary_permissionsBoundaryArn,

    -- ** AwsIamPolicyDetails
    awsIamPolicyDetails_policyName,
    awsIamPolicyDetails_policyId,
    awsIamPolicyDetails_defaultVersionId,
    awsIamPolicyDetails_path,
    awsIamPolicyDetails_updateDate,
    awsIamPolicyDetails_policyVersionList,
    awsIamPolicyDetails_description,
    awsIamPolicyDetails_createDate,
    awsIamPolicyDetails_attachmentCount,
    awsIamPolicyDetails_permissionsBoundaryUsageCount,
    awsIamPolicyDetails_isAttachable,

    -- ** AwsIamPolicyVersion
    awsIamPolicyVersion_isDefaultVersion,
    awsIamPolicyVersion_createDate,
    awsIamPolicyVersion_versionId,

    -- ** AwsIamRoleDetails
    awsIamRoleDetails_roleName,
    awsIamRoleDetails_instanceProfileList,
    awsIamRoleDetails_path,
    awsIamRoleDetails_assumeRolePolicyDocument,
    awsIamRoleDetails_attachedManagedPolicies,
    awsIamRoleDetails_permissionsBoundary,
    awsIamRoleDetails_createDate,
    awsIamRoleDetails_roleId,
    awsIamRoleDetails_rolePolicyList,
    awsIamRoleDetails_maxSessionDuration,

    -- ** AwsIamRolePolicy
    awsIamRolePolicy_policyName,

    -- ** AwsIamUserDetails
    awsIamUserDetails_userName,
    awsIamUserDetails_path,
    awsIamUserDetails_groupList,
    awsIamUserDetails_attachedManagedPolicies,
    awsIamUserDetails_permissionsBoundary,
    awsIamUserDetails_userId,
    awsIamUserDetails_createDate,
    awsIamUserDetails_userPolicyList,

    -- ** AwsIamUserPolicy
    awsIamUserPolicy_policyName,

    -- ** AwsKinesisStreamDetails
    awsKinesisStreamDetails_name,
    awsKinesisStreamDetails_arn,
    awsKinesisStreamDetails_shardCount,
    awsKinesisStreamDetails_retentionPeriodHours,
    awsKinesisStreamDetails_streamEncryption,

    -- ** AwsKinesisStreamStreamEncryptionDetails
    awsKinesisStreamStreamEncryptionDetails_encryptionType,
    awsKinesisStreamStreamEncryptionDetails_keyId,

    -- ** AwsKmsKeyDetails
    awsKmsKeyDetails_aWSAccountId,
    awsKmsKeyDetails_creationDate,
    awsKmsKeyDetails_description,
    awsKmsKeyDetails_keyRotationStatus,
    awsKmsKeyDetails_keyManager,
    awsKmsKeyDetails_keyState,
    awsKmsKeyDetails_keyId,
    awsKmsKeyDetails_origin,

    -- ** AwsLambdaFunctionCode
    awsLambdaFunctionCode_s3Bucket,
    awsLambdaFunctionCode_s3Key,
    awsLambdaFunctionCode_zipFile,
    awsLambdaFunctionCode_s3ObjectVersion,

    -- ** AwsLambdaFunctionDeadLetterConfig
    awsLambdaFunctionDeadLetterConfig_targetArn,

    -- ** AwsLambdaFunctionDetails
    awsLambdaFunctionDetails_tracingConfig,
    awsLambdaFunctionDetails_masterArn,
    awsLambdaFunctionDetails_timeout,
    awsLambdaFunctionDetails_memorySize,
    awsLambdaFunctionDetails_codeSha256,
    awsLambdaFunctionDetails_environment,
    awsLambdaFunctionDetails_code,
    awsLambdaFunctionDetails_vpcConfig,
    awsLambdaFunctionDetails_functionName,
    awsLambdaFunctionDetails_runtime,
    awsLambdaFunctionDetails_kmsKeyArn,
    awsLambdaFunctionDetails_handler,
    awsLambdaFunctionDetails_layers,
    awsLambdaFunctionDetails_packageType,
    awsLambdaFunctionDetails_revisionId,
    awsLambdaFunctionDetails_lastModified,
    awsLambdaFunctionDetails_role,
    awsLambdaFunctionDetails_architectures,
    awsLambdaFunctionDetails_version,
    awsLambdaFunctionDetails_deadLetterConfig,

    -- ** AwsLambdaFunctionEnvironment
    awsLambdaFunctionEnvironment_error,
    awsLambdaFunctionEnvironment_variables,

    -- ** AwsLambdaFunctionEnvironmentError
    awsLambdaFunctionEnvironmentError_message,
    awsLambdaFunctionEnvironmentError_errorCode,

    -- ** AwsLambdaFunctionLayer
    awsLambdaFunctionLayer_arn,
    awsLambdaFunctionLayer_codeSize,

    -- ** AwsLambdaFunctionTracingConfig
    awsLambdaFunctionTracingConfig_mode,

    -- ** AwsLambdaFunctionVpcConfig
    awsLambdaFunctionVpcConfig_securityGroupIds,
    awsLambdaFunctionVpcConfig_vpcId,
    awsLambdaFunctionVpcConfig_subnetIds,

    -- ** AwsLambdaLayerVersionDetails
    awsLambdaLayerVersionDetails_compatibleRuntimes,
    awsLambdaLayerVersionDetails_createdDate,
    awsLambdaLayerVersionDetails_version,

    -- ** AwsMountPoint
    awsMountPoint_containerPath,
    awsMountPoint_sourceVolume,

    -- ** AwsNetworkFirewallFirewallDetails
    awsNetworkFirewallFirewallDetails_deleteProtection,
    awsNetworkFirewallFirewallDetails_subnetChangeProtection,
    awsNetworkFirewallFirewallDetails_firewallId,
    awsNetworkFirewallFirewallDetails_description,
    awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection,
    awsNetworkFirewallFirewallDetails_firewallArn,
    awsNetworkFirewallFirewallDetails_subnetMappings,
    awsNetworkFirewallFirewallDetails_firewallName,
    awsNetworkFirewallFirewallDetails_vpcId,
    awsNetworkFirewallFirewallDetails_firewallPolicyArn,

    -- ** AwsNetworkFirewallFirewallPolicyDetails
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId,
    awsNetworkFirewallFirewallPolicyDetails_description,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicy,

    -- ** AwsNetworkFirewallFirewallSubnetMappingsDetails
    awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId,

    -- ** AwsNetworkFirewallRuleGroupDetails
    awsNetworkFirewallRuleGroupDetails_ruleGroupName,
    awsNetworkFirewallRuleGroupDetails_ruleGroup,
    awsNetworkFirewallRuleGroupDetails_type,
    awsNetworkFirewallRuleGroupDetails_description,
    awsNetworkFirewallRuleGroupDetails_ruleGroupId,
    awsNetworkFirewallRuleGroupDetails_ruleGroupArn,
    awsNetworkFirewallRuleGroupDetails_capacity,

    -- ** AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions,

    -- ** AwsOpenSearchServiceDomainClusterConfigDetails
    awsOpenSearchServiceDomainClusterConfigDetails_warmCount,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_warmType,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceCount,
    awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount,

    -- ** AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
    awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- ** AwsOpenSearchServiceDomainDetails
    awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions,
    awsOpenSearchServiceDomainDetails_clusterConfig,
    awsOpenSearchServiceDomainDetails_advancedSecurityOptions,
    awsOpenSearchServiceDomainDetails_domainName,
    awsOpenSearchServiceDomainDetails_arn,
    awsOpenSearchServiceDomainDetails_encryptionAtRestOptions,
    awsOpenSearchServiceDomainDetails_id,
    awsOpenSearchServiceDomainDetails_accessPolicies,
    awsOpenSearchServiceDomainDetails_vpcOptions,
    awsOpenSearchServiceDomainDetails_domainEndpointOptions,
    awsOpenSearchServiceDomainDetails_domainEndpoints,
    awsOpenSearchServiceDomainDetails_domainEndpoint,
    awsOpenSearchServiceDomainDetails_serviceSoftwareOptions,
    awsOpenSearchServiceDomainDetails_logPublishingOptions,
    awsOpenSearchServiceDomainDetails_engineVersion,

    -- ** AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint,

    -- ** AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId,

    -- ** AwsOpenSearchServiceDomainLogPublishingOption
    awsOpenSearchServiceDomainLogPublishingOption_enabled,
    awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn,

    -- ** AwsOpenSearchServiceDomainLogPublishingOptionsDetails
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs,

    -- ** AwsOpenSearchServiceDomainMasterUserOptionsDetails
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword,

    -- ** AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
    awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled,

    -- ** AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion,

    -- ** AwsOpenSearchServiceDomainVpcOptionsDetails
    awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds,
    awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds,

    -- ** AwsRdsDbClusterAssociatedRole
    awsRdsDbClusterAssociatedRole_roleArn,
    awsRdsDbClusterAssociatedRole_status,

    -- ** AwsRdsDbClusterDetails
    awsRdsDbClusterDetails_port,
    awsRdsDbClusterDetails_hostedZoneId,
    awsRdsDbClusterDetails_preferredBackupWindow,
    awsRdsDbClusterDetails_backupRetentionPeriod,
    awsRdsDbClusterDetails_masterUsername,
    awsRdsDbClusterDetails_copyTagsToSnapshot,
    awsRdsDbClusterDetails_activityStreamStatus,
    awsRdsDbClusterDetails_dbClusterParameterGroup,
    awsRdsDbClusterDetails_dbClusterMembers,
    awsRdsDbClusterDetails_databaseName,
    awsRdsDbClusterDetails_dbClusterIdentifier,
    awsRdsDbClusterDetails_domainMemberships,
    awsRdsDbClusterDetails_availabilityZones,
    awsRdsDbClusterDetails_crossAccountClone,
    awsRdsDbClusterDetails_dbClusterOptionGroupMemberships,
    awsRdsDbClusterDetails_dbSubnetGroup,
    awsRdsDbClusterDetails_status,
    awsRdsDbClusterDetails_httpEndpointEnabled,
    awsRdsDbClusterDetails_customEndpoints,
    awsRdsDbClusterDetails_engineMode,
    awsRdsDbClusterDetails_storageEncrypted,
    awsRdsDbClusterDetails_kmsKeyId,
    awsRdsDbClusterDetails_engine,
    awsRdsDbClusterDetails_allocatedStorage,
    awsRdsDbClusterDetails_readerEndpoint,
    awsRdsDbClusterDetails_deletionProtection,
    awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterDetails_preferredMaintenanceWindow,
    awsRdsDbClusterDetails_endpoint,
    awsRdsDbClusterDetails_clusterCreateTime,
    awsRdsDbClusterDetails_readReplicaIdentifiers,
    awsRdsDbClusterDetails_enabledCloudWatchLogsExports,
    awsRdsDbClusterDetails_dbClusterResourceId,
    awsRdsDbClusterDetails_associatedRoles,
    awsRdsDbClusterDetails_multiAz,
    awsRdsDbClusterDetails_engineVersion,
    awsRdsDbClusterDetails_vpcSecurityGroups,

    -- ** AwsRdsDbClusterMember
    awsRdsDbClusterMember_promotionTier,
    awsRdsDbClusterMember_dbInstanceIdentifier,
    awsRdsDbClusterMember_dbClusterParameterGroupStatus,
    awsRdsDbClusterMember_isClusterWriter,

    -- ** AwsRdsDbClusterOptionGroupMembership
    awsRdsDbClusterOptionGroupMembership_status,
    awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName,

    -- ** AwsRdsDbClusterSnapshotDetails
    awsRdsDbClusterSnapshotDetails_port,
    awsRdsDbClusterSnapshotDetails_percentProgress,
    awsRdsDbClusterSnapshotDetails_masterUsername,
    awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier,
    awsRdsDbClusterSnapshotDetails_dbClusterIdentifier,
    awsRdsDbClusterSnapshotDetails_availabilityZones,
    awsRdsDbClusterSnapshotDetails_status,
    awsRdsDbClusterSnapshotDetails_snapshotCreateTime,
    awsRdsDbClusterSnapshotDetails_storageEncrypted,
    awsRdsDbClusterSnapshotDetails_kmsKeyId,
    awsRdsDbClusterSnapshotDetails_engine,
    awsRdsDbClusterSnapshotDetails_allocatedStorage,
    awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterSnapshotDetails_vpcId,
    awsRdsDbClusterSnapshotDetails_clusterCreateTime,
    awsRdsDbClusterSnapshotDetails_engineVersion,
    awsRdsDbClusterSnapshotDetails_licenseModel,
    awsRdsDbClusterSnapshotDetails_snapshotType,

    -- ** AwsRdsDbDomainMembership
    awsRdsDbDomainMembership_domain,
    awsRdsDbDomainMembership_fqdn,
    awsRdsDbDomainMembership_status,
    awsRdsDbDomainMembership_iamRoleName,

    -- ** AwsRdsDbInstanceAssociatedRole
    awsRdsDbInstanceAssociatedRole_roleArn,
    awsRdsDbInstanceAssociatedRole_featureName,
    awsRdsDbInstanceAssociatedRole_status,

    -- ** AwsRdsDbInstanceDetails
    awsRdsDbInstanceDetails_maxAllocatedStorage,
    awsRdsDbInstanceDetails_listenerEndpoint,
    awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod,
    awsRdsDbInstanceDetails_optionGroupMemberships,
    awsRdsDbInstanceDetails_dbInstanceStatus,
    awsRdsDbInstanceDetails_preferredBackupWindow,
    awsRdsDbInstanceDetails_backupRetentionPeriod,
    awsRdsDbInstanceDetails_dbInstanceClass,
    awsRdsDbInstanceDetails_characterSetName,
    awsRdsDbInstanceDetails_masterUsername,
    awsRdsDbInstanceDetails_copyTagsToSnapshot,
    awsRdsDbInstanceDetails_promotionTier,
    awsRdsDbInstanceDetails_secondaryAvailabilityZone,
    awsRdsDbInstanceDetails_autoMinorVersionUpgrade,
    awsRdsDbInstanceDetails_dbInstanceIdentifier,
    awsRdsDbInstanceDetails_dbInstancePort,
    awsRdsDbInstanceDetails_latestRestorableTime,
    awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers,
    awsRdsDbInstanceDetails_enhancedMonitoringResourceArn,
    awsRdsDbInstanceDetails_dbClusterIdentifier,
    awsRdsDbInstanceDetails_domainMemberships,
    awsRdsDbInstanceDetails_timezone,
    awsRdsDbInstanceDetails_performanceInsightsKmsKeyId,
    awsRdsDbInstanceDetails_dbSecurityGroups,
    awsRdsDbInstanceDetails_dbSubnetGroup,
    awsRdsDbInstanceDetails_monitoringInterval,
    awsRdsDbInstanceDetails_performanceInsightsEnabled,
    awsRdsDbInstanceDetails_instanceCreateTime,
    awsRdsDbInstanceDetails_availabilityZone,
    awsRdsDbInstanceDetails_publiclyAccessible,
    awsRdsDbInstanceDetails_storageType,
    awsRdsDbInstanceDetails_processorFeatures,
    awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers,
    awsRdsDbInstanceDetails_tdeCredentialArn,
    awsRdsDbInstanceDetails_cACertificateIdentifier,
    awsRdsDbInstanceDetails_monitoringRoleArn,
    awsRdsDbInstanceDetails_storageEncrypted,
    awsRdsDbInstanceDetails_kmsKeyId,
    awsRdsDbInstanceDetails_engine,
    awsRdsDbInstanceDetails_allocatedStorage,
    awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled,
    awsRdsDbInstanceDetails_deletionProtection,
    awsRdsDbInstanceDetails_pendingModifiedValues,
    awsRdsDbInstanceDetails_preferredMaintenanceWindow,
    awsRdsDbInstanceDetails_endpoint,
    awsRdsDbInstanceDetails_dbiResourceId,
    awsRdsDbInstanceDetails_dbParameterGroups,
    awsRdsDbInstanceDetails_enabledCloudWatchLogsExports,
    awsRdsDbInstanceDetails_iops,
    awsRdsDbInstanceDetails_associatedRoles,
    awsRdsDbInstanceDetails_multiAz,
    awsRdsDbInstanceDetails_engineVersion,
    awsRdsDbInstanceDetails_dbName,
    awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier,
    awsRdsDbInstanceDetails_licenseModel,
    awsRdsDbInstanceDetails_statusInfos,
    awsRdsDbInstanceDetails_vpcSecurityGroups,

    -- ** AwsRdsDbInstanceEndpoint
    awsRdsDbInstanceEndpoint_port,
    awsRdsDbInstanceEndpoint_hostedZoneId,
    awsRdsDbInstanceEndpoint_address,

    -- ** AwsRdsDbInstanceVpcSecurityGroup
    awsRdsDbInstanceVpcSecurityGroup_status,
    awsRdsDbInstanceVpcSecurityGroup_vpcSecurityGroupId,

    -- ** AwsRdsDbOptionGroupMembership
    awsRdsDbOptionGroupMembership_optionGroupName,
    awsRdsDbOptionGroupMembership_status,

    -- ** AwsRdsDbParameterGroup
    awsRdsDbParameterGroup_dbParameterGroupName,
    awsRdsDbParameterGroup_parameterApplyStatus,

    -- ** AwsRdsDbPendingModifiedValues
    awsRdsDbPendingModifiedValues_port,
    awsRdsDbPendingModifiedValues_backupRetentionPeriod,
    awsRdsDbPendingModifiedValues_dbInstanceClass,
    awsRdsDbPendingModifiedValues_dbSubnetGroupName,
    awsRdsDbPendingModifiedValues_dbInstanceIdentifier,
    awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports,
    awsRdsDbPendingModifiedValues_masterUserPassword,
    awsRdsDbPendingModifiedValues_storageType,
    awsRdsDbPendingModifiedValues_processorFeatures,
    awsRdsDbPendingModifiedValues_caCertificateIdentifier,
    awsRdsDbPendingModifiedValues_allocatedStorage,
    awsRdsDbPendingModifiedValues_iops,
    awsRdsDbPendingModifiedValues_engineVersion,
    awsRdsDbPendingModifiedValues_multiAZ,
    awsRdsDbPendingModifiedValues_licenseModel,

    -- ** AwsRdsDbProcessorFeature
    awsRdsDbProcessorFeature_name,
    awsRdsDbProcessorFeature_value,

    -- ** AwsRdsDbSecurityGroupDetails
    awsRdsDbSecurityGroupDetails_ownerId,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupName,
    awsRdsDbSecurityGroupDetails_ec2SecurityGroups,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupArn,
    awsRdsDbSecurityGroupDetails_ipRanges,
    awsRdsDbSecurityGroupDetails_vpcId,

    -- ** AwsRdsDbSecurityGroupEc2SecurityGroup
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId,
    awsRdsDbSecurityGroupEc2SecurityGroup_status,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName,

    -- ** AwsRdsDbSecurityGroupIpRange
    awsRdsDbSecurityGroupIpRange_status,
    awsRdsDbSecurityGroupIpRange_cidrIp,

    -- ** AwsRdsDbSnapshotDetails
    awsRdsDbSnapshotDetails_port,
    awsRdsDbSnapshotDetails_percentProgress,
    awsRdsDbSnapshotDetails_masterUsername,
    awsRdsDbSnapshotDetails_sourceRegion,
    awsRdsDbSnapshotDetails_dbInstanceIdentifier,
    awsRdsDbSnapshotDetails_dbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_optionGroupName,
    awsRdsDbSnapshotDetails_timezone,
    awsRdsDbSnapshotDetails_instanceCreateTime,
    awsRdsDbSnapshotDetails_status,
    awsRdsDbSnapshotDetails_availabilityZone,
    awsRdsDbSnapshotDetails_snapshotCreateTime,
    awsRdsDbSnapshotDetails_storageType,
    awsRdsDbSnapshotDetails_processorFeatures,
    awsRdsDbSnapshotDetails_tdeCredentialArn,
    awsRdsDbSnapshotDetails_encrypted,
    awsRdsDbSnapshotDetails_kmsKeyId,
    awsRdsDbSnapshotDetails_engine,
    awsRdsDbSnapshotDetails_allocatedStorage,
    awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbSnapshotDetails_vpcId,
    awsRdsDbSnapshotDetails_dbiResourceId,
    awsRdsDbSnapshotDetails_iops,
    awsRdsDbSnapshotDetails_engineVersion,
    awsRdsDbSnapshotDetails_licenseModel,
    awsRdsDbSnapshotDetails_snapshotType,

    -- ** AwsRdsDbStatusInfo
    awsRdsDbStatusInfo_message,
    awsRdsDbStatusInfo_status,
    awsRdsDbStatusInfo_normal,
    awsRdsDbStatusInfo_statusType,

    -- ** AwsRdsDbSubnetGroup
    awsRdsDbSubnetGroup_dbSubnetGroupName,
    awsRdsDbSubnetGroup_subnetGroupStatus,
    awsRdsDbSubnetGroup_subnets,
    awsRdsDbSubnetGroup_dbSubnetGroupDescription,
    awsRdsDbSubnetGroup_dbSubnetGroupArn,
    awsRdsDbSubnetGroup_vpcId,

    -- ** AwsRdsDbSubnetGroupSubnet
    awsRdsDbSubnetGroupSubnet_subnetIdentifier,
    awsRdsDbSubnetGroupSubnet_subnetStatus,
    awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone,

    -- ** AwsRdsDbSubnetGroupSubnetAvailabilityZone
    awsRdsDbSubnetGroupSubnetAvailabilityZone_name,

    -- ** AwsRdsEventSubscriptionDetails
    awsRdsEventSubscriptionDetails_subscriptionCreationTime,
    awsRdsEventSubscriptionDetails_custSubscriptionId,
    awsRdsEventSubscriptionDetails_sourceIdsList,
    awsRdsEventSubscriptionDetails_status,
    awsRdsEventSubscriptionDetails_sourceType,
    awsRdsEventSubscriptionDetails_enabled,
    awsRdsEventSubscriptionDetails_snsTopicArn,
    awsRdsEventSubscriptionDetails_eventCategoriesList,
    awsRdsEventSubscriptionDetails_eventSubscriptionArn,
    awsRdsEventSubscriptionDetails_customerAwsId,

    -- ** AwsRdsPendingCloudWatchLogsExports
    awsRdsPendingCloudWatchLogsExports_logTypesToEnable,
    awsRdsPendingCloudWatchLogsExports_logTypesToDisable,

    -- ** AwsRedshiftClusterClusterNode
    awsRedshiftClusterClusterNode_nodeRole,
    awsRedshiftClusterClusterNode_publicIpAddress,
    awsRedshiftClusterClusterNode_privateIpAddress,

    -- ** AwsRedshiftClusterClusterParameterGroup
    awsRedshiftClusterClusterParameterGroup_parameterGroupName,
    awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList,
    awsRedshiftClusterClusterParameterGroup_parameterApplyStatus,

    -- ** AwsRedshiftClusterClusterParameterStatus
    awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription,
    awsRedshiftClusterClusterParameterStatus_parameterName,
    awsRedshiftClusterClusterParameterStatus_parameterApplyStatus,

    -- ** AwsRedshiftClusterClusterSecurityGroup
    awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName,
    awsRedshiftClusterClusterSecurityGroup_status,

    -- ** AwsRedshiftClusterClusterSnapshotCopyStatus
    awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName,
    awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion,

    -- ** AwsRedshiftClusterDeferredMaintenanceWindow
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime,

    -- ** AwsRedshiftClusterDetails
    awsRedshiftClusterDetails_clusterIdentifier,
    awsRedshiftClusterDetails_clusterPublicKey,
    awsRedshiftClusterDetails_manualSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_masterUsername,
    awsRedshiftClusterDetails_clusterParameterGroups,
    awsRedshiftClusterDetails_allowVersionUpgrade,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus,
    awsRedshiftClusterDetails_clusterSubnetGroupName,
    awsRedshiftClusterDetails_snapshotScheduleIdentifier,
    awsRedshiftClusterDetails_nextMaintenanceWindowStartTime,
    awsRedshiftClusterDetails_elasticIpStatus,
    awsRedshiftClusterDetails_clusterNodes,
    awsRedshiftClusterDetails_clusterVersion,
    awsRedshiftClusterDetails_loggingStatus,
    awsRedshiftClusterDetails_pendingActions,
    awsRedshiftClusterDetails_clusterRevisionNumber,
    awsRedshiftClusterDetails_maintenanceTrackName,
    awsRedshiftClusterDetails_iamRoles,
    awsRedshiftClusterDetails_availabilityZone,
    awsRedshiftClusterDetails_nodeType,
    awsRedshiftClusterDetails_publiclyAccessible,
    awsRedshiftClusterDetails_clusterSnapshotCopyStatus,
    awsRedshiftClusterDetails_snapshotScheduleState,
    awsRedshiftClusterDetails_hsmStatus,
    awsRedshiftClusterDetails_clusterStatus,
    awsRedshiftClusterDetails_resizeInfo,
    awsRedshiftClusterDetails_deferredMaintenanceWindows,
    awsRedshiftClusterDetails_encrypted,
    awsRedshiftClusterDetails_numberOfNodes,
    awsRedshiftClusterDetails_kmsKeyId,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime,
    awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions,
    awsRedshiftClusterDetails_pendingModifiedValues,
    awsRedshiftClusterDetails_enhancedVpcRouting,
    awsRedshiftClusterDetails_preferredMaintenanceWindow,
    awsRedshiftClusterDetails_clusterSecurityGroups,
    awsRedshiftClusterDetails_endpoint,
    awsRedshiftClusterDetails_vpcId,
    awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_clusterCreateTime,
    awsRedshiftClusterDetails_dbName,
    awsRedshiftClusterDetails_restoreStatus,
    awsRedshiftClusterDetails_clusterAvailabilityStatus,
    awsRedshiftClusterDetails_vpcSecurityGroups,

    -- ** AwsRedshiftClusterElasticIpStatus
    awsRedshiftClusterElasticIpStatus_elasticIp,
    awsRedshiftClusterElasticIpStatus_status,

    -- ** AwsRedshiftClusterEndpoint
    awsRedshiftClusterEndpoint_port,
    awsRedshiftClusterEndpoint_address,

    -- ** AwsRedshiftClusterHsmStatus
    awsRedshiftClusterHsmStatus_status,
    awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier,
    awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier,

    -- ** AwsRedshiftClusterIamRole
    awsRedshiftClusterIamRole_iamRoleArn,
    awsRedshiftClusterIamRole_applyStatus,

    -- ** AwsRedshiftClusterLoggingStatus
    awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime,
    awsRedshiftClusterLoggingStatus_s3KeyPrefix,
    awsRedshiftClusterLoggingStatus_lastFailureMessage,
    awsRedshiftClusterLoggingStatus_loggingEnabled,
    awsRedshiftClusterLoggingStatus_bucketName,
    awsRedshiftClusterLoggingStatus_lastFailureTime,

    -- ** AwsRedshiftClusterPendingModifiedValues
    awsRedshiftClusterPendingModifiedValues_clusterIdentifier,
    awsRedshiftClusterPendingModifiedValues_clusterVersion,
    awsRedshiftClusterPendingModifiedValues_maintenanceTrackName,
    awsRedshiftClusterPendingModifiedValues_masterUserPassword,
    awsRedshiftClusterPendingModifiedValues_nodeType,
    awsRedshiftClusterPendingModifiedValues_publiclyAccessible,
    awsRedshiftClusterPendingModifiedValues_encryptionType,
    awsRedshiftClusterPendingModifiedValues_numberOfNodes,
    awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting,
    awsRedshiftClusterPendingModifiedValues_clusterType,
    awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod,

    -- ** AwsRedshiftClusterResizeInfo
    awsRedshiftClusterResizeInfo_allowCancelResize,
    awsRedshiftClusterResizeInfo_resizeType,

    -- ** AwsRedshiftClusterRestoreStatus
    awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes,
    awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond,
    awsRedshiftClusterRestoreStatus_status,
    awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds,
    awsRedshiftClusterRestoreStatus_progressInMegaBytes,
    awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds,

    -- ** AwsRedshiftClusterVpcSecurityGroup
    awsRedshiftClusterVpcSecurityGroup_status,
    awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId,

    -- ** AwsS3AccountPublicAccessBlockDetails
    awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets,
    awsS3AccountPublicAccessBlockDetails_ignorePublicAcls,
    awsS3AccountPublicAccessBlockDetails_blockPublicPolicy,
    awsS3AccountPublicAccessBlockDetails_blockPublicAcls,

    -- ** AwsS3BucketBucketLifecycleConfigurationDetails
    awsS3BucketBucketLifecycleConfigurationDetails_rules,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
    awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesDetails
    awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_status,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_id,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_filter,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_key,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_value,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_key,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_value,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
    awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_days,
    awsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails_storageClass,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_date,
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_days,
    awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_storageClass,

    -- ** AwsS3BucketBucketVersioningConfiguration
    awsS3BucketBucketVersioningConfiguration_isMfaDeleteEnabled,
    awsS3BucketBucketVersioningConfiguration_status,

    -- ** AwsS3BucketDetails
    awsS3BucketDetails_accessControlList,
    awsS3BucketDetails_ownerName,
    awsS3BucketDetails_ownerId,
    awsS3BucketDetails_bucketVersioningConfiguration,
    awsS3BucketDetails_bucketNotificationConfiguration,
    awsS3BucketDetails_serverSideEncryptionConfiguration,
    awsS3BucketDetails_bucketLoggingConfiguration,
    awsS3BucketDetails_ownerAccountId,
    awsS3BucketDetails_bucketWebsiteConfiguration,
    awsS3BucketDetails_bucketLifecycleConfiguration,
    awsS3BucketDetails_createdAt,
    awsS3BucketDetails_publicAccessBlockConfiguration,

    -- ** AwsS3BucketLoggingConfiguration
    awsS3BucketLoggingConfiguration_destinationBucketName,
    awsS3BucketLoggingConfiguration_logFilePrefix,

    -- ** AwsS3BucketNotificationConfiguration
    awsS3BucketNotificationConfiguration_configurations,

    -- ** AwsS3BucketNotificationConfigurationDetail
    awsS3BucketNotificationConfigurationDetail_destination,
    awsS3BucketNotificationConfigurationDetail_type,
    awsS3BucketNotificationConfigurationDetail_filter,
    awsS3BucketNotificationConfigurationDetail_events,

    -- ** AwsS3BucketNotificationConfigurationFilter
    awsS3BucketNotificationConfigurationFilter_s3KeyFilter,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilter
    awsS3BucketNotificationConfigurationS3KeyFilter_filterRules,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilterRule
    awsS3BucketNotificationConfigurationS3KeyFilterRule_name,
    awsS3BucketNotificationConfigurationS3KeyFilterRule_value,

    -- ** AwsS3BucketServerSideEncryptionByDefault
    awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID,
    awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm,

    -- ** AwsS3BucketServerSideEncryptionConfiguration
    awsS3BucketServerSideEncryptionConfiguration_rules,

    -- ** AwsS3BucketServerSideEncryptionRule
    awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault,

    -- ** AwsS3BucketWebsiteConfiguration
    awsS3BucketWebsiteConfiguration_routingRules,
    awsS3BucketWebsiteConfiguration_errorDocument,
    awsS3BucketWebsiteConfiguration_redirectAllRequestsTo,
    awsS3BucketWebsiteConfiguration_indexDocumentSuffix,

    -- ** AwsS3BucketWebsiteConfigurationRedirectTo
    awsS3BucketWebsiteConfigurationRedirectTo_hostname,
    awsS3BucketWebsiteConfigurationRedirectTo_protocol,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRule
    awsS3BucketWebsiteConfigurationRoutingRule_condition,
    awsS3BucketWebsiteConfigurationRoutingRule_redirect,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRuleCondition
    awsS3BucketWebsiteConfigurationRoutingRuleCondition_httpErrorCodeReturnedEquals,
    awsS3BucketWebsiteConfigurationRoutingRuleCondition_keyPrefixEquals,

    -- ** AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode,

    -- ** AwsS3ObjectDetails
    awsS3ObjectDetails_serverSideEncryption,
    awsS3ObjectDetails_sSEKMSKeyId,
    awsS3ObjectDetails_lastModified,
    awsS3ObjectDetails_eTag,
    awsS3ObjectDetails_contentType,
    awsS3ObjectDetails_versionId,

    -- ** AwsSecretsManagerSecretDetails
    awsSecretsManagerSecretDetails_name,
    awsSecretsManagerSecretDetails_rotationLambdaArn,
    awsSecretsManagerSecretDetails_rotationRules,
    awsSecretsManagerSecretDetails_deleted,
    awsSecretsManagerSecretDetails_description,
    awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency,
    awsSecretsManagerSecretDetails_rotationEnabled,
    awsSecretsManagerSecretDetails_kmsKeyId,

    -- ** AwsSecretsManagerSecretRotationRules
    awsSecretsManagerSecretRotationRules_automaticallyAfterDays,

    -- ** AwsSecurityFinding
    awsSecurityFinding_productName,
    awsSecurityFinding_criticality,
    awsSecurityFinding_severity,
    awsSecurityFinding_network,
    awsSecurityFinding_recordState,
    awsSecurityFinding_relatedFindings,
    awsSecurityFinding_productFields,
    awsSecurityFinding_companyName,
    awsSecurityFinding_vulnerabilities,
    awsSecurityFinding_threatIntelIndicators,
    awsSecurityFinding_remediation,
    awsSecurityFinding_confidence,
    awsSecurityFinding_patchSummary,
    awsSecurityFinding_userDefinedFields,
    awsSecurityFinding_malware,
    awsSecurityFinding_networkPath,
    awsSecurityFinding_types,
    awsSecurityFinding_region,
    awsSecurityFinding_firstObservedAt,
    awsSecurityFinding_verificationState,
    awsSecurityFinding_lastObservedAt,
    awsSecurityFinding_threats,
    awsSecurityFinding_action,
    awsSecurityFinding_findingProviderFields,
    awsSecurityFinding_process,
    awsSecurityFinding_workflow,
    awsSecurityFinding_note,
    awsSecurityFinding_sourceUrl,
    awsSecurityFinding_workflowState,
    awsSecurityFinding_compliance,
    awsSecurityFinding_sample,
    awsSecurityFinding_schemaVersion,
    awsSecurityFinding_id,
    awsSecurityFinding_productArn,
    awsSecurityFinding_generatorId,
    awsSecurityFinding_awsAccountId,
    awsSecurityFinding_createdAt,
    awsSecurityFinding_updatedAt,
    awsSecurityFinding_title,
    awsSecurityFinding_description,
    awsSecurityFinding_resources,

    -- ** AwsSecurityFindingFilters
    awsSecurityFindingFilters_noteText,
    awsSecurityFindingFilters_malwarePath,
    awsSecurityFindingFilters_resourceId,
    awsSecurityFindingFilters_severityProduct,
    awsSecurityFindingFilters_productName,
    awsSecurityFindingFilters_networkProtocol,
    awsSecurityFindingFilters_networkDestinationIpV6,
    awsSecurityFindingFilters_awsAccountId,
    awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt,
    awsSecurityFindingFilters_resourceType,
    awsSecurityFindingFilters_recommendationText,
    awsSecurityFindingFilters_criticality,
    awsSecurityFindingFilters_resourceAwsEc2InstanceImageId,
    awsSecurityFindingFilters_recordState,
    awsSecurityFindingFilters_processPid,
    awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal,
    awsSecurityFindingFilters_networkSourceMac,
    awsSecurityFindingFilters_type,
    awsSecurityFindingFilters_productFields,
    awsSecurityFindingFilters_companyName,
    awsSecurityFindingFilters_threatIntelIndicatorValue,
    awsSecurityFindingFilters_processTerminatedAt,
    awsSecurityFindingFilters_networkDestinationIpV4,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId,
    awsSecurityFindingFilters_resourceAwsIamUserUserName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceType,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses,
    awsSecurityFindingFilters_severityNormalized,
    awsSecurityFindingFilters_resourceContainerLaunchedAt,
    awsSecurityFindingFilters_networkDestinationPort,
    awsSecurityFindingFilters_severityLabel,
    awsSecurityFindingFilters_confidence,
    awsSecurityFindingFilters_userDefinedFields,
    awsSecurityFindingFilters_noteUpdatedAt,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerName,
    awsSecurityFindingFilters_relatedFindingsId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn,
    awsSecurityFindingFilters_findingProviderFieldsConfidence,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn,
    awsSecurityFindingFilters_complianceStatus,
    awsSecurityFindingFilters_networkDestinationDomain,
    awsSecurityFindingFilters_networkSourceDomain,
    awsSecurityFindingFilters_threatIntelIndicatorSource,
    awsSecurityFindingFilters_malwareState,
    awsSecurityFindingFilters_resourceTags,
    awsSecurityFindingFilters_resourcePartition,
    awsSecurityFindingFilters_id,
    awsSecurityFindingFilters_description,
    awsSecurityFindingFilters_malwareType,
    awsSecurityFindingFilters_threatIntelIndicatorSourceUrl,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName,
    awsSecurityFindingFilters_networkSourceIpV6,
    awsSecurityFindingFilters_region,
    awsSecurityFindingFilters_networkDirection,
    awsSecurityFindingFilters_title,
    awsSecurityFindingFilters_firstObservedAt,
    awsSecurityFindingFilters_resourceRegion,
    awsSecurityFindingFilters_keyword,
    awsSecurityFindingFilters_productArn,
    awsSecurityFindingFilters_verificationState,
    awsSecurityFindingFilters_resourceContainerName,
    awsSecurityFindingFilters_threatIntelIndicatorCategory,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName,
    awsSecurityFindingFilters_findingProviderFieldsCriticality,
    awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId,
    awsSecurityFindingFilters_lastObservedAt,
    awsSecurityFindingFilters_malwareName,
    awsSecurityFindingFilters_networkSourceIpV4,
    awsSecurityFindingFilters_processName,
    awsSecurityFindingFilters_networkSourcePort,
    awsSecurityFindingFilters_noteUpdatedBy,
    awsSecurityFindingFilters_processLaunchedAt,
    awsSecurityFindingFilters_threatIntelIndicatorType,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus,
    awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId,
    awsSecurityFindingFilters_findingProviderFieldsTypes,
    awsSecurityFindingFilters_workflowStatus,
    awsSecurityFindingFilters_sourceUrl,
    awsSecurityFindingFilters_processParentPid,
    awsSecurityFindingFilters_createdAt,
    awsSecurityFindingFilters_findingProviderFieldsSeverityLabel,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses,
    awsSecurityFindingFilters_workflowState,
    awsSecurityFindingFilters_generatorId,
    awsSecurityFindingFilters_updatedAt,
    awsSecurityFindingFilters_resourceContainerImageId,
    awsSecurityFindingFilters_resourceContainerImageName,
    awsSecurityFindingFilters_processPath,
    awsSecurityFindingFilters_sample,
    awsSecurityFindingFilters_relatedFindingsProductArn,
    awsSecurityFindingFilters_resourceDetailsOther,

    -- ** AwsSecurityFindingIdentifier
    awsSecurityFindingIdentifier_id,
    awsSecurityFindingIdentifier_productArn,

    -- ** AwsSnsTopicDetails
    awsSnsTopicDetails_kmsMasterKeyId,
    awsSnsTopicDetails_sqsSuccessFeedbackRoleArn,
    awsSnsTopicDetails_sqsFailureFeedbackRoleArn,
    awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn,
    awsSnsTopicDetails_httpSuccessFeedbackRoleArn,
    awsSnsTopicDetails_owner,
    awsSnsTopicDetails_httpFailureFeedbackRoleArn,
    awsSnsTopicDetails_applicationSuccessFeedbackRoleArn,
    awsSnsTopicDetails_topicName,
    awsSnsTopicDetails_subscription,
    awsSnsTopicDetails_firehoseFailureFeedbackRoleArn,

    -- ** AwsSnsTopicSubscription
    awsSnsTopicSubscription_protocol,
    awsSnsTopicSubscription_endpoint,

    -- ** AwsSqsQueueDetails
    awsSqsQueueDetails_kmsMasterKeyId,
    awsSqsQueueDetails_queueName,
    awsSqsQueueDetails_deadLetterTargetArn,
    awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds,

    -- ** AwsSsmComplianceSummary
    awsSsmComplianceSummary_compliantInformationalCount,
    awsSsmComplianceSummary_executionType,
    awsSsmComplianceSummary_compliantCriticalCount,
    awsSsmComplianceSummary_compliantUnspecifiedCount,
    awsSsmComplianceSummary_patchBaselineId,
    awsSsmComplianceSummary_nonCompliantHighCount,
    awsSsmComplianceSummary_patchGroup,
    awsSsmComplianceSummary_nonCompliantInformationalCount,
    awsSsmComplianceSummary_status,
    awsSsmComplianceSummary_nonCompliantCriticalCount,
    awsSsmComplianceSummary_compliantHighCount,
    awsSsmComplianceSummary_compliantMediumCount,
    awsSsmComplianceSummary_nonCompliantUnspecifiedCount,
    awsSsmComplianceSummary_nonCompliantMediumCount,
    awsSsmComplianceSummary_compliantLowCount,
    awsSsmComplianceSummary_nonCompliantLowCount,
    awsSsmComplianceSummary_complianceType,
    awsSsmComplianceSummary_overallSeverity,

    -- ** AwsSsmPatch
    awsSsmPatch_complianceSummary,

    -- ** AwsSsmPatchComplianceDetails
    awsSsmPatchComplianceDetails_patch,

    -- ** AwsWafRateBasedRuleDetails
    awsWafRateBasedRuleDetails_name,
    awsWafRateBasedRuleDetails_ruleId,
    awsWafRateBasedRuleDetails_matchPredicates,
    awsWafRateBasedRuleDetails_rateLimit,
    awsWafRateBasedRuleDetails_metricName,
    awsWafRateBasedRuleDetails_rateKey,

    -- ** AwsWafRateBasedRuleMatchPredicate
    awsWafRateBasedRuleMatchPredicate_type,
    awsWafRateBasedRuleMatchPredicate_dataId,
    awsWafRateBasedRuleMatchPredicate_negated,

    -- ** AwsWafRegionalRateBasedRuleDetails
    awsWafRegionalRateBasedRuleDetails_name,
    awsWafRegionalRateBasedRuleDetails_ruleId,
    awsWafRegionalRateBasedRuleDetails_matchPredicates,
    awsWafRegionalRateBasedRuleDetails_rateLimit,
    awsWafRegionalRateBasedRuleDetails_metricName,
    awsWafRegionalRateBasedRuleDetails_rateKey,

    -- ** AwsWafRegionalRateBasedRuleMatchPredicate
    awsWafRegionalRateBasedRuleMatchPredicate_type,
    awsWafRegionalRateBasedRuleMatchPredicate_dataId,
    awsWafRegionalRateBasedRuleMatchPredicate_negated,

    -- ** AwsWafRegionalRuleDetails
    awsWafRegionalRuleDetails_name,
    awsWafRegionalRuleDetails_predicateList,
    awsWafRegionalRuleDetails_ruleId,
    awsWafRegionalRuleDetails_metricName,

    -- ** AwsWafRegionalRuleGroupDetails
    awsWafRegionalRuleGroupDetails_name,
    awsWafRegionalRuleGroupDetails_rules,
    awsWafRegionalRuleGroupDetails_ruleGroupId,
    awsWafRegionalRuleGroupDetails_metricName,

    -- ** AwsWafRegionalRuleGroupRulesActionDetails
    awsWafRegionalRuleGroupRulesActionDetails_type,

    -- ** AwsWafRegionalRuleGroupRulesDetails
    awsWafRegionalRuleGroupRulesDetails_type,
    awsWafRegionalRuleGroupRulesDetails_ruleId,
    awsWafRegionalRuleGroupRulesDetails_priority,
    awsWafRegionalRuleGroupRulesDetails_action,

    -- ** AwsWafRegionalRulePredicateListDetails
    awsWafRegionalRulePredicateListDetails_type,
    awsWafRegionalRulePredicateListDetails_dataId,
    awsWafRegionalRulePredicateListDetails_negated,

    -- ** AwsWafRegionalWebAclDetails
    awsWafRegionalWebAclDetails_name,
    awsWafRegionalWebAclDetails_rulesList,
    awsWafRegionalWebAclDetails_webAclId,
    awsWafRegionalWebAclDetails_metricName,
    awsWafRegionalWebAclDetails_defaultAction,

    -- ** AwsWafRegionalWebAclRulesListActionDetails
    awsWafRegionalWebAclRulesListActionDetails_type,

    -- ** AwsWafRegionalWebAclRulesListDetails
    awsWafRegionalWebAclRulesListDetails_type,
    awsWafRegionalWebAclRulesListDetails_ruleId,
    awsWafRegionalWebAclRulesListDetails_overrideAction,
    awsWafRegionalWebAclRulesListDetails_priority,
    awsWafRegionalWebAclRulesListDetails_action,

    -- ** AwsWafRegionalWebAclRulesListOverrideActionDetails
    awsWafRegionalWebAclRulesListOverrideActionDetails_type,

    -- ** AwsWafRuleDetails
    awsWafRuleDetails_name,
    awsWafRuleDetails_predicateList,
    awsWafRuleDetails_ruleId,
    awsWafRuleDetails_metricName,

    -- ** AwsWafRuleGroupDetails
    awsWafRuleGroupDetails_name,
    awsWafRuleGroupDetails_rules,
    awsWafRuleGroupDetails_ruleGroupId,
    awsWafRuleGroupDetails_metricName,

    -- ** AwsWafRuleGroupRulesActionDetails
    awsWafRuleGroupRulesActionDetails_type,

    -- ** AwsWafRuleGroupRulesDetails
    awsWafRuleGroupRulesDetails_type,
    awsWafRuleGroupRulesDetails_ruleId,
    awsWafRuleGroupRulesDetails_priority,
    awsWafRuleGroupRulesDetails_action,

    -- ** AwsWafRulePredicateListDetails
    awsWafRulePredicateListDetails_type,
    awsWafRulePredicateListDetails_dataId,
    awsWafRulePredicateListDetails_negated,

    -- ** AwsWafWebAclDetails
    awsWafWebAclDetails_name,
    awsWafWebAclDetails_rules,
    awsWafWebAclDetails_webAclId,
    awsWafWebAclDetails_defaultAction,

    -- ** AwsWafWebAclRule
    awsWafWebAclRule_type,
    awsWafWebAclRule_ruleId,
    awsWafWebAclRule_overrideAction,
    awsWafWebAclRule_priority,
    awsWafWebAclRule_action,
    awsWafWebAclRule_excludedRules,

    -- ** AwsXrayEncryptionConfigDetails
    awsXrayEncryptionConfigDetails_type,
    awsXrayEncryptionConfigDetails_status,
    awsXrayEncryptionConfigDetails_keyId,

    -- ** BatchUpdateFindingsUnprocessedFinding
    batchUpdateFindingsUnprocessedFinding_findingIdentifier,
    batchUpdateFindingsUnprocessedFinding_errorCode,
    batchUpdateFindingsUnprocessedFinding_errorMessage,

    -- ** BooleanFilter
    booleanFilter_value,

    -- ** Cell
    cell_row,
    cell_cellReference,
    cell_columnName,
    cell_column,

    -- ** CidrBlockAssociation
    cidrBlockAssociation_cidrBlockState,
    cidrBlockAssociation_cidrBlock,
    cidrBlockAssociation_associationId,

    -- ** City
    city_cityName,

    -- ** ClassificationResult
    classificationResult_status,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_sensitiveData,

    -- ** ClassificationStatus
    classificationStatus_code,
    classificationStatus_reason,

    -- ** Compliance
    compliance_relatedRequirements,
    compliance_status,
    compliance_statusReasons,

    -- ** ContainerDetails
    containerDetails_name,
    containerDetails_containerRuntime,
    containerDetails_privileged,
    containerDetails_launchedAt,
    containerDetails_volumeMounts,
    containerDetails_imageName,
    containerDetails_imageId,

    -- ** Country
    country_countryName,
    country_countryCode,

    -- ** CustomDataIdentifiersDetections
    customDataIdentifiersDetections_occurrences,
    customDataIdentifiersDetections_name,
    customDataIdentifiersDetections_arn,
    customDataIdentifiersDetections_count,

    -- ** CustomDataIdentifiersResult
    customDataIdentifiersResult_detections,
    customDataIdentifiersResult_totalCount,

    -- ** Cvss
    cvss_baseScore,
    cvss_baseVector,
    cvss_adjustments,
    cvss_source,
    cvss_version,

    -- ** DataClassificationDetails
    dataClassificationDetails_detailedResultsLocation,
    dataClassificationDetails_result,

    -- ** DateFilter
    dateFilter_start,
    dateFilter_dateRange,
    dateFilter_end,

    -- ** DateRange
    dateRange_unit,
    dateRange_value,

    -- ** DnsRequestAction
    dnsRequestAction_domain,
    dnsRequestAction_blocked,
    dnsRequestAction_protocol,

    -- ** FilePaths
    filePaths_resourceId,
    filePaths_filePath,
    filePaths_hash,
    filePaths_fileName,

    -- ** FindingAggregator
    findingAggregator_findingAggregatorArn,

    -- ** FindingProviderFields
    findingProviderFields_criticality,
    findingProviderFields_severity,
    findingProviderFields_relatedFindings,
    findingProviderFields_confidence,
    findingProviderFields_types,

    -- ** FindingProviderSeverity
    findingProviderSeverity_label,
    findingProviderSeverity_original,

    -- ** FirewallPolicyDetails
    firewallPolicyDetails_statelessCustomActions,
    firewallPolicyDetails_statefulRuleGroupReferences,
    firewallPolicyDetails_statelessRuleGroupReferences,
    firewallPolicyDetails_statelessDefaultActions,
    firewallPolicyDetails_statelessFragmentDefaultActions,

    -- ** FirewallPolicyStatefulRuleGroupReferencesDetails
    firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn,

    -- ** FirewallPolicyStatelessCustomActionsDetails
    firewallPolicyStatelessCustomActionsDetails_actionName,
    firewallPolicyStatelessCustomActionsDetails_actionDefinition,

    -- ** FirewallPolicyStatelessRuleGroupReferencesDetails
    firewallPolicyStatelessRuleGroupReferencesDetails_priority,
    firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn,

    -- ** GeoLocation
    geoLocation_lat,
    geoLocation_lon,

    -- ** IcmpTypeCode
    icmpTypeCode_type,
    icmpTypeCode_code,

    -- ** ImportFindingsError
    importFindingsError_id,
    importFindingsError_errorCode,
    importFindingsError_errorMessage,

    -- ** Insight
    insight_insightArn,
    insight_name,
    insight_filters,
    insight_groupByAttribute,

    -- ** InsightResultValue
    insightResultValue_groupByAttributeValue,
    insightResultValue_count,

    -- ** InsightResults
    insightResults_insightArn,
    insightResults_groupByAttribute,
    insightResults_resultValues,

    -- ** Invitation
    invitation_memberStatus,
    invitation_accountId,
    invitation_invitedAt,
    invitation_invitationId,

    -- ** IpFilter
    ipFilter_cidr,

    -- ** IpOrganizationDetails
    ipOrganizationDetails_isp,
    ipOrganizationDetails_org,
    ipOrganizationDetails_asn,
    ipOrganizationDetails_asnOrg,

    -- ** Ipv6CidrBlockAssociation
    ipv6CidrBlockAssociation_cidrBlockState,
    ipv6CidrBlockAssociation_associationId,
    ipv6CidrBlockAssociation_ipv6CidrBlock,

    -- ** KeywordFilter
    keywordFilter_value,

    -- ** LoadBalancerState
    loadBalancerState_code,
    loadBalancerState_reason,

    -- ** Malware
    malware_type,
    malware_state,
    malware_path,
    malware_name,

    -- ** MapFilter
    mapFilter_key,
    mapFilter_comparison,
    mapFilter_value,

    -- ** Member
    member_email,
    member_administratorId,
    member_memberStatus,
    member_accountId,
    member_masterId,
    member_invitedAt,
    member_updatedAt,

    -- ** Network
    network_destinationDomain,
    network_sourceIpV4,
    network_openPortRange,
    network_destinationIpV6,
    network_sourceIpV6,
    network_sourceDomain,
    network_destinationIpV4,
    network_destinationPort,
    network_sourcePort,
    network_sourceMac,
    network_protocol,
    network_direction,

    -- ** NetworkConnectionAction
    networkConnectionAction_connectionDirection,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_protocol,
    networkConnectionAction_remotePortDetails,

    -- ** NetworkHeader
    networkHeader_destination,
    networkHeader_source,
    networkHeader_protocol,

    -- ** NetworkPathComponent
    networkPathComponent_egress,
    networkPathComponent_ingress,
    networkPathComponent_componentId,
    networkPathComponent_componentType,

    -- ** NetworkPathComponentDetails
    networkPathComponentDetails_address,
    networkPathComponentDetails_portRanges,

    -- ** Note
    note_text,
    note_updatedBy,
    note_updatedAt,

    -- ** NoteUpdate
    noteUpdate_text,
    noteUpdate_updatedBy,

    -- ** NumberFilter
    numberFilter_lte,
    numberFilter_gte,
    numberFilter_eq,

    -- ** Occurrences
    occurrences_records,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_cells,
    occurrences_pages,

    -- ** Page
    page_offsetRange,
    page_pageNumber,
    page_lineRange,

    -- ** PatchSummary
    patchSummary_installedOtherCount,
    patchSummary_rebootOption,
    patchSummary_failedCount,
    patchSummary_operationEndTime,
    patchSummary_installedCount,
    patchSummary_installedRejectedCount,
    patchSummary_operationStartTime,
    patchSummary_missingCount,
    patchSummary_operation,
    patchSummary_installedPendingReboot,
    patchSummary_id,

    -- ** PortProbeAction
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- ** PortProbeDetail
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_localIpDetails,

    -- ** PortRange
    portRange_begin,
    portRange_end,

    -- ** PortRangeFromTo
    portRangeFromTo_from,
    portRangeFromTo_to,

    -- ** ProcessDetails
    processDetails_parentPid,
    processDetails_name,
    processDetails_pid,
    processDetails_terminatedAt,
    processDetails_path,
    processDetails_launchedAt,

    -- ** Product
    product_integrationTypes,
    product_productName,
    product_companyName,
    product_activationUrl,
    product_description,
    product_productSubscriptionResourcePolicy,
    product_categories,
    product_marketplaceUrl,
    product_productArn,

    -- ** Range
    range_start,
    range_end,
    range_startColumn,

    -- ** Recommendation
    recommendation_url,
    recommendation_text,

    -- ** Record
    record_jsonPath,
    record_recordIndex,

    -- ** RelatedFinding
    relatedFinding_productArn,
    relatedFinding_id,

    -- ** Remediation
    remediation_recommendation,

    -- ** Resource
    resource_tags,
    resource_dataClassification,
    resource_resourceRole,
    resource_details,
    resource_partition,
    resource_region,
    resource_type,
    resource_id,

    -- ** ResourceDetails
    resourceDetails_awsNetworkFirewallRuleGroup,
    resourceDetails_awsEc2Instance,
    resourceDetails_awsIamRole,
    resourceDetails_awsS3AccountPublicAccessBlock,
    resourceDetails_awsSqsQueue,
    resourceDetails_awsElbv2LoadBalancer,
    resourceDetails_awsCloudWatchAlarm,
    resourceDetails_awsIamPolicy,
    resourceDetails_awsEc2NetworkInterface,
    resourceDetails_awsEksCluster,
    resourceDetails_awsWafRateBasedRule,
    resourceDetails_awsEcsContainer,
    resourceDetails_awsRdsEventSubscription,
    resourceDetails_awsCloudFormationStack,
    resourceDetails_awsWafWebAcl,
    resourceDetails_other,
    resourceDetails_awsSsmPatchCompliance,
    resourceDetails_awsOpenSearchServiceDomain,
    resourceDetails_awsDynamoDbTable,
    resourceDetails_awsEc2Volume,
    resourceDetails_awsCertificateManagerCertificate,
    resourceDetails_awsEc2VpnConnection,
    resourceDetails_awsEc2Subnet,
    resourceDetails_awsIamGroup,
    resourceDetails_awsIamUser,
    resourceDetails_awsBackupBackupVault,
    resourceDetails_awsApiGatewayV2Api,
    resourceDetails_awsEcsTaskDefinition,
    resourceDetails_awsAutoScalingAutoScalingGroup,
    resourceDetails_awsEc2NetworkAcl,
    resourceDetails_awsEfsAccessPoint,
    resourceDetails_awsEc2VpcEndpointService,
    resourceDetails_awsCodeBuildProject,
    resourceDetails_awsRdsDbInstance,
    resourceDetails_awsElasticsearchDomain,
    resourceDetails_awsEcrContainerImage,
    resourceDetails_awsAutoScalingLaunchConfiguration,
    resourceDetails_awsIamAccessKey,
    resourceDetails_awsRedshiftCluster,
    resourceDetails_awsNetworkFirewallFirewallPolicy,
    resourceDetails_awsEc2Vpc,
    resourceDetails_awsEcrRepository,
    resourceDetails_awsWafRegionalRuleGroup,
    resourceDetails_awsWafRegionalRateBasedRule,
    resourceDetails_awsWafRegionalRule,
    resourceDetails_awsApiGatewayV2Stage,
    resourceDetails_awsRdsDbCluster,
    resourceDetails_awsS3Object,
    resourceDetails_awsNetworkFirewallFirewall,
    resourceDetails_awsWafRuleGroup,
    resourceDetails_awsEc2VpcPeeringConnection,
    resourceDetails_awsEc2TransitGateway,
    resourceDetails_awsS3Bucket,
    resourceDetails_awsWafRegionalWebAcl,
    resourceDetails_awsEc2Eip,
    resourceDetails_awsBackupBackupPlan,
    resourceDetails_awsLambdaFunction,
    resourceDetails_container,
    resourceDetails_awsRdsDbClusterSnapshot,
    resourceDetails_awsElbLoadBalancer,
    resourceDetails_awsCloudTrailTrail,
    resourceDetails_awsEcsService,
    resourceDetails_awsApiGatewayStage,
    resourceDetails_awsSnsTopic,
    resourceDetails_awsRdsDbSecurityGroup,
    resourceDetails_awsKinesisStream,
    resourceDetails_awsEc2SecurityGroup,
    resourceDetails_awsEcsCluster,
    resourceDetails_awsApiGatewayRestApi,
    resourceDetails_awsWafRule,
    resourceDetails_awsBackupRecoveryPoint,
    resourceDetails_awsRdsDbSnapshot,
    resourceDetails_awsElasticBeanstalkEnvironment,
    resourceDetails_awsEcsTask,
    resourceDetails_awsXrayEncryptionConfig,
    resourceDetails_awsLambdaLayerVersion,
    resourceDetails_awsKmsKey,
    resourceDetails_awsSecretsManagerSecret,
    resourceDetails_awsCloudFrontDistribution,

    -- ** Result
    result_processingResult,
    result_accountId,

    -- ** RuleGroupDetails
    ruleGroupDetails_rulesSource,
    ruleGroupDetails_ruleVariables,

    -- ** RuleGroupSource
    ruleGroupSource_rulesString,
    ruleGroupSource_statefulRules,
    ruleGroupSource_rulesSourceList,
    ruleGroupSource_statelessRulesAndCustomActions,

    -- ** RuleGroupSourceCustomActionsDetails
    ruleGroupSourceCustomActionsDetails_actionName,
    ruleGroupSourceCustomActionsDetails_actionDefinition,

    -- ** RuleGroupSourceListDetails
    ruleGroupSourceListDetails_targetTypes,
    ruleGroupSourceListDetails_generatedRulesType,
    ruleGroupSourceListDetails_targets,

    -- ** RuleGroupSourceStatefulRulesDetails
    ruleGroupSourceStatefulRulesDetails_header,
    ruleGroupSourceStatefulRulesDetails_ruleOptions,
    ruleGroupSourceStatefulRulesDetails_action,

    -- ** RuleGroupSourceStatefulRulesHeaderDetails
    ruleGroupSourceStatefulRulesHeaderDetails_destination,
    ruleGroupSourceStatefulRulesHeaderDetails_destinationPort,
    ruleGroupSourceStatefulRulesHeaderDetails_sourcePort,
    ruleGroupSourceStatefulRulesHeaderDetails_source,
    ruleGroupSourceStatefulRulesHeaderDetails_protocol,
    ruleGroupSourceStatefulRulesHeaderDetails_direction,

    -- ** RuleGroupSourceStatefulRulesOptionsDetails
    ruleGroupSourceStatefulRulesOptionsDetails_settings,
    ruleGroupSourceStatefulRulesOptionsDetails_keyword,

    -- ** RuleGroupSourceStatelessRuleDefinition
    ruleGroupSourceStatelessRuleDefinition_matchAttributes,
    ruleGroupSourceStatelessRuleDefinition_actions,

    -- ** RuleGroupSourceStatelessRuleMatchAttributes
    ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributes_sources,
    ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags,
    ruleGroupSourceStatelessRuleMatchAttributes_protocols,
    ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributes_destinations,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinations
    ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSources
    ruleGroupSourceStatelessRuleMatchAttributesSources_addressDefinition,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags,
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks,

    -- ** RuleGroupSourceStatelessRulesAndCustomActionsDetails
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions,
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules,

    -- ** RuleGroupSourceStatelessRulesDetails
    ruleGroupSourceStatelessRulesDetails_ruleDefinition,
    ruleGroupSourceStatelessRulesDetails_priority,

    -- ** RuleGroupVariables
    ruleGroupVariables_ipSets,
    ruleGroupVariables_portSets,

    -- ** RuleGroupVariablesIpSetsDetails
    ruleGroupVariablesIpSetsDetails_definition,

    -- ** RuleGroupVariablesPortSetsDetails
    ruleGroupVariablesPortSetsDetails_definition,

    -- ** SensitiveDataDetections
    sensitiveDataDetections_occurrences,
    sensitiveDataDetections_type,
    sensitiveDataDetections_count,

    -- ** SensitiveDataResult
    sensitiveDataResult_detections,
    sensitiveDataResult_category,
    sensitiveDataResult_totalCount,

    -- ** Severity
    severity_product,
    severity_label,
    severity_normalized,
    severity_original,

    -- ** SeverityUpdate
    severityUpdate_product,
    severityUpdate_label,
    severityUpdate_normalized,

    -- ** SoftwarePackage
    softwarePackage_filePath,
    softwarePackage_name,
    softwarePackage_sourceLayerArn,
    softwarePackage_fixedInVersion,
    softwarePackage_remediation,
    softwarePackage_epoch,
    softwarePackage_packageManager,
    softwarePackage_release,
    softwarePackage_sourceLayerHash,
    softwarePackage_architecture,
    softwarePackage_version,

    -- ** SortCriterion
    sortCriterion_sortOrder,
    sortCriterion_field,

    -- ** Standard
    standard_name,
    standard_standardsArn,
    standard_description,
    standard_enabledByDefault,

    -- ** StandardsControl
    standardsControl_controlId,
    standardsControl_standardsControlArn,
    standardsControl_severityRating,
    standardsControl_controlStatusUpdatedAt,
    standardsControl_disabledReason,
    standardsControl_relatedRequirements,
    standardsControl_description,
    standardsControl_title,
    standardsControl_remediationUrl,
    standardsControl_controlStatus,

    -- ** StandardsStatusReason
    standardsStatusReason_statusReasonCode,

    -- ** StandardsSubscription
    standardsSubscription_standardsStatusReason,
    standardsSubscription_standardsSubscriptionArn,
    standardsSubscription_standardsArn,
    standardsSubscription_standardsInput,
    standardsSubscription_standardsStatus,

    -- ** StandardsSubscriptionRequest
    standardsSubscriptionRequest_standardsInput,
    standardsSubscriptionRequest_standardsArn,

    -- ** StatelessCustomActionDefinition
    statelessCustomActionDefinition_publishMetricAction,

    -- ** StatelessCustomPublishMetricAction
    statelessCustomPublishMetricAction_dimensions,

    -- ** StatelessCustomPublishMetricActionDimension
    statelessCustomPublishMetricActionDimension_value,

    -- ** StatusReason
    statusReason_description,
    statusReason_reasonCode,

    -- ** StringFilter
    stringFilter_comparison,
    stringFilter_value,

    -- ** Threat
    threat_severity,
    threat_name,
    threat_itemCount,
    threat_filePaths,

    -- ** ThreatIntelIndicator
    threatIntelIndicator_type,
    threatIntelIndicator_source,
    threatIntelIndicator_lastObservedAt,
    threatIntelIndicator_category,
    threatIntelIndicator_sourceUrl,
    threatIntelIndicator_value,

    -- ** VolumeMount
    volumeMount_name,
    volumeMount_mountPath,

    -- ** VpcInfoCidrBlockSetDetails
    vpcInfoCidrBlockSetDetails_cidrBlock,

    -- ** VpcInfoIpv6CidrBlockSetDetails
    vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock,

    -- ** VpcInfoPeeringOptionsDetails
    vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc,

    -- ** Vulnerability
    vulnerability_vulnerablePackages,
    vulnerability_referenceUrls,
    vulnerability_cvss,
    vulnerability_relatedVulnerabilities,
    vulnerability_vendor,
    vulnerability_fixAvailable,
    vulnerability_id,

    -- ** VulnerabilityVendor
    vulnerabilityVendor_vendorUpdatedAt,
    vulnerabilityVendor_url,
    vulnerabilityVendor_vendorCreatedAt,
    vulnerabilityVendor_vendorSeverity,
    vulnerabilityVendor_name,

    -- ** WafAction
    wafAction_type,

    -- ** WafExcludedRule
    wafExcludedRule_ruleId,

    -- ** WafOverrideAction
    wafOverrideAction_type,

    -- ** Workflow
    workflow_status,

    -- ** WorkflowUpdate
    workflowUpdate_status,
  )
where

import Amazonka.SecurityHub.AcceptAdministratorInvitation
import Amazonka.SecurityHub.BatchDisableStandards
import Amazonka.SecurityHub.BatchEnableStandards
import Amazonka.SecurityHub.BatchImportFindings
import Amazonka.SecurityHub.BatchUpdateFindings
import Amazonka.SecurityHub.CreateActionTarget
import Amazonka.SecurityHub.CreateFindingAggregator
import Amazonka.SecurityHub.CreateInsight
import Amazonka.SecurityHub.CreateMembers
import Amazonka.SecurityHub.DeclineInvitations
import Amazonka.SecurityHub.DeleteActionTarget
import Amazonka.SecurityHub.DeleteFindingAggregator
import Amazonka.SecurityHub.DeleteInsight
import Amazonka.SecurityHub.DeleteInvitations
import Amazonka.SecurityHub.DeleteMembers
import Amazonka.SecurityHub.DescribeActionTargets
import Amazonka.SecurityHub.DescribeHub
import Amazonka.SecurityHub.DescribeOrganizationConfiguration
import Amazonka.SecurityHub.DescribeProducts
import Amazonka.SecurityHub.DescribeStandards
import Amazonka.SecurityHub.DescribeStandardsControls
import Amazonka.SecurityHub.DisableImportFindingsForProduct
import Amazonka.SecurityHub.DisableOrganizationAdminAccount
import Amazonka.SecurityHub.DisableSecurityHub
import Amazonka.SecurityHub.DisassociateFromAdministratorAccount
import Amazonka.SecurityHub.DisassociateMembers
import Amazonka.SecurityHub.EnableImportFindingsForProduct
import Amazonka.SecurityHub.EnableOrganizationAdminAccount
import Amazonka.SecurityHub.EnableSecurityHub
import Amazonka.SecurityHub.GetAdministratorAccount
import Amazonka.SecurityHub.GetEnabledStandards
import Amazonka.SecurityHub.GetFindingAggregator
import Amazonka.SecurityHub.GetFindings
import Amazonka.SecurityHub.GetInsightResults
import Amazonka.SecurityHub.GetInsights
import Amazonka.SecurityHub.GetInvitationsCount
import Amazonka.SecurityHub.GetMembers
import Amazonka.SecurityHub.InviteMembers
import Amazonka.SecurityHub.ListEnabledProductsForImport
import Amazonka.SecurityHub.ListFindingAggregators
import Amazonka.SecurityHub.ListInvitations
import Amazonka.SecurityHub.ListMembers
import Amazonka.SecurityHub.ListOrganizationAdminAccounts
import Amazonka.SecurityHub.ListTagsForResource
import Amazonka.SecurityHub.TagResource
import Amazonka.SecurityHub.Types.AccountDetails
import Amazonka.SecurityHub.Types.Action
import Amazonka.SecurityHub.Types.ActionLocalIpDetails
import Amazonka.SecurityHub.Types.ActionLocalPortDetails
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails
import Amazonka.SecurityHub.Types.ActionRemotePortDetails
import Amazonka.SecurityHub.Types.ActionTarget
import Amazonka.SecurityHub.Types.Adjustment
import Amazonka.SecurityHub.Types.AdminAccount
import Amazonka.SecurityHub.Types.AvailabilityZone
import Amazonka.SecurityHub.Types.AwsApiCallAction
import Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayAccessLogSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayCanarySettings
import Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration
import Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings
import Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationMetadataOptions
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanAdvancedBackupSettingsDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanBackupPlanDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleCopyActionsDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultNotificationsDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCreatedByDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointLifecycleDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateOptions
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateRenewalSummary
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateResourceRecord
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDriftInformationDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackOutputsDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginCustomOriginConfig
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailover
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginItem
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginS3OriginConfig
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginSslProtocols
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate
import Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDetails
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDimensionsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectArtifactsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironment
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectLogsConfigS3LogsDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectSource
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectVpcConfig
import Amazonka.SecurityHub.Types.AwsCorsConfiguration
import Amazonka.SecurityHub.Types.AwsDynamoDbTableAttributeDefinition
import Amazonka.SecurityHub.Types.AwsDynamoDbTableBillingModeSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails
import Amazonka.SecurityHub.Types.AwsDynamoDbTableGlobalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Amazonka.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplica
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableSseDescription
import Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification
import Amazonka.SecurityHub.Types.AwsEc2EipDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceMetadataOptions
import Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclEntry
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpPermission
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpRange
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupIpv6Range
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupPrefixListId
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupUserIdGroupPair
import Amazonka.SecurityHub.Types.AwsEc2SubnetDetails
import Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails
import Amazonka.SecurityHub.Types.AwsEc2VolumeAttachment
import Amazonka.SecurityHub.Types.AwsEc2VolumeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsTunnelOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionRoutesDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionVgwTelemetryDetails
import Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterClusterSettingsDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDetails
import Amazonka.SecurityHub.Types.AwsEcsContainerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentControllerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceLoadBalancersDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsServicePlacementStrategiesDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceServiceRegistriesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesHostDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeHostDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointPosixUserDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryCreationInfoDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryDetails
import Amazonka.SecurityHub.Types.AwsEksClusterDetails
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingClusterLoggingDetails
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingDetails
import Amazonka.SecurityHub.Types.AwsEksClusterResourcesVpcConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentOptionSetting
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentTier
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDomainEndpointOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainEncryptionAtRestOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainNodeToNodeEncryptionOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainServiceSoftwareOptions
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainVPCOptions
import Amazonka.SecurityHub.Types.AwsElbAppCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLbCookieStickinessPolicy
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAccessLog
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAdditionalAttribute
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAttributes
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerBackendServerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionDraining
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionSettings
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerCrossZoneLoadBalancing
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerHealthCheck
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerInstance
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListener
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListenerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerAttribute
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsIamAccessKeyDetails
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextAttributes
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContextSessionIssuer
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamGroupDetails
import Amazonka.SecurityHub.Types.AwsIamGroupPolicy
import Amazonka.SecurityHub.Types.AwsIamInstanceProfile
import Amazonka.SecurityHub.Types.AwsIamInstanceProfileRole
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamPolicyDetails
import Amazonka.SecurityHub.Types.AwsIamPolicyVersion
import Amazonka.SecurityHub.Types.AwsIamRoleDetails
import Amazonka.SecurityHub.Types.AwsIamRolePolicy
import Amazonka.SecurityHub.Types.AwsIamUserDetails
import Amazonka.SecurityHub.Types.AwsIamUserPolicy
import Amazonka.SecurityHub.Types.AwsKinesisStreamDetails
import Amazonka.SecurityHub.Types.AwsKinesisStreamStreamEncryptionDetails
import Amazonka.SecurityHub.Types.AwsKmsKeyDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionCode
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDeadLetterConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironment
import Amazonka.SecurityHub.Types.AwsLambdaFunctionEnvironmentError
import Amazonka.SecurityHub.Types.AwsLambdaFunctionLayer
import Amazonka.SecurityHub.Types.AwsLambdaFunctionTracingConfig
import Amazonka.SecurityHub.Types.AwsLambdaFunctionVpcConfig
import Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails
import Amazonka.SecurityHub.Types.AwsMountPoint
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallPolicyDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallSubnetMappingsDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainMasterUserOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainVpcOptionsDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterMember
import Amazonka.SecurityHub.Types.AwsRdsDbClusterOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceEndpoint
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbParameterGroup
import Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupEc2SecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupIpRange
import Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbStatusInfo
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroup
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone
import Amazonka.SecurityHub.Types.AwsRdsEventSubscriptionDetails
import Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails
import Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint
import Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole
import Amazonka.SecurityHub.Types.AwsRedshiftClusterLoggingStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo
import Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup
import Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketVersioningConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketDetails
import Amazonka.SecurityHub.Types.AwsS3BucketLoggingConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationDetail
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationFilter
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilter
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRule
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
import Amazonka.SecurityHub.Types.AwsS3ObjectDetails
import Amazonka.SecurityHub.Types.AwsSecretsManagerSecretDetails
import Amazonka.SecurityHub.Types.AwsSecretsManagerSecretRotationRules
import Amazonka.SecurityHub.Types.AwsSecurityFinding
import Amazonka.SecurityHub.Types.AwsSecurityFindingFilters
import Amazonka.SecurityHub.Types.AwsSecurityFindingIdentifier
import Amazonka.SecurityHub.Types.AwsSnsTopicDetails
import Amazonka.SecurityHub.Types.AwsSnsTopicSubscription
import Amazonka.SecurityHub.Types.AwsSqsQueueDetails
import Amazonka.SecurityHub.Types.AwsSsmComplianceSummary
import Amazonka.SecurityHub.Types.AwsSsmPatch
import Amazonka.SecurityHub.Types.AwsSsmPatchComplianceDetails
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleMatchPredicate
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleMatchPredicate
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupRulesDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRulePredicateListDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListActionDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclRulesListOverrideActionDetails
import Amazonka.SecurityHub.Types.AwsWafRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupRulesDetails
import Amazonka.SecurityHub.Types.AwsWafRulePredicateListDetails
import Amazonka.SecurityHub.Types.AwsWafWebAclDetails
import Amazonka.SecurityHub.Types.AwsWafWebAclRule
import Amazonka.SecurityHub.Types.AwsXrayEncryptionConfigDetails
import Amazonka.SecurityHub.Types.BatchUpdateFindingsUnprocessedFinding
import Amazonka.SecurityHub.Types.BooleanFilter
import Amazonka.SecurityHub.Types.Cell
import Amazonka.SecurityHub.Types.CidrBlockAssociation
import Amazonka.SecurityHub.Types.City
import Amazonka.SecurityHub.Types.ClassificationResult
import Amazonka.SecurityHub.Types.ClassificationStatus
import Amazonka.SecurityHub.Types.Compliance
import Amazonka.SecurityHub.Types.ContainerDetails
import Amazonka.SecurityHub.Types.Country
import Amazonka.SecurityHub.Types.CustomDataIdentifiersDetections
import Amazonka.SecurityHub.Types.CustomDataIdentifiersResult
import Amazonka.SecurityHub.Types.Cvss
import Amazonka.SecurityHub.Types.DataClassificationDetails
import Amazonka.SecurityHub.Types.DateFilter
import Amazonka.SecurityHub.Types.DateRange
import Amazonka.SecurityHub.Types.DnsRequestAction
import Amazonka.SecurityHub.Types.FilePaths
import Amazonka.SecurityHub.Types.FindingAggregator
import Amazonka.SecurityHub.Types.FindingProviderFields
import Amazonka.SecurityHub.Types.FindingProviderSeverity
import Amazonka.SecurityHub.Types.FirewallPolicyDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.GeoLocation
import Amazonka.SecurityHub.Types.IcmpTypeCode
import Amazonka.SecurityHub.Types.ImportFindingsError
import Amazonka.SecurityHub.Types.Insight
import Amazonka.SecurityHub.Types.InsightResultValue
import Amazonka.SecurityHub.Types.InsightResults
import Amazonka.SecurityHub.Types.Invitation
import Amazonka.SecurityHub.Types.IpFilter
import Amazonka.SecurityHub.Types.IpOrganizationDetails
import Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation
import Amazonka.SecurityHub.Types.KeywordFilter
import Amazonka.SecurityHub.Types.LoadBalancerState
import Amazonka.SecurityHub.Types.Malware
import Amazonka.SecurityHub.Types.MapFilter
import Amazonka.SecurityHub.Types.Member
import Amazonka.SecurityHub.Types.Network
import Amazonka.SecurityHub.Types.NetworkConnectionAction
import Amazonka.SecurityHub.Types.NetworkHeader
import Amazonka.SecurityHub.Types.NetworkPathComponent
import Amazonka.SecurityHub.Types.NetworkPathComponentDetails
import Amazonka.SecurityHub.Types.Note
import Amazonka.SecurityHub.Types.NoteUpdate
import Amazonka.SecurityHub.Types.NumberFilter
import Amazonka.SecurityHub.Types.Occurrences
import Amazonka.SecurityHub.Types.Page
import Amazonka.SecurityHub.Types.PatchSummary
import Amazonka.SecurityHub.Types.PortProbeAction
import Amazonka.SecurityHub.Types.PortProbeDetail
import Amazonka.SecurityHub.Types.PortRange
import Amazonka.SecurityHub.Types.PortRangeFromTo
import Amazonka.SecurityHub.Types.ProcessDetails
import Amazonka.SecurityHub.Types.Product
import Amazonka.SecurityHub.Types.Range
import Amazonka.SecurityHub.Types.Recommendation
import Amazonka.SecurityHub.Types.Record
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.Remediation
import Amazonka.SecurityHub.Types.Resource
import Amazonka.SecurityHub.Types.ResourceDetails
import Amazonka.SecurityHub.Types.Result
import Amazonka.SecurityHub.Types.RuleGroupDetails
import Amazonka.SecurityHub.Types.RuleGroupSource
import Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceListDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesHeaderDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatefulRulesOptionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleDefinition
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributes
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinations
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSources
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesAndCustomActionsDetails
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRulesDetails
import Amazonka.SecurityHub.Types.RuleGroupVariables
import Amazonka.SecurityHub.Types.RuleGroupVariablesIpSetsDetails
import Amazonka.SecurityHub.Types.RuleGroupVariablesPortSetsDetails
import Amazonka.SecurityHub.Types.SensitiveDataDetections
import Amazonka.SecurityHub.Types.SensitiveDataResult
import Amazonka.SecurityHub.Types.Severity
import Amazonka.SecurityHub.Types.SeverityUpdate
import Amazonka.SecurityHub.Types.SoftwarePackage
import Amazonka.SecurityHub.Types.SortCriterion
import Amazonka.SecurityHub.Types.Standard
import Amazonka.SecurityHub.Types.StandardsControl
import Amazonka.SecurityHub.Types.StandardsStatusReason
import Amazonka.SecurityHub.Types.StandardsSubscription
import Amazonka.SecurityHub.Types.StandardsSubscriptionRequest
import Amazonka.SecurityHub.Types.StatelessCustomActionDefinition
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricAction
import Amazonka.SecurityHub.Types.StatelessCustomPublishMetricActionDimension
import Amazonka.SecurityHub.Types.StatusReason
import Amazonka.SecurityHub.Types.StringFilter
import Amazonka.SecurityHub.Types.Threat
import Amazonka.SecurityHub.Types.ThreatIntelIndicator
import Amazonka.SecurityHub.Types.VolumeMount
import Amazonka.SecurityHub.Types.VpcInfoCidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoIpv6CidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoPeeringOptionsDetails
import Amazonka.SecurityHub.Types.Vulnerability
import Amazonka.SecurityHub.Types.VulnerabilityVendor
import Amazonka.SecurityHub.Types.WafAction
import Amazonka.SecurityHub.Types.WafExcludedRule
import Amazonka.SecurityHub.Types.WafOverrideAction
import Amazonka.SecurityHub.Types.Workflow
import Amazonka.SecurityHub.Types.WorkflowUpdate
import Amazonka.SecurityHub.UntagResource
import Amazonka.SecurityHub.UpdateActionTarget
import Amazonka.SecurityHub.UpdateFindingAggregator
import Amazonka.SecurityHub.UpdateFindings
import Amazonka.SecurityHub.UpdateInsight
import Amazonka.SecurityHub.UpdateOrganizationConfiguration
import Amazonka.SecurityHub.UpdateSecurityHubConfiguration
import Amazonka.SecurityHub.UpdateStandardsControl

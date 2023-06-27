{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** BatchDeleteAutomationRules
    batchDeleteAutomationRules_automationRulesArns,
    batchDeleteAutomationRulesResponse_processedAutomationRules,
    batchDeleteAutomationRulesResponse_unprocessedAutomationRules,
    batchDeleteAutomationRulesResponse_httpStatus,

    -- ** BatchDisableStandards
    batchDisableStandards_standardsSubscriptionArns,
    batchDisableStandardsResponse_standardsSubscriptions,
    batchDisableStandardsResponse_httpStatus,

    -- ** BatchEnableStandards
    batchEnableStandards_standardsSubscriptionRequests,
    batchEnableStandardsResponse_standardsSubscriptions,
    batchEnableStandardsResponse_httpStatus,

    -- ** BatchGetAutomationRules
    batchGetAutomationRules_automationRulesArns,
    batchGetAutomationRulesResponse_rules,
    batchGetAutomationRulesResponse_unprocessedAutomationRules,
    batchGetAutomationRulesResponse_httpStatus,

    -- ** BatchGetSecurityControls
    batchGetSecurityControls_securityControlIds,
    batchGetSecurityControlsResponse_unprocessedIds,
    batchGetSecurityControlsResponse_httpStatus,
    batchGetSecurityControlsResponse_securityControls,

    -- ** BatchGetStandardsControlAssociations
    batchGetStandardsControlAssociations_standardsControlAssociationIds,
    batchGetStandardsControlAssociationsResponse_unprocessedAssociations,
    batchGetStandardsControlAssociationsResponse_httpStatus,
    batchGetStandardsControlAssociationsResponse_standardsControlAssociationDetails,

    -- ** BatchImportFindings
    batchImportFindings_findings,
    batchImportFindingsResponse_failedFindings,
    batchImportFindingsResponse_httpStatus,
    batchImportFindingsResponse_failedCount,
    batchImportFindingsResponse_successCount,

    -- ** BatchUpdateAutomationRules
    batchUpdateAutomationRules_updateAutomationRulesRequestItems,
    batchUpdateAutomationRulesResponse_processedAutomationRules,
    batchUpdateAutomationRulesResponse_unprocessedAutomationRules,
    batchUpdateAutomationRulesResponse_httpStatus,

    -- ** BatchUpdateFindings
    batchUpdateFindings_confidence,
    batchUpdateFindings_criticality,
    batchUpdateFindings_note,
    batchUpdateFindings_relatedFindings,
    batchUpdateFindings_severity,
    batchUpdateFindings_types,
    batchUpdateFindings_userDefinedFields,
    batchUpdateFindings_verificationState,
    batchUpdateFindings_workflow,
    batchUpdateFindings_findingIdentifiers,
    batchUpdateFindingsResponse_httpStatus,
    batchUpdateFindingsResponse_processedFindings,
    batchUpdateFindingsResponse_unprocessedFindings,

    -- ** BatchUpdateStandardsControlAssociations
    batchUpdateStandardsControlAssociations_standardsControlAssociationUpdates,
    batchUpdateStandardsControlAssociationsResponse_unprocessedAssociationUpdates,
    batchUpdateStandardsControlAssociationsResponse_httpStatus,

    -- ** CreateActionTarget
    createActionTarget_name,
    createActionTarget_description,
    createActionTarget_id,
    createActionTargetResponse_httpStatus,
    createActionTargetResponse_actionTargetArn,

    -- ** CreateAutomationRule
    createAutomationRule_isTerminal,
    createAutomationRule_ruleStatus,
    createAutomationRule_tags,
    createAutomationRule_ruleOrder,
    createAutomationRule_ruleName,
    createAutomationRule_description,
    createAutomationRule_criteria,
    createAutomationRule_actions,
    createAutomationRuleResponse_ruleArn,
    createAutomationRuleResponse_httpStatus,

    -- ** CreateFindingAggregator
    createFindingAggregator_regions,
    createFindingAggregator_regionLinkingMode,
    createFindingAggregatorResponse_findingAggregationRegion,
    createFindingAggregatorResponse_findingAggregatorArn,
    createFindingAggregatorResponse_regionLinkingMode,
    createFindingAggregatorResponse_regions,
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
    describeActionTargets_actionTargetArns,
    describeActionTargets_maxResults,
    describeActionTargets_nextToken,
    describeActionTargetsResponse_nextToken,
    describeActionTargetsResponse_httpStatus,
    describeActionTargetsResponse_actionTargets,

    -- ** DescribeHub
    describeHub_hubArn,
    describeHubResponse_autoEnableControls,
    describeHubResponse_controlFindingGenerator,
    describeHubResponse_hubArn,
    describeHubResponse_subscribedAt,
    describeHubResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_autoEnableStandards,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** DescribeProducts
    describeProducts_maxResults,
    describeProducts_nextToken,
    describeProducts_productArn,
    describeProductsResponse_nextToken,
    describeProductsResponse_httpStatus,
    describeProductsResponse_products,

    -- ** DescribeStandards
    describeStandards_maxResults,
    describeStandards_nextToken,
    describeStandardsResponse_nextToken,
    describeStandardsResponse_standards,
    describeStandardsResponse_httpStatus,

    -- ** DescribeStandardsControls
    describeStandardsControls_maxResults,
    describeStandardsControls_nextToken,
    describeStandardsControls_standardsSubscriptionArn,
    describeStandardsControlsResponse_controls,
    describeStandardsControlsResponse_nextToken,
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
    enableSecurityHub_controlFindingGenerator,
    enableSecurityHub_enableDefaultStandards,
    enableSecurityHub_tags,
    enableSecurityHubResponse_httpStatus,

    -- ** GetAdministratorAccount
    getAdministratorAccountResponse_administrator,
    getAdministratorAccountResponse_httpStatus,

    -- ** GetEnabledStandards
    getEnabledStandards_maxResults,
    getEnabledStandards_nextToken,
    getEnabledStandards_standardsSubscriptionArns,
    getEnabledStandardsResponse_nextToken,
    getEnabledStandardsResponse_standardsSubscriptions,
    getEnabledStandardsResponse_httpStatus,

    -- ** GetFindingAggregator
    getFindingAggregator_findingAggregatorArn,
    getFindingAggregatorResponse_findingAggregationRegion,
    getFindingAggregatorResponse_findingAggregatorArn,
    getFindingAggregatorResponse_regionLinkingMode,
    getFindingAggregatorResponse_regions,
    getFindingAggregatorResponse_httpStatus,

    -- ** GetFindingHistory
    getFindingHistory_endTime,
    getFindingHistory_maxResults,
    getFindingHistory_nextToken,
    getFindingHistory_startTime,
    getFindingHistory_findingIdentifier,
    getFindingHistoryResponse_nextToken,
    getFindingHistoryResponse_records,
    getFindingHistoryResponse_httpStatus,

    -- ** GetFindings
    getFindings_filters,
    getFindings_maxResults,
    getFindings_nextToken,
    getFindings_sortCriteria,
    getFindingsResponse_nextToken,
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,

    -- ** GetInsightResults
    getInsightResults_insightArn,
    getInsightResultsResponse_httpStatus,
    getInsightResultsResponse_insightResults,

    -- ** GetInsights
    getInsights_insightArns,
    getInsights_maxResults,
    getInsights_nextToken,
    getInsightsResponse_nextToken,
    getInsightsResponse_httpStatus,
    getInsightsResponse_insights,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** GetMembers
    getMembers_accountIds,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,
    getMembersResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_accountIds,
    inviteMembersResponse_unprocessedAccounts,
    inviteMembersResponse_httpStatus,

    -- ** ListAutomationRules
    listAutomationRules_maxResults,
    listAutomationRules_nextToken,
    listAutomationRulesResponse_automationRulesMetadata,
    listAutomationRulesResponse_nextToken,
    listAutomationRulesResponse_httpStatus,

    -- ** ListEnabledProductsForImport
    listEnabledProductsForImport_maxResults,
    listEnabledProductsForImport_nextToken,
    listEnabledProductsForImportResponse_nextToken,
    listEnabledProductsForImportResponse_productSubscriptions,
    listEnabledProductsForImportResponse_httpStatus,

    -- ** ListFindingAggregators
    listFindingAggregators_maxResults,
    listFindingAggregators_nextToken,
    listFindingAggregatorsResponse_findingAggregators,
    listFindingAggregatorsResponse_nextToken,
    listFindingAggregatorsResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_maxResults,
    listInvitations_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListSecurityControlDefinitions
    listSecurityControlDefinitions_maxResults,
    listSecurityControlDefinitions_nextToken,
    listSecurityControlDefinitions_standardsArn,
    listSecurityControlDefinitionsResponse_nextToken,
    listSecurityControlDefinitionsResponse_httpStatus,
    listSecurityControlDefinitionsResponse_securityControlDefinitions,

    -- ** ListStandardsControlAssociations
    listStandardsControlAssociations_maxResults,
    listStandardsControlAssociations_nextToken,
    listStandardsControlAssociations_securityControlId,
    listStandardsControlAssociationsResponse_nextToken,
    listStandardsControlAssociationsResponse_httpStatus,
    listStandardsControlAssociationsResponse_standardsControlAssociationSummaries,

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
    updateActionTarget_description,
    updateActionTarget_name,
    updateActionTarget_actionTargetArn,
    updateActionTargetResponse_httpStatus,

    -- ** UpdateFindingAggregator
    updateFindingAggregator_regions,
    updateFindingAggregator_findingAggregatorArn,
    updateFindingAggregator_regionLinkingMode,
    updateFindingAggregatorResponse_findingAggregationRegion,
    updateFindingAggregatorResponse_findingAggregatorArn,
    updateFindingAggregatorResponse_regionLinkingMode,
    updateFindingAggregatorResponse_regions,
    updateFindingAggregatorResponse_httpStatus,

    -- ** UpdateFindings
    updateFindings_note,
    updateFindings_recordState,
    updateFindings_filters,
    updateFindingsResponse_httpStatus,

    -- ** UpdateInsight
    updateInsight_filters,
    updateInsight_groupByAttribute,
    updateInsight_name,
    updateInsight_insightArn,
    updateInsightResponse_httpStatus,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnableStandards,
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** UpdateSecurityHubConfiguration
    updateSecurityHubConfiguration_autoEnableControls,
    updateSecurityHubConfiguration_controlFindingGenerator,
    updateSecurityHubConfigurationResponse_httpStatus,

    -- ** UpdateStandardsControl
    updateStandardsControl_controlStatus,
    updateStandardsControl_disabledReason,
    updateStandardsControl_standardsControlArn,
    updateStandardsControlResponse_httpStatus,

    -- * Types

    -- ** AccountDetails
    accountDetails_email,
    accountDetails_accountId,

    -- ** Action
    action_actionType,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_networkConnectionAction,
    action_portProbeAction,

    -- ** ActionLocalIpDetails
    actionLocalIpDetails_ipAddressV4,

    -- ** ActionLocalPortDetails
    actionLocalPortDetails_port,
    actionLocalPortDetails_portName,

    -- ** ActionRemoteIpDetails
    actionRemoteIpDetails_city,
    actionRemoteIpDetails_country,
    actionRemoteIpDetails_geoLocation,
    actionRemoteIpDetails_ipAddressV4,
    actionRemoteIpDetails_organization,

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
    adminAccount_accountId,
    adminAccount_status,

    -- ** AssociatedStandard
    associatedStandard_standardsId,

    -- ** AssociationSetDetails
    associationSetDetails_associationState,
    associationSetDetails_gatewayId,
    associationSetDetails_main,
    associationSetDetails_routeTableAssociationId,
    associationSetDetails_routeTableId,
    associationSetDetails_subnetId,

    -- ** AssociationStateDetails
    associationStateDetails_state,
    associationStateDetails_statusMessage,

    -- ** AutomationRulesAction
    automationRulesAction_findingFieldsUpdate,
    automationRulesAction_type,

    -- ** AutomationRulesConfig
    automationRulesConfig_actions,
    automationRulesConfig_createdAt,
    automationRulesConfig_createdBy,
    automationRulesConfig_criteria,
    automationRulesConfig_description,
    automationRulesConfig_isTerminal,
    automationRulesConfig_ruleArn,
    automationRulesConfig_ruleName,
    automationRulesConfig_ruleOrder,
    automationRulesConfig_ruleStatus,
    automationRulesConfig_updatedAt,

    -- ** AutomationRulesFindingFieldsUpdate
    automationRulesFindingFieldsUpdate_confidence,
    automationRulesFindingFieldsUpdate_criticality,
    automationRulesFindingFieldsUpdate_note,
    automationRulesFindingFieldsUpdate_relatedFindings,
    automationRulesFindingFieldsUpdate_severity,
    automationRulesFindingFieldsUpdate_types,
    automationRulesFindingFieldsUpdate_userDefinedFields,
    automationRulesFindingFieldsUpdate_verificationState,
    automationRulesFindingFieldsUpdate_workflow,

    -- ** AutomationRulesFindingFilters
    automationRulesFindingFilters_awsAccountId,
    automationRulesFindingFilters_companyName,
    automationRulesFindingFilters_complianceAssociatedStandardsId,
    automationRulesFindingFilters_complianceSecurityControlId,
    automationRulesFindingFilters_complianceStatus,
    automationRulesFindingFilters_confidence,
    automationRulesFindingFilters_createdAt,
    automationRulesFindingFilters_criticality,
    automationRulesFindingFilters_description,
    automationRulesFindingFilters_firstObservedAt,
    automationRulesFindingFilters_generatorId,
    automationRulesFindingFilters_id,
    automationRulesFindingFilters_lastObservedAt,
    automationRulesFindingFilters_noteText,
    automationRulesFindingFilters_noteUpdatedAt,
    automationRulesFindingFilters_noteUpdatedBy,
    automationRulesFindingFilters_productArn,
    automationRulesFindingFilters_productName,
    automationRulesFindingFilters_recordState,
    automationRulesFindingFilters_relatedFindingsId,
    automationRulesFindingFilters_relatedFindingsProductArn,
    automationRulesFindingFilters_resourceDetailsOther,
    automationRulesFindingFilters_resourceId,
    automationRulesFindingFilters_resourcePartition,
    automationRulesFindingFilters_resourceRegion,
    automationRulesFindingFilters_resourceTags,
    automationRulesFindingFilters_resourceType,
    automationRulesFindingFilters_severityLabel,
    automationRulesFindingFilters_sourceUrl,
    automationRulesFindingFilters_title,
    automationRulesFindingFilters_type,
    automationRulesFindingFilters_updatedAt,
    automationRulesFindingFilters_userDefinedFields,
    automationRulesFindingFilters_verificationState,
    automationRulesFindingFilters_workflowStatus,

    -- ** AutomationRulesMetadata
    automationRulesMetadata_createdAt,
    automationRulesMetadata_createdBy,
    automationRulesMetadata_description,
    automationRulesMetadata_isTerminal,
    automationRulesMetadata_ruleArn,
    automationRulesMetadata_ruleName,
    automationRulesMetadata_ruleOrder,
    automationRulesMetadata_ruleStatus,
    automationRulesMetadata_updatedAt,

    -- ** AvailabilityZone
    availabilityZone_subnetId,
    availabilityZone_zoneName,

    -- ** AwsAmazonMqBrokerDetails
    awsAmazonMqBrokerDetails_authenticationStrategy,
    awsAmazonMqBrokerDetails_autoMinorVersionUpgrade,
    awsAmazonMqBrokerDetails_brokerArn,
    awsAmazonMqBrokerDetails_brokerId,
    awsAmazonMqBrokerDetails_brokerName,
    awsAmazonMqBrokerDetails_deploymentMode,
    awsAmazonMqBrokerDetails_encryptionOptions,
    awsAmazonMqBrokerDetails_engineType,
    awsAmazonMqBrokerDetails_engineVersion,
    awsAmazonMqBrokerDetails_hostInstanceType,
    awsAmazonMqBrokerDetails_ldapServerMetadata,
    awsAmazonMqBrokerDetails_logs,
    awsAmazonMqBrokerDetails_maintenanceWindowStartTime,
    awsAmazonMqBrokerDetails_publiclyAccessible,
    awsAmazonMqBrokerDetails_securityGroups,
    awsAmazonMqBrokerDetails_storageType,
    awsAmazonMqBrokerDetails_subnetIds,
    awsAmazonMqBrokerDetails_users,

    -- ** AwsAmazonMqBrokerEncryptionOptionsDetails
    awsAmazonMqBrokerEncryptionOptionsDetails_kmsKeyId,
    awsAmazonMqBrokerEncryptionOptionsDetails_useAwsOwnedKey,

    -- ** AwsAmazonMqBrokerLdapServerMetadataDetails
    awsAmazonMqBrokerLdapServerMetadataDetails_hosts,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleBase,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleName,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchMatching,
    awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchSubtree,
    awsAmazonMqBrokerLdapServerMetadataDetails_serviceAccountUsername,
    awsAmazonMqBrokerLdapServerMetadataDetails_userBase,
    awsAmazonMqBrokerLdapServerMetadataDetails_userRoleName,
    awsAmazonMqBrokerLdapServerMetadataDetails_userSearchMatching,
    awsAmazonMqBrokerLdapServerMetadataDetails_userSearchSubtree,

    -- ** AwsAmazonMqBrokerLogsDetails
    awsAmazonMqBrokerLogsDetails_audit,
    awsAmazonMqBrokerLogsDetails_auditLogGroup,
    awsAmazonMqBrokerLogsDetails_general,
    awsAmazonMqBrokerLogsDetails_generalLogGroup,
    awsAmazonMqBrokerLogsDetails_pending,

    -- ** AwsAmazonMqBrokerLogsPendingDetails
    awsAmazonMqBrokerLogsPendingDetails_audit,
    awsAmazonMqBrokerLogsPendingDetails_general,

    -- ** AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_dayOfWeek,
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeOfDay,
    awsAmazonMqBrokerMaintenanceWindowStartTimeDetails_timeZone,

    -- ** AwsAmazonMqBrokerUsersDetails
    awsAmazonMqBrokerUsersDetails_pendingChange,
    awsAmazonMqBrokerUsersDetails_username,

    -- ** AwsApiCallAction
    awsApiCallAction_affectedResources,
    awsApiCallAction_api,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_firstSeen,
    awsApiCallAction_lastSeen,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_serviceName,

    -- ** AwsApiCallActionDomainDetails
    awsApiCallActionDomainDetails_domain,

    -- ** AwsApiGatewayAccessLogSettings
    awsApiGatewayAccessLogSettings_destinationArn,
    awsApiGatewayAccessLogSettings_format,

    -- ** AwsApiGatewayCanarySettings
    awsApiGatewayCanarySettings_deploymentId,
    awsApiGatewayCanarySettings_percentTraffic,
    awsApiGatewayCanarySettings_stageVariableOverrides,
    awsApiGatewayCanarySettings_useStageCache,

    -- ** AwsApiGatewayEndpointConfiguration
    awsApiGatewayEndpointConfiguration_types,

    -- ** AwsApiGatewayMethodSettings
    awsApiGatewayMethodSettings_cacheDataEncrypted,
    awsApiGatewayMethodSettings_cacheTtlInSeconds,
    awsApiGatewayMethodSettings_cachingEnabled,
    awsApiGatewayMethodSettings_dataTraceEnabled,
    awsApiGatewayMethodSettings_httpMethod,
    awsApiGatewayMethodSettings_loggingLevel,
    awsApiGatewayMethodSettings_metricsEnabled,
    awsApiGatewayMethodSettings_requireAuthorizationForCacheControl,
    awsApiGatewayMethodSettings_resourcePath,
    awsApiGatewayMethodSettings_throttlingBurstLimit,
    awsApiGatewayMethodSettings_throttlingRateLimit,
    awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy,

    -- ** AwsApiGatewayRestApiDetails
    awsApiGatewayRestApiDetails_apiKeySource,
    awsApiGatewayRestApiDetails_binaryMediaTypes,
    awsApiGatewayRestApiDetails_createdDate,
    awsApiGatewayRestApiDetails_description,
    awsApiGatewayRestApiDetails_endpointConfiguration,
    awsApiGatewayRestApiDetails_id,
    awsApiGatewayRestApiDetails_minimumCompressionSize,
    awsApiGatewayRestApiDetails_name,
    awsApiGatewayRestApiDetails_version,

    -- ** AwsApiGatewayStageDetails
    awsApiGatewayStageDetails_accessLogSettings,
    awsApiGatewayStageDetails_cacheClusterEnabled,
    awsApiGatewayStageDetails_cacheClusterSize,
    awsApiGatewayStageDetails_cacheClusterStatus,
    awsApiGatewayStageDetails_canarySettings,
    awsApiGatewayStageDetails_clientCertificateId,
    awsApiGatewayStageDetails_createdDate,
    awsApiGatewayStageDetails_deploymentId,
    awsApiGatewayStageDetails_description,
    awsApiGatewayStageDetails_documentationVersion,
    awsApiGatewayStageDetails_lastUpdatedDate,
    awsApiGatewayStageDetails_methodSettings,
    awsApiGatewayStageDetails_stageName,
    awsApiGatewayStageDetails_tracingEnabled,
    awsApiGatewayStageDetails_variables,
    awsApiGatewayStageDetails_webAclArn,

    -- ** AwsApiGatewayV2ApiDetails
    awsApiGatewayV2ApiDetails_apiEndpoint,
    awsApiGatewayV2ApiDetails_apiId,
    awsApiGatewayV2ApiDetails_apiKeySelectionExpression,
    awsApiGatewayV2ApiDetails_corsConfiguration,
    awsApiGatewayV2ApiDetails_createdDate,
    awsApiGatewayV2ApiDetails_description,
    awsApiGatewayV2ApiDetails_name,
    awsApiGatewayV2ApiDetails_protocolType,
    awsApiGatewayV2ApiDetails_routeSelectionExpression,
    awsApiGatewayV2ApiDetails_version,

    -- ** AwsApiGatewayV2RouteSettings
    awsApiGatewayV2RouteSettings_dataTraceEnabled,
    awsApiGatewayV2RouteSettings_detailedMetricsEnabled,
    awsApiGatewayV2RouteSettings_loggingLevel,
    awsApiGatewayV2RouteSettings_throttlingBurstLimit,
    awsApiGatewayV2RouteSettings_throttlingRateLimit,

    -- ** AwsApiGatewayV2StageDetails
    awsApiGatewayV2StageDetails_accessLogSettings,
    awsApiGatewayV2StageDetails_apiGatewayManaged,
    awsApiGatewayV2StageDetails_autoDeploy,
    awsApiGatewayV2StageDetails_clientCertificateId,
    awsApiGatewayV2StageDetails_createdDate,
    awsApiGatewayV2StageDetails_defaultRouteSettings,
    awsApiGatewayV2StageDetails_deploymentId,
    awsApiGatewayV2StageDetails_description,
    awsApiGatewayV2StageDetails_lastDeploymentStatusMessage,
    awsApiGatewayV2StageDetails_lastUpdatedDate,
    awsApiGatewayV2StageDetails_routeSettings,
    awsApiGatewayV2StageDetails_stageName,
    awsApiGatewayV2StageDetails_stageVariables,

    -- ** AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_authenticationType,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_lambdaAuthorizerConfig,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_openIdConnectConfig,
    awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_userPoolConfig,

    -- ** AwsAppSyncGraphQlApiDetails
    awsAppSyncGraphQlApiDetails_additionalAuthenticationProviders,
    awsAppSyncGraphQlApiDetails_apiId,
    awsAppSyncGraphQlApiDetails_arn,
    awsAppSyncGraphQlApiDetails_authenticationType,
    awsAppSyncGraphQlApiDetails_id,
    awsAppSyncGraphQlApiDetails_lambdaAuthorizerConfig,
    awsAppSyncGraphQlApiDetails_logConfig,
    awsAppSyncGraphQlApiDetails_name,
    awsAppSyncGraphQlApiDetails_openIdConnectConfig,
    awsAppSyncGraphQlApiDetails_userPoolConfig,
    awsAppSyncGraphQlApiDetails_wafWebAclArn,
    awsAppSyncGraphQlApiDetails_xrayEnabled,

    -- ** AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerResultTtlInSeconds,
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerUri,
    awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_identityValidationExpression,

    -- ** AwsAppSyncGraphQlApiLogConfigDetails
    awsAppSyncGraphQlApiLogConfigDetails_cloudWatchLogsRoleArn,
    awsAppSyncGraphQlApiLogConfigDetails_excludeVerboseContent,
    awsAppSyncGraphQlApiLogConfigDetails_fieldLogLevel,

    -- ** AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_authTtL,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_clientId,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_iatTtL,
    awsAppSyncGraphQlApiOpenIdConnectConfigDetails_issuer,

    -- ** AwsAppSyncGraphQlApiUserPoolConfigDetails
    awsAppSyncGraphQlApiUserPoolConfigDetails_appIdClientRegex,
    awsAppSyncGraphQlApiUserPoolConfigDetails_awsRegion,
    awsAppSyncGraphQlApiUserPoolConfigDetails_defaultAction,
    awsAppSyncGraphQlApiUserPoolConfigDetails_userPoolId,

    -- ** AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
    awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value,

    -- ** AwsAutoScalingAutoScalingGroupDetails
    awsAutoScalingAutoScalingGroupDetails_availabilityZones,
    awsAutoScalingAutoScalingGroupDetails_capacityRebalance,
    awsAutoScalingAutoScalingGroupDetails_createdTime,
    awsAutoScalingAutoScalingGroupDetails_healthCheckGracePeriod,
    awsAutoScalingAutoScalingGroupDetails_healthCheckType,
    awsAutoScalingAutoScalingGroupDetails_launchConfigurationName,
    awsAutoScalingAutoScalingGroupDetails_launchTemplate,
    awsAutoScalingAutoScalingGroupDetails_loadBalancerNames,
    awsAutoScalingAutoScalingGroupDetails_mixedInstancesPolicy,

    -- ** AwsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_launchTemplateName,
    awsAutoScalingAutoScalingGroupLaunchTemplateLaunchTemplateSpecification_version,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_instancesDistribution,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyDetails_launchTemplate,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_onDemandPercentageAboveBaseCapacity,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotAllocationStrategy,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotInstancePools,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyInstancesDistributionDetails_spotMaxPrice,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_launchTemplateSpecification,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateDetails_overrides,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateId,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_launchTemplateName,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateLaunchTemplateSpecification_version,

    -- ** AwsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_instanceType,
    awsAutoScalingAutoScalingGroupMixedInstancesPolicyLaunchTemplateOverridesListDetails_weightedCapacity,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_deviceName,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_ebs,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_noDevice,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails_virtualName,

    -- ** AwsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_deleteOnTermination,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_encrypted,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_iops,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_snapshotId,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeSize,
    awsAutoScalingLaunchConfigurationBlockDeviceMappingsEbsDetails_volumeType,

    -- ** AwsAutoScalingLaunchConfigurationDetails
    awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress,
    awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId,
    awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups,
    awsAutoScalingLaunchConfigurationDetails_createdTime,
    awsAutoScalingLaunchConfigurationDetails_ebsOptimized,
    awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile,
    awsAutoScalingLaunchConfigurationDetails_imageId,
    awsAutoScalingLaunchConfigurationDetails_instanceMonitoring,
    awsAutoScalingLaunchConfigurationDetails_instanceType,
    awsAutoScalingLaunchConfigurationDetails_kernelId,
    awsAutoScalingLaunchConfigurationDetails_keyName,
    awsAutoScalingLaunchConfigurationDetails_launchConfigurationName,
    awsAutoScalingLaunchConfigurationDetails_metadataOptions,
    awsAutoScalingLaunchConfigurationDetails_placementTenancy,
    awsAutoScalingLaunchConfigurationDetails_ramdiskId,
    awsAutoScalingLaunchConfigurationDetails_securityGroups,
    awsAutoScalingLaunchConfigurationDetails_spotPrice,
    awsAutoScalingLaunchConfigurationDetails_userData,

    -- ** AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails
    awsAutoScalingLaunchConfigurationInstanceMonitoringDetails_enabled,

    -- ** AwsAutoScalingLaunchConfigurationMetadataOptions
    awsAutoScalingLaunchConfigurationMetadataOptions_httpEndpoint,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpPutResponseHopLimit,
    awsAutoScalingLaunchConfigurationMetadataOptions_httpTokens,

    -- ** AwsBackupBackupPlanAdvancedBackupSettingsDetails
    awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions,
    awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType,

    -- ** AwsBackupBackupPlanBackupPlanDetails
    awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings,
    awsBackupBackupPlanBackupPlanDetails_backupPlanName,
    awsBackupBackupPlanBackupPlanDetails_backupPlanRule,

    -- ** AwsBackupBackupPlanDetails
    awsBackupBackupPlanDetails_backupPlan,
    awsBackupBackupPlanDetails_backupPlanArn,
    awsBackupBackupPlanDetails_backupPlanId,
    awsBackupBackupPlanDetails_versionId,

    -- ** AwsBackupBackupPlanLifecycleDetails
    awsBackupBackupPlanLifecycleDetails_deleteAfterDays,
    awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays,

    -- ** AwsBackupBackupPlanRuleCopyActionsDetails
    awsBackupBackupPlanRuleCopyActionsDetails_destinationBackupVaultArn,
    awsBackupBackupPlanRuleCopyActionsDetails_lifecycle,

    -- ** AwsBackupBackupPlanRuleDetails
    awsBackupBackupPlanRuleDetails_completionWindowMinutes,
    awsBackupBackupPlanRuleDetails_copyActions,
    awsBackupBackupPlanRuleDetails_enableContinuousBackup,
    awsBackupBackupPlanRuleDetails_lifecycle,
    awsBackupBackupPlanRuleDetails_ruleId,
    awsBackupBackupPlanRuleDetails_ruleName,
    awsBackupBackupPlanRuleDetails_scheduleExpression,
    awsBackupBackupPlanRuleDetails_startWindowMinutes,
    awsBackupBackupPlanRuleDetails_targetBackupVault,

    -- ** AwsBackupBackupVaultDetails
    awsBackupBackupVaultDetails_accessPolicy,
    awsBackupBackupVaultDetails_backupVaultArn,
    awsBackupBackupVaultDetails_backupVaultName,
    awsBackupBackupVaultDetails_encryptionKeyArn,
    awsBackupBackupVaultDetails_notifications,

    -- ** AwsBackupBackupVaultNotificationsDetails
    awsBackupBackupVaultNotificationsDetails_backupVaultEvents,
    awsBackupBackupVaultNotificationsDetails_snsTopicArn,

    -- ** AwsBackupRecoveryPointCalculatedLifecycleDetails
    awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt,
    awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt,

    -- ** AwsBackupRecoveryPointCreatedByDetails
    awsBackupRecoveryPointCreatedByDetails_backupPlanArn,
    awsBackupRecoveryPointCreatedByDetails_backupPlanId,
    awsBackupRecoveryPointCreatedByDetails_backupPlanVersion,
    awsBackupRecoveryPointCreatedByDetails_backupRuleId,

    -- ** AwsBackupRecoveryPointDetails
    awsBackupRecoveryPointDetails_backupSizeInBytes,
    awsBackupRecoveryPointDetails_backupVaultArn,
    awsBackupRecoveryPointDetails_backupVaultName,
    awsBackupRecoveryPointDetails_calculatedLifecycle,
    awsBackupRecoveryPointDetails_completionDate,
    awsBackupRecoveryPointDetails_createdBy,
    awsBackupRecoveryPointDetails_creationDate,
    awsBackupRecoveryPointDetails_encryptionKeyArn,
    awsBackupRecoveryPointDetails_iamRoleArn,
    awsBackupRecoveryPointDetails_isEncrypted,
    awsBackupRecoveryPointDetails_lastRestoreTime,
    awsBackupRecoveryPointDetails_lifecycle,
    awsBackupRecoveryPointDetails_recoveryPointArn,
    awsBackupRecoveryPointDetails_resourceArn,
    awsBackupRecoveryPointDetails_resourceType,
    awsBackupRecoveryPointDetails_sourceBackupVaultArn,
    awsBackupRecoveryPointDetails_status,
    awsBackupRecoveryPointDetails_statusMessage,
    awsBackupRecoveryPointDetails_storageClass,

    -- ** AwsBackupRecoveryPointLifecycleDetails
    awsBackupRecoveryPointLifecycleDetails_deleteAfterDays,
    awsBackupRecoveryPointLifecycleDetails_moveToColdStorageAfterDays,

    -- ** AwsCertificateManagerCertificateDetails
    awsCertificateManagerCertificateDetails_certificateAuthorityArn,
    awsCertificateManagerCertificateDetails_createdAt,
    awsCertificateManagerCertificateDetails_domainName,
    awsCertificateManagerCertificateDetails_domainValidationOptions,
    awsCertificateManagerCertificateDetails_extendedKeyUsages,
    awsCertificateManagerCertificateDetails_failureReason,
    awsCertificateManagerCertificateDetails_importedAt,
    awsCertificateManagerCertificateDetails_inUseBy,
    awsCertificateManagerCertificateDetails_issuedAt,
    awsCertificateManagerCertificateDetails_issuer,
    awsCertificateManagerCertificateDetails_keyAlgorithm,
    awsCertificateManagerCertificateDetails_keyUsages,
    awsCertificateManagerCertificateDetails_notAfter,
    awsCertificateManagerCertificateDetails_notBefore,
    awsCertificateManagerCertificateDetails_options,
    awsCertificateManagerCertificateDetails_renewalEligibility,
    awsCertificateManagerCertificateDetails_renewalSummary,
    awsCertificateManagerCertificateDetails_serial,
    awsCertificateManagerCertificateDetails_signatureAlgorithm,
    awsCertificateManagerCertificateDetails_status,
    awsCertificateManagerCertificateDetails_subject,
    awsCertificateManagerCertificateDetails_subjectAlternativeNames,
    awsCertificateManagerCertificateDetails_type,

    -- ** AwsCertificateManagerCertificateDomainValidationOption
    awsCertificateManagerCertificateDomainValidationOption_domainName,
    awsCertificateManagerCertificateDomainValidationOption_resourceRecord,
    awsCertificateManagerCertificateDomainValidationOption_validationDomain,
    awsCertificateManagerCertificateDomainValidationOption_validationEmails,
    awsCertificateManagerCertificateDomainValidationOption_validationMethod,
    awsCertificateManagerCertificateDomainValidationOption_validationStatus,

    -- ** AwsCertificateManagerCertificateExtendedKeyUsage
    awsCertificateManagerCertificateExtendedKeyUsage_name,
    awsCertificateManagerCertificateExtendedKeyUsage_oId,

    -- ** AwsCertificateManagerCertificateKeyUsage
    awsCertificateManagerCertificateKeyUsage_name,

    -- ** AwsCertificateManagerCertificateOptions
    awsCertificateManagerCertificateOptions_certificateTransparencyLoggingPreference,

    -- ** AwsCertificateManagerCertificateRenewalSummary
    awsCertificateManagerCertificateRenewalSummary_domainValidationOptions,
    awsCertificateManagerCertificateRenewalSummary_renewalStatus,
    awsCertificateManagerCertificateRenewalSummary_renewalStatusReason,
    awsCertificateManagerCertificateRenewalSummary_updatedAt,

    -- ** AwsCertificateManagerCertificateResourceRecord
    awsCertificateManagerCertificateResourceRecord_name,
    awsCertificateManagerCertificateResourceRecord_type,
    awsCertificateManagerCertificateResourceRecord_value,

    -- ** AwsCloudFormationStackDetails
    awsCloudFormationStackDetails_capabilities,
    awsCloudFormationStackDetails_creationTime,
    awsCloudFormationStackDetails_description,
    awsCloudFormationStackDetails_disableRollback,
    awsCloudFormationStackDetails_driftInformation,
    awsCloudFormationStackDetails_enableTerminationProtection,
    awsCloudFormationStackDetails_lastUpdatedTime,
    awsCloudFormationStackDetails_notificationArns,
    awsCloudFormationStackDetails_outputs,
    awsCloudFormationStackDetails_roleArn,
    awsCloudFormationStackDetails_stackId,
    awsCloudFormationStackDetails_stackName,
    awsCloudFormationStackDetails_stackStatus,
    awsCloudFormationStackDetails_stackStatusReason,
    awsCloudFormationStackDetails_timeoutInMinutes,

    -- ** AwsCloudFormationStackDriftInformationDetails
    awsCloudFormationStackDriftInformationDetails_stackDriftStatus,

    -- ** AwsCloudFormationStackOutputsDetails
    awsCloudFormationStackOutputsDetails_description,
    awsCloudFormationStackOutputsDetails_outputKey,
    awsCloudFormationStackOutputsDetails_outputValue,

    -- ** AwsCloudFrontDistributionCacheBehavior
    awsCloudFrontDistributionCacheBehavior_viewerProtocolPolicy,

    -- ** AwsCloudFrontDistributionCacheBehaviors
    awsCloudFrontDistributionCacheBehaviors_items,

    -- ** AwsCloudFrontDistributionDefaultCacheBehavior
    awsCloudFrontDistributionDefaultCacheBehavior_viewerProtocolPolicy,

    -- ** AwsCloudFrontDistributionDetails
    awsCloudFrontDistributionDetails_cacheBehaviors,
    awsCloudFrontDistributionDetails_defaultCacheBehavior,
    awsCloudFrontDistributionDetails_defaultRootObject,
    awsCloudFrontDistributionDetails_domainName,
    awsCloudFrontDistributionDetails_eTag,
    awsCloudFrontDistributionDetails_lastModifiedTime,
    awsCloudFrontDistributionDetails_logging,
    awsCloudFrontDistributionDetails_originGroups,
    awsCloudFrontDistributionDetails_origins,
    awsCloudFrontDistributionDetails_status,
    awsCloudFrontDistributionDetails_viewerCertificate,
    awsCloudFrontDistributionDetails_webAclId,

    -- ** AwsCloudFrontDistributionLogging
    awsCloudFrontDistributionLogging_bucket,
    awsCloudFrontDistributionLogging_enabled,
    awsCloudFrontDistributionLogging_includeCookies,
    awsCloudFrontDistributionLogging_prefix,

    -- ** AwsCloudFrontDistributionOriginCustomOriginConfig
    awsCloudFrontDistributionOriginCustomOriginConfig_httpPort,
    awsCloudFrontDistributionOriginCustomOriginConfig_httpsPort,
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
    awsCloudFrontDistributionOriginItem_customOriginConfig,
    awsCloudFrontDistributionOriginItem_domainName,
    awsCloudFrontDistributionOriginItem_id,
    awsCloudFrontDistributionOriginItem_originPath,
    awsCloudFrontDistributionOriginItem_s3OriginConfig,

    -- ** AwsCloudFrontDistributionOriginS3OriginConfig
    awsCloudFrontDistributionOriginS3OriginConfig_originAccessIdentity,

    -- ** AwsCloudFrontDistributionOriginSslProtocols
    awsCloudFrontDistributionOriginSslProtocols_items,
    awsCloudFrontDistributionOriginSslProtocols_quantity,

    -- ** AwsCloudFrontDistributionOrigins
    awsCloudFrontDistributionOrigins_items,

    -- ** AwsCloudFrontDistributionViewerCertificate
    awsCloudFrontDistributionViewerCertificate_acmCertificateArn,
    awsCloudFrontDistributionViewerCertificate_certificate,
    awsCloudFrontDistributionViewerCertificate_certificateSource,
    awsCloudFrontDistributionViewerCertificate_cloudFrontDefaultCertificate,
    awsCloudFrontDistributionViewerCertificate_iamCertificateId,
    awsCloudFrontDistributionViewerCertificate_minimumProtocolVersion,
    awsCloudFrontDistributionViewerCertificate_sslSupportMethod,

    -- ** AwsCloudTrailTrailDetails
    awsCloudTrailTrailDetails_cloudWatchLogsLogGroupArn,
    awsCloudTrailTrailDetails_cloudWatchLogsRoleArn,
    awsCloudTrailTrailDetails_hasCustomEventSelectors,
    awsCloudTrailTrailDetails_homeRegion,
    awsCloudTrailTrailDetails_includeGlobalServiceEvents,
    awsCloudTrailTrailDetails_isMultiRegionTrail,
    awsCloudTrailTrailDetails_isOrganizationTrail,
    awsCloudTrailTrailDetails_kmsKeyId,
    awsCloudTrailTrailDetails_logFileValidationEnabled,
    awsCloudTrailTrailDetails_name,
    awsCloudTrailTrailDetails_s3BucketName,
    awsCloudTrailTrailDetails_s3KeyPrefix,
    awsCloudTrailTrailDetails_snsTopicArn,
    awsCloudTrailTrailDetails_snsTopicName,
    awsCloudTrailTrailDetails_trailArn,

    -- ** AwsCloudWatchAlarmDetails
    awsCloudWatchAlarmDetails_actionsEnabled,
    awsCloudWatchAlarmDetails_alarmActions,
    awsCloudWatchAlarmDetails_alarmArn,
    awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp,
    awsCloudWatchAlarmDetails_alarmDescription,
    awsCloudWatchAlarmDetails_alarmName,
    awsCloudWatchAlarmDetails_comparisonOperator,
    awsCloudWatchAlarmDetails_datapointsToAlarm,
    awsCloudWatchAlarmDetails_dimensions,
    awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile,
    awsCloudWatchAlarmDetails_evaluationPeriods,
    awsCloudWatchAlarmDetails_extendedStatistic,
    awsCloudWatchAlarmDetails_insufficientDataActions,
    awsCloudWatchAlarmDetails_metricName,
    awsCloudWatchAlarmDetails_namespace,
    awsCloudWatchAlarmDetails_okActions,
    awsCloudWatchAlarmDetails_period,
    awsCloudWatchAlarmDetails_statistic,
    awsCloudWatchAlarmDetails_threshold,
    awsCloudWatchAlarmDetails_thresholdMetricId,
    awsCloudWatchAlarmDetails_treatMissingData,
    awsCloudWatchAlarmDetails_unit,

    -- ** AwsCloudWatchAlarmDimensionsDetails
    awsCloudWatchAlarmDimensionsDetails_name,
    awsCloudWatchAlarmDimensionsDetails_value,

    -- ** AwsCodeBuildProjectArtifactsDetails
    awsCodeBuildProjectArtifactsDetails_artifactIdentifier,
    awsCodeBuildProjectArtifactsDetails_encryptionDisabled,
    awsCodeBuildProjectArtifactsDetails_location,
    awsCodeBuildProjectArtifactsDetails_name,
    awsCodeBuildProjectArtifactsDetails_namespaceType,
    awsCodeBuildProjectArtifactsDetails_overrideArtifactName,
    awsCodeBuildProjectArtifactsDetails_packaging,
    awsCodeBuildProjectArtifactsDetails_path,
    awsCodeBuildProjectArtifactsDetails_type,

    -- ** AwsCodeBuildProjectDetails
    awsCodeBuildProjectDetails_artifacts,
    awsCodeBuildProjectDetails_encryptionKey,
    awsCodeBuildProjectDetails_environment,
    awsCodeBuildProjectDetails_logsConfig,
    awsCodeBuildProjectDetails_name,
    awsCodeBuildProjectDetails_secondaryArtifacts,
    awsCodeBuildProjectDetails_serviceRole,
    awsCodeBuildProjectDetails_source,
    awsCodeBuildProjectDetails_vpcConfig,

    -- ** AwsCodeBuildProjectEnvironment
    awsCodeBuildProjectEnvironment_certificate,
    awsCodeBuildProjectEnvironment_environmentVariables,
    awsCodeBuildProjectEnvironment_imagePullCredentialsType,
    awsCodeBuildProjectEnvironment_privilegedMode,
    awsCodeBuildProjectEnvironment_registryCredential,
    awsCodeBuildProjectEnvironment_type,

    -- ** AwsCodeBuildProjectEnvironmentEnvironmentVariablesDetails
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_name,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_type,
    awsCodeBuildProjectEnvironmentEnvironmentVariablesDetails_value,

    -- ** AwsCodeBuildProjectEnvironmentRegistryCredential
    awsCodeBuildProjectEnvironmentRegistryCredential_credential,
    awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider,

    -- ** AwsCodeBuildProjectLogsConfigCloudWatchLogsDetails
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_groupName,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_status,
    awsCodeBuildProjectLogsConfigCloudWatchLogsDetails_streamName,

    -- ** AwsCodeBuildProjectLogsConfigDetails
    awsCodeBuildProjectLogsConfigDetails_cloudWatchLogs,
    awsCodeBuildProjectLogsConfigDetails_s3Logs,

    -- ** AwsCodeBuildProjectLogsConfigS3LogsDetails
    awsCodeBuildProjectLogsConfigS3LogsDetails_encryptionDisabled,
    awsCodeBuildProjectLogsConfigS3LogsDetails_location,
    awsCodeBuildProjectLogsConfigS3LogsDetails_status,

    -- ** AwsCodeBuildProjectSource
    awsCodeBuildProjectSource_gitCloneDepth,
    awsCodeBuildProjectSource_insecureSsl,
    awsCodeBuildProjectSource_location,
    awsCodeBuildProjectSource_type,

    -- ** AwsCodeBuildProjectVpcConfig
    awsCodeBuildProjectVpcConfig_securityGroupIds,
    awsCodeBuildProjectVpcConfig_subnets,
    awsCodeBuildProjectVpcConfig_vpcId,

    -- ** AwsCorsConfiguration
    awsCorsConfiguration_allowCredentials,
    awsCorsConfiguration_allowHeaders,
    awsCorsConfiguration_allowMethods,
    awsCorsConfiguration_allowOrigins,
    awsCorsConfiguration_exposeHeaders,
    awsCorsConfiguration_maxAge,

    -- ** AwsDynamoDbTableAttributeDefinition
    awsDynamoDbTableAttributeDefinition_attributeName,
    awsDynamoDbTableAttributeDefinition_attributeType,

    -- ** AwsDynamoDbTableBillingModeSummary
    awsDynamoDbTableBillingModeSummary_billingMode,
    awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime,

    -- ** AwsDynamoDbTableDetails
    awsDynamoDbTableDetails_attributeDefinitions,
    awsDynamoDbTableDetails_billingModeSummary,
    awsDynamoDbTableDetails_creationDateTime,
    awsDynamoDbTableDetails_globalSecondaryIndexes,
    awsDynamoDbTableDetails_globalTableVersion,
    awsDynamoDbTableDetails_itemCount,
    awsDynamoDbTableDetails_keySchema,
    awsDynamoDbTableDetails_latestStreamArn,
    awsDynamoDbTableDetails_latestStreamLabel,
    awsDynamoDbTableDetails_localSecondaryIndexes,
    awsDynamoDbTableDetails_provisionedThroughput,
    awsDynamoDbTableDetails_replicas,
    awsDynamoDbTableDetails_restoreSummary,
    awsDynamoDbTableDetails_sseDescription,
    awsDynamoDbTableDetails_streamSpecification,
    awsDynamoDbTableDetails_tableId,
    awsDynamoDbTableDetails_tableName,
    awsDynamoDbTableDetails_tableSizeBytes,
    awsDynamoDbTableDetails_tableStatus,

    -- ** AwsDynamoDbTableGlobalSecondaryIndex
    awsDynamoDbTableGlobalSecondaryIndex_backfilling,
    awsDynamoDbTableGlobalSecondaryIndex_indexArn,
    awsDynamoDbTableGlobalSecondaryIndex_indexName,
    awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes,
    awsDynamoDbTableGlobalSecondaryIndex_indexStatus,
    awsDynamoDbTableGlobalSecondaryIndex_itemCount,
    awsDynamoDbTableGlobalSecondaryIndex_keySchema,
    awsDynamoDbTableGlobalSecondaryIndex_projection,
    awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput,

    -- ** AwsDynamoDbTableKeySchema
    awsDynamoDbTableKeySchema_attributeName,
    awsDynamoDbTableKeySchema_keyType,

    -- ** AwsDynamoDbTableLocalSecondaryIndex
    awsDynamoDbTableLocalSecondaryIndex_indexArn,
    awsDynamoDbTableLocalSecondaryIndex_indexName,
    awsDynamoDbTableLocalSecondaryIndex_keySchema,
    awsDynamoDbTableLocalSecondaryIndex_projection,

    -- ** AwsDynamoDbTableProjection
    awsDynamoDbTableProjection_nonKeyAttributes,
    awsDynamoDbTableProjection_projectionType,

    -- ** AwsDynamoDbTableProvisionedThroughput
    awsDynamoDbTableProvisionedThroughput_lastDecreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_lastIncreaseDateTime,
    awsDynamoDbTableProvisionedThroughput_numberOfDecreasesToday,
    awsDynamoDbTableProvisionedThroughput_readCapacityUnits,
    awsDynamoDbTableProvisionedThroughput_writeCapacityUnits,

    -- ** AwsDynamoDbTableProvisionedThroughputOverride
    awsDynamoDbTableProvisionedThroughputOverride_readCapacityUnits,

    -- ** AwsDynamoDbTableReplica
    awsDynamoDbTableReplica_globalSecondaryIndexes,
    awsDynamoDbTableReplica_kmsMasterKeyId,
    awsDynamoDbTableReplica_provisionedThroughputOverride,
    awsDynamoDbTableReplica_regionName,
    awsDynamoDbTableReplica_replicaStatus,
    awsDynamoDbTableReplica_replicaStatusDescription,

    -- ** AwsDynamoDbTableReplicaGlobalSecondaryIndex
    awsDynamoDbTableReplicaGlobalSecondaryIndex_indexName,
    awsDynamoDbTableReplicaGlobalSecondaryIndex_provisionedThroughputOverride,

    -- ** AwsDynamoDbTableRestoreSummary
    awsDynamoDbTableRestoreSummary_restoreDateTime,
    awsDynamoDbTableRestoreSummary_restoreInProgress,
    awsDynamoDbTableRestoreSummary_sourceBackupArn,
    awsDynamoDbTableRestoreSummary_sourceTableArn,

    -- ** AwsDynamoDbTableSseDescription
    awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime,
    awsDynamoDbTableSseDescription_kmsMasterKeyArn,
    awsDynamoDbTableSseDescription_sseType,
    awsDynamoDbTableSseDescription_status,

    -- ** AwsDynamoDbTableStreamSpecification
    awsDynamoDbTableStreamSpecification_streamEnabled,
    awsDynamoDbTableStreamSpecification_streamViewType,

    -- ** AwsEc2EipDetails
    awsEc2EipDetails_allocationId,
    awsEc2EipDetails_associationId,
    awsEc2EipDetails_domain,
    awsEc2EipDetails_instanceId,
    awsEc2EipDetails_networkBorderGroup,
    awsEc2EipDetails_networkInterfaceId,
    awsEc2EipDetails_networkInterfaceOwnerId,
    awsEc2EipDetails_privateIpAddress,
    awsEc2EipDetails_publicIp,
    awsEc2EipDetails_publicIpv4Pool,

    -- ** AwsEc2InstanceDetails
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_imageId,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_metadataOptions,
    awsEc2InstanceDetails_monitoring,
    awsEc2InstanceDetails_networkInterfaces,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_virtualizationType,
    awsEc2InstanceDetails_vpcId,

    -- ** AwsEc2InstanceMetadataOptions
    awsEc2InstanceMetadataOptions_httpEndpoint,
    awsEc2InstanceMetadataOptions_httpProtocolIpv6,
    awsEc2InstanceMetadataOptions_httpPutResponseHopLimit,
    awsEc2InstanceMetadataOptions_httpTokens,
    awsEc2InstanceMetadataOptions_instanceMetadataTags,

    -- ** AwsEc2InstanceMonitoringDetails
    awsEc2InstanceMonitoringDetails_state,

    -- ** AwsEc2InstanceNetworkInterfacesDetails
    awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId,

    -- ** AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_deviceName,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_ebs,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_noDevice,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetDetails_virtualName,

    -- ** AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_deleteOnTermination,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_encrypted,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_iops,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_kmsKeyId,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_snapshotId,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_throughput,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeSize,
    awsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails_volumeType,

    -- ** AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
    awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationId,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationResourceGroupArn,

    -- ** AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
    awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationPreference,
    awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationTarget,

    -- ** AwsEc2LaunchTemplateDataCpuOptionsDetails
    awsEc2LaunchTemplateDataCpuOptionsDetails_coreCount,
    awsEc2LaunchTemplateDataCpuOptionsDetails_threadsPerCore,

    -- ** AwsEc2LaunchTemplateDataCreditSpecificationDetails
    awsEc2LaunchTemplateDataCreditSpecificationDetails_cpuCredits,

    -- ** AwsEc2LaunchTemplateDataDetails
    awsEc2LaunchTemplateDataDetails_blockDeviceMappingSet,
    awsEc2LaunchTemplateDataDetails_capacityReservationSpecification,
    awsEc2LaunchTemplateDataDetails_cpuOptions,
    awsEc2LaunchTemplateDataDetails_creditSpecification,
    awsEc2LaunchTemplateDataDetails_disableApiStop,
    awsEc2LaunchTemplateDataDetails_disableApiTermination,
    awsEc2LaunchTemplateDataDetails_ebsOptimized,
    awsEc2LaunchTemplateDataDetails_elasticGpuSpecificationSet,
    awsEc2LaunchTemplateDataDetails_elasticInferenceAcceleratorSet,
    awsEc2LaunchTemplateDataDetails_enclaveOptions,
    awsEc2LaunchTemplateDataDetails_hibernationOptions,
    awsEc2LaunchTemplateDataDetails_iamInstanceProfile,
    awsEc2LaunchTemplateDataDetails_imageId,
    awsEc2LaunchTemplateDataDetails_instanceInitiatedShutdownBehavior,
    awsEc2LaunchTemplateDataDetails_instanceMarketOptions,
    awsEc2LaunchTemplateDataDetails_instanceRequirements,
    awsEc2LaunchTemplateDataDetails_instanceType,
    awsEc2LaunchTemplateDataDetails_kernelId,
    awsEc2LaunchTemplateDataDetails_keyName,
    awsEc2LaunchTemplateDataDetails_licenseSet,
    awsEc2LaunchTemplateDataDetails_maintenanceOptions,
    awsEc2LaunchTemplateDataDetails_metadataOptions,
    awsEc2LaunchTemplateDataDetails_monitoring,
    awsEc2LaunchTemplateDataDetails_networkInterfaceSet,
    awsEc2LaunchTemplateDataDetails_placement,
    awsEc2LaunchTemplateDataDetails_privateDnsNameOptions,
    awsEc2LaunchTemplateDataDetails_ramDiskId,
    awsEc2LaunchTemplateDataDetails_securityGroupIdSet,
    awsEc2LaunchTemplateDataDetails_securityGroupSet,
    awsEc2LaunchTemplateDataDetails_userData,

    -- ** AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
    awsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails_type,

    -- ** AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
    awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_count,
    awsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails_type,

    -- ** AwsEc2LaunchTemplateDataEnclaveOptionsDetails
    awsEc2LaunchTemplateDataEnclaveOptionsDetails_enabled,

    -- ** AwsEc2LaunchTemplateDataHibernationOptionsDetails
    awsEc2LaunchTemplateDataHibernationOptionsDetails_configured,

    -- ** AwsEc2LaunchTemplateDataIamInstanceProfileDetails
    awsEc2LaunchTemplateDataIamInstanceProfileDetails_arn,
    awsEc2LaunchTemplateDataIamInstanceProfileDetails_name,

    -- ** AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
    awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_marketType,
    awsEc2LaunchTemplateDataInstanceMarketOptionsDetails_spotOptions,

    -- ** AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_blockDurationMinutes,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_instanceInterruptionBehavior,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_maxPrice,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_spotInstanceType,
    awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_validUntil,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
    awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsDetails
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorCount,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorManufacturers,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorNames,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTotalMemoryMiB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_acceleratorTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_bareMetal,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_baselineEbsBandwidthMbps,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_burstablePerformance,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_cpuManufacturers,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_excludedInstanceTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_instanceGenerations,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorage,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_localStorageTypes,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryGiBPerVCpu,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_memoryMiB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_networkInterfaceCount,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_onDemandMaxPricePercentageOverLowestPrice,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_requireHibernateSupport,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_spotMaxPricePercentageOverLowestPrice,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_totalLocalStorageGB,
    awsEc2LaunchTemplateDataInstanceRequirementsDetails_vCpuCount,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
    awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
    awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_min,

    -- ** AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
    awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_max,
    awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_min,

    -- ** AwsEc2LaunchTemplateDataLicenseSetDetails
    awsEc2LaunchTemplateDataLicenseSetDetails_licenseConfigurationArn,

    -- ** AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
    awsEc2LaunchTemplateDataMaintenanceOptionsDetails_autoRecovery,

    -- ** AwsEc2LaunchTemplateDataMetadataOptionsDetails
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpEndpoint,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpProtocolIpv6,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpPutResponseHopLimit,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_httpTokens,
    awsEc2LaunchTemplateDataMetadataOptionsDetails_instanceMetadataTags,

    -- ** AwsEc2LaunchTemplateDataMonitoringDetails
    awsEc2LaunchTemplateDataMonitoringDetails_enabled,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associateCarrierIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associatePublicIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deleteOnTermination,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_description,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deviceIndex,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_groups,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_interfaceType,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4PrefixCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4Prefixes,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6AddressCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Addresses,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6PrefixCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Prefixes,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkCardIndex,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkInterfaceId,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddress,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddresses,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_secondaryPrivateIpAddressCount,
    awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_subnetId,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails_ipv4Prefix,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails_ipv6Address,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
    awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails_ipv6Prefix,

    -- ** AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
    awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_primary,
    awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_privateIpAddress,

    -- ** AwsEc2LaunchTemplateDataPlacementDetails
    awsEc2LaunchTemplateDataPlacementDetails_affinity,
    awsEc2LaunchTemplateDataPlacementDetails_availabilityZone,
    awsEc2LaunchTemplateDataPlacementDetails_groupName,
    awsEc2LaunchTemplateDataPlacementDetails_hostId,
    awsEc2LaunchTemplateDataPlacementDetails_hostResourceGroupArn,
    awsEc2LaunchTemplateDataPlacementDetails_partitionNumber,
    awsEc2LaunchTemplateDataPlacementDetails_spreadDomain,
    awsEc2LaunchTemplateDataPlacementDetails_tenancy,

    -- ** AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsAAAARecord,
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsARecord,
    awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_hostnameType,

    -- ** AwsEc2LaunchTemplateDetails
    awsEc2LaunchTemplateDetails_defaultVersionNumber,
    awsEc2LaunchTemplateDetails_id,
    awsEc2LaunchTemplateDetails_latestVersionNumber,
    awsEc2LaunchTemplateDetails_launchTemplateData,
    awsEc2LaunchTemplateDetails_launchTemplateName,

    -- ** AwsEc2NetworkAclAssociation
    awsEc2NetworkAclAssociation_networkAclAssociationId,
    awsEc2NetworkAclAssociation_networkAclId,
    awsEc2NetworkAclAssociation_subnetId,

    -- ** AwsEc2NetworkAclDetails
    awsEc2NetworkAclDetails_associations,
    awsEc2NetworkAclDetails_entries,
    awsEc2NetworkAclDetails_isDefault,
    awsEc2NetworkAclDetails_networkAclId,
    awsEc2NetworkAclDetails_ownerId,
    awsEc2NetworkAclDetails_vpcId,

    -- ** AwsEc2NetworkAclEntry
    awsEc2NetworkAclEntry_cidrBlock,
    awsEc2NetworkAclEntry_egress,
    awsEc2NetworkAclEntry_icmpTypeCode,
    awsEc2NetworkAclEntry_ipv6CidrBlock,
    awsEc2NetworkAclEntry_portRange,
    awsEc2NetworkAclEntry_protocol,
    awsEc2NetworkAclEntry_ruleAction,
    awsEc2NetworkAclEntry_ruleNumber,

    -- ** AwsEc2NetworkInterfaceAttachment
    awsEc2NetworkInterfaceAttachment_attachTime,
    awsEc2NetworkInterfaceAttachment_attachmentId,
    awsEc2NetworkInterfaceAttachment_deleteOnTermination,
    awsEc2NetworkInterfaceAttachment_deviceIndex,
    awsEc2NetworkInterfaceAttachment_instanceId,
    awsEc2NetworkInterfaceAttachment_instanceOwnerId,
    awsEc2NetworkInterfaceAttachment_status,

    -- ** AwsEc2NetworkInterfaceDetails
    awsEc2NetworkInterfaceDetails_attachment,
    awsEc2NetworkInterfaceDetails_ipV6Addresses,
    awsEc2NetworkInterfaceDetails_networkInterfaceId,
    awsEc2NetworkInterfaceDetails_privateIpAddresses,
    awsEc2NetworkInterfaceDetails_publicDnsName,
    awsEc2NetworkInterfaceDetails_publicIp,
    awsEc2NetworkInterfaceDetails_securityGroups,
    awsEc2NetworkInterfaceDetails_sourceDestCheck,

    -- ** AwsEc2NetworkInterfaceIpV6AddressDetail
    awsEc2NetworkInterfaceIpV6AddressDetail_ipV6Address,

    -- ** AwsEc2NetworkInterfacePrivateIpAddressDetail
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateDnsName,
    awsEc2NetworkInterfacePrivateIpAddressDetail_privateIpAddress,

    -- ** AwsEc2NetworkInterfaceSecurityGroup
    awsEc2NetworkInterfaceSecurityGroup_groupId,
    awsEc2NetworkInterfaceSecurityGroup_groupName,

    -- ** AwsEc2RouteTableDetails
    awsEc2RouteTableDetails_associationSet,
    awsEc2RouteTableDetails_ownerId,
    awsEc2RouteTableDetails_propagatingVgwSet,
    awsEc2RouteTableDetails_routeSet,
    awsEc2RouteTableDetails_routeTableId,
    awsEc2RouteTableDetails_vpcId,

    -- ** AwsEc2SecurityGroupDetails
    awsEc2SecurityGroupDetails_groupId,
    awsEc2SecurityGroupDetails_groupName,
    awsEc2SecurityGroupDetails_ipPermissions,
    awsEc2SecurityGroupDetails_ipPermissionsEgress,
    awsEc2SecurityGroupDetails_ownerId,
    awsEc2SecurityGroupDetails_vpcId,

    -- ** AwsEc2SecurityGroupIpPermission
    awsEc2SecurityGroupIpPermission_fromPort,
    awsEc2SecurityGroupIpPermission_ipProtocol,
    awsEc2SecurityGroupIpPermission_ipRanges,
    awsEc2SecurityGroupIpPermission_ipv6Ranges,
    awsEc2SecurityGroupIpPermission_prefixListIds,
    awsEc2SecurityGroupIpPermission_toPort,
    awsEc2SecurityGroupIpPermission_userIdGroupPairs,

    -- ** AwsEc2SecurityGroupIpRange
    awsEc2SecurityGroupIpRange_cidrIp,

    -- ** AwsEc2SecurityGroupIpv6Range
    awsEc2SecurityGroupIpv6Range_cidrIpv6,

    -- ** AwsEc2SecurityGroupPrefixListId
    awsEc2SecurityGroupPrefixListId_prefixListId,

    -- ** AwsEc2SecurityGroupUserIdGroupPair
    awsEc2SecurityGroupUserIdGroupPair_groupId,
    awsEc2SecurityGroupUserIdGroupPair_groupName,
    awsEc2SecurityGroupUserIdGroupPair_peeringStatus,
    awsEc2SecurityGroupUserIdGroupPair_userId,
    awsEc2SecurityGroupUserIdGroupPair_vpcId,
    awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId,

    -- ** AwsEc2SubnetDetails
    awsEc2SubnetDetails_assignIpv6AddressOnCreation,
    awsEc2SubnetDetails_availabilityZone,
    awsEc2SubnetDetails_availabilityZoneId,
    awsEc2SubnetDetails_availableIpAddressCount,
    awsEc2SubnetDetails_cidrBlock,
    awsEc2SubnetDetails_defaultForAz,
    awsEc2SubnetDetails_ipv6CidrBlockAssociationSet,
    awsEc2SubnetDetails_mapPublicIpOnLaunch,
    awsEc2SubnetDetails_ownerId,
    awsEc2SubnetDetails_state,
    awsEc2SubnetDetails_subnetArn,
    awsEc2SubnetDetails_subnetId,
    awsEc2SubnetDetails_vpcId,

    -- ** AwsEc2TransitGatewayDetails
    awsEc2TransitGatewayDetails_amazonSideAsn,
    awsEc2TransitGatewayDetails_associationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_autoAcceptSharedAttachments,
    awsEc2TransitGatewayDetails_defaultRouteTableAssociation,
    awsEc2TransitGatewayDetails_defaultRouteTablePropagation,
    awsEc2TransitGatewayDetails_description,
    awsEc2TransitGatewayDetails_dnsSupport,
    awsEc2TransitGatewayDetails_id,
    awsEc2TransitGatewayDetails_multicastSupport,
    awsEc2TransitGatewayDetails_propagationDefaultRouteTableId,
    awsEc2TransitGatewayDetails_transitGatewayCidrBlocks,
    awsEc2TransitGatewayDetails_vpnEcmpSupport,

    -- ** AwsEc2VolumeAttachment
    awsEc2VolumeAttachment_attachTime,
    awsEc2VolumeAttachment_deleteOnTermination,
    awsEc2VolumeAttachment_instanceId,
    awsEc2VolumeAttachment_status,

    -- ** AwsEc2VolumeDetails
    awsEc2VolumeDetails_attachments,
    awsEc2VolumeDetails_createTime,
    awsEc2VolumeDetails_deviceName,
    awsEc2VolumeDetails_encrypted,
    awsEc2VolumeDetails_kmsKeyId,
    awsEc2VolumeDetails_size,
    awsEc2VolumeDetails_snapshotId,
    awsEc2VolumeDetails_status,
    awsEc2VolumeDetails_volumeId,
    awsEc2VolumeDetails_volumeScanStatus,
    awsEc2VolumeDetails_volumeType,

    -- ** AwsEc2VpcDetails
    awsEc2VpcDetails_cidrBlockAssociationSet,
    awsEc2VpcDetails_dhcpOptionsId,
    awsEc2VpcDetails_ipv6CidrBlockAssociationSet,
    awsEc2VpcDetails_state,

    -- ** AwsEc2VpcEndpointServiceDetails
    awsEc2VpcEndpointServiceDetails_acceptanceRequired,
    awsEc2VpcEndpointServiceDetails_availabilityZones,
    awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames,
    awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_managesVpcEndpoints,
    awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns,
    awsEc2VpcEndpointServiceDetails_privateDnsName,
    awsEc2VpcEndpointServiceDetails_serviceId,
    awsEc2VpcEndpointServiceDetails_serviceName,
    awsEc2VpcEndpointServiceDetails_serviceState,
    awsEc2VpcEndpointServiceDetails_serviceType,

    -- ** AwsEc2VpcEndpointServiceServiceTypeDetails
    awsEc2VpcEndpointServiceServiceTypeDetails_serviceType,

    -- ** AwsEc2VpcPeeringConnectionDetails
    awsEc2VpcPeeringConnectionDetails_accepterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_expirationTime,
    awsEc2VpcPeeringConnectionDetails_requesterVpcInfo,
    awsEc2VpcPeeringConnectionDetails_status,
    awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId,

    -- ** AwsEc2VpcPeeringConnectionStatusDetails
    awsEc2VpcPeeringConnectionStatusDetails_code,
    awsEc2VpcPeeringConnectionStatusDetails_message,

    -- ** AwsEc2VpcPeeringConnectionVpcInfoDetails
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock,
    awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet,
    awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId,
    awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions,
    awsEc2VpcPeeringConnectionVpcInfoDetails_region,
    awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId,

    -- ** AwsEc2VpnConnectionDetails
    awsEc2VpnConnectionDetails_category,
    awsEc2VpnConnectionDetails_customerGatewayConfiguration,
    awsEc2VpnConnectionDetails_customerGatewayId,
    awsEc2VpnConnectionDetails_options,
    awsEc2VpnConnectionDetails_routes,
    awsEc2VpnConnectionDetails_state,
    awsEc2VpnConnectionDetails_transitGatewayId,
    awsEc2VpnConnectionDetails_type,
    awsEc2VpnConnectionDetails_vgwTelemetry,
    awsEc2VpnConnectionDetails_vpnConnectionId,
    awsEc2VpnConnectionDetails_vpnGatewayId,

    -- ** AwsEc2VpnConnectionOptionsDetails
    awsEc2VpnConnectionOptionsDetails_staticRoutesOnly,
    awsEc2VpnConnectionOptionsDetails_tunnelOptions,

    -- ** AwsEc2VpnConnectionOptionsTunnelOptionsDetails
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize,
    awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr,

    -- ** AwsEc2VpnConnectionRoutesDetails
    awsEc2VpnConnectionRoutesDetails_destinationCidrBlock,
    awsEc2VpnConnectionRoutesDetails_state,

    -- ** AwsEc2VpnConnectionVgwTelemetryDetails
    awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount,
    awsEc2VpnConnectionVgwTelemetryDetails_certificateArn,
    awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange,
    awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress,
    awsEc2VpnConnectionVgwTelemetryDetails_status,
    awsEc2VpnConnectionVgwTelemetryDetails_statusMessage,

    -- ** AwsEcrContainerImageDetails
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_imageDigest,
    awsEcrContainerImageDetails_imagePublishedAt,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_registryId,
    awsEcrContainerImageDetails_repositoryName,

    -- ** AwsEcrRepositoryDetails
    awsEcrRepositoryDetails_arn,
    awsEcrRepositoryDetails_imageScanningConfiguration,
    awsEcrRepositoryDetails_imageTagMutability,
    awsEcrRepositoryDetails_lifecyclePolicy,
    awsEcrRepositoryDetails_repositoryName,
    awsEcrRepositoryDetails_repositoryPolicyText,

    -- ** AwsEcrRepositoryImageScanningConfigurationDetails
    awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush,

    -- ** AwsEcrRepositoryLifecyclePolicyDetails
    awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText,
    awsEcrRepositoryLifecyclePolicyDetails_registryId,

    -- ** AwsEcsClusterClusterSettingsDetails
    awsEcsClusterClusterSettingsDetails_name,
    awsEcsClusterClusterSettingsDetails_value,

    -- ** AwsEcsClusterConfigurationDetails
    awsEcsClusterConfigurationDetails_executeCommandConfiguration,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration,
    awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging,

    -- ** AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled,
    awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix,

    -- ** AwsEcsClusterDefaultCapacityProviderStrategyDetails
    awsEcsClusterDefaultCapacityProviderStrategyDetails_base,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider,
    awsEcsClusterDefaultCapacityProviderStrategyDetails_weight,

    -- ** AwsEcsClusterDetails
    awsEcsClusterDetails_activeServicesCount,
    awsEcsClusterDetails_capacityProviders,
    awsEcsClusterDetails_clusterArn,
    awsEcsClusterDetails_clusterName,
    awsEcsClusterDetails_clusterSettings,
    awsEcsClusterDetails_configuration,
    awsEcsClusterDetails_defaultCapacityProviderStrategy,
    awsEcsClusterDetails_registeredContainerInstancesCount,
    awsEcsClusterDetails_runningTasksCount,
    awsEcsClusterDetails_status,

    -- ** AwsEcsContainerDetails
    awsEcsContainerDetails_image,
    awsEcsContainerDetails_mountPoints,
    awsEcsContainerDetails_name,
    awsEcsContainerDetails_privileged,

    -- ** AwsEcsServiceCapacityProviderStrategyDetails
    awsEcsServiceCapacityProviderStrategyDetails_base,
    awsEcsServiceCapacityProviderStrategyDetails_capacityProvider,
    awsEcsServiceCapacityProviderStrategyDetails_weight,

    -- ** AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable,
    awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback,

    -- ** AwsEcsServiceDeploymentConfigurationDetails
    awsEcsServiceDeploymentConfigurationDetails_deploymentCircuitBreaker,
    awsEcsServiceDeploymentConfigurationDetails_maximumPercent,
    awsEcsServiceDeploymentConfigurationDetails_minimumHealthyPercent,

    -- ** AwsEcsServiceDeploymentControllerDetails
    awsEcsServiceDeploymentControllerDetails_type,

    -- ** AwsEcsServiceDetails
    awsEcsServiceDetails_capacityProviderStrategy,
    awsEcsServiceDetails_cluster,
    awsEcsServiceDetails_deploymentConfiguration,
    awsEcsServiceDetails_deploymentController,
    awsEcsServiceDetails_desiredCount,
    awsEcsServiceDetails_enableEcsManagedTags,
    awsEcsServiceDetails_enableExecuteCommand,
    awsEcsServiceDetails_healthCheckGracePeriodSeconds,
    awsEcsServiceDetails_launchType,
    awsEcsServiceDetails_loadBalancers,
    awsEcsServiceDetails_name,
    awsEcsServiceDetails_networkConfiguration,
    awsEcsServiceDetails_placementConstraints,
    awsEcsServiceDetails_placementStrategies,
    awsEcsServiceDetails_platformVersion,
    awsEcsServiceDetails_propagateTags,
    awsEcsServiceDetails_role,
    awsEcsServiceDetails_schedulingStrategy,
    awsEcsServiceDetails_serviceArn,
    awsEcsServiceDetails_serviceName,
    awsEcsServiceDetails_serviceRegistries,
    awsEcsServiceDetails_taskDefinition,

    -- ** AwsEcsServiceLoadBalancersDetails
    awsEcsServiceLoadBalancersDetails_containerName,
    awsEcsServiceLoadBalancersDetails_containerPort,
    awsEcsServiceLoadBalancersDetails_loadBalancerName,
    awsEcsServiceLoadBalancersDetails_targetGroupArn,

    -- ** AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups,
    awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets,

    -- ** AwsEcsServiceNetworkConfigurationDetails
    awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration,

    -- ** AwsEcsServicePlacementConstraintsDetails
    awsEcsServicePlacementConstraintsDetails_expression,
    awsEcsServicePlacementConstraintsDetails_type,

    -- ** AwsEcsServicePlacementStrategiesDetails
    awsEcsServicePlacementStrategiesDetails_field,
    awsEcsServicePlacementStrategiesDetails_type,

    -- ** AwsEcsServiceServiceRegistriesDetails
    awsEcsServiceServiceRegistriesDetails_containerName,
    awsEcsServiceServiceRegistriesDetails_containerPort,
    awsEcsServiceServiceRegistriesDetails_port,
    awsEcsServiceServiceRegistriesDetails_registryArn,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition,
    awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsDetails
    awsEcsTaskDefinitionContainerDefinitionsDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsDetails_cpu,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn,
    awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels,
    awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions,
    awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environment,
    awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles,
    awsEcsTaskDefinitionContainerDefinitionsDetails_essential,
    awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts,
    awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck,
    awsEcsTaskDefinitionContainerDefinitionsDetails_hostname,
    awsEcsTaskDefinitionContainerDefinitionsDetails_image,
    awsEcsTaskDefinitionContainerDefinitionsDetails_interactive,
    awsEcsTaskDefinitionContainerDefinitionsDetails_links,
    awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters,
    awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memory,
    awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation,
    awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints,
    awsEcsTaskDefinitionContainerDefinitionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings,
    awsEcsTaskDefinitionContainerDefinitionsDetails_privileged,
    awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal,
    awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem,
    awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials,
    awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements,
    awsEcsTaskDefinitionContainerDefinitionsDetails_secrets,
    awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout,
    awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls,
    awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits,
    awsEcsTaskDefinitionContainerDefinitionsDetails_user,
    awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom,
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
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_options,
    awsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails_type,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_command,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_interval,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_retries,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_startPeriod,
    awsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails_timeout,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions,
    awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_logDriver,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_options,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails_secretOptions,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume,

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
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name,
    awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit,

    -- ** AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly,
    awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer,

    -- ** AwsEcsTaskDefinitionDetails
    awsEcsTaskDefinitionDetails_containerDefinitions,
    awsEcsTaskDefinitionDetails_cpu,
    awsEcsTaskDefinitionDetails_executionRoleArn,
    awsEcsTaskDefinitionDetails_family,
    awsEcsTaskDefinitionDetails_inferenceAccelerators,
    awsEcsTaskDefinitionDetails_ipcMode,
    awsEcsTaskDefinitionDetails_memory,
    awsEcsTaskDefinitionDetails_networkMode,
    awsEcsTaskDefinitionDetails_pidMode,
    awsEcsTaskDefinitionDetails_placementConstraints,
    awsEcsTaskDefinitionDetails_proxyConfiguration,
    awsEcsTaskDefinitionDetails_requiresCompatibilities,
    awsEcsTaskDefinitionDetails_taskRoleArn,
    awsEcsTaskDefinitionDetails_volumes,

    -- ** AwsEcsTaskDefinitionInferenceAcceleratorsDetails
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName,
    awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType,

    -- ** AwsEcsTaskDefinitionPlacementConstraintsDetails
    awsEcsTaskDefinitionPlacementConstraintsDetails_expression,
    awsEcsTaskDefinitionPlacementConstraintsDetails_type,

    -- ** AwsEcsTaskDefinitionProxyConfigurationDetails
    awsEcsTaskDefinitionProxyConfigurationDetails_containerName,
    awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties,
    awsEcsTaskDefinitionProxyConfigurationDetails_type,

    -- ** AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_name,
    awsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails_value,

    -- ** AwsEcsTaskDefinitionVolumesDetails
    awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration,
    awsEcsTaskDefinitionVolumesDetails_host,
    awsEcsTaskDefinitionVolumesDetails_name,

    -- ** AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels,
    awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam,

    -- ** AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_authorizationConfig,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_filesystemId,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_rootDirectory,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryption,
    awsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails_transitEncryptionPort,

    -- ** AwsEcsTaskDefinitionVolumesHostDetails
    awsEcsTaskDefinitionVolumesHostDetails_sourcePath,

    -- ** AwsEcsTaskDetails
    awsEcsTaskDetails_clusterArn,
    awsEcsTaskDetails_containers,
    awsEcsTaskDetails_createdAt,
    awsEcsTaskDetails_group,
    awsEcsTaskDetails_startedAt,
    awsEcsTaskDetails_startedBy,
    awsEcsTaskDetails_taskDefinitionArn,
    awsEcsTaskDetails_version,
    awsEcsTaskDetails_volumes,

    -- ** AwsEcsTaskVolumeDetails
    awsEcsTaskVolumeDetails_host,
    awsEcsTaskVolumeDetails_name,

    -- ** AwsEcsTaskVolumeHostDetails
    awsEcsTaskVolumeHostDetails_sourcePath,

    -- ** AwsEfsAccessPointDetails
    awsEfsAccessPointDetails_accessPointId,
    awsEfsAccessPointDetails_arn,
    awsEfsAccessPointDetails_clientToken,
    awsEfsAccessPointDetails_fileSystemId,
    awsEfsAccessPointDetails_posixUser,
    awsEfsAccessPointDetails_rootDirectory,

    -- ** AwsEfsAccessPointPosixUserDetails
    awsEfsAccessPointPosixUserDetails_gid,
    awsEfsAccessPointPosixUserDetails_secondaryGids,
    awsEfsAccessPointPosixUserDetails_uid,

    -- ** AwsEfsAccessPointRootDirectoryCreationInfoDetails
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid,
    awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions,

    -- ** AwsEfsAccessPointRootDirectoryDetails
    awsEfsAccessPointRootDirectoryDetails_creationInfo,
    awsEfsAccessPointRootDirectoryDetails_path,

    -- ** AwsEksClusterDetails
    awsEksClusterDetails_arn,
    awsEksClusterDetails_certificateAuthorityData,
    awsEksClusterDetails_clusterStatus,
    awsEksClusterDetails_endpoint,
    awsEksClusterDetails_logging,
    awsEksClusterDetails_name,
    awsEksClusterDetails_resourcesVpcConfig,
    awsEksClusterDetails_roleArn,
    awsEksClusterDetails_version,

    -- ** AwsEksClusterLoggingClusterLoggingDetails
    awsEksClusterLoggingClusterLoggingDetails_enabled,
    awsEksClusterLoggingClusterLoggingDetails_types,

    -- ** AwsEksClusterLoggingDetails
    awsEksClusterLoggingDetails_clusterLogging,

    -- ** AwsEksClusterResourcesVpcConfigDetails
    awsEksClusterResourcesVpcConfigDetails_endpointPublicAccess,
    awsEksClusterResourcesVpcConfigDetails_securityGroupIds,
    awsEksClusterResourcesVpcConfigDetails_subnetIds,

    -- ** AwsElasticBeanstalkEnvironmentDetails
    awsElasticBeanstalkEnvironmentDetails_applicationName,
    awsElasticBeanstalkEnvironmentDetails_cname,
    awsElasticBeanstalkEnvironmentDetails_dateCreated,
    awsElasticBeanstalkEnvironmentDetails_dateUpdated,
    awsElasticBeanstalkEnvironmentDetails_description,
    awsElasticBeanstalkEnvironmentDetails_endpointUrl,
    awsElasticBeanstalkEnvironmentDetails_environmentArn,
    awsElasticBeanstalkEnvironmentDetails_environmentId,
    awsElasticBeanstalkEnvironmentDetails_environmentLinks,
    awsElasticBeanstalkEnvironmentDetails_environmentName,
    awsElasticBeanstalkEnvironmentDetails_optionSettings,
    awsElasticBeanstalkEnvironmentDetails_platformArn,
    awsElasticBeanstalkEnvironmentDetails_solutionStackName,
    awsElasticBeanstalkEnvironmentDetails_status,
    awsElasticBeanstalkEnvironmentDetails_tier,
    awsElasticBeanstalkEnvironmentDetails_versionLabel,

    -- ** AwsElasticBeanstalkEnvironmentEnvironmentLink
    awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName,
    awsElasticBeanstalkEnvironmentEnvironmentLink_linkName,

    -- ** AwsElasticBeanstalkEnvironmentOptionSetting
    awsElasticBeanstalkEnvironmentOptionSetting_namespace,
    awsElasticBeanstalkEnvironmentOptionSetting_optionName,
    awsElasticBeanstalkEnvironmentOptionSetting_resourceName,
    awsElasticBeanstalkEnvironmentOptionSetting_value,

    -- ** AwsElasticBeanstalkEnvironmentTier
    awsElasticBeanstalkEnvironmentTier_name,
    awsElasticBeanstalkEnvironmentTier_type,
    awsElasticBeanstalkEnvironmentTier_version,

    -- ** AwsElasticsearchDomainDetails
    awsElasticsearchDomainDetails_accessPolicies,
    awsElasticsearchDomainDetails_domainEndpointOptions,
    awsElasticsearchDomainDetails_domainId,
    awsElasticsearchDomainDetails_domainName,
    awsElasticsearchDomainDetails_elasticsearchClusterConfig,
    awsElasticsearchDomainDetails_elasticsearchVersion,
    awsElasticsearchDomainDetails_encryptionAtRestOptions,
    awsElasticsearchDomainDetails_endpoint,
    awsElasticsearchDomainDetails_endpoints,
    awsElasticsearchDomainDetails_logPublishingOptions,
    awsElasticsearchDomainDetails_nodeToNodeEncryptionOptions,
    awsElasticsearchDomainDetails_serviceSoftwareOptions,
    awsElasticsearchDomainDetails_vPCOptions,

    -- ** AwsElasticsearchDomainDomainEndpointOptions
    awsElasticsearchDomainDomainEndpointOptions_enforceHTTPS,
    awsElasticsearchDomainDomainEndpointOptions_tLSSecurityPolicy,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigDetails
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterEnabled,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_dedicatedMasterType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceCount,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_instanceType,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessConfig,
    awsElasticsearchDomainElasticsearchClusterConfigDetails_zoneAwarenessEnabled,

    -- ** AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
    awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- ** AwsElasticsearchDomainEncryptionAtRestOptions
    awsElasticsearchDomainEncryptionAtRestOptions_enabled,
    awsElasticsearchDomainEncryptionAtRestOptions_kmsKeyId,

    -- ** AwsElasticsearchDomainLogPublishingOptions
    awsElasticsearchDomainLogPublishingOptions_auditLogs,
    awsElasticsearchDomainLogPublishingOptions_indexSlowLogs,
    awsElasticsearchDomainLogPublishingOptions_searchSlowLogs,

    -- ** AwsElasticsearchDomainLogPublishingOptionsLogConfig
    awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn,
    awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled,

    -- ** AwsElasticsearchDomainNodeToNodeEncryptionOptions
    awsElasticsearchDomainNodeToNodeEncryptionOptions_enabled,

    -- ** AwsElasticsearchDomainServiceSoftwareOptions
    awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate,
    awsElasticsearchDomainServiceSoftwareOptions_cancellable,
    awsElasticsearchDomainServiceSoftwareOptions_currentVersion,
    awsElasticsearchDomainServiceSoftwareOptions_description,
    awsElasticsearchDomainServiceSoftwareOptions_newVersion,
    awsElasticsearchDomainServiceSoftwareOptions_updateAvailable,
    awsElasticsearchDomainServiceSoftwareOptions_updateStatus,

    -- ** AwsElasticsearchDomainVPCOptions
    awsElasticsearchDomainVPCOptions_availabilityZones,
    awsElasticsearchDomainVPCOptions_securityGroupIds,
    awsElasticsearchDomainVPCOptions_subnetIds,
    awsElasticsearchDomainVPCOptions_vPCId,

    -- ** AwsElbAppCookieStickinessPolicy
    awsElbAppCookieStickinessPolicy_cookieName,
    awsElbAppCookieStickinessPolicy_policyName,

    -- ** AwsElbLbCookieStickinessPolicy
    awsElbLbCookieStickinessPolicy_cookieExpirationPeriod,
    awsElbLbCookieStickinessPolicy_policyName,

    -- ** AwsElbLoadBalancerAccessLog
    awsElbLoadBalancerAccessLog_emitInterval,
    awsElbLoadBalancerAccessLog_enabled,
    awsElbLoadBalancerAccessLog_s3BucketName,
    awsElbLoadBalancerAccessLog_s3BucketPrefix,

    -- ** AwsElbLoadBalancerAdditionalAttribute
    awsElbLoadBalancerAdditionalAttribute_key,
    awsElbLoadBalancerAdditionalAttribute_value,

    -- ** AwsElbLoadBalancerAttributes
    awsElbLoadBalancerAttributes_accessLog,
    awsElbLoadBalancerAttributes_additionalAttributes,
    awsElbLoadBalancerAttributes_connectionDraining,
    awsElbLoadBalancerAttributes_connectionSettings,
    awsElbLoadBalancerAttributes_crossZoneLoadBalancing,

    -- ** AwsElbLoadBalancerBackendServerDescription
    awsElbLoadBalancerBackendServerDescription_instancePort,
    awsElbLoadBalancerBackendServerDescription_policyNames,

    -- ** AwsElbLoadBalancerConnectionDraining
    awsElbLoadBalancerConnectionDraining_enabled,
    awsElbLoadBalancerConnectionDraining_timeout,

    -- ** AwsElbLoadBalancerConnectionSettings
    awsElbLoadBalancerConnectionSettings_idleTimeout,

    -- ** AwsElbLoadBalancerCrossZoneLoadBalancing
    awsElbLoadBalancerCrossZoneLoadBalancing_enabled,

    -- ** AwsElbLoadBalancerDetails
    awsElbLoadBalancerDetails_availabilityZones,
    awsElbLoadBalancerDetails_backendServerDescriptions,
    awsElbLoadBalancerDetails_canonicalHostedZoneName,
    awsElbLoadBalancerDetails_canonicalHostedZoneNameID,
    awsElbLoadBalancerDetails_createdTime,
    awsElbLoadBalancerDetails_dnsName,
    awsElbLoadBalancerDetails_healthCheck,
    awsElbLoadBalancerDetails_instances,
    awsElbLoadBalancerDetails_listenerDescriptions,
    awsElbLoadBalancerDetails_loadBalancerAttributes,
    awsElbLoadBalancerDetails_loadBalancerName,
    awsElbLoadBalancerDetails_policies,
    awsElbLoadBalancerDetails_scheme,
    awsElbLoadBalancerDetails_securityGroups,
    awsElbLoadBalancerDetails_sourceSecurityGroup,
    awsElbLoadBalancerDetails_subnets,
    awsElbLoadBalancerDetails_vpcId,

    -- ** AwsElbLoadBalancerHealthCheck
    awsElbLoadBalancerHealthCheck_healthyThreshold,
    awsElbLoadBalancerHealthCheck_interval,
    awsElbLoadBalancerHealthCheck_target,
    awsElbLoadBalancerHealthCheck_timeout,
    awsElbLoadBalancerHealthCheck_unhealthyThreshold,

    -- ** AwsElbLoadBalancerInstance
    awsElbLoadBalancerInstance_instanceId,

    -- ** AwsElbLoadBalancerListener
    awsElbLoadBalancerListener_instancePort,
    awsElbLoadBalancerListener_instanceProtocol,
    awsElbLoadBalancerListener_loadBalancerPort,
    awsElbLoadBalancerListener_protocol,
    awsElbLoadBalancerListener_sslCertificateId,

    -- ** AwsElbLoadBalancerListenerDescription
    awsElbLoadBalancerListenerDescription_listener,
    awsElbLoadBalancerListenerDescription_policyNames,

    -- ** AwsElbLoadBalancerPolicies
    awsElbLoadBalancerPolicies_appCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_lbCookieStickinessPolicies,
    awsElbLoadBalancerPolicies_otherPolicies,

    -- ** AwsElbLoadBalancerSourceSecurityGroup
    awsElbLoadBalancerSourceSecurityGroup_groupName,
    awsElbLoadBalancerSourceSecurityGroup_ownerAlias,

    -- ** AwsElbv2LoadBalancerAttribute
    awsElbv2LoadBalancerAttribute_key,
    awsElbv2LoadBalancerAttribute_value,

    -- ** AwsElbv2LoadBalancerDetails
    awsElbv2LoadBalancerDetails_availabilityZones,
    awsElbv2LoadBalancerDetails_canonicalHostedZoneId,
    awsElbv2LoadBalancerDetails_createdTime,
    awsElbv2LoadBalancerDetails_dNSName,
    awsElbv2LoadBalancerDetails_ipAddressType,
    awsElbv2LoadBalancerDetails_loadBalancerAttributes,
    awsElbv2LoadBalancerDetails_scheme,
    awsElbv2LoadBalancerDetails_securityGroups,
    awsElbv2LoadBalancerDetails_state,
    awsElbv2LoadBalancerDetails_type,
    awsElbv2LoadBalancerDetails_vpcId,

    -- ** AwsEventSchemasRegistryDetails
    awsEventSchemasRegistryDetails_description,
    awsEventSchemasRegistryDetails_registryArn,
    awsEventSchemasRegistryDetails_registryName,

    -- ** AwsGuardDutyDetectorDataSourcesCloudTrailDetails
    awsGuardDutyDetectorDataSourcesCloudTrailDetails_status,

    -- ** AwsGuardDutyDetectorDataSourcesDetails
    awsGuardDutyDetectorDataSourcesDetails_cloudTrail,
    awsGuardDutyDetectorDataSourcesDetails_dnsLogs,
    awsGuardDutyDetectorDataSourcesDetails_flowLogs,
    awsGuardDutyDetectorDataSourcesDetails_kubernetes,
    awsGuardDutyDetectorDataSourcesDetails_malwareProtection,
    awsGuardDutyDetectorDataSourcesDetails_s3Logs,

    -- ** AwsGuardDutyDetectorDataSourcesDnsLogsDetails
    awsGuardDutyDetectorDataSourcesDnsLogsDetails_status,

    -- ** AwsGuardDutyDetectorDataSourcesFlowLogsDetails
    awsGuardDutyDetectorDataSourcesFlowLogsDetails_status,

    -- ** AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
    awsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails_status,

    -- ** AwsGuardDutyDetectorDataSourcesKubernetesDetails
    awsGuardDutyDetectorDataSourcesKubernetesDetails_auditLogs,

    -- ** AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails
    awsGuardDutyDetectorDataSourcesMalwareProtectionDetails_scanEc2InstanceWithFindings,
    awsGuardDutyDetectorDataSourcesMalwareProtectionDetails_serviceRole,

    -- ** AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails_ebsVolumes,

    -- ** AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails_reason,
    awsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails_status,

    -- ** AwsGuardDutyDetectorDataSourcesS3LogsDetails
    awsGuardDutyDetectorDataSourcesS3LogsDetails_status,

    -- ** AwsGuardDutyDetectorDetails
    awsGuardDutyDetectorDetails_dataSources,
    awsGuardDutyDetectorDetails_features,
    awsGuardDutyDetectorDetails_findingPublishingFrequency,
    awsGuardDutyDetectorDetails_serviceRole,
    awsGuardDutyDetectorDetails_status,

    -- ** AwsGuardDutyDetectorFeaturesDetails
    awsGuardDutyDetectorFeaturesDetails_name,
    awsGuardDutyDetectorFeaturesDetails_status,

    -- ** AwsIamAccessKeyDetails
    awsIamAccessKeyDetails_accessKeyId,
    awsIamAccessKeyDetails_accountId,
    awsIamAccessKeyDetails_createdAt,
    awsIamAccessKeyDetails_principalId,
    awsIamAccessKeyDetails_principalName,
    awsIamAccessKeyDetails_principalType,
    awsIamAccessKeyDetails_sessionContext,
    awsIamAccessKeyDetails_status,
    awsIamAccessKeyDetails_userName,

    -- ** AwsIamAccessKeySessionContext
    awsIamAccessKeySessionContext_attributes,
    awsIamAccessKeySessionContext_sessionIssuer,

    -- ** AwsIamAccessKeySessionContextAttributes
    awsIamAccessKeySessionContextAttributes_creationDate,
    awsIamAccessKeySessionContextAttributes_mfaAuthenticated,

    -- ** AwsIamAccessKeySessionContextSessionIssuer
    awsIamAccessKeySessionContextSessionIssuer_accountId,
    awsIamAccessKeySessionContextSessionIssuer_arn,
    awsIamAccessKeySessionContextSessionIssuer_principalId,
    awsIamAccessKeySessionContextSessionIssuer_type,
    awsIamAccessKeySessionContextSessionIssuer_userName,

    -- ** AwsIamAttachedManagedPolicy
    awsIamAttachedManagedPolicy_policyArn,
    awsIamAttachedManagedPolicy_policyName,

    -- ** AwsIamGroupDetails
    awsIamGroupDetails_attachedManagedPolicies,
    awsIamGroupDetails_createDate,
    awsIamGroupDetails_groupId,
    awsIamGroupDetails_groupName,
    awsIamGroupDetails_groupPolicyList,
    awsIamGroupDetails_path,

    -- ** AwsIamGroupPolicy
    awsIamGroupPolicy_policyName,

    -- ** AwsIamInstanceProfile
    awsIamInstanceProfile_arn,
    awsIamInstanceProfile_createDate,
    awsIamInstanceProfile_instanceProfileId,
    awsIamInstanceProfile_instanceProfileName,
    awsIamInstanceProfile_path,
    awsIamInstanceProfile_roles,

    -- ** AwsIamInstanceProfileRole
    awsIamInstanceProfileRole_arn,
    awsIamInstanceProfileRole_assumeRolePolicyDocument,
    awsIamInstanceProfileRole_createDate,
    awsIamInstanceProfileRole_path,
    awsIamInstanceProfileRole_roleId,
    awsIamInstanceProfileRole_roleName,

    -- ** AwsIamPermissionsBoundary
    awsIamPermissionsBoundary_permissionsBoundaryArn,
    awsIamPermissionsBoundary_permissionsBoundaryType,

    -- ** AwsIamPolicyDetails
    awsIamPolicyDetails_attachmentCount,
    awsIamPolicyDetails_createDate,
    awsIamPolicyDetails_defaultVersionId,
    awsIamPolicyDetails_description,
    awsIamPolicyDetails_isAttachable,
    awsIamPolicyDetails_path,
    awsIamPolicyDetails_permissionsBoundaryUsageCount,
    awsIamPolicyDetails_policyId,
    awsIamPolicyDetails_policyName,
    awsIamPolicyDetails_policyVersionList,
    awsIamPolicyDetails_updateDate,

    -- ** AwsIamPolicyVersion
    awsIamPolicyVersion_createDate,
    awsIamPolicyVersion_isDefaultVersion,
    awsIamPolicyVersion_versionId,

    -- ** AwsIamRoleDetails
    awsIamRoleDetails_assumeRolePolicyDocument,
    awsIamRoleDetails_attachedManagedPolicies,
    awsIamRoleDetails_createDate,
    awsIamRoleDetails_instanceProfileList,
    awsIamRoleDetails_maxSessionDuration,
    awsIamRoleDetails_path,
    awsIamRoleDetails_permissionsBoundary,
    awsIamRoleDetails_roleId,
    awsIamRoleDetails_roleName,
    awsIamRoleDetails_rolePolicyList,

    -- ** AwsIamRolePolicy
    awsIamRolePolicy_policyName,

    -- ** AwsIamUserDetails
    awsIamUserDetails_attachedManagedPolicies,
    awsIamUserDetails_createDate,
    awsIamUserDetails_groupList,
    awsIamUserDetails_path,
    awsIamUserDetails_permissionsBoundary,
    awsIamUserDetails_userId,
    awsIamUserDetails_userName,
    awsIamUserDetails_userPolicyList,

    -- ** AwsIamUserPolicy
    awsIamUserPolicy_policyName,

    -- ** AwsKinesisStreamDetails
    awsKinesisStreamDetails_arn,
    awsKinesisStreamDetails_name,
    awsKinesisStreamDetails_retentionPeriodHours,
    awsKinesisStreamDetails_shardCount,
    awsKinesisStreamDetails_streamEncryption,

    -- ** AwsKinesisStreamStreamEncryptionDetails
    awsKinesisStreamStreamEncryptionDetails_encryptionType,
    awsKinesisStreamStreamEncryptionDetails_keyId,

    -- ** AwsKmsKeyDetails
    awsKmsKeyDetails_aWSAccountId,
    awsKmsKeyDetails_creationDate,
    awsKmsKeyDetails_description,
    awsKmsKeyDetails_keyId,
    awsKmsKeyDetails_keyManager,
    awsKmsKeyDetails_keyRotationStatus,
    awsKmsKeyDetails_keyState,
    awsKmsKeyDetails_origin,

    -- ** AwsLambdaFunctionCode
    awsLambdaFunctionCode_s3Bucket,
    awsLambdaFunctionCode_s3Key,
    awsLambdaFunctionCode_s3ObjectVersion,
    awsLambdaFunctionCode_zipFile,

    -- ** AwsLambdaFunctionDeadLetterConfig
    awsLambdaFunctionDeadLetterConfig_targetArn,

    -- ** AwsLambdaFunctionDetails
    awsLambdaFunctionDetails_architectures,
    awsLambdaFunctionDetails_code,
    awsLambdaFunctionDetails_codeSha256,
    awsLambdaFunctionDetails_deadLetterConfig,
    awsLambdaFunctionDetails_environment,
    awsLambdaFunctionDetails_functionName,
    awsLambdaFunctionDetails_handler,
    awsLambdaFunctionDetails_kmsKeyArn,
    awsLambdaFunctionDetails_lastModified,
    awsLambdaFunctionDetails_layers,
    awsLambdaFunctionDetails_masterArn,
    awsLambdaFunctionDetails_memorySize,
    awsLambdaFunctionDetails_packageType,
    awsLambdaFunctionDetails_revisionId,
    awsLambdaFunctionDetails_role,
    awsLambdaFunctionDetails_runtime,
    awsLambdaFunctionDetails_timeout,
    awsLambdaFunctionDetails_tracingConfig,
    awsLambdaFunctionDetails_version,
    awsLambdaFunctionDetails_vpcConfig,

    -- ** AwsLambdaFunctionEnvironment
    awsLambdaFunctionEnvironment_error,
    awsLambdaFunctionEnvironment_variables,

    -- ** AwsLambdaFunctionEnvironmentError
    awsLambdaFunctionEnvironmentError_errorCode,
    awsLambdaFunctionEnvironmentError_message,

    -- ** AwsLambdaFunctionLayer
    awsLambdaFunctionLayer_arn,
    awsLambdaFunctionLayer_codeSize,

    -- ** AwsLambdaFunctionTracingConfig
    awsLambdaFunctionTracingConfig_mode,

    -- ** AwsLambdaFunctionVpcConfig
    awsLambdaFunctionVpcConfig_securityGroupIds,
    awsLambdaFunctionVpcConfig_subnetIds,
    awsLambdaFunctionVpcConfig_vpcId,

    -- ** AwsLambdaLayerVersionDetails
    awsLambdaLayerVersionDetails_compatibleRuntimes,
    awsLambdaLayerVersionDetails_createdDate,
    awsLambdaLayerVersionDetails_version,

    -- ** AwsMountPoint
    awsMountPoint_containerPath,
    awsMountPoint_sourceVolume,

    -- ** AwsNetworkFirewallFirewallDetails
    awsNetworkFirewallFirewallDetails_deleteProtection,
    awsNetworkFirewallFirewallDetails_description,
    awsNetworkFirewallFirewallDetails_firewallArn,
    awsNetworkFirewallFirewallDetails_firewallId,
    awsNetworkFirewallFirewallDetails_firewallName,
    awsNetworkFirewallFirewallDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection,
    awsNetworkFirewallFirewallDetails_subnetChangeProtection,
    awsNetworkFirewallFirewallDetails_subnetMappings,
    awsNetworkFirewallFirewallDetails_vpcId,

    -- ** AwsNetworkFirewallFirewallPolicyDetails
    awsNetworkFirewallFirewallPolicyDetails_description,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicy,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId,
    awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName,

    -- ** AwsNetworkFirewallFirewallSubnetMappingsDetails
    awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId,

    -- ** AwsNetworkFirewallRuleGroupDetails
    awsNetworkFirewallRuleGroupDetails_capacity,
    awsNetworkFirewallRuleGroupDetails_description,
    awsNetworkFirewallRuleGroupDetails_ruleGroup,
    awsNetworkFirewallRuleGroupDetails_ruleGroupArn,
    awsNetworkFirewallRuleGroupDetails_ruleGroupId,
    awsNetworkFirewallRuleGroupDetails_ruleGroupName,
    awsNetworkFirewallRuleGroupDetails_type,

    -- ** AwsOpenSearchServiceDomainAdvancedSecurityOptionsDetails
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_enabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_internalUserDatabaseEnabled,
    awsOpenSearchServiceDomainAdvancedSecurityOptionsDetails_masterUserOptions,

    -- ** AwsOpenSearchServiceDomainClusterConfigDetails
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterCount,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_dedicatedMasterType,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceCount,
    awsOpenSearchServiceDomainClusterConfigDetails_instanceType,
    awsOpenSearchServiceDomainClusterConfigDetails_warmCount,
    awsOpenSearchServiceDomainClusterConfigDetails_warmEnabled,
    awsOpenSearchServiceDomainClusterConfigDetails_warmType,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessConfig,
    awsOpenSearchServiceDomainClusterConfigDetails_zoneAwarenessEnabled,

    -- ** AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
    awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount,

    -- ** AwsOpenSearchServiceDomainDetails
    awsOpenSearchServiceDomainDetails_accessPolicies,
    awsOpenSearchServiceDomainDetails_advancedSecurityOptions,
    awsOpenSearchServiceDomainDetails_arn,
    awsOpenSearchServiceDomainDetails_clusterConfig,
    awsOpenSearchServiceDomainDetails_domainEndpoint,
    awsOpenSearchServiceDomainDetails_domainEndpointOptions,
    awsOpenSearchServiceDomainDetails_domainEndpoints,
    awsOpenSearchServiceDomainDetails_domainName,
    awsOpenSearchServiceDomainDetails_encryptionAtRestOptions,
    awsOpenSearchServiceDomainDetails_engineVersion,
    awsOpenSearchServiceDomainDetails_id,
    awsOpenSearchServiceDomainDetails_logPublishingOptions,
    awsOpenSearchServiceDomainDetails_nodeToNodeEncryptionOptions,
    awsOpenSearchServiceDomainDetails_serviceSoftwareOptions,
    awsOpenSearchServiceDomainDetails_vpcOptions,

    -- ** AwsOpenSearchServiceDomainDomainEndpointOptionsDetails
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpoint,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointCertificateArn,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_customEndpointEnabled,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_enforceHTTPS,
    awsOpenSearchServiceDomainDomainEndpointOptionsDetails_tLSSecurityPolicy,

    -- ** AwsOpenSearchServiceDomainEncryptionAtRestOptionsDetails
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_enabled,
    awsOpenSearchServiceDomainEncryptionAtRestOptionsDetails_kmsKeyId,

    -- ** AwsOpenSearchServiceDomainLogPublishingOption
    awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn,
    awsOpenSearchServiceDomainLogPublishingOption_enabled,

    -- ** AwsOpenSearchServiceDomainLogPublishingOptionsDetails
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs,
    awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs,

    -- ** AwsOpenSearchServiceDomainMasterUserOptionsDetails
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName,
    awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword,

    -- ** AwsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails
    awsOpenSearchServiceDomainNodeToNodeEncryptionOptionsDetails_enabled,

    -- ** AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable,
    awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus,

    -- ** AwsOpenSearchServiceDomainVpcOptionsDetails
    awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds,
    awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds,

    -- ** AwsRdsDbClusterAssociatedRole
    awsRdsDbClusterAssociatedRole_roleArn,
    awsRdsDbClusterAssociatedRole_status,

    -- ** AwsRdsDbClusterDetails
    awsRdsDbClusterDetails_activityStreamStatus,
    awsRdsDbClusterDetails_allocatedStorage,
    awsRdsDbClusterDetails_associatedRoles,
    awsRdsDbClusterDetails_availabilityZones,
    awsRdsDbClusterDetails_backupRetentionPeriod,
    awsRdsDbClusterDetails_clusterCreateTime,
    awsRdsDbClusterDetails_copyTagsToSnapshot,
    awsRdsDbClusterDetails_crossAccountClone,
    awsRdsDbClusterDetails_customEndpoints,
    awsRdsDbClusterDetails_databaseName,
    awsRdsDbClusterDetails_dbClusterIdentifier,
    awsRdsDbClusterDetails_dbClusterMembers,
    awsRdsDbClusterDetails_dbClusterOptionGroupMemberships,
    awsRdsDbClusterDetails_dbClusterParameterGroup,
    awsRdsDbClusterDetails_dbClusterResourceId,
    awsRdsDbClusterDetails_dbSubnetGroup,
    awsRdsDbClusterDetails_deletionProtection,
    awsRdsDbClusterDetails_domainMemberships,
    awsRdsDbClusterDetails_enabledCloudWatchLogsExports,
    awsRdsDbClusterDetails_endpoint,
    awsRdsDbClusterDetails_engine,
    awsRdsDbClusterDetails_engineMode,
    awsRdsDbClusterDetails_engineVersion,
    awsRdsDbClusterDetails_hostedZoneId,
    awsRdsDbClusterDetails_httpEndpointEnabled,
    awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterDetails_kmsKeyId,
    awsRdsDbClusterDetails_masterUsername,
    awsRdsDbClusterDetails_multiAz,
    awsRdsDbClusterDetails_port,
    awsRdsDbClusterDetails_preferredBackupWindow,
    awsRdsDbClusterDetails_preferredMaintenanceWindow,
    awsRdsDbClusterDetails_readReplicaIdentifiers,
    awsRdsDbClusterDetails_readerEndpoint,
    awsRdsDbClusterDetails_status,
    awsRdsDbClusterDetails_storageEncrypted,
    awsRdsDbClusterDetails_vpcSecurityGroups,

    -- ** AwsRdsDbClusterMember
    awsRdsDbClusterMember_dbClusterParameterGroupStatus,
    awsRdsDbClusterMember_dbInstanceIdentifier,
    awsRdsDbClusterMember_isClusterWriter,
    awsRdsDbClusterMember_promotionTier,

    -- ** AwsRdsDbClusterOptionGroupMembership
    awsRdsDbClusterOptionGroupMembership_dbClusterOptionGroupName,
    awsRdsDbClusterOptionGroupMembership_status,

    -- ** AwsRdsDbClusterSnapshotDetails
    awsRdsDbClusterSnapshotDetails_allocatedStorage,
    awsRdsDbClusterSnapshotDetails_availabilityZones,
    awsRdsDbClusterSnapshotDetails_clusterCreateTime,
    awsRdsDbClusterSnapshotDetails_dbClusterIdentifier,
    awsRdsDbClusterSnapshotDetails_dbClusterSnapshotIdentifier,
    awsRdsDbClusterSnapshotDetails_engine,
    awsRdsDbClusterSnapshotDetails_engineVersion,
    awsRdsDbClusterSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbClusterSnapshotDetails_kmsKeyId,
    awsRdsDbClusterSnapshotDetails_licenseModel,
    awsRdsDbClusterSnapshotDetails_masterUsername,
    awsRdsDbClusterSnapshotDetails_percentProgress,
    awsRdsDbClusterSnapshotDetails_port,
    awsRdsDbClusterSnapshotDetails_snapshotCreateTime,
    awsRdsDbClusterSnapshotDetails_snapshotType,
    awsRdsDbClusterSnapshotDetails_status,
    awsRdsDbClusterSnapshotDetails_storageEncrypted,
    awsRdsDbClusterSnapshotDetails_vpcId,

    -- ** AwsRdsDbDomainMembership
    awsRdsDbDomainMembership_domain,
    awsRdsDbDomainMembership_fqdn,
    awsRdsDbDomainMembership_iamRoleName,
    awsRdsDbDomainMembership_status,

    -- ** AwsRdsDbInstanceAssociatedRole
    awsRdsDbInstanceAssociatedRole_featureName,
    awsRdsDbInstanceAssociatedRole_roleArn,
    awsRdsDbInstanceAssociatedRole_status,

    -- ** AwsRdsDbInstanceDetails
    awsRdsDbInstanceDetails_allocatedStorage,
    awsRdsDbInstanceDetails_associatedRoles,
    awsRdsDbInstanceDetails_autoMinorVersionUpgrade,
    awsRdsDbInstanceDetails_availabilityZone,
    awsRdsDbInstanceDetails_backupRetentionPeriod,
    awsRdsDbInstanceDetails_cACertificateIdentifier,
    awsRdsDbInstanceDetails_characterSetName,
    awsRdsDbInstanceDetails_copyTagsToSnapshot,
    awsRdsDbInstanceDetails_dbClusterIdentifier,
    awsRdsDbInstanceDetails_dbInstanceClass,
    awsRdsDbInstanceDetails_dbInstanceIdentifier,
    awsRdsDbInstanceDetails_dbName,
    awsRdsDbInstanceDetails_dbInstancePort,
    awsRdsDbInstanceDetails_dbInstanceStatus,
    awsRdsDbInstanceDetails_dbParameterGroups,
    awsRdsDbInstanceDetails_dbSecurityGroups,
    awsRdsDbInstanceDetails_dbSubnetGroup,
    awsRdsDbInstanceDetails_dbiResourceId,
    awsRdsDbInstanceDetails_deletionProtection,
    awsRdsDbInstanceDetails_domainMemberships,
    awsRdsDbInstanceDetails_enabledCloudWatchLogsExports,
    awsRdsDbInstanceDetails_endpoint,
    awsRdsDbInstanceDetails_engine,
    awsRdsDbInstanceDetails_engineVersion,
    awsRdsDbInstanceDetails_enhancedMonitoringResourceArn,
    awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled,
    awsRdsDbInstanceDetails_instanceCreateTime,
    awsRdsDbInstanceDetails_iops,
    awsRdsDbInstanceDetails_kmsKeyId,
    awsRdsDbInstanceDetails_latestRestorableTime,
    awsRdsDbInstanceDetails_licenseModel,
    awsRdsDbInstanceDetails_listenerEndpoint,
    awsRdsDbInstanceDetails_masterUsername,
    awsRdsDbInstanceDetails_maxAllocatedStorage,
    awsRdsDbInstanceDetails_monitoringInterval,
    awsRdsDbInstanceDetails_monitoringRoleArn,
    awsRdsDbInstanceDetails_multiAz,
    awsRdsDbInstanceDetails_optionGroupMemberships,
    awsRdsDbInstanceDetails_pendingModifiedValues,
    awsRdsDbInstanceDetails_performanceInsightsEnabled,
    awsRdsDbInstanceDetails_performanceInsightsKmsKeyId,
    awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod,
    awsRdsDbInstanceDetails_preferredBackupWindow,
    awsRdsDbInstanceDetails_preferredMaintenanceWindow,
    awsRdsDbInstanceDetails_processorFeatures,
    awsRdsDbInstanceDetails_promotionTier,
    awsRdsDbInstanceDetails_publiclyAccessible,
    awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers,
    awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers,
    awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier,
    awsRdsDbInstanceDetails_secondaryAvailabilityZone,
    awsRdsDbInstanceDetails_statusInfos,
    awsRdsDbInstanceDetails_storageEncrypted,
    awsRdsDbInstanceDetails_storageType,
    awsRdsDbInstanceDetails_tdeCredentialArn,
    awsRdsDbInstanceDetails_timezone,
    awsRdsDbInstanceDetails_vpcSecurityGroups,

    -- ** AwsRdsDbInstanceEndpoint
    awsRdsDbInstanceEndpoint_address,
    awsRdsDbInstanceEndpoint_hostedZoneId,
    awsRdsDbInstanceEndpoint_port,

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
    awsRdsDbPendingModifiedValues_allocatedStorage,
    awsRdsDbPendingModifiedValues_backupRetentionPeriod,
    awsRdsDbPendingModifiedValues_caCertificateIdentifier,
    awsRdsDbPendingModifiedValues_dbInstanceClass,
    awsRdsDbPendingModifiedValues_dbInstanceIdentifier,
    awsRdsDbPendingModifiedValues_dbSubnetGroupName,
    awsRdsDbPendingModifiedValues_engineVersion,
    awsRdsDbPendingModifiedValues_iops,
    awsRdsDbPendingModifiedValues_licenseModel,
    awsRdsDbPendingModifiedValues_masterUserPassword,
    awsRdsDbPendingModifiedValues_multiAZ,
    awsRdsDbPendingModifiedValues_pendingCloudWatchLogsExports,
    awsRdsDbPendingModifiedValues_port,
    awsRdsDbPendingModifiedValues_processorFeatures,
    awsRdsDbPendingModifiedValues_storageType,

    -- ** AwsRdsDbProcessorFeature
    awsRdsDbProcessorFeature_name,
    awsRdsDbProcessorFeature_value,

    -- ** AwsRdsDbSecurityGroupDetails
    awsRdsDbSecurityGroupDetails_dbSecurityGroupArn,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupDescription,
    awsRdsDbSecurityGroupDetails_dbSecurityGroupName,
    awsRdsDbSecurityGroupDetails_ec2SecurityGroups,
    awsRdsDbSecurityGroupDetails_ipRanges,
    awsRdsDbSecurityGroupDetails_ownerId,
    awsRdsDbSecurityGroupDetails_vpcId,

    -- ** AwsRdsDbSecurityGroupEc2SecurityGroup
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupId,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupName,
    awsRdsDbSecurityGroupEc2SecurityGroup_ec2SecurityGroupOwnerId,
    awsRdsDbSecurityGroupEc2SecurityGroup_status,

    -- ** AwsRdsDbSecurityGroupIpRange
    awsRdsDbSecurityGroupIpRange_cidrIp,
    awsRdsDbSecurityGroupIpRange_status,

    -- ** AwsRdsDbSnapshotDetails
    awsRdsDbSnapshotDetails_allocatedStorage,
    awsRdsDbSnapshotDetails_availabilityZone,
    awsRdsDbSnapshotDetails_dbInstanceIdentifier,
    awsRdsDbSnapshotDetails_dbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_dbiResourceId,
    awsRdsDbSnapshotDetails_encrypted,
    awsRdsDbSnapshotDetails_engine,
    awsRdsDbSnapshotDetails_engineVersion,
    awsRdsDbSnapshotDetails_iamDatabaseAuthenticationEnabled,
    awsRdsDbSnapshotDetails_instanceCreateTime,
    awsRdsDbSnapshotDetails_iops,
    awsRdsDbSnapshotDetails_kmsKeyId,
    awsRdsDbSnapshotDetails_licenseModel,
    awsRdsDbSnapshotDetails_masterUsername,
    awsRdsDbSnapshotDetails_optionGroupName,
    awsRdsDbSnapshotDetails_percentProgress,
    awsRdsDbSnapshotDetails_port,
    awsRdsDbSnapshotDetails_processorFeatures,
    awsRdsDbSnapshotDetails_snapshotCreateTime,
    awsRdsDbSnapshotDetails_snapshotType,
    awsRdsDbSnapshotDetails_sourceDbSnapshotIdentifier,
    awsRdsDbSnapshotDetails_sourceRegion,
    awsRdsDbSnapshotDetails_status,
    awsRdsDbSnapshotDetails_storageType,
    awsRdsDbSnapshotDetails_tdeCredentialArn,
    awsRdsDbSnapshotDetails_timezone,
    awsRdsDbSnapshotDetails_vpcId,

    -- ** AwsRdsDbStatusInfo
    awsRdsDbStatusInfo_message,
    awsRdsDbStatusInfo_normal,
    awsRdsDbStatusInfo_status,
    awsRdsDbStatusInfo_statusType,

    -- ** AwsRdsDbSubnetGroup
    awsRdsDbSubnetGroup_dbSubnetGroupArn,
    awsRdsDbSubnetGroup_dbSubnetGroupDescription,
    awsRdsDbSubnetGroup_dbSubnetGroupName,
    awsRdsDbSubnetGroup_subnetGroupStatus,
    awsRdsDbSubnetGroup_subnets,
    awsRdsDbSubnetGroup_vpcId,

    -- ** AwsRdsDbSubnetGroupSubnet
    awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone,
    awsRdsDbSubnetGroupSubnet_subnetIdentifier,
    awsRdsDbSubnetGroupSubnet_subnetStatus,

    -- ** AwsRdsDbSubnetGroupSubnetAvailabilityZone
    awsRdsDbSubnetGroupSubnetAvailabilityZone_name,

    -- ** AwsRdsEventSubscriptionDetails
    awsRdsEventSubscriptionDetails_custSubscriptionId,
    awsRdsEventSubscriptionDetails_customerAwsId,
    awsRdsEventSubscriptionDetails_enabled,
    awsRdsEventSubscriptionDetails_eventCategoriesList,
    awsRdsEventSubscriptionDetails_eventSubscriptionArn,
    awsRdsEventSubscriptionDetails_snsTopicArn,
    awsRdsEventSubscriptionDetails_sourceIdsList,
    awsRdsEventSubscriptionDetails_sourceType,
    awsRdsEventSubscriptionDetails_status,
    awsRdsEventSubscriptionDetails_subscriptionCreationTime,

    -- ** AwsRdsPendingCloudWatchLogsExports
    awsRdsPendingCloudWatchLogsExports_logTypesToDisable,
    awsRdsPendingCloudWatchLogsExports_logTypesToEnable,

    -- ** AwsRedshiftClusterClusterNode
    awsRedshiftClusterClusterNode_nodeRole,
    awsRedshiftClusterClusterNode_privateIpAddress,
    awsRedshiftClusterClusterNode_publicIpAddress,

    -- ** AwsRedshiftClusterClusterParameterGroup
    awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList,
    awsRedshiftClusterClusterParameterGroup_parameterApplyStatus,
    awsRedshiftClusterClusterParameterGroup_parameterGroupName,

    -- ** AwsRedshiftClusterClusterParameterStatus
    awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription,
    awsRedshiftClusterClusterParameterStatus_parameterApplyStatus,
    awsRedshiftClusterClusterParameterStatus_parameterName,

    -- ** AwsRedshiftClusterClusterSecurityGroup
    awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName,
    awsRedshiftClusterClusterSecurityGroup_status,

    -- ** AwsRedshiftClusterClusterSnapshotCopyStatus
    awsRedshiftClusterClusterSnapshotCopyStatus_destinationRegion,
    awsRedshiftClusterClusterSnapshotCopyStatus_manualSnapshotRetentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_retentionPeriod,
    awsRedshiftClusterClusterSnapshotCopyStatus_snapshotCopyGrantName,

    -- ** AwsRedshiftClusterDeferredMaintenanceWindow
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier,
    awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime,

    -- ** AwsRedshiftClusterDetails
    awsRedshiftClusterDetails_allowVersionUpgrade,
    awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_availabilityZone,
    awsRedshiftClusterDetails_clusterAvailabilityStatus,
    awsRedshiftClusterDetails_clusterCreateTime,
    awsRedshiftClusterDetails_clusterIdentifier,
    awsRedshiftClusterDetails_clusterNodes,
    awsRedshiftClusterDetails_clusterParameterGroups,
    awsRedshiftClusterDetails_clusterPublicKey,
    awsRedshiftClusterDetails_clusterRevisionNumber,
    awsRedshiftClusterDetails_clusterSecurityGroups,
    awsRedshiftClusterDetails_clusterSnapshotCopyStatus,
    awsRedshiftClusterDetails_clusterStatus,
    awsRedshiftClusterDetails_clusterSubnetGroupName,
    awsRedshiftClusterDetails_clusterVersion,
    awsRedshiftClusterDetails_dbName,
    awsRedshiftClusterDetails_deferredMaintenanceWindows,
    awsRedshiftClusterDetails_elasticIpStatus,
    awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions,
    awsRedshiftClusterDetails_encrypted,
    awsRedshiftClusterDetails_endpoint,
    awsRedshiftClusterDetails_enhancedVpcRouting,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime,
    awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus,
    awsRedshiftClusterDetails_hsmStatus,
    awsRedshiftClusterDetails_iamRoles,
    awsRedshiftClusterDetails_kmsKeyId,
    awsRedshiftClusterDetails_loggingStatus,
    awsRedshiftClusterDetails_maintenanceTrackName,
    awsRedshiftClusterDetails_manualSnapshotRetentionPeriod,
    awsRedshiftClusterDetails_masterUsername,
    awsRedshiftClusterDetails_nextMaintenanceWindowStartTime,
    awsRedshiftClusterDetails_nodeType,
    awsRedshiftClusterDetails_numberOfNodes,
    awsRedshiftClusterDetails_pendingActions,
    awsRedshiftClusterDetails_pendingModifiedValues,
    awsRedshiftClusterDetails_preferredMaintenanceWindow,
    awsRedshiftClusterDetails_publiclyAccessible,
    awsRedshiftClusterDetails_resizeInfo,
    awsRedshiftClusterDetails_restoreStatus,
    awsRedshiftClusterDetails_snapshotScheduleIdentifier,
    awsRedshiftClusterDetails_snapshotScheduleState,
    awsRedshiftClusterDetails_vpcId,
    awsRedshiftClusterDetails_vpcSecurityGroups,

    -- ** AwsRedshiftClusterElasticIpStatus
    awsRedshiftClusterElasticIpStatus_elasticIp,
    awsRedshiftClusterElasticIpStatus_status,

    -- ** AwsRedshiftClusterEndpoint
    awsRedshiftClusterEndpoint_address,
    awsRedshiftClusterEndpoint_port,

    -- ** AwsRedshiftClusterHsmStatus
    awsRedshiftClusterHsmStatus_hsmClientCertificateIdentifier,
    awsRedshiftClusterHsmStatus_hsmConfigurationIdentifier,
    awsRedshiftClusterHsmStatus_status,

    -- ** AwsRedshiftClusterIamRole
    awsRedshiftClusterIamRole_applyStatus,
    awsRedshiftClusterIamRole_iamRoleArn,

    -- ** AwsRedshiftClusterLoggingStatus
    awsRedshiftClusterLoggingStatus_bucketName,
    awsRedshiftClusterLoggingStatus_lastFailureMessage,
    awsRedshiftClusterLoggingStatus_lastFailureTime,
    awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime,
    awsRedshiftClusterLoggingStatus_loggingEnabled,
    awsRedshiftClusterLoggingStatus_s3KeyPrefix,

    -- ** AwsRedshiftClusterPendingModifiedValues
    awsRedshiftClusterPendingModifiedValues_automatedSnapshotRetentionPeriod,
    awsRedshiftClusterPendingModifiedValues_clusterIdentifier,
    awsRedshiftClusterPendingModifiedValues_clusterType,
    awsRedshiftClusterPendingModifiedValues_clusterVersion,
    awsRedshiftClusterPendingModifiedValues_encryptionType,
    awsRedshiftClusterPendingModifiedValues_enhancedVpcRouting,
    awsRedshiftClusterPendingModifiedValues_maintenanceTrackName,
    awsRedshiftClusterPendingModifiedValues_masterUserPassword,
    awsRedshiftClusterPendingModifiedValues_nodeType,
    awsRedshiftClusterPendingModifiedValues_numberOfNodes,
    awsRedshiftClusterPendingModifiedValues_publiclyAccessible,

    -- ** AwsRedshiftClusterResizeInfo
    awsRedshiftClusterResizeInfo_allowCancelResize,
    awsRedshiftClusterResizeInfo_resizeType,

    -- ** AwsRedshiftClusterRestoreStatus
    awsRedshiftClusterRestoreStatus_currentRestoreRateInMegaBytesPerSecond,
    awsRedshiftClusterRestoreStatus_elapsedTimeInSeconds,
    awsRedshiftClusterRestoreStatus_estimatedTimeToCompletionInSeconds,
    awsRedshiftClusterRestoreStatus_progressInMegaBytes,
    awsRedshiftClusterRestoreStatus_snapshotSizeInMegaBytes,
    awsRedshiftClusterRestoreStatus_status,

    -- ** AwsRedshiftClusterVpcSecurityGroup
    awsRedshiftClusterVpcSecurityGroup_status,
    awsRedshiftClusterVpcSecurityGroup_vpcSecurityGroupId,

    -- ** AwsS3AccountPublicAccessBlockDetails
    awsS3AccountPublicAccessBlockDetails_blockPublicAcls,
    awsS3AccountPublicAccessBlockDetails_blockPublicPolicy,
    awsS3AccountPublicAccessBlockDetails_ignorePublicAcls,
    awsS3AccountPublicAccessBlockDetails_restrictPublicBuckets,

    -- ** AwsS3BucketBucketLifecycleConfigurationDetails
    awsS3BucketBucketLifecycleConfigurationDetails_rules,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
    awsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails_daysAfterInitiation,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesDetails
    awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_filter,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_id,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_status,
    awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type,

    -- ** AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag,
    awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type,

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
    awsS3BucketDetails_bucketLifecycleConfiguration,
    awsS3BucketDetails_bucketLoggingConfiguration,
    awsS3BucketDetails_bucketNotificationConfiguration,
    awsS3BucketDetails_bucketVersioningConfiguration,
    awsS3BucketDetails_bucketWebsiteConfiguration,
    awsS3BucketDetails_createdAt,
    awsS3BucketDetails_objectLockConfiguration,
    awsS3BucketDetails_ownerAccountId,
    awsS3BucketDetails_ownerId,
    awsS3BucketDetails_ownerName,
    awsS3BucketDetails_publicAccessBlockConfiguration,
    awsS3BucketDetails_serverSideEncryptionConfiguration,

    -- ** AwsS3BucketLoggingConfiguration
    awsS3BucketLoggingConfiguration_destinationBucketName,
    awsS3BucketLoggingConfiguration_logFilePrefix,

    -- ** AwsS3BucketNotificationConfiguration
    awsS3BucketNotificationConfiguration_configurations,

    -- ** AwsS3BucketNotificationConfigurationDetail
    awsS3BucketNotificationConfigurationDetail_destination,
    awsS3BucketNotificationConfigurationDetail_events,
    awsS3BucketNotificationConfigurationDetail_filter,
    awsS3BucketNotificationConfigurationDetail_type,

    -- ** AwsS3BucketNotificationConfigurationFilter
    awsS3BucketNotificationConfigurationFilter_s3KeyFilter,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilter
    awsS3BucketNotificationConfigurationS3KeyFilter_filterRules,

    -- ** AwsS3BucketNotificationConfigurationS3KeyFilterRule
    awsS3BucketNotificationConfigurationS3KeyFilterRule_name,
    awsS3BucketNotificationConfigurationS3KeyFilterRule_value,

    -- ** AwsS3BucketObjectLockConfiguration
    awsS3BucketObjectLockConfiguration_objectLockEnabled,
    awsS3BucketObjectLockConfiguration_rule,

    -- ** AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_days,
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_mode,
    awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_years,

    -- ** AwsS3BucketObjectLockConfigurationRuleDetails
    awsS3BucketObjectLockConfigurationRuleDetails_defaultRetention,

    -- ** AwsS3BucketServerSideEncryptionByDefault
    awsS3BucketServerSideEncryptionByDefault_kmsMasterKeyID,
    awsS3BucketServerSideEncryptionByDefault_sSEAlgorithm,

    -- ** AwsS3BucketServerSideEncryptionConfiguration
    awsS3BucketServerSideEncryptionConfiguration_rules,

    -- ** AwsS3BucketServerSideEncryptionRule
    awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault,

    -- ** AwsS3BucketWebsiteConfiguration
    awsS3BucketWebsiteConfiguration_errorDocument,
    awsS3BucketWebsiteConfiguration_indexDocumentSuffix,
    awsS3BucketWebsiteConfiguration_redirectAllRequestsTo,
    awsS3BucketWebsiteConfiguration_routingRules,

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
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith,
    awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith,

    -- ** AwsS3ObjectDetails
    awsS3ObjectDetails_contentType,
    awsS3ObjectDetails_eTag,
    awsS3ObjectDetails_lastModified,
    awsS3ObjectDetails_sSEKMSKeyId,
    awsS3ObjectDetails_serverSideEncryption,
    awsS3ObjectDetails_versionId,

    -- ** AwsSageMakerNotebookInstanceDetails
    awsSageMakerNotebookInstanceDetails_acceleratorTypes,
    awsSageMakerNotebookInstanceDetails_additionalCodeRepositories,
    awsSageMakerNotebookInstanceDetails_defaultCodeRepository,
    awsSageMakerNotebookInstanceDetails_directInternetAccess,
    awsSageMakerNotebookInstanceDetails_failureReason,
    awsSageMakerNotebookInstanceDetails_instanceMetadataServiceConfiguration,
    awsSageMakerNotebookInstanceDetails_instanceType,
    awsSageMakerNotebookInstanceDetails_kmsKeyId,
    awsSageMakerNotebookInstanceDetails_networkInterfaceId,
    awsSageMakerNotebookInstanceDetails_notebookInstanceArn,
    awsSageMakerNotebookInstanceDetails_notebookInstanceLifecycleConfigName,
    awsSageMakerNotebookInstanceDetails_notebookInstanceName,
    awsSageMakerNotebookInstanceDetails_notebookInstanceStatus,
    awsSageMakerNotebookInstanceDetails_platformIdentifier,
    awsSageMakerNotebookInstanceDetails_roleArn,
    awsSageMakerNotebookInstanceDetails_rootAccess,
    awsSageMakerNotebookInstanceDetails_securityGroups,
    awsSageMakerNotebookInstanceDetails_subnetId,
    awsSageMakerNotebookInstanceDetails_url,
    awsSageMakerNotebookInstanceDetails_volumeSizeInGB,

    -- ** AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails
    awsSageMakerNotebookInstanceMetadataServiceConfigurationDetails_minimumInstanceMetadataServiceVersion,

    -- ** AwsSecretsManagerSecretDetails
    awsSecretsManagerSecretDetails_deleted,
    awsSecretsManagerSecretDetails_description,
    awsSecretsManagerSecretDetails_kmsKeyId,
    awsSecretsManagerSecretDetails_name,
    awsSecretsManagerSecretDetails_rotationEnabled,
    awsSecretsManagerSecretDetails_rotationLambdaArn,
    awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency,
    awsSecretsManagerSecretDetails_rotationRules,

    -- ** AwsSecretsManagerSecretRotationRules
    awsSecretsManagerSecretRotationRules_automaticallyAfterDays,

    -- ** AwsSecurityFinding
    awsSecurityFinding_action,
    awsSecurityFinding_companyName,
    awsSecurityFinding_compliance,
    awsSecurityFinding_confidence,
    awsSecurityFinding_criticality,
    awsSecurityFinding_findingProviderFields,
    awsSecurityFinding_firstObservedAt,
    awsSecurityFinding_lastObservedAt,
    awsSecurityFinding_malware,
    awsSecurityFinding_network,
    awsSecurityFinding_networkPath,
    awsSecurityFinding_note,
    awsSecurityFinding_patchSummary,
    awsSecurityFinding_process,
    awsSecurityFinding_productFields,
    awsSecurityFinding_productName,
    awsSecurityFinding_recordState,
    awsSecurityFinding_region,
    awsSecurityFinding_relatedFindings,
    awsSecurityFinding_remediation,
    awsSecurityFinding_sample,
    awsSecurityFinding_severity,
    awsSecurityFinding_sourceUrl,
    awsSecurityFinding_threatIntelIndicators,
    awsSecurityFinding_threats,
    awsSecurityFinding_types,
    awsSecurityFinding_userDefinedFields,
    awsSecurityFinding_verificationState,
    awsSecurityFinding_vulnerabilities,
    awsSecurityFinding_workflow,
    awsSecurityFinding_workflowState,
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
    awsSecurityFindingFilters_awsAccountId,
    awsSecurityFindingFilters_companyName,
    awsSecurityFindingFilters_complianceAssociatedStandardsId,
    awsSecurityFindingFilters_complianceSecurityControlId,
    awsSecurityFindingFilters_complianceStatus,
    awsSecurityFindingFilters_confidence,
    awsSecurityFindingFilters_createdAt,
    awsSecurityFindingFilters_criticality,
    awsSecurityFindingFilters_description,
    awsSecurityFindingFilters_findingProviderFieldsConfidence,
    awsSecurityFindingFilters_findingProviderFieldsCriticality,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId,
    awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn,
    awsSecurityFindingFilters_findingProviderFieldsSeverityLabel,
    awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal,
    awsSecurityFindingFilters_findingProviderFieldsTypes,
    awsSecurityFindingFilters_firstObservedAt,
    awsSecurityFindingFilters_generatorId,
    awsSecurityFindingFilters_id,
    awsSecurityFindingFilters_keyword,
    awsSecurityFindingFilters_lastObservedAt,
    awsSecurityFindingFilters_malwareName,
    awsSecurityFindingFilters_malwarePath,
    awsSecurityFindingFilters_malwareState,
    awsSecurityFindingFilters_malwareType,
    awsSecurityFindingFilters_networkDestinationDomain,
    awsSecurityFindingFilters_networkDestinationIpV4,
    awsSecurityFindingFilters_networkDestinationIpV6,
    awsSecurityFindingFilters_networkDestinationPort,
    awsSecurityFindingFilters_networkDirection,
    awsSecurityFindingFilters_networkProtocol,
    awsSecurityFindingFilters_networkSourceDomain,
    awsSecurityFindingFilters_networkSourceIpV4,
    awsSecurityFindingFilters_networkSourceIpV6,
    awsSecurityFindingFilters_networkSourceMac,
    awsSecurityFindingFilters_networkSourcePort,
    awsSecurityFindingFilters_noteText,
    awsSecurityFindingFilters_noteUpdatedAt,
    awsSecurityFindingFilters_noteUpdatedBy,
    awsSecurityFindingFilters_processLaunchedAt,
    awsSecurityFindingFilters_processName,
    awsSecurityFindingFilters_processParentPid,
    awsSecurityFindingFilters_processPath,
    awsSecurityFindingFilters_processPid,
    awsSecurityFindingFilters_processTerminatedAt,
    awsSecurityFindingFilters_productArn,
    awsSecurityFindingFilters_productFields,
    awsSecurityFindingFilters_productName,
    awsSecurityFindingFilters_recommendationText,
    awsSecurityFindingFilters_recordState,
    awsSecurityFindingFilters_region,
    awsSecurityFindingFilters_relatedFindingsId,
    awsSecurityFindingFilters_relatedFindingsProductArn,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn,
    awsSecurityFindingFilters_resourceAwsEc2InstanceImageId,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses,
    awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses,
    awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName,
    awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt,
    awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId,
    awsSecurityFindingFilters_resourceAwsEc2InstanceType,
    awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus,
    awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName,
    awsSecurityFindingFilters_resourceAwsIamUserUserName,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerId,
    awsSecurityFindingFilters_resourceAwsS3BucketOwnerName,
    awsSecurityFindingFilters_resourceContainerImageId,
    awsSecurityFindingFilters_resourceContainerImageName,
    awsSecurityFindingFilters_resourceContainerLaunchedAt,
    awsSecurityFindingFilters_resourceContainerName,
    awsSecurityFindingFilters_resourceDetailsOther,
    awsSecurityFindingFilters_resourceId,
    awsSecurityFindingFilters_resourcePartition,
    awsSecurityFindingFilters_resourceRegion,
    awsSecurityFindingFilters_resourceTags,
    awsSecurityFindingFilters_resourceType,
    awsSecurityFindingFilters_sample,
    awsSecurityFindingFilters_severityLabel,
    awsSecurityFindingFilters_severityNormalized,
    awsSecurityFindingFilters_severityProduct,
    awsSecurityFindingFilters_sourceUrl,
    awsSecurityFindingFilters_threatIntelIndicatorCategory,
    awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt,
    awsSecurityFindingFilters_threatIntelIndicatorSource,
    awsSecurityFindingFilters_threatIntelIndicatorSourceUrl,
    awsSecurityFindingFilters_threatIntelIndicatorType,
    awsSecurityFindingFilters_threatIntelIndicatorValue,
    awsSecurityFindingFilters_title,
    awsSecurityFindingFilters_type,
    awsSecurityFindingFilters_updatedAt,
    awsSecurityFindingFilters_userDefinedFields,
    awsSecurityFindingFilters_verificationState,
    awsSecurityFindingFilters_workflowState,
    awsSecurityFindingFilters_workflowStatus,

    -- ** AwsSecurityFindingIdentifier
    awsSecurityFindingIdentifier_id,
    awsSecurityFindingIdentifier_productArn,

    -- ** AwsSnsTopicDetails
    awsSnsTopicDetails_applicationSuccessFeedbackRoleArn,
    awsSnsTopicDetails_firehoseFailureFeedbackRoleArn,
    awsSnsTopicDetails_firehoseSuccessFeedbackRoleArn,
    awsSnsTopicDetails_httpFailureFeedbackRoleArn,
    awsSnsTopicDetails_httpSuccessFeedbackRoleArn,
    awsSnsTopicDetails_kmsMasterKeyId,
    awsSnsTopicDetails_owner,
    awsSnsTopicDetails_sqsFailureFeedbackRoleArn,
    awsSnsTopicDetails_sqsSuccessFeedbackRoleArn,
    awsSnsTopicDetails_subscription,
    awsSnsTopicDetails_topicName,

    -- ** AwsSnsTopicSubscription
    awsSnsTopicSubscription_endpoint,
    awsSnsTopicSubscription_protocol,

    -- ** AwsSqsQueueDetails
    awsSqsQueueDetails_deadLetterTargetArn,
    awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds,
    awsSqsQueueDetails_kmsMasterKeyId,
    awsSqsQueueDetails_queueName,

    -- ** AwsSsmComplianceSummary
    awsSsmComplianceSummary_complianceType,
    awsSsmComplianceSummary_compliantCriticalCount,
    awsSsmComplianceSummary_compliantHighCount,
    awsSsmComplianceSummary_compliantInformationalCount,
    awsSsmComplianceSummary_compliantLowCount,
    awsSsmComplianceSummary_compliantMediumCount,
    awsSsmComplianceSummary_compliantUnspecifiedCount,
    awsSsmComplianceSummary_executionType,
    awsSsmComplianceSummary_nonCompliantCriticalCount,
    awsSsmComplianceSummary_nonCompliantHighCount,
    awsSsmComplianceSummary_nonCompliantInformationalCount,
    awsSsmComplianceSummary_nonCompliantLowCount,
    awsSsmComplianceSummary_nonCompliantMediumCount,
    awsSsmComplianceSummary_nonCompliantUnspecifiedCount,
    awsSsmComplianceSummary_overallSeverity,
    awsSsmComplianceSummary_patchBaselineId,
    awsSsmComplianceSummary_patchGroup,
    awsSsmComplianceSummary_status,

    -- ** AwsSsmPatch
    awsSsmPatch_complianceSummary,

    -- ** AwsSsmPatchComplianceDetails
    awsSsmPatchComplianceDetails_patch,

    -- ** AwsStepFunctionStateMachineDetails
    awsStepFunctionStateMachineDetails_label,
    awsStepFunctionStateMachineDetails_loggingConfiguration,
    awsStepFunctionStateMachineDetails_name,
    awsStepFunctionStateMachineDetails_roleArn,
    awsStepFunctionStateMachineDetails_stateMachineArn,
    awsStepFunctionStateMachineDetails_status,
    awsStepFunctionStateMachineDetails_tracingConfiguration,
    awsStepFunctionStateMachineDetails_type,

    -- ** AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
    awsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails_logGroupArn,

    -- ** AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
    awsStepFunctionStateMachineLoggingConfigurationDestinationsDetails_cloudWatchLogsLogGroup,

    -- ** AwsStepFunctionStateMachineLoggingConfigurationDetails
    awsStepFunctionStateMachineLoggingConfigurationDetails_destinations,
    awsStepFunctionStateMachineLoggingConfigurationDetails_includeExecutionData,
    awsStepFunctionStateMachineLoggingConfigurationDetails_level,

    -- ** AwsStepFunctionStateMachineTracingConfigurationDetails
    awsStepFunctionStateMachineTracingConfigurationDetails_enabled,

    -- ** AwsWafRateBasedRuleDetails
    awsWafRateBasedRuleDetails_matchPredicates,
    awsWafRateBasedRuleDetails_metricName,
    awsWafRateBasedRuleDetails_name,
    awsWafRateBasedRuleDetails_rateKey,
    awsWafRateBasedRuleDetails_rateLimit,
    awsWafRateBasedRuleDetails_ruleId,

    -- ** AwsWafRateBasedRuleMatchPredicate
    awsWafRateBasedRuleMatchPredicate_dataId,
    awsWafRateBasedRuleMatchPredicate_negated,
    awsWafRateBasedRuleMatchPredicate_type,

    -- ** AwsWafRegionalRateBasedRuleDetails
    awsWafRegionalRateBasedRuleDetails_matchPredicates,
    awsWafRegionalRateBasedRuleDetails_metricName,
    awsWafRegionalRateBasedRuleDetails_name,
    awsWafRegionalRateBasedRuleDetails_rateKey,
    awsWafRegionalRateBasedRuleDetails_rateLimit,
    awsWafRegionalRateBasedRuleDetails_ruleId,

    -- ** AwsWafRegionalRateBasedRuleMatchPredicate
    awsWafRegionalRateBasedRuleMatchPredicate_dataId,
    awsWafRegionalRateBasedRuleMatchPredicate_negated,
    awsWafRegionalRateBasedRuleMatchPredicate_type,

    -- ** AwsWafRegionalRuleDetails
    awsWafRegionalRuleDetails_metricName,
    awsWafRegionalRuleDetails_name,
    awsWafRegionalRuleDetails_predicateList,
    awsWafRegionalRuleDetails_ruleId,

    -- ** AwsWafRegionalRuleGroupDetails
    awsWafRegionalRuleGroupDetails_metricName,
    awsWafRegionalRuleGroupDetails_name,
    awsWafRegionalRuleGroupDetails_ruleGroupId,
    awsWafRegionalRuleGroupDetails_rules,

    -- ** AwsWafRegionalRuleGroupRulesActionDetails
    awsWafRegionalRuleGroupRulesActionDetails_type,

    -- ** AwsWafRegionalRuleGroupRulesDetails
    awsWafRegionalRuleGroupRulesDetails_action,
    awsWafRegionalRuleGroupRulesDetails_priority,
    awsWafRegionalRuleGroupRulesDetails_ruleId,
    awsWafRegionalRuleGroupRulesDetails_type,

    -- ** AwsWafRegionalRulePredicateListDetails
    awsWafRegionalRulePredicateListDetails_dataId,
    awsWafRegionalRulePredicateListDetails_negated,
    awsWafRegionalRulePredicateListDetails_type,

    -- ** AwsWafRegionalWebAclDetails
    awsWafRegionalWebAclDetails_defaultAction,
    awsWafRegionalWebAclDetails_metricName,
    awsWafRegionalWebAclDetails_name,
    awsWafRegionalWebAclDetails_rulesList,
    awsWafRegionalWebAclDetails_webAclId,

    -- ** AwsWafRegionalWebAclRulesListActionDetails
    awsWafRegionalWebAclRulesListActionDetails_type,

    -- ** AwsWafRegionalWebAclRulesListDetails
    awsWafRegionalWebAclRulesListDetails_action,
    awsWafRegionalWebAclRulesListDetails_overrideAction,
    awsWafRegionalWebAclRulesListDetails_priority,
    awsWafRegionalWebAclRulesListDetails_ruleId,
    awsWafRegionalWebAclRulesListDetails_type,

    -- ** AwsWafRegionalWebAclRulesListOverrideActionDetails
    awsWafRegionalWebAclRulesListOverrideActionDetails_type,

    -- ** AwsWafRuleDetails
    awsWafRuleDetails_metricName,
    awsWafRuleDetails_name,
    awsWafRuleDetails_predicateList,
    awsWafRuleDetails_ruleId,

    -- ** AwsWafRuleGroupDetails
    awsWafRuleGroupDetails_metricName,
    awsWafRuleGroupDetails_name,
    awsWafRuleGroupDetails_ruleGroupId,
    awsWafRuleGroupDetails_rules,

    -- ** AwsWafRuleGroupRulesActionDetails
    awsWafRuleGroupRulesActionDetails_type,

    -- ** AwsWafRuleGroupRulesDetails
    awsWafRuleGroupRulesDetails_action,
    awsWafRuleGroupRulesDetails_priority,
    awsWafRuleGroupRulesDetails_ruleId,
    awsWafRuleGroupRulesDetails_type,

    -- ** AwsWafRulePredicateListDetails
    awsWafRulePredicateListDetails_dataId,
    awsWafRulePredicateListDetails_negated,
    awsWafRulePredicateListDetails_type,

    -- ** AwsWafWebAclDetails
    awsWafWebAclDetails_defaultAction,
    awsWafWebAclDetails_name,
    awsWafWebAclDetails_rules,
    awsWafWebAclDetails_webAclId,

    -- ** AwsWafWebAclRule
    awsWafWebAclRule_action,
    awsWafWebAclRule_excludedRules,
    awsWafWebAclRule_overrideAction,
    awsWafWebAclRule_priority,
    awsWafWebAclRule_ruleId,
    awsWafWebAclRule_type,

    -- ** AwsWafv2ActionAllowDetails
    awsWafv2ActionAllowDetails_customRequestHandling,

    -- ** AwsWafv2ActionBlockDetails
    awsWafv2ActionBlockDetails_customResponse,

    -- ** AwsWafv2CustomHttpHeader
    awsWafv2CustomHttpHeader_name,
    awsWafv2CustomHttpHeader_value,

    -- ** AwsWafv2CustomRequestHandlingDetails
    awsWafv2CustomRequestHandlingDetails_insertHeaders,

    -- ** AwsWafv2CustomResponseDetails
    awsWafv2CustomResponseDetails_customResponseBodyKey,
    awsWafv2CustomResponseDetails_responseCode,
    awsWafv2CustomResponseDetails_responseHeaders,

    -- ** AwsWafv2RuleGroupDetails
    awsWafv2RuleGroupDetails_arn,
    awsWafv2RuleGroupDetails_capacity,
    awsWafv2RuleGroupDetails_description,
    awsWafv2RuleGroupDetails_id,
    awsWafv2RuleGroupDetails_name,
    awsWafv2RuleGroupDetails_rules,
    awsWafv2RuleGroupDetails_scope,
    awsWafv2RuleGroupDetails_visibilityConfig,

    -- ** AwsWafv2RulesActionCaptchaDetails
    awsWafv2RulesActionCaptchaDetails_customRequestHandling,

    -- ** AwsWafv2RulesActionCountDetails
    awsWafv2RulesActionCountDetails_customRequestHandling,

    -- ** AwsWafv2RulesActionDetails
    awsWafv2RulesActionDetails_allow,
    awsWafv2RulesActionDetails_block,
    awsWafv2RulesActionDetails_captcha,
    awsWafv2RulesActionDetails_count,

    -- ** AwsWafv2RulesDetails
    awsWafv2RulesDetails_action,
    awsWafv2RulesDetails_name,
    awsWafv2RulesDetails_overrideAction,
    awsWafv2RulesDetails_priority,
    awsWafv2RulesDetails_visibilityConfig,

    -- ** AwsWafv2VisibilityConfigDetails
    awsWafv2VisibilityConfigDetails_cloudWatchMetricsEnabled,
    awsWafv2VisibilityConfigDetails_metricName,
    awsWafv2VisibilityConfigDetails_sampledRequestsEnabled,

    -- ** AwsWafv2WebAclActionDetails
    awsWafv2WebAclActionDetails_allow,
    awsWafv2WebAclActionDetails_block,

    -- ** AwsWafv2WebAclCaptchaConfigDetails
    awsWafv2WebAclCaptchaConfigDetails_immunityTimeProperty,

    -- ** AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
    awsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails_immunityTime,

    -- ** AwsWafv2WebAclDetails
    awsWafv2WebAclDetails_arn,
    awsWafv2WebAclDetails_capacity,
    awsWafv2WebAclDetails_captchaConfig,
    awsWafv2WebAclDetails_defaultAction,
    awsWafv2WebAclDetails_description,
    awsWafv2WebAclDetails_id,
    awsWafv2WebAclDetails_managedbyFirewallManager,
    awsWafv2WebAclDetails_name,
    awsWafv2WebAclDetails_rules,
    awsWafv2WebAclDetails_visibilityConfig,

    -- ** AwsXrayEncryptionConfigDetails
    awsXrayEncryptionConfigDetails_keyId,
    awsXrayEncryptionConfigDetails_status,
    awsXrayEncryptionConfigDetails_type,

    -- ** BatchUpdateFindingsUnprocessedFinding
    batchUpdateFindingsUnprocessedFinding_findingIdentifier,
    batchUpdateFindingsUnprocessedFinding_errorCode,
    batchUpdateFindingsUnprocessedFinding_errorMessage,

    -- ** BooleanFilter
    booleanFilter_value,

    -- ** Cell
    cell_cellReference,
    cell_column,
    cell_columnName,
    cell_row,

    -- ** CidrBlockAssociation
    cidrBlockAssociation_associationId,
    cidrBlockAssociation_cidrBlock,
    cidrBlockAssociation_cidrBlockState,

    -- ** City
    city_cityName,

    -- ** ClassificationResult
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sensitiveData,
    classificationResult_sizeClassified,
    classificationResult_status,

    -- ** ClassificationStatus
    classificationStatus_code,
    classificationStatus_reason,

    -- ** Compliance
    compliance_associatedStandards,
    compliance_relatedRequirements,
    compliance_securityControlId,
    compliance_status,
    compliance_statusReasons,

    -- ** ContainerDetails
    containerDetails_containerRuntime,
    containerDetails_imageId,
    containerDetails_imageName,
    containerDetails_launchedAt,
    containerDetails_name,
    containerDetails_privileged,
    containerDetails_volumeMounts,

    -- ** Country
    country_countryCode,
    country_countryName,

    -- ** CustomDataIdentifiersDetections
    customDataIdentifiersDetections_arn,
    customDataIdentifiersDetections_count,
    customDataIdentifiersDetections_name,
    customDataIdentifiersDetections_occurrences,

    -- ** CustomDataIdentifiersResult
    customDataIdentifiersResult_detections,
    customDataIdentifiersResult_totalCount,

    -- ** Cvss
    cvss_adjustments,
    cvss_baseScore,
    cvss_baseVector,
    cvss_source,
    cvss_version,

    -- ** DataClassificationDetails
    dataClassificationDetails_detailedResultsLocation,
    dataClassificationDetails_result,

    -- ** DateFilter
    dateFilter_dateRange,
    dateFilter_end,
    dateFilter_start,

    -- ** DateRange
    dateRange_unit,
    dateRange_value,

    -- ** DnsRequestAction
    dnsRequestAction_blocked,
    dnsRequestAction_domain,
    dnsRequestAction_protocol,

    -- ** FilePaths
    filePaths_fileName,
    filePaths_filePath,
    filePaths_hash,
    filePaths_resourceId,

    -- ** FindingAggregator
    findingAggregator_findingAggregatorArn,

    -- ** FindingHistoryRecord
    findingHistoryRecord_findingCreated,
    findingHistoryRecord_findingIdentifier,
    findingHistoryRecord_nextToken,
    findingHistoryRecord_updateSource,
    findingHistoryRecord_updateTime,
    findingHistoryRecord_updates,

    -- ** FindingHistoryUpdate
    findingHistoryUpdate_newValue,
    findingHistoryUpdate_oldValue,
    findingHistoryUpdate_updatedField,

    -- ** FindingHistoryUpdateSource
    findingHistoryUpdateSource_identity,
    findingHistoryUpdateSource_type,

    -- ** FindingProviderFields
    findingProviderFields_confidence,
    findingProviderFields_criticality,
    findingProviderFields_relatedFindings,
    findingProviderFields_severity,
    findingProviderFields_types,

    -- ** FindingProviderSeverity
    findingProviderSeverity_label,
    findingProviderSeverity_original,

    -- ** FirewallPolicyDetails
    firewallPolicyDetails_statefulRuleGroupReferences,
    firewallPolicyDetails_statelessCustomActions,
    firewallPolicyDetails_statelessDefaultActions,
    firewallPolicyDetails_statelessFragmentDefaultActions,
    firewallPolicyDetails_statelessRuleGroupReferences,

    -- ** FirewallPolicyStatefulRuleGroupReferencesDetails
    firewallPolicyStatefulRuleGroupReferencesDetails_resourceArn,

    -- ** FirewallPolicyStatelessCustomActionsDetails
    firewallPolicyStatelessCustomActionsDetails_actionDefinition,
    firewallPolicyStatelessCustomActionsDetails_actionName,

    -- ** FirewallPolicyStatelessRuleGroupReferencesDetails
    firewallPolicyStatelessRuleGroupReferencesDetails_priority,
    firewallPolicyStatelessRuleGroupReferencesDetails_resourceArn,

    -- ** GeoLocation
    geoLocation_lat,
    geoLocation_lon,

    -- ** IcmpTypeCode
    icmpTypeCode_code,
    icmpTypeCode_type,

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
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_memberStatus,

    -- ** IpFilter
    ipFilter_cidr,

    -- ** IpOrganizationDetails
    ipOrganizationDetails_asn,
    ipOrganizationDetails_asnOrg,
    ipOrganizationDetails_isp,
    ipOrganizationDetails_org,

    -- ** Ipv6CidrBlockAssociation
    ipv6CidrBlockAssociation_associationId,
    ipv6CidrBlockAssociation_cidrBlockState,
    ipv6CidrBlockAssociation_ipv6CidrBlock,

    -- ** KeywordFilter
    keywordFilter_value,

    -- ** LoadBalancerState
    loadBalancerState_code,
    loadBalancerState_reason,

    -- ** Malware
    malware_path,
    malware_state,
    malware_type,
    malware_name,

    -- ** MapFilter
    mapFilter_comparison,
    mapFilter_key,
    mapFilter_value,

    -- ** Member
    member_accountId,
    member_administratorId,
    member_email,
    member_invitedAt,
    member_masterId,
    member_memberStatus,
    member_updatedAt,

    -- ** Network
    network_destinationDomain,
    network_destinationIpV4,
    network_destinationIpV6,
    network_destinationPort,
    network_direction,
    network_openPortRange,
    network_protocol,
    network_sourceDomain,
    network_sourceIpV4,
    network_sourceIpV6,
    network_sourceMac,
    network_sourcePort,

    -- ** NetworkConnectionAction
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_remotePortDetails,

    -- ** NetworkHeader
    networkHeader_destination,
    networkHeader_protocol,
    networkHeader_source,

    -- ** NetworkPathComponent
    networkPathComponent_componentId,
    networkPathComponent_componentType,
    networkPathComponent_egress,
    networkPathComponent_ingress,

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
    numberFilter_eq,
    numberFilter_gte,
    numberFilter_lte,

    -- ** Occurrences
    occurrences_cells,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_pages,
    occurrences_records,

    -- ** Page
    page_lineRange,
    page_offsetRange,
    page_pageNumber,

    -- ** PatchSummary
    patchSummary_failedCount,
    patchSummary_installedCount,
    patchSummary_installedOtherCount,
    patchSummary_installedPendingReboot,
    patchSummary_installedRejectedCount,
    patchSummary_missingCount,
    patchSummary_operation,
    patchSummary_operationEndTime,
    patchSummary_operationStartTime,
    patchSummary_rebootOption,
    patchSummary_id,

    -- ** PortProbeAction
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- ** PortProbeDetail
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_remoteIpDetails,

    -- ** PortRange
    portRange_begin,
    portRange_end,

    -- ** PortRangeFromTo
    portRangeFromTo_from,
    portRangeFromTo_to,

    -- ** ProcessDetails
    processDetails_launchedAt,
    processDetails_name,
    processDetails_parentPid,
    processDetails_path,
    processDetails_pid,
    processDetails_terminatedAt,

    -- ** Product
    product_activationUrl,
    product_categories,
    product_companyName,
    product_description,
    product_integrationTypes,
    product_marketplaceUrl,
    product_productName,
    product_productSubscriptionResourcePolicy,
    product_productArn,

    -- ** PropagatingVgwSetDetails
    propagatingVgwSetDetails_gatewayId,

    -- ** Range
    range_end,
    range_start,
    range_startColumn,

    -- ** Recommendation
    recommendation_text,
    recommendation_url,

    -- ** Record
    record_jsonPath,
    record_recordIndex,

    -- ** RelatedFinding
    relatedFinding_productArn,
    relatedFinding_id,

    -- ** Remediation
    remediation_recommendation,

    -- ** Resource
    resource_dataClassification,
    resource_details,
    resource_partition,
    resource_region,
    resource_resourceRole,
    resource_tags,
    resource_type,
    resource_id,

    -- ** ResourceDetails
    resourceDetails_awsAmazonMqBroker,
    resourceDetails_awsApiGatewayRestApi,
    resourceDetails_awsApiGatewayStage,
    resourceDetails_awsApiGatewayV2Api,
    resourceDetails_awsApiGatewayV2Stage,
    resourceDetails_awsAppSyncGraphQlApi,
    resourceDetails_awsAutoScalingAutoScalingGroup,
    resourceDetails_awsAutoScalingLaunchConfiguration,
    resourceDetails_awsBackupBackupPlan,
    resourceDetails_awsBackupBackupVault,
    resourceDetails_awsBackupRecoveryPoint,
    resourceDetails_awsCertificateManagerCertificate,
    resourceDetails_awsCloudFormationStack,
    resourceDetails_awsCloudFrontDistribution,
    resourceDetails_awsCloudTrailTrail,
    resourceDetails_awsCloudWatchAlarm,
    resourceDetails_awsCodeBuildProject,
    resourceDetails_awsDynamoDbTable,
    resourceDetails_awsEc2Eip,
    resourceDetails_awsEc2Instance,
    resourceDetails_awsEc2LaunchTemplate,
    resourceDetails_awsEc2NetworkAcl,
    resourceDetails_awsEc2NetworkInterface,
    resourceDetails_awsEc2RouteTable,
    resourceDetails_awsEc2SecurityGroup,
    resourceDetails_awsEc2Subnet,
    resourceDetails_awsEc2TransitGateway,
    resourceDetails_awsEc2Volume,
    resourceDetails_awsEc2Vpc,
    resourceDetails_awsEc2VpcEndpointService,
    resourceDetails_awsEc2VpcPeeringConnection,
    resourceDetails_awsEc2VpnConnection,
    resourceDetails_awsEcrContainerImage,
    resourceDetails_awsEcrRepository,
    resourceDetails_awsEcsCluster,
    resourceDetails_awsEcsContainer,
    resourceDetails_awsEcsService,
    resourceDetails_awsEcsTask,
    resourceDetails_awsEcsTaskDefinition,
    resourceDetails_awsEfsAccessPoint,
    resourceDetails_awsEksCluster,
    resourceDetails_awsElasticBeanstalkEnvironment,
    resourceDetails_awsElasticsearchDomain,
    resourceDetails_awsElbLoadBalancer,
    resourceDetails_awsElbv2LoadBalancer,
    resourceDetails_awsEventSchemasRegistry,
    resourceDetails_awsGuardDutyDetector,
    resourceDetails_awsIamAccessKey,
    resourceDetails_awsIamGroup,
    resourceDetails_awsIamPolicy,
    resourceDetails_awsIamRole,
    resourceDetails_awsIamUser,
    resourceDetails_awsKinesisStream,
    resourceDetails_awsKmsKey,
    resourceDetails_awsLambdaFunction,
    resourceDetails_awsLambdaLayerVersion,
    resourceDetails_awsNetworkFirewallFirewall,
    resourceDetails_awsNetworkFirewallFirewallPolicy,
    resourceDetails_awsNetworkFirewallRuleGroup,
    resourceDetails_awsOpenSearchServiceDomain,
    resourceDetails_awsRdsDbCluster,
    resourceDetails_awsRdsDbClusterSnapshot,
    resourceDetails_awsRdsDbInstance,
    resourceDetails_awsRdsDbSecurityGroup,
    resourceDetails_awsRdsDbSnapshot,
    resourceDetails_awsRdsEventSubscription,
    resourceDetails_awsRedshiftCluster,
    resourceDetails_awsS3AccountPublicAccessBlock,
    resourceDetails_awsS3Bucket,
    resourceDetails_awsS3Object,
    resourceDetails_awsSageMakerNotebookInstance,
    resourceDetails_awsSecretsManagerSecret,
    resourceDetails_awsSnsTopic,
    resourceDetails_awsSqsQueue,
    resourceDetails_awsSsmPatchCompliance,
    resourceDetails_awsStepFunctionStateMachine,
    resourceDetails_awsWafRateBasedRule,
    resourceDetails_awsWafRegionalRateBasedRule,
    resourceDetails_awsWafRegionalRule,
    resourceDetails_awsWafRegionalRuleGroup,
    resourceDetails_awsWafRegionalWebAcl,
    resourceDetails_awsWafRule,
    resourceDetails_awsWafRuleGroup,
    resourceDetails_awsWafWebAcl,
    resourceDetails_awsWafv2RuleGroup,
    resourceDetails_awsWafv2WebAcl,
    resourceDetails_awsXrayEncryptionConfig,
    resourceDetails_container,
    resourceDetails_other,

    -- ** Result
    result_accountId,
    result_processingResult,

    -- ** RouteSetDetails
    routeSetDetails_carrierGatewayId,
    routeSetDetails_coreNetworkArn,
    routeSetDetails_destinationCidrBlock,
    routeSetDetails_destinationIpv6CidrBlock,
    routeSetDetails_destinationPrefixListId,
    routeSetDetails_egressOnlyInternetGatewayId,
    routeSetDetails_gatewayId,
    routeSetDetails_instanceId,
    routeSetDetails_instanceOwnerId,
    routeSetDetails_localGatewayId,
    routeSetDetails_natGatewayId,
    routeSetDetails_networkInterfaceId,
    routeSetDetails_origin,
    routeSetDetails_state,
    routeSetDetails_transitGatewayId,
    routeSetDetails_vpcPeeringConnectionId,

    -- ** RuleGroupDetails
    ruleGroupDetails_ruleVariables,
    ruleGroupDetails_rulesSource,

    -- ** RuleGroupSource
    ruleGroupSource_rulesSourceList,
    ruleGroupSource_rulesString,
    ruleGroupSource_statefulRules,
    ruleGroupSource_statelessRulesAndCustomActions,

    -- ** RuleGroupSourceCustomActionsDetails
    ruleGroupSourceCustomActionsDetails_actionDefinition,
    ruleGroupSourceCustomActionsDetails_actionName,

    -- ** RuleGroupSourceListDetails
    ruleGroupSourceListDetails_generatedRulesType,
    ruleGroupSourceListDetails_targetTypes,
    ruleGroupSourceListDetails_targets,

    -- ** RuleGroupSourceStatefulRulesDetails
    ruleGroupSourceStatefulRulesDetails_action,
    ruleGroupSourceStatefulRulesDetails_header,
    ruleGroupSourceStatefulRulesDetails_ruleOptions,

    -- ** RuleGroupSourceStatefulRulesHeaderDetails
    ruleGroupSourceStatefulRulesHeaderDetails_destination,
    ruleGroupSourceStatefulRulesHeaderDetails_destinationPort,
    ruleGroupSourceStatefulRulesHeaderDetails_direction,
    ruleGroupSourceStatefulRulesHeaderDetails_protocol,
    ruleGroupSourceStatefulRulesHeaderDetails_source,
    ruleGroupSourceStatefulRulesHeaderDetails_sourcePort,

    -- ** RuleGroupSourceStatefulRulesOptionsDetails
    ruleGroupSourceStatefulRulesOptionsDetails_keyword,
    ruleGroupSourceStatefulRulesOptionsDetails_settings,

    -- ** RuleGroupSourceStatelessRuleDefinition
    ruleGroupSourceStatelessRuleDefinition_actions,
    ruleGroupSourceStatelessRuleDefinition_matchAttributes,

    -- ** RuleGroupSourceStatelessRuleMatchAttributes
    ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts,
    ruleGroupSourceStatelessRuleMatchAttributes_destinations,
    ruleGroupSourceStatelessRuleMatchAttributes_protocols,
    ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts,
    ruleGroupSourceStatelessRuleMatchAttributes_sources,
    ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort,
    ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesDestinations
    ruleGroupSourceStatelessRuleMatchAttributesDestinations_addressDefinition,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort,
    ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesSources
    ruleGroupSourceStatelessRuleMatchAttributesSources_addressDefinition,

    -- ** RuleGroupSourceStatelessRuleMatchAttributesTcpFlags
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_flags,
    ruleGroupSourceStatelessRuleMatchAttributesTcpFlags_masks,

    -- ** RuleGroupSourceStatelessRulesAndCustomActionsDetails
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_customActions,
    ruleGroupSourceStatelessRulesAndCustomActionsDetails_statelessRules,

    -- ** RuleGroupSourceStatelessRulesDetails
    ruleGroupSourceStatelessRulesDetails_priority,
    ruleGroupSourceStatelessRulesDetails_ruleDefinition,

    -- ** RuleGroupVariables
    ruleGroupVariables_ipSets,
    ruleGroupVariables_portSets,

    -- ** RuleGroupVariablesIpSetsDetails
    ruleGroupVariablesIpSetsDetails_definition,

    -- ** RuleGroupVariablesPortSetsDetails
    ruleGroupVariablesPortSetsDetails_definition,

    -- ** SecurityControl
    securityControl_securityControlId,
    securityControl_securityControlArn,
    securityControl_title,
    securityControl_description,
    securityControl_remediationUrl,
    securityControl_severityRating,
    securityControl_securityControlStatus,

    -- ** SecurityControlDefinition
    securityControlDefinition_securityControlId,
    securityControlDefinition_title,
    securityControlDefinition_description,
    securityControlDefinition_remediationUrl,
    securityControlDefinition_severityRating,
    securityControlDefinition_currentRegionAvailability,

    -- ** SensitiveDataDetections
    sensitiveDataDetections_count,
    sensitiveDataDetections_occurrences,
    sensitiveDataDetections_type,

    -- ** SensitiveDataResult
    sensitiveDataResult_category,
    sensitiveDataResult_detections,
    sensitiveDataResult_totalCount,

    -- ** Severity
    severity_label,
    severity_normalized,
    severity_original,
    severity_product,

    -- ** SeverityUpdate
    severityUpdate_label,
    severityUpdate_normalized,
    severityUpdate_product,

    -- ** SoftwarePackage
    softwarePackage_architecture,
    softwarePackage_epoch,
    softwarePackage_filePath,
    softwarePackage_fixedInVersion,
    softwarePackage_name,
    softwarePackage_packageManager,
    softwarePackage_release,
    softwarePackage_remediation,
    softwarePackage_sourceLayerArn,
    softwarePackage_sourceLayerHash,
    softwarePackage_version,

    -- ** SortCriterion
    sortCriterion_field,
    sortCriterion_sortOrder,

    -- ** Standard
    standard_description,
    standard_enabledByDefault,
    standard_name,
    standard_standardsArn,
    standard_standardsManagedBy,

    -- ** StandardsControl
    standardsControl_controlId,
    standardsControl_controlStatus,
    standardsControl_controlStatusUpdatedAt,
    standardsControl_description,
    standardsControl_disabledReason,
    standardsControl_relatedRequirements,
    standardsControl_remediationUrl,
    standardsControl_severityRating,
    standardsControl_standardsControlArn,
    standardsControl_title,

    -- ** StandardsControlAssociationDetail
    standardsControlAssociationDetail_relatedRequirements,
    standardsControlAssociationDetail_standardsControlArns,
    standardsControlAssociationDetail_standardsControlDescription,
    standardsControlAssociationDetail_standardsControlTitle,
    standardsControlAssociationDetail_updatedAt,
    standardsControlAssociationDetail_updatedReason,
    standardsControlAssociationDetail_standardsArn,
    standardsControlAssociationDetail_securityControlId,
    standardsControlAssociationDetail_securityControlArn,
    standardsControlAssociationDetail_associationStatus,

    -- ** StandardsControlAssociationId
    standardsControlAssociationId_securityControlId,
    standardsControlAssociationId_standardsArn,

    -- ** StandardsControlAssociationSummary
    standardsControlAssociationSummary_relatedRequirements,
    standardsControlAssociationSummary_standardsControlDescription,
    standardsControlAssociationSummary_standardsControlTitle,
    standardsControlAssociationSummary_updatedAt,
    standardsControlAssociationSummary_updatedReason,
    standardsControlAssociationSummary_standardsArn,
    standardsControlAssociationSummary_securityControlId,
    standardsControlAssociationSummary_securityControlArn,
    standardsControlAssociationSummary_associationStatus,

    -- ** StandardsControlAssociationUpdate
    standardsControlAssociationUpdate_updatedReason,
    standardsControlAssociationUpdate_standardsArn,
    standardsControlAssociationUpdate_securityControlId,
    standardsControlAssociationUpdate_associationStatus,

    -- ** StandardsManagedBy
    standardsManagedBy_company,
    standardsManagedBy_product,

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
    threat_filePaths,
    threat_itemCount,
    threat_name,
    threat_severity,

    -- ** ThreatIntelIndicator
    threatIntelIndicator_category,
    threatIntelIndicator_lastObservedAt,
    threatIntelIndicator_source,
    threatIntelIndicator_sourceUrl,
    threatIntelIndicator_type,
    threatIntelIndicator_value,

    -- ** UnprocessedAutomationRule
    unprocessedAutomationRule_errorCode,
    unprocessedAutomationRule_errorMessage,
    unprocessedAutomationRule_ruleArn,

    -- ** UnprocessedSecurityControl
    unprocessedSecurityControl_errorReason,
    unprocessedSecurityControl_securityControlId,
    unprocessedSecurityControl_errorCode,

    -- ** UnprocessedStandardsControlAssociation
    unprocessedStandardsControlAssociation_errorReason,
    unprocessedStandardsControlAssociation_standardsControlAssociationId,
    unprocessedStandardsControlAssociation_errorCode,

    -- ** UnprocessedStandardsControlAssociationUpdate
    unprocessedStandardsControlAssociationUpdate_errorReason,
    unprocessedStandardsControlAssociationUpdate_standardsControlAssociationUpdate,
    unprocessedStandardsControlAssociationUpdate_errorCode,

    -- ** UpdateAutomationRulesRequestItem
    updateAutomationRulesRequestItem_actions,
    updateAutomationRulesRequestItem_criteria,
    updateAutomationRulesRequestItem_description,
    updateAutomationRulesRequestItem_isTerminal,
    updateAutomationRulesRequestItem_ruleName,
    updateAutomationRulesRequestItem_ruleOrder,
    updateAutomationRulesRequestItem_ruleStatus,
    updateAutomationRulesRequestItem_ruleArn,

    -- ** VolumeMount
    volumeMount_mountPath,
    volumeMount_name,

    -- ** VpcInfoCidrBlockSetDetails
    vpcInfoCidrBlockSetDetails_cidrBlock,

    -- ** VpcInfoIpv6CidrBlockSetDetails
    vpcInfoIpv6CidrBlockSetDetails_ipv6CidrBlock,

    -- ** VpcInfoPeeringOptionsDetails
    vpcInfoPeeringOptionsDetails_allowDnsResolutionFromRemoteVpc,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalClassicLinkToRemoteVpc,
    vpcInfoPeeringOptionsDetails_allowEgressFromLocalVpcToRemoteClassicLink,

    -- ** Vulnerability
    vulnerability_cvss,
    vulnerability_fixAvailable,
    vulnerability_referenceUrls,
    vulnerability_relatedVulnerabilities,
    vulnerability_vendor,
    vulnerability_vulnerablePackages,
    vulnerability_id,

    -- ** VulnerabilityVendor
    vulnerabilityVendor_url,
    vulnerabilityVendor_vendorCreatedAt,
    vulnerabilityVendor_vendorSeverity,
    vulnerabilityVendor_vendorUpdatedAt,
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
import Amazonka.SecurityHub.BatchDeleteAutomationRules
import Amazonka.SecurityHub.BatchDisableStandards
import Amazonka.SecurityHub.BatchEnableStandards
import Amazonka.SecurityHub.BatchGetAutomationRules
import Amazonka.SecurityHub.BatchGetSecurityControls
import Amazonka.SecurityHub.BatchGetStandardsControlAssociations
import Amazonka.SecurityHub.BatchImportFindings
import Amazonka.SecurityHub.BatchUpdateAutomationRules
import Amazonka.SecurityHub.BatchUpdateFindings
import Amazonka.SecurityHub.BatchUpdateStandardsControlAssociations
import Amazonka.SecurityHub.CreateActionTarget
import Amazonka.SecurityHub.CreateAutomationRule
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
import Amazonka.SecurityHub.GetFindingHistory
import Amazonka.SecurityHub.GetFindings
import Amazonka.SecurityHub.GetInsightResults
import Amazonka.SecurityHub.GetInsights
import Amazonka.SecurityHub.GetInvitationsCount
import Amazonka.SecurityHub.GetMembers
import Amazonka.SecurityHub.InviteMembers
import Amazonka.SecurityHub.ListAutomationRules
import Amazonka.SecurityHub.ListEnabledProductsForImport
import Amazonka.SecurityHub.ListFindingAggregators
import Amazonka.SecurityHub.ListInvitations
import Amazonka.SecurityHub.ListMembers
import Amazonka.SecurityHub.ListOrganizationAdminAccounts
import Amazonka.SecurityHub.ListSecurityControlDefinitions
import Amazonka.SecurityHub.ListStandardsControlAssociations
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
import Amazonka.SecurityHub.Types.AssociatedStandard
import Amazonka.SecurityHub.Types.AssociationSetDetails
import Amazonka.SecurityHub.Types.AssociationStateDetails
import Amazonka.SecurityHub.Types.AutomationRulesAction
import Amazonka.SecurityHub.Types.AutomationRulesConfig
import Amazonka.SecurityHub.Types.AutomationRulesFindingFieldsUpdate
import Amazonka.SecurityHub.Types.AutomationRulesFindingFilters
import Amazonka.SecurityHub.Types.AutomationRulesMetadata
import Amazonka.SecurityHub.Types.AvailabilityZone
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerEncryptionOptionsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLdapServerMetadataDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLogsPendingDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerMaintenanceWindowStartTimeDetails
import Amazonka.SecurityHub.Types.AwsAmazonMqBrokerUsersDetails
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
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLogConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails
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
import Amazonka.SecurityHub.Types.AwsEc2InstanceMonitoringDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataBlockDeviceMappingSetEbsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCpuOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCreditSpecificationDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticGpuSpecificationSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataElasticInferenceAcceleratorSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataEnclaveOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataHibernationOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataIamInstanceProfileDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsBaselineEbsBandwidthMbpsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsNetworkInterfaceCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataLicenseSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMetadataOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMonitoringDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPlacementDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclAssociation
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclEntry
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup
import Amazonka.SecurityHub.Types.AwsEc2RouteTableDetails
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
import Amazonka.SecurityHub.Types.AwsEventSchemasRegistryDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesCloudTrailDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDnsLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesFlowLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionScanEc2InstanceWithFindingsEbsVolumesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesS3LogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorFeaturesDetails
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
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
import Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDetails
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRedirectTo
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
import Amazonka.SecurityHub.Types.AwsS3ObjectDetails
import Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceDetails
import Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails
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
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsCloudWatchLogsLogGroupDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDestinationsDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineTracingConfigurationDetails
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
import Amazonka.SecurityHub.Types.AwsWafv2ActionAllowDetails
import Amazonka.SecurityHub.Types.AwsWafv2ActionBlockDetails
import Amazonka.SecurityHub.Types.AwsWafv2CustomHttpHeader
import Amazonka.SecurityHub.Types.AwsWafv2CustomRequestHandlingDetails
import Amazonka.SecurityHub.Types.AwsWafv2CustomResponseDetails
import Amazonka.SecurityHub.Types.AwsWafv2RuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCaptchaDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionCountDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2RulesDetails
import Amazonka.SecurityHub.Types.AwsWafv2VisibilityConfigDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclActionDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclCaptchaConfigImmunityTimePropertyDetails
import Amazonka.SecurityHub.Types.AwsWafv2WebAclDetails
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
import Amazonka.SecurityHub.Types.FindingHistoryRecord
import Amazonka.SecurityHub.Types.FindingHistoryUpdate
import Amazonka.SecurityHub.Types.FindingHistoryUpdateSource
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
import Amazonka.SecurityHub.Types.PropagatingVgwSetDetails
import Amazonka.SecurityHub.Types.Range
import Amazonka.SecurityHub.Types.Recommendation
import Amazonka.SecurityHub.Types.Record
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.Remediation
import Amazonka.SecurityHub.Types.Resource
import Amazonka.SecurityHub.Types.ResourceDetails
import Amazonka.SecurityHub.Types.Result
import Amazonka.SecurityHub.Types.RouteSetDetails
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
import Amazonka.SecurityHub.Types.SecurityControl
import Amazonka.SecurityHub.Types.SecurityControlDefinition
import Amazonka.SecurityHub.Types.SensitiveDataDetections
import Amazonka.SecurityHub.Types.SensitiveDataResult
import Amazonka.SecurityHub.Types.Severity
import Amazonka.SecurityHub.Types.SeverityUpdate
import Amazonka.SecurityHub.Types.SoftwarePackage
import Amazonka.SecurityHub.Types.SortCriterion
import Amazonka.SecurityHub.Types.Standard
import Amazonka.SecurityHub.Types.StandardsControl
import Amazonka.SecurityHub.Types.StandardsControlAssociationDetail
import Amazonka.SecurityHub.Types.StandardsControlAssociationId
import Amazonka.SecurityHub.Types.StandardsControlAssociationSummary
import Amazonka.SecurityHub.Types.StandardsControlAssociationUpdate
import Amazonka.SecurityHub.Types.StandardsManagedBy
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
import Amazonka.SecurityHub.Types.UnprocessedAutomationRule
import Amazonka.SecurityHub.Types.UnprocessedSecurityControl
import Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociation
import Amazonka.SecurityHub.Types.UnprocessedStandardsControlAssociationUpdate
import Amazonka.SecurityHub.Types.UpdateAutomationRulesRequestItem
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

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Connect.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Lens
  ( -- * Operations

    -- ** AssociateApprovedOrigin
    associateApprovedOrigin_instanceId,
    associateApprovedOrigin_origin,

    -- ** AssociateBot
    associateBot_lexV2Bot,
    associateBot_lexBot,
    associateBot_instanceId,

    -- ** AssociateDefaultVocabulary
    associateDefaultVocabulary_vocabularyId,
    associateDefaultVocabulary_instanceId,
    associateDefaultVocabulary_languageCode,
    associateDefaultVocabularyResponse_httpStatus,

    -- ** AssociateInstanceStorageConfig
    associateInstanceStorageConfig_instanceId,
    associateInstanceStorageConfig_resourceType,
    associateInstanceStorageConfig_storageConfig,
    associateInstanceStorageConfigResponse_associationId,
    associateInstanceStorageConfigResponse_httpStatus,

    -- ** AssociateLambdaFunction
    associateLambdaFunction_instanceId,
    associateLambdaFunction_functionArn,

    -- ** AssociateLexBot
    associateLexBot_instanceId,
    associateLexBot_lexBot,

    -- ** AssociatePhoneNumberContactFlow
    associatePhoneNumberContactFlow_phoneNumberId,
    associatePhoneNumberContactFlow_instanceId,
    associatePhoneNumberContactFlow_contactFlowId,

    -- ** AssociateQueueQuickConnects
    associateQueueQuickConnects_instanceId,
    associateQueueQuickConnects_queueId,
    associateQueueQuickConnects_quickConnectIds,

    -- ** AssociateRoutingProfileQueues
    associateRoutingProfileQueues_instanceId,
    associateRoutingProfileQueues_routingProfileId,
    associateRoutingProfileQueues_queueConfigs,

    -- ** AssociateSecurityKey
    associateSecurityKey_instanceId,
    associateSecurityKey_key,
    associateSecurityKeyResponse_associationId,
    associateSecurityKeyResponse_httpStatus,

    -- ** ClaimPhoneNumber
    claimPhoneNumber_tags,
    claimPhoneNumber_clientToken,
    claimPhoneNumber_phoneNumberDescription,
    claimPhoneNumber_targetArn,
    claimPhoneNumber_phoneNumber,
    claimPhoneNumberResponse_phoneNumberArn,
    claimPhoneNumberResponse_phoneNumberId,
    claimPhoneNumberResponse_httpStatus,

    -- ** CreateAgentStatus
    createAgentStatus_tags,
    createAgentStatus_displayOrder,
    createAgentStatus_description,
    createAgentStatus_instanceId,
    createAgentStatus_name,
    createAgentStatus_state,
    createAgentStatusResponse_agentStatusId,
    createAgentStatusResponse_agentStatusARN,
    createAgentStatusResponse_httpStatus,

    -- ** CreateContactFlow
    createContactFlow_tags,
    createContactFlow_description,
    createContactFlow_instanceId,
    createContactFlow_name,
    createContactFlow_type,
    createContactFlow_content,
    createContactFlowResponse_contactFlowArn,
    createContactFlowResponse_contactFlowId,
    createContactFlowResponse_httpStatus,

    -- ** CreateContactFlowModule
    createContactFlowModule_tags,
    createContactFlowModule_clientToken,
    createContactFlowModule_description,
    createContactFlowModule_instanceId,
    createContactFlowModule_name,
    createContactFlowModule_content,
    createContactFlowModuleResponse_arn,
    createContactFlowModuleResponse_id,
    createContactFlowModuleResponse_httpStatus,

    -- ** CreateHoursOfOperation
    createHoursOfOperation_tags,
    createHoursOfOperation_description,
    createHoursOfOperation_instanceId,
    createHoursOfOperation_name,
    createHoursOfOperation_timeZone,
    createHoursOfOperation_config,
    createHoursOfOperationResponse_hoursOfOperationArn,
    createHoursOfOperationResponse_hoursOfOperationId,
    createHoursOfOperationResponse_httpStatus,

    -- ** CreateInstance
    createInstance_directoryId,
    createInstance_clientToken,
    createInstance_instanceAlias,
    createInstance_identityManagementType,
    createInstance_inboundCallsEnabled,
    createInstance_outboundCallsEnabled,
    createInstanceResponse_arn,
    createInstanceResponse_id,
    createInstanceResponse_httpStatus,

    -- ** CreateIntegrationAssociation
    createIntegrationAssociation_tags,
    createIntegrationAssociation_sourceType,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_httpStatus,

    -- ** CreateQueue
    createQueue_tags,
    createQueue_description,
    createQueue_quickConnectIds,
    createQueue_maxContacts,
    createQueue_outboundCallerConfig,
    createQueue_instanceId,
    createQueue_name,
    createQueue_hoursOfOperationId,
    createQueueResponse_queueArn,
    createQueueResponse_queueId,
    createQueueResponse_httpStatus,

    -- ** CreateQuickConnect
    createQuickConnect_tags,
    createQuickConnect_description,
    createQuickConnect_instanceId,
    createQuickConnect_name,
    createQuickConnect_quickConnectConfig,
    createQuickConnectResponse_quickConnectARN,
    createQuickConnectResponse_quickConnectId,
    createQuickConnectResponse_httpStatus,

    -- ** CreateRoutingProfile
    createRoutingProfile_tags,
    createRoutingProfile_queueConfigs,
    createRoutingProfile_instanceId,
    createRoutingProfile_name,
    createRoutingProfile_description,
    createRoutingProfile_defaultOutboundQueueId,
    createRoutingProfile_mediaConcurrencies,
    createRoutingProfileResponse_routingProfileArn,
    createRoutingProfileResponse_routingProfileId,
    createRoutingProfileResponse_httpStatus,

    -- ** CreateSecurityProfile
    createSecurityProfile_tags,
    createSecurityProfile_allowedAccessControlTags,
    createSecurityProfile_permissions,
    createSecurityProfile_description,
    createSecurityProfile_tagRestrictedResources,
    createSecurityProfile_securityProfileName,
    createSecurityProfile_instanceId,
    createSecurityProfileResponse_securityProfileId,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_httpStatus,

    -- ** CreateTaskTemplate
    createTaskTemplate_clientToken,
    createTaskTemplate_constraints,
    createTaskTemplate_status,
    createTaskTemplate_description,
    createTaskTemplate_defaults,
    createTaskTemplate_contactFlowId,
    createTaskTemplate_instanceId,
    createTaskTemplate_name,
    createTaskTemplate_fields,
    createTaskTemplateResponse_httpStatus,
    createTaskTemplateResponse_id,
    createTaskTemplateResponse_arn,

    -- ** CreateTrafficDistributionGroup
    createTrafficDistributionGroup_tags,
    createTrafficDistributionGroup_clientToken,
    createTrafficDistributionGroup_description,
    createTrafficDistributionGroup_name,
    createTrafficDistributionGroup_instanceId,
    createTrafficDistributionGroupResponse_arn,
    createTrafficDistributionGroupResponse_id,
    createTrafficDistributionGroupResponse_httpStatus,

    -- ** CreateUseCase
    createUseCase_tags,
    createUseCase_instanceId,
    createUseCase_integrationAssociationId,
    createUseCase_useCaseType,
    createUseCaseResponse_useCaseArn,
    createUseCaseResponse_useCaseId,
    createUseCaseResponse_httpStatus,

    -- ** CreateUser
    createUser_tags,
    createUser_hierarchyGroupId,
    createUser_identityInfo,
    createUser_password,
    createUser_directoryUserId,
    createUser_username,
    createUser_phoneConfig,
    createUser_securityProfileIds,
    createUser_routingProfileId,
    createUser_instanceId,
    createUserResponse_userArn,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** CreateUserHierarchyGroup
    createUserHierarchyGroup_tags,
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_name,
    createUserHierarchyGroup_instanceId,
    createUserHierarchyGroupResponse_hierarchyGroupId,
    createUserHierarchyGroupResponse_hierarchyGroupArn,
    createUserHierarchyGroupResponse_httpStatus,

    -- ** CreateVocabulary
    createVocabulary_tags,
    createVocabulary_clientToken,
    createVocabulary_instanceId,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabulary_content,
    createVocabularyResponse_httpStatus,
    createVocabularyResponse_vocabularyArn,
    createVocabularyResponse_vocabularyId,
    createVocabularyResponse_state,

    -- ** DeleteContactFlow
    deleteContactFlow_instanceId,
    deleteContactFlow_contactFlowId,

    -- ** DeleteContactFlowModule
    deleteContactFlowModule_instanceId,
    deleteContactFlowModule_contactFlowModuleId,
    deleteContactFlowModuleResponse_httpStatus,

    -- ** DeleteHoursOfOperation
    deleteHoursOfOperation_instanceId,
    deleteHoursOfOperation_hoursOfOperationId,

    -- ** DeleteInstance
    deleteInstance_instanceId,

    -- ** DeleteIntegrationAssociation
    deleteIntegrationAssociation_instanceId,
    deleteIntegrationAssociation_integrationAssociationId,

    -- ** DeleteQuickConnect
    deleteQuickConnect_instanceId,
    deleteQuickConnect_quickConnectId,

    -- ** DeleteSecurityProfile
    deleteSecurityProfile_instanceId,
    deleteSecurityProfile_securityProfileId,

    -- ** DeleteTaskTemplate
    deleteTaskTemplate_instanceId,
    deleteTaskTemplate_taskTemplateId,
    deleteTaskTemplateResponse_httpStatus,

    -- ** DeleteTrafficDistributionGroup
    deleteTrafficDistributionGroup_trafficDistributionGroupId,
    deleteTrafficDistributionGroupResponse_httpStatus,

    -- ** DeleteUseCase
    deleteUseCase_instanceId,
    deleteUseCase_integrationAssociationId,
    deleteUseCase_useCaseId,

    -- ** DeleteUser
    deleteUser_instanceId,
    deleteUser_userId,

    -- ** DeleteUserHierarchyGroup
    deleteUserHierarchyGroup_hierarchyGroupId,
    deleteUserHierarchyGroup_instanceId,

    -- ** DeleteVocabulary
    deleteVocabulary_instanceId,
    deleteVocabulary_vocabularyId,
    deleteVocabularyResponse_httpStatus,
    deleteVocabularyResponse_vocabularyArn,
    deleteVocabularyResponse_vocabularyId,
    deleteVocabularyResponse_state,

    -- ** DescribeAgentStatus
    describeAgentStatus_instanceId,
    describeAgentStatus_agentStatusId,
    describeAgentStatusResponse_agentStatus,
    describeAgentStatusResponse_httpStatus,

    -- ** DescribeContact
    describeContact_instanceId,
    describeContact_contactId,
    describeContactResponse_contact,
    describeContactResponse_httpStatus,

    -- ** DescribeContactFlow
    describeContactFlow_instanceId,
    describeContactFlow_contactFlowId,
    describeContactFlowResponse_contactFlow,
    describeContactFlowResponse_httpStatus,

    -- ** DescribeContactFlowModule
    describeContactFlowModule_instanceId,
    describeContactFlowModule_contactFlowModuleId,
    describeContactFlowModuleResponse_contactFlowModule,
    describeContactFlowModuleResponse_httpStatus,

    -- ** DescribeHoursOfOperation
    describeHoursOfOperation_instanceId,
    describeHoursOfOperation_hoursOfOperationId,
    describeHoursOfOperationResponse_hoursOfOperation,
    describeHoursOfOperationResponse_httpStatus,

    -- ** DescribeInstance
    describeInstance_instanceId,
    describeInstanceResponse_instance,
    describeInstanceResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_instanceId,
    describeInstanceAttribute_attributeType,
    describeInstanceAttributeResponse_attribute,
    describeInstanceAttributeResponse_httpStatus,

    -- ** DescribeInstanceStorageConfig
    describeInstanceStorageConfig_instanceId,
    describeInstanceStorageConfig_associationId,
    describeInstanceStorageConfig_resourceType,
    describeInstanceStorageConfigResponse_storageConfig,
    describeInstanceStorageConfigResponse_httpStatus,

    -- ** DescribePhoneNumber
    describePhoneNumber_phoneNumberId,
    describePhoneNumberResponse_claimedPhoneNumberSummary,
    describePhoneNumberResponse_httpStatus,

    -- ** DescribeQueue
    describeQueue_instanceId,
    describeQueue_queueId,
    describeQueueResponse_queue,
    describeQueueResponse_httpStatus,

    -- ** DescribeQuickConnect
    describeQuickConnect_instanceId,
    describeQuickConnect_quickConnectId,
    describeQuickConnectResponse_quickConnect,
    describeQuickConnectResponse_httpStatus,

    -- ** DescribeRoutingProfile
    describeRoutingProfile_instanceId,
    describeRoutingProfile_routingProfileId,
    describeRoutingProfileResponse_routingProfile,
    describeRoutingProfileResponse_httpStatus,

    -- ** DescribeSecurityProfile
    describeSecurityProfile_securityProfileId,
    describeSecurityProfile_instanceId,
    describeSecurityProfileResponse_securityProfile,
    describeSecurityProfileResponse_httpStatus,

    -- ** DescribeTrafficDistributionGroup
    describeTrafficDistributionGroup_trafficDistributionGroupId,
    describeTrafficDistributionGroupResponse_trafficDistributionGroup,
    describeTrafficDistributionGroupResponse_httpStatus,

    -- ** DescribeUser
    describeUser_userId,
    describeUser_instanceId,
    describeUserResponse_user,
    describeUserResponse_httpStatus,

    -- ** DescribeUserHierarchyGroup
    describeUserHierarchyGroup_hierarchyGroupId,
    describeUserHierarchyGroup_instanceId,
    describeUserHierarchyGroupResponse_hierarchyGroup,
    describeUserHierarchyGroupResponse_httpStatus,

    -- ** DescribeUserHierarchyStructure
    describeUserHierarchyStructure_instanceId,
    describeUserHierarchyStructureResponse_hierarchyStructure,
    describeUserHierarchyStructureResponse_httpStatus,

    -- ** DescribeVocabulary
    describeVocabulary_instanceId,
    describeVocabulary_vocabularyId,
    describeVocabularyResponse_httpStatus,
    describeVocabularyResponse_vocabulary,

    -- ** DisassociateApprovedOrigin
    disassociateApprovedOrigin_instanceId,
    disassociateApprovedOrigin_origin,

    -- ** DisassociateBot
    disassociateBot_lexV2Bot,
    disassociateBot_lexBot,
    disassociateBot_instanceId,

    -- ** DisassociateInstanceStorageConfig
    disassociateInstanceStorageConfig_instanceId,
    disassociateInstanceStorageConfig_associationId,
    disassociateInstanceStorageConfig_resourceType,

    -- ** DisassociateLambdaFunction
    disassociateLambdaFunction_instanceId,
    disassociateLambdaFunction_functionArn,

    -- ** DisassociateLexBot
    disassociateLexBot_instanceId,
    disassociateLexBot_botName,
    disassociateLexBot_lexRegion,

    -- ** DisassociatePhoneNumberContactFlow
    disassociatePhoneNumberContactFlow_phoneNumberId,
    disassociatePhoneNumberContactFlow_instanceId,

    -- ** DisassociateQueueQuickConnects
    disassociateQueueQuickConnects_instanceId,
    disassociateQueueQuickConnects_queueId,
    disassociateQueueQuickConnects_quickConnectIds,

    -- ** DisassociateRoutingProfileQueues
    disassociateRoutingProfileQueues_instanceId,
    disassociateRoutingProfileQueues_routingProfileId,
    disassociateRoutingProfileQueues_queueReferences,

    -- ** DisassociateSecurityKey
    disassociateSecurityKey_instanceId,
    disassociateSecurityKey_associationId,

    -- ** DismissUserContact
    dismissUserContact_userId,
    dismissUserContact_instanceId,
    dismissUserContact_contactId,
    dismissUserContactResponse_httpStatus,

    -- ** GetContactAttributes
    getContactAttributes_instanceId,
    getContactAttributes_initialContactId,
    getContactAttributesResponse_attributes,
    getContactAttributesResponse_httpStatus,

    -- ** GetCurrentMetricData
    getCurrentMetricData_groupings,
    getCurrentMetricData_nextToken,
    getCurrentMetricData_maxResults,
    getCurrentMetricData_instanceId,
    getCurrentMetricData_filters,
    getCurrentMetricData_currentMetrics,
    getCurrentMetricDataResponse_nextToken,
    getCurrentMetricDataResponse_dataSnapshotTime,
    getCurrentMetricDataResponse_metricResults,
    getCurrentMetricDataResponse_httpStatus,

    -- ** GetCurrentUserData
    getCurrentUserData_nextToken,
    getCurrentUserData_maxResults,
    getCurrentUserData_instanceId,
    getCurrentUserData_filters,
    getCurrentUserDataResponse_nextToken,
    getCurrentUserDataResponse_userDataList,
    getCurrentUserDataResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_instanceId,
    getFederationTokenResponse_signInUrl,
    getFederationTokenResponse_userArn,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_userId,
    getFederationTokenResponse_httpStatus,

    -- ** GetMetricData
    getMetricData_groupings,
    getMetricData_nextToken,
    getMetricData_maxResults,
    getMetricData_instanceId,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricData_filters,
    getMetricData_historicalMetrics,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_metricResults,
    getMetricDataResponse_httpStatus,

    -- ** GetTaskTemplate
    getTaskTemplate_snapshotVersion,
    getTaskTemplate_instanceId,
    getTaskTemplate_taskTemplateId,
    getTaskTemplateResponse_tags,
    getTaskTemplateResponse_createdTime,
    getTaskTemplateResponse_constraints,
    getTaskTemplateResponse_status,
    getTaskTemplateResponse_fields,
    getTaskTemplateResponse_description,
    getTaskTemplateResponse_lastModifiedTime,
    getTaskTemplateResponse_instanceId,
    getTaskTemplateResponse_defaults,
    getTaskTemplateResponse_contactFlowId,
    getTaskTemplateResponse_httpStatus,
    getTaskTemplateResponse_id,
    getTaskTemplateResponse_arn,
    getTaskTemplateResponse_name,

    -- ** GetTrafficDistribution
    getTrafficDistribution_id,
    getTrafficDistributionResponse_arn,
    getTrafficDistributionResponse_id,
    getTrafficDistributionResponse_telephonyConfig,
    getTrafficDistributionResponse_httpStatus,

    -- ** ListAgentStatuses
    listAgentStatuses_nextToken,
    listAgentStatuses_agentStatusTypes,
    listAgentStatuses_maxResults,
    listAgentStatuses_instanceId,
    listAgentStatusesResponse_nextToken,
    listAgentStatusesResponse_agentStatusSummaryList,
    listAgentStatusesResponse_httpStatus,

    -- ** ListApprovedOrigins
    listApprovedOrigins_nextToken,
    listApprovedOrigins_maxResults,
    listApprovedOrigins_instanceId,
    listApprovedOriginsResponse_nextToken,
    listApprovedOriginsResponse_origins,
    listApprovedOriginsResponse_httpStatus,

    -- ** ListBots
    listBots_nextToken,
    listBots_maxResults,
    listBots_instanceId,
    listBots_lexVersion,
    listBotsResponse_lexBots,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,

    -- ** ListContactFlowModules
    listContactFlowModules_nextToken,
    listContactFlowModules_contactFlowModuleState,
    listContactFlowModules_maxResults,
    listContactFlowModules_instanceId,
    listContactFlowModulesResponse_nextToken,
    listContactFlowModulesResponse_contactFlowModulesSummaryList,
    listContactFlowModulesResponse_httpStatus,

    -- ** ListContactFlows
    listContactFlows_nextToken,
    listContactFlows_maxResults,
    listContactFlows_contactFlowTypes,
    listContactFlows_instanceId,
    listContactFlowsResponse_nextToken,
    listContactFlowsResponse_contactFlowSummaryList,
    listContactFlowsResponse_httpStatus,

    -- ** ListContactReferences
    listContactReferences_nextToken,
    listContactReferences_instanceId,
    listContactReferences_contactId,
    listContactReferences_referenceTypes,
    listContactReferencesResponse_nextToken,
    listContactReferencesResponse_referenceSummaryList,
    listContactReferencesResponse_httpStatus,

    -- ** ListDefaultVocabularies
    listDefaultVocabularies_nextToken,
    listDefaultVocabularies_languageCode,
    listDefaultVocabularies_maxResults,
    listDefaultVocabularies_instanceId,
    listDefaultVocabulariesResponse_nextToken,
    listDefaultVocabulariesResponse_httpStatus,
    listDefaultVocabulariesResponse_defaultVocabularyList,

    -- ** ListHoursOfOperations
    listHoursOfOperations_nextToken,
    listHoursOfOperations_maxResults,
    listHoursOfOperations_instanceId,
    listHoursOfOperationsResponse_nextToken,
    listHoursOfOperationsResponse_hoursOfOperationSummaryList,
    listHoursOfOperationsResponse_httpStatus,

    -- ** ListInstanceAttributes
    listInstanceAttributes_nextToken,
    listInstanceAttributes_maxResults,
    listInstanceAttributes_instanceId,
    listInstanceAttributesResponse_nextToken,
    listInstanceAttributesResponse_attributes,
    listInstanceAttributesResponse_httpStatus,

    -- ** ListInstanceStorageConfigs
    listInstanceStorageConfigs_nextToken,
    listInstanceStorageConfigs_maxResults,
    listInstanceStorageConfigs_instanceId,
    listInstanceStorageConfigs_resourceType,
    listInstanceStorageConfigsResponse_nextToken,
    listInstanceStorageConfigsResponse_storageConfigs,
    listInstanceStorageConfigsResponse_httpStatus,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstancesResponse_nextToken,
    listInstancesResponse_instanceSummaryList,
    listInstancesResponse_httpStatus,

    -- ** ListIntegrationAssociations
    listIntegrationAssociations_nextToken,
    listIntegrationAssociations_integrationType,
    listIntegrationAssociations_maxResults,
    listIntegrationAssociations_instanceId,
    listIntegrationAssociationsResponse_nextToken,
    listIntegrationAssociationsResponse_integrationAssociationSummaryList,
    listIntegrationAssociationsResponse_httpStatus,

    -- ** ListLambdaFunctions
    listLambdaFunctions_nextToken,
    listLambdaFunctions_maxResults,
    listLambdaFunctions_instanceId,
    listLambdaFunctionsResponse_nextToken,
    listLambdaFunctionsResponse_lambdaFunctions,
    listLambdaFunctionsResponse_httpStatus,

    -- ** ListLexBots
    listLexBots_nextToken,
    listLexBots_maxResults,
    listLexBots_instanceId,
    listLexBotsResponse_lexBots,
    listLexBotsResponse_nextToken,
    listLexBotsResponse_httpStatus,

    -- ** ListPhoneNumbers
    listPhoneNumbers_nextToken,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_phoneNumberTypes,
    listPhoneNumbers_phoneNumberCountryCodes,
    listPhoneNumbers_instanceId,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumberSummaryList,
    listPhoneNumbersResponse_httpStatus,

    -- ** ListPhoneNumbersV2
    listPhoneNumbersV2_nextToken,
    listPhoneNumbersV2_targetArn,
    listPhoneNumbersV2_phoneNumberPrefix,
    listPhoneNumbersV2_maxResults,
    listPhoneNumbersV2_phoneNumberTypes,
    listPhoneNumbersV2_phoneNumberCountryCodes,
    listPhoneNumbersV2Response_nextToken,
    listPhoneNumbersV2Response_listPhoneNumbersSummaryList,
    listPhoneNumbersV2Response_httpStatus,

    -- ** ListPrompts
    listPrompts_nextToken,
    listPrompts_maxResults,
    listPrompts_instanceId,
    listPromptsResponse_nextToken,
    listPromptsResponse_promptSummaryList,
    listPromptsResponse_httpStatus,

    -- ** ListQueueQuickConnects
    listQueueQuickConnects_nextToken,
    listQueueQuickConnects_maxResults,
    listQueueQuickConnects_instanceId,
    listQueueQuickConnects_queueId,
    listQueueQuickConnectsResponse_nextToken,
    listQueueQuickConnectsResponse_quickConnectSummaryList,
    listQueueQuickConnectsResponse_httpStatus,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_maxResults,
    listQueues_queueTypes,
    listQueues_instanceId,
    listQueuesResponse_nextToken,
    listQueuesResponse_queueSummaryList,
    listQueuesResponse_httpStatus,

    -- ** ListQuickConnects
    listQuickConnects_nextToken,
    listQuickConnects_quickConnectTypes,
    listQuickConnects_maxResults,
    listQuickConnects_instanceId,
    listQuickConnectsResponse_nextToken,
    listQuickConnectsResponse_quickConnectSummaryList,
    listQuickConnectsResponse_httpStatus,

    -- ** ListRoutingProfileQueues
    listRoutingProfileQueues_nextToken,
    listRoutingProfileQueues_maxResults,
    listRoutingProfileQueues_instanceId,
    listRoutingProfileQueues_routingProfileId,
    listRoutingProfileQueuesResponse_nextToken,
    listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList,
    listRoutingProfileQueuesResponse_httpStatus,

    -- ** ListRoutingProfiles
    listRoutingProfiles_nextToken,
    listRoutingProfiles_maxResults,
    listRoutingProfiles_instanceId,
    listRoutingProfilesResponse_nextToken,
    listRoutingProfilesResponse_routingProfileSummaryList,
    listRoutingProfilesResponse_httpStatus,

    -- ** ListSecurityKeys
    listSecurityKeys_nextToken,
    listSecurityKeys_maxResults,
    listSecurityKeys_instanceId,
    listSecurityKeysResponse_nextToken,
    listSecurityKeysResponse_securityKeys,
    listSecurityKeysResponse_httpStatus,

    -- ** ListSecurityProfilePermissions
    listSecurityProfilePermissions_nextToken,
    listSecurityProfilePermissions_maxResults,
    listSecurityProfilePermissions_securityProfileId,
    listSecurityProfilePermissions_instanceId,
    listSecurityProfilePermissionsResponse_nextToken,
    listSecurityProfilePermissionsResponse_permissions,
    listSecurityProfilePermissionsResponse_httpStatus,

    -- ** ListSecurityProfiles
    listSecurityProfiles_nextToken,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_instanceId,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileSummaryList,
    listSecurityProfilesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTaskTemplates
    listTaskTemplates_name,
    listTaskTemplates_nextToken,
    listTaskTemplates_status,
    listTaskTemplates_maxResults,
    listTaskTemplates_instanceId,
    listTaskTemplatesResponse_nextToken,
    listTaskTemplatesResponse_taskTemplates,
    listTaskTemplatesResponse_httpStatus,

    -- ** ListTrafficDistributionGroups
    listTrafficDistributionGroups_nextToken,
    listTrafficDistributionGroups_instanceId,
    listTrafficDistributionGroups_maxResults,
    listTrafficDistributionGroupsResponse_nextToken,
    listTrafficDistributionGroupsResponse_trafficDistributionGroupSummaryList,
    listTrafficDistributionGroupsResponse_httpStatus,

    -- ** ListUseCases
    listUseCases_nextToken,
    listUseCases_maxResults,
    listUseCases_instanceId,
    listUseCases_integrationAssociationId,
    listUseCasesResponse_nextToken,
    listUseCasesResponse_useCaseSummaryList,
    listUseCasesResponse_httpStatus,

    -- ** ListUserHierarchyGroups
    listUserHierarchyGroups_nextToken,
    listUserHierarchyGroups_maxResults,
    listUserHierarchyGroups_instanceId,
    listUserHierarchyGroupsResponse_nextToken,
    listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList,
    listUserHierarchyGroupsResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_instanceId,
    listUsersResponse_nextToken,
    listUsersResponse_userSummaryList,
    listUsersResponse_httpStatus,

    -- ** MonitorContact
    monitorContact_clientToken,
    monitorContact_allowedMonitorCapabilities,
    monitorContact_instanceId,
    monitorContact_contactId,
    monitorContact_userId,
    monitorContactResponse_contactId,
    monitorContactResponse_contactArn,
    monitorContactResponse_httpStatus,

    -- ** PutUserStatus
    putUserStatus_userId,
    putUserStatus_instanceId,
    putUserStatus_agentStatusId,
    putUserStatusResponse_httpStatus,

    -- ** ReleasePhoneNumber
    releasePhoneNumber_clientToken,
    releasePhoneNumber_phoneNumberId,

    -- ** ReplicateInstance
    replicateInstance_clientToken,
    replicateInstance_instanceId,
    replicateInstance_replicaRegion,
    replicateInstance_replicaAlias,
    replicateInstanceResponse_arn,
    replicateInstanceResponse_id,
    replicateInstanceResponse_httpStatus,

    -- ** ResumeContactRecording
    resumeContactRecording_instanceId,
    resumeContactRecording_contactId,
    resumeContactRecording_initialContactId,
    resumeContactRecordingResponse_httpStatus,

    -- ** SearchAvailablePhoneNumbers
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_phoneNumberPrefix,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbers_targetArn,
    searchAvailablePhoneNumbers_phoneNumberCountryCode,
    searchAvailablePhoneNumbers_phoneNumberType,
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_availableNumbersList,
    searchAvailablePhoneNumbersResponse_httpStatus,

    -- ** SearchQueues
    searchQueues_nextToken,
    searchQueues_searchCriteria,
    searchQueues_searchFilter,
    searchQueues_maxResults,
    searchQueues_instanceId,
    searchQueuesResponse_nextToken,
    searchQueuesResponse_approximateTotalCount,
    searchQueuesResponse_queues,
    searchQueuesResponse_httpStatus,

    -- ** SearchRoutingProfiles
    searchRoutingProfiles_nextToken,
    searchRoutingProfiles_searchCriteria,
    searchRoutingProfiles_searchFilter,
    searchRoutingProfiles_maxResults,
    searchRoutingProfiles_instanceId,
    searchRoutingProfilesResponse_nextToken,
    searchRoutingProfilesResponse_approximateTotalCount,
    searchRoutingProfilesResponse_routingProfiles,
    searchRoutingProfilesResponse_httpStatus,

    -- ** SearchSecurityProfiles
    searchSecurityProfiles_nextToken,
    searchSecurityProfiles_searchCriteria,
    searchSecurityProfiles_searchFilter,
    searchSecurityProfiles_maxResults,
    searchSecurityProfiles_instanceId,
    searchSecurityProfilesResponse_nextToken,
    searchSecurityProfilesResponse_approximateTotalCount,
    searchSecurityProfilesResponse_securityProfiles,
    searchSecurityProfilesResponse_httpStatus,

    -- ** SearchUsers
    searchUsers_nextToken,
    searchUsers_searchCriteria,
    searchUsers_searchFilter,
    searchUsers_instanceId,
    searchUsers_maxResults,
    searchUsersResponse_nextToken,
    searchUsersResponse_users,
    searchUsersResponse_approximateTotalCount,
    searchUsersResponse_httpStatus,

    -- ** SearchVocabularies
    searchVocabularies_nextToken,
    searchVocabularies_nameStartsWith,
    searchVocabularies_state,
    searchVocabularies_languageCode,
    searchVocabularies_maxResults,
    searchVocabularies_instanceId,
    searchVocabulariesResponse_nextToken,
    searchVocabulariesResponse_vocabularySummaryList,
    searchVocabulariesResponse_httpStatus,

    -- ** StartChatContact
    startChatContact_clientToken,
    startChatContact_supportedMessagingContentTypes,
    startChatContact_initialMessage,
    startChatContact_attributes,
    startChatContact_chatDurationInMinutes,
    startChatContact_instanceId,
    startChatContact_contactFlowId,
    startChatContact_participantDetails,
    startChatContactResponse_participantId,
    startChatContactResponse_contactId,
    startChatContactResponse_participantToken,
    startChatContactResponse_httpStatus,

    -- ** StartContactRecording
    startContactRecording_instanceId,
    startContactRecording_contactId,
    startContactRecording_initialContactId,
    startContactRecording_voiceRecordingConfiguration,
    startContactRecordingResponse_httpStatus,

    -- ** StartContactStreaming
    startContactStreaming_instanceId,
    startContactStreaming_contactId,
    startContactStreaming_chatStreamingConfiguration,
    startContactStreaming_clientToken,
    startContactStreamingResponse_httpStatus,
    startContactStreamingResponse_streamingId,

    -- ** StartOutboundVoiceContact
    startOutboundVoiceContact_clientToken,
    startOutboundVoiceContact_trafficType,
    startOutboundVoiceContact_campaignId,
    startOutboundVoiceContact_answerMachineDetectionConfig,
    startOutboundVoiceContact_queueId,
    startOutboundVoiceContact_sourcePhoneNumber,
    startOutboundVoiceContact_attributes,
    startOutboundVoiceContact_destinationPhoneNumber,
    startOutboundVoiceContact_contactFlowId,
    startOutboundVoiceContact_instanceId,
    startOutboundVoiceContactResponse_contactId,
    startOutboundVoiceContactResponse_httpStatus,

    -- ** StartTaskContact
    startTaskContact_clientToken,
    startTaskContact_taskTemplateId,
    startTaskContact_description,
    startTaskContact_references,
    startTaskContact_quickConnectId,
    startTaskContact_attributes,
    startTaskContact_previousContactId,
    startTaskContact_contactFlowId,
    startTaskContact_scheduledTime,
    startTaskContact_instanceId,
    startTaskContact_name,
    startTaskContactResponse_contactId,
    startTaskContactResponse_httpStatus,

    -- ** StopContact
    stopContact_contactId,
    stopContact_instanceId,
    stopContactResponse_httpStatus,

    -- ** StopContactRecording
    stopContactRecording_instanceId,
    stopContactRecording_contactId,
    stopContactRecording_initialContactId,
    stopContactRecordingResponse_httpStatus,

    -- ** StopContactStreaming
    stopContactStreaming_instanceId,
    stopContactStreaming_contactId,
    stopContactStreaming_streamingId,
    stopContactStreamingResponse_httpStatus,

    -- ** SuspendContactRecording
    suspendContactRecording_instanceId,
    suspendContactRecording_contactId,
    suspendContactRecording_initialContactId,
    suspendContactRecordingResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TransferContact
    transferContact_clientToken,
    transferContact_queueId,
    transferContact_userId,
    transferContact_instanceId,
    transferContact_contactId,
    transferContact_contactFlowId,
    transferContactResponse_contactId,
    transferContactResponse_contactArn,
    transferContactResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateAgentStatus
    updateAgentStatus_name,
    updateAgentStatus_displayOrder,
    updateAgentStatus_state,
    updateAgentStatus_description,
    updateAgentStatus_resetOrderNumber,
    updateAgentStatus_instanceId,
    updateAgentStatus_agentStatusId,

    -- ** UpdateContact
    updateContact_name,
    updateContact_description,
    updateContact_references,
    updateContact_instanceId,
    updateContact_contactId,
    updateContactResponse_httpStatus,

    -- ** UpdateContactAttributes
    updateContactAttributes_initialContactId,
    updateContactAttributes_instanceId,
    updateContactAttributes_attributes,
    updateContactAttributesResponse_httpStatus,

    -- ** UpdateContactFlowContent
    updateContactFlowContent_instanceId,
    updateContactFlowContent_contactFlowId,
    updateContactFlowContent_content,

    -- ** UpdateContactFlowMetadata
    updateContactFlowMetadata_name,
    updateContactFlowMetadata_contactFlowState,
    updateContactFlowMetadata_description,
    updateContactFlowMetadata_instanceId,
    updateContactFlowMetadata_contactFlowId,

    -- ** UpdateContactFlowModuleContent
    updateContactFlowModuleContent_instanceId,
    updateContactFlowModuleContent_contactFlowModuleId,
    updateContactFlowModuleContent_content,
    updateContactFlowModuleContentResponse_httpStatus,

    -- ** UpdateContactFlowModuleMetadata
    updateContactFlowModuleMetadata_name,
    updateContactFlowModuleMetadata_state,
    updateContactFlowModuleMetadata_description,
    updateContactFlowModuleMetadata_instanceId,
    updateContactFlowModuleMetadata_contactFlowModuleId,
    updateContactFlowModuleMetadataResponse_httpStatus,

    -- ** UpdateContactFlowName
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- ** UpdateContactSchedule
    updateContactSchedule_instanceId,
    updateContactSchedule_contactId,
    updateContactSchedule_scheduledTime,
    updateContactScheduleResponse_httpStatus,

    -- ** UpdateHoursOfOperation
    updateHoursOfOperation_name,
    updateHoursOfOperation_timeZone,
    updateHoursOfOperation_description,
    updateHoursOfOperation_config,
    updateHoursOfOperation_instanceId,
    updateHoursOfOperation_hoursOfOperationId,

    -- ** UpdateInstanceAttribute
    updateInstanceAttribute_instanceId,
    updateInstanceAttribute_attributeType,
    updateInstanceAttribute_value,

    -- ** UpdateInstanceStorageConfig
    updateInstanceStorageConfig_instanceId,
    updateInstanceStorageConfig_associationId,
    updateInstanceStorageConfig_resourceType,
    updateInstanceStorageConfig_storageConfig,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_clientToken,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumber_targetArn,
    updatePhoneNumberResponse_phoneNumberArn,
    updatePhoneNumberResponse_phoneNumberId,
    updatePhoneNumberResponse_httpStatus,

    -- ** UpdateQueueHoursOfOperation
    updateQueueHoursOfOperation_instanceId,
    updateQueueHoursOfOperation_queueId,
    updateQueueHoursOfOperation_hoursOfOperationId,

    -- ** UpdateQueueMaxContacts
    updateQueueMaxContacts_maxContacts,
    updateQueueMaxContacts_instanceId,
    updateQueueMaxContacts_queueId,

    -- ** UpdateQueueName
    updateQueueName_name,
    updateQueueName_description,
    updateQueueName_instanceId,
    updateQueueName_queueId,

    -- ** UpdateQueueOutboundCallerConfig
    updateQueueOutboundCallerConfig_instanceId,
    updateQueueOutboundCallerConfig_queueId,
    updateQueueOutboundCallerConfig_outboundCallerConfig,

    -- ** UpdateQueueStatus
    updateQueueStatus_instanceId,
    updateQueueStatus_queueId,
    updateQueueStatus_status,

    -- ** UpdateQuickConnectConfig
    updateQuickConnectConfig_instanceId,
    updateQuickConnectConfig_quickConnectId,
    updateQuickConnectConfig_quickConnectConfig,

    -- ** UpdateQuickConnectName
    updateQuickConnectName_name,
    updateQuickConnectName_description,
    updateQuickConnectName_instanceId,
    updateQuickConnectName_quickConnectId,

    -- ** UpdateRoutingProfileConcurrency
    updateRoutingProfileConcurrency_instanceId,
    updateRoutingProfileConcurrency_routingProfileId,
    updateRoutingProfileConcurrency_mediaConcurrencies,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    updateRoutingProfileDefaultOutboundQueue_instanceId,
    updateRoutingProfileDefaultOutboundQueue_routingProfileId,
    updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId,

    -- ** UpdateRoutingProfileName
    updateRoutingProfileName_name,
    updateRoutingProfileName_description,
    updateRoutingProfileName_instanceId,
    updateRoutingProfileName_routingProfileId,

    -- ** UpdateRoutingProfileQueues
    updateRoutingProfileQueues_instanceId,
    updateRoutingProfileQueues_routingProfileId,
    updateRoutingProfileQueues_queueConfigs,

    -- ** UpdateSecurityProfile
    updateSecurityProfile_allowedAccessControlTags,
    updateSecurityProfile_permissions,
    updateSecurityProfile_description,
    updateSecurityProfile_tagRestrictedResources,
    updateSecurityProfile_securityProfileId,
    updateSecurityProfile_instanceId,

    -- ** UpdateTaskTemplate
    updateTaskTemplate_name,
    updateTaskTemplate_constraints,
    updateTaskTemplate_status,
    updateTaskTemplate_fields,
    updateTaskTemplate_description,
    updateTaskTemplate_defaults,
    updateTaskTemplate_contactFlowId,
    updateTaskTemplate_taskTemplateId,
    updateTaskTemplate_instanceId,
    updateTaskTemplateResponse_name,
    updateTaskTemplateResponse_createdTime,
    updateTaskTemplateResponse_constraints,
    updateTaskTemplateResponse_arn,
    updateTaskTemplateResponse_status,
    updateTaskTemplateResponse_fields,
    updateTaskTemplateResponse_id,
    updateTaskTemplateResponse_description,
    updateTaskTemplateResponse_lastModifiedTime,
    updateTaskTemplateResponse_instanceId,
    updateTaskTemplateResponse_defaults,
    updateTaskTemplateResponse_contactFlowId,
    updateTaskTemplateResponse_httpStatus,

    -- ** UpdateTrafficDistribution
    updateTrafficDistribution_telephonyConfig,
    updateTrafficDistribution_id,
    updateTrafficDistributionResponse_httpStatus,

    -- ** UpdateUserHierarchy
    updateUserHierarchy_hierarchyGroupId,
    updateUserHierarchy_userId,
    updateUserHierarchy_instanceId,

    -- ** UpdateUserHierarchyGroupName
    updateUserHierarchyGroupName_name,
    updateUserHierarchyGroupName_hierarchyGroupId,
    updateUserHierarchyGroupName_instanceId,

    -- ** UpdateUserHierarchyStructure
    updateUserHierarchyStructure_hierarchyStructure,
    updateUserHierarchyStructure_instanceId,

    -- ** UpdateUserIdentityInfo
    updateUserIdentityInfo_identityInfo,
    updateUserIdentityInfo_userId,
    updateUserIdentityInfo_instanceId,

    -- ** UpdateUserPhoneConfig
    updateUserPhoneConfig_phoneConfig,
    updateUserPhoneConfig_userId,
    updateUserPhoneConfig_instanceId,

    -- ** UpdateUserRoutingProfile
    updateUserRoutingProfile_routingProfileId,
    updateUserRoutingProfile_userId,
    updateUserRoutingProfile_instanceId,

    -- ** UpdateUserSecurityProfiles
    updateUserSecurityProfiles_securityProfileIds,
    updateUserSecurityProfiles_userId,
    updateUserSecurityProfiles_instanceId,

    -- * Types

    -- ** AgentContactReference
    agentContactReference_agentContactState,
    agentContactReference_contactId,
    agentContactReference_channel,
    agentContactReference_connectedToAgentTimestamp,
    agentContactReference_initiationMethod,
    agentContactReference_queue,
    agentContactReference_stateStartTimestamp,

    -- ** AgentInfo
    agentInfo_id,
    agentInfo_connectedToAgentTimestamp,

    -- ** AgentStatus
    agentStatus_tags,
    agentStatus_name,
    agentStatus_type,
    agentStatus_displayOrder,
    agentStatus_agentStatusId,
    agentStatus_state,
    agentStatus_description,
    agentStatus_agentStatusARN,

    -- ** AgentStatusReference
    agentStatusReference_statusArn,
    agentStatusReference_statusStartTimestamp,

    -- ** AgentStatusSummary
    agentStatusSummary_name,
    agentStatusSummary_type,
    agentStatusSummary_arn,
    agentStatusSummary_id,

    -- ** AnswerMachineDetectionConfig
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,
    answerMachineDetectionConfig_enableAnswerMachineDetection,

    -- ** AttachmentReference
    attachmentReference_name,
    attachmentReference_status,
    attachmentReference_value,

    -- ** Attribute
    attribute_attributeType,
    attribute_value,

    -- ** AvailableNumberSummary
    availableNumberSummary_phoneNumberCountryCode,
    availableNumberSummary_phoneNumberType,
    availableNumberSummary_phoneNumber,

    -- ** ChatMessage
    chatMessage_contentType,
    chatMessage_content,

    -- ** ChatStreamingConfiguration
    chatStreamingConfiguration_streamingEndpointArn,

    -- ** ClaimedPhoneNumberSummary
    claimedPhoneNumberSummary_tags,
    claimedPhoneNumberSummary_phoneNumberCountryCode,
    claimedPhoneNumberSummary_phoneNumberArn,
    claimedPhoneNumberSummary_phoneNumberType,
    claimedPhoneNumberSummary_phoneNumberDescription,
    claimedPhoneNumberSummary_targetArn,
    claimedPhoneNumberSummary_phoneNumberStatus,
    claimedPhoneNumberSummary_phoneNumberId,
    claimedPhoneNumberSummary_phoneNumber,

    -- ** Contact
    contact_name,
    contact_lastUpdateTimestamp,
    contact_channel,
    contact_arn,
    contact_id,
    contact_description,
    contact_queueInfo,
    contact_scheduledTimestamp,
    contact_initialContactId,
    contact_initiationTimestamp,
    contact_previousContactId,
    contact_initiationMethod,
    contact_disconnectTimestamp,
    contact_agentInfo,

    -- ** ContactFilter
    contactFilter_contactStates,

    -- ** ContactFlow
    contactFlow_tags,
    contactFlow_name,
    contactFlow_type,
    contactFlow_arn,
    contactFlow_state,
    contactFlow_id,
    contactFlow_description,
    contactFlow_content,

    -- ** ContactFlowModule
    contactFlowModule_tags,
    contactFlowModule_name,
    contactFlowModule_arn,
    contactFlowModule_state,
    contactFlowModule_status,
    contactFlowModule_id,
    contactFlowModule_description,
    contactFlowModule_content,

    -- ** ContactFlowModuleSummary
    contactFlowModuleSummary_name,
    contactFlowModuleSummary_arn,
    contactFlowModuleSummary_state,
    contactFlowModuleSummary_id,

    -- ** ContactFlowSummary
    contactFlowSummary_name,
    contactFlowSummary_contactFlowState,
    contactFlowSummary_arn,
    contactFlowSummary_id,
    contactFlowSummary_contactFlowType,

    -- ** ControlPlaneTagFilter
    controlPlaneTagFilter_orConditions,
    controlPlaneTagFilter_tagCondition,
    controlPlaneTagFilter_andConditions,

    -- ** Credentials
    credentials_accessToken,
    credentials_accessTokenExpiration,
    credentials_refreshTokenExpiration,
    credentials_refreshToken,

    -- ** CurrentMetric
    currentMetric_name,
    currentMetric_unit,

    -- ** CurrentMetricData
    currentMetricData_metric,
    currentMetricData_value,

    -- ** CurrentMetricResult
    currentMetricResult_collections,
    currentMetricResult_dimensions,

    -- ** DateReference
    dateReference_name,
    dateReference_value,

    -- ** DefaultVocabulary
    defaultVocabulary_instanceId,
    defaultVocabulary_languageCode,
    defaultVocabulary_vocabularyId,
    defaultVocabulary_vocabularyName,

    -- ** Dimensions
    dimensions_channel,
    dimensions_queue,

    -- ** Distribution
    distribution_region,
    distribution_percentage,

    -- ** EmailReference
    emailReference_name,
    emailReference_value,

    -- ** EncryptionConfig
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- ** Filters
    filters_channels,
    filters_queues,

    -- ** HierarchyGroup
    hierarchyGroup_tags,
    hierarchyGroup_name,
    hierarchyGroup_arn,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,

    -- ** HierarchyGroupCondition
    hierarchyGroupCondition_hierarchyGroupMatchType,
    hierarchyGroupCondition_value,

    -- ** HierarchyGroupSummary
    hierarchyGroupSummary_name,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,

    -- ** HierarchyGroupSummaryReference
    hierarchyGroupSummaryReference_arn,
    hierarchyGroupSummaryReference_id,

    -- ** HierarchyLevel
    hierarchyLevel_name,
    hierarchyLevel_arn,
    hierarchyLevel_id,

    -- ** HierarchyLevelUpdate
    hierarchyLevelUpdate_name,

    -- ** HierarchyPath
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelOne,
    hierarchyPath_levelFive,
    hierarchyPath_levelTwo,

    -- ** HierarchyPathReference
    hierarchyPathReference_levelThree,
    hierarchyPathReference_levelFour,
    hierarchyPathReference_levelOne,
    hierarchyPathReference_levelFive,
    hierarchyPathReference_levelTwo,

    -- ** HierarchyStructure
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelOne,
    hierarchyStructure_levelFive,
    hierarchyStructure_levelTwo,

    -- ** HierarchyStructureUpdate
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelOne,
    hierarchyStructureUpdate_levelFive,
    hierarchyStructureUpdate_levelTwo,

    -- ** HistoricalMetric
    historicalMetric_name,
    historicalMetric_threshold,
    historicalMetric_statistic,
    historicalMetric_unit,

    -- ** HistoricalMetricData
    historicalMetricData_metric,
    historicalMetricData_value,

    -- ** HistoricalMetricResult
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- ** HoursOfOperation
    hoursOfOperation_tags,
    hoursOfOperation_name,
    hoursOfOperation_timeZone,
    hoursOfOperation_description,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_hoursOfOperationId,
    hoursOfOperation_config,

    -- ** HoursOfOperationConfig
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- ** HoursOfOperationSummary
    hoursOfOperationSummary_name,
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_id,

    -- ** HoursOfOperationTimeSlice
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- ** Instance
    instance_identityManagementType,
    instance_createdTime,
    instance_instanceStatus,
    instance_arn,
    instance_statusReason,
    instance_id,
    instance_serviceRole,
    instance_inboundCallsEnabled,
    instance_instanceAlias,
    instance_outboundCallsEnabled,

    -- ** InstanceStatusReason
    instanceStatusReason_message,

    -- ** InstanceStorageConfig
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_associationId,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_storageType,

    -- ** InstanceSummary
    instanceSummary_identityManagementType,
    instanceSummary_createdTime,
    instanceSummary_instanceStatus,
    instanceSummary_arn,
    instanceSummary_id,
    instanceSummary_serviceRole,
    instanceSummary_inboundCallsEnabled,
    instanceSummary_instanceAlias,
    instanceSummary_outboundCallsEnabled,

    -- ** IntegrationAssociationSummary
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_sourceType,
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_integrationType,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_integrationAssociationId,

    -- ** InvisibleFieldInfo
    invisibleFieldInfo_id,

    -- ** KinesisFirehoseConfig
    kinesisFirehoseConfig_firehoseArn,

    -- ** KinesisStreamConfig
    kinesisStreamConfig_streamArn,

    -- ** KinesisVideoStreamConfig
    kinesisVideoStreamConfig_prefix,
    kinesisVideoStreamConfig_retentionPeriodHours,
    kinesisVideoStreamConfig_encryptionConfig,

    -- ** LexBot
    lexBot_name,
    lexBot_lexRegion,

    -- ** LexBotConfig
    lexBotConfig_lexV2Bot,
    lexBotConfig_lexBot,

    -- ** LexV2Bot
    lexV2Bot_aliasArn,

    -- ** ListPhoneNumbersSummary
    listPhoneNumbersSummary_phoneNumberCountryCode,
    listPhoneNumbersSummary_phoneNumberArn,
    listPhoneNumbersSummary_phoneNumberType,
    listPhoneNumbersSummary_targetArn,
    listPhoneNumbersSummary_phoneNumberId,
    listPhoneNumbersSummary_phoneNumber,

    -- ** MediaConcurrency
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- ** NumberReference
    numberReference_name,
    numberReference_value,

    -- ** OutboundCallerConfig
    outboundCallerConfig_outboundFlowId,
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundCallerIdName,

    -- ** ParticipantDetails
    participantDetails_displayName,

    -- ** PhoneNumberQuickConnectConfig
    phoneNumberQuickConnectConfig_phoneNumber,

    -- ** PhoneNumberStatus
    phoneNumberStatus_message,
    phoneNumberStatus_status,

    -- ** PhoneNumberSummary
    phoneNumberSummary_phoneNumberCountryCode,
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_arn,
    phoneNumberSummary_id,
    phoneNumberSummary_phoneNumber,

    -- ** PromptSummary
    promptSummary_name,
    promptSummary_arn,
    promptSummary_id,

    -- ** Queue
    queue_tags,
    queue_name,
    queue_queueArn,
    queue_status,
    queue_description,
    queue_queueId,
    queue_hoursOfOperationId,
    queue_maxContacts,
    queue_outboundCallerConfig,

    -- ** QueueInfo
    queueInfo_id,
    queueInfo_enqueueTimestamp,

    -- ** QueueQuickConnectConfig
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- ** QueueReference
    queueReference_arn,
    queueReference_id,

    -- ** QueueSearchCriteria
    queueSearchCriteria_stringCondition,
    queueSearchCriteria_orConditions,
    queueSearchCriteria_andConditions,
    queueSearchCriteria_queueTypeCondition,

    -- ** QueueSearchFilter
    queueSearchFilter_tagFilter,

    -- ** QueueSummary
    queueSummary_name,
    queueSummary_arn,
    queueSummary_id,
    queueSummary_queueType,

    -- ** QuickConnect
    quickConnect_tags,
    quickConnect_name,
    quickConnect_quickConnectConfig,
    quickConnect_description,
    quickConnect_quickConnectARN,
    quickConnect_quickConnectId,

    -- ** QuickConnectConfig
    quickConnectConfig_queueConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_quickConnectType,

    -- ** QuickConnectSummary
    quickConnectSummary_name,
    quickConnectSummary_quickConnectType,
    quickConnectSummary_arn,
    quickConnectSummary_id,

    -- ** ReadOnlyFieldInfo
    readOnlyFieldInfo_id,

    -- ** Reference
    reference_value,
    reference_type,

    -- ** ReferenceSummary
    referenceSummary_number,
    referenceSummary_attachment,
    referenceSummary_email,
    referenceSummary_date,
    referenceSummary_string,
    referenceSummary_url,

    -- ** RequiredFieldInfo
    requiredFieldInfo_id,

    -- ** RoutingProfile
    routingProfile_tags,
    routingProfile_name,
    routingProfile_numberOfAssociatedUsers,
    routingProfile_description,
    routingProfile_mediaConcurrencies,
    routingProfile_routingProfileArn,
    routingProfile_instanceId,
    routingProfile_numberOfAssociatedQueues,
    routingProfile_defaultOutboundQueueId,
    routingProfile_routingProfileId,

    -- ** RoutingProfileQueueConfig
    routingProfileQueueConfig_queueReference,
    routingProfileQueueConfig_priority,
    routingProfileQueueConfig_delay,

    -- ** RoutingProfileQueueConfigSummary
    routingProfileQueueConfigSummary_queueId,
    routingProfileQueueConfigSummary_queueArn,
    routingProfileQueueConfigSummary_queueName,
    routingProfileQueueConfigSummary_priority,
    routingProfileQueueConfigSummary_delay,
    routingProfileQueueConfigSummary_channel,

    -- ** RoutingProfileQueueReference
    routingProfileQueueReference_queueId,
    routingProfileQueueReference_channel,

    -- ** RoutingProfileReference
    routingProfileReference_arn,
    routingProfileReference_id,

    -- ** RoutingProfileSearchCriteria
    routingProfileSearchCriteria_stringCondition,
    routingProfileSearchCriteria_orConditions,
    routingProfileSearchCriteria_andConditions,

    -- ** RoutingProfileSearchFilter
    routingProfileSearchFilter_tagFilter,

    -- ** RoutingProfileSummary
    routingProfileSummary_name,
    routingProfileSummary_arn,
    routingProfileSummary_id,

    -- ** S3Config
    s3Config_encryptionConfig,
    s3Config_bucketName,
    s3Config_bucketPrefix,

    -- ** SecurityKey
    securityKey_key,
    securityKey_creationTime,
    securityKey_associationId,

    -- ** SecurityProfile
    securityProfile_tags,
    securityProfile_allowedAccessControlTags,
    securityProfile_arn,
    securityProfile_id,
    securityProfile_description,
    securityProfile_securityProfileName,
    securityProfile_organizationResourceId,
    securityProfile_tagRestrictedResources,

    -- ** SecurityProfileSearchCriteria
    securityProfileSearchCriteria_stringCondition,
    securityProfileSearchCriteria_orConditions,
    securityProfileSearchCriteria_andConditions,

    -- ** SecurityProfileSearchSummary
    securityProfileSearchSummary_tags,
    securityProfileSearchSummary_arn,
    securityProfileSearchSummary_id,
    securityProfileSearchSummary_description,
    securityProfileSearchSummary_securityProfileName,
    securityProfileSearchSummary_organizationResourceId,

    -- ** SecurityProfileSummary
    securityProfileSummary_name,
    securityProfileSummary_arn,
    securityProfileSummary_id,

    -- ** SecurityProfilesSearchFilter
    securityProfilesSearchFilter_tagFilter,

    -- ** StringCondition
    stringCondition_fieldName,
    stringCondition_comparisonType,
    stringCondition_value,

    -- ** StringReference
    stringReference_name,
    stringReference_value,

    -- ** TagCondition
    tagCondition_tagValue,
    tagCondition_tagKey,

    -- ** TaskTemplateConstraints
    taskTemplateConstraints_invisibleFields,
    taskTemplateConstraints_readOnlyFields,
    taskTemplateConstraints_requiredFields,

    -- ** TaskTemplateDefaultFieldValue
    taskTemplateDefaultFieldValue_defaultValue,
    taskTemplateDefaultFieldValue_id,

    -- ** TaskTemplateDefaults
    taskTemplateDefaults_defaultFieldValues,

    -- ** TaskTemplateField
    taskTemplateField_singleSelectOptions,
    taskTemplateField_type,
    taskTemplateField_description,
    taskTemplateField_id,

    -- ** TaskTemplateFieldIdentifier
    taskTemplateFieldIdentifier_name,

    -- ** TaskTemplateMetadata
    taskTemplateMetadata_name,
    taskTemplateMetadata_createdTime,
    taskTemplateMetadata_arn,
    taskTemplateMetadata_status,
    taskTemplateMetadata_id,
    taskTemplateMetadata_description,
    taskTemplateMetadata_lastModifiedTime,

    -- ** TelephonyConfig
    telephonyConfig_distributions,

    -- ** Threshold
    threshold_thresholdValue,
    threshold_comparison,

    -- ** TrafficDistributionGroup
    trafficDistributionGroup_tags,
    trafficDistributionGroup_name,
    trafficDistributionGroup_arn,
    trafficDistributionGroup_status,
    trafficDistributionGroup_id,
    trafficDistributionGroup_description,
    trafficDistributionGroup_instanceArn,

    -- ** TrafficDistributionGroupSummary
    trafficDistributionGroupSummary_name,
    trafficDistributionGroupSummary_arn,
    trafficDistributionGroupSummary_status,
    trafficDistributionGroupSummary_id,
    trafficDistributionGroupSummary_instanceArn,

    -- ** UrlReference
    urlReference_name,
    urlReference_value,

    -- ** UseCase
    useCase_useCaseType,
    useCase_useCaseArn,
    useCase_useCaseId,

    -- ** User
    user_tags,
    user_hierarchyGroupId,
    user_identityInfo,
    user_username,
    user_arn,
    user_securityProfileIds,
    user_phoneConfig,
    user_id,
    user_directoryUserId,
    user_routingProfileId,

    -- ** UserData
    userData_availableSlotsByChannel,
    userData_user,
    userData_hierarchyPath,
    userData_routingProfile,
    userData_status,
    userData_activeSlotsByChannel,
    userData_maxSlotsByChannel,
    userData_contacts,

    -- ** UserDataFilters
    userDataFilters_contactFilter,
    userDataFilters_queues,

    -- ** UserIdentityInfo
    userIdentityInfo_firstName,
    userIdentityInfo_email,
    userIdentityInfo_secondaryEmail,
    userIdentityInfo_lastName,
    userIdentityInfo_mobile,

    -- ** UserIdentityInfoLite
    userIdentityInfoLite_firstName,
    userIdentityInfoLite_lastName,

    -- ** UserPhoneConfig
    userPhoneConfig_autoAccept,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_phoneType,

    -- ** UserQuickConnectConfig
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- ** UserReference
    userReference_arn,
    userReference_id,

    -- ** UserSearchCriteria
    userSearchCriteria_hierarchyGroupCondition,
    userSearchCriteria_stringCondition,
    userSearchCriteria_orConditions,
    userSearchCriteria_andConditions,

    -- ** UserSearchFilter
    userSearchFilter_tagFilter,

    -- ** UserSearchSummary
    userSearchSummary_tags,
    userSearchSummary_hierarchyGroupId,
    userSearchSummary_identityInfo,
    userSearchSummary_username,
    userSearchSummary_arn,
    userSearchSummary_securityProfileIds,
    userSearchSummary_phoneConfig,
    userSearchSummary_id,
    userSearchSummary_directoryUserId,
    userSearchSummary_routingProfileId,

    -- ** UserSummary
    userSummary_username,
    userSummary_arn,
    userSummary_id,

    -- ** Vocabulary
    vocabulary_tags,
    vocabulary_content,
    vocabulary_failureReason,
    vocabulary_name,
    vocabulary_id,
    vocabulary_arn,
    vocabulary_languageCode,
    vocabulary_state,
    vocabulary_lastModifiedTime,

    -- ** VocabularySummary
    vocabularySummary_failureReason,
    vocabularySummary_name,
    vocabularySummary_id,
    vocabularySummary_arn,
    vocabularySummary_languageCode,
    vocabularySummary_state,
    vocabularySummary_lastModifiedTime,

    -- ** VoiceRecordingConfiguration
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Amazonka.Connect.AssociateApprovedOrigin
import Amazonka.Connect.AssociateBot
import Amazonka.Connect.AssociateDefaultVocabulary
import Amazonka.Connect.AssociateInstanceStorageConfig
import Amazonka.Connect.AssociateLambdaFunction
import Amazonka.Connect.AssociateLexBot
import Amazonka.Connect.AssociatePhoneNumberContactFlow
import Amazonka.Connect.AssociateQueueQuickConnects
import Amazonka.Connect.AssociateRoutingProfileQueues
import Amazonka.Connect.AssociateSecurityKey
import Amazonka.Connect.ClaimPhoneNumber
import Amazonka.Connect.CreateAgentStatus
import Amazonka.Connect.CreateContactFlow
import Amazonka.Connect.CreateContactFlowModule
import Amazonka.Connect.CreateHoursOfOperation
import Amazonka.Connect.CreateInstance
import Amazonka.Connect.CreateIntegrationAssociation
import Amazonka.Connect.CreateQueue
import Amazonka.Connect.CreateQuickConnect
import Amazonka.Connect.CreateRoutingProfile
import Amazonka.Connect.CreateSecurityProfile
import Amazonka.Connect.CreateTaskTemplate
import Amazonka.Connect.CreateTrafficDistributionGroup
import Amazonka.Connect.CreateUseCase
import Amazonka.Connect.CreateUser
import Amazonka.Connect.CreateUserHierarchyGroup
import Amazonka.Connect.CreateVocabulary
import Amazonka.Connect.DeleteContactFlow
import Amazonka.Connect.DeleteContactFlowModule
import Amazonka.Connect.DeleteHoursOfOperation
import Amazonka.Connect.DeleteInstance
import Amazonka.Connect.DeleteIntegrationAssociation
import Amazonka.Connect.DeleteQuickConnect
import Amazonka.Connect.DeleteSecurityProfile
import Amazonka.Connect.DeleteTaskTemplate
import Amazonka.Connect.DeleteTrafficDistributionGroup
import Amazonka.Connect.DeleteUseCase
import Amazonka.Connect.DeleteUser
import Amazonka.Connect.DeleteUserHierarchyGroup
import Amazonka.Connect.DeleteVocabulary
import Amazonka.Connect.DescribeAgentStatus
import Amazonka.Connect.DescribeContact
import Amazonka.Connect.DescribeContactFlow
import Amazonka.Connect.DescribeContactFlowModule
import Amazonka.Connect.DescribeHoursOfOperation
import Amazonka.Connect.DescribeInstance
import Amazonka.Connect.DescribeInstanceAttribute
import Amazonka.Connect.DescribeInstanceStorageConfig
import Amazonka.Connect.DescribePhoneNumber
import Amazonka.Connect.DescribeQueue
import Amazonka.Connect.DescribeQuickConnect
import Amazonka.Connect.DescribeRoutingProfile
import Amazonka.Connect.DescribeSecurityProfile
import Amazonka.Connect.DescribeTrafficDistributionGroup
import Amazonka.Connect.DescribeUser
import Amazonka.Connect.DescribeUserHierarchyGroup
import Amazonka.Connect.DescribeUserHierarchyStructure
import Amazonka.Connect.DescribeVocabulary
import Amazonka.Connect.DisassociateApprovedOrigin
import Amazonka.Connect.DisassociateBot
import Amazonka.Connect.DisassociateInstanceStorageConfig
import Amazonka.Connect.DisassociateLambdaFunction
import Amazonka.Connect.DisassociateLexBot
import Amazonka.Connect.DisassociatePhoneNumberContactFlow
import Amazonka.Connect.DisassociateQueueQuickConnects
import Amazonka.Connect.DisassociateRoutingProfileQueues
import Amazonka.Connect.DisassociateSecurityKey
import Amazonka.Connect.DismissUserContact
import Amazonka.Connect.GetContactAttributes
import Amazonka.Connect.GetCurrentMetricData
import Amazonka.Connect.GetCurrentUserData
import Amazonka.Connect.GetFederationToken
import Amazonka.Connect.GetMetricData
import Amazonka.Connect.GetTaskTemplate
import Amazonka.Connect.GetTrafficDistribution
import Amazonka.Connect.ListAgentStatuses
import Amazonka.Connect.ListApprovedOrigins
import Amazonka.Connect.ListBots
import Amazonka.Connect.ListContactFlowModules
import Amazonka.Connect.ListContactFlows
import Amazonka.Connect.ListContactReferences
import Amazonka.Connect.ListDefaultVocabularies
import Amazonka.Connect.ListHoursOfOperations
import Amazonka.Connect.ListInstanceAttributes
import Amazonka.Connect.ListInstanceStorageConfigs
import Amazonka.Connect.ListInstances
import Amazonka.Connect.ListIntegrationAssociations
import Amazonka.Connect.ListLambdaFunctions
import Amazonka.Connect.ListLexBots
import Amazonka.Connect.ListPhoneNumbers
import Amazonka.Connect.ListPhoneNumbersV2
import Amazonka.Connect.ListPrompts
import Amazonka.Connect.ListQueueQuickConnects
import Amazonka.Connect.ListQueues
import Amazonka.Connect.ListQuickConnects
import Amazonka.Connect.ListRoutingProfileQueues
import Amazonka.Connect.ListRoutingProfiles
import Amazonka.Connect.ListSecurityKeys
import Amazonka.Connect.ListSecurityProfilePermissions
import Amazonka.Connect.ListSecurityProfiles
import Amazonka.Connect.ListTagsForResource
import Amazonka.Connect.ListTaskTemplates
import Amazonka.Connect.ListTrafficDistributionGroups
import Amazonka.Connect.ListUseCases
import Amazonka.Connect.ListUserHierarchyGroups
import Amazonka.Connect.ListUsers
import Amazonka.Connect.MonitorContact
import Amazonka.Connect.PutUserStatus
import Amazonka.Connect.ReleasePhoneNumber
import Amazonka.Connect.ReplicateInstance
import Amazonka.Connect.ResumeContactRecording
import Amazonka.Connect.SearchAvailablePhoneNumbers
import Amazonka.Connect.SearchQueues
import Amazonka.Connect.SearchRoutingProfiles
import Amazonka.Connect.SearchSecurityProfiles
import Amazonka.Connect.SearchUsers
import Amazonka.Connect.SearchVocabularies
import Amazonka.Connect.StartChatContact
import Amazonka.Connect.StartContactRecording
import Amazonka.Connect.StartContactStreaming
import Amazonka.Connect.StartOutboundVoiceContact
import Amazonka.Connect.StartTaskContact
import Amazonka.Connect.StopContact
import Amazonka.Connect.StopContactRecording
import Amazonka.Connect.StopContactStreaming
import Amazonka.Connect.SuspendContactRecording
import Amazonka.Connect.TagResource
import Amazonka.Connect.TransferContact
import Amazonka.Connect.Types.AgentContactReference
import Amazonka.Connect.Types.AgentInfo
import Amazonka.Connect.Types.AgentStatus
import Amazonka.Connect.Types.AgentStatusReference
import Amazonka.Connect.Types.AgentStatusSummary
import Amazonka.Connect.Types.AnswerMachineDetectionConfig
import Amazonka.Connect.Types.AttachmentReference
import Amazonka.Connect.Types.Attribute
import Amazonka.Connect.Types.AvailableNumberSummary
import Amazonka.Connect.Types.ChatMessage
import Amazonka.Connect.Types.ChatStreamingConfiguration
import Amazonka.Connect.Types.ClaimedPhoneNumberSummary
import Amazonka.Connect.Types.Contact
import Amazonka.Connect.Types.ContactFilter
import Amazonka.Connect.Types.ContactFlow
import Amazonka.Connect.Types.ContactFlowModule
import Amazonka.Connect.Types.ContactFlowModuleSummary
import Amazonka.Connect.Types.ContactFlowSummary
import Amazonka.Connect.Types.ControlPlaneTagFilter
import Amazonka.Connect.Types.Credentials
import Amazonka.Connect.Types.CurrentMetric
import Amazonka.Connect.Types.CurrentMetricData
import Amazonka.Connect.Types.CurrentMetricResult
import Amazonka.Connect.Types.DateReference
import Amazonka.Connect.Types.DefaultVocabulary
import Amazonka.Connect.Types.Dimensions
import Amazonka.Connect.Types.Distribution
import Amazonka.Connect.Types.EmailReference
import Amazonka.Connect.Types.EncryptionConfig
import Amazonka.Connect.Types.Filters
import Amazonka.Connect.Types.HierarchyGroup
import Amazonka.Connect.Types.HierarchyGroupCondition
import Amazonka.Connect.Types.HierarchyGroupSummary
import Amazonka.Connect.Types.HierarchyGroupSummaryReference
import Amazonka.Connect.Types.HierarchyLevel
import Amazonka.Connect.Types.HierarchyLevelUpdate
import Amazonka.Connect.Types.HierarchyPath
import Amazonka.Connect.Types.HierarchyPathReference
import Amazonka.Connect.Types.HierarchyStructure
import Amazonka.Connect.Types.HierarchyStructureUpdate
import Amazonka.Connect.Types.HistoricalMetric
import Amazonka.Connect.Types.HistoricalMetricData
import Amazonka.Connect.Types.HistoricalMetricResult
import Amazonka.Connect.Types.HoursOfOperation
import Amazonka.Connect.Types.HoursOfOperationConfig
import Amazonka.Connect.Types.HoursOfOperationSummary
import Amazonka.Connect.Types.HoursOfOperationTimeSlice
import Amazonka.Connect.Types.Instance
import Amazonka.Connect.Types.InstanceStatusReason
import Amazonka.Connect.Types.InstanceStorageConfig
import Amazonka.Connect.Types.InstanceSummary
import Amazonka.Connect.Types.IntegrationAssociationSummary
import Amazonka.Connect.Types.InvisibleFieldInfo
import Amazonka.Connect.Types.KinesisFirehoseConfig
import Amazonka.Connect.Types.KinesisStreamConfig
import Amazonka.Connect.Types.KinesisVideoStreamConfig
import Amazonka.Connect.Types.LexBot
import Amazonka.Connect.Types.LexBotConfig
import Amazonka.Connect.Types.LexV2Bot
import Amazonka.Connect.Types.ListPhoneNumbersSummary
import Amazonka.Connect.Types.MediaConcurrency
import Amazonka.Connect.Types.NumberReference
import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.ParticipantDetails
import Amazonka.Connect.Types.PhoneNumberQuickConnectConfig
import Amazonka.Connect.Types.PhoneNumberStatus
import Amazonka.Connect.Types.PhoneNumberSummary
import Amazonka.Connect.Types.PromptSummary
import Amazonka.Connect.Types.Queue
import Amazonka.Connect.Types.QueueInfo
import Amazonka.Connect.Types.QueueQuickConnectConfig
import Amazonka.Connect.Types.QueueReference
import Amazonka.Connect.Types.QueueSearchCriteria
import Amazonka.Connect.Types.QueueSearchFilter
import Amazonka.Connect.Types.QueueSummary
import Amazonka.Connect.Types.QuickConnect
import Amazonka.Connect.Types.QuickConnectConfig
import Amazonka.Connect.Types.QuickConnectSummary
import Amazonka.Connect.Types.ReadOnlyFieldInfo
import Amazonka.Connect.Types.Reference
import Amazonka.Connect.Types.ReferenceSummary
import Amazonka.Connect.Types.RequiredFieldInfo
import Amazonka.Connect.Types.RoutingProfile
import Amazonka.Connect.Types.RoutingProfileQueueConfig
import Amazonka.Connect.Types.RoutingProfileQueueConfigSummary
import Amazonka.Connect.Types.RoutingProfileQueueReference
import Amazonka.Connect.Types.RoutingProfileReference
import Amazonka.Connect.Types.RoutingProfileSearchCriteria
import Amazonka.Connect.Types.RoutingProfileSearchFilter
import Amazonka.Connect.Types.RoutingProfileSummary
import Amazonka.Connect.Types.S3Config
import Amazonka.Connect.Types.SecurityKey
import Amazonka.Connect.Types.SecurityProfile
import Amazonka.Connect.Types.SecurityProfileSearchCriteria
import Amazonka.Connect.Types.SecurityProfileSearchSummary
import Amazonka.Connect.Types.SecurityProfileSummary
import Amazonka.Connect.Types.SecurityProfilesSearchFilter
import Amazonka.Connect.Types.StringCondition
import Amazonka.Connect.Types.StringReference
import Amazonka.Connect.Types.TagCondition
import Amazonka.Connect.Types.TaskTemplateConstraints
import Amazonka.Connect.Types.TaskTemplateDefaultFieldValue
import Amazonka.Connect.Types.TaskTemplateDefaults
import Amazonka.Connect.Types.TaskTemplateField
import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import Amazonka.Connect.Types.TaskTemplateMetadata
import Amazonka.Connect.Types.TelephonyConfig
import Amazonka.Connect.Types.Threshold
import Amazonka.Connect.Types.TrafficDistributionGroup
import Amazonka.Connect.Types.TrafficDistributionGroupSummary
import Amazonka.Connect.Types.UrlReference
import Amazonka.Connect.Types.UseCase
import Amazonka.Connect.Types.User
import Amazonka.Connect.Types.UserData
import Amazonka.Connect.Types.UserDataFilters
import Amazonka.Connect.Types.UserIdentityInfo
import Amazonka.Connect.Types.UserIdentityInfoLite
import Amazonka.Connect.Types.UserPhoneConfig
import Amazonka.Connect.Types.UserQuickConnectConfig
import Amazonka.Connect.Types.UserReference
import Amazonka.Connect.Types.UserSearchCriteria
import Amazonka.Connect.Types.UserSearchFilter
import Amazonka.Connect.Types.UserSearchSummary
import Amazonka.Connect.Types.UserSummary
import Amazonka.Connect.Types.Vocabulary
import Amazonka.Connect.Types.VocabularySummary
import Amazonka.Connect.Types.VoiceRecordingConfiguration
import Amazonka.Connect.UntagResource
import Amazonka.Connect.UpdateAgentStatus
import Amazonka.Connect.UpdateContact
import Amazonka.Connect.UpdateContactAttributes
import Amazonka.Connect.UpdateContactFlowContent
import Amazonka.Connect.UpdateContactFlowMetadata
import Amazonka.Connect.UpdateContactFlowModuleContent
import Amazonka.Connect.UpdateContactFlowModuleMetadata
import Amazonka.Connect.UpdateContactFlowName
import Amazonka.Connect.UpdateContactSchedule
import Amazonka.Connect.UpdateHoursOfOperation
import Amazonka.Connect.UpdateInstanceAttribute
import Amazonka.Connect.UpdateInstanceStorageConfig
import Amazonka.Connect.UpdatePhoneNumber
import Amazonka.Connect.UpdateQueueHoursOfOperation
import Amazonka.Connect.UpdateQueueMaxContacts
import Amazonka.Connect.UpdateQueueName
import Amazonka.Connect.UpdateQueueOutboundCallerConfig
import Amazonka.Connect.UpdateQueueStatus
import Amazonka.Connect.UpdateQuickConnectConfig
import Amazonka.Connect.UpdateQuickConnectName
import Amazonka.Connect.UpdateRoutingProfileConcurrency
import Amazonka.Connect.UpdateRoutingProfileDefaultOutboundQueue
import Amazonka.Connect.UpdateRoutingProfileName
import Amazonka.Connect.UpdateRoutingProfileQueues
import Amazonka.Connect.UpdateSecurityProfile
import Amazonka.Connect.UpdateTaskTemplate
import Amazonka.Connect.UpdateTrafficDistribution
import Amazonka.Connect.UpdateUserHierarchy
import Amazonka.Connect.UpdateUserHierarchyGroupName
import Amazonka.Connect.UpdateUserHierarchyStructure
import Amazonka.Connect.UpdateUserIdentityInfo
import Amazonka.Connect.UpdateUserPhoneConfig
import Amazonka.Connect.UpdateUserRoutingProfile
import Amazonka.Connect.UpdateUserSecurityProfiles

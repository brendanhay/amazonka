{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Lens
  ( -- * Operations

    -- ** DescribeInstance
    describeInstance_instanceId,
    describeInstanceResponse_instance,
    describeInstanceResponse_httpStatus,

    -- ** ListSecurityProfiles
    listSecurityProfiles_nextToken,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_instanceId,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileSummaryList,
    listSecurityProfilesResponse_httpStatus,

    -- ** AssociateLexBot
    associateLexBot_instanceId,
    associateLexBot_lexBot,

    -- ** UpdateInstanceAttribute
    updateInstanceAttribute_instanceId,
    updateInstanceAttribute_attributeType,
    updateInstanceAttribute_value,

    -- ** UpdateQueueStatus
    updateQueueStatus_instanceId,
    updateQueueStatus_queueId,
    updateQueueStatus_status,

    -- ** UpdateRoutingProfileQueues
    updateRoutingProfileQueues_instanceId,
    updateRoutingProfileQueues_routingProfileId,
    updateRoutingProfileQueues_queueConfigs,

    -- ** DescribeQueue
    describeQueue_instanceId,
    describeQueue_queueId,
    describeQueueResponse_queue,
    describeQueueResponse_httpStatus,

    -- ** ListInstanceAttributes
    listInstanceAttributes_nextToken,
    listInstanceAttributes_maxResults,
    listInstanceAttributes_instanceId,
    listInstanceAttributesResponse_nextToken,
    listInstanceAttributesResponse_attributes,
    listInstanceAttributesResponse_httpStatus,

    -- ** UpdateAgentStatus
    updateAgentStatus_displayOrder,
    updateAgentStatus_state,
    updateAgentStatus_name,
    updateAgentStatus_resetOrderNumber,
    updateAgentStatus_description,
    updateAgentStatus_instanceId,
    updateAgentStatus_agentStatusId,

    -- ** DescribeInstanceStorageConfig
    describeInstanceStorageConfig_instanceId,
    describeInstanceStorageConfig_associationId,
    describeInstanceStorageConfig_resourceType,
    describeInstanceStorageConfigResponse_storageConfig,
    describeInstanceStorageConfigResponse_httpStatus,

    -- ** CreateQuickConnect
    createQuickConnect_description,
    createQuickConnect_tags,
    createQuickConnect_instanceId,
    createQuickConnect_name,
    createQuickConnect_quickConnectConfig,
    createQuickConnectResponse_quickConnectId,
    createQuickConnectResponse_quickConnectARN,
    createQuickConnectResponse_httpStatus,

    -- ** DescribeContactFlow
    describeContactFlow_instanceId,
    describeContactFlow_contactFlowId,
    describeContactFlowResponse_contactFlow,
    describeContactFlowResponse_httpStatus,

    -- ** UpdateUserHierarchy
    updateUserHierarchy_hierarchyGroupId,
    updateUserHierarchy_userId,
    updateUserHierarchy_instanceId,

    -- ** UpdateUserRoutingProfile
    updateUserRoutingProfile_routingProfileId,
    updateUserRoutingProfile_userId,
    updateUserRoutingProfile_instanceId,

    -- ** UpdateUserHierarchyGroupName
    updateUserHierarchyGroupName_name,
    updateUserHierarchyGroupName_hierarchyGroupId,
    updateUserHierarchyGroupName_instanceId,

    -- ** UpdateQueueHoursOfOperation
    updateQueueHoursOfOperation_instanceId,
    updateQueueHoursOfOperation_queueId,
    updateQueueHoursOfOperation_hoursOfOperationId,

    -- ** DescribeRoutingProfile
    describeRoutingProfile_instanceId,
    describeRoutingProfile_routingProfileId,
    describeRoutingProfileResponse_routingProfile,
    describeRoutingProfileResponse_httpStatus,

    -- ** DisassociateLexBot
    disassociateLexBot_instanceId,
    disassociateLexBot_botName,
    disassociateLexBot_lexRegion,

    -- ** DeleteQuickConnect
    deleteQuickConnect_instanceId,
    deleteQuickConnect_quickConnectId,

    -- ** StartOutboundVoiceContact
    startOutboundVoiceContact_answerMachineDetectionConfig,
    startOutboundVoiceContact_clientToken,
    startOutboundVoiceContact_trafficType,
    startOutboundVoiceContact_campaignId,
    startOutboundVoiceContact_queueId,
    startOutboundVoiceContact_attributes,
    startOutboundVoiceContact_sourcePhoneNumber,
    startOutboundVoiceContact_destinationPhoneNumber,
    startOutboundVoiceContact_contactFlowId,
    startOutboundVoiceContact_instanceId,
    startOutboundVoiceContactResponse_contactId,
    startOutboundVoiceContactResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetMetricData
    getMetricData_nextToken,
    getMetricData_groupings,
    getMetricData_maxResults,
    getMetricData_instanceId,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricData_filters,
    getMetricData_historicalMetrics,
    getMetricDataResponse_metricResults,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_httpStatus,

    -- ** StartContactRecording
    startContactRecording_instanceId,
    startContactRecording_contactId,
    startContactRecording_initialContactId,
    startContactRecording_voiceRecordingConfiguration,
    startContactRecordingResponse_httpStatus,

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

    -- ** AssociateBot
    associateBot_lexBot,
    associateBot_lexV2Bot,
    associateBot_instanceId,

    -- ** AssociateQueueQuickConnects
    associateQueueQuickConnects_instanceId,
    associateQueueQuickConnects_queueId,
    associateQueueQuickConnects_quickConnectIds,

    -- ** StartTaskContact
    startTaskContact_clientToken,
    startTaskContact_references,
    startTaskContact_previousContactId,
    startTaskContact_attributes,
    startTaskContact_description,
    startTaskContact_instanceId,
    startTaskContact_contactFlowId,
    startTaskContact_name,
    startTaskContactResponse_contactId,
    startTaskContactResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_instanceId,
    listUsersResponse_nextToken,
    listUsersResponse_userSummaryList,
    listUsersResponse_httpStatus,

    -- ** ListUserHierarchyGroups
    listUserHierarchyGroups_nextToken,
    listUserHierarchyGroups_maxResults,
    listUserHierarchyGroups_instanceId,
    listUserHierarchyGroupsResponse_nextToken,
    listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList,
    listUserHierarchyGroupsResponse_httpStatus,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_queueTypes,
    listQueues_maxResults,
    listQueues_instanceId,
    listQueuesResponse_nextToken,
    listQueuesResponse_queueSummaryList,
    listQueuesResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_instanceId,
    describeInstanceAttribute_attributeType,
    describeInstanceAttributeResponse_attribute,
    describeInstanceAttributeResponse_httpStatus,

    -- ** ListBots
    listBots_nextToken,
    listBots_maxResults,
    listBots_instanceId,
    listBots_lexVersion,
    listBotsResponse_nextToken,
    listBotsResponse_lexBots,
    listBotsResponse_httpStatus,

    -- ** UpdateQuickConnectConfig
    updateQuickConnectConfig_instanceId,
    updateQuickConnectConfig_quickConnectId,
    updateQuickConnectConfig_quickConnectConfig,

    -- ** DescribeAgentStatus
    describeAgentStatus_instanceId,
    describeAgentStatus_agentStatusId,
    describeAgentStatusResponse_agentStatus,
    describeAgentStatusResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_instanceId,

    -- ** DisassociateInstanceStorageConfig
    disassociateInstanceStorageConfig_instanceId,
    disassociateInstanceStorageConfig_associationId,
    disassociateInstanceStorageConfig_resourceType,

    -- ** CreateRoutingProfile
    createRoutingProfile_queueConfigs,
    createRoutingProfile_tags,
    createRoutingProfile_instanceId,
    createRoutingProfile_name,
    createRoutingProfile_description,
    createRoutingProfile_defaultOutboundQueueId,
    createRoutingProfile_mediaConcurrencies,
    createRoutingProfileResponse_routingProfileArn,
    createRoutingProfileResponse_routingProfileId,
    createRoutingProfileResponse_httpStatus,

    -- ** UpdateInstanceStorageConfig
    updateInstanceStorageConfig_instanceId,
    updateInstanceStorageConfig_associationId,
    updateInstanceStorageConfig_resourceType,
    updateInstanceStorageConfig_storageConfig,

    -- ** DisassociateQueueQuickConnects
    disassociateQueueQuickConnects_instanceId,
    disassociateQueueQuickConnects_queueId,
    disassociateQueueQuickConnects_quickConnectIds,

    -- ** CreateUseCase
    createUseCase_tags,
    createUseCase_instanceId,
    createUseCase_integrationAssociationId,
    createUseCase_useCaseType,
    createUseCaseResponse_useCaseArn,
    createUseCaseResponse_useCaseId,
    createUseCaseResponse_httpStatus,

    -- ** DisassociateBot
    disassociateBot_lexBot,
    disassociateBot_lexV2Bot,
    disassociateBot_instanceId,

    -- ** ListQueueQuickConnects
    listQueueQuickConnects_nextToken,
    listQueueQuickConnects_maxResults,
    listQueueQuickConnects_instanceId,
    listQueueQuickConnects_queueId,
    listQueueQuickConnectsResponse_quickConnectSummaryList,
    listQueueQuickConnectsResponse_nextToken,
    listQueueQuickConnectsResponse_httpStatus,

    -- ** GetCurrentMetricData
    getCurrentMetricData_nextToken,
    getCurrentMetricData_groupings,
    getCurrentMetricData_maxResults,
    getCurrentMetricData_instanceId,
    getCurrentMetricData_filters,
    getCurrentMetricData_currentMetrics,
    getCurrentMetricDataResponse_metricResults,
    getCurrentMetricDataResponse_dataSnapshotTime,
    getCurrentMetricDataResponse_nextToken,
    getCurrentMetricDataResponse_httpStatus,

    -- ** CreateContactFlow
    createContactFlow_description,
    createContactFlow_tags,
    createContactFlow_instanceId,
    createContactFlow_name,
    createContactFlow_type,
    createContactFlow_content,
    createContactFlowResponse_contactFlowArn,
    createContactFlowResponse_contactFlowId,
    createContactFlowResponse_httpStatus,

    -- ** ListRoutingProfiles
    listRoutingProfiles_nextToken,
    listRoutingProfiles_maxResults,
    listRoutingProfiles_instanceId,
    listRoutingProfilesResponse_routingProfileSummaryList,
    listRoutingProfilesResponse_nextToken,
    listRoutingProfilesResponse_httpStatus,

    -- ** DeleteIntegrationAssociation
    deleteIntegrationAssociation_instanceId,
    deleteIntegrationAssociation_integrationAssociationId,

    -- ** DeleteHoursOfOperation
    deleteHoursOfOperation_instanceId,
    deleteHoursOfOperation_hoursOfOperationId,

    -- ** UpdateUserPhoneConfig
    updateUserPhoneConfig_phoneConfig,
    updateUserPhoneConfig_userId,
    updateUserPhoneConfig_instanceId,

    -- ** UpdateHoursOfOperation
    updateHoursOfOperation_config,
    updateHoursOfOperation_name,
    updateHoursOfOperation_timeZone,
    updateHoursOfOperation_description,
    updateHoursOfOperation_instanceId,
    updateHoursOfOperation_hoursOfOperationId,

    -- ** ListApprovedOrigins
    listApprovedOrigins_nextToken,
    listApprovedOrigins_maxResults,
    listApprovedOrigins_instanceId,
    listApprovedOriginsResponse_nextToken,
    listApprovedOriginsResponse_origins,
    listApprovedOriginsResponse_httpStatus,

    -- ** DescribeUserHierarchyStructure
    describeUserHierarchyStructure_instanceId,
    describeUserHierarchyStructureResponse_hierarchyStructure,
    describeUserHierarchyStructureResponse_httpStatus,

    -- ** ListPhoneNumbers
    listPhoneNumbers_phoneNumberTypes,
    listPhoneNumbers_phoneNumberCountryCodes,
    listPhoneNumbers_nextToken,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_instanceId,
    listPhoneNumbersResponse_phoneNumberSummaryList,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_httpStatus,

    -- ** UpdateContactAttributes
    updateContactAttributes_initialContactId,
    updateContactAttributes_instanceId,
    updateContactAttributes_attributes,
    updateContactAttributesResponse_httpStatus,

    -- ** ListUseCases
    listUseCases_nextToken,
    listUseCases_maxResults,
    listUseCases_instanceId,
    listUseCases_integrationAssociationId,
    listUseCasesResponse_useCaseSummaryList,
    listUseCasesResponse_nextToken,
    listUseCasesResponse_httpStatus,

    -- ** StartChatContact
    startChatContact_clientToken,
    startChatContact_attributes,
    startChatContact_initialMessage,
    startChatContact_instanceId,
    startChatContact_contactFlowId,
    startChatContact_participantDetails,
    startChatContactResponse_participantToken,
    startChatContactResponse_participantId,
    startChatContactResponse_contactId,
    startChatContactResponse_httpStatus,

    -- ** DeleteUseCase
    deleteUseCase_instanceId,
    deleteUseCase_integrationAssociationId,
    deleteUseCase_useCaseId,

    -- ** UpdateUserSecurityProfiles
    updateUserSecurityProfiles_securityProfileIds,
    updateUserSecurityProfiles_userId,
    updateUserSecurityProfiles_instanceId,

    -- ** GetContactAttributes
    getContactAttributes_instanceId,
    getContactAttributes_initialContactId,
    getContactAttributesResponse_attributes,
    getContactAttributesResponse_httpStatus,

    -- ** ListLambdaFunctions
    listLambdaFunctions_nextToken,
    listLambdaFunctions_maxResults,
    listLambdaFunctions_instanceId,
    listLambdaFunctionsResponse_lambdaFunctions,
    listLambdaFunctionsResponse_nextToken,
    listLambdaFunctionsResponse_httpStatus,

    -- ** DescribeUserHierarchyGroup
    describeUserHierarchyGroup_hierarchyGroupId,
    describeUserHierarchyGroup_instanceId,
    describeUserHierarchyGroupResponse_hierarchyGroup,
    describeUserHierarchyGroupResponse_httpStatus,

    -- ** DescribeUser
    describeUser_userId,
    describeUser_instanceId,
    describeUserResponse_user,
    describeUserResponse_httpStatus,

    -- ** ResumeContactRecording
    resumeContactRecording_instanceId,
    resumeContactRecording_contactId,
    resumeContactRecording_initialContactId,
    resumeContactRecordingResponse_httpStatus,

    -- ** UpdateContactFlowName
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- ** SuspendContactRecording
    suspendContactRecording_instanceId,
    suspendContactRecording_contactId,
    suspendContactRecording_initialContactId,
    suspendContactRecordingResponse_httpStatus,

    -- ** UpdateQueueName
    updateQueueName_name,
    updateQueueName_description,
    updateQueueName_instanceId,
    updateQueueName_queueId,

    -- ** UpdateQueueMaxContacts
    updateQueueMaxContacts_maxContacts,
    updateQueueMaxContacts_instanceId,
    updateQueueMaxContacts_queueId,

    -- ** ListRoutingProfileQueues
    listRoutingProfileQueues_nextToken,
    listRoutingProfileQueues_maxResults,
    listRoutingProfileQueues_instanceId,
    listRoutingProfileQueues_routingProfileId,
    listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList,
    listRoutingProfileQueuesResponse_nextToken,
    listRoutingProfileQueuesResponse_httpStatus,

    -- ** DisassociateRoutingProfileQueues
    disassociateRoutingProfileQueues_instanceId,
    disassociateRoutingProfileQueues_routingProfileId,
    disassociateRoutingProfileQueues_queueReferences,

    -- ** DisassociateLambdaFunction
    disassociateLambdaFunction_instanceId,
    disassociateLambdaFunction_functionArn,

    -- ** UpdateContactFlowContent
    updateContactFlowContent_instanceId,
    updateContactFlowContent_contactFlowId,
    updateContactFlowContent_content,

    -- ** UpdateUserHierarchyStructure
    updateUserHierarchyStructure_hierarchyStructure,
    updateUserHierarchyStructure_instanceId,

    -- ** DescribeHoursOfOperation
    describeHoursOfOperation_instanceId,
    describeHoursOfOperation_hoursOfOperationId,
    describeHoursOfOperationResponse_hoursOfOperation,
    describeHoursOfOperationResponse_httpStatus,

    -- ** ListQuickConnects
    listQuickConnects_quickConnectTypes,
    listQuickConnects_nextToken,
    listQuickConnects_maxResults,
    listQuickConnects_instanceId,
    listQuickConnectsResponse_quickConnectSummaryList,
    listQuickConnectsResponse_nextToken,
    listQuickConnectsResponse_httpStatus,

    -- ** CreateUserHierarchyGroup
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_name,
    createUserHierarchyGroup_instanceId,
    createUserHierarchyGroupResponse_hierarchyGroupArn,
    createUserHierarchyGroupResponse_hierarchyGroupId,
    createUserHierarchyGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_directoryUserId,
    createUser_identityInfo,
    createUser_password,
    createUser_hierarchyGroupId,
    createUser_tags,
    createUser_username,
    createUser_phoneConfig,
    createUser_securityProfileIds,
    createUser_routingProfileId,
    createUser_instanceId,
    createUserResponse_userId,
    createUserResponse_userArn,
    createUserResponse_httpStatus,

    -- ** CreateQueue
    createQueue_maxContacts,
    createQueue_quickConnectIds,
    createQueue_outboundCallerConfig,
    createQueue_description,
    createQueue_tags,
    createQueue_instanceId,
    createQueue_name,
    createQueue_hoursOfOperationId,
    createQueueResponse_queueArn,
    createQueueResponse_queueId,
    createQueueResponse_httpStatus,

    -- ** UpdateQuickConnectName
    updateQuickConnectName_name,
    updateQuickConnectName_description,
    updateQuickConnectName_instanceId,
    updateQuickConnectName_quickConnectId,

    -- ** ListPrompts
    listPrompts_nextToken,
    listPrompts_maxResults,
    listPrompts_instanceId,
    listPromptsResponse_promptSummaryList,
    listPromptsResponse_nextToken,
    listPromptsResponse_httpStatus,

    -- ** AssociateSecurityKey
    associateSecurityKey_instanceId,
    associateSecurityKey_key,
    associateSecurityKeyResponse_associationId,
    associateSecurityKeyResponse_httpStatus,

    -- ** StopContactRecording
    stopContactRecording_instanceId,
    stopContactRecording_contactId,
    stopContactRecording_initialContactId,
    stopContactRecordingResponse_httpStatus,

    -- ** DisassociateApprovedOrigin
    disassociateApprovedOrigin_instanceId,
    disassociateApprovedOrigin_origin,

    -- ** ListSecurityKeys
    listSecurityKeys_nextToken,
    listSecurityKeys_maxResults,
    listSecurityKeys_instanceId,
    listSecurityKeysResponse_nextToken,
    listSecurityKeysResponse_securityKeys,
    listSecurityKeysResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_instanceId,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_httpStatus,

    -- ** StopContact
    stopContact_contactId,
    stopContact_instanceId,
    stopContactResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_instanceId,
    deleteUser_userId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UpdateUserIdentityInfo
    updateUserIdentityInfo_identityInfo,
    updateUserIdentityInfo_userId,
    updateUserIdentityInfo_instanceId,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstancesResponse_instanceSummaryList,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,

    -- ** DeleteUserHierarchyGroup
    deleteUserHierarchyGroup_hierarchyGroupId,
    deleteUserHierarchyGroup_instanceId,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    updateRoutingProfileDefaultOutboundQueue_instanceId,
    updateRoutingProfileDefaultOutboundQueue_routingProfileId,
    updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId,

    -- ** UpdateQueueOutboundCallerConfig
    updateQueueOutboundCallerConfig_instanceId,
    updateQueueOutboundCallerConfig_queueId,
    updateQueueOutboundCallerConfig_outboundCallerConfig,

    -- ** ListContactFlows
    listContactFlows_contactFlowTypes,
    listContactFlows_nextToken,
    listContactFlows_maxResults,
    listContactFlows_instanceId,
    listContactFlowsResponse_contactFlowSummaryList,
    listContactFlowsResponse_nextToken,
    listContactFlowsResponse_httpStatus,

    -- ** CreateIntegrationAssociation
    createIntegrationAssociation_sourceType,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_tags,
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** AssociateApprovedOrigin
    associateApprovedOrigin_instanceId,
    associateApprovedOrigin_origin,

    -- ** CreateHoursOfOperation
    createHoursOfOperation_description,
    createHoursOfOperation_tags,
    createHoursOfOperation_instanceId,
    createHoursOfOperation_name,
    createHoursOfOperation_timeZone,
    createHoursOfOperation_config,
    createHoursOfOperationResponse_hoursOfOperationArn,
    createHoursOfOperationResponse_hoursOfOperationId,
    createHoursOfOperationResponse_httpStatus,

    -- ** DisassociateSecurityKey
    disassociateSecurityKey_instanceId,
    disassociateSecurityKey_associationId,

    -- ** UpdateRoutingProfileConcurrency
    updateRoutingProfileConcurrency_instanceId,
    updateRoutingProfileConcurrency_routingProfileId,
    updateRoutingProfileConcurrency_mediaConcurrencies,

    -- ** ListInstanceStorageConfigs
    listInstanceStorageConfigs_nextToken,
    listInstanceStorageConfigs_maxResults,
    listInstanceStorageConfigs_instanceId,
    listInstanceStorageConfigs_resourceType,
    listInstanceStorageConfigsResponse_storageConfigs,
    listInstanceStorageConfigsResponse_nextToken,
    listInstanceStorageConfigsResponse_httpStatus,

    -- ** DescribeQuickConnect
    describeQuickConnect_instanceId,
    describeQuickConnect_quickConnectId,
    describeQuickConnectResponse_quickConnect,
    describeQuickConnectResponse_httpStatus,

    -- ** AssociateInstanceStorageConfig
    associateInstanceStorageConfig_instanceId,
    associateInstanceStorageConfig_resourceType,
    associateInstanceStorageConfig_storageConfig,
    associateInstanceStorageConfigResponse_associationId,
    associateInstanceStorageConfigResponse_httpStatus,

    -- ** ListHoursOfOperations
    listHoursOfOperations_nextToken,
    listHoursOfOperations_maxResults,
    listHoursOfOperations_instanceId,
    listHoursOfOperationsResponse_nextToken,
    listHoursOfOperationsResponse_hoursOfOperationSummaryList,
    listHoursOfOperationsResponse_httpStatus,

    -- ** ListIntegrationAssociations
    listIntegrationAssociations_nextToken,
    listIntegrationAssociations_integrationType,
    listIntegrationAssociations_maxResults,
    listIntegrationAssociations_instanceId,
    listIntegrationAssociationsResponse_nextToken,
    listIntegrationAssociationsResponse_integrationAssociationSummaryList,
    listIntegrationAssociationsResponse_httpStatus,

    -- ** CreateAgentStatus
    createAgentStatus_displayOrder,
    createAgentStatus_description,
    createAgentStatus_tags,
    createAgentStatus_instanceId,
    createAgentStatus_name,
    createAgentStatus_state,
    createAgentStatusResponse_agentStatusId,
    createAgentStatusResponse_agentStatusARN,
    createAgentStatusResponse_httpStatus,

    -- ** UpdateRoutingProfileName
    updateRoutingProfileName_name,
    updateRoutingProfileName_description,
    updateRoutingProfileName_instanceId,
    updateRoutingProfileName_routingProfileId,

    -- ** ListLexBots
    listLexBots_nextToken,
    listLexBots_maxResults,
    listLexBots_instanceId,
    listLexBotsResponse_nextToken,
    listLexBotsResponse_lexBots,
    listLexBotsResponse_httpStatus,

    -- ** ListAgentStatuses
    listAgentStatuses_agentStatusTypes,
    listAgentStatuses_nextToken,
    listAgentStatuses_maxResults,
    listAgentStatuses_instanceId,
    listAgentStatusesResponse_nextToken,
    listAgentStatusesResponse_agentStatusSummaryList,
    listAgentStatusesResponse_httpStatus,

    -- ** AssociateLambdaFunction
    associateLambdaFunction_instanceId,
    associateLambdaFunction_functionArn,

    -- ** AssociateRoutingProfileQueues
    associateRoutingProfileQueues_instanceId,
    associateRoutingProfileQueues_routingProfileId,
    associateRoutingProfileQueues_queueConfigs,

    -- * Types

    -- ** AgentStatus
    agentStatus_displayOrder,
    agentStatus_state,
    agentStatus_name,
    agentStatus_agentStatusId,
    agentStatus_type,
    agentStatus_agentStatusARN,
    agentStatus_description,
    agentStatus_tags,

    -- ** AgentStatusSummary
    agentStatusSummary_arn,
    agentStatusSummary_name,
    agentStatusSummary_id,
    agentStatusSummary_type,

    -- ** AnswerMachineDetectionConfig
    answerMachineDetectionConfig_enableAnswerMachineDetection,
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,

    -- ** Attribute
    attribute_value,
    attribute_attributeType,

    -- ** ChatMessage
    chatMessage_contentType,
    chatMessage_content,

    -- ** ContactFlow
    contactFlow_arn,
    contactFlow_content,
    contactFlow_name,
    contactFlow_id,
    contactFlow_type,
    contactFlow_description,
    contactFlow_tags,

    -- ** ContactFlowSummary
    contactFlowSummary_arn,
    contactFlowSummary_name,
    contactFlowSummary_contactFlowType,
    contactFlowSummary_id,

    -- ** Credentials
    credentials_accessTokenExpiration,
    credentials_accessToken,
    credentials_refreshToken,
    credentials_refreshTokenExpiration,

    -- ** CurrentMetric
    currentMetric_name,
    currentMetric_unit,

    -- ** CurrentMetricData
    currentMetricData_value,
    currentMetricData_metric,

    -- ** CurrentMetricResult
    currentMetricResult_collections,
    currentMetricResult_dimensions,

    -- ** Dimensions
    dimensions_channel,
    dimensions_queue,

    -- ** EncryptionConfig
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- ** Filters
    filters_queues,
    filters_channels,

    -- ** HierarchyGroup
    hierarchyGroup_arn,
    hierarchyGroup_name,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,

    -- ** HierarchyGroupSummary
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_name,
    hierarchyGroupSummary_id,

    -- ** HierarchyLevel
    hierarchyLevel_arn,
    hierarchyLevel_name,
    hierarchyLevel_id,

    -- ** HierarchyLevelUpdate
    hierarchyLevelUpdate_name,

    -- ** HierarchyPath
    hierarchyPath_levelFive,
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelTwo,
    hierarchyPath_levelOne,

    -- ** HierarchyStructure
    hierarchyStructure_levelFive,
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelTwo,
    hierarchyStructure_levelOne,

    -- ** HierarchyStructureUpdate
    hierarchyStructureUpdate_levelFive,
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelTwo,
    hierarchyStructureUpdate_levelOne,

    -- ** HistoricalMetric
    historicalMetric_name,
    historicalMetric_threshold,
    historicalMetric_unit,
    historicalMetric_statistic,

    -- ** HistoricalMetricData
    historicalMetricData_value,
    historicalMetricData_metric,

    -- ** HistoricalMetricResult
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- ** HoursOfOperation
    hoursOfOperation_config,
    hoursOfOperation_name,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_hoursOfOperationId,
    hoursOfOperation_timeZone,
    hoursOfOperation_description,
    hoursOfOperation_tags,

    -- ** HoursOfOperationConfig
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- ** HoursOfOperationSummary
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_name,
    hoursOfOperationSummary_id,

    -- ** HoursOfOperationTimeSlice
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- ** Instance
    instance_arn,
    instance_createdTime,
    instance_outboundCallsEnabled,
    instance_inboundCallsEnabled,
    instance_instanceAlias,
    instance_id,
    instance_instanceStatus,
    instance_identityManagementType,
    instance_statusReason,
    instance_serviceRole,

    -- ** InstanceStatusReason
    instanceStatusReason_message,

    -- ** InstanceStorageConfig
    instanceStorageConfig_associationId,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_storageType,

    -- ** InstanceSummary
    instanceSummary_arn,
    instanceSummary_createdTime,
    instanceSummary_outboundCallsEnabled,
    instanceSummary_inboundCallsEnabled,
    instanceSummary_instanceAlias,
    instanceSummary_id,
    instanceSummary_instanceStatus,
    instanceSummary_identityManagementType,
    instanceSummary_serviceRole,

    -- ** IntegrationAssociationSummary
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_sourceType,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_integrationAssociationId,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_integrationType,

    -- ** KinesisFirehoseConfig
    kinesisFirehoseConfig_firehoseArn,

    -- ** KinesisStreamConfig
    kinesisStreamConfig_streamArn,

    -- ** KinesisVideoStreamConfig
    kinesisVideoStreamConfig_prefix,
    kinesisVideoStreamConfig_retentionPeriodHours,
    kinesisVideoStreamConfig_encryptionConfig,

    -- ** LexBot
    lexBot_lexRegion,
    lexBot_name,

    -- ** LexBotConfig
    lexBotConfig_lexBot,
    lexBotConfig_lexV2Bot,

    -- ** LexV2Bot
    lexV2Bot_aliasArn,

    -- ** MediaConcurrency
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- ** OutboundCallerConfig
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundCallerIdName,
    outboundCallerConfig_outboundFlowId,

    -- ** ParticipantDetails
    participantDetails_displayName,

    -- ** PhoneNumberQuickConnectConfig
    phoneNumberQuickConnectConfig_phoneNumber,

    -- ** PhoneNumberSummary
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_arn,
    phoneNumberSummary_phoneNumber,
    phoneNumberSummary_phoneNumberCountryCode,
    phoneNumberSummary_id,

    -- ** PromptSummary
    promptSummary_arn,
    promptSummary_name,
    promptSummary_id,

    -- ** Queue
    queue_status,
    queue_queueArn,
    queue_queueId,
    queue_maxContacts,
    queue_name,
    queue_hoursOfOperationId,
    queue_outboundCallerConfig,
    queue_description,
    queue_tags,

    -- ** QueueQuickConnectConfig
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- ** QueueReference
    queueReference_arn,
    queueReference_id,

    -- ** QueueSummary
    queueSummary_arn,
    queueSummary_name,
    queueSummary_id,
    queueSummary_queueType,

    -- ** QuickConnect
    quickConnect_name,
    quickConnect_quickConnectId,
    quickConnect_description,
    quickConnect_quickConnectARN,
    quickConnect_tags,
    quickConnect_quickConnectConfig,

    -- ** QuickConnectConfig
    quickConnectConfig_queueConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_quickConnectType,

    -- ** QuickConnectSummary
    quickConnectSummary_arn,
    quickConnectSummary_quickConnectType,
    quickConnectSummary_name,
    quickConnectSummary_id,

    -- ** Reference
    reference_value,
    reference_type,

    -- ** RoutingProfile
    routingProfile_instanceId,
    routingProfile_routingProfileArn,
    routingProfile_routingProfileId,
    routingProfile_defaultOutboundQueueId,
    routingProfile_name,
    routingProfile_mediaConcurrencies,
    routingProfile_description,
    routingProfile_tags,

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

    -- ** RoutingProfileSummary
    routingProfileSummary_arn,
    routingProfileSummary_name,
    routingProfileSummary_id,

    -- ** S3Config
    s3Config_encryptionConfig,
    s3Config_bucketName,
    s3Config_bucketPrefix,

    -- ** SecurityKey
    securityKey_creationTime,
    securityKey_associationId,
    securityKey_key,

    -- ** SecurityProfileSummary
    securityProfileSummary_arn,
    securityProfileSummary_name,
    securityProfileSummary_id,

    -- ** Threshold
    threshold_thresholdValue,
    threshold_comparison,

    -- ** UseCase
    useCase_useCaseType,
    useCase_useCaseArn,
    useCase_useCaseId,

    -- ** User
    user_routingProfileId,
    user_directoryUserId,
    user_arn,
    user_identityInfo,
    user_securityProfileIds,
    user_username,
    user_id,
    user_hierarchyGroupId,
    user_phoneConfig,
    user_tags,

    -- ** UserIdentityInfo
    userIdentityInfo_email,
    userIdentityInfo_lastName,
    userIdentityInfo_firstName,

    -- ** UserPhoneConfig
    userPhoneConfig_autoAccept,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_phoneType,

    -- ** UserQuickConnectConfig
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- ** UserSummary
    userSummary_arn,
    userSummary_username,
    userSummary_id,

    -- ** VoiceRecordingConfiguration
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Network.AWS.Connect.AssociateApprovedOrigin
import Network.AWS.Connect.AssociateBot
import Network.AWS.Connect.AssociateInstanceStorageConfig
import Network.AWS.Connect.AssociateLambdaFunction
import Network.AWS.Connect.AssociateLexBot
import Network.AWS.Connect.AssociateQueueQuickConnects
import Network.AWS.Connect.AssociateRoutingProfileQueues
import Network.AWS.Connect.AssociateSecurityKey
import Network.AWS.Connect.CreateAgentStatus
import Network.AWS.Connect.CreateContactFlow
import Network.AWS.Connect.CreateHoursOfOperation
import Network.AWS.Connect.CreateInstance
import Network.AWS.Connect.CreateIntegrationAssociation
import Network.AWS.Connect.CreateQueue
import Network.AWS.Connect.CreateQuickConnect
import Network.AWS.Connect.CreateRoutingProfile
import Network.AWS.Connect.CreateUseCase
import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.CreateUserHierarchyGroup
import Network.AWS.Connect.DeleteHoursOfOperation
import Network.AWS.Connect.DeleteInstance
import Network.AWS.Connect.DeleteIntegrationAssociation
import Network.AWS.Connect.DeleteQuickConnect
import Network.AWS.Connect.DeleteUseCase
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DeleteUserHierarchyGroup
import Network.AWS.Connect.DescribeAgentStatus
import Network.AWS.Connect.DescribeContactFlow
import Network.AWS.Connect.DescribeHoursOfOperation
import Network.AWS.Connect.DescribeInstance
import Network.AWS.Connect.DescribeInstanceAttribute
import Network.AWS.Connect.DescribeInstanceStorageConfig
import Network.AWS.Connect.DescribeQueue
import Network.AWS.Connect.DescribeQuickConnect
import Network.AWS.Connect.DescribeRoutingProfile
import Network.AWS.Connect.DescribeUser
import Network.AWS.Connect.DescribeUserHierarchyGroup
import Network.AWS.Connect.DescribeUserHierarchyStructure
import Network.AWS.Connect.DisassociateApprovedOrigin
import Network.AWS.Connect.DisassociateBot
import Network.AWS.Connect.DisassociateInstanceStorageConfig
import Network.AWS.Connect.DisassociateLambdaFunction
import Network.AWS.Connect.DisassociateLexBot
import Network.AWS.Connect.DisassociateQueueQuickConnects
import Network.AWS.Connect.DisassociateRoutingProfileQueues
import Network.AWS.Connect.DisassociateSecurityKey
import Network.AWS.Connect.GetContactAttributes
import Network.AWS.Connect.GetCurrentMetricData
import Network.AWS.Connect.GetFederationToken
import Network.AWS.Connect.GetMetricData
import Network.AWS.Connect.ListAgentStatuses
import Network.AWS.Connect.ListApprovedOrigins
import Network.AWS.Connect.ListBots
import Network.AWS.Connect.ListContactFlows
import Network.AWS.Connect.ListHoursOfOperations
import Network.AWS.Connect.ListInstanceAttributes
import Network.AWS.Connect.ListInstanceStorageConfigs
import Network.AWS.Connect.ListInstances
import Network.AWS.Connect.ListIntegrationAssociations
import Network.AWS.Connect.ListLambdaFunctions
import Network.AWS.Connect.ListLexBots
import Network.AWS.Connect.ListPhoneNumbers
import Network.AWS.Connect.ListPrompts
import Network.AWS.Connect.ListQueueQuickConnects
import Network.AWS.Connect.ListQueues
import Network.AWS.Connect.ListQuickConnects
import Network.AWS.Connect.ListRoutingProfileQueues
import Network.AWS.Connect.ListRoutingProfiles
import Network.AWS.Connect.ListSecurityKeys
import Network.AWS.Connect.ListSecurityProfiles
import Network.AWS.Connect.ListTagsForResource
import Network.AWS.Connect.ListUseCases
import Network.AWS.Connect.ListUserHierarchyGroups
import Network.AWS.Connect.ListUsers
import Network.AWS.Connect.ResumeContactRecording
import Network.AWS.Connect.StartChatContact
import Network.AWS.Connect.StartContactRecording
import Network.AWS.Connect.StartOutboundVoiceContact
import Network.AWS.Connect.StartTaskContact
import Network.AWS.Connect.StopContact
import Network.AWS.Connect.StopContactRecording
import Network.AWS.Connect.SuspendContactRecording
import Network.AWS.Connect.TagResource
import Network.AWS.Connect.Types.AgentStatus
import Network.AWS.Connect.Types.AgentStatusSummary
import Network.AWS.Connect.Types.AnswerMachineDetectionConfig
import Network.AWS.Connect.Types.Attribute
import Network.AWS.Connect.Types.ChatMessage
import Network.AWS.Connect.Types.ContactFlow
import Network.AWS.Connect.Types.ContactFlowSummary
import Network.AWS.Connect.Types.Credentials
import Network.AWS.Connect.Types.CurrentMetric
import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.CurrentMetricResult
import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.EncryptionConfig
import Network.AWS.Connect.Types.Filters
import Network.AWS.Connect.Types.HierarchyGroup
import Network.AWS.Connect.Types.HierarchyGroupSummary
import Network.AWS.Connect.Types.HierarchyLevel
import Network.AWS.Connect.Types.HierarchyLevelUpdate
import Network.AWS.Connect.Types.HierarchyPath
import Network.AWS.Connect.Types.HierarchyStructure
import Network.AWS.Connect.Types.HierarchyStructureUpdate
import Network.AWS.Connect.Types.HistoricalMetric
import Network.AWS.Connect.Types.HistoricalMetricData
import Network.AWS.Connect.Types.HistoricalMetricResult
import Network.AWS.Connect.Types.HoursOfOperation
import Network.AWS.Connect.Types.HoursOfOperationConfig
import Network.AWS.Connect.Types.HoursOfOperationSummary
import Network.AWS.Connect.Types.HoursOfOperationTimeSlice
import Network.AWS.Connect.Types.Instance
import Network.AWS.Connect.Types.InstanceStatusReason
import Network.AWS.Connect.Types.InstanceStorageConfig
import Network.AWS.Connect.Types.InstanceSummary
import Network.AWS.Connect.Types.IntegrationAssociationSummary
import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.LexBot
import Network.AWS.Connect.Types.LexBotConfig
import Network.AWS.Connect.Types.LexV2Bot
import Network.AWS.Connect.Types.MediaConcurrency
import Network.AWS.Connect.Types.OutboundCallerConfig
import Network.AWS.Connect.Types.ParticipantDetails
import Network.AWS.Connect.Types.PhoneNumberQuickConnectConfig
import Network.AWS.Connect.Types.PhoneNumberSummary
import Network.AWS.Connect.Types.PromptSummary
import Network.AWS.Connect.Types.Queue
import Network.AWS.Connect.Types.QueueQuickConnectConfig
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Connect.Types.QueueSummary
import Network.AWS.Connect.Types.QuickConnect
import Network.AWS.Connect.Types.QuickConnectConfig
import Network.AWS.Connect.Types.QuickConnectSummary
import Network.AWS.Connect.Types.Reference
import Network.AWS.Connect.Types.RoutingProfile
import Network.AWS.Connect.Types.RoutingProfileQueueConfig
import Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
import Network.AWS.Connect.Types.RoutingProfileQueueReference
import Network.AWS.Connect.Types.RoutingProfileSummary
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.SecurityKey
import Network.AWS.Connect.Types.SecurityProfileSummary
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.UseCase
import Network.AWS.Connect.Types.User
import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Connect.Types.UserQuickConnectConfig
import Network.AWS.Connect.Types.UserSummary
import Network.AWS.Connect.Types.VoiceRecordingConfiguration
import Network.AWS.Connect.UntagResource
import Network.AWS.Connect.UpdateAgentStatus
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateContactFlowContent
import Network.AWS.Connect.UpdateContactFlowName
import Network.AWS.Connect.UpdateHoursOfOperation
import Network.AWS.Connect.UpdateInstanceAttribute
import Network.AWS.Connect.UpdateInstanceStorageConfig
import Network.AWS.Connect.UpdateQueueHoursOfOperation
import Network.AWS.Connect.UpdateQueueMaxContacts
import Network.AWS.Connect.UpdateQueueName
import Network.AWS.Connect.UpdateQueueOutboundCallerConfig
import Network.AWS.Connect.UpdateQueueStatus
import Network.AWS.Connect.UpdateQuickConnectConfig
import Network.AWS.Connect.UpdateQuickConnectName
import Network.AWS.Connect.UpdateRoutingProfileConcurrency
import Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
import Network.AWS.Connect.UpdateRoutingProfileName
import Network.AWS.Connect.UpdateRoutingProfileQueues
import Network.AWS.Connect.UpdateUserHierarchy
import Network.AWS.Connect.UpdateUserHierarchyGroupName
import Network.AWS.Connect.UpdateUserHierarchyStructure
import Network.AWS.Connect.UpdateUserIdentityInfo
import Network.AWS.Connect.UpdateUserPhoneConfig
import Network.AWS.Connect.UpdateUserRoutingProfile
import Network.AWS.Connect.UpdateUserSecurityProfiles

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Connect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_name,
    createUserHierarchyGroup_instanceId,
    createUserHierarchyGroupResponse_hierarchyGroupId,
    createUserHierarchyGroupResponse_hierarchyGroupArn,
    createUserHierarchyGroupResponse_httpStatus,

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

    -- ** DescribeAgentStatus
    describeAgentStatus_instanceId,
    describeAgentStatus_agentStatusId,
    describeAgentStatusResponse_agentStatus,
    describeAgentStatusResponse_httpStatus,

    -- ** DescribeContactFlow
    describeContactFlow_instanceId,
    describeContactFlow_contactFlowId,
    describeContactFlowResponse_contactFlow,
    describeContactFlowResponse_httpStatus,

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

    -- ** GetFederationToken
    getFederationToken_instanceId,
    getFederationTokenResponse_credentials,
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

    -- ** ListContactFlows
    listContactFlows_nextToken,
    listContactFlows_maxResults,
    listContactFlows_contactFlowTypes,
    listContactFlows_instanceId,
    listContactFlowsResponse_nextToken,
    listContactFlowsResponse_contactFlowSummaryList,
    listContactFlowsResponse_httpStatus,

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

    -- ** ResumeContactRecording
    resumeContactRecording_instanceId,
    resumeContactRecording_contactId,
    resumeContactRecording_initialContactId,
    resumeContactRecordingResponse_httpStatus,

    -- ** StartChatContact
    startChatContact_clientToken,
    startChatContact_initialMessage,
    startChatContact_attributes,
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
    startTaskContact_description,
    startTaskContact_references,
    startTaskContact_attributes,
    startTaskContact_previousContactId,
    startTaskContact_instanceId,
    startTaskContact_contactFlowId,
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

    -- ** SuspendContactRecording
    suspendContactRecording_instanceId,
    suspendContactRecording_contactId,
    suspendContactRecording_initialContactId,
    suspendContactRecordingResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

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

    -- ** UpdateContactAttributes
    updateContactAttributes_initialContactId,
    updateContactAttributes_instanceId,
    updateContactAttributes_attributes,
    updateContactAttributesResponse_httpStatus,

    -- ** UpdateContactFlowContent
    updateContactFlowContent_instanceId,
    updateContactFlowContent_contactFlowId,
    updateContactFlowContent_content,

    -- ** UpdateContactFlowName
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

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

    -- ** AgentStatus
    agentStatus_tags,
    agentStatus_name,
    agentStatus_type,
    agentStatus_displayOrder,
    agentStatus_agentStatusId,
    agentStatus_state,
    agentStatus_description,
    agentStatus_agentStatusARN,

    -- ** AgentStatusSummary
    agentStatusSummary_name,
    agentStatusSummary_type,
    agentStatusSummary_arn,
    agentStatusSummary_id,

    -- ** AnswerMachineDetectionConfig
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,
    answerMachineDetectionConfig_enableAnswerMachineDetection,

    -- ** Attribute
    attribute_attributeType,
    attribute_value,

    -- ** ChatMessage
    chatMessage_contentType,
    chatMessage_content,

    -- ** ContactFlow
    contactFlow_tags,
    contactFlow_name,
    contactFlow_type,
    contactFlow_arn,
    contactFlow_id,
    contactFlow_description,
    contactFlow_content,

    -- ** ContactFlowSummary
    contactFlowSummary_name,
    contactFlowSummary_arn,
    contactFlowSummary_id,
    contactFlowSummary_contactFlowType,

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

    -- ** Dimensions
    dimensions_channel,
    dimensions_queue,

    -- ** EncryptionConfig
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- ** Filters
    filters_channels,
    filters_queues,

    -- ** HierarchyGroup
    hierarchyGroup_name,
    hierarchyGroup_arn,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,

    -- ** HierarchyGroupSummary
    hierarchyGroupSummary_name,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,

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

    -- ** MediaConcurrency
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- ** OutboundCallerConfig
    outboundCallerConfig_outboundFlowId,
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundCallerIdName,

    -- ** ParticipantDetails
    participantDetails_displayName,

    -- ** PhoneNumberQuickConnectConfig
    phoneNumberQuickConnectConfig_phoneNumber,

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

    -- ** QueueQuickConnectConfig
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- ** QueueReference
    queueReference_arn,
    queueReference_id,

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

    -- ** Reference
    reference_value,
    reference_type,

    -- ** RoutingProfile
    routingProfile_tags,
    routingProfile_name,
    routingProfile_description,
    routingProfile_mediaConcurrencies,
    routingProfile_routingProfileArn,
    routingProfile_instanceId,
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

    -- ** SecurityProfileSummary
    securityProfileSummary_name,
    securityProfileSummary_arn,
    securityProfileSummary_id,

    -- ** Threshold
    threshold_thresholdValue,
    threshold_comparison,

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

    -- ** UserIdentityInfo
    userIdentityInfo_firstName,
    userIdentityInfo_email,
    userIdentityInfo_lastName,

    -- ** UserPhoneConfig
    userPhoneConfig_autoAccept,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_phoneType,

    -- ** UserQuickConnectConfig
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- ** UserSummary
    userSummary_username,
    userSummary_arn,
    userSummary_id,

    -- ** VoiceRecordingConfiguration
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Amazonka.Connect.AssociateApprovedOrigin
import Amazonka.Connect.AssociateBot
import Amazonka.Connect.AssociateInstanceStorageConfig
import Amazonka.Connect.AssociateLambdaFunction
import Amazonka.Connect.AssociateLexBot
import Amazonka.Connect.AssociateQueueQuickConnects
import Amazonka.Connect.AssociateRoutingProfileQueues
import Amazonka.Connect.AssociateSecurityKey
import Amazonka.Connect.CreateAgentStatus
import Amazonka.Connect.CreateContactFlow
import Amazonka.Connect.CreateHoursOfOperation
import Amazonka.Connect.CreateInstance
import Amazonka.Connect.CreateIntegrationAssociation
import Amazonka.Connect.CreateQueue
import Amazonka.Connect.CreateQuickConnect
import Amazonka.Connect.CreateRoutingProfile
import Amazonka.Connect.CreateUseCase
import Amazonka.Connect.CreateUser
import Amazonka.Connect.CreateUserHierarchyGroup
import Amazonka.Connect.DeleteHoursOfOperation
import Amazonka.Connect.DeleteInstance
import Amazonka.Connect.DeleteIntegrationAssociation
import Amazonka.Connect.DeleteQuickConnect
import Amazonka.Connect.DeleteUseCase
import Amazonka.Connect.DeleteUser
import Amazonka.Connect.DeleteUserHierarchyGroup
import Amazonka.Connect.DescribeAgentStatus
import Amazonka.Connect.DescribeContactFlow
import Amazonka.Connect.DescribeHoursOfOperation
import Amazonka.Connect.DescribeInstance
import Amazonka.Connect.DescribeInstanceAttribute
import Amazonka.Connect.DescribeInstanceStorageConfig
import Amazonka.Connect.DescribeQueue
import Amazonka.Connect.DescribeQuickConnect
import Amazonka.Connect.DescribeRoutingProfile
import Amazonka.Connect.DescribeUser
import Amazonka.Connect.DescribeUserHierarchyGroup
import Amazonka.Connect.DescribeUserHierarchyStructure
import Amazonka.Connect.DisassociateApprovedOrigin
import Amazonka.Connect.DisassociateBot
import Amazonka.Connect.DisassociateInstanceStorageConfig
import Amazonka.Connect.DisassociateLambdaFunction
import Amazonka.Connect.DisassociateLexBot
import Amazonka.Connect.DisassociateQueueQuickConnects
import Amazonka.Connect.DisassociateRoutingProfileQueues
import Amazonka.Connect.DisassociateSecurityKey
import Amazonka.Connect.GetContactAttributes
import Amazonka.Connect.GetCurrentMetricData
import Amazonka.Connect.GetFederationToken
import Amazonka.Connect.GetMetricData
import Amazonka.Connect.ListAgentStatuses
import Amazonka.Connect.ListApprovedOrigins
import Amazonka.Connect.ListBots
import Amazonka.Connect.ListContactFlows
import Amazonka.Connect.ListHoursOfOperations
import Amazonka.Connect.ListInstanceAttributes
import Amazonka.Connect.ListInstanceStorageConfigs
import Amazonka.Connect.ListInstances
import Amazonka.Connect.ListIntegrationAssociations
import Amazonka.Connect.ListLambdaFunctions
import Amazonka.Connect.ListLexBots
import Amazonka.Connect.ListPhoneNumbers
import Amazonka.Connect.ListPrompts
import Amazonka.Connect.ListQueueQuickConnects
import Amazonka.Connect.ListQueues
import Amazonka.Connect.ListQuickConnects
import Amazonka.Connect.ListRoutingProfileQueues
import Amazonka.Connect.ListRoutingProfiles
import Amazonka.Connect.ListSecurityKeys
import Amazonka.Connect.ListSecurityProfiles
import Amazonka.Connect.ListTagsForResource
import Amazonka.Connect.ListUseCases
import Amazonka.Connect.ListUserHierarchyGroups
import Amazonka.Connect.ListUsers
import Amazonka.Connect.ResumeContactRecording
import Amazonka.Connect.StartChatContact
import Amazonka.Connect.StartContactRecording
import Amazonka.Connect.StartOutboundVoiceContact
import Amazonka.Connect.StartTaskContact
import Amazonka.Connect.StopContact
import Amazonka.Connect.StopContactRecording
import Amazonka.Connect.SuspendContactRecording
import Amazonka.Connect.TagResource
import Amazonka.Connect.Types.AgentStatus
import Amazonka.Connect.Types.AgentStatusSummary
import Amazonka.Connect.Types.AnswerMachineDetectionConfig
import Amazonka.Connect.Types.Attribute
import Amazonka.Connect.Types.ChatMessage
import Amazonka.Connect.Types.ContactFlow
import Amazonka.Connect.Types.ContactFlowSummary
import Amazonka.Connect.Types.Credentials
import Amazonka.Connect.Types.CurrentMetric
import Amazonka.Connect.Types.CurrentMetricData
import Amazonka.Connect.Types.CurrentMetricResult
import Amazonka.Connect.Types.Dimensions
import Amazonka.Connect.Types.EncryptionConfig
import Amazonka.Connect.Types.Filters
import Amazonka.Connect.Types.HierarchyGroup
import Amazonka.Connect.Types.HierarchyGroupSummary
import Amazonka.Connect.Types.HierarchyLevel
import Amazonka.Connect.Types.HierarchyLevelUpdate
import Amazonka.Connect.Types.HierarchyPath
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
import Amazonka.Connect.Types.KinesisFirehoseConfig
import Amazonka.Connect.Types.KinesisStreamConfig
import Amazonka.Connect.Types.KinesisVideoStreamConfig
import Amazonka.Connect.Types.LexBot
import Amazonka.Connect.Types.LexBotConfig
import Amazonka.Connect.Types.LexV2Bot
import Amazonka.Connect.Types.MediaConcurrency
import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.ParticipantDetails
import Amazonka.Connect.Types.PhoneNumberQuickConnectConfig
import Amazonka.Connect.Types.PhoneNumberSummary
import Amazonka.Connect.Types.PromptSummary
import Amazonka.Connect.Types.Queue
import Amazonka.Connect.Types.QueueQuickConnectConfig
import Amazonka.Connect.Types.QueueReference
import Amazonka.Connect.Types.QueueSummary
import Amazonka.Connect.Types.QuickConnect
import Amazonka.Connect.Types.QuickConnectConfig
import Amazonka.Connect.Types.QuickConnectSummary
import Amazonka.Connect.Types.Reference
import Amazonka.Connect.Types.RoutingProfile
import Amazonka.Connect.Types.RoutingProfileQueueConfig
import Amazonka.Connect.Types.RoutingProfileQueueConfigSummary
import Amazonka.Connect.Types.RoutingProfileQueueReference
import Amazonka.Connect.Types.RoutingProfileSummary
import Amazonka.Connect.Types.S3Config
import Amazonka.Connect.Types.SecurityKey
import Amazonka.Connect.Types.SecurityProfileSummary
import Amazonka.Connect.Types.Threshold
import Amazonka.Connect.Types.UseCase
import Amazonka.Connect.Types.User
import Amazonka.Connect.Types.UserIdentityInfo
import Amazonka.Connect.Types.UserPhoneConfig
import Amazonka.Connect.Types.UserQuickConnectConfig
import Amazonka.Connect.Types.UserSummary
import Amazonka.Connect.Types.VoiceRecordingConfiguration
import Amazonka.Connect.UntagResource
import Amazonka.Connect.UpdateAgentStatus
import Amazonka.Connect.UpdateContactAttributes
import Amazonka.Connect.UpdateContactFlowContent
import Amazonka.Connect.UpdateContactFlowName
import Amazonka.Connect.UpdateHoursOfOperation
import Amazonka.Connect.UpdateInstanceAttribute
import Amazonka.Connect.UpdateInstanceStorageConfig
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
import Amazonka.Connect.UpdateUserHierarchy
import Amazonka.Connect.UpdateUserHierarchyGroupName
import Amazonka.Connect.UpdateUserHierarchyStructure
import Amazonka.Connect.UpdateUserIdentityInfo
import Amazonka.Connect.UpdateUserPhoneConfig
import Amazonka.Connect.UpdateUserRoutingProfile
import Amazonka.Connect.UpdateUserSecurityProfiles

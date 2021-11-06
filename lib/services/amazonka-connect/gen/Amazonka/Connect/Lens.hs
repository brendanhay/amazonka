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

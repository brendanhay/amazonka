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

    -- ** CreateQuickConnect
    createQuickConnect_tags,
    createQuickConnect_description,
    createQuickConnect_instanceId,
    createQuickConnect_name,
    createQuickConnect_quickConnectConfig,
    createQuickConnectResponse_quickConnectId,
    createQuickConnectResponse_quickConnectARN,
    createQuickConnectResponse_httpStatus,

    -- ** UpdateUserRoutingProfile
    updateUserRoutingProfile_routingProfileId,
    updateUserRoutingProfile_userId,
    updateUserRoutingProfile_instanceId,

    -- ** UpdateUserHierarchyGroupName
    updateUserHierarchyGroupName_name,
    updateUserHierarchyGroupName_hierarchyGroupId,
    updateUserHierarchyGroupName_instanceId,

    -- ** UpdateContactFlowName
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- ** ListSecurityProfiles
    listSecurityProfiles_nextToken,
    listSecurityProfiles_maxResults,
    listSecurityProfiles_instanceId,
    listSecurityProfilesResponse_nextToken,
    listSecurityProfilesResponse_securityProfileSummaryList,
    listSecurityProfilesResponse_httpStatus,

    -- ** DescribeInstance
    describeInstance_instanceId,
    describeInstanceResponse_instance,
    describeInstanceResponse_httpStatus,

    -- ** ListInstanceAttributes
    listInstanceAttributes_nextToken,
    listInstanceAttributes_maxResults,
    listInstanceAttributes_instanceId,
    listInstanceAttributesResponse_nextToken,
    listInstanceAttributesResponse_attributes,
    listInstanceAttributesResponse_httpStatus,

    -- ** ListLambdaFunctions
    listLambdaFunctions_nextToken,
    listLambdaFunctions_maxResults,
    listLambdaFunctions_instanceId,
    listLambdaFunctionsResponse_nextToken,
    listLambdaFunctionsResponse_lambdaFunctions,
    listLambdaFunctionsResponse_httpStatus,

    -- ** UpdateRoutingProfileQueues
    updateRoutingProfileQueues_instanceId,
    updateRoutingProfileQueues_routingProfileId,
    updateRoutingProfileQueues_queueConfigs,

    -- ** AssociateRoutingProfileQueues
    associateRoutingProfileQueues_instanceId,
    associateRoutingProfileQueues_routingProfileId,
    associateRoutingProfileQueues_queueConfigs,

    -- ** GetContactAttributes
    getContactAttributes_instanceId,
    getContactAttributes_initialContactId,
    getContactAttributesResponse_attributes,
    getContactAttributesResponse_httpStatus,

    -- ** ListLexBots
    listLexBots_nextToken,
    listLexBots_maxResults,
    listLexBots_instanceId,
    listLexBotsResponse_nextToken,
    listLexBotsResponse_lexBots,
    listLexBotsResponse_httpStatus,

    -- ** AssociateLambdaFunction
    associateLambdaFunction_instanceId,
    associateLambdaFunction_functionArn,

    -- ** ListApprovedOrigins
    listApprovedOrigins_nextToken,
    listApprovedOrigins_maxResults,
    listApprovedOrigins_instanceId,
    listApprovedOriginsResponse_origins,
    listApprovedOriginsResponse_nextToken,
    listApprovedOriginsResponse_httpStatus,

    -- ** AssociateInstanceStorageConfig
    associateInstanceStorageConfig_instanceId,
    associateInstanceStorageConfig_resourceType,
    associateInstanceStorageConfig_storageConfig,
    associateInstanceStorageConfigResponse_associationId,
    associateInstanceStorageConfigResponse_httpStatus,

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

    -- ** UpdateUserPhoneConfig
    updateUserPhoneConfig_phoneConfig,
    updateUserPhoneConfig_userId,
    updateUserPhoneConfig_instanceId,

    -- ** UpdateContactAttributes
    updateContactAttributes_initialContactId,
    updateContactAttributes_instanceId,
    updateContactAttributes_attributes,
    updateContactAttributesResponse_httpStatus,

    -- ** ListRoutingProfiles
    listRoutingProfiles_nextToken,
    listRoutingProfiles_maxResults,
    listRoutingProfiles_instanceId,
    listRoutingProfilesResponse_nextToken,
    listRoutingProfilesResponse_routingProfileSummaryList,
    listRoutingProfilesResponse_httpStatus,

    -- ** DeleteUseCase
    deleteUseCase_instanceId,
    deleteUseCase_integrationAssociationId,
    deleteUseCase_useCaseId,

    -- ** DescribeQuickConnect
    describeQuickConnect_instanceId,
    describeQuickConnect_quickConnectId,
    describeQuickConnectResponse_quickConnect,
    describeQuickConnectResponse_httpStatus,

    -- ** ListQueueQuickConnects
    listQueueQuickConnects_nextToken,
    listQueueQuickConnects_maxResults,
    listQueueQuickConnects_instanceId,
    listQueueQuickConnects_queueId,
    listQueueQuickConnectsResponse_nextToken,
    listQueueQuickConnectsResponse_quickConnectSummaryList,
    listQueueQuickConnectsResponse_httpStatus,

    -- ** CreateRoutingProfile
    createRoutingProfile_queueConfigs,
    createRoutingProfile_tags,
    createRoutingProfile_instanceId,
    createRoutingProfile_name,
    createRoutingProfile_description,
    createRoutingProfile_defaultOutboundQueueId,
    createRoutingProfile_mediaConcurrencies,
    createRoutingProfileResponse_routingProfileId,
    createRoutingProfileResponse_routingProfileArn,
    createRoutingProfileResponse_httpStatus,

    -- ** AssociateApprovedOrigin
    associateApprovedOrigin_instanceId,
    associateApprovedOrigin_origin,

    -- ** DisassociateQueueQuickConnects
    disassociateQueueQuickConnects_instanceId,
    disassociateQueueQuickConnects_queueId,
    disassociateQueueQuickConnects_quickConnectIds,

    -- ** UpdateRoutingProfileConcurrency
    updateRoutingProfileConcurrency_instanceId,
    updateRoutingProfileConcurrency_routingProfileId,
    updateRoutingProfileConcurrency_mediaConcurrencies,

    -- ** UpdateQueueOutboundCallerConfig
    updateQueueOutboundCallerConfig_instanceId,
    updateQueueOutboundCallerConfig_queueId,
    updateQueueOutboundCallerConfig_outboundCallerConfig,

    -- ** DisassociateSecurityKey
    disassociateSecurityKey_instanceId,
    disassociateSecurityKey_associationId,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** GetCurrentMetricData
    getCurrentMetricData_nextToken,
    getCurrentMetricData_groupings,
    getCurrentMetricData_maxResults,
    getCurrentMetricData_instanceId,
    getCurrentMetricData_filters,
    getCurrentMetricData_currentMetrics,
    getCurrentMetricDataResponse_nextToken,
    getCurrentMetricDataResponse_dataSnapshotTime,
    getCurrentMetricDataResponse_metricResults,
    getCurrentMetricDataResponse_httpStatus,

    -- ** UpdateQuickConnectConfig
    updateQuickConnectConfig_instanceId,
    updateQuickConnectConfig_quickConnectId,
    updateQuickConnectConfig_quickConnectConfig,

    -- ** ListInstances
    listInstances_nextToken,
    listInstances_maxResults,
    listInstancesResponse_nextToken,
    listInstancesResponse_instanceSummaryList,
    listInstancesResponse_httpStatus,

    -- ** ListQueues
    listQueues_nextToken,
    listQueues_maxResults,
    listQueues_queueTypes,
    listQueues_instanceId,
    listQueuesResponse_nextToken,
    listQueuesResponse_queueSummaryList,
    listQueuesResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_instanceId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** StopContact
    stopContact_contactId,
    stopContact_instanceId,
    stopContactResponse_httpStatus,

    -- ** CreateUserHierarchyGroup
    createUserHierarchyGroup_parentGroupId,
    createUserHierarchyGroup_name,
    createUserHierarchyGroup_instanceId,
    createUserHierarchyGroupResponse_hierarchyGroupArn,
    createUserHierarchyGroupResponse_hierarchyGroupId,
    createUserHierarchyGroupResponse_httpStatus,

    -- ** StartContactRecording
    startContactRecording_instanceId,
    startContactRecording_contactId,
    startContactRecording_initialContactId,
    startContactRecording_voiceRecordingConfiguration,
    startContactRecordingResponse_httpStatus,

    -- ** CreateUser
    createUser_identityInfo,
    createUser_hierarchyGroupId,
    createUser_directoryUserId,
    createUser_password,
    createUser_tags,
    createUser_username,
    createUser_phoneConfig,
    createUser_securityProfileIds,
    createUser_routingProfileId,
    createUser_instanceId,
    createUserResponse_userArn,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** AssociateSecurityKey
    associateSecurityKey_instanceId,
    associateSecurityKey_key,
    associateSecurityKeyResponse_associationId,
    associateSecurityKeyResponse_httpStatus,

    -- ** AssociateQueueQuickConnects
    associateQueueQuickConnects_instanceId,
    associateQueueQuickConnects_queueId,
    associateQueueQuickConnects_quickConnectIds,

    -- ** StopContactRecording
    stopContactRecording_instanceId,
    stopContactRecording_contactId,
    stopContactRecording_initialContactId,
    stopContactRecordingResponse_httpStatus,

    -- ** DisassociateApprovedOrigin
    disassociateApprovedOrigin_instanceId,
    disassociateApprovedOrigin_origin,

    -- ** UpdateQuickConnectName
    updateQuickConnectName_name,
    updateQuickConnectName_description,
    updateQuickConnectName_instanceId,
    updateQuickConnectName_quickConnectId,

    -- ** DescribeRoutingProfile
    describeRoutingProfile_instanceId,
    describeRoutingProfile_routingProfileId,
    describeRoutingProfileResponse_routingProfile,
    describeRoutingProfileResponse_httpStatus,

    -- ** ListQuickConnects
    listQuickConnects_nextToken,
    listQuickConnects_maxResults,
    listQuickConnects_quickConnectTypes,
    listQuickConnects_instanceId,
    listQuickConnectsResponse_nextToken,
    listQuickConnectsResponse_quickConnectSummaryList,
    listQuickConnectsResponse_httpStatus,

    -- ** DisassociateLexBot
    disassociateLexBot_instanceId,
    disassociateLexBot_botName,
    disassociateLexBot_lexRegion,

    -- ** DeleteQuickConnect
    deleteQuickConnect_instanceId,
    deleteQuickConnect_quickConnectId,

    -- ** ListRoutingProfileQueues
    listRoutingProfileQueues_nextToken,
    listRoutingProfileQueues_maxResults,
    listRoutingProfileQueues_instanceId,
    listRoutingProfileQueues_routingProfileId,
    listRoutingProfileQueuesResponse_nextToken,
    listRoutingProfileQueuesResponse_routingProfileQueueConfigSummaryList,
    listRoutingProfileQueuesResponse_httpStatus,

    -- ** UpdateUserHierarchy
    updateUserHierarchy_hierarchyGroupId,
    updateUserHierarchy_userId,
    updateUserHierarchy_instanceId,

    -- ** DisassociateLambdaFunction
    disassociateLambdaFunction_instanceId,
    disassociateLambdaFunction_functionArn,

    -- ** UpdateQueueMaxContacts
    updateQueueMaxContacts_instanceId,
    updateQueueMaxContacts_queueId,
    updateQueueMaxContacts_maxContacts,

    -- ** DescribeInstanceStorageConfig
    describeInstanceStorageConfig_instanceId,
    describeInstanceStorageConfig_associationId,
    describeInstanceStorageConfig_resourceType,
    describeInstanceStorageConfigResponse_storageConfig,
    describeInstanceStorageConfigResponse_httpStatus,

    -- ** UpdateQueueHoursOfOperation
    updateQueueHoursOfOperation_instanceId,
    updateQueueHoursOfOperation_queueId,
    updateQueueHoursOfOperation_hoursOfOperationId,

    -- ** DisassociateRoutingProfileQueues
    disassociateRoutingProfileQueues_instanceId,
    disassociateRoutingProfileQueues_routingProfileId,
    disassociateRoutingProfileQueues_queueReferences,

    -- ** DescribeContactFlow
    describeContactFlow_instanceId,
    describeContactFlow_contactFlowId,
    describeContactFlowResponse_contactFlow,
    describeContactFlowResponse_httpStatus,

    -- ** UpdateQueueStatus
    updateQueueStatus_instanceId,
    updateQueueStatus_queueId,
    updateQueueStatus_status,

    -- ** DescribeQueue
    describeQueue_instanceId,
    describeQueue_queueId,
    describeQueueResponse_queue,
    describeQueueResponse_httpStatus,

    -- ** AssociateLexBot
    associateLexBot_instanceId,
    associateLexBot_lexBot,

    -- ** UpdateInstanceAttribute
    updateInstanceAttribute_instanceId,
    updateInstanceAttribute_attributeType,
    updateInstanceAttribute_value,

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

    -- ** ResumeContactRecording
    resumeContactRecording_instanceId,
    resumeContactRecording_contactId,
    resumeContactRecording_initialContactId,
    resumeContactRecordingResponse_httpStatus,

    -- ** UpdateRoutingProfileName
    updateRoutingProfileName_name,
    updateRoutingProfileName_description,
    updateRoutingProfileName_instanceId,
    updateRoutingProfileName_routingProfileId,

    -- ** StartChatContact
    startChatContact_initialMessage,
    startChatContact_attributes,
    startChatContact_clientToken,
    startChatContact_instanceId,
    startChatContact_contactFlowId,
    startChatContact_participantDetails,
    startChatContactResponse_participantToken,
    startChatContactResponse_contactId,
    startChatContactResponse_participantId,
    startChatContactResponse_httpStatus,

    -- ** DeleteIntegrationAssociation
    deleteIntegrationAssociation_instanceId,
    deleteIntegrationAssociation_integrationAssociationId,

    -- ** ListPhoneNumbers
    listPhoneNumbers_nextToken,
    listPhoneNumbers_phoneNumberTypes,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_phoneNumberCountryCodes,
    listPhoneNumbers_instanceId,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumberSummaryList,
    listPhoneNumbersResponse_httpStatus,

    -- ** ListIntegrationAssociations
    listIntegrationAssociations_nextToken,
    listIntegrationAssociations_maxResults,
    listIntegrationAssociations_instanceId,
    listIntegrationAssociationsResponse_nextToken,
    listIntegrationAssociationsResponse_integrationAssociationSummaryList,
    listIntegrationAssociationsResponse_httpStatus,

    -- ** ListUseCases
    listUseCases_nextToken,
    listUseCases_maxResults,
    listUseCases_instanceId,
    listUseCases_integrationAssociationId,
    listUseCasesResponse_nextToken,
    listUseCasesResponse_useCaseSummaryList,
    listUseCasesResponse_httpStatus,

    -- ** UpdateUserSecurityProfiles
    updateUserSecurityProfiles_securityProfileIds,
    updateUserSecurityProfiles_userId,
    updateUserSecurityProfiles_instanceId,

    -- ** DescribeUserHierarchyStructure
    describeUserHierarchyStructure_instanceId,
    describeUserHierarchyStructureResponse_hierarchyStructure,
    describeUserHierarchyStructureResponse_httpStatus,

    -- ** ListHoursOfOperations
    listHoursOfOperations_nextToken,
    listHoursOfOperations_maxResults,
    listHoursOfOperations_instanceId,
    listHoursOfOperationsResponse_nextToken,
    listHoursOfOperationsResponse_hoursOfOperationSummaryList,
    listHoursOfOperationsResponse_httpStatus,

    -- ** CreateUseCase
    createUseCase_instanceId,
    createUseCase_integrationAssociationId,
    createUseCase_useCaseType,
    createUseCaseResponse_useCaseArn,
    createUseCaseResponse_useCaseId,
    createUseCaseResponse_httpStatus,

    -- ** ListContactFlows
    listContactFlows_nextToken,
    listContactFlows_contactFlowTypes,
    listContactFlows_maxResults,
    listContactFlows_instanceId,
    listContactFlowsResponse_nextToken,
    listContactFlowsResponse_contactFlowSummaryList,
    listContactFlowsResponse_httpStatus,

    -- ** UpdateInstanceStorageConfig
    updateInstanceStorageConfig_instanceId,
    updateInstanceStorageConfig_associationId,
    updateInstanceStorageConfig_resourceType,
    updateInstanceStorageConfig_storageConfig,

    -- ** ListInstanceStorageConfigs
    listInstanceStorageConfigs_nextToken,
    listInstanceStorageConfigs_maxResults,
    listInstanceStorageConfigs_instanceId,
    listInstanceStorageConfigs_resourceType,
    listInstanceStorageConfigsResponse_nextToken,
    listInstanceStorageConfigsResponse_storageConfigs,
    listInstanceStorageConfigsResponse_httpStatus,

    -- ** CreateIntegrationAssociation
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_sourceType,
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_httpStatus,

    -- ** DeleteUserHierarchyGroup
    deleteUserHierarchyGroup_hierarchyGroupId,
    deleteUserHierarchyGroup_instanceId,

    -- ** DeleteUser
    deleteUser_instanceId,
    deleteUser_userId,

    -- ** DisassociateInstanceStorageConfig
    disassociateInstanceStorageConfig_instanceId,
    disassociateInstanceStorageConfig_associationId,
    disassociateInstanceStorageConfig_resourceType,

    -- ** ListUserHierarchyGroups
    listUserHierarchyGroups_nextToken,
    listUserHierarchyGroups_maxResults,
    listUserHierarchyGroups_instanceId,
    listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList,
    listUserHierarchyGroupsResponse_nextToken,
    listUserHierarchyGroupsResponse_httpStatus,

    -- ** UpdateUserIdentityInfo
    updateUserIdentityInfo_identityInfo,
    updateUserIdentityInfo_userId,
    updateUserIdentityInfo_instanceId,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_instanceId,
    listUsersResponse_userSummaryList,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,

    -- ** GetFederationToken
    getFederationToken_instanceId,
    getFederationTokenResponse_credentials,
    getFederationTokenResponse_httpStatus,

    -- ** DescribeInstanceAttribute
    describeInstanceAttribute_instanceId,
    describeInstanceAttribute_attributeType,
    describeInstanceAttributeResponse_attribute,
    describeInstanceAttributeResponse_httpStatus,

    -- ** ListSecurityKeys
    listSecurityKeys_nextToken,
    listSecurityKeys_maxResults,
    listSecurityKeys_instanceId,
    listSecurityKeysResponse_nextToken,
    listSecurityKeysResponse_securityKeys,
    listSecurityKeysResponse_httpStatus,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    updateRoutingProfileDefaultOutboundQueue_instanceId,
    updateRoutingProfileDefaultOutboundQueue_routingProfileId,
    updateRoutingProfileDefaultOutboundQueue_defaultOutboundQueueId,

    -- ** CreateQueue
    createQueue_maxContacts,
    createQueue_tags,
    createQueue_description,
    createQueue_outboundCallerConfig,
    createQueue_quickConnectIds,
    createQueue_instanceId,
    createQueue_name,
    createQueue_hoursOfOperationId,
    createQueueResponse_queueId,
    createQueueResponse_queueArn,
    createQueueResponse_httpStatus,

    -- ** CreateInstance
    createInstance_instanceAlias,
    createInstance_directoryId,
    createInstance_clientToken,
    createInstance_identityManagementType,
    createInstance_inboundCallsEnabled,
    createInstance_outboundCallsEnabled,
    createInstanceResponse_arn,
    createInstanceResponse_id,
    createInstanceResponse_httpStatus,

    -- ** StartTaskContact
    startTaskContact_previousContactId,
    startTaskContact_references,
    startTaskContact_attributes,
    startTaskContact_description,
    startTaskContact_clientToken,
    startTaskContact_instanceId,
    startTaskContact_contactFlowId,
    startTaskContact_name,
    startTaskContactResponse_contactId,
    startTaskContactResponse_httpStatus,

    -- ** ListPrompts
    listPrompts_nextToken,
    listPrompts_maxResults,
    listPrompts_instanceId,
    listPromptsResponse_nextToken,
    listPromptsResponse_promptSummaryList,
    listPromptsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartOutboundVoiceContact
    startOutboundVoiceContact_queueId,
    startOutboundVoiceContact_sourcePhoneNumber,
    startOutboundVoiceContact_attributes,
    startOutboundVoiceContact_clientToken,
    startOutboundVoiceContact_destinationPhoneNumber,
    startOutboundVoiceContact_contactFlowId,
    startOutboundVoiceContact_instanceId,
    startOutboundVoiceContactResponse_contactId,
    startOutboundVoiceContactResponse_httpStatus,

    -- ** UpdateUserHierarchyStructure
    updateUserHierarchyStructure_hierarchyStructure,
    updateUserHierarchyStructure_instanceId,

    -- ** UpdateContactFlowContent
    updateContactFlowContent_instanceId,
    updateContactFlowContent_contactFlowId,
    updateContactFlowContent_content,

    -- ** GetMetricData
    getMetricData_nextToken,
    getMetricData_groupings,
    getMetricData_maxResults,
    getMetricData_instanceId,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricData_filters,
    getMetricData_historicalMetrics,
    getMetricDataResponse_nextToken,
    getMetricDataResponse_metricResults,
    getMetricDataResponse_httpStatus,

    -- ** DescribeHoursOfOperation
    describeHoursOfOperation_instanceId,
    describeHoursOfOperation_hoursOfOperationId,
    describeHoursOfOperationResponse_hoursOfOperation,
    describeHoursOfOperationResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_attributeType,
    attribute_value,

    -- ** ChatMessage
    chatMessage_contentType,
    chatMessage_content,

    -- ** ContactFlow
    contactFlow_arn,
    contactFlow_id,
    contactFlow_name,
    contactFlow_content,
    contactFlow_tags,
    contactFlow_description,
    contactFlow_type,

    -- ** ContactFlowSummary
    contactFlowSummary_arn,
    contactFlowSummary_id,
    contactFlowSummary_contactFlowType,
    contactFlowSummary_name,

    -- ** Credentials
    credentials_refreshTokenExpiration,
    credentials_accessToken,
    credentials_accessTokenExpiration,
    credentials_refreshToken,

    -- ** CurrentMetric
    currentMetric_unit,
    currentMetric_name,

    -- ** CurrentMetricData
    currentMetricData_metric,
    currentMetricData_value,

    -- ** CurrentMetricResult
    currentMetricResult_collections,
    currentMetricResult_dimensions,

    -- ** Dimensions
    dimensions_queue,
    dimensions_channel,

    -- ** EncryptionConfig
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- ** Filters
    filters_channels,
    filters_queues,

    -- ** HierarchyGroup
    hierarchyGroup_levelId,
    hierarchyGroup_arn,
    hierarchyGroup_id,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_name,

    -- ** HierarchyGroupSummary
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,
    hierarchyGroupSummary_name,

    -- ** HierarchyLevel
    hierarchyLevel_arn,
    hierarchyLevel_id,
    hierarchyLevel_name,

    -- ** HierarchyLevelUpdate
    hierarchyLevelUpdate_name,

    -- ** HierarchyPath
    hierarchyPath_levelThree,
    hierarchyPath_levelFour,
    hierarchyPath_levelTwo,
    hierarchyPath_levelOne,
    hierarchyPath_levelFive,

    -- ** HierarchyStructure
    hierarchyStructure_levelThree,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelTwo,
    hierarchyStructure_levelOne,
    hierarchyStructure_levelFive,

    -- ** HierarchyStructureUpdate
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelTwo,
    hierarchyStructureUpdate_levelOne,
    hierarchyStructureUpdate_levelFive,

    -- ** HistoricalMetric
    historicalMetric_threshold,
    historicalMetric_unit,
    historicalMetric_name,
    historicalMetric_statistic,

    -- ** HistoricalMetricData
    historicalMetricData_metric,
    historicalMetricData_value,

    -- ** HistoricalMetricResult
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- ** HoursOfOperation
    hoursOfOperation_config,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_name,
    hoursOfOperation_tags,
    hoursOfOperation_description,
    hoursOfOperation_timeZone,
    hoursOfOperation_hoursOfOperationId,

    -- ** HoursOfOperationConfig
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- ** HoursOfOperationSummary
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_id,
    hoursOfOperationSummary_name,

    -- ** HoursOfOperationTimeSlice
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- ** Instance
    instance_instanceAlias,
    instance_serviceRole,
    instance_outboundCallsEnabled,
    instance_arn,
    instance_id,
    instance_instanceStatus,
    instance_identityManagementType,
    instance_createdTime,
    instance_inboundCallsEnabled,
    instance_statusReason,

    -- ** InstanceStatusReason
    instanceStatusReason_message,

    -- ** InstanceStorageConfig
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_associationId,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_storageType,

    -- ** InstanceSummary
    instanceSummary_instanceAlias,
    instanceSummary_serviceRole,
    instanceSummary_outboundCallsEnabled,
    instanceSummary_arn,
    instanceSummary_id,
    instanceSummary_instanceStatus,
    instanceSummary_identityManagementType,
    instanceSummary_createdTime,
    instanceSummary_inboundCallsEnabled,

    -- ** IntegrationAssociationSummary
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_integrationType,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_sourceType,
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

    -- ** MediaConcurrency
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- ** OutboundCallerConfig
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundFlowId,
    outboundCallerConfig_outboundCallerIdName,

    -- ** ParticipantDetails
    participantDetails_displayName,

    -- ** PhoneNumberQuickConnectConfig
    phoneNumberQuickConnectConfig_phoneNumber,

    -- ** PhoneNumberSummary
    phoneNumberSummary_phoneNumber,
    phoneNumberSummary_arn,
    phoneNumberSummary_id,
    phoneNumberSummary_phoneNumberType,
    phoneNumberSummary_phoneNumberCountryCode,

    -- ** PromptSummary
    promptSummary_arn,
    promptSummary_id,
    promptSummary_name,

    -- ** Queue
    queue_maxContacts,
    queue_status,
    queue_queueId,
    queue_name,
    queue_queueArn,
    queue_tags,
    queue_description,
    queue_outboundCallerConfig,
    queue_hoursOfOperationId,

    -- ** QueueQuickConnectConfig
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- ** QueueReference
    queueReference_arn,
    queueReference_id,

    -- ** QueueSummary
    queueSummary_queueType,
    queueSummary_arn,
    queueSummary_id,
    queueSummary_name,

    -- ** QuickConnect
    quickConnect_quickConnectId,
    quickConnect_name,
    quickConnect_tags,
    quickConnect_quickConnectConfig,
    quickConnect_quickConnectARN,
    quickConnect_description,

    -- ** QuickConnectConfig
    quickConnectConfig_userConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_queueConfig,
    quickConnectConfig_quickConnectType,

    -- ** QuickConnectSummary
    quickConnectSummary_quickConnectType,
    quickConnectSummary_arn,
    quickConnectSummary_id,
    quickConnectSummary_name,

    -- ** Reference
    reference_value,
    reference_type,

    -- ** RoutingProfile
    routingProfile_instanceId,
    routingProfile_defaultOutboundQueueId,
    routingProfile_routingProfileId,
    routingProfile_mediaConcurrencies,
    routingProfile_name,
    routingProfile_tags,
    routingProfile_description,
    routingProfile_routingProfileArn,

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
    routingProfileSummary_id,
    routingProfileSummary_name,

    -- ** S3Config
    s3Config_encryptionConfig,
    s3Config_bucketName,
    s3Config_bucketPrefix,

    -- ** SecurityKey
    securityKey_key,
    securityKey_creationTime,
    securityKey_associationId,

    -- ** SecurityProfileSummary
    securityProfileSummary_arn,
    securityProfileSummary_id,
    securityProfileSummary_name,

    -- ** Threshold
    threshold_thresholdValue,
    threshold_comparison,

    -- ** UseCase
    useCase_useCaseArn,
    useCase_useCaseType,
    useCase_useCaseId,

    -- ** User
    user_securityProfileIds,
    user_identityInfo,
    user_arn,
    user_id,
    user_hierarchyGroupId,
    user_directoryUserId,
    user_routingProfileId,
    user_tags,
    user_phoneConfig,
    user_username,

    -- ** UserIdentityInfo
    userIdentityInfo_email,
    userIdentityInfo_firstName,
    userIdentityInfo_lastName,

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
    userSummary_id,
    userSummary_username,

    -- ** VoiceRecordingConfiguration
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Network.AWS.Connect.AssociateApprovedOrigin
import Network.AWS.Connect.AssociateInstanceStorageConfig
import Network.AWS.Connect.AssociateLambdaFunction
import Network.AWS.Connect.AssociateLexBot
import Network.AWS.Connect.AssociateQueueQuickConnects
import Network.AWS.Connect.AssociateRoutingProfileQueues
import Network.AWS.Connect.AssociateSecurityKey
import Network.AWS.Connect.CreateContactFlow
import Network.AWS.Connect.CreateInstance
import Network.AWS.Connect.CreateIntegrationAssociation
import Network.AWS.Connect.CreateQueue
import Network.AWS.Connect.CreateQuickConnect
import Network.AWS.Connect.CreateRoutingProfile
import Network.AWS.Connect.CreateUseCase
import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.CreateUserHierarchyGroup
import Network.AWS.Connect.DeleteInstance
import Network.AWS.Connect.DeleteIntegrationAssociation
import Network.AWS.Connect.DeleteQuickConnect
import Network.AWS.Connect.DeleteUseCase
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DeleteUserHierarchyGroup
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
import Network.AWS.Connect.ListApprovedOrigins
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
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateContactFlowContent
import Network.AWS.Connect.UpdateContactFlowName
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

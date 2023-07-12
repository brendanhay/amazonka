{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Connect.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ContactFlowNotPublishedException,
    _ContactNotFoundException,
    _DestinationNotAllowedException,
    _DuplicateResourceException,
    _IdempotencyException,
    _InternalServiceException,
    _InvalidContactFlowException,
    _InvalidContactFlowModuleException,
    _InvalidParameterException,
    _InvalidRequestException,
    _LimitExceededException,
    _OutboundContactNotPermittedException,
    _PropertyValidationException,
    _ResourceConflictException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceNotReadyException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _UserNotFoundException,

    -- * ActionType
    ActionType (..),

    -- * AgentStatusState
    AgentStatusState (..),

    -- * AgentStatusType
    AgentStatusType (..),

    -- * Channel
    Channel (..),

    -- * Comparison
    Comparison (..),

    -- * ContactFlowModuleState
    ContactFlowModuleState (..),

    -- * ContactFlowModuleStatus
    ContactFlowModuleStatus (..),

    -- * ContactFlowState
    ContactFlowState (..),

    -- * ContactFlowType
    ContactFlowType (..),

    -- * ContactInitiationMethod
    ContactInitiationMethod (..),

    -- * ContactState
    ContactState (..),

    -- * CurrentMetricName
    CurrentMetricName (..),

    -- * DirectoryType
    DirectoryType (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * EventSourceName
    EventSourceName (..),

    -- * Grouping
    Grouping (..),

    -- * HierarchyGroupMatchType
    HierarchyGroupMatchType (..),

    -- * HistoricalMetricName
    HistoricalMetricName (..),

    -- * HoursOfOperationDays
    HoursOfOperationDays (..),

    -- * InstanceAttributeType
    InstanceAttributeType (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- * IntegrationType
    IntegrationType (..),

    -- * LexVersion
    LexVersion (..),

    -- * MonitorCapability
    MonitorCapability (..),

    -- * NotificationContentType
    NotificationContentType (..),

    -- * NotificationDeliveryType
    NotificationDeliveryType (..),

    -- * ParticipantTimerAction
    ParticipantTimerAction (..),

    -- * ParticipantTimerType
    ParticipantTimerType (..),

    -- * PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * PhoneNumberWorkflowStatus
    PhoneNumberWorkflowStatus (..),

    -- * PhoneType
    PhoneType (..),

    -- * QueueStatus
    QueueStatus (..),

    -- * QueueType
    QueueType (..),

    -- * QuickConnectType
    QuickConnectType (..),

    -- * ReferenceStatus
    ReferenceStatus (..),

    -- * ReferenceType
    ReferenceType (..),

    -- * RulePublishStatus
    RulePublishStatus (..),

    -- * SearchableQueueType
    SearchableQueueType (..),

    -- * SortOrder
    SortOrder (..),

    -- * SourceType
    SourceType (..),

    -- * Statistic
    Statistic (..),

    -- * StorageType
    StorageType (..),

    -- * StringComparisonType
    StringComparisonType (..),

    -- * TaskTemplateFieldType
    TaskTemplateFieldType (..),

    -- * TaskTemplateStatus
    TaskTemplateStatus (..),

    -- * TimerEligibleParticipantRoles
    TimerEligibleParticipantRoles (..),

    -- * TrafficDistributionGroupStatus
    TrafficDistributionGroupStatus (..),

    -- * TrafficType
    TrafficType (..),

    -- * Unit
    Unit (..),

    -- * UseCaseType
    UseCaseType (..),

    -- * VocabularyLanguageCode
    VocabularyLanguageCode (..),

    -- * VocabularyState
    VocabularyState (..),

    -- * VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- * ActionSummary
    ActionSummary (..),
    newActionSummary,
    actionSummary_actionType,

    -- * AgentContactReference
    AgentContactReference (..),
    newAgentContactReference,
    agentContactReference_agentContactState,
    agentContactReference_channel,
    agentContactReference_connectedToAgentTimestamp,
    agentContactReference_contactId,
    agentContactReference_initiationMethod,
    agentContactReference_queue,
    agentContactReference_stateStartTimestamp,

    -- * AgentInfo
    AgentInfo (..),
    newAgentInfo,
    agentInfo_connectedToAgentTimestamp,
    agentInfo_id,

    -- * AgentStatus
    AgentStatus (..),
    newAgentStatus,
    agentStatus_agentStatusARN,
    agentStatus_agentStatusId,
    agentStatus_description,
    agentStatus_displayOrder,
    agentStatus_name,
    agentStatus_state,
    agentStatus_tags,
    agentStatus_type,

    -- * AgentStatusReference
    AgentStatusReference (..),
    newAgentStatusReference,
    agentStatusReference_statusArn,
    agentStatusReference_statusName,
    agentStatusReference_statusStartTimestamp,

    -- * AgentStatusSummary
    AgentStatusSummary (..),
    newAgentStatusSummary,
    agentStatusSummary_arn,
    agentStatusSummary_id,
    agentStatusSummary_name,
    agentStatusSummary_type,

    -- * AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (..),
    newAnswerMachineDetectionConfig,
    answerMachineDetectionConfig_awaitAnswerMachinePrompt,
    answerMachineDetectionConfig_enableAnswerMachineDetection,

    -- * AssignContactCategoryActionDefinition
    AssignContactCategoryActionDefinition (..),
    newAssignContactCategoryActionDefinition,

    -- * AttachmentReference
    AttachmentReference (..),
    newAttachmentReference,
    attachmentReference_name,
    attachmentReference_status,
    attachmentReference_value,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_attributeType,
    attribute_value,

    -- * AvailableNumberSummary
    AvailableNumberSummary (..),
    newAvailableNumberSummary,
    availableNumberSummary_phoneNumber,
    availableNumberSummary_phoneNumberCountryCode,
    availableNumberSummary_phoneNumberType,

    -- * ChatMessage
    ChatMessage (..),
    newChatMessage,
    chatMessage_contentType,
    chatMessage_content,

    -- * ChatParticipantRoleConfig
    ChatParticipantRoleConfig (..),
    newChatParticipantRoleConfig,
    chatParticipantRoleConfig_participantTimerConfigList,

    -- * ChatStreamingConfiguration
    ChatStreamingConfiguration (..),
    newChatStreamingConfiguration,
    chatStreamingConfiguration_streamingEndpointArn,

    -- * ClaimedPhoneNumberSummary
    ClaimedPhoneNumberSummary (..),
    newClaimedPhoneNumberSummary,
    claimedPhoneNumberSummary_phoneNumber,
    claimedPhoneNumberSummary_phoneNumberArn,
    claimedPhoneNumberSummary_phoneNumberCountryCode,
    claimedPhoneNumberSummary_phoneNumberDescription,
    claimedPhoneNumberSummary_phoneNumberId,
    claimedPhoneNumberSummary_phoneNumberStatus,
    claimedPhoneNumberSummary_phoneNumberType,
    claimedPhoneNumberSummary_tags,
    claimedPhoneNumberSummary_targetArn,

    -- * Contact
    Contact (..),
    newContact,
    contact_agentInfo,
    contact_arn,
    contact_channel,
    contact_description,
    contact_disconnectTimestamp,
    contact_id,
    contact_initialContactId,
    contact_initiationMethod,
    contact_initiationTimestamp,
    contact_lastUpdateTimestamp,
    contact_name,
    contact_previousContactId,
    contact_queueInfo,
    contact_scheduledTimestamp,

    -- * ContactFilter
    ContactFilter (..),
    newContactFilter,
    contactFilter_contactStates,

    -- * ContactFlow
    ContactFlow (..),
    newContactFlow,
    contactFlow_arn,
    contactFlow_content,
    contactFlow_description,
    contactFlow_id,
    contactFlow_name,
    contactFlow_state,
    contactFlow_tags,
    contactFlow_type,

    -- * ContactFlowModule
    ContactFlowModule (..),
    newContactFlowModule,
    contactFlowModule_arn,
    contactFlowModule_content,
    contactFlowModule_description,
    contactFlowModule_id,
    contactFlowModule_name,
    contactFlowModule_state,
    contactFlowModule_status,
    contactFlowModule_tags,

    -- * ContactFlowModuleSummary
    ContactFlowModuleSummary (..),
    newContactFlowModuleSummary,
    contactFlowModuleSummary_arn,
    contactFlowModuleSummary_id,
    contactFlowModuleSummary_name,
    contactFlowModuleSummary_state,

    -- * ContactFlowSummary
    ContactFlowSummary (..),
    newContactFlowSummary,
    contactFlowSummary_arn,
    contactFlowSummary_contactFlowState,
    contactFlowSummary_contactFlowType,
    contactFlowSummary_id,
    contactFlowSummary_name,

    -- * ControlPlaneTagFilter
    ControlPlaneTagFilter (..),
    newControlPlaneTagFilter,
    controlPlaneTagFilter_andConditions,
    controlPlaneTagFilter_orConditions,
    controlPlaneTagFilter_tagCondition,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_accessToken,
    credentials_accessTokenExpiration,
    credentials_refreshToken,
    credentials_refreshTokenExpiration,

    -- * CurrentMetric
    CurrentMetric (..),
    newCurrentMetric,
    currentMetric_name,
    currentMetric_unit,

    -- * CurrentMetricData
    CurrentMetricData (..),
    newCurrentMetricData,
    currentMetricData_metric,
    currentMetricData_value,

    -- * CurrentMetricResult
    CurrentMetricResult (..),
    newCurrentMetricResult,
    currentMetricResult_collections,
    currentMetricResult_dimensions,

    -- * CurrentMetricSortCriteria
    CurrentMetricSortCriteria (..),
    newCurrentMetricSortCriteria,
    currentMetricSortCriteria_sortByMetric,
    currentMetricSortCriteria_sortOrder,

    -- * DateReference
    DateReference (..),
    newDateReference,
    dateReference_name,
    dateReference_value,

    -- * DefaultVocabulary
    DefaultVocabulary (..),
    newDefaultVocabulary,
    defaultVocabulary_instanceId,
    defaultVocabulary_languageCode,
    defaultVocabulary_vocabularyId,
    defaultVocabulary_vocabularyName,

    -- * Dimensions
    Dimensions (..),
    newDimensions,
    dimensions_channel,
    dimensions_queue,
    dimensions_routingProfile,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_region,
    distribution_percentage,

    -- * EmailReference
    EmailReference (..),
    newEmailReference,
    emailReference_name,
    emailReference_value,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_encryptionType,
    encryptionConfig_keyId,

    -- * EventBridgeActionDefinition
    EventBridgeActionDefinition (..),
    newEventBridgeActionDefinition,
    eventBridgeActionDefinition_name,

    -- * Filters
    Filters (..),
    newFilters,
    filters_channels,
    filters_queues,
    filters_routingProfiles,

    -- * HierarchyGroup
    HierarchyGroup (..),
    newHierarchyGroup,
    hierarchyGroup_arn,
    hierarchyGroup_hierarchyPath,
    hierarchyGroup_id,
    hierarchyGroup_levelId,
    hierarchyGroup_name,
    hierarchyGroup_tags,

    -- * HierarchyGroupCondition
    HierarchyGroupCondition (..),
    newHierarchyGroupCondition,
    hierarchyGroupCondition_hierarchyGroupMatchType,
    hierarchyGroupCondition_value,

    -- * HierarchyGroupSummary
    HierarchyGroupSummary (..),
    newHierarchyGroupSummary,
    hierarchyGroupSummary_arn,
    hierarchyGroupSummary_id,
    hierarchyGroupSummary_name,

    -- * HierarchyGroupSummaryReference
    HierarchyGroupSummaryReference (..),
    newHierarchyGroupSummaryReference,
    hierarchyGroupSummaryReference_arn,
    hierarchyGroupSummaryReference_id,

    -- * HierarchyLevel
    HierarchyLevel (..),
    newHierarchyLevel,
    hierarchyLevel_arn,
    hierarchyLevel_id,
    hierarchyLevel_name,

    -- * HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    newHierarchyLevelUpdate,
    hierarchyLevelUpdate_name,

    -- * HierarchyPath
    HierarchyPath (..),
    newHierarchyPath,
    hierarchyPath_levelFive,
    hierarchyPath_levelFour,
    hierarchyPath_levelOne,
    hierarchyPath_levelThree,
    hierarchyPath_levelTwo,

    -- * HierarchyPathReference
    HierarchyPathReference (..),
    newHierarchyPathReference,
    hierarchyPathReference_levelFive,
    hierarchyPathReference_levelFour,
    hierarchyPathReference_levelOne,
    hierarchyPathReference_levelThree,
    hierarchyPathReference_levelTwo,

    -- * HierarchyStructure
    HierarchyStructure (..),
    newHierarchyStructure,
    hierarchyStructure_levelFive,
    hierarchyStructure_levelFour,
    hierarchyStructure_levelOne,
    hierarchyStructure_levelThree,
    hierarchyStructure_levelTwo,

    -- * HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    newHierarchyStructureUpdate,
    hierarchyStructureUpdate_levelFive,
    hierarchyStructureUpdate_levelFour,
    hierarchyStructureUpdate_levelOne,
    hierarchyStructureUpdate_levelThree,
    hierarchyStructureUpdate_levelTwo,

    -- * HistoricalMetric
    HistoricalMetric (..),
    newHistoricalMetric,
    historicalMetric_name,
    historicalMetric_statistic,
    historicalMetric_threshold,
    historicalMetric_unit,

    -- * HistoricalMetricData
    HistoricalMetricData (..),
    newHistoricalMetricData,
    historicalMetricData_metric,
    historicalMetricData_value,

    -- * HistoricalMetricResult
    HistoricalMetricResult (..),
    newHistoricalMetricResult,
    historicalMetricResult_collections,
    historicalMetricResult_dimensions,

    -- * HoursOfOperation
    HoursOfOperation (..),
    newHoursOfOperation,
    hoursOfOperation_config,
    hoursOfOperation_description,
    hoursOfOperation_hoursOfOperationArn,
    hoursOfOperation_hoursOfOperationId,
    hoursOfOperation_name,
    hoursOfOperation_tags,
    hoursOfOperation_timeZone,

    -- * HoursOfOperationConfig
    HoursOfOperationConfig (..),
    newHoursOfOperationConfig,
    hoursOfOperationConfig_day,
    hoursOfOperationConfig_startTime,
    hoursOfOperationConfig_endTime,

    -- * HoursOfOperationSummary
    HoursOfOperationSummary (..),
    newHoursOfOperationSummary,
    hoursOfOperationSummary_arn,
    hoursOfOperationSummary_id,
    hoursOfOperationSummary_name,

    -- * HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (..),
    newHoursOfOperationTimeSlice,
    hoursOfOperationTimeSlice_hours,
    hoursOfOperationTimeSlice_minutes,

    -- * Instance
    Instance (..),
    newInstance,
    instance_arn,
    instance_createdTime,
    instance_id,
    instance_identityManagementType,
    instance_inboundCallsEnabled,
    instance_instanceAlias,
    instance_instanceStatus,
    instance_outboundCallsEnabled,
    instance_serviceRole,
    instance_statusReason,

    -- * InstanceStatusReason
    InstanceStatusReason (..),
    newInstanceStatusReason,
    instanceStatusReason_message,

    -- * InstanceStorageConfig
    InstanceStorageConfig (..),
    newInstanceStorageConfig,
    instanceStorageConfig_associationId,
    instanceStorageConfig_kinesisFirehoseConfig,
    instanceStorageConfig_kinesisStreamConfig,
    instanceStorageConfig_kinesisVideoStreamConfig,
    instanceStorageConfig_s3Config,
    instanceStorageConfig_storageType,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_arn,
    instanceSummary_createdTime,
    instanceSummary_id,
    instanceSummary_identityManagementType,
    instanceSummary_inboundCallsEnabled,
    instanceSummary_instanceAlias,
    instanceSummary_instanceStatus,
    instanceSummary_outboundCallsEnabled,
    instanceSummary_serviceRole,

    -- * IntegrationAssociationSummary
    IntegrationAssociationSummary (..),
    newIntegrationAssociationSummary,
    integrationAssociationSummary_instanceId,
    integrationAssociationSummary_integrationArn,
    integrationAssociationSummary_integrationAssociationArn,
    integrationAssociationSummary_integrationAssociationId,
    integrationAssociationSummary_integrationType,
    integrationAssociationSummary_sourceApplicationName,
    integrationAssociationSummary_sourceApplicationUrl,
    integrationAssociationSummary_sourceType,

    -- * InvisibleFieldInfo
    InvisibleFieldInfo (..),
    newInvisibleFieldInfo,
    invisibleFieldInfo_id,

    -- * KinesisFirehoseConfig
    KinesisFirehoseConfig (..),
    newKinesisFirehoseConfig,
    kinesisFirehoseConfig_firehoseArn,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    newKinesisStreamConfig,
    kinesisStreamConfig_streamArn,

    -- * KinesisVideoStreamConfig
    KinesisVideoStreamConfig (..),
    newKinesisVideoStreamConfig,
    kinesisVideoStreamConfig_prefix,
    kinesisVideoStreamConfig_retentionPeriodHours,
    kinesisVideoStreamConfig_encryptionConfig,

    -- * LexBot
    LexBot (..),
    newLexBot,
    lexBot_lexRegion,
    lexBot_name,

    -- * LexBotConfig
    LexBotConfig (..),
    newLexBotConfig,
    lexBotConfig_lexBot,
    lexBotConfig_lexV2Bot,

    -- * LexV2Bot
    LexV2Bot (..),
    newLexV2Bot,
    lexV2Bot_aliasArn,

    -- * ListPhoneNumbersSummary
    ListPhoneNumbersSummary (..),
    newListPhoneNumbersSummary,
    listPhoneNumbersSummary_phoneNumber,
    listPhoneNumbersSummary_phoneNumberArn,
    listPhoneNumbersSummary_phoneNumberCountryCode,
    listPhoneNumbersSummary_phoneNumberId,
    listPhoneNumbersSummary_phoneNumberType,
    listPhoneNumbersSummary_targetArn,

    -- * MediaConcurrency
    MediaConcurrency (..),
    newMediaConcurrency,
    mediaConcurrency_channel,
    mediaConcurrency_concurrency,

    -- * NotificationRecipientType
    NotificationRecipientType (..),
    newNotificationRecipientType,
    notificationRecipientType_userIds,
    notificationRecipientType_userTags,

    -- * NumberReference
    NumberReference (..),
    newNumberReference,
    numberReference_name,
    numberReference_value,

    -- * OutboundCallerConfig
    OutboundCallerConfig (..),
    newOutboundCallerConfig,
    outboundCallerConfig_outboundCallerIdName,
    outboundCallerConfig_outboundCallerIdNumberId,
    outboundCallerConfig_outboundFlowId,

    -- * ParticipantDetails
    ParticipantDetails (..),
    newParticipantDetails,
    participantDetails_displayName,

    -- * ParticipantTimerConfiguration
    ParticipantTimerConfiguration (..),
    newParticipantTimerConfiguration,
    participantTimerConfiguration_participantRole,
    participantTimerConfiguration_timerType,
    participantTimerConfiguration_timerValue,

    -- * ParticipantTimerValue
    ParticipantTimerValue (..),
    newParticipantTimerValue,
    participantTimerValue_participantTimerAction,
    participantTimerValue_participantTimerDurationInMinutes,

    -- * PhoneNumberQuickConnectConfig
    PhoneNumberQuickConnectConfig (..),
    newPhoneNumberQuickConnectConfig,
    phoneNumberQuickConnectConfig_phoneNumber,

    -- * PhoneNumberStatus
    PhoneNumberStatus (..),
    newPhoneNumberStatus,
    phoneNumberStatus_message,
    phoneNumberStatus_status,

    -- * PhoneNumberSummary
    PhoneNumberSummary (..),
    newPhoneNumberSummary,
    phoneNumberSummary_arn,
    phoneNumberSummary_id,
    phoneNumberSummary_phoneNumber,
    phoneNumberSummary_phoneNumberCountryCode,
    phoneNumberSummary_phoneNumberType,

    -- * PromptSummary
    PromptSummary (..),
    newPromptSummary,
    promptSummary_arn,
    promptSummary_id,
    promptSummary_name,

    -- * Queue
    Queue (..),
    newQueue,
    queue_description,
    queue_hoursOfOperationId,
    queue_maxContacts,
    queue_name,
    queue_outboundCallerConfig,
    queue_queueArn,
    queue_queueId,
    queue_status,
    queue_tags,

    -- * QueueInfo
    QueueInfo (..),
    newQueueInfo,
    queueInfo_enqueueTimestamp,
    queueInfo_id,

    -- * QueueQuickConnectConfig
    QueueQuickConnectConfig (..),
    newQueueQuickConnectConfig,
    queueQuickConnectConfig_queueId,
    queueQuickConnectConfig_contactFlowId,

    -- * QueueReference
    QueueReference (..),
    newQueueReference,
    queueReference_arn,
    queueReference_id,

    -- * QueueSearchCriteria
    QueueSearchCriteria (..),
    newQueueSearchCriteria,
    queueSearchCriteria_andConditions,
    queueSearchCriteria_orConditions,
    queueSearchCriteria_queueTypeCondition,
    queueSearchCriteria_stringCondition,

    -- * QueueSearchFilter
    QueueSearchFilter (..),
    newQueueSearchFilter,
    queueSearchFilter_tagFilter,

    -- * QueueSummary
    QueueSummary (..),
    newQueueSummary,
    queueSummary_arn,
    queueSummary_id,
    queueSummary_name,
    queueSummary_queueType,

    -- * QuickConnect
    QuickConnect (..),
    newQuickConnect,
    quickConnect_description,
    quickConnect_name,
    quickConnect_quickConnectARN,
    quickConnect_quickConnectConfig,
    quickConnect_quickConnectId,
    quickConnect_tags,

    -- * QuickConnectConfig
    QuickConnectConfig (..),
    newQuickConnectConfig,
    quickConnectConfig_phoneConfig,
    quickConnectConfig_queueConfig,
    quickConnectConfig_userConfig,
    quickConnectConfig_quickConnectType,

    -- * QuickConnectSummary
    QuickConnectSummary (..),
    newQuickConnectSummary,
    quickConnectSummary_arn,
    quickConnectSummary_id,
    quickConnectSummary_name,
    quickConnectSummary_quickConnectType,

    -- * ReadOnlyFieldInfo
    ReadOnlyFieldInfo (..),
    newReadOnlyFieldInfo,
    readOnlyFieldInfo_id,

    -- * Reference
    Reference (..),
    newReference,
    reference_value,
    reference_type,

    -- * ReferenceSummary
    ReferenceSummary (..),
    newReferenceSummary,
    referenceSummary_attachment,
    referenceSummary_date,
    referenceSummary_email,
    referenceSummary_number,
    referenceSummary_string,
    referenceSummary_url,

    -- * RequiredFieldInfo
    RequiredFieldInfo (..),
    newRequiredFieldInfo,
    requiredFieldInfo_id,

    -- * RoutingProfile
    RoutingProfile (..),
    newRoutingProfile,
    routingProfile_defaultOutboundQueueId,
    routingProfile_description,
    routingProfile_instanceId,
    routingProfile_mediaConcurrencies,
    routingProfile_name,
    routingProfile_numberOfAssociatedQueues,
    routingProfile_numberOfAssociatedUsers,
    routingProfile_routingProfileArn,
    routingProfile_routingProfileId,
    routingProfile_tags,

    -- * RoutingProfileQueueConfig
    RoutingProfileQueueConfig (..),
    newRoutingProfileQueueConfig,
    routingProfileQueueConfig_queueReference,
    routingProfileQueueConfig_priority,
    routingProfileQueueConfig_delay,

    -- * RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (..),
    newRoutingProfileQueueConfigSummary,
    routingProfileQueueConfigSummary_queueId,
    routingProfileQueueConfigSummary_queueArn,
    routingProfileQueueConfigSummary_queueName,
    routingProfileQueueConfigSummary_priority,
    routingProfileQueueConfigSummary_delay,
    routingProfileQueueConfigSummary_channel,

    -- * RoutingProfileQueueReference
    RoutingProfileQueueReference (..),
    newRoutingProfileQueueReference,
    routingProfileQueueReference_queueId,
    routingProfileQueueReference_channel,

    -- * RoutingProfileReference
    RoutingProfileReference (..),
    newRoutingProfileReference,
    routingProfileReference_arn,
    routingProfileReference_id,

    -- * RoutingProfileSearchCriteria
    RoutingProfileSearchCriteria (..),
    newRoutingProfileSearchCriteria,
    routingProfileSearchCriteria_andConditions,
    routingProfileSearchCriteria_orConditions,
    routingProfileSearchCriteria_stringCondition,

    -- * RoutingProfileSearchFilter
    RoutingProfileSearchFilter (..),
    newRoutingProfileSearchFilter,
    routingProfileSearchFilter_tagFilter,

    -- * RoutingProfileSummary
    RoutingProfileSummary (..),
    newRoutingProfileSummary,
    routingProfileSummary_arn,
    routingProfileSummary_id,
    routingProfileSummary_name,

    -- * Rule
    Rule (..),
    newRule,
    rule_tags,
    rule_name,
    rule_ruleId,
    rule_ruleArn,
    rule_triggerEventSource,
    rule_function,
    rule_actions,
    rule_publishStatus,
    rule_createdTime,
    rule_lastUpdatedTime,
    rule_lastUpdatedBy,

    -- * RuleAction
    RuleAction (..),
    newRuleAction,
    ruleAction_assignContactCategoryAction,
    ruleAction_eventBridgeAction,
    ruleAction_sendNotificationAction,
    ruleAction_taskAction,
    ruleAction_actionType,

    -- * RuleSummary
    RuleSummary (..),
    newRuleSummary,
    ruleSummary_name,
    ruleSummary_ruleId,
    ruleSummary_ruleArn,
    ruleSummary_eventSourceName,
    ruleSummary_publishStatus,
    ruleSummary_actionSummaries,
    ruleSummary_createdTime,
    ruleSummary_lastUpdatedTime,

    -- * RuleTriggerEventSource
    RuleTriggerEventSource (..),
    newRuleTriggerEventSource,
    ruleTriggerEventSource_integrationAssociationId,
    ruleTriggerEventSource_eventSourceName,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_encryptionConfig,
    s3Config_bucketName,
    s3Config_bucketPrefix,

    -- * SecurityKey
    SecurityKey (..),
    newSecurityKey,
    securityKey_associationId,
    securityKey_creationTime,
    securityKey_key,

    -- * SecurityProfile
    SecurityProfile (..),
    newSecurityProfile,
    securityProfile_allowedAccessControlTags,
    securityProfile_arn,
    securityProfile_description,
    securityProfile_id,
    securityProfile_organizationResourceId,
    securityProfile_securityProfileName,
    securityProfile_tagRestrictedResources,
    securityProfile_tags,

    -- * SecurityProfileSearchCriteria
    SecurityProfileSearchCriteria (..),
    newSecurityProfileSearchCriteria,
    securityProfileSearchCriteria_andConditions,
    securityProfileSearchCriteria_orConditions,
    securityProfileSearchCriteria_stringCondition,

    -- * SecurityProfileSearchSummary
    SecurityProfileSearchSummary (..),
    newSecurityProfileSearchSummary,
    securityProfileSearchSummary_arn,
    securityProfileSearchSummary_description,
    securityProfileSearchSummary_id,
    securityProfileSearchSummary_organizationResourceId,
    securityProfileSearchSummary_securityProfileName,
    securityProfileSearchSummary_tags,

    -- * SecurityProfileSummary
    SecurityProfileSummary (..),
    newSecurityProfileSummary,
    securityProfileSummary_arn,
    securityProfileSummary_id,
    securityProfileSummary_name,

    -- * SecurityProfilesSearchFilter
    SecurityProfilesSearchFilter (..),
    newSecurityProfilesSearchFilter,
    securityProfilesSearchFilter_tagFilter,

    -- * SendNotificationActionDefinition
    SendNotificationActionDefinition (..),
    newSendNotificationActionDefinition,
    sendNotificationActionDefinition_subject,
    sendNotificationActionDefinition_deliveryMethod,
    sendNotificationActionDefinition_content,
    sendNotificationActionDefinition_contentType,
    sendNotificationActionDefinition_recipient,

    -- * StringCondition
    StringCondition (..),
    newStringCondition,
    stringCondition_comparisonType,
    stringCondition_fieldName,
    stringCondition_value,

    -- * StringReference
    StringReference (..),
    newStringReference,
    stringReference_name,
    stringReference_value,

    -- * TagCondition
    TagCondition (..),
    newTagCondition,
    tagCondition_tagKey,
    tagCondition_tagValue,

    -- * TaskActionDefinition
    TaskActionDefinition (..),
    newTaskActionDefinition,
    taskActionDefinition_description,
    taskActionDefinition_references,
    taskActionDefinition_name,
    taskActionDefinition_contactFlowId,

    -- * TaskTemplateConstraints
    TaskTemplateConstraints (..),
    newTaskTemplateConstraints,
    taskTemplateConstraints_invisibleFields,
    taskTemplateConstraints_readOnlyFields,
    taskTemplateConstraints_requiredFields,

    -- * TaskTemplateDefaultFieldValue
    TaskTemplateDefaultFieldValue (..),
    newTaskTemplateDefaultFieldValue,
    taskTemplateDefaultFieldValue_defaultValue,
    taskTemplateDefaultFieldValue_id,

    -- * TaskTemplateDefaults
    TaskTemplateDefaults (..),
    newTaskTemplateDefaults,
    taskTemplateDefaults_defaultFieldValues,

    -- * TaskTemplateField
    TaskTemplateField (..),
    newTaskTemplateField,
    taskTemplateField_description,
    taskTemplateField_singleSelectOptions,
    taskTemplateField_type,
    taskTemplateField_id,

    -- * TaskTemplateFieldIdentifier
    TaskTemplateFieldIdentifier (..),
    newTaskTemplateFieldIdentifier,
    taskTemplateFieldIdentifier_name,

    -- * TaskTemplateMetadata
    TaskTemplateMetadata (..),
    newTaskTemplateMetadata,
    taskTemplateMetadata_arn,
    taskTemplateMetadata_createdTime,
    taskTemplateMetadata_description,
    taskTemplateMetadata_id,
    taskTemplateMetadata_lastModifiedTime,
    taskTemplateMetadata_name,
    taskTemplateMetadata_status,

    -- * TelephonyConfig
    TelephonyConfig (..),
    newTelephonyConfig,
    telephonyConfig_distributions,

    -- * Threshold
    Threshold (..),
    newThreshold,
    threshold_comparison,
    threshold_thresholdValue,

    -- * TrafficDistributionGroup
    TrafficDistributionGroup (..),
    newTrafficDistributionGroup,
    trafficDistributionGroup_arn,
    trafficDistributionGroup_description,
    trafficDistributionGroup_id,
    trafficDistributionGroup_instanceArn,
    trafficDistributionGroup_name,
    trafficDistributionGroup_status,
    trafficDistributionGroup_tags,

    -- * TrafficDistributionGroupSummary
    TrafficDistributionGroupSummary (..),
    newTrafficDistributionGroupSummary,
    trafficDistributionGroupSummary_arn,
    trafficDistributionGroupSummary_id,
    trafficDistributionGroupSummary_instanceArn,
    trafficDistributionGroupSummary_name,
    trafficDistributionGroupSummary_status,

    -- * UpdateParticipantRoleConfigChannelInfo
    UpdateParticipantRoleConfigChannelInfo (..),
    newUpdateParticipantRoleConfigChannelInfo,
    updateParticipantRoleConfigChannelInfo_chat,

    -- * UrlReference
    UrlReference (..),
    newUrlReference,
    urlReference_name,
    urlReference_value,

    -- * UseCase
    UseCase (..),
    newUseCase,
    useCase_useCaseArn,
    useCase_useCaseId,
    useCase_useCaseType,

    -- * User
    User (..),
    newUser,
    user_arn,
    user_directoryUserId,
    user_hierarchyGroupId,
    user_id,
    user_identityInfo,
    user_phoneConfig,
    user_routingProfileId,
    user_securityProfileIds,
    user_tags,
    user_username,

    -- * UserData
    UserData (..),
    newUserData,
    userData_activeSlotsByChannel,
    userData_availableSlotsByChannel,
    userData_contacts,
    userData_hierarchyPath,
    userData_maxSlotsByChannel,
    userData_nextStatus,
    userData_routingProfile,
    userData_status,
    userData_user,

    -- * UserDataFilters
    UserDataFilters (..),
    newUserDataFilters,
    userDataFilters_agents,
    userDataFilters_contactFilter,
    userDataFilters_queues,
    userDataFilters_routingProfiles,
    userDataFilters_userHierarchyGroups,

    -- * UserIdentityInfo
    UserIdentityInfo (..),
    newUserIdentityInfo,
    userIdentityInfo_email,
    userIdentityInfo_firstName,
    userIdentityInfo_lastName,
    userIdentityInfo_mobile,
    userIdentityInfo_secondaryEmail,

    -- * UserIdentityInfoLite
    UserIdentityInfoLite (..),
    newUserIdentityInfoLite,
    userIdentityInfoLite_firstName,
    userIdentityInfoLite_lastName,

    -- * UserPhoneConfig
    UserPhoneConfig (..),
    newUserPhoneConfig,
    userPhoneConfig_afterContactWorkTimeLimit,
    userPhoneConfig_autoAccept,
    userPhoneConfig_deskPhoneNumber,
    userPhoneConfig_phoneType,

    -- * UserQuickConnectConfig
    UserQuickConnectConfig (..),
    newUserQuickConnectConfig,
    userQuickConnectConfig_userId,
    userQuickConnectConfig_contactFlowId,

    -- * UserReference
    UserReference (..),
    newUserReference,
    userReference_arn,
    userReference_id,

    -- * UserSearchCriteria
    UserSearchCriteria (..),
    newUserSearchCriteria,
    userSearchCriteria_andConditions,
    userSearchCriteria_hierarchyGroupCondition,
    userSearchCriteria_orConditions,
    userSearchCriteria_stringCondition,

    -- * UserSearchFilter
    UserSearchFilter (..),
    newUserSearchFilter,
    userSearchFilter_tagFilter,

    -- * UserSearchSummary
    UserSearchSummary (..),
    newUserSearchSummary,
    userSearchSummary_arn,
    userSearchSummary_directoryUserId,
    userSearchSummary_hierarchyGroupId,
    userSearchSummary_id,
    userSearchSummary_identityInfo,
    userSearchSummary_phoneConfig,
    userSearchSummary_routingProfileId,
    userSearchSummary_securityProfileIds,
    userSearchSummary_tags,
    userSearchSummary_username,

    -- * UserSummary
    UserSummary (..),
    newUserSummary,
    userSummary_arn,
    userSummary_id,
    userSummary_username,

    -- * Vocabulary
    Vocabulary (..),
    newVocabulary,
    vocabulary_content,
    vocabulary_failureReason,
    vocabulary_tags,
    vocabulary_name,
    vocabulary_id,
    vocabulary_arn,
    vocabulary_languageCode,
    vocabulary_state,
    vocabulary_lastModifiedTime,

    -- * VocabularySummary
    VocabularySummary (..),
    newVocabularySummary,
    vocabularySummary_failureReason,
    vocabularySummary_name,
    vocabularySummary_id,
    vocabularySummary_arn,
    vocabularySummary_languageCode,
    vocabularySummary_state,
    vocabularySummary_lastModifiedTime,

    -- * VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    newVoiceRecordingConfiguration,
    voiceRecordingConfiguration_voiceRecordingTrack,
  )
where

import Amazonka.Connect.Types.ActionSummary
import Amazonka.Connect.Types.ActionType
import Amazonka.Connect.Types.AgentContactReference
import Amazonka.Connect.Types.AgentInfo
import Amazonka.Connect.Types.AgentStatus
import Amazonka.Connect.Types.AgentStatusReference
import Amazonka.Connect.Types.AgentStatusState
import Amazonka.Connect.Types.AgentStatusSummary
import Amazonka.Connect.Types.AgentStatusType
import Amazonka.Connect.Types.AnswerMachineDetectionConfig
import Amazonka.Connect.Types.AssignContactCategoryActionDefinition
import Amazonka.Connect.Types.AttachmentReference
import Amazonka.Connect.Types.Attribute
import Amazonka.Connect.Types.AvailableNumberSummary
import Amazonka.Connect.Types.Channel
import Amazonka.Connect.Types.ChatMessage
import Amazonka.Connect.Types.ChatParticipantRoleConfig
import Amazonka.Connect.Types.ChatStreamingConfiguration
import Amazonka.Connect.Types.ClaimedPhoneNumberSummary
import Amazonka.Connect.Types.Comparison
import Amazonka.Connect.Types.Contact
import Amazonka.Connect.Types.ContactFilter
import Amazonka.Connect.Types.ContactFlow
import Amazonka.Connect.Types.ContactFlowModule
import Amazonka.Connect.Types.ContactFlowModuleState
import Amazonka.Connect.Types.ContactFlowModuleStatus
import Amazonka.Connect.Types.ContactFlowModuleSummary
import Amazonka.Connect.Types.ContactFlowState
import Amazonka.Connect.Types.ContactFlowSummary
import Amazonka.Connect.Types.ContactFlowType
import Amazonka.Connect.Types.ContactInitiationMethod
import Amazonka.Connect.Types.ContactState
import Amazonka.Connect.Types.ControlPlaneTagFilter
import Amazonka.Connect.Types.Credentials
import Amazonka.Connect.Types.CurrentMetric
import Amazonka.Connect.Types.CurrentMetricData
import Amazonka.Connect.Types.CurrentMetricName
import Amazonka.Connect.Types.CurrentMetricResult
import Amazonka.Connect.Types.CurrentMetricSortCriteria
import Amazonka.Connect.Types.DateReference
import Amazonka.Connect.Types.DefaultVocabulary
import Amazonka.Connect.Types.Dimensions
import Amazonka.Connect.Types.DirectoryType
import Amazonka.Connect.Types.Distribution
import Amazonka.Connect.Types.EmailReference
import Amazonka.Connect.Types.EncryptionConfig
import Amazonka.Connect.Types.EncryptionType
import Amazonka.Connect.Types.EventBridgeActionDefinition
import Amazonka.Connect.Types.EventSourceName
import Amazonka.Connect.Types.Filters
import Amazonka.Connect.Types.Grouping
import Amazonka.Connect.Types.HierarchyGroup
import Amazonka.Connect.Types.HierarchyGroupCondition
import Amazonka.Connect.Types.HierarchyGroupMatchType
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
import Amazonka.Connect.Types.HistoricalMetricName
import Amazonka.Connect.Types.HistoricalMetricResult
import Amazonka.Connect.Types.HoursOfOperation
import Amazonka.Connect.Types.HoursOfOperationConfig
import Amazonka.Connect.Types.HoursOfOperationDays
import Amazonka.Connect.Types.HoursOfOperationSummary
import Amazonka.Connect.Types.HoursOfOperationTimeSlice
import Amazonka.Connect.Types.Instance
import Amazonka.Connect.Types.InstanceAttributeType
import Amazonka.Connect.Types.InstanceStatus
import Amazonka.Connect.Types.InstanceStatusReason
import Amazonka.Connect.Types.InstanceStorageConfig
import Amazonka.Connect.Types.InstanceStorageResourceType
import Amazonka.Connect.Types.InstanceSummary
import Amazonka.Connect.Types.IntegrationAssociationSummary
import Amazonka.Connect.Types.IntegrationType
import Amazonka.Connect.Types.InvisibleFieldInfo
import Amazonka.Connect.Types.KinesisFirehoseConfig
import Amazonka.Connect.Types.KinesisStreamConfig
import Amazonka.Connect.Types.KinesisVideoStreamConfig
import Amazonka.Connect.Types.LexBot
import Amazonka.Connect.Types.LexBotConfig
import Amazonka.Connect.Types.LexV2Bot
import Amazonka.Connect.Types.LexVersion
import Amazonka.Connect.Types.ListPhoneNumbersSummary
import Amazonka.Connect.Types.MediaConcurrency
import Amazonka.Connect.Types.MonitorCapability
import Amazonka.Connect.Types.NotificationContentType
import Amazonka.Connect.Types.NotificationDeliveryType
import Amazonka.Connect.Types.NotificationRecipientType
import Amazonka.Connect.Types.NumberReference
import Amazonka.Connect.Types.OutboundCallerConfig
import Amazonka.Connect.Types.ParticipantDetails
import Amazonka.Connect.Types.ParticipantTimerAction
import Amazonka.Connect.Types.ParticipantTimerConfiguration
import Amazonka.Connect.Types.ParticipantTimerType
import Amazonka.Connect.Types.ParticipantTimerValue
import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberQuickConnectConfig
import Amazonka.Connect.Types.PhoneNumberStatus
import Amazonka.Connect.Types.PhoneNumberSummary
import Amazonka.Connect.Types.PhoneNumberType
import Amazonka.Connect.Types.PhoneNumberWorkflowStatus
import Amazonka.Connect.Types.PhoneType
import Amazonka.Connect.Types.PromptSummary
import Amazonka.Connect.Types.Queue
import Amazonka.Connect.Types.QueueInfo
import Amazonka.Connect.Types.QueueQuickConnectConfig
import Amazonka.Connect.Types.QueueReference
import Amazonka.Connect.Types.QueueSearchCriteria
import Amazonka.Connect.Types.QueueSearchFilter
import Amazonka.Connect.Types.QueueStatus
import Amazonka.Connect.Types.QueueSummary
import Amazonka.Connect.Types.QueueType
import Amazonka.Connect.Types.QuickConnect
import Amazonka.Connect.Types.QuickConnectConfig
import Amazonka.Connect.Types.QuickConnectSummary
import Amazonka.Connect.Types.QuickConnectType
import Amazonka.Connect.Types.ReadOnlyFieldInfo
import Amazonka.Connect.Types.Reference
import Amazonka.Connect.Types.ReferenceStatus
import Amazonka.Connect.Types.ReferenceSummary
import Amazonka.Connect.Types.ReferenceType
import Amazonka.Connect.Types.RequiredFieldInfo
import Amazonka.Connect.Types.RoutingProfile
import Amazonka.Connect.Types.RoutingProfileQueueConfig
import Amazonka.Connect.Types.RoutingProfileQueueConfigSummary
import Amazonka.Connect.Types.RoutingProfileQueueReference
import Amazonka.Connect.Types.RoutingProfileReference
import Amazonka.Connect.Types.RoutingProfileSearchCriteria
import Amazonka.Connect.Types.RoutingProfileSearchFilter
import Amazonka.Connect.Types.RoutingProfileSummary
import Amazonka.Connect.Types.Rule
import Amazonka.Connect.Types.RuleAction
import Amazonka.Connect.Types.RulePublishStatus
import Amazonka.Connect.Types.RuleSummary
import Amazonka.Connect.Types.RuleTriggerEventSource
import Amazonka.Connect.Types.S3Config
import Amazonka.Connect.Types.SearchableQueueType
import Amazonka.Connect.Types.SecurityKey
import Amazonka.Connect.Types.SecurityProfile
import Amazonka.Connect.Types.SecurityProfileSearchCriteria
import Amazonka.Connect.Types.SecurityProfileSearchSummary
import Amazonka.Connect.Types.SecurityProfileSummary
import Amazonka.Connect.Types.SecurityProfilesSearchFilter
import Amazonka.Connect.Types.SendNotificationActionDefinition
import Amazonka.Connect.Types.SortOrder
import Amazonka.Connect.Types.SourceType
import Amazonka.Connect.Types.Statistic
import Amazonka.Connect.Types.StorageType
import Amazonka.Connect.Types.StringComparisonType
import Amazonka.Connect.Types.StringCondition
import Amazonka.Connect.Types.StringReference
import Amazonka.Connect.Types.TagCondition
import Amazonka.Connect.Types.TaskActionDefinition
import Amazonka.Connect.Types.TaskTemplateConstraints
import Amazonka.Connect.Types.TaskTemplateDefaultFieldValue
import Amazonka.Connect.Types.TaskTemplateDefaults
import Amazonka.Connect.Types.TaskTemplateField
import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import Amazonka.Connect.Types.TaskTemplateFieldType
import Amazonka.Connect.Types.TaskTemplateMetadata
import Amazonka.Connect.Types.TaskTemplateStatus
import Amazonka.Connect.Types.TelephonyConfig
import Amazonka.Connect.Types.Threshold
import Amazonka.Connect.Types.TimerEligibleParticipantRoles
import Amazonka.Connect.Types.TrafficDistributionGroup
import Amazonka.Connect.Types.TrafficDistributionGroupStatus
import Amazonka.Connect.Types.TrafficDistributionGroupSummary
import Amazonka.Connect.Types.TrafficType
import Amazonka.Connect.Types.Unit
import Amazonka.Connect.Types.UpdateParticipantRoleConfigChannelInfo
import Amazonka.Connect.Types.UrlReference
import Amazonka.Connect.Types.UseCase
import Amazonka.Connect.Types.UseCaseType
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
import Amazonka.Connect.Types.VocabularyLanguageCode
import Amazonka.Connect.Types.VocabularyState
import Amazonka.Connect.Types.VocabularySummary
import Amazonka.Connect.Types.VoiceRecordingConfiguration
import Amazonka.Connect.Types.VoiceRecordingTrack
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Connect",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "connect",
      Core.signingName = "connect",
      Core.version = "2017-08-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Connect",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The flow has not been published.
_ContactFlowNotPublishedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ContactFlowNotPublishedException =
  Core._MatchServiceError
    defaultService
    "ContactFlowNotPublishedException"
    Prelude.. Core.hasStatus 404

-- | The contact with the specified ID is not active or does not exist.
-- Applies to Voice calls only, not to Chat, Task, or Voice Callback.
_ContactNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ContactNotFoundException =
  Core._MatchServiceError
    defaultService
    "ContactNotFoundException"
    Prelude.. Core.hasStatus 410

-- | Outbound calls to the destination number are not allowed.
_DestinationNotAllowedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DestinationNotAllowedException =
  Core._MatchServiceError
    defaultService
    "DestinationNotAllowedException"
    Prelude.. Core.hasStatus 403

-- | A resource with the specified name already exists.
_DuplicateResourceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"
    Prelude.. Core.hasStatus 409

-- | An entity with the same name already exists.
_IdempotencyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotencyException =
  Core._MatchServiceError
    defaultService
    "IdempotencyException"
    Prelude.. Core.hasStatus 409

-- | Request processing failed because of an error or failure with the
-- service.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The flow is not valid.
_InvalidContactFlowException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidContactFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidContactFlowException"
    Prelude.. Core.hasStatus 400

-- | The problems with the module. Please fix before trying again.
_InvalidContactFlowModuleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidContactFlowModuleException =
  Core._MatchServiceError
    defaultService
    "InvalidContactFlowModuleException"
    Prelude.. Core.hasStatus 400

-- | One or more of the specified parameters are not valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The request is not valid.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The allowed limit for the resource has been exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | The contact is not permitted.
_OutboundContactNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OutboundContactNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OutboundContactNotPermittedException"
    Prelude.. Core.hasStatus 403

-- | The property is not valid.
_PropertyValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PropertyValidationException =
  Core._MatchServiceError
    defaultService
    "PropertyValidationException"
    Prelude.. Core.hasStatus 400

-- | A resource already has that name.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | That resource is already in use. Please try another.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | The specified resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource is not ready.
_ResourceNotReadyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 409

-- | The service quota has been exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The throttling limit has been exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | No user with the specified credentials was found in the Amazon Connect
-- instance.
_UserNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"
    Prelude.. Core.hasStatus 404

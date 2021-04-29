{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Connect is a cloud-based contact center solution that you use to
-- set up and manage a customer contact center and provide reliable
-- customer engagement at any scale.
--
-- Amazon Connect provides metrics and real-time reporting that enable you
-- to optimize contact routing. You can also resolve customer issues more
-- efficiently by getting customers in touch with the appropriate agents.
--
-- There are limits to the number of Amazon Connect resources that you can
-- create. There are also limits to the number of requests that you can
-- make per second. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas>
-- in the /Amazon Connect Administrator Guide/.
--
-- You can connect programmatically to an AWS service by using an endpoint.
-- For a list of Amazon Connect endpoints, see
-- <https://docs.aws.amazon.com/general/latest/gr/connect_region.html Amazon Connect Endpoints>.
--
-- Working with contact flows? Check out the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Network.AWS.Connect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** OutboundContactNotPermittedException
    _OutboundContactNotPermittedException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ContactFlowNotPublishedException
    _ContactFlowNotPublishedException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ContactNotFoundException
    _ContactNotFoundException,

    -- ** InvalidContactFlowException
    _InvalidContactFlowException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** DestinationNotAllowedException
    _DestinationNotAllowedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SuspendContactRecording
    SuspendContactRecording (SuspendContactRecording'),
    newSuspendContactRecording,
    SuspendContactRecordingResponse (SuspendContactRecordingResponse'),
    newSuspendContactRecordingResponse,

    -- ** UpdateQueueName
    UpdateQueueName (UpdateQueueName'),
    newUpdateQueueName,
    UpdateQueueNameResponse (UpdateQueueNameResponse'),
    newUpdateQueueNameResponse,

    -- ** CreateQuickConnect
    CreateQuickConnect (CreateQuickConnect'),
    newCreateQuickConnect,
    CreateQuickConnectResponse (CreateQuickConnectResponse'),
    newCreateQuickConnectResponse,

    -- ** UpdateUserRoutingProfile
    UpdateUserRoutingProfile (UpdateUserRoutingProfile'),
    newUpdateUserRoutingProfile,
    UpdateUserRoutingProfileResponse (UpdateUserRoutingProfileResponse'),
    newUpdateUserRoutingProfileResponse,

    -- ** UpdateUserHierarchyGroupName
    UpdateUserHierarchyGroupName (UpdateUserHierarchyGroupName'),
    newUpdateUserHierarchyGroupName,
    UpdateUserHierarchyGroupNameResponse (UpdateUserHierarchyGroupNameResponse'),
    newUpdateUserHierarchyGroupNameResponse,

    -- ** UpdateContactFlowName
    UpdateContactFlowName (UpdateContactFlowName'),
    newUpdateContactFlowName,
    UpdateContactFlowNameResponse (UpdateContactFlowNameResponse'),
    newUpdateContactFlowNameResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** DescribeInstance
    DescribeInstance (DescribeInstance'),
    newDescribeInstance,
    DescribeInstanceResponse (DescribeInstanceResponse'),
    newDescribeInstanceResponse,

    -- ** ListInstanceAttributes (Paginated)
    ListInstanceAttributes (ListInstanceAttributes'),
    newListInstanceAttributes,
    ListInstanceAttributesResponse (ListInstanceAttributesResponse'),
    newListInstanceAttributesResponse,

    -- ** ListLambdaFunctions (Paginated)
    ListLambdaFunctions (ListLambdaFunctions'),
    newListLambdaFunctions,
    ListLambdaFunctionsResponse (ListLambdaFunctionsResponse'),
    newListLambdaFunctionsResponse,

    -- ** UpdateRoutingProfileQueues
    UpdateRoutingProfileQueues (UpdateRoutingProfileQueues'),
    newUpdateRoutingProfileQueues,
    UpdateRoutingProfileQueuesResponse (UpdateRoutingProfileQueuesResponse'),
    newUpdateRoutingProfileQueuesResponse,

    -- ** AssociateRoutingProfileQueues
    AssociateRoutingProfileQueues (AssociateRoutingProfileQueues'),
    newAssociateRoutingProfileQueues,
    AssociateRoutingProfileQueuesResponse (AssociateRoutingProfileQueuesResponse'),
    newAssociateRoutingProfileQueuesResponse,

    -- ** GetContactAttributes
    GetContactAttributes (GetContactAttributes'),
    newGetContactAttributes,
    GetContactAttributesResponse (GetContactAttributesResponse'),
    newGetContactAttributesResponse,

    -- ** ListLexBots (Paginated)
    ListLexBots (ListLexBots'),
    newListLexBots,
    ListLexBotsResponse (ListLexBotsResponse'),
    newListLexBotsResponse,

    -- ** AssociateLambdaFunction
    AssociateLambdaFunction (AssociateLambdaFunction'),
    newAssociateLambdaFunction,
    AssociateLambdaFunctionResponse (AssociateLambdaFunctionResponse'),
    newAssociateLambdaFunctionResponse,

    -- ** ListApprovedOrigins (Paginated)
    ListApprovedOrigins (ListApprovedOrigins'),
    newListApprovedOrigins,
    ListApprovedOriginsResponse (ListApprovedOriginsResponse'),
    newListApprovedOriginsResponse,

    -- ** AssociateInstanceStorageConfig
    AssociateInstanceStorageConfig (AssociateInstanceStorageConfig'),
    newAssociateInstanceStorageConfig,
    AssociateInstanceStorageConfigResponse (AssociateInstanceStorageConfigResponse'),
    newAssociateInstanceStorageConfigResponse,

    -- ** CreateContactFlow
    CreateContactFlow (CreateContactFlow'),
    newCreateContactFlow,
    CreateContactFlowResponse (CreateContactFlowResponse'),
    newCreateContactFlowResponse,

    -- ** UpdateUserPhoneConfig
    UpdateUserPhoneConfig (UpdateUserPhoneConfig'),
    newUpdateUserPhoneConfig,
    UpdateUserPhoneConfigResponse (UpdateUserPhoneConfigResponse'),
    newUpdateUserPhoneConfigResponse,

    -- ** UpdateContactAttributes
    UpdateContactAttributes (UpdateContactAttributes'),
    newUpdateContactAttributes,
    UpdateContactAttributesResponse (UpdateContactAttributesResponse'),
    newUpdateContactAttributesResponse,

    -- ** ListRoutingProfiles (Paginated)
    ListRoutingProfiles (ListRoutingProfiles'),
    newListRoutingProfiles,
    ListRoutingProfilesResponse (ListRoutingProfilesResponse'),
    newListRoutingProfilesResponse,

    -- ** DeleteUseCase
    DeleteUseCase (DeleteUseCase'),
    newDeleteUseCase,
    DeleteUseCaseResponse (DeleteUseCaseResponse'),
    newDeleteUseCaseResponse,

    -- ** DescribeQuickConnect
    DescribeQuickConnect (DescribeQuickConnect'),
    newDescribeQuickConnect,
    DescribeQuickConnectResponse (DescribeQuickConnectResponse'),
    newDescribeQuickConnectResponse,

    -- ** ListQueueQuickConnects (Paginated)
    ListQueueQuickConnects (ListQueueQuickConnects'),
    newListQueueQuickConnects,
    ListQueueQuickConnectsResponse (ListQueueQuickConnectsResponse'),
    newListQueueQuickConnectsResponse,

    -- ** CreateRoutingProfile
    CreateRoutingProfile (CreateRoutingProfile'),
    newCreateRoutingProfile,
    CreateRoutingProfileResponse (CreateRoutingProfileResponse'),
    newCreateRoutingProfileResponse,

    -- ** AssociateApprovedOrigin
    AssociateApprovedOrigin (AssociateApprovedOrigin'),
    newAssociateApprovedOrigin,
    AssociateApprovedOriginResponse (AssociateApprovedOriginResponse'),
    newAssociateApprovedOriginResponse,

    -- ** DisassociateQueueQuickConnects
    DisassociateQueueQuickConnects (DisassociateQueueQuickConnects'),
    newDisassociateQueueQuickConnects,
    DisassociateQueueQuickConnectsResponse (DisassociateQueueQuickConnectsResponse'),
    newDisassociateQueueQuickConnectsResponse,

    -- ** UpdateRoutingProfileConcurrency
    UpdateRoutingProfileConcurrency (UpdateRoutingProfileConcurrency'),
    newUpdateRoutingProfileConcurrency,
    UpdateRoutingProfileConcurrencyResponse (UpdateRoutingProfileConcurrencyResponse'),
    newUpdateRoutingProfileConcurrencyResponse,

    -- ** UpdateQueueOutboundCallerConfig
    UpdateQueueOutboundCallerConfig (UpdateQueueOutboundCallerConfig'),
    newUpdateQueueOutboundCallerConfig,
    UpdateQueueOutboundCallerConfigResponse (UpdateQueueOutboundCallerConfigResponse'),
    newUpdateQueueOutboundCallerConfigResponse,

    -- ** DisassociateSecurityKey
    DisassociateSecurityKey (DisassociateSecurityKey'),
    newDisassociateSecurityKey,
    DisassociateSecurityKeyResponse (DisassociateSecurityKeyResponse'),
    newDisassociateSecurityKeyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetCurrentMetricData
    GetCurrentMetricData (GetCurrentMetricData'),
    newGetCurrentMetricData,
    GetCurrentMetricDataResponse (GetCurrentMetricDataResponse'),
    newGetCurrentMetricDataResponse,

    -- ** UpdateQuickConnectConfig
    UpdateQuickConnectConfig (UpdateQuickConnectConfig'),
    newUpdateQuickConnectConfig,
    UpdateQuickConnectConfigResponse (UpdateQuickConnectConfigResponse'),
    newUpdateQuickConnectConfigResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** StopContact
    StopContact (StopContact'),
    newStopContact,
    StopContactResponse (StopContactResponse'),
    newStopContactResponse,

    -- ** CreateUserHierarchyGroup
    CreateUserHierarchyGroup (CreateUserHierarchyGroup'),
    newCreateUserHierarchyGroup,
    CreateUserHierarchyGroupResponse (CreateUserHierarchyGroupResponse'),
    newCreateUserHierarchyGroupResponse,

    -- ** StartContactRecording
    StartContactRecording (StartContactRecording'),
    newStartContactRecording,
    StartContactRecordingResponse (StartContactRecordingResponse'),
    newStartContactRecordingResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** AssociateSecurityKey
    AssociateSecurityKey (AssociateSecurityKey'),
    newAssociateSecurityKey,
    AssociateSecurityKeyResponse (AssociateSecurityKeyResponse'),
    newAssociateSecurityKeyResponse,

    -- ** AssociateQueueQuickConnects
    AssociateQueueQuickConnects (AssociateQueueQuickConnects'),
    newAssociateQueueQuickConnects,
    AssociateQueueQuickConnectsResponse (AssociateQueueQuickConnectsResponse'),
    newAssociateQueueQuickConnectsResponse,

    -- ** StopContactRecording
    StopContactRecording (StopContactRecording'),
    newStopContactRecording,
    StopContactRecordingResponse (StopContactRecordingResponse'),
    newStopContactRecordingResponse,

    -- ** DisassociateApprovedOrigin
    DisassociateApprovedOrigin (DisassociateApprovedOrigin'),
    newDisassociateApprovedOrigin,
    DisassociateApprovedOriginResponse (DisassociateApprovedOriginResponse'),
    newDisassociateApprovedOriginResponse,

    -- ** UpdateQuickConnectName
    UpdateQuickConnectName (UpdateQuickConnectName'),
    newUpdateQuickConnectName,
    UpdateQuickConnectNameResponse (UpdateQuickConnectNameResponse'),
    newUpdateQuickConnectNameResponse,

    -- ** DescribeRoutingProfile
    DescribeRoutingProfile (DescribeRoutingProfile'),
    newDescribeRoutingProfile,
    DescribeRoutingProfileResponse (DescribeRoutingProfileResponse'),
    newDescribeRoutingProfileResponse,

    -- ** ListQuickConnects (Paginated)
    ListQuickConnects (ListQuickConnects'),
    newListQuickConnects,
    ListQuickConnectsResponse (ListQuickConnectsResponse'),
    newListQuickConnectsResponse,

    -- ** DisassociateLexBot
    DisassociateLexBot (DisassociateLexBot'),
    newDisassociateLexBot,
    DisassociateLexBotResponse (DisassociateLexBotResponse'),
    newDisassociateLexBotResponse,

    -- ** DeleteQuickConnect
    DeleteQuickConnect (DeleteQuickConnect'),
    newDeleteQuickConnect,
    DeleteQuickConnectResponse (DeleteQuickConnectResponse'),
    newDeleteQuickConnectResponse,

    -- ** ListRoutingProfileQueues (Paginated)
    ListRoutingProfileQueues (ListRoutingProfileQueues'),
    newListRoutingProfileQueues,
    ListRoutingProfileQueuesResponse (ListRoutingProfileQueuesResponse'),
    newListRoutingProfileQueuesResponse,

    -- ** UpdateUserHierarchy
    UpdateUserHierarchy (UpdateUserHierarchy'),
    newUpdateUserHierarchy,
    UpdateUserHierarchyResponse (UpdateUserHierarchyResponse'),
    newUpdateUserHierarchyResponse,

    -- ** DisassociateLambdaFunction
    DisassociateLambdaFunction (DisassociateLambdaFunction'),
    newDisassociateLambdaFunction,
    DisassociateLambdaFunctionResponse (DisassociateLambdaFunctionResponse'),
    newDisassociateLambdaFunctionResponse,

    -- ** UpdateQueueMaxContacts
    UpdateQueueMaxContacts (UpdateQueueMaxContacts'),
    newUpdateQueueMaxContacts,
    UpdateQueueMaxContactsResponse (UpdateQueueMaxContactsResponse'),
    newUpdateQueueMaxContactsResponse,

    -- ** DescribeInstanceStorageConfig
    DescribeInstanceStorageConfig (DescribeInstanceStorageConfig'),
    newDescribeInstanceStorageConfig,
    DescribeInstanceStorageConfigResponse (DescribeInstanceStorageConfigResponse'),
    newDescribeInstanceStorageConfigResponse,

    -- ** UpdateQueueHoursOfOperation
    UpdateQueueHoursOfOperation (UpdateQueueHoursOfOperation'),
    newUpdateQueueHoursOfOperation,
    UpdateQueueHoursOfOperationResponse (UpdateQueueHoursOfOperationResponse'),
    newUpdateQueueHoursOfOperationResponse,

    -- ** DisassociateRoutingProfileQueues
    DisassociateRoutingProfileQueues (DisassociateRoutingProfileQueues'),
    newDisassociateRoutingProfileQueues,
    DisassociateRoutingProfileQueuesResponse (DisassociateRoutingProfileQueuesResponse'),
    newDisassociateRoutingProfileQueuesResponse,

    -- ** DescribeContactFlow
    DescribeContactFlow (DescribeContactFlow'),
    newDescribeContactFlow,
    DescribeContactFlowResponse (DescribeContactFlowResponse'),
    newDescribeContactFlowResponse,

    -- ** UpdateQueueStatus
    UpdateQueueStatus (UpdateQueueStatus'),
    newUpdateQueueStatus,
    UpdateQueueStatusResponse (UpdateQueueStatusResponse'),
    newUpdateQueueStatusResponse,

    -- ** DescribeQueue
    DescribeQueue (DescribeQueue'),
    newDescribeQueue,
    DescribeQueueResponse (DescribeQueueResponse'),
    newDescribeQueueResponse,

    -- ** AssociateLexBot
    AssociateLexBot (AssociateLexBot'),
    newAssociateLexBot,
    AssociateLexBotResponse (AssociateLexBotResponse'),
    newAssociateLexBotResponse,

    -- ** UpdateInstanceAttribute
    UpdateInstanceAttribute (UpdateInstanceAttribute'),
    newUpdateInstanceAttribute,
    UpdateInstanceAttributeResponse (UpdateInstanceAttributeResponse'),
    newUpdateInstanceAttributeResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** DescribeUserHierarchyGroup
    DescribeUserHierarchyGroup (DescribeUserHierarchyGroup'),
    newDescribeUserHierarchyGroup,
    DescribeUserHierarchyGroupResponse (DescribeUserHierarchyGroupResponse'),
    newDescribeUserHierarchyGroupResponse,

    -- ** ResumeContactRecording
    ResumeContactRecording (ResumeContactRecording'),
    newResumeContactRecording,
    ResumeContactRecordingResponse (ResumeContactRecordingResponse'),
    newResumeContactRecordingResponse,

    -- ** UpdateRoutingProfileName
    UpdateRoutingProfileName (UpdateRoutingProfileName'),
    newUpdateRoutingProfileName,
    UpdateRoutingProfileNameResponse (UpdateRoutingProfileNameResponse'),
    newUpdateRoutingProfileNameResponse,

    -- ** StartChatContact
    StartChatContact (StartChatContact'),
    newStartChatContact,
    StartChatContactResponse (StartChatContactResponse'),
    newStartChatContactResponse,

    -- ** DeleteIntegrationAssociation
    DeleteIntegrationAssociation (DeleteIntegrationAssociation'),
    newDeleteIntegrationAssociation,
    DeleteIntegrationAssociationResponse (DeleteIntegrationAssociationResponse'),
    newDeleteIntegrationAssociationResponse,

    -- ** ListPhoneNumbers (Paginated)
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** ListIntegrationAssociations (Paginated)
    ListIntegrationAssociations (ListIntegrationAssociations'),
    newListIntegrationAssociations,
    ListIntegrationAssociationsResponse (ListIntegrationAssociationsResponse'),
    newListIntegrationAssociationsResponse,

    -- ** ListUseCases (Paginated)
    ListUseCases (ListUseCases'),
    newListUseCases,
    ListUseCasesResponse (ListUseCasesResponse'),
    newListUseCasesResponse,

    -- ** UpdateUserSecurityProfiles
    UpdateUserSecurityProfiles (UpdateUserSecurityProfiles'),
    newUpdateUserSecurityProfiles,
    UpdateUserSecurityProfilesResponse (UpdateUserSecurityProfilesResponse'),
    newUpdateUserSecurityProfilesResponse,

    -- ** DescribeUserHierarchyStructure
    DescribeUserHierarchyStructure (DescribeUserHierarchyStructure'),
    newDescribeUserHierarchyStructure,
    DescribeUserHierarchyStructureResponse (DescribeUserHierarchyStructureResponse'),
    newDescribeUserHierarchyStructureResponse,

    -- ** ListHoursOfOperations (Paginated)
    ListHoursOfOperations (ListHoursOfOperations'),
    newListHoursOfOperations,
    ListHoursOfOperationsResponse (ListHoursOfOperationsResponse'),
    newListHoursOfOperationsResponse,

    -- ** CreateUseCase
    CreateUseCase (CreateUseCase'),
    newCreateUseCase,
    CreateUseCaseResponse (CreateUseCaseResponse'),
    newCreateUseCaseResponse,

    -- ** ListContactFlows (Paginated)
    ListContactFlows (ListContactFlows'),
    newListContactFlows,
    ListContactFlowsResponse (ListContactFlowsResponse'),
    newListContactFlowsResponse,

    -- ** UpdateInstanceStorageConfig
    UpdateInstanceStorageConfig (UpdateInstanceStorageConfig'),
    newUpdateInstanceStorageConfig,
    UpdateInstanceStorageConfigResponse (UpdateInstanceStorageConfigResponse'),
    newUpdateInstanceStorageConfigResponse,

    -- ** ListInstanceStorageConfigs (Paginated)
    ListInstanceStorageConfigs (ListInstanceStorageConfigs'),
    newListInstanceStorageConfigs,
    ListInstanceStorageConfigsResponse (ListInstanceStorageConfigsResponse'),
    newListInstanceStorageConfigsResponse,

    -- ** CreateIntegrationAssociation
    CreateIntegrationAssociation (CreateIntegrationAssociation'),
    newCreateIntegrationAssociation,
    CreateIntegrationAssociationResponse (CreateIntegrationAssociationResponse'),
    newCreateIntegrationAssociationResponse,

    -- ** DeleteUserHierarchyGroup
    DeleteUserHierarchyGroup (DeleteUserHierarchyGroup'),
    newDeleteUserHierarchyGroup,
    DeleteUserHierarchyGroupResponse (DeleteUserHierarchyGroupResponse'),
    newDeleteUserHierarchyGroupResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DisassociateInstanceStorageConfig
    DisassociateInstanceStorageConfig (DisassociateInstanceStorageConfig'),
    newDisassociateInstanceStorageConfig,
    DisassociateInstanceStorageConfigResponse (DisassociateInstanceStorageConfigResponse'),
    newDisassociateInstanceStorageConfigResponse,

    -- ** ListUserHierarchyGroups (Paginated)
    ListUserHierarchyGroups (ListUserHierarchyGroups'),
    newListUserHierarchyGroups,
    ListUserHierarchyGroupsResponse (ListUserHierarchyGroupsResponse'),
    newListUserHierarchyGroupsResponse,

    -- ** UpdateUserIdentityInfo
    UpdateUserIdentityInfo (UpdateUserIdentityInfo'),
    newUpdateUserIdentityInfo,
    UpdateUserIdentityInfoResponse (UpdateUserIdentityInfoResponse'),
    newUpdateUserIdentityInfoResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** GetFederationToken
    GetFederationToken (GetFederationToken'),
    newGetFederationToken,
    GetFederationTokenResponse (GetFederationTokenResponse'),
    newGetFederationTokenResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** ListSecurityKeys (Paginated)
    ListSecurityKeys (ListSecurityKeys'),
    newListSecurityKeys,
    ListSecurityKeysResponse (ListSecurityKeysResponse'),
    newListSecurityKeysResponse,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    UpdateRoutingProfileDefaultOutboundQueue (UpdateRoutingProfileDefaultOutboundQueue'),
    newUpdateRoutingProfileDefaultOutboundQueue,
    UpdateRoutingProfileDefaultOutboundQueueResponse (UpdateRoutingProfileDefaultOutboundQueueResponse'),
    newUpdateRoutingProfileDefaultOutboundQueueResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** CreateInstance
    CreateInstance (CreateInstance'),
    newCreateInstance,
    CreateInstanceResponse (CreateInstanceResponse'),
    newCreateInstanceResponse,

    -- ** StartTaskContact
    StartTaskContact (StartTaskContact'),
    newStartTaskContact,
    StartTaskContactResponse (StartTaskContactResponse'),
    newStartTaskContactResponse,

    -- ** ListPrompts (Paginated)
    ListPrompts (ListPrompts'),
    newListPrompts,
    ListPromptsResponse (ListPromptsResponse'),
    newListPromptsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartOutboundVoiceContact
    StartOutboundVoiceContact (StartOutboundVoiceContact'),
    newStartOutboundVoiceContact,
    StartOutboundVoiceContactResponse (StartOutboundVoiceContactResponse'),
    newStartOutboundVoiceContactResponse,

    -- ** UpdateUserHierarchyStructure
    UpdateUserHierarchyStructure (UpdateUserHierarchyStructure'),
    newUpdateUserHierarchyStructure,
    UpdateUserHierarchyStructureResponse (UpdateUserHierarchyStructureResponse'),
    newUpdateUserHierarchyStructureResponse,

    -- ** UpdateContactFlowContent
    UpdateContactFlowContent (UpdateContactFlowContent'),
    newUpdateContactFlowContent,
    UpdateContactFlowContentResponse (UpdateContactFlowContentResponse'),
    newUpdateContactFlowContentResponse,

    -- ** GetMetricData (Paginated)
    GetMetricData (GetMetricData'),
    newGetMetricData,
    GetMetricDataResponse (GetMetricDataResponse'),
    newGetMetricDataResponse,

    -- ** DescribeHoursOfOperation
    DescribeHoursOfOperation (DescribeHoursOfOperation'),
    newDescribeHoursOfOperation,
    DescribeHoursOfOperationResponse (DescribeHoursOfOperationResponse'),
    newDescribeHoursOfOperationResponse,

    -- * Types

    -- ** Channel
    Channel (..),

    -- ** Comparison
    Comparison (..),

    -- ** ContactFlowType
    ContactFlowType (..),

    -- ** CurrentMetricName
    CurrentMetricName (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** Grouping
    Grouping (..),

    -- ** HistoricalMetricName
    HistoricalMetricName (..),

    -- ** HoursOfOperationDays
    HoursOfOperationDays (..),

    -- ** InstanceAttributeType
    InstanceAttributeType (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- ** IntegrationType
    IntegrationType (..),

    -- ** PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** PhoneType
    PhoneType (..),

    -- ** QueueStatus
    QueueStatus (..),

    -- ** QueueType
    QueueType (..),

    -- ** QuickConnectType
    QuickConnectType (..),

    -- ** ReferenceType
    ReferenceType (..),

    -- ** SourceType
    SourceType (..),

    -- ** Statistic
    Statistic (..),

    -- ** StorageType
    StorageType (..),

    -- ** Unit
    Unit (..),

    -- ** UseCaseType
    UseCaseType (..),

    -- ** VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** ChatMessage
    ChatMessage (ChatMessage'),
    newChatMessage,

    -- ** ContactFlow
    ContactFlow (ContactFlow'),
    newContactFlow,

    -- ** ContactFlowSummary
    ContactFlowSummary (ContactFlowSummary'),
    newContactFlowSummary,

    -- ** Credentials
    Credentials (Credentials'),
    newCredentials,

    -- ** CurrentMetric
    CurrentMetric (CurrentMetric'),
    newCurrentMetric,

    -- ** CurrentMetricData
    CurrentMetricData (CurrentMetricData'),
    newCurrentMetricData,

    -- ** CurrentMetricResult
    CurrentMetricResult (CurrentMetricResult'),
    newCurrentMetricResult,

    -- ** Dimensions
    Dimensions (Dimensions'),
    newDimensions,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** Filters
    Filters (Filters'),
    newFilters,

    -- ** HierarchyGroup
    HierarchyGroup (HierarchyGroup'),
    newHierarchyGroup,

    -- ** HierarchyGroupSummary
    HierarchyGroupSummary (HierarchyGroupSummary'),
    newHierarchyGroupSummary,

    -- ** HierarchyLevel
    HierarchyLevel (HierarchyLevel'),
    newHierarchyLevel,

    -- ** HierarchyLevelUpdate
    HierarchyLevelUpdate (HierarchyLevelUpdate'),
    newHierarchyLevelUpdate,

    -- ** HierarchyPath
    HierarchyPath (HierarchyPath'),
    newHierarchyPath,

    -- ** HierarchyStructure
    HierarchyStructure (HierarchyStructure'),
    newHierarchyStructure,

    -- ** HierarchyStructureUpdate
    HierarchyStructureUpdate (HierarchyStructureUpdate'),
    newHierarchyStructureUpdate,

    -- ** HistoricalMetric
    HistoricalMetric (HistoricalMetric'),
    newHistoricalMetric,

    -- ** HistoricalMetricData
    HistoricalMetricData (HistoricalMetricData'),
    newHistoricalMetricData,

    -- ** HistoricalMetricResult
    HistoricalMetricResult (HistoricalMetricResult'),
    newHistoricalMetricResult,

    -- ** HoursOfOperation
    HoursOfOperation (HoursOfOperation'),
    newHoursOfOperation,

    -- ** HoursOfOperationConfig
    HoursOfOperationConfig (HoursOfOperationConfig'),
    newHoursOfOperationConfig,

    -- ** HoursOfOperationSummary
    HoursOfOperationSummary (HoursOfOperationSummary'),
    newHoursOfOperationSummary,

    -- ** HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (HoursOfOperationTimeSlice'),
    newHoursOfOperationTimeSlice,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceStatusReason
    InstanceStatusReason (InstanceStatusReason'),
    newInstanceStatusReason,

    -- ** InstanceStorageConfig
    InstanceStorageConfig (InstanceStorageConfig'),
    newInstanceStorageConfig,

    -- ** InstanceSummary
    InstanceSummary (InstanceSummary'),
    newInstanceSummary,

    -- ** IntegrationAssociationSummary
    IntegrationAssociationSummary (IntegrationAssociationSummary'),
    newIntegrationAssociationSummary,

    -- ** KinesisFirehoseConfig
    KinesisFirehoseConfig (KinesisFirehoseConfig'),
    newKinesisFirehoseConfig,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (KinesisStreamConfig'),
    newKinesisStreamConfig,

    -- ** KinesisVideoStreamConfig
    KinesisVideoStreamConfig (KinesisVideoStreamConfig'),
    newKinesisVideoStreamConfig,

    -- ** LexBot
    LexBot (LexBot'),
    newLexBot,

    -- ** MediaConcurrency
    MediaConcurrency (MediaConcurrency'),
    newMediaConcurrency,

    -- ** OutboundCallerConfig
    OutboundCallerConfig (OutboundCallerConfig'),
    newOutboundCallerConfig,

    -- ** ParticipantDetails
    ParticipantDetails (ParticipantDetails'),
    newParticipantDetails,

    -- ** PhoneNumberQuickConnectConfig
    PhoneNumberQuickConnectConfig (PhoneNumberQuickConnectConfig'),
    newPhoneNumberQuickConnectConfig,

    -- ** PhoneNumberSummary
    PhoneNumberSummary (PhoneNumberSummary'),
    newPhoneNumberSummary,

    -- ** PromptSummary
    PromptSummary (PromptSummary'),
    newPromptSummary,

    -- ** Queue
    Queue (Queue'),
    newQueue,

    -- ** QueueQuickConnectConfig
    QueueQuickConnectConfig (QueueQuickConnectConfig'),
    newQueueQuickConnectConfig,

    -- ** QueueReference
    QueueReference (QueueReference'),
    newQueueReference,

    -- ** QueueSummary
    QueueSummary (QueueSummary'),
    newQueueSummary,

    -- ** QuickConnect
    QuickConnect (QuickConnect'),
    newQuickConnect,

    -- ** QuickConnectConfig
    QuickConnectConfig (QuickConnectConfig'),
    newQuickConnectConfig,

    -- ** QuickConnectSummary
    QuickConnectSummary (QuickConnectSummary'),
    newQuickConnectSummary,

    -- ** Reference
    Reference (Reference'),
    newReference,

    -- ** RoutingProfile
    RoutingProfile (RoutingProfile'),
    newRoutingProfile,

    -- ** RoutingProfileQueueConfig
    RoutingProfileQueueConfig (RoutingProfileQueueConfig'),
    newRoutingProfileQueueConfig,

    -- ** RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (RoutingProfileQueueConfigSummary'),
    newRoutingProfileQueueConfigSummary,

    -- ** RoutingProfileQueueReference
    RoutingProfileQueueReference (RoutingProfileQueueReference'),
    newRoutingProfileQueueReference,

    -- ** RoutingProfileSummary
    RoutingProfileSummary (RoutingProfileSummary'),
    newRoutingProfileSummary,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** SecurityKey
    SecurityKey (SecurityKey'),
    newSecurityKey,

    -- ** SecurityProfileSummary
    SecurityProfileSummary (SecurityProfileSummary'),
    newSecurityProfileSummary,

    -- ** Threshold
    Threshold (Threshold'),
    newThreshold,

    -- ** UseCase
    UseCase (UseCase'),
    newUseCase,

    -- ** User
    User (User'),
    newUser,

    -- ** UserIdentityInfo
    UserIdentityInfo (UserIdentityInfo'),
    newUserIdentityInfo,

    -- ** UserPhoneConfig
    UserPhoneConfig (UserPhoneConfig'),
    newUserPhoneConfig,

    -- ** UserQuickConnectConfig
    UserQuickConnectConfig (UserQuickConnectConfig'),
    newUserQuickConnectConfig,

    -- ** UserSummary
    UserSummary (UserSummary'),
    newUserSummary,

    -- ** VoiceRecordingConfiguration
    VoiceRecordingConfiguration (VoiceRecordingConfiguration'),
    newVoiceRecordingConfiguration,
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
import Network.AWS.Connect.Lens
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
import Network.AWS.Connect.Types
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
import Network.AWS.Connect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Connect'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

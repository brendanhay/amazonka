{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Connect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-08-08@ of the AWS service descriptions, licensed under Apache 2.0.
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
-- You can connect programmatically to an Amazon Web Services service by
-- using an endpoint. For a list of Amazon Connect endpoints, see
-- <https://docs.aws.amazon.com/general/latest/gr/connect_region.html Amazon Connect Endpoints>.
module Amazonka.Connect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ContactFlowNotPublishedException
    _ContactFlowNotPublishedException,

    -- ** ContactNotFoundException
    _ContactNotFoundException,

    -- ** DestinationNotAllowedException
    _DestinationNotAllowedException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** IdempotencyException
    _IdempotencyException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidContactFlowException
    _InvalidContactFlowException,

    -- ** InvalidContactFlowModuleException
    _InvalidContactFlowModuleException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** OutboundContactNotPermittedException
    _OutboundContactNotPermittedException,

    -- ** PropertyValidationException
    _PropertyValidationException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateApprovedOrigin
    AssociateApprovedOrigin (AssociateApprovedOrigin'),
    newAssociateApprovedOrigin,
    AssociateApprovedOriginResponse (AssociateApprovedOriginResponse'),
    newAssociateApprovedOriginResponse,

    -- ** AssociateBot
    AssociateBot (AssociateBot'),
    newAssociateBot,
    AssociateBotResponse (AssociateBotResponse'),
    newAssociateBotResponse,

    -- ** AssociateDefaultVocabulary
    AssociateDefaultVocabulary (AssociateDefaultVocabulary'),
    newAssociateDefaultVocabulary,
    AssociateDefaultVocabularyResponse (AssociateDefaultVocabularyResponse'),
    newAssociateDefaultVocabularyResponse,

    -- ** AssociateInstanceStorageConfig
    AssociateInstanceStorageConfig (AssociateInstanceStorageConfig'),
    newAssociateInstanceStorageConfig,
    AssociateInstanceStorageConfigResponse (AssociateInstanceStorageConfigResponse'),
    newAssociateInstanceStorageConfigResponse,

    -- ** AssociateLambdaFunction
    AssociateLambdaFunction (AssociateLambdaFunction'),
    newAssociateLambdaFunction,
    AssociateLambdaFunctionResponse (AssociateLambdaFunctionResponse'),
    newAssociateLambdaFunctionResponse,

    -- ** AssociateLexBot
    AssociateLexBot (AssociateLexBot'),
    newAssociateLexBot,
    AssociateLexBotResponse (AssociateLexBotResponse'),
    newAssociateLexBotResponse,

    -- ** AssociatePhoneNumberContactFlow
    AssociatePhoneNumberContactFlow (AssociatePhoneNumberContactFlow'),
    newAssociatePhoneNumberContactFlow,
    AssociatePhoneNumberContactFlowResponse (AssociatePhoneNumberContactFlowResponse'),
    newAssociatePhoneNumberContactFlowResponse,

    -- ** AssociateQueueQuickConnects
    AssociateQueueQuickConnects (AssociateQueueQuickConnects'),
    newAssociateQueueQuickConnects,
    AssociateQueueQuickConnectsResponse (AssociateQueueQuickConnectsResponse'),
    newAssociateQueueQuickConnectsResponse,

    -- ** AssociateRoutingProfileQueues
    AssociateRoutingProfileQueues (AssociateRoutingProfileQueues'),
    newAssociateRoutingProfileQueues,
    AssociateRoutingProfileQueuesResponse (AssociateRoutingProfileQueuesResponse'),
    newAssociateRoutingProfileQueuesResponse,

    -- ** AssociateSecurityKey
    AssociateSecurityKey (AssociateSecurityKey'),
    newAssociateSecurityKey,
    AssociateSecurityKeyResponse (AssociateSecurityKeyResponse'),
    newAssociateSecurityKeyResponse,

    -- ** ClaimPhoneNumber
    ClaimPhoneNumber (ClaimPhoneNumber'),
    newClaimPhoneNumber,
    ClaimPhoneNumberResponse (ClaimPhoneNumberResponse'),
    newClaimPhoneNumberResponse,

    -- ** CreateAgentStatus
    CreateAgentStatus (CreateAgentStatus'),
    newCreateAgentStatus,
    CreateAgentStatusResponse (CreateAgentStatusResponse'),
    newCreateAgentStatusResponse,

    -- ** CreateContactFlow
    CreateContactFlow (CreateContactFlow'),
    newCreateContactFlow,
    CreateContactFlowResponse (CreateContactFlowResponse'),
    newCreateContactFlowResponse,

    -- ** CreateContactFlowModule
    CreateContactFlowModule (CreateContactFlowModule'),
    newCreateContactFlowModule,
    CreateContactFlowModuleResponse (CreateContactFlowModuleResponse'),
    newCreateContactFlowModuleResponse,

    -- ** CreateHoursOfOperation
    CreateHoursOfOperation (CreateHoursOfOperation'),
    newCreateHoursOfOperation,
    CreateHoursOfOperationResponse (CreateHoursOfOperationResponse'),
    newCreateHoursOfOperationResponse,

    -- ** CreateInstance
    CreateInstance (CreateInstance'),
    newCreateInstance,
    CreateInstanceResponse (CreateInstanceResponse'),
    newCreateInstanceResponse,

    -- ** CreateIntegrationAssociation
    CreateIntegrationAssociation (CreateIntegrationAssociation'),
    newCreateIntegrationAssociation,
    CreateIntegrationAssociationResponse (CreateIntegrationAssociationResponse'),
    newCreateIntegrationAssociationResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** CreateQuickConnect
    CreateQuickConnect (CreateQuickConnect'),
    newCreateQuickConnect,
    CreateQuickConnectResponse (CreateQuickConnectResponse'),
    newCreateQuickConnectResponse,

    -- ** CreateRoutingProfile
    CreateRoutingProfile (CreateRoutingProfile'),
    newCreateRoutingProfile,
    CreateRoutingProfileResponse (CreateRoutingProfileResponse'),
    newCreateRoutingProfileResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** CreateSecurityProfile
    CreateSecurityProfile (CreateSecurityProfile'),
    newCreateSecurityProfile,
    CreateSecurityProfileResponse (CreateSecurityProfileResponse'),
    newCreateSecurityProfileResponse,

    -- ** CreateTaskTemplate
    CreateTaskTemplate (CreateTaskTemplate'),
    newCreateTaskTemplate,
    CreateTaskTemplateResponse (CreateTaskTemplateResponse'),
    newCreateTaskTemplateResponse,

    -- ** CreateTrafficDistributionGroup
    CreateTrafficDistributionGroup (CreateTrafficDistributionGroup'),
    newCreateTrafficDistributionGroup,
    CreateTrafficDistributionGroupResponse (CreateTrafficDistributionGroupResponse'),
    newCreateTrafficDistributionGroupResponse,

    -- ** CreateUseCase
    CreateUseCase (CreateUseCase'),
    newCreateUseCase,
    CreateUseCaseResponse (CreateUseCaseResponse'),
    newCreateUseCaseResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateUserHierarchyGroup
    CreateUserHierarchyGroup (CreateUserHierarchyGroup'),
    newCreateUserHierarchyGroup,
    CreateUserHierarchyGroupResponse (CreateUserHierarchyGroupResponse'),
    newCreateUserHierarchyGroupResponse,

    -- ** CreateVocabulary
    CreateVocabulary (CreateVocabulary'),
    newCreateVocabulary,
    CreateVocabularyResponse (CreateVocabularyResponse'),
    newCreateVocabularyResponse,

    -- ** DeleteContactFlow
    DeleteContactFlow (DeleteContactFlow'),
    newDeleteContactFlow,
    DeleteContactFlowResponse (DeleteContactFlowResponse'),
    newDeleteContactFlowResponse,

    -- ** DeleteContactFlowModule
    DeleteContactFlowModule (DeleteContactFlowModule'),
    newDeleteContactFlowModule,
    DeleteContactFlowModuleResponse (DeleteContactFlowModuleResponse'),
    newDeleteContactFlowModuleResponse,

    -- ** DeleteHoursOfOperation
    DeleteHoursOfOperation (DeleteHoursOfOperation'),
    newDeleteHoursOfOperation,
    DeleteHoursOfOperationResponse (DeleteHoursOfOperationResponse'),
    newDeleteHoursOfOperationResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** DeleteIntegrationAssociation
    DeleteIntegrationAssociation (DeleteIntegrationAssociation'),
    newDeleteIntegrationAssociation,
    DeleteIntegrationAssociationResponse (DeleteIntegrationAssociationResponse'),
    newDeleteIntegrationAssociationResponse,

    -- ** DeleteQuickConnect
    DeleteQuickConnect (DeleteQuickConnect'),
    newDeleteQuickConnect,
    DeleteQuickConnectResponse (DeleteQuickConnectResponse'),
    newDeleteQuickConnectResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DeleteSecurityProfile
    DeleteSecurityProfile (DeleteSecurityProfile'),
    newDeleteSecurityProfile,
    DeleteSecurityProfileResponse (DeleteSecurityProfileResponse'),
    newDeleteSecurityProfileResponse,

    -- ** DeleteTaskTemplate
    DeleteTaskTemplate (DeleteTaskTemplate'),
    newDeleteTaskTemplate,
    DeleteTaskTemplateResponse (DeleteTaskTemplateResponse'),
    newDeleteTaskTemplateResponse,

    -- ** DeleteTrafficDistributionGroup
    DeleteTrafficDistributionGroup (DeleteTrafficDistributionGroup'),
    newDeleteTrafficDistributionGroup,
    DeleteTrafficDistributionGroupResponse (DeleteTrafficDistributionGroupResponse'),
    newDeleteTrafficDistributionGroupResponse,

    -- ** DeleteUseCase
    DeleteUseCase (DeleteUseCase'),
    newDeleteUseCase,
    DeleteUseCaseResponse (DeleteUseCaseResponse'),
    newDeleteUseCaseResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DeleteUserHierarchyGroup
    DeleteUserHierarchyGroup (DeleteUserHierarchyGroup'),
    newDeleteUserHierarchyGroup,
    DeleteUserHierarchyGroupResponse (DeleteUserHierarchyGroupResponse'),
    newDeleteUserHierarchyGroupResponse,

    -- ** DeleteVocabulary
    DeleteVocabulary (DeleteVocabulary'),
    newDeleteVocabulary,
    DeleteVocabularyResponse (DeleteVocabularyResponse'),
    newDeleteVocabularyResponse,

    -- ** DescribeAgentStatus
    DescribeAgentStatus (DescribeAgentStatus'),
    newDescribeAgentStatus,
    DescribeAgentStatusResponse (DescribeAgentStatusResponse'),
    newDescribeAgentStatusResponse,

    -- ** DescribeContact
    DescribeContact (DescribeContact'),
    newDescribeContact,
    DescribeContactResponse (DescribeContactResponse'),
    newDescribeContactResponse,

    -- ** DescribeContactFlow
    DescribeContactFlow (DescribeContactFlow'),
    newDescribeContactFlow,
    DescribeContactFlowResponse (DescribeContactFlowResponse'),
    newDescribeContactFlowResponse,

    -- ** DescribeContactFlowModule
    DescribeContactFlowModule (DescribeContactFlowModule'),
    newDescribeContactFlowModule,
    DescribeContactFlowModuleResponse (DescribeContactFlowModuleResponse'),
    newDescribeContactFlowModuleResponse,

    -- ** DescribeHoursOfOperation
    DescribeHoursOfOperation (DescribeHoursOfOperation'),
    newDescribeHoursOfOperation,
    DescribeHoursOfOperationResponse (DescribeHoursOfOperationResponse'),
    newDescribeHoursOfOperationResponse,

    -- ** DescribeInstance
    DescribeInstance (DescribeInstance'),
    newDescribeInstance,
    DescribeInstanceResponse (DescribeInstanceResponse'),
    newDescribeInstanceResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** DescribeInstanceStorageConfig
    DescribeInstanceStorageConfig (DescribeInstanceStorageConfig'),
    newDescribeInstanceStorageConfig,
    DescribeInstanceStorageConfigResponse (DescribeInstanceStorageConfigResponse'),
    newDescribeInstanceStorageConfigResponse,

    -- ** DescribePhoneNumber
    DescribePhoneNumber (DescribePhoneNumber'),
    newDescribePhoneNumber,
    DescribePhoneNumberResponse (DescribePhoneNumberResponse'),
    newDescribePhoneNumberResponse,

    -- ** DescribeQueue
    DescribeQueue (DescribeQueue'),
    newDescribeQueue,
    DescribeQueueResponse (DescribeQueueResponse'),
    newDescribeQueueResponse,

    -- ** DescribeQuickConnect
    DescribeQuickConnect (DescribeQuickConnect'),
    newDescribeQuickConnect,
    DescribeQuickConnectResponse (DescribeQuickConnectResponse'),
    newDescribeQuickConnectResponse,

    -- ** DescribeRoutingProfile
    DescribeRoutingProfile (DescribeRoutingProfile'),
    newDescribeRoutingProfile,
    DescribeRoutingProfileResponse (DescribeRoutingProfileResponse'),
    newDescribeRoutingProfileResponse,

    -- ** DescribeRule
    DescribeRule (DescribeRule'),
    newDescribeRule,
    DescribeRuleResponse (DescribeRuleResponse'),
    newDescribeRuleResponse,

    -- ** DescribeSecurityProfile
    DescribeSecurityProfile (DescribeSecurityProfile'),
    newDescribeSecurityProfile,
    DescribeSecurityProfileResponse (DescribeSecurityProfileResponse'),
    newDescribeSecurityProfileResponse,

    -- ** DescribeTrafficDistributionGroup
    DescribeTrafficDistributionGroup (DescribeTrafficDistributionGroup'),
    newDescribeTrafficDistributionGroup,
    DescribeTrafficDistributionGroupResponse (DescribeTrafficDistributionGroupResponse'),
    newDescribeTrafficDistributionGroupResponse,

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

    -- ** DescribeUserHierarchyStructure
    DescribeUserHierarchyStructure (DescribeUserHierarchyStructure'),
    newDescribeUserHierarchyStructure,
    DescribeUserHierarchyStructureResponse (DescribeUserHierarchyStructureResponse'),
    newDescribeUserHierarchyStructureResponse,

    -- ** DescribeVocabulary
    DescribeVocabulary (DescribeVocabulary'),
    newDescribeVocabulary,
    DescribeVocabularyResponse (DescribeVocabularyResponse'),
    newDescribeVocabularyResponse,

    -- ** DisassociateApprovedOrigin
    DisassociateApprovedOrigin (DisassociateApprovedOrigin'),
    newDisassociateApprovedOrigin,
    DisassociateApprovedOriginResponse (DisassociateApprovedOriginResponse'),
    newDisassociateApprovedOriginResponse,

    -- ** DisassociateBot
    DisassociateBot (DisassociateBot'),
    newDisassociateBot,
    DisassociateBotResponse (DisassociateBotResponse'),
    newDisassociateBotResponse,

    -- ** DisassociateInstanceStorageConfig
    DisassociateInstanceStorageConfig (DisassociateInstanceStorageConfig'),
    newDisassociateInstanceStorageConfig,
    DisassociateInstanceStorageConfigResponse (DisassociateInstanceStorageConfigResponse'),
    newDisassociateInstanceStorageConfigResponse,

    -- ** DisassociateLambdaFunction
    DisassociateLambdaFunction (DisassociateLambdaFunction'),
    newDisassociateLambdaFunction,
    DisassociateLambdaFunctionResponse (DisassociateLambdaFunctionResponse'),
    newDisassociateLambdaFunctionResponse,

    -- ** DisassociateLexBot
    DisassociateLexBot (DisassociateLexBot'),
    newDisassociateLexBot,
    DisassociateLexBotResponse (DisassociateLexBotResponse'),
    newDisassociateLexBotResponse,

    -- ** DisassociatePhoneNumberContactFlow
    DisassociatePhoneNumberContactFlow (DisassociatePhoneNumberContactFlow'),
    newDisassociatePhoneNumberContactFlow,
    DisassociatePhoneNumberContactFlowResponse (DisassociatePhoneNumberContactFlowResponse'),
    newDisassociatePhoneNumberContactFlowResponse,

    -- ** DisassociateQueueQuickConnects
    DisassociateQueueQuickConnects (DisassociateQueueQuickConnects'),
    newDisassociateQueueQuickConnects,
    DisassociateQueueQuickConnectsResponse (DisassociateQueueQuickConnectsResponse'),
    newDisassociateQueueQuickConnectsResponse,

    -- ** DisassociateRoutingProfileQueues
    DisassociateRoutingProfileQueues (DisassociateRoutingProfileQueues'),
    newDisassociateRoutingProfileQueues,
    DisassociateRoutingProfileQueuesResponse (DisassociateRoutingProfileQueuesResponse'),
    newDisassociateRoutingProfileQueuesResponse,

    -- ** DisassociateSecurityKey
    DisassociateSecurityKey (DisassociateSecurityKey'),
    newDisassociateSecurityKey,
    DisassociateSecurityKeyResponse (DisassociateSecurityKeyResponse'),
    newDisassociateSecurityKeyResponse,

    -- ** DismissUserContact
    DismissUserContact (DismissUserContact'),
    newDismissUserContact,
    DismissUserContactResponse (DismissUserContactResponse'),
    newDismissUserContactResponse,

    -- ** GetContactAttributes
    GetContactAttributes (GetContactAttributes'),
    newGetContactAttributes,
    GetContactAttributesResponse (GetContactAttributesResponse'),
    newGetContactAttributesResponse,

    -- ** GetCurrentMetricData
    GetCurrentMetricData (GetCurrentMetricData'),
    newGetCurrentMetricData,
    GetCurrentMetricDataResponse (GetCurrentMetricDataResponse'),
    newGetCurrentMetricDataResponse,

    -- ** GetCurrentUserData
    GetCurrentUserData (GetCurrentUserData'),
    newGetCurrentUserData,
    GetCurrentUserDataResponse (GetCurrentUserDataResponse'),
    newGetCurrentUserDataResponse,

    -- ** GetFederationToken
    GetFederationToken (GetFederationToken'),
    newGetFederationToken,
    GetFederationTokenResponse (GetFederationTokenResponse'),
    newGetFederationTokenResponse,

    -- ** GetMetricData (Paginated)
    GetMetricData (GetMetricData'),
    newGetMetricData,
    GetMetricDataResponse (GetMetricDataResponse'),
    newGetMetricDataResponse,

    -- ** GetTaskTemplate
    GetTaskTemplate (GetTaskTemplate'),
    newGetTaskTemplate,
    GetTaskTemplateResponse (GetTaskTemplateResponse'),
    newGetTaskTemplateResponse,

    -- ** GetTrafficDistribution
    GetTrafficDistribution (GetTrafficDistribution'),
    newGetTrafficDistribution,
    GetTrafficDistributionResponse (GetTrafficDistributionResponse'),
    newGetTrafficDistributionResponse,

    -- ** ListAgentStatuses (Paginated)
    ListAgentStatuses (ListAgentStatuses'),
    newListAgentStatuses,
    ListAgentStatusesResponse (ListAgentStatusesResponse'),
    newListAgentStatusesResponse,

    -- ** ListApprovedOrigins (Paginated)
    ListApprovedOrigins (ListApprovedOrigins'),
    newListApprovedOrigins,
    ListApprovedOriginsResponse (ListApprovedOriginsResponse'),
    newListApprovedOriginsResponse,

    -- ** ListBots (Paginated)
    ListBots (ListBots'),
    newListBots,
    ListBotsResponse (ListBotsResponse'),
    newListBotsResponse,

    -- ** ListContactFlowModules (Paginated)
    ListContactFlowModules (ListContactFlowModules'),
    newListContactFlowModules,
    ListContactFlowModulesResponse (ListContactFlowModulesResponse'),
    newListContactFlowModulesResponse,

    -- ** ListContactFlows (Paginated)
    ListContactFlows (ListContactFlows'),
    newListContactFlows,
    ListContactFlowsResponse (ListContactFlowsResponse'),
    newListContactFlowsResponse,

    -- ** ListContactReferences (Paginated)
    ListContactReferences (ListContactReferences'),
    newListContactReferences,
    ListContactReferencesResponse (ListContactReferencesResponse'),
    newListContactReferencesResponse,

    -- ** ListDefaultVocabularies (Paginated)
    ListDefaultVocabularies (ListDefaultVocabularies'),
    newListDefaultVocabularies,
    ListDefaultVocabulariesResponse (ListDefaultVocabulariesResponse'),
    newListDefaultVocabulariesResponse,

    -- ** ListHoursOfOperations (Paginated)
    ListHoursOfOperations (ListHoursOfOperations'),
    newListHoursOfOperations,
    ListHoursOfOperationsResponse (ListHoursOfOperationsResponse'),
    newListHoursOfOperationsResponse,

    -- ** ListInstanceAttributes (Paginated)
    ListInstanceAttributes (ListInstanceAttributes'),
    newListInstanceAttributes,
    ListInstanceAttributesResponse (ListInstanceAttributesResponse'),
    newListInstanceAttributesResponse,

    -- ** ListInstanceStorageConfigs (Paginated)
    ListInstanceStorageConfigs (ListInstanceStorageConfigs'),
    newListInstanceStorageConfigs,
    ListInstanceStorageConfigsResponse (ListInstanceStorageConfigsResponse'),
    newListInstanceStorageConfigsResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** ListIntegrationAssociations (Paginated)
    ListIntegrationAssociations (ListIntegrationAssociations'),
    newListIntegrationAssociations,
    ListIntegrationAssociationsResponse (ListIntegrationAssociationsResponse'),
    newListIntegrationAssociationsResponse,

    -- ** ListLambdaFunctions (Paginated)
    ListLambdaFunctions (ListLambdaFunctions'),
    newListLambdaFunctions,
    ListLambdaFunctionsResponse (ListLambdaFunctionsResponse'),
    newListLambdaFunctionsResponse,

    -- ** ListLexBots (Paginated)
    ListLexBots (ListLexBots'),
    newListLexBots,
    ListLexBotsResponse (ListLexBotsResponse'),
    newListLexBotsResponse,

    -- ** ListPhoneNumbers (Paginated)
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** ListPhoneNumbersV2 (Paginated)
    ListPhoneNumbersV2 (ListPhoneNumbersV2'),
    newListPhoneNumbersV2,
    ListPhoneNumbersV2Response (ListPhoneNumbersV2Response'),
    newListPhoneNumbersV2Response,

    -- ** ListPrompts (Paginated)
    ListPrompts (ListPrompts'),
    newListPrompts,
    ListPromptsResponse (ListPromptsResponse'),
    newListPromptsResponse,

    -- ** ListQueueQuickConnects (Paginated)
    ListQueueQuickConnects (ListQueueQuickConnects'),
    newListQueueQuickConnects,
    ListQueueQuickConnectsResponse (ListQueueQuickConnectsResponse'),
    newListQueueQuickConnectsResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** ListQuickConnects (Paginated)
    ListQuickConnects (ListQuickConnects'),
    newListQuickConnects,
    ListQuickConnectsResponse (ListQuickConnectsResponse'),
    newListQuickConnectsResponse,

    -- ** ListRoutingProfileQueues (Paginated)
    ListRoutingProfileQueues (ListRoutingProfileQueues'),
    newListRoutingProfileQueues,
    ListRoutingProfileQueuesResponse (ListRoutingProfileQueuesResponse'),
    newListRoutingProfileQueuesResponse,

    -- ** ListRoutingProfiles (Paginated)
    ListRoutingProfiles (ListRoutingProfiles'),
    newListRoutingProfiles,
    ListRoutingProfilesResponse (ListRoutingProfilesResponse'),
    newListRoutingProfilesResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListSecurityKeys (Paginated)
    ListSecurityKeys (ListSecurityKeys'),
    newListSecurityKeys,
    ListSecurityKeysResponse (ListSecurityKeysResponse'),
    newListSecurityKeysResponse,

    -- ** ListSecurityProfilePermissions (Paginated)
    ListSecurityProfilePermissions (ListSecurityProfilePermissions'),
    newListSecurityProfilePermissions,
    ListSecurityProfilePermissionsResponse (ListSecurityProfilePermissionsResponse'),
    newListSecurityProfilePermissionsResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTaskTemplates (Paginated)
    ListTaskTemplates (ListTaskTemplates'),
    newListTaskTemplates,
    ListTaskTemplatesResponse (ListTaskTemplatesResponse'),
    newListTaskTemplatesResponse,

    -- ** ListTrafficDistributionGroups (Paginated)
    ListTrafficDistributionGroups (ListTrafficDistributionGroups'),
    newListTrafficDistributionGroups,
    ListTrafficDistributionGroupsResponse (ListTrafficDistributionGroupsResponse'),
    newListTrafficDistributionGroupsResponse,

    -- ** ListUseCases (Paginated)
    ListUseCases (ListUseCases'),
    newListUseCases,
    ListUseCasesResponse (ListUseCasesResponse'),
    newListUseCasesResponse,

    -- ** ListUserHierarchyGroups (Paginated)
    ListUserHierarchyGroups (ListUserHierarchyGroups'),
    newListUserHierarchyGroups,
    ListUserHierarchyGroupsResponse (ListUserHierarchyGroupsResponse'),
    newListUserHierarchyGroupsResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** MonitorContact
    MonitorContact (MonitorContact'),
    newMonitorContact,
    MonitorContactResponse (MonitorContactResponse'),
    newMonitorContactResponse,

    -- ** PutUserStatus
    PutUserStatus (PutUserStatus'),
    newPutUserStatus,
    PutUserStatusResponse (PutUserStatusResponse'),
    newPutUserStatusResponse,

    -- ** ReleasePhoneNumber
    ReleasePhoneNumber (ReleasePhoneNumber'),
    newReleasePhoneNumber,
    ReleasePhoneNumberResponse (ReleasePhoneNumberResponse'),
    newReleasePhoneNumberResponse,

    -- ** ReplicateInstance
    ReplicateInstance (ReplicateInstance'),
    newReplicateInstance,
    ReplicateInstanceResponse (ReplicateInstanceResponse'),
    newReplicateInstanceResponse,

    -- ** ResumeContactRecording
    ResumeContactRecording (ResumeContactRecording'),
    newResumeContactRecording,
    ResumeContactRecordingResponse (ResumeContactRecordingResponse'),
    newResumeContactRecordingResponse,

    -- ** SearchAvailablePhoneNumbers (Paginated)
    SearchAvailablePhoneNumbers (SearchAvailablePhoneNumbers'),
    newSearchAvailablePhoneNumbers,
    SearchAvailablePhoneNumbersResponse (SearchAvailablePhoneNumbersResponse'),
    newSearchAvailablePhoneNumbersResponse,

    -- ** SearchQueues (Paginated)
    SearchQueues (SearchQueues'),
    newSearchQueues,
    SearchQueuesResponse (SearchQueuesResponse'),
    newSearchQueuesResponse,

    -- ** SearchRoutingProfiles (Paginated)
    SearchRoutingProfiles (SearchRoutingProfiles'),
    newSearchRoutingProfiles,
    SearchRoutingProfilesResponse (SearchRoutingProfilesResponse'),
    newSearchRoutingProfilesResponse,

    -- ** SearchSecurityProfiles (Paginated)
    SearchSecurityProfiles (SearchSecurityProfiles'),
    newSearchSecurityProfiles,
    SearchSecurityProfilesResponse (SearchSecurityProfilesResponse'),
    newSearchSecurityProfilesResponse,

    -- ** SearchUsers (Paginated)
    SearchUsers (SearchUsers'),
    newSearchUsers,
    SearchUsersResponse (SearchUsersResponse'),
    newSearchUsersResponse,

    -- ** SearchVocabularies (Paginated)
    SearchVocabularies (SearchVocabularies'),
    newSearchVocabularies,
    SearchVocabulariesResponse (SearchVocabulariesResponse'),
    newSearchVocabulariesResponse,

    -- ** StartChatContact
    StartChatContact (StartChatContact'),
    newStartChatContact,
    StartChatContactResponse (StartChatContactResponse'),
    newStartChatContactResponse,

    -- ** StartContactRecording
    StartContactRecording (StartContactRecording'),
    newStartContactRecording,
    StartContactRecordingResponse (StartContactRecordingResponse'),
    newStartContactRecordingResponse,

    -- ** StartContactStreaming
    StartContactStreaming (StartContactStreaming'),
    newStartContactStreaming,
    StartContactStreamingResponse (StartContactStreamingResponse'),
    newStartContactStreamingResponse,

    -- ** StartOutboundVoiceContact
    StartOutboundVoiceContact (StartOutboundVoiceContact'),
    newStartOutboundVoiceContact,
    StartOutboundVoiceContactResponse (StartOutboundVoiceContactResponse'),
    newStartOutboundVoiceContactResponse,

    -- ** StartTaskContact
    StartTaskContact (StartTaskContact'),
    newStartTaskContact,
    StartTaskContactResponse (StartTaskContactResponse'),
    newStartTaskContactResponse,

    -- ** StopContact
    StopContact (StopContact'),
    newStopContact,
    StopContactResponse (StopContactResponse'),
    newStopContactResponse,

    -- ** StopContactRecording
    StopContactRecording (StopContactRecording'),
    newStopContactRecording,
    StopContactRecordingResponse (StopContactRecordingResponse'),
    newStopContactRecordingResponse,

    -- ** StopContactStreaming
    StopContactStreaming (StopContactStreaming'),
    newStopContactStreaming,
    StopContactStreamingResponse (StopContactStreamingResponse'),
    newStopContactStreamingResponse,

    -- ** SuspendContactRecording
    SuspendContactRecording (SuspendContactRecording'),
    newSuspendContactRecording,
    SuspendContactRecordingResponse (SuspendContactRecordingResponse'),
    newSuspendContactRecordingResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TransferContact
    TransferContact (TransferContact'),
    newTransferContact,
    TransferContactResponse (TransferContactResponse'),
    newTransferContactResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAgentStatus
    UpdateAgentStatus (UpdateAgentStatus'),
    newUpdateAgentStatus,
    UpdateAgentStatusResponse (UpdateAgentStatusResponse'),
    newUpdateAgentStatusResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** UpdateContactAttributes
    UpdateContactAttributes (UpdateContactAttributes'),
    newUpdateContactAttributes,
    UpdateContactAttributesResponse (UpdateContactAttributesResponse'),
    newUpdateContactAttributesResponse,

    -- ** UpdateContactFlowContent
    UpdateContactFlowContent (UpdateContactFlowContent'),
    newUpdateContactFlowContent,
    UpdateContactFlowContentResponse (UpdateContactFlowContentResponse'),
    newUpdateContactFlowContentResponse,

    -- ** UpdateContactFlowMetadata
    UpdateContactFlowMetadata (UpdateContactFlowMetadata'),
    newUpdateContactFlowMetadata,
    UpdateContactFlowMetadataResponse (UpdateContactFlowMetadataResponse'),
    newUpdateContactFlowMetadataResponse,

    -- ** UpdateContactFlowModuleContent
    UpdateContactFlowModuleContent (UpdateContactFlowModuleContent'),
    newUpdateContactFlowModuleContent,
    UpdateContactFlowModuleContentResponse (UpdateContactFlowModuleContentResponse'),
    newUpdateContactFlowModuleContentResponse,

    -- ** UpdateContactFlowModuleMetadata
    UpdateContactFlowModuleMetadata (UpdateContactFlowModuleMetadata'),
    newUpdateContactFlowModuleMetadata,
    UpdateContactFlowModuleMetadataResponse (UpdateContactFlowModuleMetadataResponse'),
    newUpdateContactFlowModuleMetadataResponse,

    -- ** UpdateContactFlowName
    UpdateContactFlowName (UpdateContactFlowName'),
    newUpdateContactFlowName,
    UpdateContactFlowNameResponse (UpdateContactFlowNameResponse'),
    newUpdateContactFlowNameResponse,

    -- ** UpdateContactSchedule
    UpdateContactSchedule (UpdateContactSchedule'),
    newUpdateContactSchedule,
    UpdateContactScheduleResponse (UpdateContactScheduleResponse'),
    newUpdateContactScheduleResponse,

    -- ** UpdateHoursOfOperation
    UpdateHoursOfOperation (UpdateHoursOfOperation'),
    newUpdateHoursOfOperation,
    UpdateHoursOfOperationResponse (UpdateHoursOfOperationResponse'),
    newUpdateHoursOfOperationResponse,

    -- ** UpdateInstanceAttribute
    UpdateInstanceAttribute (UpdateInstanceAttribute'),
    newUpdateInstanceAttribute,
    UpdateInstanceAttributeResponse (UpdateInstanceAttributeResponse'),
    newUpdateInstanceAttributeResponse,

    -- ** UpdateInstanceStorageConfig
    UpdateInstanceStorageConfig (UpdateInstanceStorageConfig'),
    newUpdateInstanceStorageConfig,
    UpdateInstanceStorageConfigResponse (UpdateInstanceStorageConfigResponse'),
    newUpdateInstanceStorageConfigResponse,

    -- ** UpdateParticipantRoleConfig
    UpdateParticipantRoleConfig (UpdateParticipantRoleConfig'),
    newUpdateParticipantRoleConfig,
    UpdateParticipantRoleConfigResponse (UpdateParticipantRoleConfigResponse'),
    newUpdateParticipantRoleConfigResponse,

    -- ** UpdatePhoneNumber
    UpdatePhoneNumber (UpdatePhoneNumber'),
    newUpdatePhoneNumber,
    UpdatePhoneNumberResponse (UpdatePhoneNumberResponse'),
    newUpdatePhoneNumberResponse,

    -- ** UpdateQueueHoursOfOperation
    UpdateQueueHoursOfOperation (UpdateQueueHoursOfOperation'),
    newUpdateQueueHoursOfOperation,
    UpdateQueueHoursOfOperationResponse (UpdateQueueHoursOfOperationResponse'),
    newUpdateQueueHoursOfOperationResponse,

    -- ** UpdateQueueMaxContacts
    UpdateQueueMaxContacts (UpdateQueueMaxContacts'),
    newUpdateQueueMaxContacts,
    UpdateQueueMaxContactsResponse (UpdateQueueMaxContactsResponse'),
    newUpdateQueueMaxContactsResponse,

    -- ** UpdateQueueName
    UpdateQueueName (UpdateQueueName'),
    newUpdateQueueName,
    UpdateQueueNameResponse (UpdateQueueNameResponse'),
    newUpdateQueueNameResponse,

    -- ** UpdateQueueOutboundCallerConfig
    UpdateQueueOutboundCallerConfig (UpdateQueueOutboundCallerConfig'),
    newUpdateQueueOutboundCallerConfig,
    UpdateQueueOutboundCallerConfigResponse (UpdateQueueOutboundCallerConfigResponse'),
    newUpdateQueueOutboundCallerConfigResponse,

    -- ** UpdateQueueStatus
    UpdateQueueStatus (UpdateQueueStatus'),
    newUpdateQueueStatus,
    UpdateQueueStatusResponse (UpdateQueueStatusResponse'),
    newUpdateQueueStatusResponse,

    -- ** UpdateQuickConnectConfig
    UpdateQuickConnectConfig (UpdateQuickConnectConfig'),
    newUpdateQuickConnectConfig,
    UpdateQuickConnectConfigResponse (UpdateQuickConnectConfigResponse'),
    newUpdateQuickConnectConfigResponse,

    -- ** UpdateQuickConnectName
    UpdateQuickConnectName (UpdateQuickConnectName'),
    newUpdateQuickConnectName,
    UpdateQuickConnectNameResponse (UpdateQuickConnectNameResponse'),
    newUpdateQuickConnectNameResponse,

    -- ** UpdateRoutingProfileConcurrency
    UpdateRoutingProfileConcurrency (UpdateRoutingProfileConcurrency'),
    newUpdateRoutingProfileConcurrency,
    UpdateRoutingProfileConcurrencyResponse (UpdateRoutingProfileConcurrencyResponse'),
    newUpdateRoutingProfileConcurrencyResponse,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    UpdateRoutingProfileDefaultOutboundQueue (UpdateRoutingProfileDefaultOutboundQueue'),
    newUpdateRoutingProfileDefaultOutboundQueue,
    UpdateRoutingProfileDefaultOutboundQueueResponse (UpdateRoutingProfileDefaultOutboundQueueResponse'),
    newUpdateRoutingProfileDefaultOutboundQueueResponse,

    -- ** UpdateRoutingProfileName
    UpdateRoutingProfileName (UpdateRoutingProfileName'),
    newUpdateRoutingProfileName,
    UpdateRoutingProfileNameResponse (UpdateRoutingProfileNameResponse'),
    newUpdateRoutingProfileNameResponse,

    -- ** UpdateRoutingProfileQueues
    UpdateRoutingProfileQueues (UpdateRoutingProfileQueues'),
    newUpdateRoutingProfileQueues,
    UpdateRoutingProfileQueuesResponse (UpdateRoutingProfileQueuesResponse'),
    newUpdateRoutingProfileQueuesResponse,

    -- ** UpdateRule
    UpdateRule (UpdateRule'),
    newUpdateRule,
    UpdateRuleResponse (UpdateRuleResponse'),
    newUpdateRuleResponse,

    -- ** UpdateSecurityProfile
    UpdateSecurityProfile (UpdateSecurityProfile'),
    newUpdateSecurityProfile,
    UpdateSecurityProfileResponse (UpdateSecurityProfileResponse'),
    newUpdateSecurityProfileResponse,

    -- ** UpdateTaskTemplate
    UpdateTaskTemplate (UpdateTaskTemplate'),
    newUpdateTaskTemplate,
    UpdateTaskTemplateResponse (UpdateTaskTemplateResponse'),
    newUpdateTaskTemplateResponse,

    -- ** UpdateTrafficDistribution
    UpdateTrafficDistribution (UpdateTrafficDistribution'),
    newUpdateTrafficDistribution,
    UpdateTrafficDistributionResponse (UpdateTrafficDistributionResponse'),
    newUpdateTrafficDistributionResponse,

    -- ** UpdateUserHierarchy
    UpdateUserHierarchy (UpdateUserHierarchy'),
    newUpdateUserHierarchy,
    UpdateUserHierarchyResponse (UpdateUserHierarchyResponse'),
    newUpdateUserHierarchyResponse,

    -- ** UpdateUserHierarchyGroupName
    UpdateUserHierarchyGroupName (UpdateUserHierarchyGroupName'),
    newUpdateUserHierarchyGroupName,
    UpdateUserHierarchyGroupNameResponse (UpdateUserHierarchyGroupNameResponse'),
    newUpdateUserHierarchyGroupNameResponse,

    -- ** UpdateUserHierarchyStructure
    UpdateUserHierarchyStructure (UpdateUserHierarchyStructure'),
    newUpdateUserHierarchyStructure,
    UpdateUserHierarchyStructureResponse (UpdateUserHierarchyStructureResponse'),
    newUpdateUserHierarchyStructureResponse,

    -- ** UpdateUserIdentityInfo
    UpdateUserIdentityInfo (UpdateUserIdentityInfo'),
    newUpdateUserIdentityInfo,
    UpdateUserIdentityInfoResponse (UpdateUserIdentityInfoResponse'),
    newUpdateUserIdentityInfoResponse,

    -- ** UpdateUserPhoneConfig
    UpdateUserPhoneConfig (UpdateUserPhoneConfig'),
    newUpdateUserPhoneConfig,
    UpdateUserPhoneConfigResponse (UpdateUserPhoneConfigResponse'),
    newUpdateUserPhoneConfigResponse,

    -- ** UpdateUserRoutingProfile
    UpdateUserRoutingProfile (UpdateUserRoutingProfile'),
    newUpdateUserRoutingProfile,
    UpdateUserRoutingProfileResponse (UpdateUserRoutingProfileResponse'),
    newUpdateUserRoutingProfileResponse,

    -- ** UpdateUserSecurityProfiles
    UpdateUserSecurityProfiles (UpdateUserSecurityProfiles'),
    newUpdateUserSecurityProfiles,
    UpdateUserSecurityProfilesResponse (UpdateUserSecurityProfilesResponse'),
    newUpdateUserSecurityProfilesResponse,

    -- * Types

    -- ** ActionType
    ActionType (..),

    -- ** AgentStatusState
    AgentStatusState (..),

    -- ** AgentStatusType
    AgentStatusType (..),

    -- ** Channel
    Channel (..),

    -- ** Comparison
    Comparison (..),

    -- ** ContactFlowModuleState
    ContactFlowModuleState (..),

    -- ** ContactFlowModuleStatus
    ContactFlowModuleStatus (..),

    -- ** ContactFlowState
    ContactFlowState (..),

    -- ** ContactFlowType
    ContactFlowType (..),

    -- ** ContactInitiationMethod
    ContactInitiationMethod (..),

    -- ** ContactState
    ContactState (..),

    -- ** CurrentMetricName
    CurrentMetricName (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** EventSourceName
    EventSourceName (..),

    -- ** Grouping
    Grouping (..),

    -- ** HierarchyGroupMatchType
    HierarchyGroupMatchType (..),

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

    -- ** LexVersion
    LexVersion (..),

    -- ** MonitorCapability
    MonitorCapability (..),

    -- ** NotificationContentType
    NotificationContentType (..),

    -- ** NotificationDeliveryType
    NotificationDeliveryType (..),

    -- ** ParticipantTimerAction
    ParticipantTimerAction (..),

    -- ** ParticipantTimerType
    ParticipantTimerType (..),

    -- ** PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** PhoneNumberWorkflowStatus
    PhoneNumberWorkflowStatus (..),

    -- ** PhoneType
    PhoneType (..),

    -- ** QueueStatus
    QueueStatus (..),

    -- ** QueueType
    QueueType (..),

    -- ** QuickConnectType
    QuickConnectType (..),

    -- ** ReferenceStatus
    ReferenceStatus (..),

    -- ** ReferenceType
    ReferenceType (..),

    -- ** RulePublishStatus
    RulePublishStatus (..),

    -- ** SearchableQueueType
    SearchableQueueType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SourceType
    SourceType (..),

    -- ** Statistic
    Statistic (..),

    -- ** StorageType
    StorageType (..),

    -- ** StringComparisonType
    StringComparisonType (..),

    -- ** TaskTemplateFieldType
    TaskTemplateFieldType (..),

    -- ** TaskTemplateStatus
    TaskTemplateStatus (..),

    -- ** TimerEligibleParticipantRoles
    TimerEligibleParticipantRoles (..),

    -- ** TrafficDistributionGroupStatus
    TrafficDistributionGroupStatus (..),

    -- ** TrafficType
    TrafficType (..),

    -- ** Unit
    Unit (..),

    -- ** UseCaseType
    UseCaseType (..),

    -- ** VocabularyLanguageCode
    VocabularyLanguageCode (..),

    -- ** VocabularyState
    VocabularyState (..),

    -- ** VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- ** ActionSummary
    ActionSummary (ActionSummary'),
    newActionSummary,

    -- ** AgentContactReference
    AgentContactReference (AgentContactReference'),
    newAgentContactReference,

    -- ** AgentInfo
    AgentInfo (AgentInfo'),
    newAgentInfo,

    -- ** AgentStatus
    AgentStatus (AgentStatus'),
    newAgentStatus,

    -- ** AgentStatusReference
    AgentStatusReference (AgentStatusReference'),
    newAgentStatusReference,

    -- ** AgentStatusSummary
    AgentStatusSummary (AgentStatusSummary'),
    newAgentStatusSummary,

    -- ** AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (AnswerMachineDetectionConfig'),
    newAnswerMachineDetectionConfig,

    -- ** AssignContactCategoryActionDefinition
    AssignContactCategoryActionDefinition (AssignContactCategoryActionDefinition'),
    newAssignContactCategoryActionDefinition,

    -- ** AttachmentReference
    AttachmentReference (AttachmentReference'),
    newAttachmentReference,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** AvailableNumberSummary
    AvailableNumberSummary (AvailableNumberSummary'),
    newAvailableNumberSummary,

    -- ** ChatMessage
    ChatMessage (ChatMessage'),
    newChatMessage,

    -- ** ChatParticipantRoleConfig
    ChatParticipantRoleConfig (ChatParticipantRoleConfig'),
    newChatParticipantRoleConfig,

    -- ** ChatStreamingConfiguration
    ChatStreamingConfiguration (ChatStreamingConfiguration'),
    newChatStreamingConfiguration,

    -- ** ClaimedPhoneNumberSummary
    ClaimedPhoneNumberSummary (ClaimedPhoneNumberSummary'),
    newClaimedPhoneNumberSummary,

    -- ** Contact
    Contact (Contact'),
    newContact,

    -- ** ContactFilter
    ContactFilter (ContactFilter'),
    newContactFilter,

    -- ** ContactFlow
    ContactFlow (ContactFlow'),
    newContactFlow,

    -- ** ContactFlowModule
    ContactFlowModule (ContactFlowModule'),
    newContactFlowModule,

    -- ** ContactFlowModuleSummary
    ContactFlowModuleSummary (ContactFlowModuleSummary'),
    newContactFlowModuleSummary,

    -- ** ContactFlowSummary
    ContactFlowSummary (ContactFlowSummary'),
    newContactFlowSummary,

    -- ** ControlPlaneTagFilter
    ControlPlaneTagFilter (ControlPlaneTagFilter'),
    newControlPlaneTagFilter,

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

    -- ** CurrentMetricSortCriteria
    CurrentMetricSortCriteria (CurrentMetricSortCriteria'),
    newCurrentMetricSortCriteria,

    -- ** DateReference
    DateReference (DateReference'),
    newDateReference,

    -- ** DefaultVocabulary
    DefaultVocabulary (DefaultVocabulary'),
    newDefaultVocabulary,

    -- ** Dimensions
    Dimensions (Dimensions'),
    newDimensions,

    -- ** Distribution
    Distribution (Distribution'),
    newDistribution,

    -- ** EmailReference
    EmailReference (EmailReference'),
    newEmailReference,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** EventBridgeActionDefinition
    EventBridgeActionDefinition (EventBridgeActionDefinition'),
    newEventBridgeActionDefinition,

    -- ** Filters
    Filters (Filters'),
    newFilters,

    -- ** HierarchyGroup
    HierarchyGroup (HierarchyGroup'),
    newHierarchyGroup,

    -- ** HierarchyGroupCondition
    HierarchyGroupCondition (HierarchyGroupCondition'),
    newHierarchyGroupCondition,

    -- ** HierarchyGroupSummary
    HierarchyGroupSummary (HierarchyGroupSummary'),
    newHierarchyGroupSummary,

    -- ** HierarchyGroupSummaryReference
    HierarchyGroupSummaryReference (HierarchyGroupSummaryReference'),
    newHierarchyGroupSummaryReference,

    -- ** HierarchyLevel
    HierarchyLevel (HierarchyLevel'),
    newHierarchyLevel,

    -- ** HierarchyLevelUpdate
    HierarchyLevelUpdate (HierarchyLevelUpdate'),
    newHierarchyLevelUpdate,

    -- ** HierarchyPath
    HierarchyPath (HierarchyPath'),
    newHierarchyPath,

    -- ** HierarchyPathReference
    HierarchyPathReference (HierarchyPathReference'),
    newHierarchyPathReference,

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

    -- ** InvisibleFieldInfo
    InvisibleFieldInfo (InvisibleFieldInfo'),
    newInvisibleFieldInfo,

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

    -- ** LexBotConfig
    LexBotConfig (LexBotConfig'),
    newLexBotConfig,

    -- ** LexV2Bot
    LexV2Bot (LexV2Bot'),
    newLexV2Bot,

    -- ** ListPhoneNumbersSummary
    ListPhoneNumbersSummary (ListPhoneNumbersSummary'),
    newListPhoneNumbersSummary,

    -- ** MediaConcurrency
    MediaConcurrency (MediaConcurrency'),
    newMediaConcurrency,

    -- ** NotificationRecipientType
    NotificationRecipientType (NotificationRecipientType'),
    newNotificationRecipientType,

    -- ** NumberReference
    NumberReference (NumberReference'),
    newNumberReference,

    -- ** OutboundCallerConfig
    OutboundCallerConfig (OutboundCallerConfig'),
    newOutboundCallerConfig,

    -- ** ParticipantDetails
    ParticipantDetails (ParticipantDetails'),
    newParticipantDetails,

    -- ** ParticipantTimerConfiguration
    ParticipantTimerConfiguration (ParticipantTimerConfiguration'),
    newParticipantTimerConfiguration,

    -- ** ParticipantTimerValue
    ParticipantTimerValue (ParticipantTimerValue'),
    newParticipantTimerValue,

    -- ** PhoneNumberQuickConnectConfig
    PhoneNumberQuickConnectConfig (PhoneNumberQuickConnectConfig'),
    newPhoneNumberQuickConnectConfig,

    -- ** PhoneNumberStatus
    PhoneNumberStatus (PhoneNumberStatus'),
    newPhoneNumberStatus,

    -- ** PhoneNumberSummary
    PhoneNumberSummary (PhoneNumberSummary'),
    newPhoneNumberSummary,

    -- ** PromptSummary
    PromptSummary (PromptSummary'),
    newPromptSummary,

    -- ** Queue
    Queue (Queue'),
    newQueue,

    -- ** QueueInfo
    QueueInfo (QueueInfo'),
    newQueueInfo,

    -- ** QueueQuickConnectConfig
    QueueQuickConnectConfig (QueueQuickConnectConfig'),
    newQueueQuickConnectConfig,

    -- ** QueueReference
    QueueReference (QueueReference'),
    newQueueReference,

    -- ** QueueSearchCriteria
    QueueSearchCriteria (QueueSearchCriteria'),
    newQueueSearchCriteria,

    -- ** QueueSearchFilter
    QueueSearchFilter (QueueSearchFilter'),
    newQueueSearchFilter,

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

    -- ** ReadOnlyFieldInfo
    ReadOnlyFieldInfo (ReadOnlyFieldInfo'),
    newReadOnlyFieldInfo,

    -- ** Reference
    Reference (Reference'),
    newReference,

    -- ** ReferenceSummary
    ReferenceSummary (ReferenceSummary'),
    newReferenceSummary,

    -- ** RequiredFieldInfo
    RequiredFieldInfo (RequiredFieldInfo'),
    newRequiredFieldInfo,

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

    -- ** RoutingProfileReference
    RoutingProfileReference (RoutingProfileReference'),
    newRoutingProfileReference,

    -- ** RoutingProfileSearchCriteria
    RoutingProfileSearchCriteria (RoutingProfileSearchCriteria'),
    newRoutingProfileSearchCriteria,

    -- ** RoutingProfileSearchFilter
    RoutingProfileSearchFilter (RoutingProfileSearchFilter'),
    newRoutingProfileSearchFilter,

    -- ** RoutingProfileSummary
    RoutingProfileSummary (RoutingProfileSummary'),
    newRoutingProfileSummary,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleAction
    RuleAction (RuleAction'),
    newRuleAction,

    -- ** RuleSummary
    RuleSummary (RuleSummary'),
    newRuleSummary,

    -- ** RuleTriggerEventSource
    RuleTriggerEventSource (RuleTriggerEventSource'),
    newRuleTriggerEventSource,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** SecurityKey
    SecurityKey (SecurityKey'),
    newSecurityKey,

    -- ** SecurityProfile
    SecurityProfile (SecurityProfile'),
    newSecurityProfile,

    -- ** SecurityProfileSearchCriteria
    SecurityProfileSearchCriteria (SecurityProfileSearchCriteria'),
    newSecurityProfileSearchCriteria,

    -- ** SecurityProfileSearchSummary
    SecurityProfileSearchSummary (SecurityProfileSearchSummary'),
    newSecurityProfileSearchSummary,

    -- ** SecurityProfileSummary
    SecurityProfileSummary (SecurityProfileSummary'),
    newSecurityProfileSummary,

    -- ** SecurityProfilesSearchFilter
    SecurityProfilesSearchFilter (SecurityProfilesSearchFilter'),
    newSecurityProfilesSearchFilter,

    -- ** SendNotificationActionDefinition
    SendNotificationActionDefinition (SendNotificationActionDefinition'),
    newSendNotificationActionDefinition,

    -- ** StringCondition
    StringCondition (StringCondition'),
    newStringCondition,

    -- ** StringReference
    StringReference (StringReference'),
    newStringReference,

    -- ** TagCondition
    TagCondition (TagCondition'),
    newTagCondition,

    -- ** TaskActionDefinition
    TaskActionDefinition (TaskActionDefinition'),
    newTaskActionDefinition,

    -- ** TaskTemplateConstraints
    TaskTemplateConstraints (TaskTemplateConstraints'),
    newTaskTemplateConstraints,

    -- ** TaskTemplateDefaultFieldValue
    TaskTemplateDefaultFieldValue (TaskTemplateDefaultFieldValue'),
    newTaskTemplateDefaultFieldValue,

    -- ** TaskTemplateDefaults
    TaskTemplateDefaults (TaskTemplateDefaults'),
    newTaskTemplateDefaults,

    -- ** TaskTemplateField
    TaskTemplateField (TaskTemplateField'),
    newTaskTemplateField,

    -- ** TaskTemplateFieldIdentifier
    TaskTemplateFieldIdentifier (TaskTemplateFieldIdentifier'),
    newTaskTemplateFieldIdentifier,

    -- ** TaskTemplateMetadata
    TaskTemplateMetadata (TaskTemplateMetadata'),
    newTaskTemplateMetadata,

    -- ** TelephonyConfig
    TelephonyConfig (TelephonyConfig'),
    newTelephonyConfig,

    -- ** Threshold
    Threshold (Threshold'),
    newThreshold,

    -- ** TrafficDistributionGroup
    TrafficDistributionGroup (TrafficDistributionGroup'),
    newTrafficDistributionGroup,

    -- ** TrafficDistributionGroupSummary
    TrafficDistributionGroupSummary (TrafficDistributionGroupSummary'),
    newTrafficDistributionGroupSummary,

    -- ** UpdateParticipantRoleConfigChannelInfo
    UpdateParticipantRoleConfigChannelInfo (UpdateParticipantRoleConfigChannelInfo'),
    newUpdateParticipantRoleConfigChannelInfo,

    -- ** UrlReference
    UrlReference (UrlReference'),
    newUrlReference,

    -- ** UseCase
    UseCase (UseCase'),
    newUseCase,

    -- ** User
    User (User'),
    newUser,

    -- ** UserData
    UserData (UserData'),
    newUserData,

    -- ** UserDataFilters
    UserDataFilters (UserDataFilters'),
    newUserDataFilters,

    -- ** UserIdentityInfo
    UserIdentityInfo (UserIdentityInfo'),
    newUserIdentityInfo,

    -- ** UserIdentityInfoLite
    UserIdentityInfoLite (UserIdentityInfoLite'),
    newUserIdentityInfoLite,

    -- ** UserPhoneConfig
    UserPhoneConfig (UserPhoneConfig'),
    newUserPhoneConfig,

    -- ** UserQuickConnectConfig
    UserQuickConnectConfig (UserQuickConnectConfig'),
    newUserQuickConnectConfig,

    -- ** UserReference
    UserReference (UserReference'),
    newUserReference,

    -- ** UserSearchCriteria
    UserSearchCriteria (UserSearchCriteria'),
    newUserSearchCriteria,

    -- ** UserSearchFilter
    UserSearchFilter (UserSearchFilter'),
    newUserSearchFilter,

    -- ** UserSearchSummary
    UserSearchSummary (UserSearchSummary'),
    newUserSearchSummary,

    -- ** UserSummary
    UserSummary (UserSummary'),
    newUserSummary,

    -- ** Vocabulary
    Vocabulary (Vocabulary'),
    newVocabulary,

    -- ** VocabularySummary
    VocabularySummary (VocabularySummary'),
    newVocabularySummary,

    -- ** VoiceRecordingConfiguration
    VoiceRecordingConfiguration (VoiceRecordingConfiguration'),
    newVoiceRecordingConfiguration,
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
import Amazonka.Connect.CreateRule
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
import Amazonka.Connect.DeleteRule
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
import Amazonka.Connect.DescribeRule
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
import Amazonka.Connect.Lens
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
import Amazonka.Connect.ListRules
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
import Amazonka.Connect.Types
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
import Amazonka.Connect.UpdateParticipantRoleConfig
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
import Amazonka.Connect.UpdateRule
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
import Amazonka.Connect.Waiters

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

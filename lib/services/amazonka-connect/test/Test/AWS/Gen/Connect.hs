{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Connect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Connect where

import Amazonka.Connect
import qualified Data.Proxy as Proxy
import Test.AWS.Connect.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeInstance $
--             newDescribeInstance
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestAssociateLexBot $
--             newAssociateLexBot
--
--         , requestUpdateInstanceAttribute $
--             newUpdateInstanceAttribute
--
--         , requestUpdateQueueStatus $
--             newUpdateQueueStatus
--
--         , requestUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueues
--
--         , requestDescribeQueue $
--             newDescribeQueue
--
--         , requestListInstanceAttributes $
--             newListInstanceAttributes
--
--         , requestUpdateAgentStatus $
--             newUpdateAgentStatus
--
--         , requestDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfig
--
--         , requestCreateQuickConnect $
--             newCreateQuickConnect
--
--         , requestDescribeContactFlow $
--             newDescribeContactFlow
--
--         , requestUpdateUserHierarchy $
--             newUpdateUserHierarchy
--
--         , requestUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfile
--
--         , requestUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupName
--
--         , requestUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperation
--
--         , requestDescribeRoutingProfile $
--             newDescribeRoutingProfile
--
--         , requestDisassociateLexBot $
--             newDisassociateLexBot
--
--         , requestDeleteQuickConnect $
--             newDeleteQuickConnect
--
--         , requestStartOutboundVoiceContact $
--             newStartOutboundVoiceContact
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetMetricData $
--             newGetMetricData
--
--         , requestStartContactRecording $
--             newStartContactRecording
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestAssociateBot $
--             newAssociateBot
--
--         , requestAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnects
--
--         , requestStartTaskContact $
--             newStartTaskContact
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListUserHierarchyGroups $
--             newListUserHierarchyGroups
--
--         , requestListQueues $
--             newListQueues
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestListBots $
--             newListBots
--
--         , requestUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfig
--
--         , requestDescribeAgentStatus $
--             newDescribeAgentStatus
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfig
--
--         , requestCreateRoutingProfile $
--             newCreateRoutingProfile
--
--         , requestUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfig
--
--         , requestDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnects
--
--         , requestCreateUseCase $
--             newCreateUseCase
--
--         , requestDisassociateBot $
--             newDisassociateBot
--
--         , requestListQueueQuickConnects $
--             newListQueueQuickConnects
--
--         , requestGetCurrentMetricData $
--             newGetCurrentMetricData
--
--         , requestCreateContactFlow $
--             newCreateContactFlow
--
--         , requestListRoutingProfiles $
--             newListRoutingProfiles
--
--         , requestDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociation
--
--         , requestDeleteHoursOfOperation $
--             newDeleteHoursOfOperation
--
--         , requestUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfig
--
--         , requestUpdateHoursOfOperation $
--             newUpdateHoursOfOperation
--
--         , requestListApprovedOrigins $
--             newListApprovedOrigins
--
--         , requestDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructure
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestUpdateContactAttributes $
--             newUpdateContactAttributes
--
--         , requestListUseCases $
--             newListUseCases
--
--         , requestStartChatContact $
--             newStartChatContact
--
--         , requestDeleteUseCase $
--             newDeleteUseCase
--
--         , requestUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfiles
--
--         , requestGetContactAttributes $
--             newGetContactAttributes
--
--         , requestListLambdaFunctions $
--             newListLambdaFunctions
--
--         , requestDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroup
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestResumeContactRecording $
--             newResumeContactRecording
--
--         , requestUpdateContactFlowName $
--             newUpdateContactFlowName
--
--         , requestSuspendContactRecording $
--             newSuspendContactRecording
--
--         , requestUpdateQueueName $
--             newUpdateQueueName
--
--         , requestUpdateQueueMaxContacts $
--             newUpdateQueueMaxContacts
--
--         , requestListRoutingProfileQueues $
--             newListRoutingProfileQueues
--
--         , requestDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueues
--
--         , requestDisassociateLambdaFunction $
--             newDisassociateLambdaFunction
--
--         , requestUpdateContactFlowContent $
--             newUpdateContactFlowContent
--
--         , requestUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructure
--
--         , requestDescribeHoursOfOperation $
--             newDescribeHoursOfOperation
--
--         , requestListQuickConnects $
--             newListQuickConnects
--
--         , requestCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestUpdateQuickConnectName $
--             newUpdateQuickConnectName
--
--         , requestListPrompts $
--             newListPrompts
--
--         , requestAssociateSecurityKey $
--             newAssociateSecurityKey
--
--         , requestStopContactRecording $
--             newStopContactRecording
--
--         , requestDisassociateApprovedOrigin $
--             newDisassociateApprovedOrigin
--
--         , requestListSecurityKeys $
--             newListSecurityKeys
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--         , requestStopContact $
--             newStopContact
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfo
--
--         , requestListInstances $
--             newListInstances
--
--         , requestDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroup
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueue
--
--         , requestUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfig
--
--         , requestListContactFlows $
--             newListContactFlows
--
--         , requestCreateIntegrationAssociation $
--             newCreateIntegrationAssociation
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAssociateApprovedOrigin $
--             newAssociateApprovedOrigin
--
--         , requestCreateHoursOfOperation $
--             newCreateHoursOfOperation
--
--         , requestDisassociateSecurityKey $
--             newDisassociateSecurityKey
--
--         , requestUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrency
--
--         , requestListInstanceStorageConfigs $
--             newListInstanceStorageConfigs
--
--         , requestDescribeQuickConnect $
--             newDescribeQuickConnect
--
--         , requestAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfig
--
--         , requestListHoursOfOperations $
--             newListHoursOfOperations
--
--         , requestListIntegrationAssociations $
--             newListIntegrationAssociations
--
--         , requestCreateAgentStatus $
--             newCreateAgentStatus
--
--         , requestUpdateRoutingProfileName $
--             newUpdateRoutingProfileName
--
--         , requestListLexBots $
--             newListLexBots
--
--         , requestListAgentStatuses $
--             newListAgentStatuses
--
--         , requestAssociateLambdaFunction $
--             newAssociateLambdaFunction
--
--         , requestAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueues
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInstance $
--             newDescribeInstanceResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseAssociateLexBot $
--             newAssociateLexBotResponse
--
--         , responseUpdateInstanceAttribute $
--             newUpdateInstanceAttributeResponse
--
--         , responseUpdateQueueStatus $
--             newUpdateQueueStatusResponse
--
--         , responseUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueuesResponse
--
--         , responseDescribeQueue $
--             newDescribeQueueResponse
--
--         , responseListInstanceAttributes $
--             newListInstanceAttributesResponse
--
--         , responseUpdateAgentStatus $
--             newUpdateAgentStatusResponse
--
--         , responseDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfigResponse
--
--         , responseCreateQuickConnect $
--             newCreateQuickConnectResponse
--
--         , responseDescribeContactFlow $
--             newDescribeContactFlowResponse
--
--         , responseUpdateUserHierarchy $
--             newUpdateUserHierarchyResponse
--
--         , responseUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfileResponse
--
--         , responseUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupNameResponse
--
--         , responseUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperationResponse
--
--         , responseDescribeRoutingProfile $
--             newDescribeRoutingProfileResponse
--
--         , responseDisassociateLexBot $
--             newDisassociateLexBotResponse
--
--         , responseDeleteQuickConnect $
--             newDeleteQuickConnectResponse
--
--         , responseStartOutboundVoiceContact $
--             newStartOutboundVoiceContactResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--         , responseStartContactRecording $
--             newStartContactRecordingResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseAssociateBot $
--             newAssociateBotResponse
--
--         , responseAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnectsResponse
--
--         , responseStartTaskContact $
--             newStartTaskContactResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListUserHierarchyGroups $
--             newListUserHierarchyGroupsResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfigResponse
--
--         , responseDescribeAgentStatus $
--             newDescribeAgentStatusResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfigResponse
--
--         , responseCreateRoutingProfile $
--             newCreateRoutingProfileResponse
--
--         , responseUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfigResponse
--
--         , responseDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnectsResponse
--
--         , responseCreateUseCase $
--             newCreateUseCaseResponse
--
--         , responseDisassociateBot $
--             newDisassociateBotResponse
--
--         , responseListQueueQuickConnects $
--             newListQueueQuickConnectsResponse
--
--         , responseGetCurrentMetricData $
--             newGetCurrentMetricDataResponse
--
--         , responseCreateContactFlow $
--             newCreateContactFlowResponse
--
--         , responseListRoutingProfiles $
--             newListRoutingProfilesResponse
--
--         , responseDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociationResponse
--
--         , responseDeleteHoursOfOperation $
--             newDeleteHoursOfOperationResponse
--
--         , responseUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfigResponse
--
--         , responseUpdateHoursOfOperation $
--             newUpdateHoursOfOperationResponse
--
--         , responseListApprovedOrigins $
--             newListApprovedOriginsResponse
--
--         , responseDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructureResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseUpdateContactAttributes $
--             newUpdateContactAttributesResponse
--
--         , responseListUseCases $
--             newListUseCasesResponse
--
--         , responseStartChatContact $
--             newStartChatContactResponse
--
--         , responseDeleteUseCase $
--             newDeleteUseCaseResponse
--
--         , responseUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfilesResponse
--
--         , responseGetContactAttributes $
--             newGetContactAttributesResponse
--
--         , responseListLambdaFunctions $
--             newListLambdaFunctionsResponse
--
--         , responseDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroupResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseResumeContactRecording $
--             newResumeContactRecordingResponse
--
--         , responseUpdateContactFlowName $
--             newUpdateContactFlowNameResponse
--
--         , responseSuspendContactRecording $
--             newSuspendContactRecordingResponse
--
--         , responseUpdateQueueName $
--             newUpdateQueueNameResponse
--
--         , responseUpdateQueueMaxContacts $
--             newUpdateQueueMaxContactsResponse
--
--         , responseListRoutingProfileQueues $
--             newListRoutingProfileQueuesResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueuesResponse
--
--         , responseDisassociateLambdaFunction $
--             newDisassociateLambdaFunctionResponse
--
--         , responseUpdateContactFlowContent $
--             newUpdateContactFlowContentResponse
--
--         , responseUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructureResponse
--
--         , responseDescribeHoursOfOperation $
--             newDescribeHoursOfOperationResponse
--
--         , responseListQuickConnects $
--             newListQuickConnectsResponse
--
--         , responseCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseUpdateQuickConnectName $
--             newUpdateQuickConnectNameResponse
--
--         , responseListPrompts $
--             newListPromptsResponse
--
--         , responseAssociateSecurityKey $
--             newAssociateSecurityKeyResponse
--
--         , responseStopContactRecording $
--             newStopContactRecordingResponse
--
--         , responseDisassociateApprovedOrigin $
--             newDisassociateApprovedOriginResponse
--
--         , responseListSecurityKeys $
--             newListSecurityKeysResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--         , responseStopContact $
--             newStopContactResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfoResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroupResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfigResponse
--
--         , responseListContactFlows $
--             newListContactFlowsResponse
--
--         , responseCreateIntegrationAssociation $
--             newCreateIntegrationAssociationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAssociateApprovedOrigin $
--             newAssociateApprovedOriginResponse
--
--         , responseCreateHoursOfOperation $
--             newCreateHoursOfOperationResponse
--
--         , responseDisassociateSecurityKey $
--             newDisassociateSecurityKeyResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrencyResponse
--
--         , responseListInstanceStorageConfigs $
--             newListInstanceStorageConfigsResponse
--
--         , responseDescribeQuickConnect $
--             newDescribeQuickConnectResponse
--
--         , responseAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfigResponse
--
--         , responseListHoursOfOperations $
--             newListHoursOfOperationsResponse
--
--         , responseListIntegrationAssociations $
--             newListIntegrationAssociationsResponse
--
--         , responseCreateAgentStatus $
--             newCreateAgentStatusResponse
--
--         , responseUpdateRoutingProfileName $
--             newUpdateRoutingProfileNameResponse
--
--         , responseListLexBots $
--             newListLexBotsResponse
--
--         , responseListAgentStatuses $
--             newListAgentStatusesResponse
--
--         , responseAssociateLambdaFunction $
--             newAssociateLambdaFunctionResponse
--
--         , responseAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueuesResponse
--
--           ]
--     ]

-- Requests

requestDescribeInstance :: DescribeInstance -> TestTree
requestDescribeInstance =
  req
    "DescribeInstance"
    "fixture/DescribeInstance.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestAssociateLexBot :: AssociateLexBot -> TestTree
requestAssociateLexBot =
  req
    "AssociateLexBot"
    "fixture/AssociateLexBot.yaml"

requestUpdateInstanceAttribute :: UpdateInstanceAttribute -> TestTree
requestUpdateInstanceAttribute =
  req
    "UpdateInstanceAttribute"
    "fixture/UpdateInstanceAttribute.yaml"

requestUpdateQueueStatus :: UpdateQueueStatus -> TestTree
requestUpdateQueueStatus =
  req
    "UpdateQueueStatus"
    "fixture/UpdateQueueStatus.yaml"

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues =
  req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestDescribeQueue :: DescribeQueue -> TestTree
requestDescribeQueue =
  req
    "DescribeQueue"
    "fixture/DescribeQueue.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes =
  req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestUpdateAgentStatus :: UpdateAgentStatus -> TestTree
requestUpdateAgentStatus =
  req
    "UpdateAgentStatus"
    "fixture/UpdateAgentStatus.yaml"

requestDescribeInstanceStorageConfig :: DescribeInstanceStorageConfig -> TestTree
requestDescribeInstanceStorageConfig =
  req
    "DescribeInstanceStorageConfig"
    "fixture/DescribeInstanceStorageConfig.yaml"

requestCreateQuickConnect :: CreateQuickConnect -> TestTree
requestCreateQuickConnect =
  req
    "CreateQuickConnect"
    "fixture/CreateQuickConnect.yaml"

requestDescribeContactFlow :: DescribeContactFlow -> TestTree
requestDescribeContactFlow =
  req
    "DescribeContactFlow"
    "fixture/DescribeContactFlow.yaml"

requestUpdateUserHierarchy :: UpdateUserHierarchy -> TestTree
requestUpdateUserHierarchy =
  req
    "UpdateUserHierarchy"
    "fixture/UpdateUserHierarchy.yaml"

requestUpdateUserRoutingProfile :: UpdateUserRoutingProfile -> TestTree
requestUpdateUserRoutingProfile =
  req
    "UpdateUserRoutingProfile"
    "fixture/UpdateUserRoutingProfile.yaml"

requestUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupName -> TestTree
requestUpdateUserHierarchyGroupName =
  req
    "UpdateUserHierarchyGroupName"
    "fixture/UpdateUserHierarchyGroupName.yaml"

requestUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperation -> TestTree
requestUpdateQueueHoursOfOperation =
  req
    "UpdateQueueHoursOfOperation"
    "fixture/UpdateQueueHoursOfOperation.yaml"

requestDescribeRoutingProfile :: DescribeRoutingProfile -> TestTree
requestDescribeRoutingProfile =
  req
    "DescribeRoutingProfile"
    "fixture/DescribeRoutingProfile.yaml"

requestDisassociateLexBot :: DisassociateLexBot -> TestTree
requestDisassociateLexBot =
  req
    "DisassociateLexBot"
    "fixture/DisassociateLexBot.yaml"

requestDeleteQuickConnect :: DeleteQuickConnect -> TestTree
requestDeleteQuickConnect =
  req
    "DeleteQuickConnect"
    "fixture/DeleteQuickConnect.yaml"

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact =
  req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestStartContactRecording :: StartContactRecording -> TestTree
requestStartContactRecording =
  req
    "StartContactRecording"
    "fixture/StartContactRecording.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestAssociateBot :: AssociateBot -> TestTree
requestAssociateBot =
  req
    "AssociateBot"
    "fixture/AssociateBot.yaml"

requestAssociateQueueQuickConnects :: AssociateQueueQuickConnects -> TestTree
requestAssociateQueueQuickConnects =
  req
    "AssociateQueueQuickConnects"
    "fixture/AssociateQueueQuickConnects.yaml"

requestStartTaskContact :: StartTaskContact -> TestTree
requestStartTaskContact =
  req
    "StartTaskContact"
    "fixture/StartTaskContact.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups =
  req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestUpdateQuickConnectConfig :: UpdateQuickConnectConfig -> TestTree
requestUpdateQuickConnectConfig =
  req
    "UpdateQuickConnectConfig"
    "fixture/UpdateQuickConnectConfig.yaml"

requestDescribeAgentStatus :: DescribeAgentStatus -> TestTree
requestDescribeAgentStatus =
  req
    "DescribeAgentStatus"
    "fixture/DescribeAgentStatus.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfig -> TestTree
requestDisassociateInstanceStorageConfig =
  req
    "DisassociateInstanceStorageConfig"
    "fixture/DisassociateInstanceStorageConfig.yaml"

requestCreateRoutingProfile :: CreateRoutingProfile -> TestTree
requestCreateRoutingProfile =
  req
    "CreateRoutingProfile"
    "fixture/CreateRoutingProfile.yaml"

requestUpdateInstanceStorageConfig :: UpdateInstanceStorageConfig -> TestTree
requestUpdateInstanceStorageConfig =
  req
    "UpdateInstanceStorageConfig"
    "fixture/UpdateInstanceStorageConfig.yaml"

requestDisassociateQueueQuickConnects :: DisassociateQueueQuickConnects -> TestTree
requestDisassociateQueueQuickConnects =
  req
    "DisassociateQueueQuickConnects"
    "fixture/DisassociateQueueQuickConnects.yaml"

requestCreateUseCase :: CreateUseCase -> TestTree
requestCreateUseCase =
  req
    "CreateUseCase"
    "fixture/CreateUseCase.yaml"

requestDisassociateBot :: DisassociateBot -> TestTree
requestDisassociateBot =
  req
    "DisassociateBot"
    "fixture/DisassociateBot.yaml"

requestListQueueQuickConnects :: ListQueueQuickConnects -> TestTree
requestListQueueQuickConnects =
  req
    "ListQueueQuickConnects"
    "fixture/ListQueueQuickConnects.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData =
  req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

requestCreateContactFlow :: CreateContactFlow -> TestTree
requestCreateContactFlow =
  req
    "CreateContactFlow"
    "fixture/CreateContactFlow.yaml"

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles =
  req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

requestDeleteIntegrationAssociation :: DeleteIntegrationAssociation -> TestTree
requestDeleteIntegrationAssociation =
  req
    "DeleteIntegrationAssociation"
    "fixture/DeleteIntegrationAssociation.yaml"

requestDeleteHoursOfOperation :: DeleteHoursOfOperation -> TestTree
requestDeleteHoursOfOperation =
  req
    "DeleteHoursOfOperation"
    "fixture/DeleteHoursOfOperation.yaml"

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig =
  req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

requestUpdateHoursOfOperation :: UpdateHoursOfOperation -> TestTree
requestUpdateHoursOfOperation =
  req
    "UpdateHoursOfOperation"
    "fixture/UpdateHoursOfOperation.yaml"

requestListApprovedOrigins :: ListApprovedOrigins -> TestTree
requestListApprovedOrigins =
  req
    "ListApprovedOrigins"
    "fixture/ListApprovedOrigins.yaml"

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure =
  req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestUpdateContactAttributes :: UpdateContactAttributes -> TestTree
requestUpdateContactAttributes =
  req
    "UpdateContactAttributes"
    "fixture/UpdateContactAttributes.yaml"

requestListUseCases :: ListUseCases -> TestTree
requestListUseCases =
  req
    "ListUseCases"
    "fixture/ListUseCases.yaml"

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact =
  req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

requestDeleteUseCase :: DeleteUseCase -> TestTree
requestDeleteUseCase =
  req
    "DeleteUseCase"
    "fixture/DeleteUseCase.yaml"

requestUpdateUserSecurityProfiles :: UpdateUserSecurityProfiles -> TestTree
requestUpdateUserSecurityProfiles =
  req
    "UpdateUserSecurityProfiles"
    "fixture/UpdateUserSecurityProfiles.yaml"

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes =
  req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestListLambdaFunctions :: ListLambdaFunctions -> TestTree
requestListLambdaFunctions =
  req
    "ListLambdaFunctions"
    "fixture/ListLambdaFunctions.yaml"

requestDescribeUserHierarchyGroup :: DescribeUserHierarchyGroup -> TestTree
requestDescribeUserHierarchyGroup =
  req
    "DescribeUserHierarchyGroup"
    "fixture/DescribeUserHierarchyGroup.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestResumeContactRecording :: ResumeContactRecording -> TestTree
requestResumeContactRecording =
  req
    "ResumeContactRecording"
    "fixture/ResumeContactRecording.yaml"

requestUpdateContactFlowName :: UpdateContactFlowName -> TestTree
requestUpdateContactFlowName =
  req
    "UpdateContactFlowName"
    "fixture/UpdateContactFlowName.yaml"

requestSuspendContactRecording :: SuspendContactRecording -> TestTree
requestSuspendContactRecording =
  req
    "SuspendContactRecording"
    "fixture/SuspendContactRecording.yaml"

requestUpdateQueueName :: UpdateQueueName -> TestTree
requestUpdateQueueName =
  req
    "UpdateQueueName"
    "fixture/UpdateQueueName.yaml"

requestUpdateQueueMaxContacts :: UpdateQueueMaxContacts -> TestTree
requestUpdateQueueMaxContacts =
  req
    "UpdateQueueMaxContacts"
    "fixture/UpdateQueueMaxContacts.yaml"

requestListRoutingProfileQueues :: ListRoutingProfileQueues -> TestTree
requestListRoutingProfileQueues =
  req
    "ListRoutingProfileQueues"
    "fixture/ListRoutingProfileQueues.yaml"

requestDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueues -> TestTree
requestDisassociateRoutingProfileQueues =
  req
    "DisassociateRoutingProfileQueues"
    "fixture/DisassociateRoutingProfileQueues.yaml"

requestDisassociateLambdaFunction :: DisassociateLambdaFunction -> TestTree
requestDisassociateLambdaFunction =
  req
    "DisassociateLambdaFunction"
    "fixture/DisassociateLambdaFunction.yaml"

requestUpdateContactFlowContent :: UpdateContactFlowContent -> TestTree
requestUpdateContactFlowContent =
  req
    "UpdateContactFlowContent"
    "fixture/UpdateContactFlowContent.yaml"

requestUpdateUserHierarchyStructure :: UpdateUserHierarchyStructure -> TestTree
requestUpdateUserHierarchyStructure =
  req
    "UpdateUserHierarchyStructure"
    "fixture/UpdateUserHierarchyStructure.yaml"

requestDescribeHoursOfOperation :: DescribeHoursOfOperation -> TestTree
requestDescribeHoursOfOperation =
  req
    "DescribeHoursOfOperation"
    "fixture/DescribeHoursOfOperation.yaml"

requestListQuickConnects :: ListQuickConnects -> TestTree
requestListQuickConnects =
  req
    "ListQuickConnects"
    "fixture/ListQuickConnects.yaml"

requestCreateUserHierarchyGroup :: CreateUserHierarchyGroup -> TestTree
requestCreateUserHierarchyGroup =
  req
    "CreateUserHierarchyGroup"
    "fixture/CreateUserHierarchyGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestUpdateQuickConnectName :: UpdateQuickConnectName -> TestTree
requestUpdateQuickConnectName =
  req
    "UpdateQuickConnectName"
    "fixture/UpdateQuickConnectName.yaml"

requestListPrompts :: ListPrompts -> TestTree
requestListPrompts =
  req
    "ListPrompts"
    "fixture/ListPrompts.yaml"

requestAssociateSecurityKey :: AssociateSecurityKey -> TestTree
requestAssociateSecurityKey =
  req
    "AssociateSecurityKey"
    "fixture/AssociateSecurityKey.yaml"

requestStopContactRecording :: StopContactRecording -> TestTree
requestStopContactRecording =
  req
    "StopContactRecording"
    "fixture/StopContactRecording.yaml"

requestDisassociateApprovedOrigin :: DisassociateApprovedOrigin -> TestTree
requestDisassociateApprovedOrigin =
  req
    "DisassociateApprovedOrigin"
    "fixture/DisassociateApprovedOrigin.yaml"

requestListSecurityKeys :: ListSecurityKeys -> TestTree
requestListSecurityKeys =
  req
    "ListSecurityKeys"
    "fixture/ListSecurityKeys.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken =
  req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact =
  req
    "StopContact"
    "fixture/StopContact.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo =
  req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestDeleteUserHierarchyGroup :: DeleteUserHierarchyGroup -> TestTree
requestDeleteUserHierarchyGroup =
  req
    "DeleteUserHierarchyGroup"
    "fixture/DeleteUserHierarchyGroup.yaml"

requestUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueue -> TestTree
requestUpdateRoutingProfileDefaultOutboundQueue =
  req
    "UpdateRoutingProfileDefaultOutboundQueue"
    "fixture/UpdateRoutingProfileDefaultOutboundQueue.yaml"

requestUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfig -> TestTree
requestUpdateQueueOutboundCallerConfig =
  req
    "UpdateQueueOutboundCallerConfig"
    "fixture/UpdateQueueOutboundCallerConfig.yaml"

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows =
  req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestCreateIntegrationAssociation :: CreateIntegrationAssociation -> TestTree
requestCreateIntegrationAssociation =
  req
    "CreateIntegrationAssociation"
    "fixture/CreateIntegrationAssociation.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAssociateApprovedOrigin :: AssociateApprovedOrigin -> TestTree
requestAssociateApprovedOrigin =
  req
    "AssociateApprovedOrigin"
    "fixture/AssociateApprovedOrigin.yaml"

requestCreateHoursOfOperation :: CreateHoursOfOperation -> TestTree
requestCreateHoursOfOperation =
  req
    "CreateHoursOfOperation"
    "fixture/CreateHoursOfOperation.yaml"

requestDisassociateSecurityKey :: DisassociateSecurityKey -> TestTree
requestDisassociateSecurityKey =
  req
    "DisassociateSecurityKey"
    "fixture/DisassociateSecurityKey.yaml"

requestUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrency -> TestTree
requestUpdateRoutingProfileConcurrency =
  req
    "UpdateRoutingProfileConcurrency"
    "fixture/UpdateRoutingProfileConcurrency.yaml"

requestListInstanceStorageConfigs :: ListInstanceStorageConfigs -> TestTree
requestListInstanceStorageConfigs =
  req
    "ListInstanceStorageConfigs"
    "fixture/ListInstanceStorageConfigs.yaml"

requestDescribeQuickConnect :: DescribeQuickConnect -> TestTree
requestDescribeQuickConnect =
  req
    "DescribeQuickConnect"
    "fixture/DescribeQuickConnect.yaml"

requestAssociateInstanceStorageConfig :: AssociateInstanceStorageConfig -> TestTree
requestAssociateInstanceStorageConfig =
  req
    "AssociateInstanceStorageConfig"
    "fixture/AssociateInstanceStorageConfig.yaml"

requestListHoursOfOperations :: ListHoursOfOperations -> TestTree
requestListHoursOfOperations =
  req
    "ListHoursOfOperations"
    "fixture/ListHoursOfOperations.yaml"

requestListIntegrationAssociations :: ListIntegrationAssociations -> TestTree
requestListIntegrationAssociations =
  req
    "ListIntegrationAssociations"
    "fixture/ListIntegrationAssociations.yaml"

requestCreateAgentStatus :: CreateAgentStatus -> TestTree
requestCreateAgentStatus =
  req
    "CreateAgentStatus"
    "fixture/CreateAgentStatus.yaml"

requestUpdateRoutingProfileName :: UpdateRoutingProfileName -> TestTree
requestUpdateRoutingProfileName =
  req
    "UpdateRoutingProfileName"
    "fixture/UpdateRoutingProfileName.yaml"

requestListLexBots :: ListLexBots -> TestTree
requestListLexBots =
  req
    "ListLexBots"
    "fixture/ListLexBots.yaml"

requestListAgentStatuses :: ListAgentStatuses -> TestTree
requestListAgentStatuses =
  req
    "ListAgentStatuses"
    "fixture/ListAgentStatuses.yaml"

requestAssociateLambdaFunction :: AssociateLambdaFunction -> TestTree
requestAssociateLambdaFunction =
  req
    "AssociateLambdaFunction"
    "fixture/AssociateLambdaFunction.yaml"

requestAssociateRoutingProfileQueues :: AssociateRoutingProfileQueues -> TestTree
requestAssociateRoutingProfileQueues =
  req
    "AssociateRoutingProfileQueues"
    "fixture/AssociateRoutingProfileQueues.yaml"

-- Responses

responseDescribeInstance :: DescribeInstanceResponse -> TestTree
responseDescribeInstance =
  res
    "DescribeInstanceResponse"
    "fixture/DescribeInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstance)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfiles)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot =
  res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLexBot)

responseUpdateInstanceAttribute :: UpdateInstanceAttributeResponse -> TestTree
responseUpdateInstanceAttribute =
  res
    "UpdateInstanceAttributeResponse"
    "fixture/UpdateInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceAttribute)

responseUpdateQueueStatus :: UpdateQueueStatusResponse -> TestTree
responseUpdateQueueStatus =
  res
    "UpdateQueueStatusResponse"
    "fixture/UpdateQueueStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueStatus)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues =
  res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileQueues)

responseDescribeQueue :: DescribeQueueResponse -> TestTree
responseDescribeQueue =
  res
    "DescribeQueueResponse"
    "fixture/DescribeQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueue)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes =
  res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceAttributes)

responseUpdateAgentStatus :: UpdateAgentStatusResponse -> TestTree
responseUpdateAgentStatus =
  res
    "UpdateAgentStatusResponse"
    "fixture/UpdateAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAgentStatus)

responseDescribeInstanceStorageConfig :: DescribeInstanceStorageConfigResponse -> TestTree
responseDescribeInstanceStorageConfig =
  res
    "DescribeInstanceStorageConfigResponse"
    "fixture/DescribeInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceStorageConfig)

responseCreateQuickConnect :: CreateQuickConnectResponse -> TestTree
responseCreateQuickConnect =
  res
    "CreateQuickConnectResponse"
    "fixture/CreateQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQuickConnect)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow =
  res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContactFlow)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy =
  res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchy)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile =
  res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserRoutingProfile)

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName =
  res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchyGroupName)

responseUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperationResponse -> TestTree
responseUpdateQueueHoursOfOperation =
  res
    "UpdateQueueHoursOfOperationResponse"
    "fixture/UpdateQueueHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueHoursOfOperation)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile =
  res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoutingProfile)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot =
  res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLexBot)

responseDeleteQuickConnect :: DeleteQuickConnectResponse -> TestTree
responseDeleteQuickConnect =
  res
    "DeleteQuickConnectResponse"
    "fixture/DeleteQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQuickConnect)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact =
  res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOutboundVoiceContact)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricData)

responseStartContactRecording :: StartContactRecordingResponse -> TestTree
responseStartContactRecording =
  res
    "StartContactRecordingResponse"
    "fixture/StartContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContactRecording)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstance)

responseAssociateBot :: AssociateBotResponse -> TestTree
responseAssociateBot =
  res
    "AssociateBotResponse"
    "fixture/AssociateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateBot)

responseAssociateQueueQuickConnects :: AssociateQueueQuickConnectsResponse -> TestTree
responseAssociateQueueQuickConnects =
  res
    "AssociateQueueQuickConnectsResponse"
    "fixture/AssociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateQueueQuickConnects)

responseStartTaskContact :: StartTaskContactResponse -> TestTree
responseStartTaskContact =
  res
    "StartTaskContactResponse"
    "fixture/StartTaskContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTaskContact)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups =
  res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserHierarchyGroups)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAttribute)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBots)

responseUpdateQuickConnectConfig :: UpdateQuickConnectConfigResponse -> TestTree
responseUpdateQuickConnectConfig =
  res
    "UpdateQuickConnectConfigResponse"
    "fixture/UpdateQuickConnectConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuickConnectConfig)

responseDescribeAgentStatus :: DescribeAgentStatusResponse -> TestTree
responseDescribeAgentStatus =
  res
    "DescribeAgentStatusResponse"
    "fixture/DescribeAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgentStatus)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfigResponse -> TestTree
responseDisassociateInstanceStorageConfig =
  res
    "DisassociateInstanceStorageConfigResponse"
    "fixture/DisassociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateInstanceStorageConfig)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile =
  res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoutingProfile)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig =
  res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceStorageConfig)

responseDisassociateQueueQuickConnects :: DisassociateQueueQuickConnectsResponse -> TestTree
responseDisassociateQueueQuickConnects =
  res
    "DisassociateQueueQuickConnectsResponse"
    "fixture/DisassociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateQueueQuickConnects)

responseCreateUseCase :: CreateUseCaseResponse -> TestTree
responseCreateUseCase =
  res
    "CreateUseCaseResponse"
    "fixture/CreateUseCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUseCase)

responseDisassociateBot :: DisassociateBotResponse -> TestTree
responseDisassociateBot =
  res
    "DisassociateBotResponse"
    "fixture/DisassociateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateBot)

responseListQueueQuickConnects :: ListQueueQuickConnectsResponse -> TestTree
responseListQueueQuickConnects =
  res
    "ListQueueQuickConnectsResponse"
    "fixture/ListQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueueQuickConnects)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData =
  res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCurrentMetricData)

responseCreateContactFlow :: CreateContactFlowResponse -> TestTree
responseCreateContactFlow =
  res
    "CreateContactFlowResponse"
    "fixture/CreateContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactFlow)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles =
  res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingProfiles)

responseDeleteIntegrationAssociation :: DeleteIntegrationAssociationResponse -> TestTree
responseDeleteIntegrationAssociation =
  res
    "DeleteIntegrationAssociationResponse"
    "fixture/DeleteIntegrationAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegrationAssociation)

responseDeleteHoursOfOperation :: DeleteHoursOfOperationResponse -> TestTree
responseDeleteHoursOfOperation =
  res
    "DeleteHoursOfOperationResponse"
    "fixture/DeleteHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHoursOfOperation)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig =
  res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPhoneConfig)

responseUpdateHoursOfOperation :: UpdateHoursOfOperationResponse -> TestTree
responseUpdateHoursOfOperation =
  res
    "UpdateHoursOfOperationResponse"
    "fixture/UpdateHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHoursOfOperation)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins =
  res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApprovedOrigins)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure =
  res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserHierarchyStructure)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbers)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes =
  res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactAttributes)

responseListUseCases :: ListUseCasesResponse -> TestTree
responseListUseCases =
  res
    "ListUseCasesResponse"
    "fixture/ListUseCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUseCases)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact =
  res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChatContact)

responseDeleteUseCase :: DeleteUseCaseResponse -> TestTree
responseDeleteUseCase =
  res
    "DeleteUseCaseResponse"
    "fixture/DeleteUseCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUseCase)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles =
  res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserSecurityProfiles)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes =
  res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactAttributes)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions =
  res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLambdaFunctions)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup =
  res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserHierarchyGroup)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording =
  res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeContactRecording)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName =
  res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowName)

responseSuspendContactRecording :: SuspendContactRecordingResponse -> TestTree
responseSuspendContactRecording =
  res
    "SuspendContactRecordingResponse"
    "fixture/SuspendContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendContactRecording)

responseUpdateQueueName :: UpdateQueueNameResponse -> TestTree
responseUpdateQueueName =
  res
    "UpdateQueueNameResponse"
    "fixture/UpdateQueueNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueName)

responseUpdateQueueMaxContacts :: UpdateQueueMaxContactsResponse -> TestTree
responseUpdateQueueMaxContacts =
  res
    "UpdateQueueMaxContactsResponse"
    "fixture/UpdateQueueMaxContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueMaxContacts)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues =
  res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingProfileQueues)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues =
  res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRoutingProfileQueues)

responseDisassociateLambdaFunction :: DisassociateLambdaFunctionResponse -> TestTree
responseDisassociateLambdaFunction =
  res
    "DisassociateLambdaFunctionResponse"
    "fixture/DisassociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLambdaFunction)

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent =
  res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowContent)

responseUpdateUserHierarchyStructure :: UpdateUserHierarchyStructureResponse -> TestTree
responseUpdateUserHierarchyStructure =
  res
    "UpdateUserHierarchyStructureResponse"
    "fixture/UpdateUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchyStructure)

responseDescribeHoursOfOperation :: DescribeHoursOfOperationResponse -> TestTree
responseDescribeHoursOfOperation =
  res
    "DescribeHoursOfOperationResponse"
    "fixture/DescribeHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHoursOfOperation)

responseListQuickConnects :: ListQuickConnectsResponse -> TestTree
responseListQuickConnects =
  res
    "ListQuickConnectsResponse"
    "fixture/ListQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQuickConnects)

responseCreateUserHierarchyGroup :: CreateUserHierarchyGroupResponse -> TestTree
responseCreateUserHierarchyGroup =
  res
    "CreateUserHierarchyGroupResponse"
    "fixture/CreateUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserHierarchyGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseUpdateQuickConnectName :: UpdateQuickConnectNameResponse -> TestTree
responseUpdateQuickConnectName =
  res
    "UpdateQuickConnectNameResponse"
    "fixture/UpdateQuickConnectNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuickConnectName)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts =
  res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrompts)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey =
  res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSecurityKey)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording =
  res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContactRecording)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin =
  res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApprovedOrigin)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys =
  res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityKeys)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFederationToken)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact =
  res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContact)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo =
  res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserIdentityInfo)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup =
  res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserHierarchyGroup)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue =
  res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfigResponse -> TestTree
responseUpdateQueueOutboundCallerConfig =
  res
    "UpdateQueueOutboundCallerConfigResponse"
    "fixture/UpdateQueueOutboundCallerConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueOutboundCallerConfig)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows =
  res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactFlows)

responseCreateIntegrationAssociation :: CreateIntegrationAssociationResponse -> TestTree
responseCreateIntegrationAssociation =
  res
    "CreateIntegrationAssociationResponse"
    "fixture/CreateIntegrationAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegrationAssociation)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin =
  res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApprovedOrigin)

responseCreateHoursOfOperation :: CreateHoursOfOperationResponse -> TestTree
responseCreateHoursOfOperation =
  res
    "CreateHoursOfOperationResponse"
    "fixture/CreateHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHoursOfOperation)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey =
  res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSecurityKey)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency =
  res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileConcurrency)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs =
  res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceStorageConfigs)

responseDescribeQuickConnect :: DescribeQuickConnectResponse -> TestTree
responseDescribeQuickConnect =
  res
    "DescribeQuickConnectResponse"
    "fixture/DescribeQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuickConnect)

responseAssociateInstanceStorageConfig :: AssociateInstanceStorageConfigResponse -> TestTree
responseAssociateInstanceStorageConfig =
  res
    "AssociateInstanceStorageConfigResponse"
    "fixture/AssociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateInstanceStorageConfig)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations =
  res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHoursOfOperations)

responseListIntegrationAssociations :: ListIntegrationAssociationsResponse -> TestTree
responseListIntegrationAssociations =
  res
    "ListIntegrationAssociationsResponse"
    "fixture/ListIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIntegrationAssociations)

responseCreateAgentStatus :: CreateAgentStatusResponse -> TestTree
responseCreateAgentStatus =
  res
    "CreateAgentStatusResponse"
    "fixture/CreateAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAgentStatus)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName =
  res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileName)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots =
  res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLexBots)

responseListAgentStatuses :: ListAgentStatusesResponse -> TestTree
responseListAgentStatuses =
  res
    "ListAgentStatusesResponse"
    "fixture/ListAgentStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgentStatuses)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction =
  res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLambdaFunction)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues =
  res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRoutingProfileQueues)

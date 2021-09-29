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

import Data.Proxy
import Network.AWS.Connect
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
--         [ requestUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupName
--
--         , requestUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfile
--
--         , requestSuspendContactRecording $
--             newSuspendContactRecording
--
--         , requestUpdateQueueName $
--             newUpdateQueueName
--
--         , requestCreateQuickConnect $
--             newCreateQuickConnect
--
--         , requestListInstanceAttributes $
--             newListInstanceAttributes
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueues
--
--         , requestDescribeInstance $
--             newDescribeInstance
--
--         , requestUpdateContactFlowName $
--             newUpdateContactFlowName
--
--         , requestListLambdaFunctions $
--             newListLambdaFunctions
--
--         , requestListLexBots $
--             newListLexBots
--
--         , requestAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueues
--
--         , requestListAgentStatuses $
--             newListAgentStatuses
--
--         , requestAssociateLambdaFunction $
--             newAssociateLambdaFunction
--
--         , requestGetContactAttributes $
--             newGetContactAttributes
--
--         , requestCreateAgentStatus $
--             newCreateAgentStatus
--
--         , requestListApprovedOrigins $
--             newListApprovedOrigins
--
--         , requestUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfig
--
--         , requestUpdateContactAttributes $
--             newUpdateContactAttributes
--
--         , requestDeleteUseCase $
--             newDeleteUseCase
--
--         , requestListRoutingProfiles $
--             newListRoutingProfiles
--
--         , requestDescribeQuickConnect $
--             newDescribeQuickConnect
--
--         , requestAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfig
--
--         , requestCreateContactFlow $
--             newCreateContactFlow
--
--         , requestListQueueQuickConnects $
--             newListQueueQuickConnects
--
--         , requestUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrency
--
--         , requestDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnects
--
--         , requestUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfig
--
--         , requestGetCurrentMetricData $
--             newGetCurrentMetricData
--
--         , requestCreateRoutingProfile $
--             newCreateRoutingProfile
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDisassociateSecurityKey $
--             newDisassociateSecurityKey
--
--         , requestAssociateApprovedOrigin $
--             newAssociateApprovedOrigin
--
--         , requestListQueues $
--             newListQueues
--
--         , requestListBots $
--             newListBots
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfig
--
--         , requestListInstances $
--             newListInstances
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestStopContact $
--             newStopContact
--
--         , requestAssociateSecurityKey $
--             newAssociateSecurityKey
--
--         , requestStopContactRecording $
--             newStopContactRecording
--
--         , requestUpdateQuickConnectName $
--             newUpdateQuickConnectName
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroup
--
--         , requestStartContactRecording $
--             newStartContactRecording
--
--         , requestDisassociateApprovedOrigin $
--             newDisassociateApprovedOrigin
--
--         , requestAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnects
--
--         , requestDescribeRoutingProfile $
--             newDescribeRoutingProfile
--
--         , requestDisassociateLexBot $
--             newDisassociateLexBot
--
--         , requestListQuickConnects $
--             newListQuickConnects
--
--         , requestDeleteQuickConnect $
--             newDeleteQuickConnect
--
--         , requestDescribeContactFlow $
--             newDescribeContactFlow
--
--         , requestDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueues
--
--         , requestUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperation
--
--         , requestUpdateUserHierarchy $
--             newUpdateUserHierarchy
--
--         , requestDisassociateLambdaFunction $
--             newDisassociateLambdaFunction
--
--         , requestUpdateQueueMaxContacts $
--             newUpdateQueueMaxContacts
--
--         , requestDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfig
--
--         , requestListRoutingProfileQueues $
--             newListRoutingProfileQueues
--
--         , requestUpdateInstanceAttribute $
--             newUpdateInstanceAttribute
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestAssociateLexBot $
--             newAssociateLexBot
--
--         , requestUpdateQueueStatus $
--             newUpdateQueueStatus
--
--         , requestResumeContactRecording $
--             newResumeContactRecording
--
--         , requestUpdateAgentStatus $
--             newUpdateAgentStatus
--
--         , requestDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroup
--
--         , requestDescribeQueue $
--             newDescribeQueue
--
--         , requestUpdateRoutingProfileName $
--             newUpdateRoutingProfileName
--
--         , requestDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructure
--
--         , requestStartChatContact $
--             newStartChatContact
--
--         , requestDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociation
--
--         , requestListUseCases $
--             newListUseCases
--
--         , requestUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfiles
--
--         , requestDeleteHoursOfOperation $
--             newDeleteHoursOfOperation
--
--         , requestUpdateHoursOfOperation $
--             newUpdateHoursOfOperation
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestListHoursOfOperations $
--             newListHoursOfOperations
--
--         , requestListIntegrationAssociations $
--             newListIntegrationAssociations
--
--         , requestListContactFlows $
--             newListContactFlows
--
--         , requestListInstanceStorageConfigs $
--             newListInstanceStorageConfigs
--
--         , requestCreateHoursOfOperation $
--             newCreateHoursOfOperation
--
--         , requestCreateUseCase $
--             newCreateUseCase
--
--         , requestDisassociateBot $
--             newDisassociateBot
--
--         , requestCreateIntegrationAssociation $
--             newCreateIntegrationAssociation
--
--         , requestUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfig
--
--         , requestUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfo
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueue
--
--         , requestListUsers $
--             newListUsers
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--         , requestDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroup
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestDescribeAgentStatus $
--             newDescribeAgentStatus
--
--         , requestListSecurityKeys $
--             newListSecurityKeys
--
--         , requestListUserHierarchyGroups $
--             newListUserHierarchyGroups
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfig
--
--         , requestAssociateBot $
--             newAssociateBot
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestListPrompts $
--             newListPrompts
--
--         , requestStartTaskContact $
--             newStartTaskContact
--
--         , requestUpdateContactFlowContent $
--             newUpdateContactFlowContent
--
--         , requestDescribeHoursOfOperation $
--             newDescribeHoursOfOperation
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartOutboundVoiceContact $
--             newStartOutboundVoiceContact
--
--         , requestUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructure
--
--         , requestGetMetricData $
--             newGetMetricData
--
--           ]

--     , testGroup "response"
--         [ responseUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupNameResponse
--
--         , responseUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfileResponse
--
--         , responseSuspendContactRecording $
--             newSuspendContactRecordingResponse
--
--         , responseUpdateQueueName $
--             newUpdateQueueNameResponse
--
--         , responseCreateQuickConnect $
--             newCreateQuickConnectResponse
--
--         , responseListInstanceAttributes $
--             newListInstanceAttributesResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueuesResponse
--
--         , responseDescribeInstance $
--             newDescribeInstanceResponse
--
--         , responseUpdateContactFlowName $
--             newUpdateContactFlowNameResponse
--
--         , responseListLambdaFunctions $
--             newListLambdaFunctionsResponse
--
--         , responseListLexBots $
--             newListLexBotsResponse
--
--         , responseAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueuesResponse
--
--         , responseListAgentStatuses $
--             newListAgentStatusesResponse
--
--         , responseAssociateLambdaFunction $
--             newAssociateLambdaFunctionResponse
--
--         , responseGetContactAttributes $
--             newGetContactAttributesResponse
--
--         , responseCreateAgentStatus $
--             newCreateAgentStatusResponse
--
--         , responseListApprovedOrigins $
--             newListApprovedOriginsResponse
--
--         , responseUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfigResponse
--
--         , responseUpdateContactAttributes $
--             newUpdateContactAttributesResponse
--
--         , responseDeleteUseCase $
--             newDeleteUseCaseResponse
--
--         , responseListRoutingProfiles $
--             newListRoutingProfilesResponse
--
--         , responseDescribeQuickConnect $
--             newDescribeQuickConnectResponse
--
--         , responseAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfigResponse
--
--         , responseCreateContactFlow $
--             newCreateContactFlowResponse
--
--         , responseListQueueQuickConnects $
--             newListQueueQuickConnectsResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrencyResponse
--
--         , responseDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnectsResponse
--
--         , responseUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfigResponse
--
--         , responseGetCurrentMetricData $
--             newGetCurrentMetricDataResponse
--
--         , responseCreateRoutingProfile $
--             newCreateRoutingProfileResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDisassociateSecurityKey $
--             newDisassociateSecurityKeyResponse
--
--         , responseAssociateApprovedOrigin $
--             newAssociateApprovedOriginResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfigResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseStopContact $
--             newStopContactResponse
--
--         , responseAssociateSecurityKey $
--             newAssociateSecurityKeyResponse
--
--         , responseStopContactRecording $
--             newStopContactRecordingResponse
--
--         , responseUpdateQuickConnectName $
--             newUpdateQuickConnectNameResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroupResponse
--
--         , responseStartContactRecording $
--             newStartContactRecordingResponse
--
--         , responseDisassociateApprovedOrigin $
--             newDisassociateApprovedOriginResponse
--
--         , responseAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnectsResponse
--
--         , responseDescribeRoutingProfile $
--             newDescribeRoutingProfileResponse
--
--         , responseDisassociateLexBot $
--             newDisassociateLexBotResponse
--
--         , responseListQuickConnects $
--             newListQuickConnectsResponse
--
--         , responseDeleteQuickConnect $
--             newDeleteQuickConnectResponse
--
--         , responseDescribeContactFlow $
--             newDescribeContactFlowResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueuesResponse
--
--         , responseUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperationResponse
--
--         , responseUpdateUserHierarchy $
--             newUpdateUserHierarchyResponse
--
--         , responseDisassociateLambdaFunction $
--             newDisassociateLambdaFunctionResponse
--
--         , responseUpdateQueueMaxContacts $
--             newUpdateQueueMaxContactsResponse
--
--         , responseDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfigResponse
--
--         , responseListRoutingProfileQueues $
--             newListRoutingProfileQueuesResponse
--
--         , responseUpdateInstanceAttribute $
--             newUpdateInstanceAttributeResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseAssociateLexBot $
--             newAssociateLexBotResponse
--
--         , responseUpdateQueueStatus $
--             newUpdateQueueStatusResponse
--
--         , responseResumeContactRecording $
--             newResumeContactRecordingResponse
--
--         , responseUpdateAgentStatus $
--             newUpdateAgentStatusResponse
--
--         , responseDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroupResponse
--
--         , responseDescribeQueue $
--             newDescribeQueueResponse
--
--         , responseUpdateRoutingProfileName $
--             newUpdateRoutingProfileNameResponse
--
--         , responseDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructureResponse
--
--         , responseStartChatContact $
--             newStartChatContactResponse
--
--         , responseDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociationResponse
--
--         , responseListUseCases $
--             newListUseCasesResponse
--
--         , responseUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfilesResponse
--
--         , responseDeleteHoursOfOperation $
--             newDeleteHoursOfOperationResponse
--
--         , responseUpdateHoursOfOperation $
--             newUpdateHoursOfOperationResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseListHoursOfOperations $
--             newListHoursOfOperationsResponse
--
--         , responseListIntegrationAssociations $
--             newListIntegrationAssociationsResponse
--
--         , responseListContactFlows $
--             newListContactFlowsResponse
--
--         , responseListInstanceStorageConfigs $
--             newListInstanceStorageConfigsResponse
--
--         , responseCreateHoursOfOperation $
--             newCreateHoursOfOperationResponse
--
--         , responseCreateUseCase $
--             newCreateUseCaseResponse
--
--         , responseDisassociateBot $
--             newDisassociateBotResponse
--
--         , responseCreateIntegrationAssociation $
--             newCreateIntegrationAssociationResponse
--
--         , responseUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfigResponse
--
--         , responseUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfoResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--         , responseDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroupResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseDescribeAgentStatus $
--             newDescribeAgentStatusResponse
--
--         , responseListSecurityKeys $
--             newListSecurityKeysResponse
--
--         , responseListUserHierarchyGroups $
--             newListUserHierarchyGroupsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfigResponse
--
--         , responseAssociateBot $
--             newAssociateBotResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseListPrompts $
--             newListPromptsResponse
--
--         , responseStartTaskContact $
--             newStartTaskContactResponse
--
--         , responseUpdateContactFlowContent $
--             newUpdateContactFlowContentResponse
--
--         , responseDescribeHoursOfOperation $
--             newDescribeHoursOfOperationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartOutboundVoiceContact $
--             newStartOutboundVoiceContactResponse
--
--         , responseUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructureResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--           ]
--     ]

-- Requests

requestUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupName -> TestTree
requestUpdateUserHierarchyGroupName =
  req
    "UpdateUserHierarchyGroupName"
    "fixture/UpdateUserHierarchyGroupName.yaml"

requestUpdateUserRoutingProfile :: UpdateUserRoutingProfile -> TestTree
requestUpdateUserRoutingProfile =
  req
    "UpdateUserRoutingProfile"
    "fixture/UpdateUserRoutingProfile.yaml"

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

requestCreateQuickConnect :: CreateQuickConnect -> TestTree
requestCreateQuickConnect =
  req
    "CreateQuickConnect"
    "fixture/CreateQuickConnect.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes =
  req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues =
  req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestDescribeInstance :: DescribeInstance -> TestTree
requestDescribeInstance =
  req
    "DescribeInstance"
    "fixture/DescribeInstance.yaml"

requestUpdateContactFlowName :: UpdateContactFlowName -> TestTree
requestUpdateContactFlowName =
  req
    "UpdateContactFlowName"
    "fixture/UpdateContactFlowName.yaml"

requestListLambdaFunctions :: ListLambdaFunctions -> TestTree
requestListLambdaFunctions =
  req
    "ListLambdaFunctions"
    "fixture/ListLambdaFunctions.yaml"

requestListLexBots :: ListLexBots -> TestTree
requestListLexBots =
  req
    "ListLexBots"
    "fixture/ListLexBots.yaml"

requestAssociateRoutingProfileQueues :: AssociateRoutingProfileQueues -> TestTree
requestAssociateRoutingProfileQueues =
  req
    "AssociateRoutingProfileQueues"
    "fixture/AssociateRoutingProfileQueues.yaml"

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

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes =
  req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestCreateAgentStatus :: CreateAgentStatus -> TestTree
requestCreateAgentStatus =
  req
    "CreateAgentStatus"
    "fixture/CreateAgentStatus.yaml"

requestListApprovedOrigins :: ListApprovedOrigins -> TestTree
requestListApprovedOrigins =
  req
    "ListApprovedOrigins"
    "fixture/ListApprovedOrigins.yaml"

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig =
  req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

requestUpdateContactAttributes :: UpdateContactAttributes -> TestTree
requestUpdateContactAttributes =
  req
    "UpdateContactAttributes"
    "fixture/UpdateContactAttributes.yaml"

requestDeleteUseCase :: DeleteUseCase -> TestTree
requestDeleteUseCase =
  req
    "DeleteUseCase"
    "fixture/DeleteUseCase.yaml"

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles =
  req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

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

requestCreateContactFlow :: CreateContactFlow -> TestTree
requestCreateContactFlow =
  req
    "CreateContactFlow"
    "fixture/CreateContactFlow.yaml"

requestListQueueQuickConnects :: ListQueueQuickConnects -> TestTree
requestListQueueQuickConnects =
  req
    "ListQueueQuickConnects"
    "fixture/ListQueueQuickConnects.yaml"

requestUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrency -> TestTree
requestUpdateRoutingProfileConcurrency =
  req
    "UpdateRoutingProfileConcurrency"
    "fixture/UpdateRoutingProfileConcurrency.yaml"

requestDisassociateQueueQuickConnects :: DisassociateQueueQuickConnects -> TestTree
requestDisassociateQueueQuickConnects =
  req
    "DisassociateQueueQuickConnects"
    "fixture/DisassociateQueueQuickConnects.yaml"

requestUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfig -> TestTree
requestUpdateQueueOutboundCallerConfig =
  req
    "UpdateQueueOutboundCallerConfig"
    "fixture/UpdateQueueOutboundCallerConfig.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData =
  req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

requestCreateRoutingProfile :: CreateRoutingProfile -> TestTree
requestCreateRoutingProfile =
  req
    "CreateRoutingProfile"
    "fixture/CreateRoutingProfile.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDisassociateSecurityKey :: DisassociateSecurityKey -> TestTree
requestDisassociateSecurityKey =
  req
    "DisassociateSecurityKey"
    "fixture/DisassociateSecurityKey.yaml"

requestAssociateApprovedOrigin :: AssociateApprovedOrigin -> TestTree
requestAssociateApprovedOrigin =
  req
    "AssociateApprovedOrigin"
    "fixture/AssociateApprovedOrigin.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateQuickConnectConfig :: UpdateQuickConnectConfig -> TestTree
requestUpdateQuickConnectConfig =
  req
    "UpdateQuickConnectConfig"
    "fixture/UpdateQuickConnectConfig.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact =
  req
    "StopContact"
    "fixture/StopContact.yaml"

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

requestUpdateQuickConnectName :: UpdateQuickConnectName -> TestTree
requestUpdateQuickConnectName =
  req
    "UpdateQuickConnectName"
    "fixture/UpdateQuickConnectName.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateUserHierarchyGroup :: CreateUserHierarchyGroup -> TestTree
requestCreateUserHierarchyGroup =
  req
    "CreateUserHierarchyGroup"
    "fixture/CreateUserHierarchyGroup.yaml"

requestStartContactRecording :: StartContactRecording -> TestTree
requestStartContactRecording =
  req
    "StartContactRecording"
    "fixture/StartContactRecording.yaml"

requestDisassociateApprovedOrigin :: DisassociateApprovedOrigin -> TestTree
requestDisassociateApprovedOrigin =
  req
    "DisassociateApprovedOrigin"
    "fixture/DisassociateApprovedOrigin.yaml"

requestAssociateQueueQuickConnects :: AssociateQueueQuickConnects -> TestTree
requestAssociateQueueQuickConnects =
  req
    "AssociateQueueQuickConnects"
    "fixture/AssociateQueueQuickConnects.yaml"

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

requestListQuickConnects :: ListQuickConnects -> TestTree
requestListQuickConnects =
  req
    "ListQuickConnects"
    "fixture/ListQuickConnects.yaml"

requestDeleteQuickConnect :: DeleteQuickConnect -> TestTree
requestDeleteQuickConnect =
  req
    "DeleteQuickConnect"
    "fixture/DeleteQuickConnect.yaml"

requestDescribeContactFlow :: DescribeContactFlow -> TestTree
requestDescribeContactFlow =
  req
    "DescribeContactFlow"
    "fixture/DescribeContactFlow.yaml"

requestDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueues -> TestTree
requestDisassociateRoutingProfileQueues =
  req
    "DisassociateRoutingProfileQueues"
    "fixture/DisassociateRoutingProfileQueues.yaml"

requestUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperation -> TestTree
requestUpdateQueueHoursOfOperation =
  req
    "UpdateQueueHoursOfOperation"
    "fixture/UpdateQueueHoursOfOperation.yaml"

requestUpdateUserHierarchy :: UpdateUserHierarchy -> TestTree
requestUpdateUserHierarchy =
  req
    "UpdateUserHierarchy"
    "fixture/UpdateUserHierarchy.yaml"

requestDisassociateLambdaFunction :: DisassociateLambdaFunction -> TestTree
requestDisassociateLambdaFunction =
  req
    "DisassociateLambdaFunction"
    "fixture/DisassociateLambdaFunction.yaml"

requestUpdateQueueMaxContacts :: UpdateQueueMaxContacts -> TestTree
requestUpdateQueueMaxContacts =
  req
    "UpdateQueueMaxContacts"
    "fixture/UpdateQueueMaxContacts.yaml"

requestDescribeInstanceStorageConfig :: DescribeInstanceStorageConfig -> TestTree
requestDescribeInstanceStorageConfig =
  req
    "DescribeInstanceStorageConfig"
    "fixture/DescribeInstanceStorageConfig.yaml"

requestListRoutingProfileQueues :: ListRoutingProfileQueues -> TestTree
requestListRoutingProfileQueues =
  req
    "ListRoutingProfileQueues"
    "fixture/ListRoutingProfileQueues.yaml"

requestUpdateInstanceAttribute :: UpdateInstanceAttribute -> TestTree
requestUpdateInstanceAttribute =
  req
    "UpdateInstanceAttribute"
    "fixture/UpdateInstanceAttribute.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestAssociateLexBot :: AssociateLexBot -> TestTree
requestAssociateLexBot =
  req
    "AssociateLexBot"
    "fixture/AssociateLexBot.yaml"

requestUpdateQueueStatus :: UpdateQueueStatus -> TestTree
requestUpdateQueueStatus =
  req
    "UpdateQueueStatus"
    "fixture/UpdateQueueStatus.yaml"

requestResumeContactRecording :: ResumeContactRecording -> TestTree
requestResumeContactRecording =
  req
    "ResumeContactRecording"
    "fixture/ResumeContactRecording.yaml"

requestUpdateAgentStatus :: UpdateAgentStatus -> TestTree
requestUpdateAgentStatus =
  req
    "UpdateAgentStatus"
    "fixture/UpdateAgentStatus.yaml"

requestDescribeUserHierarchyGroup :: DescribeUserHierarchyGroup -> TestTree
requestDescribeUserHierarchyGroup =
  req
    "DescribeUserHierarchyGroup"
    "fixture/DescribeUserHierarchyGroup.yaml"

requestDescribeQueue :: DescribeQueue -> TestTree
requestDescribeQueue =
  req
    "DescribeQueue"
    "fixture/DescribeQueue.yaml"

requestUpdateRoutingProfileName :: UpdateRoutingProfileName -> TestTree
requestUpdateRoutingProfileName =
  req
    "UpdateRoutingProfileName"
    "fixture/UpdateRoutingProfileName.yaml"

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure =
  req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact =
  req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

requestDeleteIntegrationAssociation :: DeleteIntegrationAssociation -> TestTree
requestDeleteIntegrationAssociation =
  req
    "DeleteIntegrationAssociation"
    "fixture/DeleteIntegrationAssociation.yaml"

requestListUseCases :: ListUseCases -> TestTree
requestListUseCases =
  req
    "ListUseCases"
    "fixture/ListUseCases.yaml"

requestUpdateUserSecurityProfiles :: UpdateUserSecurityProfiles -> TestTree
requestUpdateUserSecurityProfiles =
  req
    "UpdateUserSecurityProfiles"
    "fixture/UpdateUserSecurityProfiles.yaml"

requestDeleteHoursOfOperation :: DeleteHoursOfOperation -> TestTree
requestDeleteHoursOfOperation =
  req
    "DeleteHoursOfOperation"
    "fixture/DeleteHoursOfOperation.yaml"

requestUpdateHoursOfOperation :: UpdateHoursOfOperation -> TestTree
requestUpdateHoursOfOperation =
  req
    "UpdateHoursOfOperation"
    "fixture/UpdateHoursOfOperation.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

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

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows =
  req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestListInstanceStorageConfigs :: ListInstanceStorageConfigs -> TestTree
requestListInstanceStorageConfigs =
  req
    "ListInstanceStorageConfigs"
    "fixture/ListInstanceStorageConfigs.yaml"

requestCreateHoursOfOperation :: CreateHoursOfOperation -> TestTree
requestCreateHoursOfOperation =
  req
    "CreateHoursOfOperation"
    "fixture/CreateHoursOfOperation.yaml"

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

requestCreateIntegrationAssociation :: CreateIntegrationAssociation -> TestTree
requestCreateIntegrationAssociation =
  req
    "CreateIntegrationAssociation"
    "fixture/CreateIntegrationAssociation.yaml"

requestUpdateInstanceStorageConfig :: UpdateInstanceStorageConfig -> TestTree
requestUpdateInstanceStorageConfig =
  req
    "UpdateInstanceStorageConfig"
    "fixture/UpdateInstanceStorageConfig.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo =
  req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

requestUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueue -> TestTree
requestUpdateRoutingProfileDefaultOutboundQueue =
  req
    "UpdateRoutingProfileDefaultOutboundQueue"
    "fixture/UpdateRoutingProfileDefaultOutboundQueue.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken =
  req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestDeleteUserHierarchyGroup :: DeleteUserHierarchyGroup -> TestTree
requestDeleteUserHierarchyGroup =
  req
    "DeleteUserHierarchyGroup"
    "fixture/DeleteUserHierarchyGroup.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDescribeAgentStatus :: DescribeAgentStatus -> TestTree
requestDescribeAgentStatus =
  req
    "DescribeAgentStatus"
    "fixture/DescribeAgentStatus.yaml"

requestListSecurityKeys :: ListSecurityKeys -> TestTree
requestListSecurityKeys =
  req
    "ListSecurityKeys"
    "fixture/ListSecurityKeys.yaml"

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups =
  req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfig -> TestTree
requestDisassociateInstanceStorageConfig =
  req
    "DisassociateInstanceStorageConfig"
    "fixture/DisassociateInstanceStorageConfig.yaml"

requestAssociateBot :: AssociateBot -> TestTree
requestAssociateBot =
  req
    "AssociateBot"
    "fixture/AssociateBot.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestListPrompts :: ListPrompts -> TestTree
requestListPrompts =
  req
    "ListPrompts"
    "fixture/ListPrompts.yaml"

requestStartTaskContact :: StartTaskContact -> TestTree
requestStartTaskContact =
  req
    "StartTaskContact"
    "fixture/StartTaskContact.yaml"

requestUpdateContactFlowContent :: UpdateContactFlowContent -> TestTree
requestUpdateContactFlowContent =
  req
    "UpdateContactFlowContent"
    "fixture/UpdateContactFlowContent.yaml"

requestDescribeHoursOfOperation :: DescribeHoursOfOperation -> TestTree
requestDescribeHoursOfOperation =
  req
    "DescribeHoursOfOperation"
    "fixture/DescribeHoursOfOperation.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact =
  req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestUpdateUserHierarchyStructure :: UpdateUserHierarchyStructure -> TestTree
requestUpdateUserHierarchyStructure =
  req
    "UpdateUserHierarchyStructure"
    "fixture/UpdateUserHierarchyStructure.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

-- Responses

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName =
  res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserHierarchyGroupName)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile =
  res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserRoutingProfile)

responseSuspendContactRecording :: SuspendContactRecordingResponse -> TestTree
responseSuspendContactRecording =
  res
    "SuspendContactRecordingResponse"
    "fixture/SuspendContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy SuspendContactRecording)

responseUpdateQueueName :: UpdateQueueNameResponse -> TestTree
responseUpdateQueueName =
  res
    "UpdateQueueNameResponse"
    "fixture/UpdateQueueNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueName)

responseCreateQuickConnect :: CreateQuickConnectResponse -> TestTree
responseCreateQuickConnect =
  res
    "CreateQuickConnectResponse"
    "fixture/CreateQuickConnectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQuickConnect)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes =
  res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceAttributes)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfiles)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues =
  res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileQueues)

responseDescribeInstance :: DescribeInstanceResponse -> TestTree
responseDescribeInstance =
  res
    "DescribeInstanceResponse"
    "fixture/DescribeInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstance)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName =
  res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactFlowName)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions =
  res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLambdaFunctions)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots =
  res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLexBots)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues =
  res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRoutingProfileQueues)

responseListAgentStatuses :: ListAgentStatusesResponse -> TestTree
responseListAgentStatuses =
  res
    "ListAgentStatusesResponse"
    "fixture/ListAgentStatusesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAgentStatuses)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction =
  res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLambdaFunction)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes =
  res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactAttributes)

responseCreateAgentStatus :: CreateAgentStatusResponse -> TestTree
responseCreateAgentStatus =
  res
    "CreateAgentStatusResponse"
    "fixture/CreateAgentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAgentStatus)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins =
  res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApprovedOrigins)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig =
  res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPhoneConfig)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes =
  res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactAttributes)

responseDeleteUseCase :: DeleteUseCaseResponse -> TestTree
responseDeleteUseCase =
  res
    "DeleteUseCaseResponse"
    "fixture/DeleteUseCaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUseCase)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles =
  res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoutingProfiles)

responseDescribeQuickConnect :: DescribeQuickConnectResponse -> TestTree
responseDescribeQuickConnect =
  res
    "DescribeQuickConnectResponse"
    "fixture/DescribeQuickConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQuickConnect)

responseAssociateInstanceStorageConfig :: AssociateInstanceStorageConfigResponse -> TestTree
responseAssociateInstanceStorageConfig =
  res
    "AssociateInstanceStorageConfigResponse"
    "fixture/AssociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateInstanceStorageConfig)

responseCreateContactFlow :: CreateContactFlowResponse -> TestTree
responseCreateContactFlow =
  res
    "CreateContactFlowResponse"
    "fixture/CreateContactFlowResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactFlow)

responseListQueueQuickConnects :: ListQueueQuickConnectsResponse -> TestTree
responseListQueueQuickConnects =
  res
    "ListQueueQuickConnectsResponse"
    "fixture/ListQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueueQuickConnects)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency =
  res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileConcurrency)

responseDisassociateQueueQuickConnects :: DisassociateQueueQuickConnectsResponse -> TestTree
responseDisassociateQueueQuickConnects =
  res
    "DisassociateQueueQuickConnectsResponse"
    "fixture/DisassociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateQueueQuickConnects)

responseUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfigResponse -> TestTree
responseUpdateQueueOutboundCallerConfig =
  res
    "UpdateQueueOutboundCallerConfigResponse"
    "fixture/UpdateQueueOutboundCallerConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueOutboundCallerConfig)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData =
  res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetCurrentMetricData)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile =
  res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoutingProfile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey =
  res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSecurityKey)

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin =
  res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateApprovedOrigin)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueues)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBots)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUpdateQuickConnectConfig :: UpdateQuickConnectConfigResponse -> TestTree
responseUpdateQuickConnectConfig =
  res
    "UpdateQuickConnectConfigResponse"
    "fixture/UpdateQuickConnectConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQuickConnectConfig)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstances)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstance)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact =
  res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    defaultService
    (Proxy :: Proxy StopContact)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey =
  res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSecurityKey)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording =
  res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy StopContactRecording)

responseUpdateQuickConnectName :: UpdateQuickConnectNameResponse -> TestTree
responseUpdateQuickConnectName =
  res
    "UpdateQuickConnectNameResponse"
    "fixture/UpdateQuickConnectNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQuickConnectName)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseCreateUserHierarchyGroup :: CreateUserHierarchyGroupResponse -> TestTree
responseCreateUserHierarchyGroup =
  res
    "CreateUserHierarchyGroupResponse"
    "fixture/CreateUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserHierarchyGroup)

responseStartContactRecording :: StartContactRecordingResponse -> TestTree
responseStartContactRecording =
  res
    "StartContactRecordingResponse"
    "fixture/StartContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy StartContactRecording)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin =
  res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateApprovedOrigin)

responseAssociateQueueQuickConnects :: AssociateQueueQuickConnectsResponse -> TestTree
responseAssociateQueueQuickConnects =
  res
    "AssociateQueueQuickConnectsResponse"
    "fixture/AssociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateQueueQuickConnects)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile =
  res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRoutingProfile)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot =
  res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateLexBot)

responseListQuickConnects :: ListQuickConnectsResponse -> TestTree
responseListQuickConnects =
  res
    "ListQuickConnectsResponse"
    "fixture/ListQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQuickConnects)

responseDeleteQuickConnect :: DeleteQuickConnectResponse -> TestTree
responseDeleteQuickConnect =
  res
    "DeleteQuickConnectResponse"
    "fixture/DeleteQuickConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQuickConnect)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow =
  res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContactFlow)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues =
  res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateRoutingProfileQueues)

responseUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperationResponse -> TestTree
responseUpdateQueueHoursOfOperation =
  res
    "UpdateQueueHoursOfOperationResponse"
    "fixture/UpdateQueueHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueHoursOfOperation)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy =
  res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserHierarchy)

responseDisassociateLambdaFunction :: DisassociateLambdaFunctionResponse -> TestTree
responseDisassociateLambdaFunction =
  res
    "DisassociateLambdaFunctionResponse"
    "fixture/DisassociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateLambdaFunction)

responseUpdateQueueMaxContacts :: UpdateQueueMaxContactsResponse -> TestTree
responseUpdateQueueMaxContacts =
  res
    "UpdateQueueMaxContactsResponse"
    "fixture/UpdateQueueMaxContactsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueMaxContacts)

responseDescribeInstanceStorageConfig :: DescribeInstanceStorageConfigResponse -> TestTree
responseDescribeInstanceStorageConfig =
  res
    "DescribeInstanceStorageConfigResponse"
    "fixture/DescribeInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceStorageConfig)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues =
  res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoutingProfileQueues)

responseUpdateInstanceAttribute :: UpdateInstanceAttributeResponse -> TestTree
responseUpdateInstanceAttribute =
  res
    "UpdateInstanceAttributeResponse"
    "fixture/UpdateInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstanceAttribute)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUser)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot =
  res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLexBot)

responseUpdateQueueStatus :: UpdateQueueStatusResponse -> TestTree
responseUpdateQueueStatus =
  res
    "UpdateQueueStatusResponse"
    "fixture/UpdateQueueStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueStatus)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording =
  res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeContactRecording)

responseUpdateAgentStatus :: UpdateAgentStatusResponse -> TestTree
responseUpdateAgentStatus =
  res
    "UpdateAgentStatusResponse"
    "fixture/UpdateAgentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAgentStatus)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup =
  res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserHierarchyGroup)

responseDescribeQueue :: DescribeQueueResponse -> TestTree
responseDescribeQueue =
  res
    "DescribeQueueResponse"
    "fixture/DescribeQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQueue)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName =
  res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileName)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure =
  res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserHierarchyStructure)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact =
  res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    defaultService
    (Proxy :: Proxy StartChatContact)

responseDeleteIntegrationAssociation :: DeleteIntegrationAssociationResponse -> TestTree
responseDeleteIntegrationAssociation =
  res
    "DeleteIntegrationAssociationResponse"
    "fixture/DeleteIntegrationAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntegrationAssociation)

responseListUseCases :: ListUseCasesResponse -> TestTree
responseListUseCases =
  res
    "ListUseCasesResponse"
    "fixture/ListUseCasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUseCases)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles =
  res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserSecurityProfiles)

responseDeleteHoursOfOperation :: DeleteHoursOfOperationResponse -> TestTree
responseDeleteHoursOfOperation =
  res
    "DeleteHoursOfOperationResponse"
    "fixture/DeleteHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHoursOfOperation)

responseUpdateHoursOfOperation :: UpdateHoursOfOperationResponse -> TestTree
responseUpdateHoursOfOperation =
  res
    "UpdateHoursOfOperationResponse"
    "fixture/UpdateHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateHoursOfOperation)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumbers)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations =
  res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHoursOfOperations)

responseListIntegrationAssociations :: ListIntegrationAssociationsResponse -> TestTree
responseListIntegrationAssociations =
  res
    "ListIntegrationAssociationsResponse"
    "fixture/ListIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIntegrationAssociations)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows =
  res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContactFlows)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs =
  res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceStorageConfigs)

responseCreateHoursOfOperation :: CreateHoursOfOperationResponse -> TestTree
responseCreateHoursOfOperation =
  res
    "CreateHoursOfOperationResponse"
    "fixture/CreateHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHoursOfOperation)

responseCreateUseCase :: CreateUseCaseResponse -> TestTree
responseCreateUseCase =
  res
    "CreateUseCaseResponse"
    "fixture/CreateUseCaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUseCase)

responseDisassociateBot :: DisassociateBotResponse -> TestTree
responseDisassociateBot =
  res
    "DisassociateBotResponse"
    "fixture/DisassociateBotResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateBot)

responseCreateIntegrationAssociation :: CreateIntegrationAssociationResponse -> TestTree
responseCreateIntegrationAssociation =
  res
    "CreateIntegrationAssociationResponse"
    "fixture/CreateIntegrationAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIntegrationAssociation)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig =
  res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstanceStorageConfig)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo =
  res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserIdentityInfo)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue =
  res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetFederationToken)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup =
  res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserHierarchyGroup)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDescribeAgentStatus :: DescribeAgentStatusResponse -> TestTree
responseDescribeAgentStatus =
  res
    "DescribeAgentStatusResponse"
    "fixture/DescribeAgentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAgentStatus)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys =
  res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityKeys)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups =
  res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserHierarchyGroups)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfigResponse -> TestTree
responseDisassociateInstanceStorageConfig =
  res
    "DisassociateInstanceStorageConfigResponse"
    "fixture/DisassociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateInstanceStorageConfig)

responseAssociateBot :: AssociateBotResponse -> TestTree
responseAssociateBot =
  res
    "AssociateBotResponse"
    "fixture/AssociateBotResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateBot)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstance)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQueue)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts =
  res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrompts)

responseStartTaskContact :: StartTaskContactResponse -> TestTree
responseStartTaskContact =
  res
    "StartTaskContactResponse"
    "fixture/StartTaskContactResponse.proto"
    defaultService
    (Proxy :: Proxy StartTaskContact)

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent =
  res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactFlowContent)

responseDescribeHoursOfOperation :: DescribeHoursOfOperationResponse -> TestTree
responseDescribeHoursOfOperation =
  res
    "DescribeHoursOfOperationResponse"
    "fixture/DescribeHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHoursOfOperation)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact =
  res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    defaultService
    (Proxy :: Proxy StartOutboundVoiceContact)

responseUpdateUserHierarchyStructure :: UpdateUserHierarchyStructureResponse -> TestTree
responseUpdateUserHierarchyStructure =
  res
    "UpdateUserHierarchyStructureResponse"
    "fixture/UpdateUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserHierarchyStructure)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricData)

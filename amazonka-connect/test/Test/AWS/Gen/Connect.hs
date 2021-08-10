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
--         [ requestSuspendContactRecording $
--             newSuspendContactRecording
--
--         , requestUpdateQueueName $
--             newUpdateQueueName
--
--         , requestCreateQuickConnect $
--             newCreateQuickConnect
--
--         , requestUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfile
--
--         , requestUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupName
--
--         , requestUpdateContactFlowName $
--             newUpdateContactFlowName
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestDescribeInstance $
--             newDescribeInstance
--
--         , requestListInstanceAttributes $
--             newListInstanceAttributes
--
--         , requestListLambdaFunctions $
--             newListLambdaFunctions
--
--         , requestUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueues
--
--         , requestAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueues
--
--         , requestGetContactAttributes $
--             newGetContactAttributes
--
--         , requestListLexBots $
--             newListLexBots
--
--         , requestAssociateLambdaFunction $
--             newAssociateLambdaFunction
--
--         , requestListApprovedOrigins $
--             newListApprovedOrigins
--
--         , requestAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfig
--
--         , requestCreateContactFlow $
--             newCreateContactFlow
--
--         , requestUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfig
--
--         , requestUpdateContactAttributes $
--             newUpdateContactAttributes
--
--         , requestListRoutingProfiles $
--             newListRoutingProfiles
--
--         , requestDeleteUseCase $
--             newDeleteUseCase
--
--         , requestDescribeQuickConnect $
--             newDescribeQuickConnect
--
--         , requestListQueueQuickConnects $
--             newListQueueQuickConnects
--
--         , requestCreateRoutingProfile $
--             newCreateRoutingProfile
--
--         , requestAssociateApprovedOrigin $
--             newAssociateApprovedOrigin
--
--         , requestDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnects
--
--         , requestUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrency
--
--         , requestUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfig
--
--         , requestDisassociateSecurityKey $
--             newDisassociateSecurityKey
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetCurrentMetricData $
--             newGetCurrentMetricData
--
--         , requestUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfig
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListQueues $
--             newListQueues
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopContact $
--             newStopContact
--
--         , requestCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroup
--
--         , requestStartContactRecording $
--             newStartContactRecording
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestAssociateSecurityKey $
--             newAssociateSecurityKey
--
--         , requestAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnects
--
--         , requestStopContactRecording $
--             newStopContactRecording
--
--         , requestDisassociateApprovedOrigin $
--             newDisassociateApprovedOrigin
--
--         , requestUpdateQuickConnectName $
--             newUpdateQuickConnectName
--
--         , requestDescribeRoutingProfile $
--             newDescribeRoutingProfile
--
--         , requestListQuickConnects $
--             newListQuickConnects
--
--         , requestDisassociateLexBot $
--             newDisassociateLexBot
--
--         , requestDeleteQuickConnect $
--             newDeleteQuickConnect
--
--         , requestListRoutingProfileQueues $
--             newListRoutingProfileQueues
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
--         , requestUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperation
--
--         , requestDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueues
--
--         , requestDescribeContactFlow $
--             newDescribeContactFlow
--
--         , requestUpdateQueueStatus $
--             newUpdateQueueStatus
--
--         , requestDescribeQueue $
--             newDescribeQueue
--
--         , requestAssociateLexBot $
--             newAssociateLexBot
--
--         , requestUpdateInstanceAttribute $
--             newUpdateInstanceAttribute
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroup
--
--         , requestResumeContactRecording $
--             newResumeContactRecording
--
--         , requestUpdateRoutingProfileName $
--             newUpdateRoutingProfileName
--
--         , requestStartChatContact $
--             newStartChatContact
--
--         , requestDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociation
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestListIntegrationAssociations $
--             newListIntegrationAssociations
--
--         , requestListUseCases $
--             newListUseCases
--
--         , requestUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfiles
--
--         , requestDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructure
--
--         , requestListHoursOfOperations $
--             newListHoursOfOperations
--
--         , requestCreateUseCase $
--             newCreateUseCase
--
--         , requestListContactFlows $
--             newListContactFlows
--
--         , requestUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfig
--
--         , requestListInstanceStorageConfigs $
--             newListInstanceStorageConfigs
--
--         , requestCreateIntegrationAssociation $
--             newCreateIntegrationAssociation
--
--         , requestDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroup
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfig
--
--         , requestListUserHierarchyGroups $
--             newListUserHierarchyGroups
--
--         , requestUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfo
--
--         , requestListUsers $
--             newListUsers
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestListSecurityKeys $
--             newListSecurityKeys
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueue
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestStartTaskContact $
--             newStartTaskContact
--
--         , requestListPrompts $
--             newListPrompts
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
--         , requestUpdateContactFlowContent $
--             newUpdateContactFlowContent
--
--         , requestGetMetricData $
--             newGetMetricData
--
--         , requestDescribeHoursOfOperation $
--             newDescribeHoursOfOperation
--
--           ]

--     , testGroup "response"
--         [ responseSuspendContactRecording $
--             newSuspendContactRecordingResponse
--
--         , responseUpdateQueueName $
--             newUpdateQueueNameResponse
--
--         , responseCreateQuickConnect $
--             newCreateQuickConnectResponse
--
--         , responseUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfileResponse
--
--         , responseUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupNameResponse
--
--         , responseUpdateContactFlowName $
--             newUpdateContactFlowNameResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseDescribeInstance $
--             newDescribeInstanceResponse
--
--         , responseListInstanceAttributes $
--             newListInstanceAttributesResponse
--
--         , responseListLambdaFunctions $
--             newListLambdaFunctionsResponse
--
--         , responseUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueuesResponse
--
--         , responseAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueuesResponse
--
--         , responseGetContactAttributes $
--             newGetContactAttributesResponse
--
--         , responseListLexBots $
--             newListLexBotsResponse
--
--         , responseAssociateLambdaFunction $
--             newAssociateLambdaFunctionResponse
--
--         , responseListApprovedOrigins $
--             newListApprovedOriginsResponse
--
--         , responseAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfigResponse
--
--         , responseCreateContactFlow $
--             newCreateContactFlowResponse
--
--         , responseUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfigResponse
--
--         , responseUpdateContactAttributes $
--             newUpdateContactAttributesResponse
--
--         , responseListRoutingProfiles $
--             newListRoutingProfilesResponse
--
--         , responseDeleteUseCase $
--             newDeleteUseCaseResponse
--
--         , responseDescribeQuickConnect $
--             newDescribeQuickConnectResponse
--
--         , responseListQueueQuickConnects $
--             newListQueueQuickConnectsResponse
--
--         , responseCreateRoutingProfile $
--             newCreateRoutingProfileResponse
--
--         , responseAssociateApprovedOrigin $
--             newAssociateApprovedOriginResponse
--
--         , responseDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnectsResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrencyResponse
--
--         , responseUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfigResponse
--
--         , responseDisassociateSecurityKey $
--             newDisassociateSecurityKeyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetCurrentMetricData $
--             newGetCurrentMetricDataResponse
--
--         , responseUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfigResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopContact $
--             newStopContactResponse
--
--         , responseCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroupResponse
--
--         , responseStartContactRecording $
--             newStartContactRecordingResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseAssociateSecurityKey $
--             newAssociateSecurityKeyResponse
--
--         , responseAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnectsResponse
--
--         , responseStopContactRecording $
--             newStopContactRecordingResponse
--
--         , responseDisassociateApprovedOrigin $
--             newDisassociateApprovedOriginResponse
--
--         , responseUpdateQuickConnectName $
--             newUpdateQuickConnectNameResponse
--
--         , responseDescribeRoutingProfile $
--             newDescribeRoutingProfileResponse
--
--         , responseListQuickConnects $
--             newListQuickConnectsResponse
--
--         , responseDisassociateLexBot $
--             newDisassociateLexBotResponse
--
--         , responseDeleteQuickConnect $
--             newDeleteQuickConnectResponse
--
--         , responseListRoutingProfileQueues $
--             newListRoutingProfileQueuesResponse
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
--         , responseUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperationResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueuesResponse
--
--         , responseDescribeContactFlow $
--             newDescribeContactFlowResponse
--
--         , responseUpdateQueueStatus $
--             newUpdateQueueStatusResponse
--
--         , responseDescribeQueue $
--             newDescribeQueueResponse
--
--         , responseAssociateLexBot $
--             newAssociateLexBotResponse
--
--         , responseUpdateInstanceAttribute $
--             newUpdateInstanceAttributeResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroupResponse
--
--         , responseResumeContactRecording $
--             newResumeContactRecordingResponse
--
--         , responseUpdateRoutingProfileName $
--             newUpdateRoutingProfileNameResponse
--
--         , responseStartChatContact $
--             newStartChatContactResponse
--
--         , responseDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociationResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseListIntegrationAssociations $
--             newListIntegrationAssociationsResponse
--
--         , responseListUseCases $
--             newListUseCasesResponse
--
--         , responseUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfilesResponse
--
--         , responseDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructureResponse
--
--         , responseListHoursOfOperations $
--             newListHoursOfOperationsResponse
--
--         , responseCreateUseCase $
--             newCreateUseCaseResponse
--
--         , responseListContactFlows $
--             newListContactFlowsResponse
--
--         , responseUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfigResponse
--
--         , responseListInstanceStorageConfigs $
--             newListInstanceStorageConfigsResponse
--
--         , responseCreateIntegrationAssociation $
--             newCreateIntegrationAssociationResponse
--
--         , responseDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroupResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfigResponse
--
--         , responseListUserHierarchyGroups $
--             newListUserHierarchyGroupsResponse
--
--         , responseUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfoResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseListSecurityKeys $
--             newListSecurityKeysResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseStartTaskContact $
--             newStartTaskContactResponse
--
--         , responseListPrompts $
--             newListPromptsResponse
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
--         , responseUpdateContactFlowContent $
--             newUpdateContactFlowContentResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--         , responseDescribeHoursOfOperation $
--             newDescribeHoursOfOperationResponse
--
--           ]
--     ]

-- Requests

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

requestUpdateContactFlowName :: UpdateContactFlowName -> TestTree
requestUpdateContactFlowName =
  req
    "UpdateContactFlowName"
    "fixture/UpdateContactFlowName.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestDescribeInstance :: DescribeInstance -> TestTree
requestDescribeInstance =
  req
    "DescribeInstance"
    "fixture/DescribeInstance.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes =
  req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestListLambdaFunctions :: ListLambdaFunctions -> TestTree
requestListLambdaFunctions =
  req
    "ListLambdaFunctions"
    "fixture/ListLambdaFunctions.yaml"

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues =
  req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestAssociateRoutingProfileQueues :: AssociateRoutingProfileQueues -> TestTree
requestAssociateRoutingProfileQueues =
  req
    "AssociateRoutingProfileQueues"
    "fixture/AssociateRoutingProfileQueues.yaml"

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes =
  req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestListLexBots :: ListLexBots -> TestTree
requestListLexBots =
  req
    "ListLexBots"
    "fixture/ListLexBots.yaml"

requestAssociateLambdaFunction :: AssociateLambdaFunction -> TestTree
requestAssociateLambdaFunction =
  req
    "AssociateLambdaFunction"
    "fixture/AssociateLambdaFunction.yaml"

requestListApprovedOrigins :: ListApprovedOrigins -> TestTree
requestListApprovedOrigins =
  req
    "ListApprovedOrigins"
    "fixture/ListApprovedOrigins.yaml"

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

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles =
  req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

requestDeleteUseCase :: DeleteUseCase -> TestTree
requestDeleteUseCase =
  req
    "DeleteUseCase"
    "fixture/DeleteUseCase.yaml"

requestDescribeQuickConnect :: DescribeQuickConnect -> TestTree
requestDescribeQuickConnect =
  req
    "DescribeQuickConnect"
    "fixture/DescribeQuickConnect.yaml"

requestListQueueQuickConnects :: ListQueueQuickConnects -> TestTree
requestListQueueQuickConnects =
  req
    "ListQueueQuickConnects"
    "fixture/ListQueueQuickConnects.yaml"

requestCreateRoutingProfile :: CreateRoutingProfile -> TestTree
requestCreateRoutingProfile =
  req
    "CreateRoutingProfile"
    "fixture/CreateRoutingProfile.yaml"

requestAssociateApprovedOrigin :: AssociateApprovedOrigin -> TestTree
requestAssociateApprovedOrigin =
  req
    "AssociateApprovedOrigin"
    "fixture/AssociateApprovedOrigin.yaml"

requestDisassociateQueueQuickConnects :: DisassociateQueueQuickConnects -> TestTree
requestDisassociateQueueQuickConnects =
  req
    "DisassociateQueueQuickConnects"
    "fixture/DisassociateQueueQuickConnects.yaml"

requestUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrency -> TestTree
requestUpdateRoutingProfileConcurrency =
  req
    "UpdateRoutingProfileConcurrency"
    "fixture/UpdateRoutingProfileConcurrency.yaml"

requestUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfig -> TestTree
requestUpdateQueueOutboundCallerConfig =
  req
    "UpdateQueueOutboundCallerConfig"
    "fixture/UpdateQueueOutboundCallerConfig.yaml"

requestDisassociateSecurityKey :: DisassociateSecurityKey -> TestTree
requestDisassociateSecurityKey =
  req
    "DisassociateSecurityKey"
    "fixture/DisassociateSecurityKey.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData =
  req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

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

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact =
  req
    "StopContact"
    "fixture/StopContact.yaml"

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

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestAssociateSecurityKey :: AssociateSecurityKey -> TestTree
requestAssociateSecurityKey =
  req
    "AssociateSecurityKey"
    "fixture/AssociateSecurityKey.yaml"

requestAssociateQueueQuickConnects :: AssociateQueueQuickConnects -> TestTree
requestAssociateQueueQuickConnects =
  req
    "AssociateQueueQuickConnects"
    "fixture/AssociateQueueQuickConnects.yaml"

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

requestUpdateQuickConnectName :: UpdateQuickConnectName -> TestTree
requestUpdateQuickConnectName =
  req
    "UpdateQuickConnectName"
    "fixture/UpdateQuickConnectName.yaml"

requestDescribeRoutingProfile :: DescribeRoutingProfile -> TestTree
requestDescribeRoutingProfile =
  req
    "DescribeRoutingProfile"
    "fixture/DescribeRoutingProfile.yaml"

requestListQuickConnects :: ListQuickConnects -> TestTree
requestListQuickConnects =
  req
    "ListQuickConnects"
    "fixture/ListQuickConnects.yaml"

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

requestListRoutingProfileQueues :: ListRoutingProfileQueues -> TestTree
requestListRoutingProfileQueues =
  req
    "ListRoutingProfileQueues"
    "fixture/ListRoutingProfileQueues.yaml"

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

requestUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperation -> TestTree
requestUpdateQueueHoursOfOperation =
  req
    "UpdateQueueHoursOfOperation"
    "fixture/UpdateQueueHoursOfOperation.yaml"

requestDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueues -> TestTree
requestDisassociateRoutingProfileQueues =
  req
    "DisassociateRoutingProfileQueues"
    "fixture/DisassociateRoutingProfileQueues.yaml"

requestDescribeContactFlow :: DescribeContactFlow -> TestTree
requestDescribeContactFlow =
  req
    "DescribeContactFlow"
    "fixture/DescribeContactFlow.yaml"

requestUpdateQueueStatus :: UpdateQueueStatus -> TestTree
requestUpdateQueueStatus =
  req
    "UpdateQueueStatus"
    "fixture/UpdateQueueStatus.yaml"

requestDescribeQueue :: DescribeQueue -> TestTree
requestDescribeQueue =
  req
    "DescribeQueue"
    "fixture/DescribeQueue.yaml"

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

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDescribeUserHierarchyGroup :: DescribeUserHierarchyGroup -> TestTree
requestDescribeUserHierarchyGroup =
  req
    "DescribeUserHierarchyGroup"
    "fixture/DescribeUserHierarchyGroup.yaml"

requestResumeContactRecording :: ResumeContactRecording -> TestTree
requestResumeContactRecording =
  req
    "ResumeContactRecording"
    "fixture/ResumeContactRecording.yaml"

requestUpdateRoutingProfileName :: UpdateRoutingProfileName -> TestTree
requestUpdateRoutingProfileName =
  req
    "UpdateRoutingProfileName"
    "fixture/UpdateRoutingProfileName.yaml"

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

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestListIntegrationAssociations :: ListIntegrationAssociations -> TestTree
requestListIntegrationAssociations =
  req
    "ListIntegrationAssociations"
    "fixture/ListIntegrationAssociations.yaml"

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

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure =
  req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestListHoursOfOperations :: ListHoursOfOperations -> TestTree
requestListHoursOfOperations =
  req
    "ListHoursOfOperations"
    "fixture/ListHoursOfOperations.yaml"

requestCreateUseCase :: CreateUseCase -> TestTree
requestCreateUseCase =
  req
    "CreateUseCase"
    "fixture/CreateUseCase.yaml"

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows =
  req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestUpdateInstanceStorageConfig :: UpdateInstanceStorageConfig -> TestTree
requestUpdateInstanceStorageConfig =
  req
    "UpdateInstanceStorageConfig"
    "fixture/UpdateInstanceStorageConfig.yaml"

requestListInstanceStorageConfigs :: ListInstanceStorageConfigs -> TestTree
requestListInstanceStorageConfigs =
  req
    "ListInstanceStorageConfigs"
    "fixture/ListInstanceStorageConfigs.yaml"

requestCreateIntegrationAssociation :: CreateIntegrationAssociation -> TestTree
requestCreateIntegrationAssociation =
  req
    "CreateIntegrationAssociation"
    "fixture/CreateIntegrationAssociation.yaml"

requestDeleteUserHierarchyGroup :: DeleteUserHierarchyGroup -> TestTree
requestDeleteUserHierarchyGroup =
  req
    "DeleteUserHierarchyGroup"
    "fixture/DeleteUserHierarchyGroup.yaml"

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

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups =
  req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo =
  req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

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

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestListSecurityKeys :: ListSecurityKeys -> TestTree
requestListSecurityKeys =
  req
    "ListSecurityKeys"
    "fixture/ListSecurityKeys.yaml"

requestUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueue -> TestTree
requestUpdateRoutingProfileDefaultOutboundQueue =
  req
    "UpdateRoutingProfileDefaultOutboundQueue"
    "fixture/UpdateRoutingProfileDefaultOutboundQueue.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestStartTaskContact :: StartTaskContact -> TestTree
requestStartTaskContact =
  req
    "StartTaskContact"
    "fixture/StartTaskContact.yaml"

requestListPrompts :: ListPrompts -> TestTree
requestListPrompts =
  req
    "ListPrompts"
    "fixture/ListPrompts.yaml"

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

requestUpdateContactFlowContent :: UpdateContactFlowContent -> TestTree
requestUpdateContactFlowContent =
  req
    "UpdateContactFlowContent"
    "fixture/UpdateContactFlowContent.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestDescribeHoursOfOperation :: DescribeHoursOfOperation -> TestTree
requestDescribeHoursOfOperation =
  req
    "DescribeHoursOfOperation"
    "fixture/DescribeHoursOfOperation.yaml"

-- Responses

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

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile =
  res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserRoutingProfile)

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName =
  res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserHierarchyGroupName)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName =
  res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactFlowName)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityProfiles)

responseDescribeInstance :: DescribeInstanceResponse -> TestTree
responseDescribeInstance =
  res
    "DescribeInstanceResponse"
    "fixture/DescribeInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstance)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes =
  res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceAttributes)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions =
  res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLambdaFunctions)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues =
  res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileQueues)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues =
  res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRoutingProfileQueues)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes =
  res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactAttributes)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots =
  res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLexBots)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction =
  res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLambdaFunction)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins =
  res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApprovedOrigins)

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

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles =
  res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoutingProfiles)

responseDeleteUseCase :: DeleteUseCaseResponse -> TestTree
responseDeleteUseCase =
  res
    "DeleteUseCaseResponse"
    "fixture/DeleteUseCaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUseCase)

responseDescribeQuickConnect :: DescribeQuickConnectResponse -> TestTree
responseDescribeQuickConnect =
  res
    "DescribeQuickConnectResponse"
    "fixture/DescribeQuickConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQuickConnect)

responseListQueueQuickConnects :: ListQueueQuickConnectsResponse -> TestTree
responseListQueueQuickConnects =
  res
    "ListQueueQuickConnectsResponse"
    "fixture/ListQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueueQuickConnects)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile =
  res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoutingProfile)

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin =
  res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateApprovedOrigin)

responseDisassociateQueueQuickConnects :: DisassociateQueueQuickConnectsResponse -> TestTree
responseDisassociateQueueQuickConnects =
  res
    "DisassociateQueueQuickConnectsResponse"
    "fixture/DisassociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateQueueQuickConnects)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency =
  res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileConcurrency)

responseUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfigResponse -> TestTree
responseUpdateQueueOutboundCallerConfig =
  res
    "UpdateQueueOutboundCallerConfigResponse"
    "fixture/UpdateQueueOutboundCallerConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueOutboundCallerConfig)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey =
  res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSecurityKey)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData =
  res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetCurrentMetricData)

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

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueues)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact =
  res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    defaultService
    (Proxy :: Proxy StopContact)

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

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey =
  res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSecurityKey)

responseAssociateQueueQuickConnects :: AssociateQueueQuickConnectsResponse -> TestTree
responseAssociateQueueQuickConnects =
  res
    "AssociateQueueQuickConnectsResponse"
    "fixture/AssociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateQueueQuickConnects)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording =
  res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy StopContactRecording)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin =
  res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateApprovedOrigin)

responseUpdateQuickConnectName :: UpdateQuickConnectNameResponse -> TestTree
responseUpdateQuickConnectName =
  res
    "UpdateQuickConnectNameResponse"
    "fixture/UpdateQuickConnectNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQuickConnectName)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile =
  res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRoutingProfile)

responseListQuickConnects :: ListQuickConnectsResponse -> TestTree
responseListQuickConnects =
  res
    "ListQuickConnectsResponse"
    "fixture/ListQuickConnectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQuickConnects)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot =
  res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateLexBot)

responseDeleteQuickConnect :: DeleteQuickConnectResponse -> TestTree
responseDeleteQuickConnect =
  res
    "DeleteQuickConnectResponse"
    "fixture/DeleteQuickConnectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQuickConnect)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues =
  res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoutingProfileQueues)

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

responseUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperationResponse -> TestTree
responseUpdateQueueHoursOfOperation =
  res
    "UpdateQueueHoursOfOperationResponse"
    "fixture/UpdateQueueHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueHoursOfOperation)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues =
  res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateRoutingProfileQueues)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow =
  res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContactFlow)

responseUpdateQueueStatus :: UpdateQueueStatusResponse -> TestTree
responseUpdateQueueStatus =
  res
    "UpdateQueueStatusResponse"
    "fixture/UpdateQueueStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueueStatus)

responseDescribeQueue :: DescribeQueueResponse -> TestTree
responseDescribeQueue =
  res
    "DescribeQueueResponse"
    "fixture/DescribeQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeQueue)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot =
  res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLexBot)

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

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup =
  res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserHierarchyGroup)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording =
  res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeContactRecording)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName =
  res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileName)

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

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumbers)

responseListIntegrationAssociations :: ListIntegrationAssociationsResponse -> TestTree
responseListIntegrationAssociations =
  res
    "ListIntegrationAssociationsResponse"
    "fixture/ListIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIntegrationAssociations)

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

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure =
  res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserHierarchyStructure)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations =
  res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHoursOfOperations)

responseCreateUseCase :: CreateUseCaseResponse -> TestTree
responseCreateUseCase =
  res
    "CreateUseCaseResponse"
    "fixture/CreateUseCaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUseCase)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows =
  res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContactFlows)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig =
  res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstanceStorageConfig)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs =
  res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceStorageConfigs)

responseCreateIntegrationAssociation :: CreateIntegrationAssociationResponse -> TestTree
responseCreateIntegrationAssociation =
  res
    "CreateIntegrationAssociationResponse"
    "fixture/CreateIntegrationAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIntegrationAssociation)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup =
  res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserHierarchyGroup)

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

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups =
  res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserHierarchyGroups)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo =
  res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserIdentityInfo)

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

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceAttribute)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys =
  res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecurityKeys)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue =
  res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQueue)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstance)

responseStartTaskContact :: StartTaskContactResponse -> TestTree
responseStartTaskContact =
  res
    "StartTaskContactResponse"
    "fixture/StartTaskContactResponse.proto"
    defaultService
    (Proxy :: Proxy StartTaskContact)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts =
  res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrompts)

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

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent =
  res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactFlowContent)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetMetricData)

responseDescribeHoursOfOperation :: DescribeHoursOfOperationResponse -> TestTree
responseDescribeHoursOfOperation =
  res
    "DescribeHoursOfOperationResponse"
    "fixture/DescribeHoursOfOperationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHoursOfOperation)

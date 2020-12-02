{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Connect
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDescribeInstance $
--             describeInstance
--
--         , requestListSecurityProfiles $
--             listSecurityProfiles
--
--         , requestAssociateLexBot $
--             associateLexBot
--
--         , requestUpdateInstanceAttribute $
--             updateInstanceAttribute
--
--         , requestUpdateRoutingProfileQueues $
--             updateRoutingProfileQueues
--
--         , requestListInstanceAttributes $
--             listInstanceAttributes
--
--         , requestDescribeInstanceStorageConfig $
--             describeInstanceStorageConfig
--
--         , requestDescribeContactFlow $
--             describeContactFlow
--
--         , requestUpdateUserHierarchy $
--             updateUserHierarchy
--
--         , requestUpdateUserRoutingProfile $
--             updateUserRoutingProfile
--
--         , requestUpdateUserHierarchyGroupName $
--             updateUserHierarchyGroupName
--
--         , requestDescribeRoutingProfile $
--             describeRoutingProfile
--
--         , requestDisassociateLexBot $
--             disassociateLexBot
--
--         , requestStartOutboundVoiceContact $
--             startOutboundVoiceContact
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetMetricData $
--             getMetricData
--
--         , requestStartContactRecording $
--             startContactRecording
--
--         , requestCreateInstance $
--             createInstance
--
--         , requestListUsers $
--             listUsers
--
--         , requestListUserHierarchyGroups $
--             listUserHierarchyGroups
--
--         , requestListQueues $
--             listQueues
--
--         , requestDescribeInstanceAttribute $
--             describeInstanceAttribute
--
--         , requestDeleteInstance $
--             deleteInstance
--
--         , requestDisassociateInstanceStorageConfig $
--             disassociateInstanceStorageConfig
--
--         , requestCreateRoutingProfile $
--             createRoutingProfile
--
--         , requestUpdateInstanceStorageConfig $
--             updateInstanceStorageConfig
--
--         , requestGetCurrentMetricData $
--             getCurrentMetricData
--
--         , requestCreateContactFlow $
--             createContactFlow
--
--         , requestListRoutingProfiles $
--             listRoutingProfiles
--
--         , requestUpdateUserPhoneConfig $
--             updateUserPhoneConfig
--
--         , requestListApprovedOrigins $
--             listApprovedOrigins
--
--         , requestDescribeUserHierarchyStructure $
--             describeUserHierarchyStructure
--
--         , requestListPhoneNumbers $
--             listPhoneNumbers
--
--         , requestUpdateContactAttributes $
--             updateContactAttributes
--
--         , requestStartChatContact $
--             startChatContact
--
--         , requestUpdateUserSecurityProfiles $
--             updateUserSecurityProfiles
--
--         , requestGetContactAttributes $
--             getContactAttributes
--
--         , requestListLambdaFunctions $
--             listLambdaFunctions
--
--         , requestDescribeUserHierarchyGroup $
--             describeUserHierarchyGroup
--
--         , requestDescribeUser $
--             describeUser
--
--         , requestResumeContactRecording $
--             resumeContactRecording
--
--         , requestUpdateContactFlowName $
--             updateContactFlowName
--
--         , requestSuspendContactRecording $
--             suspendContactRecording
--
--         , requestListRoutingProfileQueues $
--             listRoutingProfileQueues
--
--         , requestDisassociateRoutingProfileQueues $
--             disassociateRoutingProfileQueues
--
--         , requestDisassociateLambdaFunction $
--             disassociateLambdaFunction
--
--         , requestUpdateContactFlowContent $
--             updateContactFlowContent
--
--         , requestUpdateUserHierarchyStructure $
--             updateUserHierarchyStructure
--
--         , requestCreateUserHierarchyGroup $
--             createUserHierarchyGroup
--
--         , requestCreateUser $
--             createUser
--
--         , requestListPrompts $
--             listPrompts
--
--         , requestAssociateSecurityKey $
--             associateSecurityKey
--
--         , requestStopContactRecording $
--             stopContactRecording
--
--         , requestDisassociateApprovedOrigin $
--             disassociateApprovedOrigin
--
--         , requestListSecurityKeys $
--             listSecurityKeys
--
--         , requestGetFederationToken $
--             getFederationToken
--
--         , requestStopContact $
--             stopContact
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestTagResource $
--             tagResource
--
--         , requestUpdateUserIdentityInfo $
--             updateUserIdentityInfo
--
--         , requestListInstances $
--             listInstances
--
--         , requestDeleteUserHierarchyGroup $
--             deleteUserHierarchyGroup
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             updateRoutingProfileDefaultOutboundQueue
--
--         , requestListContactFlows $
--             listContactFlows
--
--         , requestUntagResource $
--             untagResource
--
--         , requestAssociateApprovedOrigin $
--             associateApprovedOrigin
--
--         , requestDisassociateSecurityKey $
--             disassociateSecurityKey
--
--         , requestUpdateRoutingProfileConcurrency $
--             updateRoutingProfileConcurrency
--
--         , requestListInstanceStorageConfigs $
--             listInstanceStorageConfigs
--
--         , requestAssociateInstanceStorageConfig $
--             associateInstanceStorageConfig
--
--         , requestListHoursOfOperations $
--             listHoursOfOperations
--
--         , requestUpdateRoutingProfileName $
--             updateRoutingProfileName
--
--         , requestListLexBots $
--             listLexBots
--
--         , requestAssociateLambdaFunction $
--             associateLambdaFunction
--
--         , requestAssociateRoutingProfileQueues $
--             associateRoutingProfileQueues
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInstance $
--             describeInstanceResponse
--
--         , responseListSecurityProfiles $
--             listSecurityProfilesResponse
--
--         , responseAssociateLexBot $
--             associateLexBotResponse
--
--         , responseUpdateInstanceAttribute $
--             updateInstanceAttributeResponse
--
--         , responseUpdateRoutingProfileQueues $
--             updateRoutingProfileQueuesResponse
--
--         , responseListInstanceAttributes $
--             listInstanceAttributesResponse
--
--         , responseDescribeInstanceStorageConfig $
--             describeInstanceStorageConfigResponse
--
--         , responseDescribeContactFlow $
--             describeContactFlowResponse
--
--         , responseUpdateUserHierarchy $
--             updateUserHierarchyResponse
--
--         , responseUpdateUserRoutingProfile $
--             updateUserRoutingProfileResponse
--
--         , responseUpdateUserHierarchyGroupName $
--             updateUserHierarchyGroupNameResponse
--
--         , responseDescribeRoutingProfile $
--             describeRoutingProfileResponse
--
--         , responseDisassociateLexBot $
--             disassociateLexBotResponse
--
--         , responseStartOutboundVoiceContact $
--             startOutboundVoiceContactResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetMetricData $
--             getMetricDataResponse
--
--         , responseStartContactRecording $
--             startContactRecordingResponse
--
--         , responseCreateInstance $
--             createInstanceResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseListUserHierarchyGroups $
--             listUserHierarchyGroupsResponse
--
--         , responseListQueues $
--             listQueuesResponse
--
--         , responseDescribeInstanceAttribute $
--             describeInstanceAttributeResponse
--
--         , responseDeleteInstance $
--             deleteInstanceResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             disassociateInstanceStorageConfigResponse
--
--         , responseCreateRoutingProfile $
--             createRoutingProfileResponse
--
--         , responseUpdateInstanceStorageConfig $
--             updateInstanceStorageConfigResponse
--
--         , responseGetCurrentMetricData $
--             getCurrentMetricDataResponse
--
--         , responseCreateContactFlow $
--             createContactFlowResponse
--
--         , responseListRoutingProfiles $
--             listRoutingProfilesResponse
--
--         , responseUpdateUserPhoneConfig $
--             updateUserPhoneConfigResponse
--
--         , responseListApprovedOrigins $
--             listApprovedOriginsResponse
--
--         , responseDescribeUserHierarchyStructure $
--             describeUserHierarchyStructureResponse
--
--         , responseListPhoneNumbers $
--             listPhoneNumbersResponse
--
--         , responseUpdateContactAttributes $
--             updateContactAttributesResponse
--
--         , responseStartChatContact $
--             startChatContactResponse
--
--         , responseUpdateUserSecurityProfiles $
--             updateUserSecurityProfilesResponse
--
--         , responseGetContactAttributes $
--             getContactAttributesResponse
--
--         , responseListLambdaFunctions $
--             listLambdaFunctionsResponse
--
--         , responseDescribeUserHierarchyGroup $
--             describeUserHierarchyGroupResponse
--
--         , responseDescribeUser $
--             describeUserResponse
--
--         , responseResumeContactRecording $
--             resumeContactRecordingResponse
--
--         , responseUpdateContactFlowName $
--             updateContactFlowNameResponse
--
--         , responseSuspendContactRecording $
--             suspendContactRecordingResponse
--
--         , responseListRoutingProfileQueues $
--             listRoutingProfileQueuesResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             disassociateRoutingProfileQueuesResponse
--
--         , responseDisassociateLambdaFunction $
--             disassociateLambdaFunctionResponse
--
--         , responseUpdateContactFlowContent $
--             updateContactFlowContentResponse
--
--         , responseUpdateUserHierarchyStructure $
--             updateUserHierarchyStructureResponse
--
--         , responseCreateUserHierarchyGroup $
--             createUserHierarchyGroupResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseListPrompts $
--             listPromptsResponse
--
--         , responseAssociateSecurityKey $
--             associateSecurityKeyResponse
--
--         , responseStopContactRecording $
--             stopContactRecordingResponse
--
--         , responseDisassociateApprovedOrigin $
--             disassociateApprovedOriginResponse
--
--         , responseListSecurityKeys $
--             listSecurityKeysResponse
--
--         , responseGetFederationToken $
--             getFederationTokenResponse
--
--         , responseStopContact $
--             stopContactResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUpdateUserIdentityInfo $
--             updateUserIdentityInfoResponse
--
--         , responseListInstances $
--             listInstancesResponse
--
--         , responseDeleteUserHierarchyGroup $
--             deleteUserHierarchyGroupResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             updateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseListContactFlows $
--             listContactFlowsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseAssociateApprovedOrigin $
--             associateApprovedOriginResponse
--
--         , responseDisassociateSecurityKey $
--             disassociateSecurityKeyResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             updateRoutingProfileConcurrencyResponse
--
--         , responseListInstanceStorageConfigs $
--             listInstanceStorageConfigsResponse
--
--         , responseAssociateInstanceStorageConfig $
--             associateInstanceStorageConfigResponse
--
--         , responseListHoursOfOperations $
--             listHoursOfOperationsResponse
--
--         , responseUpdateRoutingProfileName $
--             updateRoutingProfileNameResponse
--
--         , responseListLexBots $
--             listLexBotsResponse
--
--         , responseAssociateLambdaFunction $
--             associateLambdaFunctionResponse
--
--         , responseAssociateRoutingProfileQueues $
--             associateRoutingProfileQueuesResponse
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

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues =
  req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes =
  req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestDescribeInstanceStorageConfig :: DescribeInstanceStorageConfig -> TestTree
requestDescribeInstanceStorageConfig =
  req
    "DescribeInstanceStorageConfig"
    "fixture/DescribeInstanceStorageConfig.yaml"

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

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig =
  req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

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

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact =
  req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

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

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows =
  req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

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
    connect
    (Proxy :: Proxy DescribeInstance)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    connect
    (Proxy :: Proxy ListSecurityProfiles)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot =
  res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    connect
    (Proxy :: Proxy AssociateLexBot)

responseUpdateInstanceAttribute :: UpdateInstanceAttributeResponse -> TestTree
responseUpdateInstanceAttribute =
  res
    "UpdateInstanceAttributeResponse"
    "fixture/UpdateInstanceAttributeResponse.proto"
    connect
    (Proxy :: Proxy UpdateInstanceAttribute)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues =
  res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    connect
    (Proxy :: Proxy UpdateRoutingProfileQueues)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes =
  res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    connect
    (Proxy :: Proxy ListInstanceAttributes)

responseDescribeInstanceStorageConfig :: DescribeInstanceStorageConfigResponse -> TestTree
responseDescribeInstanceStorageConfig =
  res
    "DescribeInstanceStorageConfigResponse"
    "fixture/DescribeInstanceStorageConfigResponse.proto"
    connect
    (Proxy :: Proxy DescribeInstanceStorageConfig)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow =
  res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    connect
    (Proxy :: Proxy DescribeContactFlow)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy =
  res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserHierarchy)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile =
  res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserRoutingProfile)

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName =
  res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserHierarchyGroupName)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile =
  res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    connect
    (Proxy :: Proxy DescribeRoutingProfile)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot =
  res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    connect
    (Proxy :: Proxy DisassociateLexBot)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact =
  res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    connect
    (Proxy :: Proxy StartOutboundVoiceContact)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    connect
    (Proxy :: Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    connect
    (Proxy :: Proxy GetMetricData)

responseStartContactRecording :: StartContactRecordingResponse -> TestTree
responseStartContactRecording =
  res
    "StartContactRecordingResponse"
    "fixture/StartContactRecordingResponse.proto"
    connect
    (Proxy :: Proxy StartContactRecording)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    connect
    (Proxy :: Proxy CreateInstance)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    connect
    (Proxy :: Proxy ListUsers)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups =
  res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    connect
    (Proxy :: Proxy ListUserHierarchyGroups)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    connect
    (Proxy :: Proxy ListQueues)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    connect
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    connect
    (Proxy :: Proxy DeleteInstance)

responseDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfigResponse -> TestTree
responseDisassociateInstanceStorageConfig =
  res
    "DisassociateInstanceStorageConfigResponse"
    "fixture/DisassociateInstanceStorageConfigResponse.proto"
    connect
    (Proxy :: Proxy DisassociateInstanceStorageConfig)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile =
  res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    connect
    (Proxy :: Proxy CreateRoutingProfile)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig =
  res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    connect
    (Proxy :: Proxy UpdateInstanceStorageConfig)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData =
  res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    connect
    (Proxy :: Proxy GetCurrentMetricData)

responseCreateContactFlow :: CreateContactFlowResponse -> TestTree
responseCreateContactFlow =
  res
    "CreateContactFlowResponse"
    "fixture/CreateContactFlowResponse.proto"
    connect
    (Proxy :: Proxy CreateContactFlow)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles =
  res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    connect
    (Proxy :: Proxy ListRoutingProfiles)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig =
  res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserPhoneConfig)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins =
  res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    connect
    (Proxy :: Proxy ListApprovedOrigins)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure =
  res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    connect
    (Proxy :: Proxy DescribeUserHierarchyStructure)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    connect
    (Proxy :: Proxy ListPhoneNumbers)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes =
  res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    connect
    (Proxy :: Proxy UpdateContactAttributes)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact =
  res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    connect
    (Proxy :: Proxy StartChatContact)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles =
  res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserSecurityProfiles)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes =
  res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    connect
    (Proxy :: Proxy GetContactAttributes)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions =
  res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    connect
    (Proxy :: Proxy ListLambdaFunctions)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup =
  res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    connect
    (Proxy :: Proxy DescribeUserHierarchyGroup)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    connect
    (Proxy :: Proxy DescribeUser)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording =
  res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    connect
    (Proxy :: Proxy ResumeContactRecording)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName =
  res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    connect
    (Proxy :: Proxy UpdateContactFlowName)

responseSuspendContactRecording :: SuspendContactRecordingResponse -> TestTree
responseSuspendContactRecording =
  res
    "SuspendContactRecordingResponse"
    "fixture/SuspendContactRecordingResponse.proto"
    connect
    (Proxy :: Proxy SuspendContactRecording)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues =
  res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    connect
    (Proxy :: Proxy ListRoutingProfileQueues)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues =
  res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    connect
    (Proxy :: Proxy DisassociateRoutingProfileQueues)

responseDisassociateLambdaFunction :: DisassociateLambdaFunctionResponse -> TestTree
responseDisassociateLambdaFunction =
  res
    "DisassociateLambdaFunctionResponse"
    "fixture/DisassociateLambdaFunctionResponse.proto"
    connect
    (Proxy :: Proxy DisassociateLambdaFunction)

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent =
  res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    connect
    (Proxy :: Proxy UpdateContactFlowContent)

responseUpdateUserHierarchyStructure :: UpdateUserHierarchyStructureResponse -> TestTree
responseUpdateUserHierarchyStructure =
  res
    "UpdateUserHierarchyStructureResponse"
    "fixture/UpdateUserHierarchyStructureResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserHierarchyStructure)

responseCreateUserHierarchyGroup :: CreateUserHierarchyGroupResponse -> TestTree
responseCreateUserHierarchyGroup =
  res
    "CreateUserHierarchyGroupResponse"
    "fixture/CreateUserHierarchyGroupResponse.proto"
    connect
    (Proxy :: Proxy CreateUserHierarchyGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    connect
    (Proxy :: Proxy CreateUser)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts =
  res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    connect
    (Proxy :: Proxy ListPrompts)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey =
  res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    connect
    (Proxy :: Proxy AssociateSecurityKey)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording =
  res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    connect
    (Proxy :: Proxy StopContactRecording)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin =
  res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    connect
    (Proxy :: Proxy DisassociateApprovedOrigin)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys =
  res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    connect
    (Proxy :: Proxy ListSecurityKeys)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    connect
    (Proxy :: Proxy GetFederationToken)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact =
  res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    connect
    (Proxy :: Proxy StopContact)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    connect
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    connect
    (Proxy :: Proxy TagResource)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo =
  res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserIdentityInfo)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    connect
    (Proxy :: Proxy ListInstances)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup =
  res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    connect
    (Proxy :: Proxy DeleteUserHierarchyGroup)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue =
  res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    connect
    (Proxy :: Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows =
  res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    connect
    (Proxy :: Proxy ListContactFlows)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    connect
    (Proxy :: Proxy UntagResource)

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin =
  res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    connect
    (Proxy :: Proxy AssociateApprovedOrigin)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey =
  res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    connect
    (Proxy :: Proxy DisassociateSecurityKey)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency =
  res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    connect
    (Proxy :: Proxy UpdateRoutingProfileConcurrency)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs =
  res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    connect
    (Proxy :: Proxy ListInstanceStorageConfigs)

responseAssociateInstanceStorageConfig :: AssociateInstanceStorageConfigResponse -> TestTree
responseAssociateInstanceStorageConfig =
  res
    "AssociateInstanceStorageConfigResponse"
    "fixture/AssociateInstanceStorageConfigResponse.proto"
    connect
    (Proxy :: Proxy AssociateInstanceStorageConfig)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations =
  res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    connect
    (Proxy :: Proxy ListHoursOfOperations)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName =
  res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    connect
    (Proxy :: Proxy UpdateRoutingProfileName)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots =
  res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    connect
    (Proxy :: Proxy ListLexBots)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction =
  res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    connect
    (Proxy :: Proxy AssociateLambdaFunction)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues =
  res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    connect
    (Proxy :: Proxy AssociateRoutingProfileQueues)

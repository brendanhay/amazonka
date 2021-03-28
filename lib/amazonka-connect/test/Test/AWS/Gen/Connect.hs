{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Connect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Connect where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Connect
import Test.AWS.Connect.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeInstance $
--             mkDescribeInstance
--
--         , requestListSecurityProfiles $
--             mkListSecurityProfiles
--
--         , requestAssociateLexBot $
--             mkAssociateLexBot
--
--         , requestUpdateInstanceAttribute $
--             mkUpdateInstanceAttribute
--
--         , requestUpdateRoutingProfileQueues $
--             mkUpdateRoutingProfileQueues
--
--         , requestListInstanceAttributes $
--             mkListInstanceAttributes
--
--         , requestDescribeInstanceStorageConfig $
--             mkDescribeInstanceStorageConfig
--
--         , requestDescribeContactFlow $
--             mkDescribeContactFlow
--
--         , requestUpdateUserHierarchy $
--             mkUpdateUserHierarchy
--
--         , requestUpdateUserRoutingProfile $
--             mkUpdateUserRoutingProfile
--
--         , requestUpdateUserHierarchyGroupName $
--             mkUpdateUserHierarchyGroupName
--
--         , requestDescribeRoutingProfile $
--             mkDescribeRoutingProfile
--
--         , requestDisassociateLexBot $
--             mkDisassociateLexBot
--
--         , requestStartOutboundVoiceContact $
--             mkStartOutboundVoiceContact
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetMetricData $
--             mkGetMetricData
--
--         , requestStartContactRecording $
--             mkStartContactRecording
--
--         , requestCreateInstance $
--             mkCreateInstance
--
--         , requestListUsers $
--             mkListUsers
--
--         , requestListUserHierarchyGroups $
--             mkListUserHierarchyGroups
--
--         , requestListQueues $
--             mkListQueues
--
--         , requestDescribeInstanceAttribute $
--             mkDescribeInstanceAttribute
--
--         , requestDeleteInstance $
--             mkDeleteInstance
--
--         , requestDisassociateInstanceStorageConfig $
--             mkDisassociateInstanceStorageConfig
--
--         , requestCreateRoutingProfile $
--             mkCreateRoutingProfile
--
--         , requestUpdateInstanceStorageConfig $
--             mkUpdateInstanceStorageConfig
--
--         , requestGetCurrentMetricData $
--             mkGetCurrentMetricData
--
--         , requestCreateContactFlow $
--             mkCreateContactFlow
--
--         , requestListRoutingProfiles $
--             mkListRoutingProfiles
--
--         , requestUpdateUserPhoneConfig $
--             mkUpdateUserPhoneConfig
--
--         , requestListApprovedOrigins $
--             mkListApprovedOrigins
--
--         , requestDescribeUserHierarchyStructure $
--             mkDescribeUserHierarchyStructure
--
--         , requestListPhoneNumbers $
--             mkListPhoneNumbers
--
--         , requestUpdateContactAttributes $
--             mkUpdateContactAttributes
--
--         , requestStartChatContact $
--             mkStartChatContact
--
--         , requestUpdateUserSecurityProfiles $
--             mkUpdateUserSecurityProfiles
--
--         , requestGetContactAttributes $
--             mkGetContactAttributes
--
--         , requestListLambdaFunctions $
--             mkListLambdaFunctions
--
--         , requestDescribeUserHierarchyGroup $
--             mkDescribeUserHierarchyGroup
--
--         , requestDescribeUser $
--             mkDescribeUser
--
--         , requestResumeContactRecording $
--             mkResumeContactRecording
--
--         , requestUpdateContactFlowName $
--             mkUpdateContactFlowName
--
--         , requestSuspendContactRecording $
--             mkSuspendContactRecording
--
--         , requestListRoutingProfileQueues $
--             mkListRoutingProfileQueues
--
--         , requestDisassociateRoutingProfileQueues $
--             mkDisassociateRoutingProfileQueues
--
--         , requestDisassociateLambdaFunction $
--             mkDisassociateLambdaFunction
--
--         , requestUpdateContactFlowContent $
--             mkUpdateContactFlowContent
--
--         , requestUpdateUserHierarchyStructure $
--             mkUpdateUserHierarchyStructure
--
--         , requestCreateUserHierarchyGroup $
--             mkCreateUserHierarchyGroup
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestListPrompts $
--             mkListPrompts
--
--         , requestAssociateSecurityKey $
--             mkAssociateSecurityKey
--
--         , requestStopContactRecording $
--             mkStopContactRecording
--
--         , requestDisassociateApprovedOrigin $
--             mkDisassociateApprovedOrigin
--
--         , requestListSecurityKeys $
--             mkListSecurityKeys
--
--         , requestGetFederationToken $
--             mkGetFederationToken
--
--         , requestStopContact $
--             mkStopContact
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUpdateUserIdentityInfo $
--             mkUpdateUserIdentityInfo
--
--         , requestListInstances $
--             mkListInstances
--
--         , requestDeleteUserHierarchyGroup $
--             mkDeleteUserHierarchyGroup
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             mkUpdateRoutingProfileDefaultOutboundQueue
--
--         , requestListContactFlows $
--             mkListContactFlows
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestAssociateApprovedOrigin $
--             mkAssociateApprovedOrigin
--
--         , requestDisassociateSecurityKey $
--             mkDisassociateSecurityKey
--
--         , requestUpdateRoutingProfileConcurrency $
--             mkUpdateRoutingProfileConcurrency
--
--         , requestListInstanceStorageConfigs $
--             mkListInstanceStorageConfigs
--
--         , requestAssociateInstanceStorageConfig $
--             mkAssociateInstanceStorageConfig
--
--         , requestListHoursOfOperations $
--             mkListHoursOfOperations
--
--         , requestUpdateRoutingProfileName $
--             mkUpdateRoutingProfileName
--
--         , requestListLexBots $
--             mkListLexBots
--
--         , requestAssociateLambdaFunction $
--             mkAssociateLambdaFunction
--
--         , requestAssociateRoutingProfileQueues $
--             mkAssociateRoutingProfileQueues
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInstance $
--             mkDescribeInstanceResponse
--
--         , responseListSecurityProfiles $
--             mkListSecurityProfilesResponse
--
--         , responseAssociateLexBot $
--             mkAssociateLexBotResponse
--
--         , responseUpdateInstanceAttribute $
--             mkUpdateInstanceAttributeResponse
--
--         , responseUpdateRoutingProfileQueues $
--             mkUpdateRoutingProfileQueuesResponse
--
--         , responseListInstanceAttributes $
--             mkListInstanceAttributesResponse
--
--         , responseDescribeInstanceStorageConfig $
--             mkDescribeInstanceStorageConfigResponse
--
--         , responseDescribeContactFlow $
--             mkDescribeContactFlowResponse
--
--         , responseUpdateUserHierarchy $
--             mkUpdateUserHierarchyResponse
--
--         , responseUpdateUserRoutingProfile $
--             mkUpdateUserRoutingProfileResponse
--
--         , responseUpdateUserHierarchyGroupName $
--             mkUpdateUserHierarchyGroupNameResponse
--
--         , responseDescribeRoutingProfile $
--             mkDescribeRoutingProfileResponse
--
--         , responseDisassociateLexBot $
--             mkDisassociateLexBotResponse
--
--         , responseStartOutboundVoiceContact $
--             mkStartOutboundVoiceContactResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetMetricData $
--             mkGetMetricDataResponse
--
--         , responseStartContactRecording $
--             mkStartContactRecordingResponse
--
--         , responseCreateInstance $
--             mkCreateInstanceResponse
--
--         , responseListUsers $
--             mkListUsersResponse
--
--         , responseListUserHierarchyGroups $
--             mkListUserHierarchyGroupsResponse
--
--         , responseListQueues $
--             mkListQueuesResponse
--
--         , responseDescribeInstanceAttribute $
--             mkDescribeInstanceAttributeResponse
--
--         , responseDeleteInstance $
--             mkDeleteInstanceResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             mkDisassociateInstanceStorageConfigResponse
--
--         , responseCreateRoutingProfile $
--             mkCreateRoutingProfileResponse
--
--         , responseUpdateInstanceStorageConfig $
--             mkUpdateInstanceStorageConfigResponse
--
--         , responseGetCurrentMetricData $
--             mkGetCurrentMetricDataResponse
--
--         , responseCreateContactFlow $
--             mkCreateContactFlowResponse
--
--         , responseListRoutingProfiles $
--             mkListRoutingProfilesResponse
--
--         , responseUpdateUserPhoneConfig $
--             mkUpdateUserPhoneConfigResponse
--
--         , responseListApprovedOrigins $
--             mkListApprovedOriginsResponse
--
--         , responseDescribeUserHierarchyStructure $
--             mkDescribeUserHierarchyStructureResponse
--
--         , responseListPhoneNumbers $
--             mkListPhoneNumbersResponse
--
--         , responseUpdateContactAttributes $
--             mkUpdateContactAttributesResponse
--
--         , responseStartChatContact $
--             mkStartChatContactResponse
--
--         , responseUpdateUserSecurityProfiles $
--             mkUpdateUserSecurityProfilesResponse
--
--         , responseGetContactAttributes $
--             mkGetContactAttributesResponse
--
--         , responseListLambdaFunctions $
--             mkListLambdaFunctionsResponse
--
--         , responseDescribeUserHierarchyGroup $
--             mkDescribeUserHierarchyGroupResponse
--
--         , responseDescribeUser $
--             mkDescribeUserResponse
--
--         , responseResumeContactRecording $
--             mkResumeContactRecordingResponse
--
--         , responseUpdateContactFlowName $
--             mkUpdateContactFlowNameResponse
--
--         , responseSuspendContactRecording $
--             mkSuspendContactRecordingResponse
--
--         , responseListRoutingProfileQueues $
--             mkListRoutingProfileQueuesResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             mkDisassociateRoutingProfileQueuesResponse
--
--         , responseDisassociateLambdaFunction $
--             mkDisassociateLambdaFunctionResponse
--
--         , responseUpdateContactFlowContent $
--             mkUpdateContactFlowContentResponse
--
--         , responseUpdateUserHierarchyStructure $
--             mkUpdateUserHierarchyStructureResponse
--
--         , responseCreateUserHierarchyGroup $
--             mkCreateUserHierarchyGroupResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responseListPrompts $
--             mkListPromptsResponse
--
--         , responseAssociateSecurityKey $
--             mkAssociateSecurityKeyResponse
--
--         , responseStopContactRecording $
--             mkStopContactRecordingResponse
--
--         , responseDisassociateApprovedOrigin $
--             mkDisassociateApprovedOriginResponse
--
--         , responseListSecurityKeys $
--             mkListSecurityKeysResponse
--
--         , responseGetFederationToken $
--             mkGetFederationTokenResponse
--
--         , responseStopContact $
--             mkStopContactResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUpdateUserIdentityInfo $
--             mkUpdateUserIdentityInfoResponse
--
--         , responseListInstances $
--             mkListInstancesResponse
--
--         , responseDeleteUserHierarchyGroup $
--             mkDeleteUserHierarchyGroupResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             mkUpdateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseListContactFlows $
--             mkListContactFlowsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseAssociateApprovedOrigin $
--             mkAssociateApprovedOriginResponse
--
--         , responseDisassociateSecurityKey $
--             mkDisassociateSecurityKeyResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             mkUpdateRoutingProfileConcurrencyResponse
--
--         , responseListInstanceStorageConfigs $
--             mkListInstanceStorageConfigsResponse
--
--         , responseAssociateInstanceStorageConfig $
--             mkAssociateInstanceStorageConfigResponse
--
--         , responseListHoursOfOperations $
--             mkListHoursOfOperationsResponse
--
--         , responseUpdateRoutingProfileName $
--             mkUpdateRoutingProfileNameResponse
--
--         , responseListLexBots $
--             mkListLexBotsResponse
--
--         , responseAssociateLambdaFunction $
--             mkAssociateLambdaFunctionResponse
--
--         , responseAssociateRoutingProfileQueues $
--             mkAssociateRoutingProfileQueuesResponse
--
--           ]
--     ]

-- Requests

requestDescribeInstance :: DescribeInstance -> TestTree
requestDescribeInstance = req
    "DescribeInstance"
    "fixture/DescribeInstance.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles = req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestAssociateLexBot :: AssociateLexBot -> TestTree
requestAssociateLexBot = req
    "AssociateLexBot"
    "fixture/AssociateLexBot.yaml"

requestUpdateInstanceAttribute :: UpdateInstanceAttribute -> TestTree
requestUpdateInstanceAttribute = req
    "UpdateInstanceAttribute"
    "fixture/UpdateInstanceAttribute.yaml"

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues = req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes = req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestDescribeInstanceStorageConfig :: DescribeInstanceStorageConfig -> TestTree
requestDescribeInstanceStorageConfig = req
    "DescribeInstanceStorageConfig"
    "fixture/DescribeInstanceStorageConfig.yaml"

requestDescribeContactFlow :: DescribeContactFlow -> TestTree
requestDescribeContactFlow = req
    "DescribeContactFlow"
    "fixture/DescribeContactFlow.yaml"

requestUpdateUserHierarchy :: UpdateUserHierarchy -> TestTree
requestUpdateUserHierarchy = req
    "UpdateUserHierarchy"
    "fixture/UpdateUserHierarchy.yaml"

requestUpdateUserRoutingProfile :: UpdateUserRoutingProfile -> TestTree
requestUpdateUserRoutingProfile = req
    "UpdateUserRoutingProfile"
    "fixture/UpdateUserRoutingProfile.yaml"

requestUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupName -> TestTree
requestUpdateUserHierarchyGroupName = req
    "UpdateUserHierarchyGroupName"
    "fixture/UpdateUserHierarchyGroupName.yaml"

requestDescribeRoutingProfile :: DescribeRoutingProfile -> TestTree
requestDescribeRoutingProfile = req
    "DescribeRoutingProfile"
    "fixture/DescribeRoutingProfile.yaml"

requestDisassociateLexBot :: DisassociateLexBot -> TestTree
requestDisassociateLexBot = req
    "DisassociateLexBot"
    "fixture/DisassociateLexBot.yaml"

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact = req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData = req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestStartContactRecording :: StartContactRecording -> TestTree
requestStartContactRecording = req
    "StartContactRecording"
    "fixture/StartContactRecording.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance = req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups = req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues = req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute = req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfig -> TestTree
requestDisassociateInstanceStorageConfig = req
    "DisassociateInstanceStorageConfig"
    "fixture/DisassociateInstanceStorageConfig.yaml"

requestCreateRoutingProfile :: CreateRoutingProfile -> TestTree
requestCreateRoutingProfile = req
    "CreateRoutingProfile"
    "fixture/CreateRoutingProfile.yaml"

requestUpdateInstanceStorageConfig :: UpdateInstanceStorageConfig -> TestTree
requestUpdateInstanceStorageConfig = req
    "UpdateInstanceStorageConfig"
    "fixture/UpdateInstanceStorageConfig.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData = req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

requestCreateContactFlow :: CreateContactFlow -> TestTree
requestCreateContactFlow = req
    "CreateContactFlow"
    "fixture/CreateContactFlow.yaml"

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles = req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig = req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

requestListApprovedOrigins :: ListApprovedOrigins -> TestTree
requestListApprovedOrigins = req
    "ListApprovedOrigins"
    "fixture/ListApprovedOrigins.yaml"

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure = req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers = req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestUpdateContactAttributes :: UpdateContactAttributes -> TestTree
requestUpdateContactAttributes = req
    "UpdateContactAttributes"
    "fixture/UpdateContactAttributes.yaml"

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact = req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

requestUpdateUserSecurityProfiles :: UpdateUserSecurityProfiles -> TestTree
requestUpdateUserSecurityProfiles = req
    "UpdateUserSecurityProfiles"
    "fixture/UpdateUserSecurityProfiles.yaml"

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes = req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestListLambdaFunctions :: ListLambdaFunctions -> TestTree
requestListLambdaFunctions = req
    "ListLambdaFunctions"
    "fixture/ListLambdaFunctions.yaml"

requestDescribeUserHierarchyGroup :: DescribeUserHierarchyGroup -> TestTree
requestDescribeUserHierarchyGroup = req
    "DescribeUserHierarchyGroup"
    "fixture/DescribeUserHierarchyGroup.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser = req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestResumeContactRecording :: ResumeContactRecording -> TestTree
requestResumeContactRecording = req
    "ResumeContactRecording"
    "fixture/ResumeContactRecording.yaml"

requestUpdateContactFlowName :: UpdateContactFlowName -> TestTree
requestUpdateContactFlowName = req
    "UpdateContactFlowName"
    "fixture/UpdateContactFlowName.yaml"

requestSuspendContactRecording :: SuspendContactRecording -> TestTree
requestSuspendContactRecording = req
    "SuspendContactRecording"
    "fixture/SuspendContactRecording.yaml"

requestListRoutingProfileQueues :: ListRoutingProfileQueues -> TestTree
requestListRoutingProfileQueues = req
    "ListRoutingProfileQueues"
    "fixture/ListRoutingProfileQueues.yaml"

requestDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueues -> TestTree
requestDisassociateRoutingProfileQueues = req
    "DisassociateRoutingProfileQueues"
    "fixture/DisassociateRoutingProfileQueues.yaml"

requestDisassociateLambdaFunction :: DisassociateLambdaFunction -> TestTree
requestDisassociateLambdaFunction = req
    "DisassociateLambdaFunction"
    "fixture/DisassociateLambdaFunction.yaml"

requestUpdateContactFlowContent :: UpdateContactFlowContent -> TestTree
requestUpdateContactFlowContent = req
    "UpdateContactFlowContent"
    "fixture/UpdateContactFlowContent.yaml"

requestUpdateUserHierarchyStructure :: UpdateUserHierarchyStructure -> TestTree
requestUpdateUserHierarchyStructure = req
    "UpdateUserHierarchyStructure"
    "fixture/UpdateUserHierarchyStructure.yaml"

requestCreateUserHierarchyGroup :: CreateUserHierarchyGroup -> TestTree
requestCreateUserHierarchyGroup = req
    "CreateUserHierarchyGroup"
    "fixture/CreateUserHierarchyGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestListPrompts :: ListPrompts -> TestTree
requestListPrompts = req
    "ListPrompts"
    "fixture/ListPrompts.yaml"

requestAssociateSecurityKey :: AssociateSecurityKey -> TestTree
requestAssociateSecurityKey = req
    "AssociateSecurityKey"
    "fixture/AssociateSecurityKey.yaml"

requestStopContactRecording :: StopContactRecording -> TestTree
requestStopContactRecording = req
    "StopContactRecording"
    "fixture/StopContactRecording.yaml"

requestDisassociateApprovedOrigin :: DisassociateApprovedOrigin -> TestTree
requestDisassociateApprovedOrigin = req
    "DisassociateApprovedOrigin"
    "fixture/DisassociateApprovedOrigin.yaml"

requestListSecurityKeys :: ListSecurityKeys -> TestTree
requestListSecurityKeys = req
    "ListSecurityKeys"
    "fixture/ListSecurityKeys.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken = req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact = req
    "StopContact"
    "fixture/StopContact.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo = req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances = req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestDeleteUserHierarchyGroup :: DeleteUserHierarchyGroup -> TestTree
requestDeleteUserHierarchyGroup = req
    "DeleteUserHierarchyGroup"
    "fixture/DeleteUserHierarchyGroup.yaml"

requestUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueue -> TestTree
requestUpdateRoutingProfileDefaultOutboundQueue = req
    "UpdateRoutingProfileDefaultOutboundQueue"
    "fixture/UpdateRoutingProfileDefaultOutboundQueue.yaml"

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows = req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAssociateApprovedOrigin :: AssociateApprovedOrigin -> TestTree
requestAssociateApprovedOrigin = req
    "AssociateApprovedOrigin"
    "fixture/AssociateApprovedOrigin.yaml"

requestDisassociateSecurityKey :: DisassociateSecurityKey -> TestTree
requestDisassociateSecurityKey = req
    "DisassociateSecurityKey"
    "fixture/DisassociateSecurityKey.yaml"

requestUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrency -> TestTree
requestUpdateRoutingProfileConcurrency = req
    "UpdateRoutingProfileConcurrency"
    "fixture/UpdateRoutingProfileConcurrency.yaml"

requestListInstanceStorageConfigs :: ListInstanceStorageConfigs -> TestTree
requestListInstanceStorageConfigs = req
    "ListInstanceStorageConfigs"
    "fixture/ListInstanceStorageConfigs.yaml"

requestAssociateInstanceStorageConfig :: AssociateInstanceStorageConfig -> TestTree
requestAssociateInstanceStorageConfig = req
    "AssociateInstanceStorageConfig"
    "fixture/AssociateInstanceStorageConfig.yaml"

requestListHoursOfOperations :: ListHoursOfOperations -> TestTree
requestListHoursOfOperations = req
    "ListHoursOfOperations"
    "fixture/ListHoursOfOperations.yaml"

requestUpdateRoutingProfileName :: UpdateRoutingProfileName -> TestTree
requestUpdateRoutingProfileName = req
    "UpdateRoutingProfileName"
    "fixture/UpdateRoutingProfileName.yaml"

requestListLexBots :: ListLexBots -> TestTree
requestListLexBots = req
    "ListLexBots"
    "fixture/ListLexBots.yaml"

requestAssociateLambdaFunction :: AssociateLambdaFunction -> TestTree
requestAssociateLambdaFunction = req
    "AssociateLambdaFunction"
    "fixture/AssociateLambdaFunction.yaml"

requestAssociateRoutingProfileQueues :: AssociateRoutingProfileQueues -> TestTree
requestAssociateRoutingProfileQueues = req
    "AssociateRoutingProfileQueues"
    "fixture/AssociateRoutingProfileQueues.yaml"

-- Responses

responseDescribeInstance :: DescribeInstanceResponse -> TestTree
responseDescribeInstance = res
    "DescribeInstanceResponse"
    "fixture/DescribeInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstance)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles = res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecurityProfiles)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot = res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateLexBot)

responseUpdateInstanceAttribute :: UpdateInstanceAttributeResponse -> TestTree
responseUpdateInstanceAttribute = res
    "UpdateInstanceAttributeResponse"
    "fixture/UpdateInstanceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateInstanceAttribute)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues = res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoutingProfileQueues)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes = res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceAttributes)

responseDescribeInstanceStorageConfig :: DescribeInstanceStorageConfigResponse -> TestTree
responseDescribeInstanceStorageConfig = res
    "DescribeInstanceStorageConfigResponse"
    "fixture/DescribeInstanceStorageConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceStorageConfig)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow = res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeContactFlow)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy = res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserHierarchy)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile = res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserRoutingProfile)

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName = res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserHierarchyGroupName)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile = res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRoutingProfile)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot = res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateLexBot)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact = res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartOutboundVoiceContact)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData = res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMetricData)

responseStartContactRecording :: StartContactRecordingResponse -> TestTree
responseStartContactRecording = res
    "StartContactRecordingResponse"
    "fixture/StartContactRecordingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartContactRecording)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance = res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstance)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUsers)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups = res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListUserHierarchyGroups)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListQueues)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute = res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstanceAttribute)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInstance)

responseDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfigResponse -> TestTree
responseDisassociateInstanceStorageConfig = res
    "DisassociateInstanceStorageConfigResponse"
    "fixture/DisassociateInstanceStorageConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateInstanceStorageConfig)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile = res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRoutingProfile)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig = res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateInstanceStorageConfig)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData = res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCurrentMetricData)

responseCreateContactFlow :: CreateContactFlowResponse -> TestTree
responseCreateContactFlow = res
    "CreateContactFlowResponse"
    "fixture/CreateContactFlowResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContactFlow)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles = res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoutingProfiles)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig = res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserPhoneConfig)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins = res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApprovedOrigins)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure = res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUserHierarchyStructure)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers = res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPhoneNumbers)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes = res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContactAttributes)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact = res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartChatContact)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles = res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserSecurityProfiles)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes = res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContactAttributes)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions = res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLambdaFunctions)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup = res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUserHierarchyGroup)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser = res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUser)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording = res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResumeContactRecording)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName = res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContactFlowName)

responseSuspendContactRecording :: SuspendContactRecordingResponse -> TestTree
responseSuspendContactRecording = res
    "SuspendContactRecordingResponse"
    "fixture/SuspendContactRecordingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SuspendContactRecording)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues = res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoutingProfileQueues)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues = res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateRoutingProfileQueues)

responseDisassociateLambdaFunction :: DisassociateLambdaFunctionResponse -> TestTree
responseDisassociateLambdaFunction = res
    "DisassociateLambdaFunctionResponse"
    "fixture/DisassociateLambdaFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateLambdaFunction)

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent = res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContactFlowContent)

responseUpdateUserHierarchyStructure :: UpdateUserHierarchyStructureResponse -> TestTree
responseUpdateUserHierarchyStructure = res
    "UpdateUserHierarchyStructureResponse"
    "fixture/UpdateUserHierarchyStructureResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserHierarchyStructure)

responseCreateUserHierarchyGroup :: CreateUserHierarchyGroupResponse -> TestTree
responseCreateUserHierarchyGroup = res
    "CreateUserHierarchyGroupResponse"
    "fixture/CreateUserHierarchyGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUserHierarchyGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUser)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts = res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPrompts)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey = res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateSecurityKey)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording = res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopContactRecording)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin = res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateApprovedOrigin)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys = res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecurityKeys)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken = res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFederationToken)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact = res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopContact)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo = res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserIdentityInfo)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstances)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup = res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserHierarchyGroup)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue = res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows = res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListContactFlows)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin = res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateApprovedOrigin)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey = res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateSecurityKey)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency = res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoutingProfileConcurrency)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs = res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListInstanceStorageConfigs)

responseAssociateInstanceStorageConfig :: AssociateInstanceStorageConfigResponse -> TestTree
responseAssociateInstanceStorageConfig = res
    "AssociateInstanceStorageConfigResponse"
    "fixture/AssociateInstanceStorageConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateInstanceStorageConfig)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations = res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHoursOfOperations)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName = res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRoutingProfileName)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots = res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLexBots)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction = res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateLambdaFunction)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues = res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateRoutingProfileQueues)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Connect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Connect where

import Amazonka.Connect
import qualified Data.Proxy as Proxy
import Test.Amazonka.Connect.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateApprovedOrigin $
--             newAssociateApprovedOrigin
--
--         , requestAssociateBot $
--             newAssociateBot
--
--         , requestAssociateDefaultVocabulary $
--             newAssociateDefaultVocabulary
--
--         , requestAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfig
--
--         , requestAssociateLambdaFunction $
--             newAssociateLambdaFunction
--
--         , requestAssociateLexBot $
--             newAssociateLexBot
--
--         , requestAssociatePhoneNumberContactFlow $
--             newAssociatePhoneNumberContactFlow
--
--         , requestAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnects
--
--         , requestAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueues
--
--         , requestAssociateSecurityKey $
--             newAssociateSecurityKey
--
--         , requestClaimPhoneNumber $
--             newClaimPhoneNumber
--
--         , requestCreateAgentStatus $
--             newCreateAgentStatus
--
--         , requestCreateContactFlow $
--             newCreateContactFlow
--
--         , requestCreateContactFlowModule $
--             newCreateContactFlowModule
--
--         , requestCreateHoursOfOperation $
--             newCreateHoursOfOperation
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestCreateIntegrationAssociation $
--             newCreateIntegrationAssociation
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestCreateQuickConnect $
--             newCreateQuickConnect
--
--         , requestCreateRoutingProfile $
--             newCreateRoutingProfile
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestCreateSecurityProfile $
--             newCreateSecurityProfile
--
--         , requestCreateTaskTemplate $
--             newCreateTaskTemplate
--
--         , requestCreateTrafficDistributionGroup $
--             newCreateTrafficDistributionGroup
--
--         , requestCreateUseCase $
--             newCreateUseCase
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroup
--
--         , requestCreateVocabulary $
--             newCreateVocabulary
--
--         , requestDeleteContactFlow $
--             newDeleteContactFlow
--
--         , requestDeleteContactFlowModule $
--             newDeleteContactFlowModule
--
--         , requestDeleteHoursOfOperation $
--             newDeleteHoursOfOperation
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociation
--
--         , requestDeleteQuickConnect $
--             newDeleteQuickConnect
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDeleteSecurityProfile $
--             newDeleteSecurityProfile
--
--         , requestDeleteTaskTemplate $
--             newDeleteTaskTemplate
--
--         , requestDeleteTrafficDistributionGroup $
--             newDeleteTrafficDistributionGroup
--
--         , requestDeleteUseCase $
--             newDeleteUseCase
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroup
--
--         , requestDeleteVocabulary $
--             newDeleteVocabulary
--
--         , requestDescribeAgentStatus $
--             newDescribeAgentStatus
--
--         , requestDescribeContact $
--             newDescribeContact
--
--         , requestDescribeContactFlow $
--             newDescribeContactFlow
--
--         , requestDescribeContactFlowModule $
--             newDescribeContactFlowModule
--
--         , requestDescribeHoursOfOperation $
--             newDescribeHoursOfOperation
--
--         , requestDescribeInstance $
--             newDescribeInstance
--
--         , requestDescribeInstanceAttribute $
--             newDescribeInstanceAttribute
--
--         , requestDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfig
--
--         , requestDescribePhoneNumber $
--             newDescribePhoneNumber
--
--         , requestDescribeQueue $
--             newDescribeQueue
--
--         , requestDescribeQuickConnect $
--             newDescribeQuickConnect
--
--         , requestDescribeRoutingProfile $
--             newDescribeRoutingProfile
--
--         , requestDescribeRule $
--             newDescribeRule
--
--         , requestDescribeSecurityProfile $
--             newDescribeSecurityProfile
--
--         , requestDescribeTrafficDistributionGroup $
--             newDescribeTrafficDistributionGroup
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroup
--
--         , requestDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructure
--
--         , requestDescribeVocabulary $
--             newDescribeVocabulary
--
--         , requestDisassociateApprovedOrigin $
--             newDisassociateApprovedOrigin
--
--         , requestDisassociateBot $
--             newDisassociateBot
--
--         , requestDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfig
--
--         , requestDisassociateLambdaFunction $
--             newDisassociateLambdaFunction
--
--         , requestDisassociateLexBot $
--             newDisassociateLexBot
--
--         , requestDisassociatePhoneNumberContactFlow $
--             newDisassociatePhoneNumberContactFlow
--
--         , requestDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnects
--
--         , requestDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueues
--
--         , requestDisassociateSecurityKey $
--             newDisassociateSecurityKey
--
--         , requestDismissUserContact $
--             newDismissUserContact
--
--         , requestGetContactAttributes $
--             newGetContactAttributes
--
--         , requestGetCurrentMetricData $
--             newGetCurrentMetricData
--
--         , requestGetCurrentUserData $
--             newGetCurrentUserData
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--         , requestGetMetricData $
--             newGetMetricData
--
--         , requestGetTaskTemplate $
--             newGetTaskTemplate
--
--         , requestGetTrafficDistribution $
--             newGetTrafficDistribution
--
--         , requestListAgentStatuses $
--             newListAgentStatuses
--
--         , requestListApprovedOrigins $
--             newListApprovedOrigins
--
--         , requestListBots $
--             newListBots
--
--         , requestListContactFlowModules $
--             newListContactFlowModules
--
--         , requestListContactFlows $
--             newListContactFlows
--
--         , requestListContactReferences $
--             newListContactReferences
--
--         , requestListDefaultVocabularies $
--             newListDefaultVocabularies
--
--         , requestListHoursOfOperations $
--             newListHoursOfOperations
--
--         , requestListInstanceAttributes $
--             newListInstanceAttributes
--
--         , requestListInstanceStorageConfigs $
--             newListInstanceStorageConfigs
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListIntegrationAssociations $
--             newListIntegrationAssociations
--
--         , requestListLambdaFunctions $
--             newListLambdaFunctions
--
--         , requestListLexBots $
--             newListLexBots
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestListPhoneNumbersV2 $
--             newListPhoneNumbersV2
--
--         , requestListPrompts $
--             newListPrompts
--
--         , requestListQueueQuickConnects $
--             newListQueueQuickConnects
--
--         , requestListQueues $
--             newListQueues
--
--         , requestListQuickConnects $
--             newListQuickConnects
--
--         , requestListRoutingProfileQueues $
--             newListRoutingProfileQueues
--
--         , requestListRoutingProfiles $
--             newListRoutingProfiles
--
--         , requestListRules $
--             newListRules
--
--         , requestListSecurityKeys $
--             newListSecurityKeys
--
--         , requestListSecurityProfilePermissions $
--             newListSecurityProfilePermissions
--
--         , requestListSecurityProfiles $
--             newListSecurityProfiles
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTaskTemplates $
--             newListTaskTemplates
--
--         , requestListTrafficDistributionGroups $
--             newListTrafficDistributionGroups
--
--         , requestListUseCases $
--             newListUseCases
--
--         , requestListUserHierarchyGroups $
--             newListUserHierarchyGroups
--
--         , requestListUsers $
--             newListUsers
--
--         , requestMonitorContact $
--             newMonitorContact
--
--         , requestPutUserStatus $
--             newPutUserStatus
--
--         , requestReleasePhoneNumber $
--             newReleasePhoneNumber
--
--         , requestReplicateInstance $
--             newReplicateInstance
--
--         , requestResumeContactRecording $
--             newResumeContactRecording
--
--         , requestSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbers
--
--         , requestSearchQueues $
--             newSearchQueues
--
--         , requestSearchRoutingProfiles $
--             newSearchRoutingProfiles
--
--         , requestSearchSecurityProfiles $
--             newSearchSecurityProfiles
--
--         , requestSearchUsers $
--             newSearchUsers
--
--         , requestSearchVocabularies $
--             newSearchVocabularies
--
--         , requestStartChatContact $
--             newStartChatContact
--
--         , requestStartContactRecording $
--             newStartContactRecording
--
--         , requestStartContactStreaming $
--             newStartContactStreaming
--
--         , requestStartOutboundVoiceContact $
--             newStartOutboundVoiceContact
--
--         , requestStartTaskContact $
--             newStartTaskContact
--
--         , requestStopContact $
--             newStopContact
--
--         , requestStopContactRecording $
--             newStopContactRecording
--
--         , requestStopContactStreaming $
--             newStopContactStreaming
--
--         , requestSuspendContactRecording $
--             newSuspendContactRecording
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTransferContact $
--             newTransferContact
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAgentStatus $
--             newUpdateAgentStatus
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestUpdateContactAttributes $
--             newUpdateContactAttributes
--
--         , requestUpdateContactFlowContent $
--             newUpdateContactFlowContent
--
--         , requestUpdateContactFlowMetadata $
--             newUpdateContactFlowMetadata
--
--         , requestUpdateContactFlowModuleContent $
--             newUpdateContactFlowModuleContent
--
--         , requestUpdateContactFlowModuleMetadata $
--             newUpdateContactFlowModuleMetadata
--
--         , requestUpdateContactFlowName $
--             newUpdateContactFlowName
--
--         , requestUpdateContactSchedule $
--             newUpdateContactSchedule
--
--         , requestUpdateHoursOfOperation $
--             newUpdateHoursOfOperation
--
--         , requestUpdateInstanceAttribute $
--             newUpdateInstanceAttribute
--
--         , requestUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfig
--
--         , requestUpdateParticipantRoleConfig $
--             newUpdateParticipantRoleConfig
--
--         , requestUpdatePhoneNumber $
--             newUpdatePhoneNumber
--
--         , requestUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperation
--
--         , requestUpdateQueueMaxContacts $
--             newUpdateQueueMaxContacts
--
--         , requestUpdateQueueName $
--             newUpdateQueueName
--
--         , requestUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfig
--
--         , requestUpdateQueueStatus $
--             newUpdateQueueStatus
--
--         , requestUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfig
--
--         , requestUpdateQuickConnectName $
--             newUpdateQuickConnectName
--
--         , requestUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrency
--
--         , requestUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueue
--
--         , requestUpdateRoutingProfileName $
--             newUpdateRoutingProfileName
--
--         , requestUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueues
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestUpdateSecurityProfile $
--             newUpdateSecurityProfile
--
--         , requestUpdateTaskTemplate $
--             newUpdateTaskTemplate
--
--         , requestUpdateTrafficDistribution $
--             newUpdateTrafficDistribution
--
--         , requestUpdateUserHierarchy $
--             newUpdateUserHierarchy
--
--         , requestUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupName
--
--         , requestUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructure
--
--         , requestUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfo
--
--         , requestUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfig
--
--         , requestUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfile
--
--         , requestUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfiles
--
--           ]

--     , testGroup "response"
--         [ responseAssociateApprovedOrigin $
--             newAssociateApprovedOriginResponse
--
--         , responseAssociateBot $
--             newAssociateBotResponse
--
--         , responseAssociateDefaultVocabulary $
--             newAssociateDefaultVocabularyResponse
--
--         , responseAssociateInstanceStorageConfig $
--             newAssociateInstanceStorageConfigResponse
--
--         , responseAssociateLambdaFunction $
--             newAssociateLambdaFunctionResponse
--
--         , responseAssociateLexBot $
--             newAssociateLexBotResponse
--
--         , responseAssociatePhoneNumberContactFlow $
--             newAssociatePhoneNumberContactFlowResponse
--
--         , responseAssociateQueueQuickConnects $
--             newAssociateQueueQuickConnectsResponse
--
--         , responseAssociateRoutingProfileQueues $
--             newAssociateRoutingProfileQueuesResponse
--
--         , responseAssociateSecurityKey $
--             newAssociateSecurityKeyResponse
--
--         , responseClaimPhoneNumber $
--             newClaimPhoneNumberResponse
--
--         , responseCreateAgentStatus $
--             newCreateAgentStatusResponse
--
--         , responseCreateContactFlow $
--             newCreateContactFlowResponse
--
--         , responseCreateContactFlowModule $
--             newCreateContactFlowModuleResponse
--
--         , responseCreateHoursOfOperation $
--             newCreateHoursOfOperationResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseCreateIntegrationAssociation $
--             newCreateIntegrationAssociationResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseCreateQuickConnect $
--             newCreateQuickConnectResponse
--
--         , responseCreateRoutingProfile $
--             newCreateRoutingProfileResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseCreateSecurityProfile $
--             newCreateSecurityProfileResponse
--
--         , responseCreateTaskTemplate $
--             newCreateTaskTemplateResponse
--
--         , responseCreateTrafficDistributionGroup $
--             newCreateTrafficDistributionGroupResponse
--
--         , responseCreateUseCase $
--             newCreateUseCaseResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateUserHierarchyGroup $
--             newCreateUserHierarchyGroupResponse
--
--         , responseCreateVocabulary $
--             newCreateVocabularyResponse
--
--         , responseDeleteContactFlow $
--             newDeleteContactFlowResponse
--
--         , responseDeleteContactFlowModule $
--             newDeleteContactFlowModuleResponse
--
--         , responseDeleteHoursOfOperation $
--             newDeleteHoursOfOperationResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseDeleteIntegrationAssociation $
--             newDeleteIntegrationAssociationResponse
--
--         , responseDeleteQuickConnect $
--             newDeleteQuickConnectResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDeleteSecurityProfile $
--             newDeleteSecurityProfileResponse
--
--         , responseDeleteTaskTemplate $
--             newDeleteTaskTemplateResponse
--
--         , responseDeleteTrafficDistributionGroup $
--             newDeleteTrafficDistributionGroupResponse
--
--         , responseDeleteUseCase $
--             newDeleteUseCaseResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteUserHierarchyGroup $
--             newDeleteUserHierarchyGroupResponse
--
--         , responseDeleteVocabulary $
--             newDeleteVocabularyResponse
--
--         , responseDescribeAgentStatus $
--             newDescribeAgentStatusResponse
--
--         , responseDescribeContact $
--             newDescribeContactResponse
--
--         , responseDescribeContactFlow $
--             newDescribeContactFlowResponse
--
--         , responseDescribeContactFlowModule $
--             newDescribeContactFlowModuleResponse
--
--         , responseDescribeHoursOfOperation $
--             newDescribeHoursOfOperationResponse
--
--         , responseDescribeInstance $
--             newDescribeInstanceResponse
--
--         , responseDescribeInstanceAttribute $
--             newDescribeInstanceAttributeResponse
--
--         , responseDescribeInstanceStorageConfig $
--             newDescribeInstanceStorageConfigResponse
--
--         , responseDescribePhoneNumber $
--             newDescribePhoneNumberResponse
--
--         , responseDescribeQueue $
--             newDescribeQueueResponse
--
--         , responseDescribeQuickConnect $
--             newDescribeQuickConnectResponse
--
--         , responseDescribeRoutingProfile $
--             newDescribeRoutingProfileResponse
--
--         , responseDescribeRule $
--             newDescribeRuleResponse
--
--         , responseDescribeSecurityProfile $
--             newDescribeSecurityProfileResponse
--
--         , responseDescribeTrafficDistributionGroup $
--             newDescribeTrafficDistributionGroupResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDescribeUserHierarchyGroup $
--             newDescribeUserHierarchyGroupResponse
--
--         , responseDescribeUserHierarchyStructure $
--             newDescribeUserHierarchyStructureResponse
--
--         , responseDescribeVocabulary $
--             newDescribeVocabularyResponse
--
--         , responseDisassociateApprovedOrigin $
--             newDisassociateApprovedOriginResponse
--
--         , responseDisassociateBot $
--             newDisassociateBotResponse
--
--         , responseDisassociateInstanceStorageConfig $
--             newDisassociateInstanceStorageConfigResponse
--
--         , responseDisassociateLambdaFunction $
--             newDisassociateLambdaFunctionResponse
--
--         , responseDisassociateLexBot $
--             newDisassociateLexBotResponse
--
--         , responseDisassociatePhoneNumberContactFlow $
--             newDisassociatePhoneNumberContactFlowResponse
--
--         , responseDisassociateQueueQuickConnects $
--             newDisassociateQueueQuickConnectsResponse
--
--         , responseDisassociateRoutingProfileQueues $
--             newDisassociateRoutingProfileQueuesResponse
--
--         , responseDisassociateSecurityKey $
--             newDisassociateSecurityKeyResponse
--
--         , responseDismissUserContact $
--             newDismissUserContactResponse
--
--         , responseGetContactAttributes $
--             newGetContactAttributesResponse
--
--         , responseGetCurrentMetricData $
--             newGetCurrentMetricDataResponse
--
--         , responseGetCurrentUserData $
--             newGetCurrentUserDataResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--         , responseGetMetricData $
--             newGetMetricDataResponse
--
--         , responseGetTaskTemplate $
--             newGetTaskTemplateResponse
--
--         , responseGetTrafficDistribution $
--             newGetTrafficDistributionResponse
--
--         , responseListAgentStatuses $
--             newListAgentStatusesResponse
--
--         , responseListApprovedOrigins $
--             newListApprovedOriginsResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseListContactFlowModules $
--             newListContactFlowModulesResponse
--
--         , responseListContactFlows $
--             newListContactFlowsResponse
--
--         , responseListContactReferences $
--             newListContactReferencesResponse
--
--         , responseListDefaultVocabularies $
--             newListDefaultVocabulariesResponse
--
--         , responseListHoursOfOperations $
--             newListHoursOfOperationsResponse
--
--         , responseListInstanceAttributes $
--             newListInstanceAttributesResponse
--
--         , responseListInstanceStorageConfigs $
--             newListInstanceStorageConfigsResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListIntegrationAssociations $
--             newListIntegrationAssociationsResponse
--
--         , responseListLambdaFunctions $
--             newListLambdaFunctionsResponse
--
--         , responseListLexBots $
--             newListLexBotsResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseListPhoneNumbersV2 $
--             newListPhoneNumbersV2Response
--
--         , responseListPrompts $
--             newListPromptsResponse
--
--         , responseListQueueQuickConnects $
--             newListQueueQuickConnectsResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseListQuickConnects $
--             newListQuickConnectsResponse
--
--         , responseListRoutingProfileQueues $
--             newListRoutingProfileQueuesResponse
--
--         , responseListRoutingProfiles $
--             newListRoutingProfilesResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListSecurityKeys $
--             newListSecurityKeysResponse
--
--         , responseListSecurityProfilePermissions $
--             newListSecurityProfilePermissionsResponse
--
--         , responseListSecurityProfiles $
--             newListSecurityProfilesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTaskTemplates $
--             newListTaskTemplatesResponse
--
--         , responseListTrafficDistributionGroups $
--             newListTrafficDistributionGroupsResponse
--
--         , responseListUseCases $
--             newListUseCasesResponse
--
--         , responseListUserHierarchyGroups $
--             newListUserHierarchyGroupsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseMonitorContact $
--             newMonitorContactResponse
--
--         , responsePutUserStatus $
--             newPutUserStatusResponse
--
--         , responseReleasePhoneNumber $
--             newReleasePhoneNumberResponse
--
--         , responseReplicateInstance $
--             newReplicateInstanceResponse
--
--         , responseResumeContactRecording $
--             newResumeContactRecordingResponse
--
--         , responseSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbersResponse
--
--         , responseSearchQueues $
--             newSearchQueuesResponse
--
--         , responseSearchRoutingProfiles $
--             newSearchRoutingProfilesResponse
--
--         , responseSearchSecurityProfiles $
--             newSearchSecurityProfilesResponse
--
--         , responseSearchUsers $
--             newSearchUsersResponse
--
--         , responseSearchVocabularies $
--             newSearchVocabulariesResponse
--
--         , responseStartChatContact $
--             newStartChatContactResponse
--
--         , responseStartContactRecording $
--             newStartContactRecordingResponse
--
--         , responseStartContactStreaming $
--             newStartContactStreamingResponse
--
--         , responseStartOutboundVoiceContact $
--             newStartOutboundVoiceContactResponse
--
--         , responseStartTaskContact $
--             newStartTaskContactResponse
--
--         , responseStopContact $
--             newStopContactResponse
--
--         , responseStopContactRecording $
--             newStopContactRecordingResponse
--
--         , responseStopContactStreaming $
--             newStopContactStreamingResponse
--
--         , responseSuspendContactRecording $
--             newSuspendContactRecordingResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTransferContact $
--             newTransferContactResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAgentStatus $
--             newUpdateAgentStatusResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseUpdateContactAttributes $
--             newUpdateContactAttributesResponse
--
--         , responseUpdateContactFlowContent $
--             newUpdateContactFlowContentResponse
--
--         , responseUpdateContactFlowMetadata $
--             newUpdateContactFlowMetadataResponse
--
--         , responseUpdateContactFlowModuleContent $
--             newUpdateContactFlowModuleContentResponse
--
--         , responseUpdateContactFlowModuleMetadata $
--             newUpdateContactFlowModuleMetadataResponse
--
--         , responseUpdateContactFlowName $
--             newUpdateContactFlowNameResponse
--
--         , responseUpdateContactSchedule $
--             newUpdateContactScheduleResponse
--
--         , responseUpdateHoursOfOperation $
--             newUpdateHoursOfOperationResponse
--
--         , responseUpdateInstanceAttribute $
--             newUpdateInstanceAttributeResponse
--
--         , responseUpdateInstanceStorageConfig $
--             newUpdateInstanceStorageConfigResponse
--
--         , responseUpdateParticipantRoleConfig $
--             newUpdateParticipantRoleConfigResponse
--
--         , responseUpdatePhoneNumber $
--             newUpdatePhoneNumberResponse
--
--         , responseUpdateQueueHoursOfOperation $
--             newUpdateQueueHoursOfOperationResponse
--
--         , responseUpdateQueueMaxContacts $
--             newUpdateQueueMaxContactsResponse
--
--         , responseUpdateQueueName $
--             newUpdateQueueNameResponse
--
--         , responseUpdateQueueOutboundCallerConfig $
--             newUpdateQueueOutboundCallerConfigResponse
--
--         , responseUpdateQueueStatus $
--             newUpdateQueueStatusResponse
--
--         , responseUpdateQuickConnectConfig $
--             newUpdateQuickConnectConfigResponse
--
--         , responseUpdateQuickConnectName $
--             newUpdateQuickConnectNameResponse
--
--         , responseUpdateRoutingProfileConcurrency $
--             newUpdateRoutingProfileConcurrencyResponse
--
--         , responseUpdateRoutingProfileDefaultOutboundQueue $
--             newUpdateRoutingProfileDefaultOutboundQueueResponse
--
--         , responseUpdateRoutingProfileName $
--             newUpdateRoutingProfileNameResponse
--
--         , responseUpdateRoutingProfileQueues $
--             newUpdateRoutingProfileQueuesResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseUpdateSecurityProfile $
--             newUpdateSecurityProfileResponse
--
--         , responseUpdateTaskTemplate $
--             newUpdateTaskTemplateResponse
--
--         , responseUpdateTrafficDistribution $
--             newUpdateTrafficDistributionResponse
--
--         , responseUpdateUserHierarchy $
--             newUpdateUserHierarchyResponse
--
--         , responseUpdateUserHierarchyGroupName $
--             newUpdateUserHierarchyGroupNameResponse
--
--         , responseUpdateUserHierarchyStructure $
--             newUpdateUserHierarchyStructureResponse
--
--         , responseUpdateUserIdentityInfo $
--             newUpdateUserIdentityInfoResponse
--
--         , responseUpdateUserPhoneConfig $
--             newUpdateUserPhoneConfigResponse
--
--         , responseUpdateUserRoutingProfile $
--             newUpdateUserRoutingProfileResponse
--
--         , responseUpdateUserSecurityProfiles $
--             newUpdateUserSecurityProfilesResponse
--
--           ]
--     ]

-- Requests

requestAssociateApprovedOrigin :: AssociateApprovedOrigin -> TestTree
requestAssociateApprovedOrigin =
  req
    "AssociateApprovedOrigin"
    "fixture/AssociateApprovedOrigin.yaml"

requestAssociateBot :: AssociateBot -> TestTree
requestAssociateBot =
  req
    "AssociateBot"
    "fixture/AssociateBot.yaml"

requestAssociateDefaultVocabulary :: AssociateDefaultVocabulary -> TestTree
requestAssociateDefaultVocabulary =
  req
    "AssociateDefaultVocabulary"
    "fixture/AssociateDefaultVocabulary.yaml"

requestAssociateInstanceStorageConfig :: AssociateInstanceStorageConfig -> TestTree
requestAssociateInstanceStorageConfig =
  req
    "AssociateInstanceStorageConfig"
    "fixture/AssociateInstanceStorageConfig.yaml"

requestAssociateLambdaFunction :: AssociateLambdaFunction -> TestTree
requestAssociateLambdaFunction =
  req
    "AssociateLambdaFunction"
    "fixture/AssociateLambdaFunction.yaml"

requestAssociateLexBot :: AssociateLexBot -> TestTree
requestAssociateLexBot =
  req
    "AssociateLexBot"
    "fixture/AssociateLexBot.yaml"

requestAssociatePhoneNumberContactFlow :: AssociatePhoneNumberContactFlow -> TestTree
requestAssociatePhoneNumberContactFlow =
  req
    "AssociatePhoneNumberContactFlow"
    "fixture/AssociatePhoneNumberContactFlow.yaml"

requestAssociateQueueQuickConnects :: AssociateQueueQuickConnects -> TestTree
requestAssociateQueueQuickConnects =
  req
    "AssociateQueueQuickConnects"
    "fixture/AssociateQueueQuickConnects.yaml"

requestAssociateRoutingProfileQueues :: AssociateRoutingProfileQueues -> TestTree
requestAssociateRoutingProfileQueues =
  req
    "AssociateRoutingProfileQueues"
    "fixture/AssociateRoutingProfileQueues.yaml"

requestAssociateSecurityKey :: AssociateSecurityKey -> TestTree
requestAssociateSecurityKey =
  req
    "AssociateSecurityKey"
    "fixture/AssociateSecurityKey.yaml"

requestClaimPhoneNumber :: ClaimPhoneNumber -> TestTree
requestClaimPhoneNumber =
  req
    "ClaimPhoneNumber"
    "fixture/ClaimPhoneNumber.yaml"

requestCreateAgentStatus :: CreateAgentStatus -> TestTree
requestCreateAgentStatus =
  req
    "CreateAgentStatus"
    "fixture/CreateAgentStatus.yaml"

requestCreateContactFlow :: CreateContactFlow -> TestTree
requestCreateContactFlow =
  req
    "CreateContactFlow"
    "fixture/CreateContactFlow.yaml"

requestCreateContactFlowModule :: CreateContactFlowModule -> TestTree
requestCreateContactFlowModule =
  req
    "CreateContactFlowModule"
    "fixture/CreateContactFlowModule.yaml"

requestCreateHoursOfOperation :: CreateHoursOfOperation -> TestTree
requestCreateHoursOfOperation =
  req
    "CreateHoursOfOperation"
    "fixture/CreateHoursOfOperation.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestCreateIntegrationAssociation :: CreateIntegrationAssociation -> TestTree
requestCreateIntegrationAssociation =
  req
    "CreateIntegrationAssociation"
    "fixture/CreateIntegrationAssociation.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestCreateQuickConnect :: CreateQuickConnect -> TestTree
requestCreateQuickConnect =
  req
    "CreateQuickConnect"
    "fixture/CreateQuickConnect.yaml"

requestCreateRoutingProfile :: CreateRoutingProfile -> TestTree
requestCreateRoutingProfile =
  req
    "CreateRoutingProfile"
    "fixture/CreateRoutingProfile.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateSecurityProfile :: CreateSecurityProfile -> TestTree
requestCreateSecurityProfile =
  req
    "CreateSecurityProfile"
    "fixture/CreateSecurityProfile.yaml"

requestCreateTaskTemplate :: CreateTaskTemplate -> TestTree
requestCreateTaskTemplate =
  req
    "CreateTaskTemplate"
    "fixture/CreateTaskTemplate.yaml"

requestCreateTrafficDistributionGroup :: CreateTrafficDistributionGroup -> TestTree
requestCreateTrafficDistributionGroup =
  req
    "CreateTrafficDistributionGroup"
    "fixture/CreateTrafficDistributionGroup.yaml"

requestCreateUseCase :: CreateUseCase -> TestTree
requestCreateUseCase =
  req
    "CreateUseCase"
    "fixture/CreateUseCase.yaml"

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

requestCreateVocabulary :: CreateVocabulary -> TestTree
requestCreateVocabulary =
  req
    "CreateVocabulary"
    "fixture/CreateVocabulary.yaml"

requestDeleteContactFlow :: DeleteContactFlow -> TestTree
requestDeleteContactFlow =
  req
    "DeleteContactFlow"
    "fixture/DeleteContactFlow.yaml"

requestDeleteContactFlowModule :: DeleteContactFlowModule -> TestTree
requestDeleteContactFlowModule =
  req
    "DeleteContactFlowModule"
    "fixture/DeleteContactFlowModule.yaml"

requestDeleteHoursOfOperation :: DeleteHoursOfOperation -> TestTree
requestDeleteHoursOfOperation =
  req
    "DeleteHoursOfOperation"
    "fixture/DeleteHoursOfOperation.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDeleteIntegrationAssociation :: DeleteIntegrationAssociation -> TestTree
requestDeleteIntegrationAssociation =
  req
    "DeleteIntegrationAssociation"
    "fixture/DeleteIntegrationAssociation.yaml"

requestDeleteQuickConnect :: DeleteQuickConnect -> TestTree
requestDeleteQuickConnect =
  req
    "DeleteQuickConnect"
    "fixture/DeleteQuickConnect.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDeleteSecurityProfile :: DeleteSecurityProfile -> TestTree
requestDeleteSecurityProfile =
  req
    "DeleteSecurityProfile"
    "fixture/DeleteSecurityProfile.yaml"

requestDeleteTaskTemplate :: DeleteTaskTemplate -> TestTree
requestDeleteTaskTemplate =
  req
    "DeleteTaskTemplate"
    "fixture/DeleteTaskTemplate.yaml"

requestDeleteTrafficDistributionGroup :: DeleteTrafficDistributionGroup -> TestTree
requestDeleteTrafficDistributionGroup =
  req
    "DeleteTrafficDistributionGroup"
    "fixture/DeleteTrafficDistributionGroup.yaml"

requestDeleteUseCase :: DeleteUseCase -> TestTree
requestDeleteUseCase =
  req
    "DeleteUseCase"
    "fixture/DeleteUseCase.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteUserHierarchyGroup :: DeleteUserHierarchyGroup -> TestTree
requestDeleteUserHierarchyGroup =
  req
    "DeleteUserHierarchyGroup"
    "fixture/DeleteUserHierarchyGroup.yaml"

requestDeleteVocabulary :: DeleteVocabulary -> TestTree
requestDeleteVocabulary =
  req
    "DeleteVocabulary"
    "fixture/DeleteVocabulary.yaml"

requestDescribeAgentStatus :: DescribeAgentStatus -> TestTree
requestDescribeAgentStatus =
  req
    "DescribeAgentStatus"
    "fixture/DescribeAgentStatus.yaml"

requestDescribeContact :: DescribeContact -> TestTree
requestDescribeContact =
  req
    "DescribeContact"
    "fixture/DescribeContact.yaml"

requestDescribeContactFlow :: DescribeContactFlow -> TestTree
requestDescribeContactFlow =
  req
    "DescribeContactFlow"
    "fixture/DescribeContactFlow.yaml"

requestDescribeContactFlowModule :: DescribeContactFlowModule -> TestTree
requestDescribeContactFlowModule =
  req
    "DescribeContactFlowModule"
    "fixture/DescribeContactFlowModule.yaml"

requestDescribeHoursOfOperation :: DescribeHoursOfOperation -> TestTree
requestDescribeHoursOfOperation =
  req
    "DescribeHoursOfOperation"
    "fixture/DescribeHoursOfOperation.yaml"

requestDescribeInstance :: DescribeInstance -> TestTree
requestDescribeInstance =
  req
    "DescribeInstance"
    "fixture/DescribeInstance.yaml"

requestDescribeInstanceAttribute :: DescribeInstanceAttribute -> TestTree
requestDescribeInstanceAttribute =
  req
    "DescribeInstanceAttribute"
    "fixture/DescribeInstanceAttribute.yaml"

requestDescribeInstanceStorageConfig :: DescribeInstanceStorageConfig -> TestTree
requestDescribeInstanceStorageConfig =
  req
    "DescribeInstanceStorageConfig"
    "fixture/DescribeInstanceStorageConfig.yaml"

requestDescribePhoneNumber :: DescribePhoneNumber -> TestTree
requestDescribePhoneNumber =
  req
    "DescribePhoneNumber"
    "fixture/DescribePhoneNumber.yaml"

requestDescribeQueue :: DescribeQueue -> TestTree
requestDescribeQueue =
  req
    "DescribeQueue"
    "fixture/DescribeQueue.yaml"

requestDescribeQuickConnect :: DescribeQuickConnect -> TestTree
requestDescribeQuickConnect =
  req
    "DescribeQuickConnect"
    "fixture/DescribeQuickConnect.yaml"

requestDescribeRoutingProfile :: DescribeRoutingProfile -> TestTree
requestDescribeRoutingProfile =
  req
    "DescribeRoutingProfile"
    "fixture/DescribeRoutingProfile.yaml"

requestDescribeRule :: DescribeRule -> TestTree
requestDescribeRule =
  req
    "DescribeRule"
    "fixture/DescribeRule.yaml"

requestDescribeSecurityProfile :: DescribeSecurityProfile -> TestTree
requestDescribeSecurityProfile =
  req
    "DescribeSecurityProfile"
    "fixture/DescribeSecurityProfile.yaml"

requestDescribeTrafficDistributionGroup :: DescribeTrafficDistributionGroup -> TestTree
requestDescribeTrafficDistributionGroup =
  req
    "DescribeTrafficDistributionGroup"
    "fixture/DescribeTrafficDistributionGroup.yaml"

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

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure =
  req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestDescribeVocabulary :: DescribeVocabulary -> TestTree
requestDescribeVocabulary =
  req
    "DescribeVocabulary"
    "fixture/DescribeVocabulary.yaml"

requestDisassociateApprovedOrigin :: DisassociateApprovedOrigin -> TestTree
requestDisassociateApprovedOrigin =
  req
    "DisassociateApprovedOrigin"
    "fixture/DisassociateApprovedOrigin.yaml"

requestDisassociateBot :: DisassociateBot -> TestTree
requestDisassociateBot =
  req
    "DisassociateBot"
    "fixture/DisassociateBot.yaml"

requestDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfig -> TestTree
requestDisassociateInstanceStorageConfig =
  req
    "DisassociateInstanceStorageConfig"
    "fixture/DisassociateInstanceStorageConfig.yaml"

requestDisassociateLambdaFunction :: DisassociateLambdaFunction -> TestTree
requestDisassociateLambdaFunction =
  req
    "DisassociateLambdaFunction"
    "fixture/DisassociateLambdaFunction.yaml"

requestDisassociateLexBot :: DisassociateLexBot -> TestTree
requestDisassociateLexBot =
  req
    "DisassociateLexBot"
    "fixture/DisassociateLexBot.yaml"

requestDisassociatePhoneNumberContactFlow :: DisassociatePhoneNumberContactFlow -> TestTree
requestDisassociatePhoneNumberContactFlow =
  req
    "DisassociatePhoneNumberContactFlow"
    "fixture/DisassociatePhoneNumberContactFlow.yaml"

requestDisassociateQueueQuickConnects :: DisassociateQueueQuickConnects -> TestTree
requestDisassociateQueueQuickConnects =
  req
    "DisassociateQueueQuickConnects"
    "fixture/DisassociateQueueQuickConnects.yaml"

requestDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueues -> TestTree
requestDisassociateRoutingProfileQueues =
  req
    "DisassociateRoutingProfileQueues"
    "fixture/DisassociateRoutingProfileQueues.yaml"

requestDisassociateSecurityKey :: DisassociateSecurityKey -> TestTree
requestDisassociateSecurityKey =
  req
    "DisassociateSecurityKey"
    "fixture/DisassociateSecurityKey.yaml"

requestDismissUserContact :: DismissUserContact -> TestTree
requestDismissUserContact =
  req
    "DismissUserContact"
    "fixture/DismissUserContact.yaml"

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes =
  req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData =
  req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

requestGetCurrentUserData :: GetCurrentUserData -> TestTree
requestGetCurrentUserData =
  req
    "GetCurrentUserData"
    "fixture/GetCurrentUserData.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken =
  req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData =
  req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestGetTaskTemplate :: GetTaskTemplate -> TestTree
requestGetTaskTemplate =
  req
    "GetTaskTemplate"
    "fixture/GetTaskTemplate.yaml"

requestGetTrafficDistribution :: GetTrafficDistribution -> TestTree
requestGetTrafficDistribution =
  req
    "GetTrafficDistribution"
    "fixture/GetTrafficDistribution.yaml"

requestListAgentStatuses :: ListAgentStatuses -> TestTree
requestListAgentStatuses =
  req
    "ListAgentStatuses"
    "fixture/ListAgentStatuses.yaml"

requestListApprovedOrigins :: ListApprovedOrigins -> TestTree
requestListApprovedOrigins =
  req
    "ListApprovedOrigins"
    "fixture/ListApprovedOrigins.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestListContactFlowModules :: ListContactFlowModules -> TestTree
requestListContactFlowModules =
  req
    "ListContactFlowModules"
    "fixture/ListContactFlowModules.yaml"

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows =
  req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestListContactReferences :: ListContactReferences -> TestTree
requestListContactReferences =
  req
    "ListContactReferences"
    "fixture/ListContactReferences.yaml"

requestListDefaultVocabularies :: ListDefaultVocabularies -> TestTree
requestListDefaultVocabularies =
  req
    "ListDefaultVocabularies"
    "fixture/ListDefaultVocabularies.yaml"

requestListHoursOfOperations :: ListHoursOfOperations -> TestTree
requestListHoursOfOperations =
  req
    "ListHoursOfOperations"
    "fixture/ListHoursOfOperations.yaml"

requestListInstanceAttributes :: ListInstanceAttributes -> TestTree
requestListInstanceAttributes =
  req
    "ListInstanceAttributes"
    "fixture/ListInstanceAttributes.yaml"

requestListInstanceStorageConfigs :: ListInstanceStorageConfigs -> TestTree
requestListInstanceStorageConfigs =
  req
    "ListInstanceStorageConfigs"
    "fixture/ListInstanceStorageConfigs.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListIntegrationAssociations :: ListIntegrationAssociations -> TestTree
requestListIntegrationAssociations =
  req
    "ListIntegrationAssociations"
    "fixture/ListIntegrationAssociations.yaml"

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

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestListPhoneNumbersV2 :: ListPhoneNumbersV2 -> TestTree
requestListPhoneNumbersV2 =
  req
    "ListPhoneNumbersV2"
    "fixture/ListPhoneNumbersV2.yaml"

requestListPrompts :: ListPrompts -> TestTree
requestListPrompts =
  req
    "ListPrompts"
    "fixture/ListPrompts.yaml"

requestListQueueQuickConnects :: ListQueueQuickConnects -> TestTree
requestListQueueQuickConnects =
  req
    "ListQueueQuickConnects"
    "fixture/ListQueueQuickConnects.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestListQuickConnects :: ListQuickConnects -> TestTree
requestListQuickConnects =
  req
    "ListQuickConnects"
    "fixture/ListQuickConnects.yaml"

requestListRoutingProfileQueues :: ListRoutingProfileQueues -> TestTree
requestListRoutingProfileQueues =
  req
    "ListRoutingProfileQueues"
    "fixture/ListRoutingProfileQueues.yaml"

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles =
  req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListSecurityKeys :: ListSecurityKeys -> TestTree
requestListSecurityKeys =
  req
    "ListSecurityKeys"
    "fixture/ListSecurityKeys.yaml"

requestListSecurityProfilePermissions :: ListSecurityProfilePermissions -> TestTree
requestListSecurityProfilePermissions =
  req
    "ListSecurityProfilePermissions"
    "fixture/ListSecurityProfilePermissions.yaml"

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles =
  req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTaskTemplates :: ListTaskTemplates -> TestTree
requestListTaskTemplates =
  req
    "ListTaskTemplates"
    "fixture/ListTaskTemplates.yaml"

requestListTrafficDistributionGroups :: ListTrafficDistributionGroups -> TestTree
requestListTrafficDistributionGroups =
  req
    "ListTrafficDistributionGroups"
    "fixture/ListTrafficDistributionGroups.yaml"

requestListUseCases :: ListUseCases -> TestTree
requestListUseCases =
  req
    "ListUseCases"
    "fixture/ListUseCases.yaml"

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups =
  req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestMonitorContact :: MonitorContact -> TestTree
requestMonitorContact =
  req
    "MonitorContact"
    "fixture/MonitorContact.yaml"

requestPutUserStatus :: PutUserStatus -> TestTree
requestPutUserStatus =
  req
    "PutUserStatus"
    "fixture/PutUserStatus.yaml"

requestReleasePhoneNumber :: ReleasePhoneNumber -> TestTree
requestReleasePhoneNumber =
  req
    "ReleasePhoneNumber"
    "fixture/ReleasePhoneNumber.yaml"

requestReplicateInstance :: ReplicateInstance -> TestTree
requestReplicateInstance =
  req
    "ReplicateInstance"
    "fixture/ReplicateInstance.yaml"

requestResumeContactRecording :: ResumeContactRecording -> TestTree
requestResumeContactRecording =
  req
    "ResumeContactRecording"
    "fixture/ResumeContactRecording.yaml"

requestSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbers -> TestTree
requestSearchAvailablePhoneNumbers =
  req
    "SearchAvailablePhoneNumbers"
    "fixture/SearchAvailablePhoneNumbers.yaml"

requestSearchQueues :: SearchQueues -> TestTree
requestSearchQueues =
  req
    "SearchQueues"
    "fixture/SearchQueues.yaml"

requestSearchRoutingProfiles :: SearchRoutingProfiles -> TestTree
requestSearchRoutingProfiles =
  req
    "SearchRoutingProfiles"
    "fixture/SearchRoutingProfiles.yaml"

requestSearchSecurityProfiles :: SearchSecurityProfiles -> TestTree
requestSearchSecurityProfiles =
  req
    "SearchSecurityProfiles"
    "fixture/SearchSecurityProfiles.yaml"

requestSearchUsers :: SearchUsers -> TestTree
requestSearchUsers =
  req
    "SearchUsers"
    "fixture/SearchUsers.yaml"

requestSearchVocabularies :: SearchVocabularies -> TestTree
requestSearchVocabularies =
  req
    "SearchVocabularies"
    "fixture/SearchVocabularies.yaml"

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact =
  req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

requestStartContactRecording :: StartContactRecording -> TestTree
requestStartContactRecording =
  req
    "StartContactRecording"
    "fixture/StartContactRecording.yaml"

requestStartContactStreaming :: StartContactStreaming -> TestTree
requestStartContactStreaming =
  req
    "StartContactStreaming"
    "fixture/StartContactStreaming.yaml"

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact =
  req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestStartTaskContact :: StartTaskContact -> TestTree
requestStartTaskContact =
  req
    "StartTaskContact"
    "fixture/StartTaskContact.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact =
  req
    "StopContact"
    "fixture/StopContact.yaml"

requestStopContactRecording :: StopContactRecording -> TestTree
requestStopContactRecording =
  req
    "StopContactRecording"
    "fixture/StopContactRecording.yaml"

requestStopContactStreaming :: StopContactStreaming -> TestTree
requestStopContactStreaming =
  req
    "StopContactStreaming"
    "fixture/StopContactStreaming.yaml"

requestSuspendContactRecording :: SuspendContactRecording -> TestTree
requestSuspendContactRecording =
  req
    "SuspendContactRecording"
    "fixture/SuspendContactRecording.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTransferContact :: TransferContact -> TestTree
requestTransferContact =
  req
    "TransferContact"
    "fixture/TransferContact.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAgentStatus :: UpdateAgentStatus -> TestTree
requestUpdateAgentStatus =
  req
    "UpdateAgentStatus"
    "fixture/UpdateAgentStatus.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestUpdateContactAttributes :: UpdateContactAttributes -> TestTree
requestUpdateContactAttributes =
  req
    "UpdateContactAttributes"
    "fixture/UpdateContactAttributes.yaml"

requestUpdateContactFlowContent :: UpdateContactFlowContent -> TestTree
requestUpdateContactFlowContent =
  req
    "UpdateContactFlowContent"
    "fixture/UpdateContactFlowContent.yaml"

requestUpdateContactFlowMetadata :: UpdateContactFlowMetadata -> TestTree
requestUpdateContactFlowMetadata =
  req
    "UpdateContactFlowMetadata"
    "fixture/UpdateContactFlowMetadata.yaml"

requestUpdateContactFlowModuleContent :: UpdateContactFlowModuleContent -> TestTree
requestUpdateContactFlowModuleContent =
  req
    "UpdateContactFlowModuleContent"
    "fixture/UpdateContactFlowModuleContent.yaml"

requestUpdateContactFlowModuleMetadata :: UpdateContactFlowModuleMetadata -> TestTree
requestUpdateContactFlowModuleMetadata =
  req
    "UpdateContactFlowModuleMetadata"
    "fixture/UpdateContactFlowModuleMetadata.yaml"

requestUpdateContactFlowName :: UpdateContactFlowName -> TestTree
requestUpdateContactFlowName =
  req
    "UpdateContactFlowName"
    "fixture/UpdateContactFlowName.yaml"

requestUpdateContactSchedule :: UpdateContactSchedule -> TestTree
requestUpdateContactSchedule =
  req
    "UpdateContactSchedule"
    "fixture/UpdateContactSchedule.yaml"

requestUpdateHoursOfOperation :: UpdateHoursOfOperation -> TestTree
requestUpdateHoursOfOperation =
  req
    "UpdateHoursOfOperation"
    "fixture/UpdateHoursOfOperation.yaml"

requestUpdateInstanceAttribute :: UpdateInstanceAttribute -> TestTree
requestUpdateInstanceAttribute =
  req
    "UpdateInstanceAttribute"
    "fixture/UpdateInstanceAttribute.yaml"

requestUpdateInstanceStorageConfig :: UpdateInstanceStorageConfig -> TestTree
requestUpdateInstanceStorageConfig =
  req
    "UpdateInstanceStorageConfig"
    "fixture/UpdateInstanceStorageConfig.yaml"

requestUpdateParticipantRoleConfig :: UpdateParticipantRoleConfig -> TestTree
requestUpdateParticipantRoleConfig =
  req
    "UpdateParticipantRoleConfig"
    "fixture/UpdateParticipantRoleConfig.yaml"

requestUpdatePhoneNumber :: UpdatePhoneNumber -> TestTree
requestUpdatePhoneNumber =
  req
    "UpdatePhoneNumber"
    "fixture/UpdatePhoneNumber.yaml"

requestUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperation -> TestTree
requestUpdateQueueHoursOfOperation =
  req
    "UpdateQueueHoursOfOperation"
    "fixture/UpdateQueueHoursOfOperation.yaml"

requestUpdateQueueMaxContacts :: UpdateQueueMaxContacts -> TestTree
requestUpdateQueueMaxContacts =
  req
    "UpdateQueueMaxContacts"
    "fixture/UpdateQueueMaxContacts.yaml"

requestUpdateQueueName :: UpdateQueueName -> TestTree
requestUpdateQueueName =
  req
    "UpdateQueueName"
    "fixture/UpdateQueueName.yaml"

requestUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfig -> TestTree
requestUpdateQueueOutboundCallerConfig =
  req
    "UpdateQueueOutboundCallerConfig"
    "fixture/UpdateQueueOutboundCallerConfig.yaml"

requestUpdateQueueStatus :: UpdateQueueStatus -> TestTree
requestUpdateQueueStatus =
  req
    "UpdateQueueStatus"
    "fixture/UpdateQueueStatus.yaml"

requestUpdateQuickConnectConfig :: UpdateQuickConnectConfig -> TestTree
requestUpdateQuickConnectConfig =
  req
    "UpdateQuickConnectConfig"
    "fixture/UpdateQuickConnectConfig.yaml"

requestUpdateQuickConnectName :: UpdateQuickConnectName -> TestTree
requestUpdateQuickConnectName =
  req
    "UpdateQuickConnectName"
    "fixture/UpdateQuickConnectName.yaml"

requestUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrency -> TestTree
requestUpdateRoutingProfileConcurrency =
  req
    "UpdateRoutingProfileConcurrency"
    "fixture/UpdateRoutingProfileConcurrency.yaml"

requestUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueue -> TestTree
requestUpdateRoutingProfileDefaultOutboundQueue =
  req
    "UpdateRoutingProfileDefaultOutboundQueue"
    "fixture/UpdateRoutingProfileDefaultOutboundQueue.yaml"

requestUpdateRoutingProfileName :: UpdateRoutingProfileName -> TestTree
requestUpdateRoutingProfileName =
  req
    "UpdateRoutingProfileName"
    "fixture/UpdateRoutingProfileName.yaml"

requestUpdateRoutingProfileQueues :: UpdateRoutingProfileQueues -> TestTree
requestUpdateRoutingProfileQueues =
  req
    "UpdateRoutingProfileQueues"
    "fixture/UpdateRoutingProfileQueues.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestUpdateSecurityProfile :: UpdateSecurityProfile -> TestTree
requestUpdateSecurityProfile =
  req
    "UpdateSecurityProfile"
    "fixture/UpdateSecurityProfile.yaml"

requestUpdateTaskTemplate :: UpdateTaskTemplate -> TestTree
requestUpdateTaskTemplate =
  req
    "UpdateTaskTemplate"
    "fixture/UpdateTaskTemplate.yaml"

requestUpdateTrafficDistribution :: UpdateTrafficDistribution -> TestTree
requestUpdateTrafficDistribution =
  req
    "UpdateTrafficDistribution"
    "fixture/UpdateTrafficDistribution.yaml"

requestUpdateUserHierarchy :: UpdateUserHierarchy -> TestTree
requestUpdateUserHierarchy =
  req
    "UpdateUserHierarchy"
    "fixture/UpdateUserHierarchy.yaml"

requestUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupName -> TestTree
requestUpdateUserHierarchyGroupName =
  req
    "UpdateUserHierarchyGroupName"
    "fixture/UpdateUserHierarchyGroupName.yaml"

requestUpdateUserHierarchyStructure :: UpdateUserHierarchyStructure -> TestTree
requestUpdateUserHierarchyStructure =
  req
    "UpdateUserHierarchyStructure"
    "fixture/UpdateUserHierarchyStructure.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo =
  req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig =
  req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

requestUpdateUserRoutingProfile :: UpdateUserRoutingProfile -> TestTree
requestUpdateUserRoutingProfile =
  req
    "UpdateUserRoutingProfile"
    "fixture/UpdateUserRoutingProfile.yaml"

requestUpdateUserSecurityProfiles :: UpdateUserSecurityProfiles -> TestTree
requestUpdateUserSecurityProfiles =
  req
    "UpdateUserSecurityProfiles"
    "fixture/UpdateUserSecurityProfiles.yaml"

-- Responses

responseAssociateApprovedOrigin :: AssociateApprovedOriginResponse -> TestTree
responseAssociateApprovedOrigin =
  res
    "AssociateApprovedOriginResponse"
    "fixture/AssociateApprovedOriginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApprovedOrigin)

responseAssociateBot :: AssociateBotResponse -> TestTree
responseAssociateBot =
  res
    "AssociateBotResponse"
    "fixture/AssociateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateBot)

responseAssociateDefaultVocabulary :: AssociateDefaultVocabularyResponse -> TestTree
responseAssociateDefaultVocabulary =
  res
    "AssociateDefaultVocabularyResponse"
    "fixture/AssociateDefaultVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDefaultVocabulary)

responseAssociateInstanceStorageConfig :: AssociateInstanceStorageConfigResponse -> TestTree
responseAssociateInstanceStorageConfig =
  res
    "AssociateInstanceStorageConfigResponse"
    "fixture/AssociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateInstanceStorageConfig)

responseAssociateLambdaFunction :: AssociateLambdaFunctionResponse -> TestTree
responseAssociateLambdaFunction =
  res
    "AssociateLambdaFunctionResponse"
    "fixture/AssociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLambdaFunction)

responseAssociateLexBot :: AssociateLexBotResponse -> TestTree
responseAssociateLexBot =
  res
    "AssociateLexBotResponse"
    "fixture/AssociateLexBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLexBot)

responseAssociatePhoneNumberContactFlow :: AssociatePhoneNumberContactFlowResponse -> TestTree
responseAssociatePhoneNumberContactFlow =
  res
    "AssociatePhoneNumberContactFlowResponse"
    "fixture/AssociatePhoneNumberContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumberContactFlow)

responseAssociateQueueQuickConnects :: AssociateQueueQuickConnectsResponse -> TestTree
responseAssociateQueueQuickConnects =
  res
    "AssociateQueueQuickConnectsResponse"
    "fixture/AssociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateQueueQuickConnects)

responseAssociateRoutingProfileQueues :: AssociateRoutingProfileQueuesResponse -> TestTree
responseAssociateRoutingProfileQueues =
  res
    "AssociateRoutingProfileQueuesResponse"
    "fixture/AssociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRoutingProfileQueues)

responseAssociateSecurityKey :: AssociateSecurityKeyResponse -> TestTree
responseAssociateSecurityKey =
  res
    "AssociateSecurityKeyResponse"
    "fixture/AssociateSecurityKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSecurityKey)

responseClaimPhoneNumber :: ClaimPhoneNumberResponse -> TestTree
responseClaimPhoneNumber =
  res
    "ClaimPhoneNumberResponse"
    "fixture/ClaimPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimPhoneNumber)

responseCreateAgentStatus :: CreateAgentStatusResponse -> TestTree
responseCreateAgentStatus =
  res
    "CreateAgentStatusResponse"
    "fixture/CreateAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAgentStatus)

responseCreateContactFlow :: CreateContactFlowResponse -> TestTree
responseCreateContactFlow =
  res
    "CreateContactFlowResponse"
    "fixture/CreateContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactFlow)

responseCreateContactFlowModule :: CreateContactFlowModuleResponse -> TestTree
responseCreateContactFlowModule =
  res
    "CreateContactFlowModuleResponse"
    "fixture/CreateContactFlowModuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactFlowModule)

responseCreateHoursOfOperation :: CreateHoursOfOperationResponse -> TestTree
responseCreateHoursOfOperation =
  res
    "CreateHoursOfOperationResponse"
    "fixture/CreateHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHoursOfOperation)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstance)

responseCreateIntegrationAssociation :: CreateIntegrationAssociationResponse -> TestTree
responseCreateIntegrationAssociation =
  res
    "CreateIntegrationAssociationResponse"
    "fixture/CreateIntegrationAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegrationAssociation)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseCreateQuickConnect :: CreateQuickConnectResponse -> TestTree
responseCreateQuickConnect =
  res
    "CreateQuickConnectResponse"
    "fixture/CreateQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQuickConnect)

responseCreateRoutingProfile :: CreateRoutingProfileResponse -> TestTree
responseCreateRoutingProfile =
  res
    "CreateRoutingProfileResponse"
    "fixture/CreateRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoutingProfile)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseCreateSecurityProfile :: CreateSecurityProfileResponse -> TestTree
responseCreateSecurityProfile =
  res
    "CreateSecurityProfileResponse"
    "fixture/CreateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecurityProfile)

responseCreateTaskTemplate :: CreateTaskTemplateResponse -> TestTree
responseCreateTaskTemplate =
  res
    "CreateTaskTemplateResponse"
    "fixture/CreateTaskTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTaskTemplate)

responseCreateTrafficDistributionGroup :: CreateTrafficDistributionGroupResponse -> TestTree
responseCreateTrafficDistributionGroup =
  res
    "CreateTrafficDistributionGroupResponse"
    "fixture/CreateTrafficDistributionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficDistributionGroup)

responseCreateUseCase :: CreateUseCaseResponse -> TestTree
responseCreateUseCase =
  res
    "CreateUseCaseResponse"
    "fixture/CreateUseCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUseCase)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateUserHierarchyGroup :: CreateUserHierarchyGroupResponse -> TestTree
responseCreateUserHierarchyGroup =
  res
    "CreateUserHierarchyGroupResponse"
    "fixture/CreateUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserHierarchyGroup)

responseCreateVocabulary :: CreateVocabularyResponse -> TestTree
responseCreateVocabulary =
  res
    "CreateVocabularyResponse"
    "fixture/CreateVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVocabulary)

responseDeleteContactFlow :: DeleteContactFlowResponse -> TestTree
responseDeleteContactFlow =
  res
    "DeleteContactFlowResponse"
    "fixture/DeleteContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactFlow)

responseDeleteContactFlowModule :: DeleteContactFlowModuleResponse -> TestTree
responseDeleteContactFlowModule =
  res
    "DeleteContactFlowModuleResponse"
    "fixture/DeleteContactFlowModuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactFlowModule)

responseDeleteHoursOfOperation :: DeleteHoursOfOperationResponse -> TestTree
responseDeleteHoursOfOperation =
  res
    "DeleteHoursOfOperationResponse"
    "fixture/DeleteHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHoursOfOperation)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseDeleteIntegrationAssociation :: DeleteIntegrationAssociationResponse -> TestTree
responseDeleteIntegrationAssociation =
  res
    "DeleteIntegrationAssociationResponse"
    "fixture/DeleteIntegrationAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegrationAssociation)

responseDeleteQuickConnect :: DeleteQuickConnectResponse -> TestTree
responseDeleteQuickConnect =
  res
    "DeleteQuickConnectResponse"
    "fixture/DeleteQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQuickConnect)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseDeleteSecurityProfile :: DeleteSecurityProfileResponse -> TestTree
responseDeleteSecurityProfile =
  res
    "DeleteSecurityProfileResponse"
    "fixture/DeleteSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecurityProfile)

responseDeleteTaskTemplate :: DeleteTaskTemplateResponse -> TestTree
responseDeleteTaskTemplate =
  res
    "DeleteTaskTemplateResponse"
    "fixture/DeleteTaskTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTaskTemplate)

responseDeleteTrafficDistributionGroup :: DeleteTrafficDistributionGroupResponse -> TestTree
responseDeleteTrafficDistributionGroup =
  res
    "DeleteTrafficDistributionGroupResponse"
    "fixture/DeleteTrafficDistributionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficDistributionGroup)

responseDeleteUseCase :: DeleteUseCaseResponse -> TestTree
responseDeleteUseCase =
  res
    "DeleteUseCaseResponse"
    "fixture/DeleteUseCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUseCase)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteUserHierarchyGroup :: DeleteUserHierarchyGroupResponse -> TestTree
responseDeleteUserHierarchyGroup =
  res
    "DeleteUserHierarchyGroupResponse"
    "fixture/DeleteUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserHierarchyGroup)

responseDeleteVocabulary :: DeleteVocabularyResponse -> TestTree
responseDeleteVocabulary =
  res
    "DeleteVocabularyResponse"
    "fixture/DeleteVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVocabulary)

responseDescribeAgentStatus :: DescribeAgentStatusResponse -> TestTree
responseDescribeAgentStatus =
  res
    "DescribeAgentStatusResponse"
    "fixture/DescribeAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgentStatus)

responseDescribeContact :: DescribeContactResponse -> TestTree
responseDescribeContact =
  res
    "DescribeContactResponse"
    "fixture/DescribeContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContact)

responseDescribeContactFlow :: DescribeContactFlowResponse -> TestTree
responseDescribeContactFlow =
  res
    "DescribeContactFlowResponse"
    "fixture/DescribeContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContactFlow)

responseDescribeContactFlowModule :: DescribeContactFlowModuleResponse -> TestTree
responseDescribeContactFlowModule =
  res
    "DescribeContactFlowModuleResponse"
    "fixture/DescribeContactFlowModuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContactFlowModule)

responseDescribeHoursOfOperation :: DescribeHoursOfOperationResponse -> TestTree
responseDescribeHoursOfOperation =
  res
    "DescribeHoursOfOperationResponse"
    "fixture/DescribeHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHoursOfOperation)

responseDescribeInstance :: DescribeInstanceResponse -> TestTree
responseDescribeInstance =
  res
    "DescribeInstanceResponse"
    "fixture/DescribeInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstance)

responseDescribeInstanceAttribute :: DescribeInstanceAttributeResponse -> TestTree
responseDescribeInstanceAttribute =
  res
    "DescribeInstanceAttributeResponse"
    "fixture/DescribeInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAttribute)

responseDescribeInstanceStorageConfig :: DescribeInstanceStorageConfigResponse -> TestTree
responseDescribeInstanceStorageConfig =
  res
    "DescribeInstanceStorageConfigResponse"
    "fixture/DescribeInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceStorageConfig)

responseDescribePhoneNumber :: DescribePhoneNumberResponse -> TestTree
responseDescribePhoneNumber =
  res
    "DescribePhoneNumberResponse"
    "fixture/DescribePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePhoneNumber)

responseDescribeQueue :: DescribeQueueResponse -> TestTree
responseDescribeQueue =
  res
    "DescribeQueueResponse"
    "fixture/DescribeQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQueue)

responseDescribeQuickConnect :: DescribeQuickConnectResponse -> TestTree
responseDescribeQuickConnect =
  res
    "DescribeQuickConnectResponse"
    "fixture/DescribeQuickConnectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeQuickConnect)

responseDescribeRoutingProfile :: DescribeRoutingProfileResponse -> TestTree
responseDescribeRoutingProfile =
  res
    "DescribeRoutingProfileResponse"
    "fixture/DescribeRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoutingProfile)

responseDescribeRule :: DescribeRuleResponse -> TestTree
responseDescribeRule =
  res
    "DescribeRuleResponse"
    "fixture/DescribeRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRule)

responseDescribeSecurityProfile :: DescribeSecurityProfileResponse -> TestTree
responseDescribeSecurityProfile =
  res
    "DescribeSecurityProfileResponse"
    "fixture/DescribeSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecurityProfile)

responseDescribeTrafficDistributionGroup :: DescribeTrafficDistributionGroupResponse -> TestTree
responseDescribeTrafficDistributionGroup =
  res
    "DescribeTrafficDistributionGroupResponse"
    "fixture/DescribeTrafficDistributionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficDistributionGroup)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup =
  res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserHierarchyGroup)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure =
  res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserHierarchyStructure)

responseDescribeVocabulary :: DescribeVocabularyResponse -> TestTree
responseDescribeVocabulary =
  res
    "DescribeVocabularyResponse"
    "fixture/DescribeVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVocabulary)

responseDisassociateApprovedOrigin :: DisassociateApprovedOriginResponse -> TestTree
responseDisassociateApprovedOrigin =
  res
    "DisassociateApprovedOriginResponse"
    "fixture/DisassociateApprovedOriginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApprovedOrigin)

responseDisassociateBot :: DisassociateBotResponse -> TestTree
responseDisassociateBot =
  res
    "DisassociateBotResponse"
    "fixture/DisassociateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateBot)

responseDisassociateInstanceStorageConfig :: DisassociateInstanceStorageConfigResponse -> TestTree
responseDisassociateInstanceStorageConfig =
  res
    "DisassociateInstanceStorageConfigResponse"
    "fixture/DisassociateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateInstanceStorageConfig)

responseDisassociateLambdaFunction :: DisassociateLambdaFunctionResponse -> TestTree
responseDisassociateLambdaFunction =
  res
    "DisassociateLambdaFunctionResponse"
    "fixture/DisassociateLambdaFunctionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLambdaFunction)

responseDisassociateLexBot :: DisassociateLexBotResponse -> TestTree
responseDisassociateLexBot =
  res
    "DisassociateLexBotResponse"
    "fixture/DisassociateLexBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLexBot)

responseDisassociatePhoneNumberContactFlow :: DisassociatePhoneNumberContactFlowResponse -> TestTree
responseDisassociatePhoneNumberContactFlow =
  res
    "DisassociatePhoneNumberContactFlowResponse"
    "fixture/DisassociatePhoneNumberContactFlowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumberContactFlow)

responseDisassociateQueueQuickConnects :: DisassociateQueueQuickConnectsResponse -> TestTree
responseDisassociateQueueQuickConnects =
  res
    "DisassociateQueueQuickConnectsResponse"
    "fixture/DisassociateQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateQueueQuickConnects)

responseDisassociateRoutingProfileQueues :: DisassociateRoutingProfileQueuesResponse -> TestTree
responseDisassociateRoutingProfileQueues =
  res
    "DisassociateRoutingProfileQueuesResponse"
    "fixture/DisassociateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRoutingProfileQueues)

responseDisassociateSecurityKey :: DisassociateSecurityKeyResponse -> TestTree
responseDisassociateSecurityKey =
  res
    "DisassociateSecurityKeyResponse"
    "fixture/DisassociateSecurityKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSecurityKey)

responseDismissUserContact :: DismissUserContactResponse -> TestTree
responseDismissUserContact =
  res
    "DismissUserContactResponse"
    "fixture/DismissUserContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DismissUserContact)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes =
  res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactAttributes)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData =
  res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCurrentMetricData)

responseGetCurrentUserData :: GetCurrentUserDataResponse -> TestTree
responseGetCurrentUserData =
  res
    "GetCurrentUserDataResponse"
    "fixture/GetCurrentUserDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCurrentUserData)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFederationToken)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData =
  res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMetricData)

responseGetTaskTemplate :: GetTaskTemplateResponse -> TestTree
responseGetTaskTemplate =
  res
    "GetTaskTemplateResponse"
    "fixture/GetTaskTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTaskTemplate)

responseGetTrafficDistribution :: GetTrafficDistributionResponse -> TestTree
responseGetTrafficDistribution =
  res
    "GetTrafficDistributionResponse"
    "fixture/GetTrafficDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficDistribution)

responseListAgentStatuses :: ListAgentStatusesResponse -> TestTree
responseListAgentStatuses =
  res
    "ListAgentStatusesResponse"
    "fixture/ListAgentStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgentStatuses)

responseListApprovedOrigins :: ListApprovedOriginsResponse -> TestTree
responseListApprovedOrigins =
  res
    "ListApprovedOriginsResponse"
    "fixture/ListApprovedOriginsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApprovedOrigins)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBots)

responseListContactFlowModules :: ListContactFlowModulesResponse -> TestTree
responseListContactFlowModules =
  res
    "ListContactFlowModulesResponse"
    "fixture/ListContactFlowModulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactFlowModules)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows =
  res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactFlows)

responseListContactReferences :: ListContactReferencesResponse -> TestTree
responseListContactReferences =
  res
    "ListContactReferencesResponse"
    "fixture/ListContactReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactReferences)

responseListDefaultVocabularies :: ListDefaultVocabulariesResponse -> TestTree
responseListDefaultVocabularies =
  res
    "ListDefaultVocabulariesResponse"
    "fixture/ListDefaultVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDefaultVocabularies)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations =
  res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHoursOfOperations)

responseListInstanceAttributes :: ListInstanceAttributesResponse -> TestTree
responseListInstanceAttributes =
  res
    "ListInstanceAttributesResponse"
    "fixture/ListInstanceAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceAttributes)

responseListInstanceStorageConfigs :: ListInstanceStorageConfigsResponse -> TestTree
responseListInstanceStorageConfigs =
  res
    "ListInstanceStorageConfigsResponse"
    "fixture/ListInstanceStorageConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceStorageConfigs)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseListIntegrationAssociations :: ListIntegrationAssociationsResponse -> TestTree
responseListIntegrationAssociations =
  res
    "ListIntegrationAssociationsResponse"
    "fixture/ListIntegrationAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIntegrationAssociations)

responseListLambdaFunctions :: ListLambdaFunctionsResponse -> TestTree
responseListLambdaFunctions =
  res
    "ListLambdaFunctionsResponse"
    "fixture/ListLambdaFunctionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLambdaFunctions)

responseListLexBots :: ListLexBotsResponse -> TestTree
responseListLexBots =
  res
    "ListLexBotsResponse"
    "fixture/ListLexBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLexBots)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbers)

responseListPhoneNumbersV2 :: ListPhoneNumbersV2Response -> TestTree
responseListPhoneNumbersV2 =
  res
    "ListPhoneNumbersV2Response"
    "fixture/ListPhoneNumbersV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbersV2)

responseListPrompts :: ListPromptsResponse -> TestTree
responseListPrompts =
  res
    "ListPromptsResponse"
    "fixture/ListPromptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrompts)

responseListQueueQuickConnects :: ListQueueQuickConnectsResponse -> TestTree
responseListQueueQuickConnects =
  res
    "ListQueueQuickConnectsResponse"
    "fixture/ListQueueQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueueQuickConnects)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responseListQuickConnects :: ListQuickConnectsResponse -> TestTree
responseListQuickConnects =
  res
    "ListQuickConnectsResponse"
    "fixture/ListQuickConnectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQuickConnects)

responseListRoutingProfileQueues :: ListRoutingProfileQueuesResponse -> TestTree
responseListRoutingProfileQueues =
  res
    "ListRoutingProfileQueuesResponse"
    "fixture/ListRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingProfileQueues)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles =
  res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingProfiles)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListSecurityKeys :: ListSecurityKeysResponse -> TestTree
responseListSecurityKeys =
  res
    "ListSecurityKeysResponse"
    "fixture/ListSecurityKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityKeys)

responseListSecurityProfilePermissions :: ListSecurityProfilePermissionsResponse -> TestTree
responseListSecurityProfilePermissions =
  res
    "ListSecurityProfilePermissionsResponse"
    "fixture/ListSecurityProfilePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfilePermissions)

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles =
  res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecurityProfiles)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTaskTemplates :: ListTaskTemplatesResponse -> TestTree
responseListTaskTemplates =
  res
    "ListTaskTemplatesResponse"
    "fixture/ListTaskTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskTemplates)

responseListTrafficDistributionGroups :: ListTrafficDistributionGroupsResponse -> TestTree
responseListTrafficDistributionGroups =
  res
    "ListTrafficDistributionGroupsResponse"
    "fixture/ListTrafficDistributionGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficDistributionGroups)

responseListUseCases :: ListUseCasesResponse -> TestTree
responseListUseCases =
  res
    "ListUseCasesResponse"
    "fixture/ListUseCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUseCases)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups =
  res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserHierarchyGroups)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseMonitorContact :: MonitorContactResponse -> TestTree
responseMonitorContact =
  res
    "MonitorContactResponse"
    "fixture/MonitorContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MonitorContact)

responsePutUserStatus :: PutUserStatusResponse -> TestTree
responsePutUserStatus =
  res
    "PutUserStatusResponse"
    "fixture/PutUserStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutUserStatus)

responseReleasePhoneNumber :: ReleasePhoneNumberResponse -> TestTree
responseReleasePhoneNumber =
  res
    "ReleasePhoneNumberResponse"
    "fixture/ReleasePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleasePhoneNumber)

responseReplicateInstance :: ReplicateInstanceResponse -> TestTree
responseReplicateInstance =
  res
    "ReplicateInstanceResponse"
    "fixture/ReplicateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplicateInstance)

responseResumeContactRecording :: ResumeContactRecordingResponse -> TestTree
responseResumeContactRecording =
  res
    "ResumeContactRecordingResponse"
    "fixture/ResumeContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeContactRecording)

responseSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbersResponse -> TestTree
responseSearchAvailablePhoneNumbers =
  res
    "SearchAvailablePhoneNumbersResponse"
    "fixture/SearchAvailablePhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAvailablePhoneNumbers)

responseSearchQueues :: SearchQueuesResponse -> TestTree
responseSearchQueues =
  res
    "SearchQueuesResponse"
    "fixture/SearchQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchQueues)

responseSearchRoutingProfiles :: SearchRoutingProfilesResponse -> TestTree
responseSearchRoutingProfiles =
  res
    "SearchRoutingProfilesResponse"
    "fixture/SearchRoutingProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchRoutingProfiles)

responseSearchSecurityProfiles :: SearchSecurityProfilesResponse -> TestTree
responseSearchSecurityProfiles =
  res
    "SearchSecurityProfilesResponse"
    "fixture/SearchSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSecurityProfiles)

responseSearchUsers :: SearchUsersResponse -> TestTree
responseSearchUsers =
  res
    "SearchUsersResponse"
    "fixture/SearchUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchUsers)

responseSearchVocabularies :: SearchVocabulariesResponse -> TestTree
responseSearchVocabularies =
  res
    "SearchVocabulariesResponse"
    "fixture/SearchVocabulariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchVocabularies)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact =
  res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChatContact)

responseStartContactRecording :: StartContactRecordingResponse -> TestTree
responseStartContactRecording =
  res
    "StartContactRecordingResponse"
    "fixture/StartContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContactRecording)

responseStartContactStreaming :: StartContactStreamingResponse -> TestTree
responseStartContactStreaming =
  res
    "StartContactStreamingResponse"
    "fixture/StartContactStreamingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContactStreaming)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact =
  res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartOutboundVoiceContact)

responseStartTaskContact :: StartTaskContactResponse -> TestTree
responseStartTaskContact =
  res
    "StartTaskContactResponse"
    "fixture/StartTaskContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTaskContact)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact =
  res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContact)

responseStopContactRecording :: StopContactRecordingResponse -> TestTree
responseStopContactRecording =
  res
    "StopContactRecordingResponse"
    "fixture/StopContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContactRecording)

responseStopContactStreaming :: StopContactStreamingResponse -> TestTree
responseStopContactStreaming =
  res
    "StopContactStreamingResponse"
    "fixture/StopContactStreamingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopContactStreaming)

responseSuspendContactRecording :: SuspendContactRecordingResponse -> TestTree
responseSuspendContactRecording =
  res
    "SuspendContactRecordingResponse"
    "fixture/SuspendContactRecordingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendContactRecording)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTransferContact :: TransferContactResponse -> TestTree
responseTransferContact =
  res
    "TransferContactResponse"
    "fixture/TransferContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferContact)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAgentStatus :: UpdateAgentStatusResponse -> TestTree
responseUpdateAgentStatus =
  res
    "UpdateAgentStatusResponse"
    "fixture/UpdateAgentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAgentStatus)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContact)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes =
  res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactAttributes)

responseUpdateContactFlowContent :: UpdateContactFlowContentResponse -> TestTree
responseUpdateContactFlowContent =
  res
    "UpdateContactFlowContentResponse"
    "fixture/UpdateContactFlowContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowContent)

responseUpdateContactFlowMetadata :: UpdateContactFlowMetadataResponse -> TestTree
responseUpdateContactFlowMetadata =
  res
    "UpdateContactFlowMetadataResponse"
    "fixture/UpdateContactFlowMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowMetadata)

responseUpdateContactFlowModuleContent :: UpdateContactFlowModuleContentResponse -> TestTree
responseUpdateContactFlowModuleContent =
  res
    "UpdateContactFlowModuleContentResponse"
    "fixture/UpdateContactFlowModuleContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowModuleContent)

responseUpdateContactFlowModuleMetadata :: UpdateContactFlowModuleMetadataResponse -> TestTree
responseUpdateContactFlowModuleMetadata =
  res
    "UpdateContactFlowModuleMetadataResponse"
    "fixture/UpdateContactFlowModuleMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowModuleMetadata)

responseUpdateContactFlowName :: UpdateContactFlowNameResponse -> TestTree
responseUpdateContactFlowName =
  res
    "UpdateContactFlowNameResponse"
    "fixture/UpdateContactFlowNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactFlowName)

responseUpdateContactSchedule :: UpdateContactScheduleResponse -> TestTree
responseUpdateContactSchedule =
  res
    "UpdateContactScheduleResponse"
    "fixture/UpdateContactScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactSchedule)

responseUpdateHoursOfOperation :: UpdateHoursOfOperationResponse -> TestTree
responseUpdateHoursOfOperation =
  res
    "UpdateHoursOfOperationResponse"
    "fixture/UpdateHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHoursOfOperation)

responseUpdateInstanceAttribute :: UpdateInstanceAttributeResponse -> TestTree
responseUpdateInstanceAttribute =
  res
    "UpdateInstanceAttributeResponse"
    "fixture/UpdateInstanceAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceAttribute)

responseUpdateInstanceStorageConfig :: UpdateInstanceStorageConfigResponse -> TestTree
responseUpdateInstanceStorageConfig =
  res
    "UpdateInstanceStorageConfigResponse"
    "fixture/UpdateInstanceStorageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceStorageConfig)

responseUpdateParticipantRoleConfig :: UpdateParticipantRoleConfigResponse -> TestTree
responseUpdateParticipantRoleConfig =
  res
    "UpdateParticipantRoleConfigResponse"
    "fixture/UpdateParticipantRoleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateParticipantRoleConfig)

responseUpdatePhoneNumber :: UpdatePhoneNumberResponse -> TestTree
responseUpdatePhoneNumber =
  res
    "UpdatePhoneNumberResponse"
    "fixture/UpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumber)

responseUpdateQueueHoursOfOperation :: UpdateQueueHoursOfOperationResponse -> TestTree
responseUpdateQueueHoursOfOperation =
  res
    "UpdateQueueHoursOfOperationResponse"
    "fixture/UpdateQueueHoursOfOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueHoursOfOperation)

responseUpdateQueueMaxContacts :: UpdateQueueMaxContactsResponse -> TestTree
responseUpdateQueueMaxContacts =
  res
    "UpdateQueueMaxContactsResponse"
    "fixture/UpdateQueueMaxContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueMaxContacts)

responseUpdateQueueName :: UpdateQueueNameResponse -> TestTree
responseUpdateQueueName =
  res
    "UpdateQueueNameResponse"
    "fixture/UpdateQueueNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueName)

responseUpdateQueueOutboundCallerConfig :: UpdateQueueOutboundCallerConfigResponse -> TestTree
responseUpdateQueueOutboundCallerConfig =
  res
    "UpdateQueueOutboundCallerConfigResponse"
    "fixture/UpdateQueueOutboundCallerConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueOutboundCallerConfig)

responseUpdateQueueStatus :: UpdateQueueStatusResponse -> TestTree
responseUpdateQueueStatus =
  res
    "UpdateQueueStatusResponse"
    "fixture/UpdateQueueStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueueStatus)

responseUpdateQuickConnectConfig :: UpdateQuickConnectConfigResponse -> TestTree
responseUpdateQuickConnectConfig =
  res
    "UpdateQuickConnectConfigResponse"
    "fixture/UpdateQuickConnectConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuickConnectConfig)

responseUpdateQuickConnectName :: UpdateQuickConnectNameResponse -> TestTree
responseUpdateQuickConnectName =
  res
    "UpdateQuickConnectNameResponse"
    "fixture/UpdateQuickConnectNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQuickConnectName)

responseUpdateRoutingProfileConcurrency :: UpdateRoutingProfileConcurrencyResponse -> TestTree
responseUpdateRoutingProfileConcurrency =
  res
    "UpdateRoutingProfileConcurrencyResponse"
    "fixture/UpdateRoutingProfileConcurrencyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileConcurrency)

responseUpdateRoutingProfileDefaultOutboundQueue :: UpdateRoutingProfileDefaultOutboundQueueResponse -> TestTree
responseUpdateRoutingProfileDefaultOutboundQueue =
  res
    "UpdateRoutingProfileDefaultOutboundQueueResponse"
    "fixture/UpdateRoutingProfileDefaultOutboundQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileDefaultOutboundQueue)

responseUpdateRoutingProfileName :: UpdateRoutingProfileNameResponse -> TestTree
responseUpdateRoutingProfileName =
  res
    "UpdateRoutingProfileNameResponse"
    "fixture/UpdateRoutingProfileNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileName)

responseUpdateRoutingProfileQueues :: UpdateRoutingProfileQueuesResponse -> TestTree
responseUpdateRoutingProfileQueues =
  res
    "UpdateRoutingProfileQueuesResponse"
    "fixture/UpdateRoutingProfileQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingProfileQueues)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRule)

responseUpdateSecurityProfile :: UpdateSecurityProfileResponse -> TestTree
responseUpdateSecurityProfile =
  res
    "UpdateSecurityProfileResponse"
    "fixture/UpdateSecurityProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurityProfile)

responseUpdateTaskTemplate :: UpdateTaskTemplateResponse -> TestTree
responseUpdateTaskTemplate =
  res
    "UpdateTaskTemplateResponse"
    "fixture/UpdateTaskTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskTemplate)

responseUpdateTrafficDistribution :: UpdateTrafficDistributionResponse -> TestTree
responseUpdateTrafficDistribution =
  res
    "UpdateTrafficDistributionResponse"
    "fixture/UpdateTrafficDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrafficDistribution)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy =
  res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchy)

responseUpdateUserHierarchyGroupName :: UpdateUserHierarchyGroupNameResponse -> TestTree
responseUpdateUserHierarchyGroupName =
  res
    "UpdateUserHierarchyGroupNameResponse"
    "fixture/UpdateUserHierarchyGroupNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchyGroupName)

responseUpdateUserHierarchyStructure :: UpdateUserHierarchyStructureResponse -> TestTree
responseUpdateUserHierarchyStructure =
  res
    "UpdateUserHierarchyStructureResponse"
    "fixture/UpdateUserHierarchyStructureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserHierarchyStructure)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo =
  res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserIdentityInfo)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig =
  res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPhoneConfig)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile =
  res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserRoutingProfile)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles =
  res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserSecurityProfiles)

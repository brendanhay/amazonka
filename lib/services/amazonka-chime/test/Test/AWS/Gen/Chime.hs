{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Chime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Chime where

import Data.Proxy
import Network.AWS.Chime
import Test.AWS.Chime.Internal
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
--         [ requestDescribeChannelMembership $
--             newDescribeChannelMembership
--
--         , requestCreateAppInstance $
--             newCreateAppInstance
--
--         , requestGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroup
--
--         , requestListVoiceConnectors $
--             newListVoiceConnectors
--
--         , requestListRoomMemberships $
--             newListRoomMemberships
--
--         , requestGetPhoneNumberSettings $
--             newGetPhoneNumberSettings
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestListAttendees $
--             newListAttendees
--
--         , requestPutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfiguration
--
--         , requestGetVoiceConnectorTermination $
--             newGetVoiceConnectorTermination
--
--         , requestDeleteAttendee $
--             newDeleteAttendee
--
--         , requestGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxy
--
--         , requestDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfiguration
--
--         , requestGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfiguration
--
--         , requestUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCall
--
--         , requestStopMeetingTranscription $
--             newStopMeetingTranscription
--
--         , requestGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettings
--
--         , requestPutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfiguration
--
--         , requestCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendees
--
--         , requestListChannels $
--             newListChannels
--
--         , requestDisassociatePhoneNumberFromUser $
--             newDisassociatePhoneNumberFromUser
--
--         , requestDisassociateSigninDelegateGroupsFromAccount $
--             newDisassociateSigninDelegateGroupsFromAccount
--
--         , requestResetPersonalPIN $
--             newResetPersonalPIN
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdmin
--
--         , requestCreateAttendee $
--             newCreateAttendee
--
--         , requestListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountries
--
--         , requestDeleteSipRule $
--             newDeleteSipRule
--
--         , requestUpdateSipRule $
--             newUpdateSipRule
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOrigination
--
--         , requestDeleteSipMediaApplication $
--             newDeleteSipMediaApplication
--
--         , requestUpdateSipMediaApplication $
--             newUpdateSipMediaApplication
--
--         , requestDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnector
--
--         , requestGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpoint
--
--         , requestPutVoiceConnectorOrigination $
--             newPutVoiceConnectorOrigination
--
--         , requestCreateAppInstanceUser $
--             newCreateAppInstanceUser
--
--         , requestListAttendeeTags $
--             newListAttendeeTags
--
--         , requestListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUser
--
--         , requestRedactChannelMessage $
--             newRedactChannelMessage
--
--         , requestPutRetentionSettings $
--             newPutRetentionSettings
--
--         , requestListUsers $
--             newListUsers
--
--         , requestDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfiguration
--
--         , requestAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroup
--
--         , requestPutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettings
--
--         , requestGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfiguration
--
--         , requestListBots $
--             newListBots
--
--         , requestDeleteChannelMembership $
--             newDeleteChannelMembership
--
--         , requestPutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfiguration
--
--         , requestListChannelMemberships $
--             newListChannelMemberships
--
--         , requestGetGlobalSettings $
--             newGetGlobalSettings
--
--         , requestDeleteMeeting $
--             newDeleteMeeting
--
--         , requestListMeetings $
--             newListMeetings
--
--         , requestGetAttendee $
--             newGetAttendee
--
--         , requestDeleteAccount $
--             newDeleteAccount
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestUpdateBot $
--             newUpdateBot
--
--         , requestListPhoneNumberOrders $
--             newListPhoneNumberOrders
--
--         , requestSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbers
--
--         , requestCreateAppInstanceAdmin $
--             newCreateAppInstanceAdmin
--
--         , requestTagMeeting $
--             newTagMeeting
--
--         , requestListVoiceConnectorGroups $
--             newListVoiceConnectorGroups
--
--         , requestLogoutUser $
--             newLogoutUser
--
--         , requestListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentials
--
--         , requestCreateMediaCapturePipeline $
--             newCreateMediaCapturePipeline
--
--         , requestCreateProxySession $
--             newCreateProxySession
--
--         , requestDeleteEventsConfiguration $
--             newDeleteEventsConfiguration
--
--         , requestPutEventsConfiguration $
--             newPutEventsConfiguration
--
--         , requestGetChannelMessage $
--             newGetChannelMessage
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestPutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfiguration
--
--         , requestDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUser
--
--         , requestListAppInstanceAdmins $
--             newListAppInstanceAdmins
--
--         , requestDeletePhoneNumber $
--             newDeletePhoneNumber
--
--         , requestUpdatePhoneNumber $
--             newUpdatePhoneNumber
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestCreateChannelModerator $
--             newCreateChannelModerator
--
--         , requestGetAppInstanceStreamingConfigurations $
--             newGetAppInstanceStreamingConfigurations
--
--         , requestListAppInstances $
--             newListAppInstances
--
--         , requestDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUser
--
--         , requestGetPhoneNumber $
--             newGetPhoneNumber
--
--         , requestGetEventsConfiguration $
--             newGetEventsConfiguration
--
--         , requestGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfiguration
--
--         , requestBatchUpdateUser $
--             newBatchUpdateUser
--
--         , requestSendChannelMessage $
--             newSendChannelMessage
--
--         , requestTagAttendee $
--             newTagAttendee
--
--         , requestUpdateVoiceConnector $
--             newUpdateVoiceConnector
--
--         , requestDeleteVoiceConnector $
--             newDeleteVoiceConnector
--
--         , requestGetMediaCapturePipeline $
--             newGetMediaCapturePipeline
--
--         , requestUpdateRoomMembership $
--             newUpdateRoomMembership
--
--         , requestGetProxySession $
--             newGetProxySession
--
--         , requestDeleteRoomMembership $
--             newDeleteRoomMembership
--
--         , requestDescribeAppInstanceUser $
--             newDescribeAppInstanceUser
--
--         , requestBatchUnsuspendUser $
--             newBatchUnsuspendUser
--
--         , requestDeleteChannelBan $
--             newDeleteChannelBan
--
--         , requestGetMeeting $
--             newGetMeeting
--
--         , requestRestorePhoneNumber $
--             newRestorePhoneNumber
--
--         , requestGetRetentionSettings $
--             newGetRetentionSettings
--
--         , requestGetBot $
--             newGetBot
--
--         , requestGetUser $
--             newGetUser
--
--         , requestUntagAttendee $
--             newUntagAttendee
--
--         , requestStartMeetingTranscription $
--             newStartMeetingTranscription
--
--         , requestListChannelBans $
--             newListChannelBans
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestBatchSuspendUser $
--             newBatchSuspendUser
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestDescribeChannelModerator $
--             newDescribeChannelModerator
--
--         , requestAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnector
--
--         , requestGetPhoneNumberOrder $
--             newGetPhoneNumberOrder
--
--         , requestGetSipRule $
--             newGetSipRule
--
--         , requestGetUserSettings $
--             newGetUserSettings
--
--         , requestGetSipMediaApplication $
--             newGetSipMediaApplication
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestCreateChannelBan $
--             newCreateChannelBan
--
--         , requestListMeetingTags $
--             newListMeetingTags
--
--         , requestListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUser
--
--         , requestGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOrigination
--
--         , requestBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumber
--
--         , requestDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroup
--
--         , requestUpdateChannelReadMarker $
--             newUpdateChannelReadMarker
--
--         , requestCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCall
--
--         , requestBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumber
--
--         , requestListSipMediaApplications $
--             newListSipMediaApplications
--
--         , requestCreateMeeting $
--             newCreateMeeting
--
--         , requestCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrder
--
--         , requestListSipRules $
--             newListSipRules
--
--         , requestCreateBot $
--             newCreateBot
--
--         , requestUpdateUserSettings $
--             newUpdateUserSettings
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestBatchCreateRoomMembership $
--             newBatchCreateRoomMembership
--
--         , requestDescribeAppInstance $
--             newDescribeAppInstance
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestCreateChannelMembership $
--             newCreateChannelMembership
--
--         , requestDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTermination
--
--         , requestAssociatePhoneNumberWithUser $
--             newAssociatePhoneNumberWithUser
--
--         , requestDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxy
--
--         , requestCreateSipMediaApplication $
--             newCreateSipMediaApplication
--
--         , requestPutVoiceConnectorProxy $
--             newPutVoiceConnectorProxy
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestPutVoiceConnectorTermination $
--             newPutVoiceConnectorTermination
--
--         , requestGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfiguration
--
--         , requestPutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentials
--
--         , requestListAppInstanceUsers $
--             newListAppInstanceUsers
--
--         , requestAssociateSigninDelegateGroupsWithAccount $
--             newAssociateSigninDelegateGroupsWithAccount
--
--         , requestCreateSipRule $
--             newCreateSipRule
--
--         , requestDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentials
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteAppInstanceUser $
--             newDeleteAppInstanceUser
--
--         , requestUpdateAppInstanceUser $
--             newUpdateAppInstanceUser
--
--         , requestUntagMeeting $
--             newUntagMeeting
--
--         , requestUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroup
--
--         , requestRedactConversationMessage $
--             newRedactConversationMessage
--
--         , requestDeleteChannelModerator $
--             newDeleteChannelModerator
--
--         , requestDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroup
--
--         , requestDescribeChannelBan $
--             newDescribeChannelBan
--
--         , requestDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipeline
--
--         , requestUpdateProxySession $
--             newUpdateProxySession
--
--         , requestDeleteProxySession $
--             newDeleteProxySession
--
--         , requestGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealth
--
--         , requestCreateMeetingDialOut $
--             newCreateMeetingDialOut
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListProxySessions $
--             newListProxySessions
--
--         , requestListMediaCapturePipelines $
--             newListMediaCapturePipelines
--
--         , requestUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettings
--
--         , requestInviteUsers $
--             newInviteUsers
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestListChannelModerators $
--             newListChannelModerators
--
--         , requestGetVoiceConnector $
--             newGetVoiceConnector
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroup
--
--         , requestDeleteAppInstanceStreamingConfigurations $
--             newDeleteAppInstanceStreamingConfigurations
--
--         , requestListRooms $
--             newListRooms
--
--         , requestBatchCreateAttendee $
--             newBatchCreateAttendee
--
--         , requestDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdmin
--
--         , requestPutAppInstanceStreamingConfigurations $
--             newPutAppInstanceStreamingConfigurations
--
--         , requestRegenerateSecurityToken $
--             newRegenerateSecurityToken
--
--         , requestDeleteChannelMessage $
--             newDeleteChannelMessage
--
--         , requestUpdateChannelMessage $
--             newUpdateChannelMessage
--
--         , requestDeleteAppInstance $
--             newDeleteAppInstance
--
--         , requestUpdateAppInstance $
--             newUpdateAppInstance
--
--         , requestCreateVoiceConnector $
--             newCreateVoiceConnector
--
--         , requestListChannelMessages $
--             newListChannelMessages
--
--         , requestRedactRoomMessage $
--             newRedactRoomMessage
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestCreateRoomMembership $
--             newCreateRoomMembership
--
--         , requestBatchCreateChannelMembership $
--             newBatchCreateChannelMembership
--
--           ]

--     , testGroup "response"
--         [ responseDescribeChannelMembership $
--             newDescribeChannelMembershipResponse
--
--         , responseCreateAppInstance $
--             newCreateAppInstanceResponse
--
--         , responseGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroupResponse
--
--         , responseListVoiceConnectors $
--             newListVoiceConnectorsResponse
--
--         , responseListRoomMemberships $
--             newListRoomMembershipsResponse
--
--         , responseGetPhoneNumberSettings $
--             newGetPhoneNumberSettingsResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseListAttendees $
--             newListAttendeesResponse
--
--         , responsePutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfigurationResponse
--
--         , responseGetVoiceConnectorTermination $
--             newGetVoiceConnectorTerminationResponse
--
--         , responseDeleteAttendee $
--             newDeleteAttendeeResponse
--
--         , responseGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxyResponse
--
--         , responseDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfigurationResponse
--
--         , responseUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCallResponse
--
--         , responseStopMeetingTranscription $
--             newStopMeetingTranscriptionResponse
--
--         , responseGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettingsResponse
--
--         , responsePutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendeesResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseDisassociatePhoneNumberFromUser $
--             newDisassociatePhoneNumberFromUserResponse
--
--         , responseDisassociateSigninDelegateGroupsFromAccount $
--             newDisassociateSigninDelegateGroupsFromAccountResponse
--
--         , responseResetPersonalPIN $
--             newResetPersonalPINResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdminResponse
--
--         , responseCreateAttendee $
--             newCreateAttendeeResponse
--
--         , responseListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountriesResponse
--
--         , responseDeleteSipRule $
--             newDeleteSipRuleResponse
--
--         , responseUpdateSipRule $
--             newUpdateSipRuleResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOriginationResponse
--
--         , responseDeleteSipMediaApplication $
--             newDeleteSipMediaApplicationResponse
--
--         , responseUpdateSipMediaApplication $
--             newUpdateSipMediaApplicationResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnectorResponse
--
--         , responseGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpointResponse
--
--         , responsePutVoiceConnectorOrigination $
--             newPutVoiceConnectorOriginationResponse
--
--         , responseCreateAppInstanceUser $
--             newCreateAppInstanceUserResponse
--
--         , responseListAttendeeTags $
--             newListAttendeeTagsResponse
--
--         , responseListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUserResponse
--
--         , responseRedactChannelMessage $
--             newRedactChannelMessageResponse
--
--         , responsePutRetentionSettings $
--             newPutRetentionSettingsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfigurationResponse
--
--         , responseAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroupResponse
--
--         , responsePutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettingsResponse
--
--         , responseGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfigurationResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseDeleteChannelMembership $
--             newDeleteChannelMembershipResponse
--
--         , responsePutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfigurationResponse
--
--         , responseListChannelMemberships $
--             newListChannelMembershipsResponse
--
--         , responseGetGlobalSettings $
--             newGetGlobalSettingsResponse
--
--         , responseDeleteMeeting $
--             newDeleteMeetingResponse
--
--         , responseListMeetings $
--             newListMeetingsResponse
--
--         , responseGetAttendee $
--             newGetAttendeeResponse
--
--         , responseDeleteAccount $
--             newDeleteAccountResponse
--
--         , responseUpdateAccount $
--             newUpdateAccountResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseUpdateBot $
--             newUpdateBotResponse
--
--         , responseListPhoneNumberOrders $
--             newListPhoneNumberOrdersResponse
--
--         , responseSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbersResponse
--
--         , responseCreateAppInstanceAdmin $
--             newCreateAppInstanceAdminResponse
--
--         , responseTagMeeting $
--             newTagMeetingResponse
--
--         , responseListVoiceConnectorGroups $
--             newListVoiceConnectorGroupsResponse
--
--         , responseLogoutUser $
--             newLogoutUserResponse
--
--         , responseListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentialsResponse
--
--         , responseCreateMediaCapturePipeline $
--             newCreateMediaCapturePipelineResponse
--
--         , responseCreateProxySession $
--             newCreateProxySessionResponse
--
--         , responseDeleteEventsConfiguration $
--             newDeleteEventsConfigurationResponse
--
--         , responsePutEventsConfiguration $
--             newPutEventsConfigurationResponse
--
--         , responseGetChannelMessage $
--             newGetChannelMessageResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responsePutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfigurationResponse
--
--         , responseDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUserResponse
--
--         , responseListAppInstanceAdmins $
--             newListAppInstanceAdminsResponse
--
--         , responseDeletePhoneNumber $
--             newDeletePhoneNumberResponse
--
--         , responseUpdatePhoneNumber $
--             newUpdatePhoneNumberResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseCreateChannelModerator $
--             newCreateChannelModeratorResponse
--
--         , responseGetAppInstanceStreamingConfigurations $
--             newGetAppInstanceStreamingConfigurationsResponse
--
--         , responseListAppInstances $
--             newListAppInstancesResponse
--
--         , responseDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUserResponse
--
--         , responseGetPhoneNumber $
--             newGetPhoneNumberResponse
--
--         , responseGetEventsConfiguration $
--             newGetEventsConfigurationResponse
--
--         , responseGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfigurationResponse
--
--         , responseBatchUpdateUser $
--             newBatchUpdateUserResponse
--
--         , responseSendChannelMessage $
--             newSendChannelMessageResponse
--
--         , responseTagAttendee $
--             newTagAttendeeResponse
--
--         , responseUpdateVoiceConnector $
--             newUpdateVoiceConnectorResponse
--
--         , responseDeleteVoiceConnector $
--             newDeleteVoiceConnectorResponse
--
--         , responseGetMediaCapturePipeline $
--             newGetMediaCapturePipelineResponse
--
--         , responseUpdateRoomMembership $
--             newUpdateRoomMembershipResponse
--
--         , responseGetProxySession $
--             newGetProxySessionResponse
--
--         , responseDeleteRoomMembership $
--             newDeleteRoomMembershipResponse
--
--         , responseDescribeAppInstanceUser $
--             newDescribeAppInstanceUserResponse
--
--         , responseBatchUnsuspendUser $
--             newBatchUnsuspendUserResponse
--
--         , responseDeleteChannelBan $
--             newDeleteChannelBanResponse
--
--         , responseGetMeeting $
--             newGetMeetingResponse
--
--         , responseRestorePhoneNumber $
--             newRestorePhoneNumberResponse
--
--         , responseGetRetentionSettings $
--             newGetRetentionSettingsResponse
--
--         , responseGetBot $
--             newGetBotResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseUntagAttendee $
--             newUntagAttendeeResponse
--
--         , responseStartMeetingTranscription $
--             newStartMeetingTranscriptionResponse
--
--         , responseListChannelBans $
--             newListChannelBansResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseBatchSuspendUser $
--             newBatchSuspendUserResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responseDescribeChannelModerator $
--             newDescribeChannelModeratorResponse
--
--         , responseAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnectorResponse
--
--         , responseGetPhoneNumberOrder $
--             newGetPhoneNumberOrderResponse
--
--         , responseGetSipRule $
--             newGetSipRuleResponse
--
--         , responseGetUserSettings $
--             newGetUserSettingsResponse
--
--         , responseGetSipMediaApplication $
--             newGetSipMediaApplicationResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseCreateChannelBan $
--             newCreateChannelBanResponse
--
--         , responseListMeetingTags $
--             newListMeetingTagsResponse
--
--         , responseListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUserResponse
--
--         , responseGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOriginationResponse
--
--         , responseBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumberResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse
--
--         , responseUpdateChannelReadMarker $
--             newUpdateChannelReadMarkerResponse
--
--         , responseCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCallResponse
--
--         , responseBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumberResponse
--
--         , responseListSipMediaApplications $
--             newListSipMediaApplicationsResponse
--
--         , responseCreateMeeting $
--             newCreateMeetingResponse
--
--         , responseCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrderResponse
--
--         , responseListSipRules $
--             newListSipRulesResponse
--
--         , responseCreateBot $
--             newCreateBotResponse
--
--         , responseUpdateUserSettings $
--             newUpdateUserSettingsResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseBatchCreateRoomMembership $
--             newBatchCreateRoomMembershipResponse
--
--         , responseDescribeAppInstance $
--             newDescribeAppInstanceResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseCreateChannelMembership $
--             newCreateChannelMembershipResponse
--
--         , responseDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTerminationResponse
--
--         , responseAssociatePhoneNumberWithUser $
--             newAssociatePhoneNumberWithUserResponse
--
--         , responseDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxyResponse
--
--         , responseCreateSipMediaApplication $
--             newCreateSipMediaApplicationResponse
--
--         , responsePutVoiceConnectorProxy $
--             newPutVoiceConnectorProxyResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responsePutVoiceConnectorTermination $
--             newPutVoiceConnectorTerminationResponse
--
--         , responseGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responsePutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentialsResponse
--
--         , responseListAppInstanceUsers $
--             newListAppInstanceUsersResponse
--
--         , responseAssociateSigninDelegateGroupsWithAccount $
--             newAssociateSigninDelegateGroupsWithAccountResponse
--
--         , responseCreateSipRule $
--             newCreateSipRuleResponse
--
--         , responseDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentialsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteAppInstanceUser $
--             newDeleteAppInstanceUserResponse
--
--         , responseUpdateAppInstanceUser $
--             newUpdateAppInstanceUserResponse
--
--         , responseUntagMeeting $
--             newUntagMeetingResponse
--
--         , responseUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroupResponse
--
--         , responseRedactConversationMessage $
--             newRedactConversationMessageResponse
--
--         , responseDeleteChannelModerator $
--             newDeleteChannelModeratorResponse
--
--         , responseDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroupResponse
--
--         , responseDescribeChannelBan $
--             newDescribeChannelBanResponse
--
--         , responseDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipelineResponse
--
--         , responseUpdateProxySession $
--             newUpdateProxySessionResponse
--
--         , responseDeleteProxySession $
--             newDeleteProxySessionResponse
--
--         , responseGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealthResponse
--
--         , responseCreateMeetingDialOut $
--             newCreateMeetingDialOutResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListProxySessions $
--             newListProxySessionsResponse
--
--         , responseListMediaCapturePipelines $
--             newListMediaCapturePipelinesResponse
--
--         , responseUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettingsResponse
--
--         , responseInviteUsers $
--             newInviteUsersResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseListChannelModerators $
--             newListChannelModeratorsResponse
--
--         , responseGetVoiceConnector $
--             newGetVoiceConnectorResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroupResponse
--
--         , responseDeleteAppInstanceStreamingConfigurations $
--             newDeleteAppInstanceStreamingConfigurationsResponse
--
--         , responseListRooms $
--             newListRoomsResponse
--
--         , responseBatchCreateAttendee $
--             newBatchCreateAttendeeResponse
--
--         , responseDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdminResponse
--
--         , responsePutAppInstanceStreamingConfigurations $
--             newPutAppInstanceStreamingConfigurationsResponse
--
--         , responseRegenerateSecurityToken $
--             newRegenerateSecurityTokenResponse
--
--         , responseDeleteChannelMessage $
--             newDeleteChannelMessageResponse
--
--         , responseUpdateChannelMessage $
--             newUpdateChannelMessageResponse
--
--         , responseDeleteAppInstance $
--             newDeleteAppInstanceResponse
--
--         , responseUpdateAppInstance $
--             newUpdateAppInstanceResponse
--
--         , responseCreateVoiceConnector $
--             newCreateVoiceConnectorResponse
--
--         , responseListChannelMessages $
--             newListChannelMessagesResponse
--
--         , responseRedactRoomMessage $
--             newRedactRoomMessageResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseCreateRoomMembership $
--             newCreateRoomMembershipResponse
--
--         , responseBatchCreateChannelMembership $
--             newBatchCreateChannelMembershipResponse
--
--           ]
--     ]

-- Requests

requestDescribeChannelMembership :: DescribeChannelMembership -> TestTree
requestDescribeChannelMembership =
  req
    "DescribeChannelMembership"
    "fixture/DescribeChannelMembership.yaml"

requestCreateAppInstance :: CreateAppInstance -> TestTree
requestCreateAppInstance =
  req
    "CreateAppInstance"
    "fixture/CreateAppInstance.yaml"

requestGetVoiceConnectorGroup :: GetVoiceConnectorGroup -> TestTree
requestGetVoiceConnectorGroup =
  req
    "GetVoiceConnectorGroup"
    "fixture/GetVoiceConnectorGroup.yaml"

requestListVoiceConnectors :: ListVoiceConnectors -> TestTree
requestListVoiceConnectors =
  req
    "ListVoiceConnectors"
    "fixture/ListVoiceConnectors.yaml"

requestListRoomMemberships :: ListRoomMemberships -> TestTree
requestListRoomMemberships =
  req
    "ListRoomMemberships"
    "fixture/ListRoomMemberships.yaml"

requestGetPhoneNumberSettings :: GetPhoneNumberSettings -> TestTree
requestGetPhoneNumberSettings =
  req
    "GetPhoneNumberSettings"
    "fixture/GetPhoneNumberSettings.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestListAttendees :: ListAttendees -> TestTree
requestListAttendees =
  req
    "ListAttendees"
    "fixture/ListAttendees.yaml"

requestPutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfiguration -> TestTree
requestPutVoiceConnectorLoggingConfiguration =
  req
    "PutVoiceConnectorLoggingConfiguration"
    "fixture/PutVoiceConnectorLoggingConfiguration.yaml"

requestGetVoiceConnectorTermination :: GetVoiceConnectorTermination -> TestTree
requestGetVoiceConnectorTermination =
  req
    "GetVoiceConnectorTermination"
    "fixture/GetVoiceConnectorTermination.yaml"

requestDeleteAttendee :: DeleteAttendee -> TestTree
requestDeleteAttendee =
  req
    "DeleteAttendee"
    "fixture/DeleteAttendee.yaml"

requestGetVoiceConnectorProxy :: GetVoiceConnectorProxy -> TestTree
requestGetVoiceConnectorProxy =
  req
    "GetVoiceConnectorProxy"
    "fixture/GetVoiceConnectorProxy.yaml"

requestDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestDeleteVoiceConnectorEmergencyCallingConfiguration =
  req
    "DeleteVoiceConnectorEmergencyCallingConfiguration"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfiguration.yaml"

requestGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfiguration -> TestTree
requestGetVoiceConnectorStreamingConfiguration =
  req
    "GetVoiceConnectorStreamingConfiguration"
    "fixture/GetVoiceConnectorStreamingConfiguration.yaml"

requestUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCall -> TestTree
requestUpdateSipMediaApplicationCall =
  req
    "UpdateSipMediaApplicationCall"
    "fixture/UpdateSipMediaApplicationCall.yaml"

requestStopMeetingTranscription :: StopMeetingTranscription -> TestTree
requestStopMeetingTranscription =
  req
    "StopMeetingTranscription"
    "fixture/StopMeetingTranscription.yaml"

requestGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettings -> TestTree
requestGetAppInstanceRetentionSettings =
  req
    "GetAppInstanceRetentionSettings"
    "fixture/GetAppInstanceRetentionSettings.yaml"

requestPutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestPutVoiceConnectorEmergencyCallingConfiguration =
  req
    "PutVoiceConnectorEmergencyCallingConfiguration"
    "fixture/PutVoiceConnectorEmergencyCallingConfiguration.yaml"

requestCreateMeetingWithAttendees :: CreateMeetingWithAttendees -> TestTree
requestCreateMeetingWithAttendees =
  req
    "CreateMeetingWithAttendees"
    "fixture/CreateMeetingWithAttendees.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDisassociatePhoneNumberFromUser :: DisassociatePhoneNumberFromUser -> TestTree
requestDisassociatePhoneNumberFromUser =
  req
    "DisassociatePhoneNumberFromUser"
    "fixture/DisassociatePhoneNumberFromUser.yaml"

requestDisassociateSigninDelegateGroupsFromAccount :: DisassociateSigninDelegateGroupsFromAccount -> TestTree
requestDisassociateSigninDelegateGroupsFromAccount =
  req
    "DisassociateSigninDelegateGroupsFromAccount"
    "fixture/DisassociateSigninDelegateGroupsFromAccount.yaml"

requestResetPersonalPIN :: ResetPersonalPIN -> TestTree
requestResetPersonalPIN =
  req
    "ResetPersonalPIN"
    "fixture/ResetPersonalPIN.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestDescribeAppInstanceAdmin :: DescribeAppInstanceAdmin -> TestTree
requestDescribeAppInstanceAdmin =
  req
    "DescribeAppInstanceAdmin"
    "fixture/DescribeAppInstanceAdmin.yaml"

requestCreateAttendee :: CreateAttendee -> TestTree
requestCreateAttendee =
  req
    "CreateAttendee"
    "fixture/CreateAttendee.yaml"

requestListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountries -> TestTree
requestListSupportedPhoneNumberCountries =
  req
    "ListSupportedPhoneNumberCountries"
    "fixture/ListSupportedPhoneNumberCountries.yaml"

requestDeleteSipRule :: DeleteSipRule -> TestTree
requestDeleteSipRule =
  req
    "DeleteSipRule"
    "fixture/DeleteSipRule.yaml"

requestUpdateSipRule :: UpdateSipRule -> TestTree
requestUpdateSipRule =
  req
    "UpdateSipRule"
    "fixture/UpdateSipRule.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOrigination -> TestTree
requestDeleteVoiceConnectorOrigination =
  req
    "DeleteVoiceConnectorOrigination"
    "fixture/DeleteVoiceConnectorOrigination.yaml"

requestDeleteSipMediaApplication :: DeleteSipMediaApplication -> TestTree
requestDeleteSipMediaApplication =
  req
    "DeleteSipMediaApplication"
    "fixture/DeleteSipMediaApplication.yaml"

requestUpdateSipMediaApplication :: UpdateSipMediaApplication -> TestTree
requestUpdateSipMediaApplication =
  req
    "UpdateSipMediaApplication"
    "fixture/UpdateSipMediaApplication.yaml"

requestDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnector -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnector =
  req
    "DisassociatePhoneNumbersFromVoiceConnector"
    "fixture/DisassociatePhoneNumbersFromVoiceConnector.yaml"

requestGetMessagingSessionEndpoint :: GetMessagingSessionEndpoint -> TestTree
requestGetMessagingSessionEndpoint =
  req
    "GetMessagingSessionEndpoint"
    "fixture/GetMessagingSessionEndpoint.yaml"

requestPutVoiceConnectorOrigination :: PutVoiceConnectorOrigination -> TestTree
requestPutVoiceConnectorOrigination =
  req
    "PutVoiceConnectorOrigination"
    "fixture/PutVoiceConnectorOrigination.yaml"

requestCreateAppInstanceUser :: CreateAppInstanceUser -> TestTree
requestCreateAppInstanceUser =
  req
    "CreateAppInstanceUser"
    "fixture/CreateAppInstanceUser.yaml"

requestListAttendeeTags :: ListAttendeeTags -> TestTree
requestListAttendeeTags =
  req
    "ListAttendeeTags"
    "fixture/ListAttendeeTags.yaml"

requestListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUser -> TestTree
requestListChannelsModeratedByAppInstanceUser =
  req
    "ListChannelsModeratedByAppInstanceUser"
    "fixture/ListChannelsModeratedByAppInstanceUser.yaml"

requestRedactChannelMessage :: RedactChannelMessage -> TestTree
requestRedactChannelMessage =
  req
    "RedactChannelMessage"
    "fixture/RedactChannelMessage.yaml"

requestPutRetentionSettings :: PutRetentionSettings -> TestTree
requestPutRetentionSettings =
  req
    "PutRetentionSettings"
    "fixture/PutRetentionSettings.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfiguration -> TestTree
requestDeleteVoiceConnectorStreamingConfiguration =
  req
    "DeleteVoiceConnectorStreamingConfiguration"
    "fixture/DeleteVoiceConnectorStreamingConfiguration.yaml"

requestAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroup -> TestTree
requestAssociatePhoneNumbersWithVoiceConnectorGroup =
  req
    "AssociatePhoneNumbersWithVoiceConnectorGroup"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroup.yaml"

requestPutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettings -> TestTree
requestPutAppInstanceRetentionSettings =
  req
    "PutAppInstanceRetentionSettings"
    "fixture/PutAppInstanceRetentionSettings.yaml"

requestGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfiguration -> TestTree
requestGetVoiceConnectorLoggingConfiguration =
  req
    "GetVoiceConnectorLoggingConfiguration"
    "fixture/GetVoiceConnectorLoggingConfiguration.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestDeleteChannelMembership :: DeleteChannelMembership -> TestTree
requestDeleteChannelMembership =
  req
    "DeleteChannelMembership"
    "fixture/DeleteChannelMembership.yaml"

requestPutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfiguration -> TestTree
requestPutVoiceConnectorStreamingConfiguration =
  req
    "PutVoiceConnectorStreamingConfiguration"
    "fixture/PutVoiceConnectorStreamingConfiguration.yaml"

requestListChannelMemberships :: ListChannelMemberships -> TestTree
requestListChannelMemberships =
  req
    "ListChannelMemberships"
    "fixture/ListChannelMemberships.yaml"

requestGetGlobalSettings :: GetGlobalSettings -> TestTree
requestGetGlobalSettings =
  req
    "GetGlobalSettings"
    "fixture/GetGlobalSettings.yaml"

requestDeleteMeeting :: DeleteMeeting -> TestTree
requestDeleteMeeting =
  req
    "DeleteMeeting"
    "fixture/DeleteMeeting.yaml"

requestListMeetings :: ListMeetings -> TestTree
requestListMeetings =
  req
    "ListMeetings"
    "fixture/ListMeetings.yaml"

requestGetAttendee :: GetAttendee -> TestTree
requestGetAttendee =
  req
    "GetAttendee"
    "fixture/GetAttendee.yaml"

requestDeleteAccount :: DeleteAccount -> TestTree
requestDeleteAccount =
  req
    "DeleteAccount"
    "fixture/DeleteAccount.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestUpdateBot :: UpdateBot -> TestTree
requestUpdateBot =
  req
    "UpdateBot"
    "fixture/UpdateBot.yaml"

requestListPhoneNumberOrders :: ListPhoneNumberOrders -> TestTree
requestListPhoneNumberOrders =
  req
    "ListPhoneNumberOrders"
    "fixture/ListPhoneNumberOrders.yaml"

requestSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbers -> TestTree
requestSearchAvailablePhoneNumbers =
  req
    "SearchAvailablePhoneNumbers"
    "fixture/SearchAvailablePhoneNumbers.yaml"

requestCreateAppInstanceAdmin :: CreateAppInstanceAdmin -> TestTree
requestCreateAppInstanceAdmin =
  req
    "CreateAppInstanceAdmin"
    "fixture/CreateAppInstanceAdmin.yaml"

requestTagMeeting :: TagMeeting -> TestTree
requestTagMeeting =
  req
    "TagMeeting"
    "fixture/TagMeeting.yaml"

requestListVoiceConnectorGroups :: ListVoiceConnectorGroups -> TestTree
requestListVoiceConnectorGroups =
  req
    "ListVoiceConnectorGroups"
    "fixture/ListVoiceConnectorGroups.yaml"

requestLogoutUser :: LogoutUser -> TestTree
requestLogoutUser =
  req
    "LogoutUser"
    "fixture/LogoutUser.yaml"

requestListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentials -> TestTree
requestListVoiceConnectorTerminationCredentials =
  req
    "ListVoiceConnectorTerminationCredentials"
    "fixture/ListVoiceConnectorTerminationCredentials.yaml"

requestCreateMediaCapturePipeline :: CreateMediaCapturePipeline -> TestTree
requestCreateMediaCapturePipeline =
  req
    "CreateMediaCapturePipeline"
    "fixture/CreateMediaCapturePipeline.yaml"

requestCreateProxySession :: CreateProxySession -> TestTree
requestCreateProxySession =
  req
    "CreateProxySession"
    "fixture/CreateProxySession.yaml"

requestDeleteEventsConfiguration :: DeleteEventsConfiguration -> TestTree
requestDeleteEventsConfiguration =
  req
    "DeleteEventsConfiguration"
    "fixture/DeleteEventsConfiguration.yaml"

requestPutEventsConfiguration :: PutEventsConfiguration -> TestTree
requestPutEventsConfiguration =
  req
    "PutEventsConfiguration"
    "fixture/PutEventsConfiguration.yaml"

requestGetChannelMessage :: GetChannelMessage -> TestTree
requestGetChannelMessage =
  req
    "GetChannelMessage"
    "fixture/GetChannelMessage.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestPutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfiguration -> TestTree
requestPutSipMediaApplicationLoggingConfiguration =
  req
    "PutSipMediaApplicationLoggingConfiguration"
    "fixture/PutSipMediaApplicationLoggingConfiguration.yaml"

requestDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUser -> TestTree
requestDescribeChannelMembershipForAppInstanceUser =
  req
    "DescribeChannelMembershipForAppInstanceUser"
    "fixture/DescribeChannelMembershipForAppInstanceUser.yaml"

requestListAppInstanceAdmins :: ListAppInstanceAdmins -> TestTree
requestListAppInstanceAdmins =
  req
    "ListAppInstanceAdmins"
    "fixture/ListAppInstanceAdmins.yaml"

requestDeletePhoneNumber :: DeletePhoneNumber -> TestTree
requestDeletePhoneNumber =
  req
    "DeletePhoneNumber"
    "fixture/DeletePhoneNumber.yaml"

requestUpdatePhoneNumber :: UpdatePhoneNumber -> TestTree
requestUpdatePhoneNumber =
  req
    "UpdatePhoneNumber"
    "fixture/UpdatePhoneNumber.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestCreateChannelModerator :: CreateChannelModerator -> TestTree
requestCreateChannelModerator =
  req
    "CreateChannelModerator"
    "fixture/CreateChannelModerator.yaml"

requestGetAppInstanceStreamingConfigurations :: GetAppInstanceStreamingConfigurations -> TestTree
requestGetAppInstanceStreamingConfigurations =
  req
    "GetAppInstanceStreamingConfigurations"
    "fixture/GetAppInstanceStreamingConfigurations.yaml"

requestListAppInstances :: ListAppInstances -> TestTree
requestListAppInstances =
  req
    "ListAppInstances"
    "fixture/ListAppInstances.yaml"

requestDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUser -> TestTree
requestDescribeChannelModeratedByAppInstanceUser =
  req
    "DescribeChannelModeratedByAppInstanceUser"
    "fixture/DescribeChannelModeratedByAppInstanceUser.yaml"

requestGetPhoneNumber :: GetPhoneNumber -> TestTree
requestGetPhoneNumber =
  req
    "GetPhoneNumber"
    "fixture/GetPhoneNumber.yaml"

requestGetEventsConfiguration :: GetEventsConfiguration -> TestTree
requestGetEventsConfiguration =
  req
    "GetEventsConfiguration"
    "fixture/GetEventsConfiguration.yaml"

requestGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfiguration -> TestTree
requestGetSipMediaApplicationLoggingConfiguration =
  req
    "GetSipMediaApplicationLoggingConfiguration"
    "fixture/GetSipMediaApplicationLoggingConfiguration.yaml"

requestBatchUpdateUser :: BatchUpdateUser -> TestTree
requestBatchUpdateUser =
  req
    "BatchUpdateUser"
    "fixture/BatchUpdateUser.yaml"

requestSendChannelMessage :: SendChannelMessage -> TestTree
requestSendChannelMessage =
  req
    "SendChannelMessage"
    "fixture/SendChannelMessage.yaml"

requestTagAttendee :: TagAttendee -> TestTree
requestTagAttendee =
  req
    "TagAttendee"
    "fixture/TagAttendee.yaml"

requestUpdateVoiceConnector :: UpdateVoiceConnector -> TestTree
requestUpdateVoiceConnector =
  req
    "UpdateVoiceConnector"
    "fixture/UpdateVoiceConnector.yaml"

requestDeleteVoiceConnector :: DeleteVoiceConnector -> TestTree
requestDeleteVoiceConnector =
  req
    "DeleteVoiceConnector"
    "fixture/DeleteVoiceConnector.yaml"

requestGetMediaCapturePipeline :: GetMediaCapturePipeline -> TestTree
requestGetMediaCapturePipeline =
  req
    "GetMediaCapturePipeline"
    "fixture/GetMediaCapturePipeline.yaml"

requestUpdateRoomMembership :: UpdateRoomMembership -> TestTree
requestUpdateRoomMembership =
  req
    "UpdateRoomMembership"
    "fixture/UpdateRoomMembership.yaml"

requestGetProxySession :: GetProxySession -> TestTree
requestGetProxySession =
  req
    "GetProxySession"
    "fixture/GetProxySession.yaml"

requestDeleteRoomMembership :: DeleteRoomMembership -> TestTree
requestDeleteRoomMembership =
  req
    "DeleteRoomMembership"
    "fixture/DeleteRoomMembership.yaml"

requestDescribeAppInstanceUser :: DescribeAppInstanceUser -> TestTree
requestDescribeAppInstanceUser =
  req
    "DescribeAppInstanceUser"
    "fixture/DescribeAppInstanceUser.yaml"

requestBatchUnsuspendUser :: BatchUnsuspendUser -> TestTree
requestBatchUnsuspendUser =
  req
    "BatchUnsuspendUser"
    "fixture/BatchUnsuspendUser.yaml"

requestDeleteChannelBan :: DeleteChannelBan -> TestTree
requestDeleteChannelBan =
  req
    "DeleteChannelBan"
    "fixture/DeleteChannelBan.yaml"

requestGetMeeting :: GetMeeting -> TestTree
requestGetMeeting =
  req
    "GetMeeting"
    "fixture/GetMeeting.yaml"

requestRestorePhoneNumber :: RestorePhoneNumber -> TestTree
requestRestorePhoneNumber =
  req
    "RestorePhoneNumber"
    "fixture/RestorePhoneNumber.yaml"

requestGetRetentionSettings :: GetRetentionSettings -> TestTree
requestGetRetentionSettings =
  req
    "GetRetentionSettings"
    "fixture/GetRetentionSettings.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot =
  req
    "GetBot"
    "fixture/GetBot.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestUntagAttendee :: UntagAttendee -> TestTree
requestUntagAttendee =
  req
    "UntagAttendee"
    "fixture/UntagAttendee.yaml"

requestStartMeetingTranscription :: StartMeetingTranscription -> TestTree
requestStartMeetingTranscription =
  req
    "StartMeetingTranscription"
    "fixture/StartMeetingTranscription.yaml"

requestListChannelBans :: ListChannelBans -> TestTree
requestListChannelBans =
  req
    "ListChannelBans"
    "fixture/ListChannelBans.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestBatchSuspendUser :: BatchSuspendUser -> TestTree
requestBatchSuspendUser =
  req
    "BatchSuspendUser"
    "fixture/BatchSuspendUser.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestDescribeChannelModerator :: DescribeChannelModerator -> TestTree
requestDescribeChannelModerator =
  req
    "DescribeChannelModerator"
    "fixture/DescribeChannelModerator.yaml"

requestAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnector -> TestTree
requestAssociatePhoneNumbersWithVoiceConnector =
  req
    "AssociatePhoneNumbersWithVoiceConnector"
    "fixture/AssociatePhoneNumbersWithVoiceConnector.yaml"

requestGetPhoneNumberOrder :: GetPhoneNumberOrder -> TestTree
requestGetPhoneNumberOrder =
  req
    "GetPhoneNumberOrder"
    "fixture/GetPhoneNumberOrder.yaml"

requestGetSipRule :: GetSipRule -> TestTree
requestGetSipRule =
  req
    "GetSipRule"
    "fixture/GetSipRule.yaml"

requestGetUserSettings :: GetUserSettings -> TestTree
requestGetUserSettings =
  req
    "GetUserSettings"
    "fixture/GetUserSettings.yaml"

requestGetSipMediaApplication :: GetSipMediaApplication -> TestTree
requestGetSipMediaApplication =
  req
    "GetSipMediaApplication"
    "fixture/GetSipMediaApplication.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestCreateChannelBan :: CreateChannelBan -> TestTree
requestCreateChannelBan =
  req
    "CreateChannelBan"
    "fixture/CreateChannelBan.yaml"

requestListMeetingTags :: ListMeetingTags -> TestTree
requestListMeetingTags =
  req
    "ListMeetingTags"
    "fixture/ListMeetingTags.yaml"

requestListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUser -> TestTree
requestListChannelMembershipsForAppInstanceUser =
  req
    "ListChannelMembershipsForAppInstanceUser"
    "fixture/ListChannelMembershipsForAppInstanceUser.yaml"

requestGetVoiceConnectorOrigination :: GetVoiceConnectorOrigination -> TestTree
requestGetVoiceConnectorOrigination =
  req
    "GetVoiceConnectorOrigination"
    "fixture/GetVoiceConnectorOrigination.yaml"

requestBatchUpdatePhoneNumber :: BatchUpdatePhoneNumber -> TestTree
requestBatchUpdatePhoneNumber =
  req
    "BatchUpdatePhoneNumber"
    "fixture/BatchUpdatePhoneNumber.yaml"

requestDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroup -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnectorGroup =
  req
    "DisassociatePhoneNumbersFromVoiceConnectorGroup"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroup.yaml"

requestUpdateChannelReadMarker :: UpdateChannelReadMarker -> TestTree
requestUpdateChannelReadMarker =
  req
    "UpdateChannelReadMarker"
    "fixture/UpdateChannelReadMarker.yaml"

requestCreateSipMediaApplicationCall :: CreateSipMediaApplicationCall -> TestTree
requestCreateSipMediaApplicationCall =
  req
    "CreateSipMediaApplicationCall"
    "fixture/CreateSipMediaApplicationCall.yaml"

requestBatchDeletePhoneNumber :: BatchDeletePhoneNumber -> TestTree
requestBatchDeletePhoneNumber =
  req
    "BatchDeletePhoneNumber"
    "fixture/BatchDeletePhoneNumber.yaml"

requestListSipMediaApplications :: ListSipMediaApplications -> TestTree
requestListSipMediaApplications =
  req
    "ListSipMediaApplications"
    "fixture/ListSipMediaApplications.yaml"

requestCreateMeeting :: CreateMeeting -> TestTree
requestCreateMeeting =
  req
    "CreateMeeting"
    "fixture/CreateMeeting.yaml"

requestCreatePhoneNumberOrder :: CreatePhoneNumberOrder -> TestTree
requestCreatePhoneNumberOrder =
  req
    "CreatePhoneNumberOrder"
    "fixture/CreatePhoneNumberOrder.yaml"

requestListSipRules :: ListSipRules -> TestTree
requestListSipRules =
  req
    "ListSipRules"
    "fixture/ListSipRules.yaml"

requestCreateBot :: CreateBot -> TestTree
requestCreateBot =
  req
    "CreateBot"
    "fixture/CreateBot.yaml"

requestUpdateUserSettings :: UpdateUserSettings -> TestTree
requestUpdateUserSettings =
  req
    "UpdateUserSettings"
    "fixture/UpdateUserSettings.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestBatchCreateRoomMembership :: BatchCreateRoomMembership -> TestTree
requestBatchCreateRoomMembership =
  req
    "BatchCreateRoomMembership"
    "fixture/BatchCreateRoomMembership.yaml"

requestDescribeAppInstance :: DescribeAppInstance -> TestTree
requestDescribeAppInstance =
  req
    "DescribeAppInstance"
    "fixture/DescribeAppInstance.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestCreateChannelMembership :: CreateChannelMembership -> TestTree
requestCreateChannelMembership =
  req
    "CreateChannelMembership"
    "fixture/CreateChannelMembership.yaml"

requestDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTermination -> TestTree
requestDeleteVoiceConnectorTermination =
  req
    "DeleteVoiceConnectorTermination"
    "fixture/DeleteVoiceConnectorTermination.yaml"

requestAssociatePhoneNumberWithUser :: AssociatePhoneNumberWithUser -> TestTree
requestAssociatePhoneNumberWithUser =
  req
    "AssociatePhoneNumberWithUser"
    "fixture/AssociatePhoneNumberWithUser.yaml"

requestDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxy -> TestTree
requestDeleteVoiceConnectorProxy =
  req
    "DeleteVoiceConnectorProxy"
    "fixture/DeleteVoiceConnectorProxy.yaml"

requestCreateSipMediaApplication :: CreateSipMediaApplication -> TestTree
requestCreateSipMediaApplication =
  req
    "CreateSipMediaApplication"
    "fixture/CreateSipMediaApplication.yaml"

requestPutVoiceConnectorProxy :: PutVoiceConnectorProxy -> TestTree
requestPutVoiceConnectorProxy =
  req
    "PutVoiceConnectorProxy"
    "fixture/PutVoiceConnectorProxy.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestPutVoiceConnectorTermination :: PutVoiceConnectorTermination -> TestTree
requestPutVoiceConnectorTermination =
  req
    "PutVoiceConnectorTermination"
    "fixture/PutVoiceConnectorTermination.yaml"

requestGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestGetVoiceConnectorEmergencyCallingConfiguration =
  req
    "GetVoiceConnectorEmergencyCallingConfiguration"
    "fixture/GetVoiceConnectorEmergencyCallingConfiguration.yaml"

requestPutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentials -> TestTree
requestPutVoiceConnectorTerminationCredentials =
  req
    "PutVoiceConnectorTerminationCredentials"
    "fixture/PutVoiceConnectorTerminationCredentials.yaml"

requestListAppInstanceUsers :: ListAppInstanceUsers -> TestTree
requestListAppInstanceUsers =
  req
    "ListAppInstanceUsers"
    "fixture/ListAppInstanceUsers.yaml"

requestAssociateSigninDelegateGroupsWithAccount :: AssociateSigninDelegateGroupsWithAccount -> TestTree
requestAssociateSigninDelegateGroupsWithAccount =
  req
    "AssociateSigninDelegateGroupsWithAccount"
    "fixture/AssociateSigninDelegateGroupsWithAccount.yaml"

requestCreateSipRule :: CreateSipRule -> TestTree
requestCreateSipRule =
  req
    "CreateSipRule"
    "fixture/CreateSipRule.yaml"

requestDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentials -> TestTree
requestDeleteVoiceConnectorTerminationCredentials =
  req
    "DeleteVoiceConnectorTerminationCredentials"
    "fixture/DeleteVoiceConnectorTerminationCredentials.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteAppInstanceUser :: DeleteAppInstanceUser -> TestTree
requestDeleteAppInstanceUser =
  req
    "DeleteAppInstanceUser"
    "fixture/DeleteAppInstanceUser.yaml"

requestUpdateAppInstanceUser :: UpdateAppInstanceUser -> TestTree
requestUpdateAppInstanceUser =
  req
    "UpdateAppInstanceUser"
    "fixture/UpdateAppInstanceUser.yaml"

requestUntagMeeting :: UntagMeeting -> TestTree
requestUntagMeeting =
  req
    "UntagMeeting"
    "fixture/UntagMeeting.yaml"

requestUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroup -> TestTree
requestUpdateVoiceConnectorGroup =
  req
    "UpdateVoiceConnectorGroup"
    "fixture/UpdateVoiceConnectorGroup.yaml"

requestRedactConversationMessage :: RedactConversationMessage -> TestTree
requestRedactConversationMessage =
  req
    "RedactConversationMessage"
    "fixture/RedactConversationMessage.yaml"

requestDeleteChannelModerator :: DeleteChannelModerator -> TestTree
requestDeleteChannelModerator =
  req
    "DeleteChannelModerator"
    "fixture/DeleteChannelModerator.yaml"

requestDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroup -> TestTree
requestDeleteVoiceConnectorGroup =
  req
    "DeleteVoiceConnectorGroup"
    "fixture/DeleteVoiceConnectorGroup.yaml"

requestDescribeChannelBan :: DescribeChannelBan -> TestTree
requestDescribeChannelBan =
  req
    "DescribeChannelBan"
    "fixture/DescribeChannelBan.yaml"

requestDeleteMediaCapturePipeline :: DeleteMediaCapturePipeline -> TestTree
requestDeleteMediaCapturePipeline =
  req
    "DeleteMediaCapturePipeline"
    "fixture/DeleteMediaCapturePipeline.yaml"

requestUpdateProxySession :: UpdateProxySession -> TestTree
requestUpdateProxySession =
  req
    "UpdateProxySession"
    "fixture/UpdateProxySession.yaml"

requestDeleteProxySession :: DeleteProxySession -> TestTree
requestDeleteProxySession =
  req
    "DeleteProxySession"
    "fixture/DeleteProxySession.yaml"

requestGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealth -> TestTree
requestGetVoiceConnectorTerminationHealth =
  req
    "GetVoiceConnectorTerminationHealth"
    "fixture/GetVoiceConnectorTerminationHealth.yaml"

requestCreateMeetingDialOut :: CreateMeetingDialOut -> TestTree
requestCreateMeetingDialOut =
  req
    "CreateMeetingDialOut"
    "fixture/CreateMeetingDialOut.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListProxySessions :: ListProxySessions -> TestTree
requestListProxySessions =
  req
    "ListProxySessions"
    "fixture/ListProxySessions.yaml"

requestListMediaCapturePipelines :: ListMediaCapturePipelines -> TestTree
requestListMediaCapturePipelines =
  req
    "ListMediaCapturePipelines"
    "fixture/ListMediaCapturePipelines.yaml"

requestUpdatePhoneNumberSettings :: UpdatePhoneNumberSettings -> TestTree
requestUpdatePhoneNumberSettings =
  req
    "UpdatePhoneNumberSettings"
    "fixture/UpdatePhoneNumberSettings.yaml"

requestInviteUsers :: InviteUsers -> TestTree
requestInviteUsers =
  req
    "InviteUsers"
    "fixture/InviteUsers.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestListChannelModerators :: ListChannelModerators -> TestTree
requestListChannelModerators =
  req
    "ListChannelModerators"
    "fixture/ListChannelModerators.yaml"

requestGetVoiceConnector :: GetVoiceConnector -> TestTree
requestGetVoiceConnector =
  req
    "GetVoiceConnector"
    "fixture/GetVoiceConnector.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestCreateVoiceConnectorGroup :: CreateVoiceConnectorGroup -> TestTree
requestCreateVoiceConnectorGroup =
  req
    "CreateVoiceConnectorGroup"
    "fixture/CreateVoiceConnectorGroup.yaml"

requestDeleteAppInstanceStreamingConfigurations :: DeleteAppInstanceStreamingConfigurations -> TestTree
requestDeleteAppInstanceStreamingConfigurations =
  req
    "DeleteAppInstanceStreamingConfigurations"
    "fixture/DeleteAppInstanceStreamingConfigurations.yaml"

requestListRooms :: ListRooms -> TestTree
requestListRooms =
  req
    "ListRooms"
    "fixture/ListRooms.yaml"

requestBatchCreateAttendee :: BatchCreateAttendee -> TestTree
requestBatchCreateAttendee =
  req
    "BatchCreateAttendee"
    "fixture/BatchCreateAttendee.yaml"

requestDeleteAppInstanceAdmin :: DeleteAppInstanceAdmin -> TestTree
requestDeleteAppInstanceAdmin =
  req
    "DeleteAppInstanceAdmin"
    "fixture/DeleteAppInstanceAdmin.yaml"

requestPutAppInstanceStreamingConfigurations :: PutAppInstanceStreamingConfigurations -> TestTree
requestPutAppInstanceStreamingConfigurations =
  req
    "PutAppInstanceStreamingConfigurations"
    "fixture/PutAppInstanceStreamingConfigurations.yaml"

requestRegenerateSecurityToken :: RegenerateSecurityToken -> TestTree
requestRegenerateSecurityToken =
  req
    "RegenerateSecurityToken"
    "fixture/RegenerateSecurityToken.yaml"

requestDeleteChannelMessage :: DeleteChannelMessage -> TestTree
requestDeleteChannelMessage =
  req
    "DeleteChannelMessage"
    "fixture/DeleteChannelMessage.yaml"

requestUpdateChannelMessage :: UpdateChannelMessage -> TestTree
requestUpdateChannelMessage =
  req
    "UpdateChannelMessage"
    "fixture/UpdateChannelMessage.yaml"

requestDeleteAppInstance :: DeleteAppInstance -> TestTree
requestDeleteAppInstance =
  req
    "DeleteAppInstance"
    "fixture/DeleteAppInstance.yaml"

requestUpdateAppInstance :: UpdateAppInstance -> TestTree
requestUpdateAppInstance =
  req
    "UpdateAppInstance"
    "fixture/UpdateAppInstance.yaml"

requestCreateVoiceConnector :: CreateVoiceConnector -> TestTree
requestCreateVoiceConnector =
  req
    "CreateVoiceConnector"
    "fixture/CreateVoiceConnector.yaml"

requestListChannelMessages :: ListChannelMessages -> TestTree
requestListChannelMessages =
  req
    "ListChannelMessages"
    "fixture/ListChannelMessages.yaml"

requestRedactRoomMessage :: RedactRoomMessage -> TestTree
requestRedactRoomMessage =
  req
    "RedactRoomMessage"
    "fixture/RedactRoomMessage.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestCreateRoomMembership :: CreateRoomMembership -> TestTree
requestCreateRoomMembership =
  req
    "CreateRoomMembership"
    "fixture/CreateRoomMembership.yaml"

requestBatchCreateChannelMembership :: BatchCreateChannelMembership -> TestTree
requestBatchCreateChannelMembership =
  req
    "BatchCreateChannelMembership"
    "fixture/BatchCreateChannelMembership.yaml"

-- Responses

responseDescribeChannelMembership :: DescribeChannelMembershipResponse -> TestTree
responseDescribeChannelMembership =
  res
    "DescribeChannelMembershipResponse"
    "fixture/DescribeChannelMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannelMembership)

responseCreateAppInstance :: CreateAppInstanceResponse -> TestTree
responseCreateAppInstance =
  res
    "CreateAppInstanceResponse"
    "fixture/CreateAppInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppInstance)

responseGetVoiceConnectorGroup :: GetVoiceConnectorGroupResponse -> TestTree
responseGetVoiceConnectorGroup =
  res
    "GetVoiceConnectorGroupResponse"
    "fixture/GetVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorGroup)

responseListVoiceConnectors :: ListVoiceConnectorsResponse -> TestTree
responseListVoiceConnectors =
  res
    "ListVoiceConnectorsResponse"
    "fixture/ListVoiceConnectorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVoiceConnectors)

responseListRoomMemberships :: ListRoomMembershipsResponse -> TestTree
responseListRoomMemberships =
  res
    "ListRoomMembershipsResponse"
    "fixture/ListRoomMembershipsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoomMemberships)

responseGetPhoneNumberSettings :: GetPhoneNumberSettingsResponse -> TestTree
responseGetPhoneNumberSettings =
  res
    "GetPhoneNumberSettingsResponse"
    "fixture/GetPhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetPhoneNumberSettings)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalSettings)

responseListAttendees :: ListAttendeesResponse -> TestTree
responseListAttendees =
  res
    "ListAttendeesResponse"
    "fixture/ListAttendeesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttendees)

responsePutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfigurationResponse -> TestTree
responsePutVoiceConnectorLoggingConfiguration =
  res
    "PutVoiceConnectorLoggingConfigurationResponse"
    "fixture/PutVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorLoggingConfiguration)

responseGetVoiceConnectorTermination :: GetVoiceConnectorTerminationResponse -> TestTree
responseGetVoiceConnectorTermination =
  res
    "GetVoiceConnectorTerminationResponse"
    "fixture/GetVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorTermination)

responseDeleteAttendee :: DeleteAttendeeResponse -> TestTree
responseDeleteAttendee =
  res
    "DeleteAttendeeResponse"
    "fixture/DeleteAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAttendee)

responseGetVoiceConnectorProxy :: GetVoiceConnectorProxyResponse -> TestTree
responseGetVoiceConnectorProxy =
  res
    "GetVoiceConnectorProxyResponse"
    "fixture/GetVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorProxy)

responseDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorEmergencyCallingConfiguration =
  res
    "DeleteVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorEmergencyCallingConfiguration)

responseGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfigurationResponse -> TestTree
responseGetVoiceConnectorStreamingConfiguration =
  res
    "GetVoiceConnectorStreamingConfigurationResponse"
    "fixture/GetVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorStreamingConfiguration)

responseUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCallResponse -> TestTree
responseUpdateSipMediaApplicationCall =
  res
    "UpdateSipMediaApplicationCallResponse"
    "fixture/UpdateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSipMediaApplicationCall)

responseStopMeetingTranscription :: StopMeetingTranscriptionResponse -> TestTree
responseStopMeetingTranscription =
  res
    "StopMeetingTranscriptionResponse"
    "fixture/StopMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy StopMeetingTranscription)

responseGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettingsResponse -> TestTree
responseGetAppInstanceRetentionSettings =
  res
    "GetAppInstanceRetentionSettingsResponse"
    "fixture/GetAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppInstanceRetentionSettings)

responsePutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responsePutVoiceConnectorEmergencyCallingConfiguration =
  res
    "PutVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/PutVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorEmergencyCallingConfiguration)

responseCreateMeetingWithAttendees :: CreateMeetingWithAttendeesResponse -> TestTree
responseCreateMeetingWithAttendees =
  res
    "CreateMeetingWithAttendeesResponse"
    "fixture/CreateMeetingWithAttendeesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMeetingWithAttendees)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannels)

responseDisassociatePhoneNumberFromUser :: DisassociatePhoneNumberFromUserResponse -> TestTree
responseDisassociatePhoneNumberFromUser =
  res
    "DisassociatePhoneNumberFromUserResponse"
    "fixture/DisassociatePhoneNumberFromUserResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociatePhoneNumberFromUser)

responseDisassociateSigninDelegateGroupsFromAccount :: DisassociateSigninDelegateGroupsFromAccountResponse -> TestTree
responseDisassociateSigninDelegateGroupsFromAccount =
  res
    "DisassociateSigninDelegateGroupsFromAccountResponse"
    "fixture/DisassociateSigninDelegateGroupsFromAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateSigninDelegateGroupsFromAccount)

responseResetPersonalPIN :: ResetPersonalPINResponse -> TestTree
responseResetPersonalPIN =
  res
    "ResetPersonalPINResponse"
    "fixture/ResetPersonalPINResponse.proto"
    defaultService
    (Proxy :: Proxy ResetPersonalPIN)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannel)

responseDescribeAppInstanceAdmin :: DescribeAppInstanceAdminResponse -> TestTree
responseDescribeAppInstanceAdmin =
  res
    "DescribeAppInstanceAdminResponse"
    "fixture/DescribeAppInstanceAdminResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAppInstanceAdmin)

responseCreateAttendee :: CreateAttendeeResponse -> TestTree
responseCreateAttendee =
  res
    "CreateAttendeeResponse"
    "fixture/CreateAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAttendee)

responseListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountriesResponse -> TestTree
responseListSupportedPhoneNumberCountries =
  res
    "ListSupportedPhoneNumberCountriesResponse"
    "fixture/ListSupportedPhoneNumberCountriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSupportedPhoneNumberCountries)

responseDeleteSipRule :: DeleteSipRuleResponse -> TestTree
responseDeleteSipRule =
  res
    "DeleteSipRuleResponse"
    "fixture/DeleteSipRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSipRule)

responseUpdateSipRule :: UpdateSipRuleResponse -> TestTree
responseUpdateSipRule =
  res
    "UpdateSipRuleResponse"
    "fixture/UpdateSipRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSipRule)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccountSettings)

responseDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOriginationResponse -> TestTree
responseDeleteVoiceConnectorOrigination =
  res
    "DeleteVoiceConnectorOriginationResponse"
    "fixture/DeleteVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorOrigination)

responseDeleteSipMediaApplication :: DeleteSipMediaApplicationResponse -> TestTree
responseDeleteSipMediaApplication =
  res
    "DeleteSipMediaApplicationResponse"
    "fixture/DeleteSipMediaApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSipMediaApplication)

responseUpdateSipMediaApplication :: UpdateSipMediaApplicationResponse -> TestTree
responseUpdateSipMediaApplication =
  res
    "UpdateSipMediaApplicationResponse"
    "fixture/UpdateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSipMediaApplication)

responseDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnectorResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnector =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociatePhoneNumbersFromVoiceConnector)

responseGetMessagingSessionEndpoint :: GetMessagingSessionEndpointResponse -> TestTree
responseGetMessagingSessionEndpoint =
  res
    "GetMessagingSessionEndpointResponse"
    "fixture/GetMessagingSessionEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetMessagingSessionEndpoint)

responsePutVoiceConnectorOrigination :: PutVoiceConnectorOriginationResponse -> TestTree
responsePutVoiceConnectorOrigination =
  res
    "PutVoiceConnectorOriginationResponse"
    "fixture/PutVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorOrigination)

responseCreateAppInstanceUser :: CreateAppInstanceUserResponse -> TestTree
responseCreateAppInstanceUser =
  res
    "CreateAppInstanceUserResponse"
    "fixture/CreateAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppInstanceUser)

responseListAttendeeTags :: ListAttendeeTagsResponse -> TestTree
responseListAttendeeTags =
  res
    "ListAttendeeTagsResponse"
    "fixture/ListAttendeeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttendeeTags)

responseListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUserResponse -> TestTree
responseListChannelsModeratedByAppInstanceUser =
  res
    "ListChannelsModeratedByAppInstanceUserResponse"
    "fixture/ListChannelsModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelsModeratedByAppInstanceUser)

responseRedactChannelMessage :: RedactChannelMessageResponse -> TestTree
responseRedactChannelMessage =
  res
    "RedactChannelMessageResponse"
    "fixture/RedactChannelMessageResponse.proto"
    defaultService
    (Proxy :: Proxy RedactChannelMessage)

responsePutRetentionSettings :: PutRetentionSettingsResponse -> TestTree
responsePutRetentionSettings =
  res
    "PutRetentionSettingsResponse"
    "fixture/PutRetentionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionSettings)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorStreamingConfiguration =
  res
    "DeleteVoiceConnectorStreamingConfigurationResponse"
    "fixture/DeleteVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorStreamingConfiguration)

responseAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroupResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnectorGroup =
  res
    "AssociatePhoneNumbersWithVoiceConnectorGroupResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePhoneNumbersWithVoiceConnectorGroup)

responsePutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettingsResponse -> TestTree
responsePutAppInstanceRetentionSettings =
  res
    "PutAppInstanceRetentionSettingsResponse"
    "fixture/PutAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppInstanceRetentionSettings)

responseGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfigurationResponse -> TestTree
responseGetVoiceConnectorLoggingConfiguration =
  res
    "GetVoiceConnectorLoggingConfigurationResponse"
    "fixture/GetVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorLoggingConfiguration)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBots)

responseDeleteChannelMembership :: DeleteChannelMembershipResponse -> TestTree
responseDeleteChannelMembership =
  res
    "DeleteChannelMembershipResponse"
    "fixture/DeleteChannelMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannelMembership)

responsePutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfigurationResponse -> TestTree
responsePutVoiceConnectorStreamingConfiguration =
  res
    "PutVoiceConnectorStreamingConfigurationResponse"
    "fixture/PutVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorStreamingConfiguration)

responseListChannelMemberships :: ListChannelMembershipsResponse -> TestTree
responseListChannelMemberships =
  res
    "ListChannelMembershipsResponse"
    "fixture/ListChannelMembershipsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelMemberships)

responseGetGlobalSettings :: GetGlobalSettingsResponse -> TestTree
responseGetGlobalSettings =
  res
    "GetGlobalSettingsResponse"
    "fixture/GetGlobalSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetGlobalSettings)

responseDeleteMeeting :: DeleteMeetingResponse -> TestTree
responseDeleteMeeting =
  res
    "DeleteMeetingResponse"
    "fixture/DeleteMeetingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMeeting)

responseListMeetings :: ListMeetingsResponse -> TestTree
responseListMeetings =
  res
    "ListMeetingsResponse"
    "fixture/ListMeetingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMeetings)

responseGetAttendee :: GetAttendeeResponse -> TestTree
responseGetAttendee =
  res
    "GetAttendeeResponse"
    "fixture/GetAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy GetAttendee)

responseDeleteAccount :: DeleteAccountResponse -> TestTree
responseDeleteAccount =
  res
    "DeleteAccountResponse"
    "fixture/DeleteAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccount)

responseUpdateAccount :: UpdateAccountResponse -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccount)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccounts)

responseUpdateBot :: UpdateBotResponse -> TestTree
responseUpdateBot =
  res
    "UpdateBotResponse"
    "fixture/UpdateBotResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBot)

responseListPhoneNumberOrders :: ListPhoneNumberOrdersResponse -> TestTree
responseListPhoneNumberOrders =
  res
    "ListPhoneNumberOrdersResponse"
    "fixture/ListPhoneNumberOrdersResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumberOrders)

responseSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbersResponse -> TestTree
responseSearchAvailablePhoneNumbers =
  res
    "SearchAvailablePhoneNumbersResponse"
    "fixture/SearchAvailablePhoneNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy SearchAvailablePhoneNumbers)

responseCreateAppInstanceAdmin :: CreateAppInstanceAdminResponse -> TestTree
responseCreateAppInstanceAdmin =
  res
    "CreateAppInstanceAdminResponse"
    "fixture/CreateAppInstanceAdminResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppInstanceAdmin)

responseTagMeeting :: TagMeetingResponse -> TestTree
responseTagMeeting =
  res
    "TagMeetingResponse"
    "fixture/TagMeetingResponse.proto"
    defaultService
    (Proxy :: Proxy TagMeeting)

responseListVoiceConnectorGroups :: ListVoiceConnectorGroupsResponse -> TestTree
responseListVoiceConnectorGroups =
  res
    "ListVoiceConnectorGroupsResponse"
    "fixture/ListVoiceConnectorGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVoiceConnectorGroups)

responseLogoutUser :: LogoutUserResponse -> TestTree
responseLogoutUser =
  res
    "LogoutUserResponse"
    "fixture/LogoutUserResponse.proto"
    defaultService
    (Proxy :: Proxy LogoutUser)

responseListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentialsResponse -> TestTree
responseListVoiceConnectorTerminationCredentials =
  res
    "ListVoiceConnectorTerminationCredentialsResponse"
    "fixture/ListVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVoiceConnectorTerminationCredentials)

responseCreateMediaCapturePipeline :: CreateMediaCapturePipelineResponse -> TestTree
responseCreateMediaCapturePipeline =
  res
    "CreateMediaCapturePipelineResponse"
    "fixture/CreateMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMediaCapturePipeline)

responseCreateProxySession :: CreateProxySessionResponse -> TestTree
responseCreateProxySession =
  res
    "CreateProxySessionResponse"
    "fixture/CreateProxySessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProxySession)

responseDeleteEventsConfiguration :: DeleteEventsConfigurationResponse -> TestTree
responseDeleteEventsConfiguration =
  res
    "DeleteEventsConfigurationResponse"
    "fixture/DeleteEventsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventsConfiguration)

responsePutEventsConfiguration :: PutEventsConfigurationResponse -> TestTree
responsePutEventsConfiguration =
  res
    "PutEventsConfigurationResponse"
    "fixture/PutEventsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutEventsConfiguration)

responseGetChannelMessage :: GetChannelMessageResponse -> TestTree
responseGetChannelMessage =
  res
    "GetChannelMessageResponse"
    "fixture/GetChannelMessageResponse.proto"
    defaultService
    (Proxy :: Proxy GetChannelMessage)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoom)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoom)

responsePutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfigurationResponse -> TestTree
responsePutSipMediaApplicationLoggingConfiguration =
  res
    "PutSipMediaApplicationLoggingConfigurationResponse"
    "fixture/PutSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutSipMediaApplicationLoggingConfiguration)

responseDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUserResponse -> TestTree
responseDescribeChannelMembershipForAppInstanceUser =
  res
    "DescribeChannelMembershipForAppInstanceUserResponse"
    "fixture/DescribeChannelMembershipForAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannelMembershipForAppInstanceUser)

responseListAppInstanceAdmins :: ListAppInstanceAdminsResponse -> TestTree
responseListAppInstanceAdmins =
  res
    "ListAppInstanceAdminsResponse"
    "fixture/ListAppInstanceAdminsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppInstanceAdmins)

responseDeletePhoneNumber :: DeletePhoneNumberResponse -> TestTree
responseDeletePhoneNumber =
  res
    "DeletePhoneNumberResponse"
    "fixture/DeletePhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePhoneNumber)

responseUpdatePhoneNumber :: UpdatePhoneNumberResponse -> TestTree
responseUpdatePhoneNumber =
  res
    "UpdatePhoneNumberResponse"
    "fixture/UpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePhoneNumber)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumbers)

responseCreateChannelModerator :: CreateChannelModeratorResponse -> TestTree
responseCreateChannelModerator =
  res
    "CreateChannelModeratorResponse"
    "fixture/CreateChannelModeratorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannelModerator)

responseGetAppInstanceStreamingConfigurations :: GetAppInstanceStreamingConfigurationsResponse -> TestTree
responseGetAppInstanceStreamingConfigurations =
  res
    "GetAppInstanceStreamingConfigurationsResponse"
    "fixture/GetAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppInstanceStreamingConfigurations)

responseListAppInstances :: ListAppInstancesResponse -> TestTree
responseListAppInstances =
  res
    "ListAppInstancesResponse"
    "fixture/ListAppInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppInstances)

responseDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUserResponse -> TestTree
responseDescribeChannelModeratedByAppInstanceUser =
  res
    "DescribeChannelModeratedByAppInstanceUserResponse"
    "fixture/DescribeChannelModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannelModeratedByAppInstanceUser)

responseGetPhoneNumber :: GetPhoneNumberResponse -> TestTree
responseGetPhoneNumber =
  res
    "GetPhoneNumberResponse"
    "fixture/GetPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy GetPhoneNumber)

responseGetEventsConfiguration :: GetEventsConfigurationResponse -> TestTree
responseGetEventsConfiguration =
  res
    "GetEventsConfigurationResponse"
    "fixture/GetEventsConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetEventsConfiguration)

responseGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfigurationResponse -> TestTree
responseGetSipMediaApplicationLoggingConfiguration =
  res
    "GetSipMediaApplicationLoggingConfigurationResponse"
    "fixture/GetSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSipMediaApplicationLoggingConfiguration)

responseBatchUpdateUser :: BatchUpdateUserResponse -> TestTree
responseBatchUpdateUser =
  res
    "BatchUpdateUserResponse"
    "fixture/BatchUpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUpdateUser)

responseSendChannelMessage :: SendChannelMessageResponse -> TestTree
responseSendChannelMessage =
  res
    "SendChannelMessageResponse"
    "fixture/SendChannelMessageResponse.proto"
    defaultService
    (Proxy :: Proxy SendChannelMessage)

responseTagAttendee :: TagAttendeeResponse -> TestTree
responseTagAttendee =
  res
    "TagAttendeeResponse"
    "fixture/TagAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy TagAttendee)

responseUpdateVoiceConnector :: UpdateVoiceConnectorResponse -> TestTree
responseUpdateVoiceConnector =
  res
    "UpdateVoiceConnectorResponse"
    "fixture/UpdateVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVoiceConnector)

responseDeleteVoiceConnector :: DeleteVoiceConnectorResponse -> TestTree
responseDeleteVoiceConnector =
  res
    "DeleteVoiceConnectorResponse"
    "fixture/DeleteVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnector)

responseGetMediaCapturePipeline :: GetMediaCapturePipelineResponse -> TestTree
responseGetMediaCapturePipeline =
  res
    "GetMediaCapturePipelineResponse"
    "fixture/GetMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy GetMediaCapturePipeline)

responseUpdateRoomMembership :: UpdateRoomMembershipResponse -> TestTree
responseUpdateRoomMembership =
  res
    "UpdateRoomMembershipResponse"
    "fixture/UpdateRoomMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRoomMembership)

responseGetProxySession :: GetProxySessionResponse -> TestTree
responseGetProxySession =
  res
    "GetProxySessionResponse"
    "fixture/GetProxySessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetProxySession)

responseDeleteRoomMembership :: DeleteRoomMembershipResponse -> TestTree
responseDeleteRoomMembership =
  res
    "DeleteRoomMembershipResponse"
    "fixture/DeleteRoomMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRoomMembership)

responseDescribeAppInstanceUser :: DescribeAppInstanceUserResponse -> TestTree
responseDescribeAppInstanceUser =
  res
    "DescribeAppInstanceUserResponse"
    "fixture/DescribeAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAppInstanceUser)

responseBatchUnsuspendUser :: BatchUnsuspendUserResponse -> TestTree
responseBatchUnsuspendUser =
  res
    "BatchUnsuspendUserResponse"
    "fixture/BatchUnsuspendUserResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUnsuspendUser)

responseDeleteChannelBan :: DeleteChannelBanResponse -> TestTree
responseDeleteChannelBan =
  res
    "DeleteChannelBanResponse"
    "fixture/DeleteChannelBanResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannelBan)

responseGetMeeting :: GetMeetingResponse -> TestTree
responseGetMeeting =
  res
    "GetMeetingResponse"
    "fixture/GetMeetingResponse.proto"
    defaultService
    (Proxy :: Proxy GetMeeting)

responseRestorePhoneNumber :: RestorePhoneNumberResponse -> TestTree
responseRestorePhoneNumber =
  res
    "RestorePhoneNumberResponse"
    "fixture/RestorePhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy RestorePhoneNumber)

responseGetRetentionSettings :: GetRetentionSettingsResponse -> TestTree
responseGetRetentionSettings =
  res
    "GetRetentionSettingsResponse"
    "fixture/GetRetentionSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRetentionSettings)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    defaultService
    (Proxy :: Proxy GetBot)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetUser)

responseUntagAttendee :: UntagAttendeeResponse -> TestTree
responseUntagAttendee =
  res
    "UntagAttendeeResponse"
    "fixture/UntagAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy UntagAttendee)

responseStartMeetingTranscription :: StartMeetingTranscriptionResponse -> TestTree
responseStartMeetingTranscription =
  res
    "StartMeetingTranscriptionResponse"
    "fixture/StartMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy StartMeetingTranscription)

responseListChannelBans :: ListChannelBansResponse -> TestTree
responseListChannelBans =
  res
    "ListChannelBansResponse"
    "fixture/ListChannelBansResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelBans)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannel)

responseBatchSuspendUser :: BatchSuspendUserResponse -> TestTree
responseBatchSuspendUser =
  res
    "BatchSuspendUserResponse"
    "fixture/BatchSuspendUserResponse.proto"
    defaultService
    (Proxy :: Proxy BatchSuspendUser)

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccount)

responseDescribeChannelModerator :: DescribeChannelModeratorResponse -> TestTree
responseDescribeChannelModerator =
  res
    "DescribeChannelModeratorResponse"
    "fixture/DescribeChannelModeratorResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannelModerator)

responseAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnectorResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnector =
  res
    "AssociatePhoneNumbersWithVoiceConnectorResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePhoneNumbersWithVoiceConnector)

responseGetPhoneNumberOrder :: GetPhoneNumberOrderResponse -> TestTree
responseGetPhoneNumberOrder =
  res
    "GetPhoneNumberOrderResponse"
    "fixture/GetPhoneNumberOrderResponse.proto"
    defaultService
    (Proxy :: Proxy GetPhoneNumberOrder)

responseGetSipRule :: GetSipRuleResponse -> TestTree
responseGetSipRule =
  res
    "GetSipRuleResponse"
    "fixture/GetSipRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetSipRule)

responseGetUserSettings :: GetUserSettingsResponse -> TestTree
responseGetUserSettings =
  res
    "GetUserSettingsResponse"
    "fixture/GetUserSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserSettings)

responseGetSipMediaApplication :: GetSipMediaApplicationResponse -> TestTree
responseGetSipMediaApplication =
  res
    "GetSipMediaApplicationResponse"
    "fixture/GetSipMediaApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSipMediaApplication)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSettings)

responseCreateChannelBan :: CreateChannelBanResponse -> TestTree
responseCreateChannelBan =
  res
    "CreateChannelBanResponse"
    "fixture/CreateChannelBanResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannelBan)

responseListMeetingTags :: ListMeetingTagsResponse -> TestTree
responseListMeetingTags =
  res
    "ListMeetingTagsResponse"
    "fixture/ListMeetingTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMeetingTags)

responseListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUserResponse -> TestTree
responseListChannelMembershipsForAppInstanceUser =
  res
    "ListChannelMembershipsForAppInstanceUserResponse"
    "fixture/ListChannelMembershipsForAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelMembershipsForAppInstanceUser)

responseGetVoiceConnectorOrigination :: GetVoiceConnectorOriginationResponse -> TestTree
responseGetVoiceConnectorOrigination =
  res
    "GetVoiceConnectorOriginationResponse"
    "fixture/GetVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorOrigination)

responseBatchUpdatePhoneNumber :: BatchUpdatePhoneNumberResponse -> TestTree
responseBatchUpdatePhoneNumber =
  res
    "BatchUpdatePhoneNumberResponse"
    "fixture/BatchUpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUpdatePhoneNumber)

responseDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroupResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnectorGroup =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorGroupResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociatePhoneNumbersFromVoiceConnectorGroup)

responseUpdateChannelReadMarker :: UpdateChannelReadMarkerResponse -> TestTree
responseUpdateChannelReadMarker =
  res
    "UpdateChannelReadMarkerResponse"
    "fixture/UpdateChannelReadMarkerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannelReadMarker)

responseCreateSipMediaApplicationCall :: CreateSipMediaApplicationCallResponse -> TestTree
responseCreateSipMediaApplicationCall =
  res
    "CreateSipMediaApplicationCallResponse"
    "fixture/CreateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSipMediaApplicationCall)

responseBatchDeletePhoneNumber :: BatchDeletePhoneNumberResponse -> TestTree
responseBatchDeletePhoneNumber =
  res
    "BatchDeletePhoneNumberResponse"
    "fixture/BatchDeletePhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeletePhoneNumber)

responseListSipMediaApplications :: ListSipMediaApplicationsResponse -> TestTree
responseListSipMediaApplications =
  res
    "ListSipMediaApplicationsResponse"
    "fixture/ListSipMediaApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSipMediaApplications)

responseCreateMeeting :: CreateMeetingResponse -> TestTree
responseCreateMeeting =
  res
    "CreateMeetingResponse"
    "fixture/CreateMeetingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMeeting)

responseCreatePhoneNumberOrder :: CreatePhoneNumberOrderResponse -> TestTree
responseCreatePhoneNumberOrder =
  res
    "CreatePhoneNumberOrderResponse"
    "fixture/CreatePhoneNumberOrderResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePhoneNumberOrder)

responseListSipRules :: ListSipRulesResponse -> TestTree
responseListSipRules =
  res
    "ListSipRulesResponse"
    "fixture/ListSipRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSipRules)

responseCreateBot :: CreateBotResponse -> TestTree
responseCreateBot =
  res
    "CreateBotResponse"
    "fixture/CreateBotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBot)

responseUpdateUserSettings :: UpdateUserSettingsResponse -> TestTree
responseUpdateUserSettings =
  res
    "UpdateUserSettingsResponse"
    "fixture/UpdateUserSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserSettings)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseBatchCreateRoomMembership :: BatchCreateRoomMembershipResponse -> TestTree
responseBatchCreateRoomMembership =
  res
    "BatchCreateRoomMembershipResponse"
    "fixture/BatchCreateRoomMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCreateRoomMembership)

responseDescribeAppInstance :: DescribeAppInstanceResponse -> TestTree
responseDescribeAppInstance =
  res
    "DescribeAppInstanceResponse"
    "fixture/DescribeAppInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAppInstance)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccount)

responseCreateChannelMembership :: CreateChannelMembershipResponse -> TestTree
responseCreateChannelMembership =
  res
    "CreateChannelMembershipResponse"
    "fixture/CreateChannelMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannelMembership)

responseDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTerminationResponse -> TestTree
responseDeleteVoiceConnectorTermination =
  res
    "DeleteVoiceConnectorTerminationResponse"
    "fixture/DeleteVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorTermination)

responseAssociatePhoneNumberWithUser :: AssociatePhoneNumberWithUserResponse -> TestTree
responseAssociatePhoneNumberWithUser =
  res
    "AssociatePhoneNumberWithUserResponse"
    "fixture/AssociatePhoneNumberWithUserResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePhoneNumberWithUser)

responseDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxyResponse -> TestTree
responseDeleteVoiceConnectorProxy =
  res
    "DeleteVoiceConnectorProxyResponse"
    "fixture/DeleteVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorProxy)

responseCreateSipMediaApplication :: CreateSipMediaApplicationResponse -> TestTree
responseCreateSipMediaApplication =
  res
    "CreateSipMediaApplicationResponse"
    "fixture/CreateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSipMediaApplication)

responsePutVoiceConnectorProxy :: PutVoiceConnectorProxyResponse -> TestTree
responsePutVoiceConnectorProxy =
  res
    "PutVoiceConnectorProxyResponse"
    "fixture/PutVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorProxy)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUser)

responsePutVoiceConnectorTermination :: PutVoiceConnectorTerminationResponse -> TestTree
responsePutVoiceConnectorTermination =
  res
    "PutVoiceConnectorTerminationResponse"
    "fixture/PutVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorTermination)

responseGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseGetVoiceConnectorEmergencyCallingConfiguration =
  res
    "GetVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/GetVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorEmergencyCallingConfiguration)

responsePutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentialsResponse -> TestTree
responsePutVoiceConnectorTerminationCredentials =
  res
    "PutVoiceConnectorTerminationCredentialsResponse"
    "fixture/PutVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy PutVoiceConnectorTerminationCredentials)

responseListAppInstanceUsers :: ListAppInstanceUsersResponse -> TestTree
responseListAppInstanceUsers =
  res
    "ListAppInstanceUsersResponse"
    "fixture/ListAppInstanceUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppInstanceUsers)

responseAssociateSigninDelegateGroupsWithAccount :: AssociateSigninDelegateGroupsWithAccountResponse -> TestTree
responseAssociateSigninDelegateGroupsWithAccount =
  res
    "AssociateSigninDelegateGroupsWithAccountResponse"
    "fixture/AssociateSigninDelegateGroupsWithAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSigninDelegateGroupsWithAccount)

responseCreateSipRule :: CreateSipRuleResponse -> TestTree
responseCreateSipRule =
  res
    "CreateSipRuleResponse"
    "fixture/CreateSipRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSipRule)

responseDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentialsResponse -> TestTree
responseDeleteVoiceConnectorTerminationCredentials =
  res
    "DeleteVoiceConnectorTerminationCredentialsResponse"
    "fixture/DeleteVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorTerminationCredentials)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteAppInstanceUser :: DeleteAppInstanceUserResponse -> TestTree
responseDeleteAppInstanceUser =
  res
    "DeleteAppInstanceUserResponse"
    "fixture/DeleteAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppInstanceUser)

responseUpdateAppInstanceUser :: UpdateAppInstanceUserResponse -> TestTree
responseUpdateAppInstanceUser =
  res
    "UpdateAppInstanceUserResponse"
    "fixture/UpdateAppInstanceUserResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAppInstanceUser)

responseUntagMeeting :: UntagMeetingResponse -> TestTree
responseUntagMeeting =
  res
    "UntagMeetingResponse"
    "fixture/UntagMeetingResponse.proto"
    defaultService
    (Proxy :: Proxy UntagMeeting)

responseUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroupResponse -> TestTree
responseUpdateVoiceConnectorGroup =
  res
    "UpdateVoiceConnectorGroupResponse"
    "fixture/UpdateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVoiceConnectorGroup)

responseRedactConversationMessage :: RedactConversationMessageResponse -> TestTree
responseRedactConversationMessage =
  res
    "RedactConversationMessageResponse"
    "fixture/RedactConversationMessageResponse.proto"
    defaultService
    (Proxy :: Proxy RedactConversationMessage)

responseDeleteChannelModerator :: DeleteChannelModeratorResponse -> TestTree
responseDeleteChannelModerator =
  res
    "DeleteChannelModeratorResponse"
    "fixture/DeleteChannelModeratorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannelModerator)

responseDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroupResponse -> TestTree
responseDeleteVoiceConnectorGroup =
  res
    "DeleteVoiceConnectorGroupResponse"
    "fixture/DeleteVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceConnectorGroup)

responseDescribeChannelBan :: DescribeChannelBanResponse -> TestTree
responseDescribeChannelBan =
  res
    "DescribeChannelBanResponse"
    "fixture/DescribeChannelBanResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannelBan)

responseDeleteMediaCapturePipeline :: DeleteMediaCapturePipelineResponse -> TestTree
responseDeleteMediaCapturePipeline =
  res
    "DeleteMediaCapturePipelineResponse"
    "fixture/DeleteMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMediaCapturePipeline)

responseUpdateProxySession :: UpdateProxySessionResponse -> TestTree
responseUpdateProxySession =
  res
    "UpdateProxySessionResponse"
    "fixture/UpdateProxySessionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProxySession)

responseDeleteProxySession :: DeleteProxySessionResponse -> TestTree
responseDeleteProxySession =
  res
    "DeleteProxySessionResponse"
    "fixture/DeleteProxySessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProxySession)

responseGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealthResponse -> TestTree
responseGetVoiceConnectorTerminationHealth =
  res
    "GetVoiceConnectorTerminationHealthResponse"
    "fixture/GetVoiceConnectorTerminationHealthResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnectorTerminationHealth)

responseCreateMeetingDialOut :: CreateMeetingDialOutResponse -> TestTree
responseCreateMeetingDialOut =
  res
    "CreateMeetingDialOutResponse"
    "fixture/CreateMeetingDialOutResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMeetingDialOut)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListProxySessions :: ListProxySessionsResponse -> TestTree
responseListProxySessions =
  res
    "ListProxySessionsResponse"
    "fixture/ListProxySessionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProxySessions)

responseListMediaCapturePipelines :: ListMediaCapturePipelinesResponse -> TestTree
responseListMediaCapturePipelines =
  res
    "ListMediaCapturePipelinesResponse"
    "fixture/ListMediaCapturePipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMediaCapturePipelines)

responseUpdatePhoneNumberSettings :: UpdatePhoneNumberSettingsResponse -> TestTree
responseUpdatePhoneNumberSettings =
  res
    "UpdatePhoneNumberSettingsResponse"
    "fixture/UpdatePhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePhoneNumberSettings)

responseInviteUsers :: InviteUsersResponse -> TestTree
responseInviteUsers =
  res
    "InviteUsersResponse"
    "fixture/InviteUsersResponse.proto"
    defaultService
    (Proxy :: Proxy InviteUsers)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoom)

responseListChannelModerators :: ListChannelModeratorsResponse -> TestTree
responseListChannelModerators =
  res
    "ListChannelModeratorsResponse"
    "fixture/ListChannelModeratorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelModerators)

responseGetVoiceConnector :: GetVoiceConnectorResponse -> TestTree
responseGetVoiceConnector =
  res
    "GetVoiceConnectorResponse"
    "fixture/GetVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceConnector)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannel)

responseCreateVoiceConnectorGroup :: CreateVoiceConnectorGroupResponse -> TestTree
responseCreateVoiceConnectorGroup =
  res
    "CreateVoiceConnectorGroupResponse"
    "fixture/CreateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVoiceConnectorGroup)

responseDeleteAppInstanceStreamingConfigurations :: DeleteAppInstanceStreamingConfigurationsResponse -> TestTree
responseDeleteAppInstanceStreamingConfigurations =
  res
    "DeleteAppInstanceStreamingConfigurationsResponse"
    "fixture/DeleteAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppInstanceStreamingConfigurations)

responseListRooms :: ListRoomsResponse -> TestTree
responseListRooms =
  res
    "ListRoomsResponse"
    "fixture/ListRoomsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRooms)

responseBatchCreateAttendee :: BatchCreateAttendeeResponse -> TestTree
responseBatchCreateAttendee =
  res
    "BatchCreateAttendeeResponse"
    "fixture/BatchCreateAttendeeResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCreateAttendee)

responseDeleteAppInstanceAdmin :: DeleteAppInstanceAdminResponse -> TestTree
responseDeleteAppInstanceAdmin =
  res
    "DeleteAppInstanceAdminResponse"
    "fixture/DeleteAppInstanceAdminResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppInstanceAdmin)

responsePutAppInstanceStreamingConfigurations :: PutAppInstanceStreamingConfigurationsResponse -> TestTree
responsePutAppInstanceStreamingConfigurations =
  res
    "PutAppInstanceStreamingConfigurationsResponse"
    "fixture/PutAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy PutAppInstanceStreamingConfigurations)

responseRegenerateSecurityToken :: RegenerateSecurityTokenResponse -> TestTree
responseRegenerateSecurityToken =
  res
    "RegenerateSecurityTokenResponse"
    "fixture/RegenerateSecurityTokenResponse.proto"
    defaultService
    (Proxy :: Proxy RegenerateSecurityToken)

responseDeleteChannelMessage :: DeleteChannelMessageResponse -> TestTree
responseDeleteChannelMessage =
  res
    "DeleteChannelMessageResponse"
    "fixture/DeleteChannelMessageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannelMessage)

responseUpdateChannelMessage :: UpdateChannelMessageResponse -> TestTree
responseUpdateChannelMessage =
  res
    "UpdateChannelMessageResponse"
    "fixture/UpdateChannelMessageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannelMessage)

responseDeleteAppInstance :: DeleteAppInstanceResponse -> TestTree
responseDeleteAppInstance =
  res
    "DeleteAppInstanceResponse"
    "fixture/DeleteAppInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppInstance)

responseUpdateAppInstance :: UpdateAppInstanceResponse -> TestTree
responseUpdateAppInstance =
  res
    "UpdateAppInstanceResponse"
    "fixture/UpdateAppInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAppInstance)

responseCreateVoiceConnector :: CreateVoiceConnectorResponse -> TestTree
responseCreateVoiceConnector =
  res
    "CreateVoiceConnectorResponse"
    "fixture/CreateVoiceConnectorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVoiceConnector)

responseListChannelMessages :: ListChannelMessagesResponse -> TestTree
responseListChannelMessages =
  res
    "ListChannelMessagesResponse"
    "fixture/ListChannelMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannelMessages)

responseRedactRoomMessage :: RedactRoomMessageResponse -> TestTree
responseRedactRoomMessage =
  res
    "RedactRoomMessageResponse"
    "fixture/RedactRoomMessageResponse.proto"
    defaultService
    (Proxy :: Proxy RedactRoomMessage)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy :: Proxy GetRoom)

responseCreateRoomMembership :: CreateRoomMembershipResponse -> TestTree
responseCreateRoomMembership =
  res
    "CreateRoomMembershipResponse"
    "fixture/CreateRoomMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRoomMembership)

responseBatchCreateChannelMembership :: BatchCreateChannelMembershipResponse -> TestTree
responseBatchCreateChannelMembership =
  res
    "BatchCreateChannelMembershipResponse"
    "fixture/BatchCreateChannelMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy BatchCreateChannelMembership)

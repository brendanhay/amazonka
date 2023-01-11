{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Chime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Chime where

import Amazonka.Chime
import qualified Data.Proxy as Proxy
import Test.Amazonka.Chime.Internal
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
--         [ requestAssociatePhoneNumberWithUser $
--             newAssociatePhoneNumberWithUser
--
--         , requestAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnector
--
--         , requestAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroup
--
--         , requestAssociateSigninDelegateGroupsWithAccount $
--             newAssociateSigninDelegateGroupsWithAccount
--
--         , requestBatchCreateAttendee $
--             newBatchCreateAttendee
--
--         , requestBatchCreateChannelMembership $
--             newBatchCreateChannelMembership
--
--         , requestBatchCreateRoomMembership $
--             newBatchCreateRoomMembership
--
--         , requestBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumber
--
--         , requestBatchSuspendUser $
--             newBatchSuspendUser
--
--         , requestBatchUnsuspendUser $
--             newBatchUnsuspendUser
--
--         , requestBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumber
--
--         , requestBatchUpdateUser $
--             newBatchUpdateUser
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestCreateAppInstance $
--             newCreateAppInstance
--
--         , requestCreateAppInstanceAdmin $
--             newCreateAppInstanceAdmin
--
--         , requestCreateAppInstanceUser $
--             newCreateAppInstanceUser
--
--         , requestCreateAttendee $
--             newCreateAttendee
--
--         , requestCreateBot $
--             newCreateBot
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestCreateChannelBan $
--             newCreateChannelBan
--
--         , requestCreateChannelMembership $
--             newCreateChannelMembership
--
--         , requestCreateChannelModerator $
--             newCreateChannelModerator
--
--         , requestCreateMediaCapturePipeline $
--             newCreateMediaCapturePipeline
--
--         , requestCreateMeeting $
--             newCreateMeeting
--
--         , requestCreateMeetingDialOut $
--             newCreateMeetingDialOut
--
--         , requestCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendees
--
--         , requestCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrder
--
--         , requestCreateProxySession $
--             newCreateProxySession
--
--         , requestCreateRoom $
--             newCreateRoom
--
--         , requestCreateRoomMembership $
--             newCreateRoomMembership
--
--         , requestCreateSipMediaApplication $
--             newCreateSipMediaApplication
--
--         , requestCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCall
--
--         , requestCreateSipRule $
--             newCreateSipRule
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestCreateVoiceConnector $
--             newCreateVoiceConnector
--
--         , requestCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroup
--
--         , requestDeleteAccount $
--             newDeleteAccount
--
--         , requestDeleteAppInstance $
--             newDeleteAppInstance
--
--         , requestDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdmin
--
--         , requestDeleteAppInstanceStreamingConfigurations $
--             newDeleteAppInstanceStreamingConfigurations
--
--         , requestDeleteAppInstanceUser $
--             newDeleteAppInstanceUser
--
--         , requestDeleteAttendee $
--             newDeleteAttendee
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeleteChannelBan $
--             newDeleteChannelBan
--
--         , requestDeleteChannelMembership $
--             newDeleteChannelMembership
--
--         , requestDeleteChannelMessage $
--             newDeleteChannelMessage
--
--         , requestDeleteChannelModerator $
--             newDeleteChannelModerator
--
--         , requestDeleteEventsConfiguration $
--             newDeleteEventsConfiguration
--
--         , requestDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipeline
--
--         , requestDeleteMeeting $
--             newDeleteMeeting
--
--         , requestDeletePhoneNumber $
--             newDeletePhoneNumber
--
--         , requestDeleteProxySession $
--             newDeleteProxySession
--
--         , requestDeleteRoom $
--             newDeleteRoom
--
--         , requestDeleteRoomMembership $
--             newDeleteRoomMembership
--
--         , requestDeleteSipMediaApplication $
--             newDeleteSipMediaApplication
--
--         , requestDeleteSipRule $
--             newDeleteSipRule
--
--         , requestDeleteVoiceConnector $
--             newDeleteVoiceConnector
--
--         , requestDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfiguration
--
--         , requestDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroup
--
--         , requestDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOrigination
--
--         , requestDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxy
--
--         , requestDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfiguration
--
--         , requestDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTermination
--
--         , requestDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentials
--
--         , requestDescribeAppInstance $
--             newDescribeAppInstance
--
--         , requestDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdmin
--
--         , requestDescribeAppInstanceUser $
--             newDescribeAppInstanceUser
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestDescribeChannelBan $
--             newDescribeChannelBan
--
--         , requestDescribeChannelMembership $
--             newDescribeChannelMembership
--
--         , requestDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUser
--
--         , requestDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUser
--
--         , requestDescribeChannelModerator $
--             newDescribeChannelModerator
--
--         , requestDisassociatePhoneNumberFromUser $
--             newDisassociatePhoneNumberFromUser
--
--         , requestDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnector
--
--         , requestDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroup
--
--         , requestDisassociateSigninDelegateGroupsFromAccount $
--             newDisassociateSigninDelegateGroupsFromAccount
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettings
--
--         , requestGetAppInstanceStreamingConfigurations $
--             newGetAppInstanceStreamingConfigurations
--
--         , requestGetAttendee $
--             newGetAttendee
--
--         , requestGetBot $
--             newGetBot
--
--         , requestGetChannelMessage $
--             newGetChannelMessage
--
--         , requestGetEventsConfiguration $
--             newGetEventsConfiguration
--
--         , requestGetGlobalSettings $
--             newGetGlobalSettings
--
--         , requestGetMediaCapturePipeline $
--             newGetMediaCapturePipeline
--
--         , requestGetMeeting $
--             newGetMeeting
--
--         , requestGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpoint
--
--         , requestGetPhoneNumber $
--             newGetPhoneNumber
--
--         , requestGetPhoneNumberOrder $
--             newGetPhoneNumberOrder
--
--         , requestGetPhoneNumberSettings $
--             newGetPhoneNumberSettings
--
--         , requestGetProxySession $
--             newGetProxySession
--
--         , requestGetRetentionSettings $
--             newGetRetentionSettings
--
--         , requestGetRoom $
--             newGetRoom
--
--         , requestGetSipMediaApplication $
--             newGetSipMediaApplication
--
--         , requestGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfiguration
--
--         , requestGetSipRule $
--             newGetSipRule
--
--         , requestGetUser $
--             newGetUser
--
--         , requestGetUserSettings $
--             newGetUserSettings
--
--         , requestGetVoiceConnector $
--             newGetVoiceConnector
--
--         , requestGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfiguration
--
--         , requestGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroup
--
--         , requestGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfiguration
--
--         , requestGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOrigination
--
--         , requestGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxy
--
--         , requestGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfiguration
--
--         , requestGetVoiceConnectorTermination $
--             newGetVoiceConnectorTermination
--
--         , requestGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealth
--
--         , requestInviteUsers $
--             newInviteUsers
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestListAppInstanceAdmins $
--             newListAppInstanceAdmins
--
--         , requestListAppInstanceUsers $
--             newListAppInstanceUsers
--
--         , requestListAppInstances $
--             newListAppInstances
--
--         , requestListAttendeeTags $
--             newListAttendeeTags
--
--         , requestListAttendees $
--             newListAttendees
--
--         , requestListBots $
--             newListBots
--
--         , requestListChannelBans $
--             newListChannelBans
--
--         , requestListChannelMemberships $
--             newListChannelMemberships
--
--         , requestListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUser
--
--         , requestListChannelMessages $
--             newListChannelMessages
--
--         , requestListChannelModerators $
--             newListChannelModerators
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUser
--
--         , requestListMediaCapturePipelines $
--             newListMediaCapturePipelines
--
--         , requestListMeetingTags $
--             newListMeetingTags
--
--         , requestListMeetings $
--             newListMeetings
--
--         , requestListPhoneNumberOrders $
--             newListPhoneNumberOrders
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestListProxySessions $
--             newListProxySessions
--
--         , requestListRoomMemberships $
--             newListRoomMemberships
--
--         , requestListRooms $
--             newListRooms
--
--         , requestListSipMediaApplications $
--             newListSipMediaApplications
--
--         , requestListSipRules $
--             newListSipRules
--
--         , requestListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountries
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListVoiceConnectorGroups $
--             newListVoiceConnectorGroups
--
--         , requestListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentials
--
--         , requestListVoiceConnectors $
--             newListVoiceConnectors
--
--         , requestLogoutUser $
--             newLogoutUser
--
--         , requestPutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettings
--
--         , requestPutAppInstanceStreamingConfigurations $
--             newPutAppInstanceStreamingConfigurations
--
--         , requestPutEventsConfiguration $
--             newPutEventsConfiguration
--
--         , requestPutRetentionSettings $
--             newPutRetentionSettings
--
--         , requestPutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfiguration
--
--         , requestPutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfiguration
--
--         , requestPutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfiguration
--
--         , requestPutVoiceConnectorOrigination $
--             newPutVoiceConnectorOrigination
--
--         , requestPutVoiceConnectorProxy $
--             newPutVoiceConnectorProxy
--
--         , requestPutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfiguration
--
--         , requestPutVoiceConnectorTermination $
--             newPutVoiceConnectorTermination
--
--         , requestPutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentials
--
--         , requestRedactChannelMessage $
--             newRedactChannelMessage
--
--         , requestRedactConversationMessage $
--             newRedactConversationMessage
--
--         , requestRedactRoomMessage $
--             newRedactRoomMessage
--
--         , requestRegenerateSecurityToken $
--             newRegenerateSecurityToken
--
--         , requestResetPersonalPIN $
--             newResetPersonalPIN
--
--         , requestRestorePhoneNumber $
--             newRestorePhoneNumber
--
--         , requestSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbers
--
--         , requestSendChannelMessage $
--             newSendChannelMessage
--
--         , requestStartMeetingTranscription $
--             newStartMeetingTranscription
--
--         , requestStopMeetingTranscription $
--             newStopMeetingTranscription
--
--         , requestTagAttendee $
--             newTagAttendee
--
--         , requestTagMeeting $
--             newTagMeeting
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagAttendee $
--             newUntagAttendee
--
--         , requestUntagMeeting $
--             newUntagMeeting
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestUpdateAppInstance $
--             newUpdateAppInstance
--
--         , requestUpdateAppInstanceUser $
--             newUpdateAppInstanceUser
--
--         , requestUpdateBot $
--             newUpdateBot
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestUpdateChannelMessage $
--             newUpdateChannelMessage
--
--         , requestUpdateChannelReadMarker $
--             newUpdateChannelReadMarker
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestUpdatePhoneNumber $
--             newUpdatePhoneNumber
--
--         , requestUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettings
--
--         , requestUpdateProxySession $
--             newUpdateProxySession
--
--         , requestUpdateRoom $
--             newUpdateRoom
--
--         , requestUpdateRoomMembership $
--             newUpdateRoomMembership
--
--         , requestUpdateSipMediaApplication $
--             newUpdateSipMediaApplication
--
--         , requestUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCall
--
--         , requestUpdateSipRule $
--             newUpdateSipRule
--
--         , requestUpdateUser $
--             newUpdateUser
--
--         , requestUpdateUserSettings $
--             newUpdateUserSettings
--
--         , requestUpdateVoiceConnector $
--             newUpdateVoiceConnector
--
--         , requestUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroup
--
--         , requestValidateE911Address $
--             newValidateE911Address
--
--           ]

--     , testGroup "response"
--         [ responseAssociatePhoneNumberWithUser $
--             newAssociatePhoneNumberWithUserResponse
--
--         , responseAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnectorResponse
--
--         , responseAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroupResponse
--
--         , responseAssociateSigninDelegateGroupsWithAccount $
--             newAssociateSigninDelegateGroupsWithAccountResponse
--
--         , responseBatchCreateAttendee $
--             newBatchCreateAttendeeResponse
--
--         , responseBatchCreateChannelMembership $
--             newBatchCreateChannelMembershipResponse
--
--         , responseBatchCreateRoomMembership $
--             newBatchCreateRoomMembershipResponse
--
--         , responseBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumberResponse
--
--         , responseBatchSuspendUser $
--             newBatchSuspendUserResponse
--
--         , responseBatchUnsuspendUser $
--             newBatchUnsuspendUserResponse
--
--         , responseBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumberResponse
--
--         , responseBatchUpdateUser $
--             newBatchUpdateUserResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseCreateAppInstance $
--             newCreateAppInstanceResponse
--
--         , responseCreateAppInstanceAdmin $
--             newCreateAppInstanceAdminResponse
--
--         , responseCreateAppInstanceUser $
--             newCreateAppInstanceUserResponse
--
--         , responseCreateAttendee $
--             newCreateAttendeeResponse
--
--         , responseCreateBot $
--             newCreateBotResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateChannelBan $
--             newCreateChannelBanResponse
--
--         , responseCreateChannelMembership $
--             newCreateChannelMembershipResponse
--
--         , responseCreateChannelModerator $
--             newCreateChannelModeratorResponse
--
--         , responseCreateMediaCapturePipeline $
--             newCreateMediaCapturePipelineResponse
--
--         , responseCreateMeeting $
--             newCreateMeetingResponse
--
--         , responseCreateMeetingDialOut $
--             newCreateMeetingDialOutResponse
--
--         , responseCreateMeetingWithAttendees $
--             newCreateMeetingWithAttendeesResponse
--
--         , responseCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrderResponse
--
--         , responseCreateProxySession $
--             newCreateProxySessionResponse
--
--         , responseCreateRoom $
--             newCreateRoomResponse
--
--         , responseCreateRoomMembership $
--             newCreateRoomMembershipResponse
--
--         , responseCreateSipMediaApplication $
--             newCreateSipMediaApplicationResponse
--
--         , responseCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCallResponse
--
--         , responseCreateSipRule $
--             newCreateSipRuleResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseCreateVoiceConnector $
--             newCreateVoiceConnectorResponse
--
--         , responseCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroupResponse
--
--         , responseDeleteAccount $
--             newDeleteAccountResponse
--
--         , responseDeleteAppInstance $
--             newDeleteAppInstanceResponse
--
--         , responseDeleteAppInstanceAdmin $
--             newDeleteAppInstanceAdminResponse
--
--         , responseDeleteAppInstanceStreamingConfigurations $
--             newDeleteAppInstanceStreamingConfigurationsResponse
--
--         , responseDeleteAppInstanceUser $
--             newDeleteAppInstanceUserResponse
--
--         , responseDeleteAttendee $
--             newDeleteAttendeeResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeleteChannelBan $
--             newDeleteChannelBanResponse
--
--         , responseDeleteChannelMembership $
--             newDeleteChannelMembershipResponse
--
--         , responseDeleteChannelMessage $
--             newDeleteChannelMessageResponse
--
--         , responseDeleteChannelModerator $
--             newDeleteChannelModeratorResponse
--
--         , responseDeleteEventsConfiguration $
--             newDeleteEventsConfigurationResponse
--
--         , responseDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipelineResponse
--
--         , responseDeleteMeeting $
--             newDeleteMeetingResponse
--
--         , responseDeletePhoneNumber $
--             newDeletePhoneNumberResponse
--
--         , responseDeleteProxySession $
--             newDeleteProxySessionResponse
--
--         , responseDeleteRoom $
--             newDeleteRoomResponse
--
--         , responseDeleteRoomMembership $
--             newDeleteRoomMembershipResponse
--
--         , responseDeleteSipMediaApplication $
--             newDeleteSipMediaApplicationResponse
--
--         , responseDeleteSipRule $
--             newDeleteSipRuleResponse
--
--         , responseDeleteVoiceConnector $
--             newDeleteVoiceConnectorResponse
--
--         , responseDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroupResponse
--
--         , responseDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOriginationResponse
--
--         , responseDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxyResponse
--
--         , responseDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfigurationResponse
--
--         , responseDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTerminationResponse
--
--         , responseDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentialsResponse
--
--         , responseDescribeAppInstance $
--             newDescribeAppInstanceResponse
--
--         , responseDescribeAppInstanceAdmin $
--             newDescribeAppInstanceAdminResponse
--
--         , responseDescribeAppInstanceUser $
--             newDescribeAppInstanceUserResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseDescribeChannelBan $
--             newDescribeChannelBanResponse
--
--         , responseDescribeChannelMembership $
--             newDescribeChannelMembershipResponse
--
--         , responseDescribeChannelMembershipForAppInstanceUser $
--             newDescribeChannelMembershipForAppInstanceUserResponse
--
--         , responseDescribeChannelModeratedByAppInstanceUser $
--             newDescribeChannelModeratedByAppInstanceUserResponse
--
--         , responseDescribeChannelModerator $
--             newDescribeChannelModeratorResponse
--
--         , responseDisassociatePhoneNumberFromUser $
--             newDisassociatePhoneNumberFromUserResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnectorResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse
--
--         , responseDisassociateSigninDelegateGroupsFromAccount $
--             newDisassociateSigninDelegateGroupsFromAccountResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetAppInstanceRetentionSettings $
--             newGetAppInstanceRetentionSettingsResponse
--
--         , responseGetAppInstanceStreamingConfigurations $
--             newGetAppInstanceStreamingConfigurationsResponse
--
--         , responseGetAttendee $
--             newGetAttendeeResponse
--
--         , responseGetBot $
--             newGetBotResponse
--
--         , responseGetChannelMessage $
--             newGetChannelMessageResponse
--
--         , responseGetEventsConfiguration $
--             newGetEventsConfigurationResponse
--
--         , responseGetGlobalSettings $
--             newGetGlobalSettingsResponse
--
--         , responseGetMediaCapturePipeline $
--             newGetMediaCapturePipelineResponse
--
--         , responseGetMeeting $
--             newGetMeetingResponse
--
--         , responseGetMessagingSessionEndpoint $
--             newGetMessagingSessionEndpointResponse
--
--         , responseGetPhoneNumber $
--             newGetPhoneNumberResponse
--
--         , responseGetPhoneNumberOrder $
--             newGetPhoneNumberOrderResponse
--
--         , responseGetPhoneNumberSettings $
--             newGetPhoneNumberSettingsResponse
--
--         , responseGetProxySession $
--             newGetProxySessionResponse
--
--         , responseGetRetentionSettings $
--             newGetRetentionSettingsResponse
--
--         , responseGetRoom $
--             newGetRoomResponse
--
--         , responseGetSipMediaApplication $
--             newGetSipMediaApplicationResponse
--
--         , responseGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfigurationResponse
--
--         , responseGetSipRule $
--             newGetSipRuleResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseGetUserSettings $
--             newGetUserSettingsResponse
--
--         , responseGetVoiceConnector $
--             newGetVoiceConnectorResponse
--
--         , responseGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroupResponse
--
--         , responseGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfigurationResponse
--
--         , responseGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOriginationResponse
--
--         , responseGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxyResponse
--
--         , responseGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfigurationResponse
--
--         , responseGetVoiceConnectorTermination $
--             newGetVoiceConnectorTerminationResponse
--
--         , responseGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealthResponse
--
--         , responseInviteUsers $
--             newInviteUsersResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseListAppInstanceAdmins $
--             newListAppInstanceAdminsResponse
--
--         , responseListAppInstanceUsers $
--             newListAppInstanceUsersResponse
--
--         , responseListAppInstances $
--             newListAppInstancesResponse
--
--         , responseListAttendeeTags $
--             newListAttendeeTagsResponse
--
--         , responseListAttendees $
--             newListAttendeesResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseListChannelBans $
--             newListChannelBansResponse
--
--         , responseListChannelMemberships $
--             newListChannelMembershipsResponse
--
--         , responseListChannelMembershipsForAppInstanceUser $
--             newListChannelMembershipsForAppInstanceUserResponse
--
--         , responseListChannelMessages $
--             newListChannelMessagesResponse
--
--         , responseListChannelModerators $
--             newListChannelModeratorsResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListChannelsModeratedByAppInstanceUser $
--             newListChannelsModeratedByAppInstanceUserResponse
--
--         , responseListMediaCapturePipelines $
--             newListMediaCapturePipelinesResponse
--
--         , responseListMeetingTags $
--             newListMeetingTagsResponse
--
--         , responseListMeetings $
--             newListMeetingsResponse
--
--         , responseListPhoneNumberOrders $
--             newListPhoneNumberOrdersResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseListProxySessions $
--             newListProxySessionsResponse
--
--         , responseListRoomMemberships $
--             newListRoomMembershipsResponse
--
--         , responseListRooms $
--             newListRoomsResponse
--
--         , responseListSipMediaApplications $
--             newListSipMediaApplicationsResponse
--
--         , responseListSipRules $
--             newListSipRulesResponse
--
--         , responseListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountriesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListVoiceConnectorGroups $
--             newListVoiceConnectorGroupsResponse
--
--         , responseListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentialsResponse
--
--         , responseListVoiceConnectors $
--             newListVoiceConnectorsResponse
--
--         , responseLogoutUser $
--             newLogoutUserResponse
--
--         , responsePutAppInstanceRetentionSettings $
--             newPutAppInstanceRetentionSettingsResponse
--
--         , responsePutAppInstanceStreamingConfigurations $
--             newPutAppInstanceStreamingConfigurationsResponse
--
--         , responsePutEventsConfiguration $
--             newPutEventsConfigurationResponse
--
--         , responsePutRetentionSettings $
--             newPutRetentionSettingsResponse
--
--         , responsePutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfigurationResponse
--
--         , responsePutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responsePutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfigurationResponse
--
--         , responsePutVoiceConnectorOrigination $
--             newPutVoiceConnectorOriginationResponse
--
--         , responsePutVoiceConnectorProxy $
--             newPutVoiceConnectorProxyResponse
--
--         , responsePutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfigurationResponse
--
--         , responsePutVoiceConnectorTermination $
--             newPutVoiceConnectorTerminationResponse
--
--         , responsePutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentialsResponse
--
--         , responseRedactChannelMessage $
--             newRedactChannelMessageResponse
--
--         , responseRedactConversationMessage $
--             newRedactConversationMessageResponse
--
--         , responseRedactRoomMessage $
--             newRedactRoomMessageResponse
--
--         , responseRegenerateSecurityToken $
--             newRegenerateSecurityTokenResponse
--
--         , responseResetPersonalPIN $
--             newResetPersonalPINResponse
--
--         , responseRestorePhoneNumber $
--             newRestorePhoneNumberResponse
--
--         , responseSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbersResponse
--
--         , responseSendChannelMessage $
--             newSendChannelMessageResponse
--
--         , responseStartMeetingTranscription $
--             newStartMeetingTranscriptionResponse
--
--         , responseStopMeetingTranscription $
--             newStopMeetingTranscriptionResponse
--
--         , responseTagAttendee $
--             newTagAttendeeResponse
--
--         , responseTagMeeting $
--             newTagMeetingResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagAttendee $
--             newUntagAttendeeResponse
--
--         , responseUntagMeeting $
--             newUntagMeetingResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccount $
--             newUpdateAccountResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseUpdateAppInstance $
--             newUpdateAppInstanceResponse
--
--         , responseUpdateAppInstanceUser $
--             newUpdateAppInstanceUserResponse
--
--         , responseUpdateBot $
--             newUpdateBotResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseUpdateChannelMessage $
--             newUpdateChannelMessageResponse
--
--         , responseUpdateChannelReadMarker $
--             newUpdateChannelReadMarkerResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseUpdatePhoneNumber $
--             newUpdatePhoneNumberResponse
--
--         , responseUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettingsResponse
--
--         , responseUpdateProxySession $
--             newUpdateProxySessionResponse
--
--         , responseUpdateRoom $
--             newUpdateRoomResponse
--
--         , responseUpdateRoomMembership $
--             newUpdateRoomMembershipResponse
--
--         , responseUpdateSipMediaApplication $
--             newUpdateSipMediaApplicationResponse
--
--         , responseUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCallResponse
--
--         , responseUpdateSipRule $
--             newUpdateSipRuleResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--         , responseUpdateUserSettings $
--             newUpdateUserSettingsResponse
--
--         , responseUpdateVoiceConnector $
--             newUpdateVoiceConnectorResponse
--
--         , responseUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroupResponse
--
--         , responseValidateE911Address $
--             newValidateE911AddressResponse
--
--           ]
--     ]

-- Requests

requestAssociatePhoneNumberWithUser :: AssociatePhoneNumberWithUser -> TestTree
requestAssociatePhoneNumberWithUser =
  req
    "AssociatePhoneNumberWithUser"
    "fixture/AssociatePhoneNumberWithUser.yaml"

requestAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnector -> TestTree
requestAssociatePhoneNumbersWithVoiceConnector =
  req
    "AssociatePhoneNumbersWithVoiceConnector"
    "fixture/AssociatePhoneNumbersWithVoiceConnector.yaml"

requestAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroup -> TestTree
requestAssociatePhoneNumbersWithVoiceConnectorGroup =
  req
    "AssociatePhoneNumbersWithVoiceConnectorGroup"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroup.yaml"

requestAssociateSigninDelegateGroupsWithAccount :: AssociateSigninDelegateGroupsWithAccount -> TestTree
requestAssociateSigninDelegateGroupsWithAccount =
  req
    "AssociateSigninDelegateGroupsWithAccount"
    "fixture/AssociateSigninDelegateGroupsWithAccount.yaml"

requestBatchCreateAttendee :: BatchCreateAttendee -> TestTree
requestBatchCreateAttendee =
  req
    "BatchCreateAttendee"
    "fixture/BatchCreateAttendee.yaml"

requestBatchCreateChannelMembership :: BatchCreateChannelMembership -> TestTree
requestBatchCreateChannelMembership =
  req
    "BatchCreateChannelMembership"
    "fixture/BatchCreateChannelMembership.yaml"

requestBatchCreateRoomMembership :: BatchCreateRoomMembership -> TestTree
requestBatchCreateRoomMembership =
  req
    "BatchCreateRoomMembership"
    "fixture/BatchCreateRoomMembership.yaml"

requestBatchDeletePhoneNumber :: BatchDeletePhoneNumber -> TestTree
requestBatchDeletePhoneNumber =
  req
    "BatchDeletePhoneNumber"
    "fixture/BatchDeletePhoneNumber.yaml"

requestBatchSuspendUser :: BatchSuspendUser -> TestTree
requestBatchSuspendUser =
  req
    "BatchSuspendUser"
    "fixture/BatchSuspendUser.yaml"

requestBatchUnsuspendUser :: BatchUnsuspendUser -> TestTree
requestBatchUnsuspendUser =
  req
    "BatchUnsuspendUser"
    "fixture/BatchUnsuspendUser.yaml"

requestBatchUpdatePhoneNumber :: BatchUpdatePhoneNumber -> TestTree
requestBatchUpdatePhoneNumber =
  req
    "BatchUpdatePhoneNumber"
    "fixture/BatchUpdatePhoneNumber.yaml"

requestBatchUpdateUser :: BatchUpdateUser -> TestTree
requestBatchUpdateUser =
  req
    "BatchUpdateUser"
    "fixture/BatchUpdateUser.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestCreateAppInstance :: CreateAppInstance -> TestTree
requestCreateAppInstance =
  req
    "CreateAppInstance"
    "fixture/CreateAppInstance.yaml"

requestCreateAppInstanceAdmin :: CreateAppInstanceAdmin -> TestTree
requestCreateAppInstanceAdmin =
  req
    "CreateAppInstanceAdmin"
    "fixture/CreateAppInstanceAdmin.yaml"

requestCreateAppInstanceUser :: CreateAppInstanceUser -> TestTree
requestCreateAppInstanceUser =
  req
    "CreateAppInstanceUser"
    "fixture/CreateAppInstanceUser.yaml"

requestCreateAttendee :: CreateAttendee -> TestTree
requestCreateAttendee =
  req
    "CreateAttendee"
    "fixture/CreateAttendee.yaml"

requestCreateBot :: CreateBot -> TestTree
requestCreateBot =
  req
    "CreateBot"
    "fixture/CreateBot.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateChannelBan :: CreateChannelBan -> TestTree
requestCreateChannelBan =
  req
    "CreateChannelBan"
    "fixture/CreateChannelBan.yaml"

requestCreateChannelMembership :: CreateChannelMembership -> TestTree
requestCreateChannelMembership =
  req
    "CreateChannelMembership"
    "fixture/CreateChannelMembership.yaml"

requestCreateChannelModerator :: CreateChannelModerator -> TestTree
requestCreateChannelModerator =
  req
    "CreateChannelModerator"
    "fixture/CreateChannelModerator.yaml"

requestCreateMediaCapturePipeline :: CreateMediaCapturePipeline -> TestTree
requestCreateMediaCapturePipeline =
  req
    "CreateMediaCapturePipeline"
    "fixture/CreateMediaCapturePipeline.yaml"

requestCreateMeeting :: CreateMeeting -> TestTree
requestCreateMeeting =
  req
    "CreateMeeting"
    "fixture/CreateMeeting.yaml"

requestCreateMeetingDialOut :: CreateMeetingDialOut -> TestTree
requestCreateMeetingDialOut =
  req
    "CreateMeetingDialOut"
    "fixture/CreateMeetingDialOut.yaml"

requestCreateMeetingWithAttendees :: CreateMeetingWithAttendees -> TestTree
requestCreateMeetingWithAttendees =
  req
    "CreateMeetingWithAttendees"
    "fixture/CreateMeetingWithAttendees.yaml"

requestCreatePhoneNumberOrder :: CreatePhoneNumberOrder -> TestTree
requestCreatePhoneNumberOrder =
  req
    "CreatePhoneNumberOrder"
    "fixture/CreatePhoneNumberOrder.yaml"

requestCreateProxySession :: CreateProxySession -> TestTree
requestCreateProxySession =
  req
    "CreateProxySession"
    "fixture/CreateProxySession.yaml"

requestCreateRoom :: CreateRoom -> TestTree
requestCreateRoom =
  req
    "CreateRoom"
    "fixture/CreateRoom.yaml"

requestCreateRoomMembership :: CreateRoomMembership -> TestTree
requestCreateRoomMembership =
  req
    "CreateRoomMembership"
    "fixture/CreateRoomMembership.yaml"

requestCreateSipMediaApplication :: CreateSipMediaApplication -> TestTree
requestCreateSipMediaApplication =
  req
    "CreateSipMediaApplication"
    "fixture/CreateSipMediaApplication.yaml"

requestCreateSipMediaApplicationCall :: CreateSipMediaApplicationCall -> TestTree
requestCreateSipMediaApplicationCall =
  req
    "CreateSipMediaApplicationCall"
    "fixture/CreateSipMediaApplicationCall.yaml"

requestCreateSipRule :: CreateSipRule -> TestTree
requestCreateSipRule =
  req
    "CreateSipRule"
    "fixture/CreateSipRule.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestCreateVoiceConnector :: CreateVoiceConnector -> TestTree
requestCreateVoiceConnector =
  req
    "CreateVoiceConnector"
    "fixture/CreateVoiceConnector.yaml"

requestCreateVoiceConnectorGroup :: CreateVoiceConnectorGroup -> TestTree
requestCreateVoiceConnectorGroup =
  req
    "CreateVoiceConnectorGroup"
    "fixture/CreateVoiceConnectorGroup.yaml"

requestDeleteAccount :: DeleteAccount -> TestTree
requestDeleteAccount =
  req
    "DeleteAccount"
    "fixture/DeleteAccount.yaml"

requestDeleteAppInstance :: DeleteAppInstance -> TestTree
requestDeleteAppInstance =
  req
    "DeleteAppInstance"
    "fixture/DeleteAppInstance.yaml"

requestDeleteAppInstanceAdmin :: DeleteAppInstanceAdmin -> TestTree
requestDeleteAppInstanceAdmin =
  req
    "DeleteAppInstanceAdmin"
    "fixture/DeleteAppInstanceAdmin.yaml"

requestDeleteAppInstanceStreamingConfigurations :: DeleteAppInstanceStreamingConfigurations -> TestTree
requestDeleteAppInstanceStreamingConfigurations =
  req
    "DeleteAppInstanceStreamingConfigurations"
    "fixture/DeleteAppInstanceStreamingConfigurations.yaml"

requestDeleteAppInstanceUser :: DeleteAppInstanceUser -> TestTree
requestDeleteAppInstanceUser =
  req
    "DeleteAppInstanceUser"
    "fixture/DeleteAppInstanceUser.yaml"

requestDeleteAttendee :: DeleteAttendee -> TestTree
requestDeleteAttendee =
  req
    "DeleteAttendee"
    "fixture/DeleteAttendee.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeleteChannelBan :: DeleteChannelBan -> TestTree
requestDeleteChannelBan =
  req
    "DeleteChannelBan"
    "fixture/DeleteChannelBan.yaml"

requestDeleteChannelMembership :: DeleteChannelMembership -> TestTree
requestDeleteChannelMembership =
  req
    "DeleteChannelMembership"
    "fixture/DeleteChannelMembership.yaml"

requestDeleteChannelMessage :: DeleteChannelMessage -> TestTree
requestDeleteChannelMessage =
  req
    "DeleteChannelMessage"
    "fixture/DeleteChannelMessage.yaml"

requestDeleteChannelModerator :: DeleteChannelModerator -> TestTree
requestDeleteChannelModerator =
  req
    "DeleteChannelModerator"
    "fixture/DeleteChannelModerator.yaml"

requestDeleteEventsConfiguration :: DeleteEventsConfiguration -> TestTree
requestDeleteEventsConfiguration =
  req
    "DeleteEventsConfiguration"
    "fixture/DeleteEventsConfiguration.yaml"

requestDeleteMediaCapturePipeline :: DeleteMediaCapturePipeline -> TestTree
requestDeleteMediaCapturePipeline =
  req
    "DeleteMediaCapturePipeline"
    "fixture/DeleteMediaCapturePipeline.yaml"

requestDeleteMeeting :: DeleteMeeting -> TestTree
requestDeleteMeeting =
  req
    "DeleteMeeting"
    "fixture/DeleteMeeting.yaml"

requestDeletePhoneNumber :: DeletePhoneNumber -> TestTree
requestDeletePhoneNumber =
  req
    "DeletePhoneNumber"
    "fixture/DeletePhoneNumber.yaml"

requestDeleteProxySession :: DeleteProxySession -> TestTree
requestDeleteProxySession =
  req
    "DeleteProxySession"
    "fixture/DeleteProxySession.yaml"

requestDeleteRoom :: DeleteRoom -> TestTree
requestDeleteRoom =
  req
    "DeleteRoom"
    "fixture/DeleteRoom.yaml"

requestDeleteRoomMembership :: DeleteRoomMembership -> TestTree
requestDeleteRoomMembership =
  req
    "DeleteRoomMembership"
    "fixture/DeleteRoomMembership.yaml"

requestDeleteSipMediaApplication :: DeleteSipMediaApplication -> TestTree
requestDeleteSipMediaApplication =
  req
    "DeleteSipMediaApplication"
    "fixture/DeleteSipMediaApplication.yaml"

requestDeleteSipRule :: DeleteSipRule -> TestTree
requestDeleteSipRule =
  req
    "DeleteSipRule"
    "fixture/DeleteSipRule.yaml"

requestDeleteVoiceConnector :: DeleteVoiceConnector -> TestTree
requestDeleteVoiceConnector =
  req
    "DeleteVoiceConnector"
    "fixture/DeleteVoiceConnector.yaml"

requestDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestDeleteVoiceConnectorEmergencyCallingConfiguration =
  req
    "DeleteVoiceConnectorEmergencyCallingConfiguration"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfiguration.yaml"

requestDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroup -> TestTree
requestDeleteVoiceConnectorGroup =
  req
    "DeleteVoiceConnectorGroup"
    "fixture/DeleteVoiceConnectorGroup.yaml"

requestDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOrigination -> TestTree
requestDeleteVoiceConnectorOrigination =
  req
    "DeleteVoiceConnectorOrigination"
    "fixture/DeleteVoiceConnectorOrigination.yaml"

requestDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxy -> TestTree
requestDeleteVoiceConnectorProxy =
  req
    "DeleteVoiceConnectorProxy"
    "fixture/DeleteVoiceConnectorProxy.yaml"

requestDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfiguration -> TestTree
requestDeleteVoiceConnectorStreamingConfiguration =
  req
    "DeleteVoiceConnectorStreamingConfiguration"
    "fixture/DeleteVoiceConnectorStreamingConfiguration.yaml"

requestDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTermination -> TestTree
requestDeleteVoiceConnectorTermination =
  req
    "DeleteVoiceConnectorTermination"
    "fixture/DeleteVoiceConnectorTermination.yaml"

requestDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentials -> TestTree
requestDeleteVoiceConnectorTerminationCredentials =
  req
    "DeleteVoiceConnectorTerminationCredentials"
    "fixture/DeleteVoiceConnectorTerminationCredentials.yaml"

requestDescribeAppInstance :: DescribeAppInstance -> TestTree
requestDescribeAppInstance =
  req
    "DescribeAppInstance"
    "fixture/DescribeAppInstance.yaml"

requestDescribeAppInstanceAdmin :: DescribeAppInstanceAdmin -> TestTree
requestDescribeAppInstanceAdmin =
  req
    "DescribeAppInstanceAdmin"
    "fixture/DescribeAppInstanceAdmin.yaml"

requestDescribeAppInstanceUser :: DescribeAppInstanceUser -> TestTree
requestDescribeAppInstanceUser =
  req
    "DescribeAppInstanceUser"
    "fixture/DescribeAppInstanceUser.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDescribeChannelBan :: DescribeChannelBan -> TestTree
requestDescribeChannelBan =
  req
    "DescribeChannelBan"
    "fixture/DescribeChannelBan.yaml"

requestDescribeChannelMembership :: DescribeChannelMembership -> TestTree
requestDescribeChannelMembership =
  req
    "DescribeChannelMembership"
    "fixture/DescribeChannelMembership.yaml"

requestDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUser -> TestTree
requestDescribeChannelMembershipForAppInstanceUser =
  req
    "DescribeChannelMembershipForAppInstanceUser"
    "fixture/DescribeChannelMembershipForAppInstanceUser.yaml"

requestDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUser -> TestTree
requestDescribeChannelModeratedByAppInstanceUser =
  req
    "DescribeChannelModeratedByAppInstanceUser"
    "fixture/DescribeChannelModeratedByAppInstanceUser.yaml"

requestDescribeChannelModerator :: DescribeChannelModerator -> TestTree
requestDescribeChannelModerator =
  req
    "DescribeChannelModerator"
    "fixture/DescribeChannelModerator.yaml"

requestDisassociatePhoneNumberFromUser :: DisassociatePhoneNumberFromUser -> TestTree
requestDisassociatePhoneNumberFromUser =
  req
    "DisassociatePhoneNumberFromUser"
    "fixture/DisassociatePhoneNumberFromUser.yaml"

requestDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnector -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnector =
  req
    "DisassociatePhoneNumbersFromVoiceConnector"
    "fixture/DisassociatePhoneNumbersFromVoiceConnector.yaml"

requestDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroup -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnectorGroup =
  req
    "DisassociatePhoneNumbersFromVoiceConnectorGroup"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroup.yaml"

requestDisassociateSigninDelegateGroupsFromAccount :: DisassociateSigninDelegateGroupsFromAccount -> TestTree
requestDisassociateSigninDelegateGroupsFromAccount =
  req
    "DisassociateSigninDelegateGroupsFromAccount"
    "fixture/DisassociateSigninDelegateGroupsFromAccount.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettings -> TestTree
requestGetAppInstanceRetentionSettings =
  req
    "GetAppInstanceRetentionSettings"
    "fixture/GetAppInstanceRetentionSettings.yaml"

requestGetAppInstanceStreamingConfigurations :: GetAppInstanceStreamingConfigurations -> TestTree
requestGetAppInstanceStreamingConfigurations =
  req
    "GetAppInstanceStreamingConfigurations"
    "fixture/GetAppInstanceStreamingConfigurations.yaml"

requestGetAttendee :: GetAttendee -> TestTree
requestGetAttendee =
  req
    "GetAttendee"
    "fixture/GetAttendee.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot =
  req
    "GetBot"
    "fixture/GetBot.yaml"

requestGetChannelMessage :: GetChannelMessage -> TestTree
requestGetChannelMessage =
  req
    "GetChannelMessage"
    "fixture/GetChannelMessage.yaml"

requestGetEventsConfiguration :: GetEventsConfiguration -> TestTree
requestGetEventsConfiguration =
  req
    "GetEventsConfiguration"
    "fixture/GetEventsConfiguration.yaml"

requestGetGlobalSettings :: GetGlobalSettings -> TestTree
requestGetGlobalSettings =
  req
    "GetGlobalSettings"
    "fixture/GetGlobalSettings.yaml"

requestGetMediaCapturePipeline :: GetMediaCapturePipeline -> TestTree
requestGetMediaCapturePipeline =
  req
    "GetMediaCapturePipeline"
    "fixture/GetMediaCapturePipeline.yaml"

requestGetMeeting :: GetMeeting -> TestTree
requestGetMeeting =
  req
    "GetMeeting"
    "fixture/GetMeeting.yaml"

requestGetMessagingSessionEndpoint :: GetMessagingSessionEndpoint -> TestTree
requestGetMessagingSessionEndpoint =
  req
    "GetMessagingSessionEndpoint"
    "fixture/GetMessagingSessionEndpoint.yaml"

requestGetPhoneNumber :: GetPhoneNumber -> TestTree
requestGetPhoneNumber =
  req
    "GetPhoneNumber"
    "fixture/GetPhoneNumber.yaml"

requestGetPhoneNumberOrder :: GetPhoneNumberOrder -> TestTree
requestGetPhoneNumberOrder =
  req
    "GetPhoneNumberOrder"
    "fixture/GetPhoneNumberOrder.yaml"

requestGetPhoneNumberSettings :: GetPhoneNumberSettings -> TestTree
requestGetPhoneNumberSettings =
  req
    "GetPhoneNumberSettings"
    "fixture/GetPhoneNumberSettings.yaml"

requestGetProxySession :: GetProxySession -> TestTree
requestGetProxySession =
  req
    "GetProxySession"
    "fixture/GetProxySession.yaml"

requestGetRetentionSettings :: GetRetentionSettings -> TestTree
requestGetRetentionSettings =
  req
    "GetRetentionSettings"
    "fixture/GetRetentionSettings.yaml"

requestGetRoom :: GetRoom -> TestTree
requestGetRoom =
  req
    "GetRoom"
    "fixture/GetRoom.yaml"

requestGetSipMediaApplication :: GetSipMediaApplication -> TestTree
requestGetSipMediaApplication =
  req
    "GetSipMediaApplication"
    "fixture/GetSipMediaApplication.yaml"

requestGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfiguration -> TestTree
requestGetSipMediaApplicationLoggingConfiguration =
  req
    "GetSipMediaApplicationLoggingConfiguration"
    "fixture/GetSipMediaApplicationLoggingConfiguration.yaml"

requestGetSipRule :: GetSipRule -> TestTree
requestGetSipRule =
  req
    "GetSipRule"
    "fixture/GetSipRule.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestGetUserSettings :: GetUserSettings -> TestTree
requestGetUserSettings =
  req
    "GetUserSettings"
    "fixture/GetUserSettings.yaml"

requestGetVoiceConnector :: GetVoiceConnector -> TestTree
requestGetVoiceConnector =
  req
    "GetVoiceConnector"
    "fixture/GetVoiceConnector.yaml"

requestGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestGetVoiceConnectorEmergencyCallingConfiguration =
  req
    "GetVoiceConnectorEmergencyCallingConfiguration"
    "fixture/GetVoiceConnectorEmergencyCallingConfiguration.yaml"

requestGetVoiceConnectorGroup :: GetVoiceConnectorGroup -> TestTree
requestGetVoiceConnectorGroup =
  req
    "GetVoiceConnectorGroup"
    "fixture/GetVoiceConnectorGroup.yaml"

requestGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfiguration -> TestTree
requestGetVoiceConnectorLoggingConfiguration =
  req
    "GetVoiceConnectorLoggingConfiguration"
    "fixture/GetVoiceConnectorLoggingConfiguration.yaml"

requestGetVoiceConnectorOrigination :: GetVoiceConnectorOrigination -> TestTree
requestGetVoiceConnectorOrigination =
  req
    "GetVoiceConnectorOrigination"
    "fixture/GetVoiceConnectorOrigination.yaml"

requestGetVoiceConnectorProxy :: GetVoiceConnectorProxy -> TestTree
requestGetVoiceConnectorProxy =
  req
    "GetVoiceConnectorProxy"
    "fixture/GetVoiceConnectorProxy.yaml"

requestGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfiguration -> TestTree
requestGetVoiceConnectorStreamingConfiguration =
  req
    "GetVoiceConnectorStreamingConfiguration"
    "fixture/GetVoiceConnectorStreamingConfiguration.yaml"

requestGetVoiceConnectorTermination :: GetVoiceConnectorTermination -> TestTree
requestGetVoiceConnectorTermination =
  req
    "GetVoiceConnectorTermination"
    "fixture/GetVoiceConnectorTermination.yaml"

requestGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealth -> TestTree
requestGetVoiceConnectorTerminationHealth =
  req
    "GetVoiceConnectorTerminationHealth"
    "fixture/GetVoiceConnectorTerminationHealth.yaml"

requestInviteUsers :: InviteUsers -> TestTree
requestInviteUsers =
  req
    "InviteUsers"
    "fixture/InviteUsers.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestListAppInstanceAdmins :: ListAppInstanceAdmins -> TestTree
requestListAppInstanceAdmins =
  req
    "ListAppInstanceAdmins"
    "fixture/ListAppInstanceAdmins.yaml"

requestListAppInstanceUsers :: ListAppInstanceUsers -> TestTree
requestListAppInstanceUsers =
  req
    "ListAppInstanceUsers"
    "fixture/ListAppInstanceUsers.yaml"

requestListAppInstances :: ListAppInstances -> TestTree
requestListAppInstances =
  req
    "ListAppInstances"
    "fixture/ListAppInstances.yaml"

requestListAttendeeTags :: ListAttendeeTags -> TestTree
requestListAttendeeTags =
  req
    "ListAttendeeTags"
    "fixture/ListAttendeeTags.yaml"

requestListAttendees :: ListAttendees -> TestTree
requestListAttendees =
  req
    "ListAttendees"
    "fixture/ListAttendees.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestListChannelBans :: ListChannelBans -> TestTree
requestListChannelBans =
  req
    "ListChannelBans"
    "fixture/ListChannelBans.yaml"

requestListChannelMemberships :: ListChannelMemberships -> TestTree
requestListChannelMemberships =
  req
    "ListChannelMemberships"
    "fixture/ListChannelMemberships.yaml"

requestListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUser -> TestTree
requestListChannelMembershipsForAppInstanceUser =
  req
    "ListChannelMembershipsForAppInstanceUser"
    "fixture/ListChannelMembershipsForAppInstanceUser.yaml"

requestListChannelMessages :: ListChannelMessages -> TestTree
requestListChannelMessages =
  req
    "ListChannelMessages"
    "fixture/ListChannelMessages.yaml"

requestListChannelModerators :: ListChannelModerators -> TestTree
requestListChannelModerators =
  req
    "ListChannelModerators"
    "fixture/ListChannelModerators.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUser -> TestTree
requestListChannelsModeratedByAppInstanceUser =
  req
    "ListChannelsModeratedByAppInstanceUser"
    "fixture/ListChannelsModeratedByAppInstanceUser.yaml"

requestListMediaCapturePipelines :: ListMediaCapturePipelines -> TestTree
requestListMediaCapturePipelines =
  req
    "ListMediaCapturePipelines"
    "fixture/ListMediaCapturePipelines.yaml"

requestListMeetingTags :: ListMeetingTags -> TestTree
requestListMeetingTags =
  req
    "ListMeetingTags"
    "fixture/ListMeetingTags.yaml"

requestListMeetings :: ListMeetings -> TestTree
requestListMeetings =
  req
    "ListMeetings"
    "fixture/ListMeetings.yaml"

requestListPhoneNumberOrders :: ListPhoneNumberOrders -> TestTree
requestListPhoneNumberOrders =
  req
    "ListPhoneNumberOrders"
    "fixture/ListPhoneNumberOrders.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestListProxySessions :: ListProxySessions -> TestTree
requestListProxySessions =
  req
    "ListProxySessions"
    "fixture/ListProxySessions.yaml"

requestListRoomMemberships :: ListRoomMemberships -> TestTree
requestListRoomMemberships =
  req
    "ListRoomMemberships"
    "fixture/ListRoomMemberships.yaml"

requestListRooms :: ListRooms -> TestTree
requestListRooms =
  req
    "ListRooms"
    "fixture/ListRooms.yaml"

requestListSipMediaApplications :: ListSipMediaApplications -> TestTree
requestListSipMediaApplications =
  req
    "ListSipMediaApplications"
    "fixture/ListSipMediaApplications.yaml"

requestListSipRules :: ListSipRules -> TestTree
requestListSipRules =
  req
    "ListSipRules"
    "fixture/ListSipRules.yaml"

requestListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountries -> TestTree
requestListSupportedPhoneNumberCountries =
  req
    "ListSupportedPhoneNumberCountries"
    "fixture/ListSupportedPhoneNumberCountries.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListVoiceConnectorGroups :: ListVoiceConnectorGroups -> TestTree
requestListVoiceConnectorGroups =
  req
    "ListVoiceConnectorGroups"
    "fixture/ListVoiceConnectorGroups.yaml"

requestListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentials -> TestTree
requestListVoiceConnectorTerminationCredentials =
  req
    "ListVoiceConnectorTerminationCredentials"
    "fixture/ListVoiceConnectorTerminationCredentials.yaml"

requestListVoiceConnectors :: ListVoiceConnectors -> TestTree
requestListVoiceConnectors =
  req
    "ListVoiceConnectors"
    "fixture/ListVoiceConnectors.yaml"

requestLogoutUser :: LogoutUser -> TestTree
requestLogoutUser =
  req
    "LogoutUser"
    "fixture/LogoutUser.yaml"

requestPutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettings -> TestTree
requestPutAppInstanceRetentionSettings =
  req
    "PutAppInstanceRetentionSettings"
    "fixture/PutAppInstanceRetentionSettings.yaml"

requestPutAppInstanceStreamingConfigurations :: PutAppInstanceStreamingConfigurations -> TestTree
requestPutAppInstanceStreamingConfigurations =
  req
    "PutAppInstanceStreamingConfigurations"
    "fixture/PutAppInstanceStreamingConfigurations.yaml"

requestPutEventsConfiguration :: PutEventsConfiguration -> TestTree
requestPutEventsConfiguration =
  req
    "PutEventsConfiguration"
    "fixture/PutEventsConfiguration.yaml"

requestPutRetentionSettings :: PutRetentionSettings -> TestTree
requestPutRetentionSettings =
  req
    "PutRetentionSettings"
    "fixture/PutRetentionSettings.yaml"

requestPutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfiguration -> TestTree
requestPutSipMediaApplicationLoggingConfiguration =
  req
    "PutSipMediaApplicationLoggingConfiguration"
    "fixture/PutSipMediaApplicationLoggingConfiguration.yaml"

requestPutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestPutVoiceConnectorEmergencyCallingConfiguration =
  req
    "PutVoiceConnectorEmergencyCallingConfiguration"
    "fixture/PutVoiceConnectorEmergencyCallingConfiguration.yaml"

requestPutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfiguration -> TestTree
requestPutVoiceConnectorLoggingConfiguration =
  req
    "PutVoiceConnectorLoggingConfiguration"
    "fixture/PutVoiceConnectorLoggingConfiguration.yaml"

requestPutVoiceConnectorOrigination :: PutVoiceConnectorOrigination -> TestTree
requestPutVoiceConnectorOrigination =
  req
    "PutVoiceConnectorOrigination"
    "fixture/PutVoiceConnectorOrigination.yaml"

requestPutVoiceConnectorProxy :: PutVoiceConnectorProxy -> TestTree
requestPutVoiceConnectorProxy =
  req
    "PutVoiceConnectorProxy"
    "fixture/PutVoiceConnectorProxy.yaml"

requestPutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfiguration -> TestTree
requestPutVoiceConnectorStreamingConfiguration =
  req
    "PutVoiceConnectorStreamingConfiguration"
    "fixture/PutVoiceConnectorStreamingConfiguration.yaml"

requestPutVoiceConnectorTermination :: PutVoiceConnectorTermination -> TestTree
requestPutVoiceConnectorTermination =
  req
    "PutVoiceConnectorTermination"
    "fixture/PutVoiceConnectorTermination.yaml"

requestPutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentials -> TestTree
requestPutVoiceConnectorTerminationCredentials =
  req
    "PutVoiceConnectorTerminationCredentials"
    "fixture/PutVoiceConnectorTerminationCredentials.yaml"

requestRedactChannelMessage :: RedactChannelMessage -> TestTree
requestRedactChannelMessage =
  req
    "RedactChannelMessage"
    "fixture/RedactChannelMessage.yaml"

requestRedactConversationMessage :: RedactConversationMessage -> TestTree
requestRedactConversationMessage =
  req
    "RedactConversationMessage"
    "fixture/RedactConversationMessage.yaml"

requestRedactRoomMessage :: RedactRoomMessage -> TestTree
requestRedactRoomMessage =
  req
    "RedactRoomMessage"
    "fixture/RedactRoomMessage.yaml"

requestRegenerateSecurityToken :: RegenerateSecurityToken -> TestTree
requestRegenerateSecurityToken =
  req
    "RegenerateSecurityToken"
    "fixture/RegenerateSecurityToken.yaml"

requestResetPersonalPIN :: ResetPersonalPIN -> TestTree
requestResetPersonalPIN =
  req
    "ResetPersonalPIN"
    "fixture/ResetPersonalPIN.yaml"

requestRestorePhoneNumber :: RestorePhoneNumber -> TestTree
requestRestorePhoneNumber =
  req
    "RestorePhoneNumber"
    "fixture/RestorePhoneNumber.yaml"

requestSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbers -> TestTree
requestSearchAvailablePhoneNumbers =
  req
    "SearchAvailablePhoneNumbers"
    "fixture/SearchAvailablePhoneNumbers.yaml"

requestSendChannelMessage :: SendChannelMessage -> TestTree
requestSendChannelMessage =
  req
    "SendChannelMessage"
    "fixture/SendChannelMessage.yaml"

requestStartMeetingTranscription :: StartMeetingTranscription -> TestTree
requestStartMeetingTranscription =
  req
    "StartMeetingTranscription"
    "fixture/StartMeetingTranscription.yaml"

requestStopMeetingTranscription :: StopMeetingTranscription -> TestTree
requestStopMeetingTranscription =
  req
    "StopMeetingTranscription"
    "fixture/StopMeetingTranscription.yaml"

requestTagAttendee :: TagAttendee -> TestTree
requestTagAttendee =
  req
    "TagAttendee"
    "fixture/TagAttendee.yaml"

requestTagMeeting :: TagMeeting -> TestTree
requestTagMeeting =
  req
    "TagMeeting"
    "fixture/TagMeeting.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagAttendee :: UntagAttendee -> TestTree
requestUntagAttendee =
  req
    "UntagAttendee"
    "fixture/UntagAttendee.yaml"

requestUntagMeeting :: UntagMeeting -> TestTree
requestUntagMeeting =
  req
    "UntagMeeting"
    "fixture/UntagMeeting.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestUpdateAppInstance :: UpdateAppInstance -> TestTree
requestUpdateAppInstance =
  req
    "UpdateAppInstance"
    "fixture/UpdateAppInstance.yaml"

requestUpdateAppInstanceUser :: UpdateAppInstanceUser -> TestTree
requestUpdateAppInstanceUser =
  req
    "UpdateAppInstanceUser"
    "fixture/UpdateAppInstanceUser.yaml"

requestUpdateBot :: UpdateBot -> TestTree
requestUpdateBot =
  req
    "UpdateBot"
    "fixture/UpdateBot.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestUpdateChannelMessage :: UpdateChannelMessage -> TestTree
requestUpdateChannelMessage =
  req
    "UpdateChannelMessage"
    "fixture/UpdateChannelMessage.yaml"

requestUpdateChannelReadMarker :: UpdateChannelReadMarker -> TestTree
requestUpdateChannelReadMarker =
  req
    "UpdateChannelReadMarker"
    "fixture/UpdateChannelReadMarker.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestUpdatePhoneNumber :: UpdatePhoneNumber -> TestTree
requestUpdatePhoneNumber =
  req
    "UpdatePhoneNumber"
    "fixture/UpdatePhoneNumber.yaml"

requestUpdatePhoneNumberSettings :: UpdatePhoneNumberSettings -> TestTree
requestUpdatePhoneNumberSettings =
  req
    "UpdatePhoneNumberSettings"
    "fixture/UpdatePhoneNumberSettings.yaml"

requestUpdateProxySession :: UpdateProxySession -> TestTree
requestUpdateProxySession =
  req
    "UpdateProxySession"
    "fixture/UpdateProxySession.yaml"

requestUpdateRoom :: UpdateRoom -> TestTree
requestUpdateRoom =
  req
    "UpdateRoom"
    "fixture/UpdateRoom.yaml"

requestUpdateRoomMembership :: UpdateRoomMembership -> TestTree
requestUpdateRoomMembership =
  req
    "UpdateRoomMembership"
    "fixture/UpdateRoomMembership.yaml"

requestUpdateSipMediaApplication :: UpdateSipMediaApplication -> TestTree
requestUpdateSipMediaApplication =
  req
    "UpdateSipMediaApplication"
    "fixture/UpdateSipMediaApplication.yaml"

requestUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCall -> TestTree
requestUpdateSipMediaApplicationCall =
  req
    "UpdateSipMediaApplicationCall"
    "fixture/UpdateSipMediaApplicationCall.yaml"

requestUpdateSipRule :: UpdateSipRule -> TestTree
requestUpdateSipRule =
  req
    "UpdateSipRule"
    "fixture/UpdateSipRule.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

requestUpdateUserSettings :: UpdateUserSettings -> TestTree
requestUpdateUserSettings =
  req
    "UpdateUserSettings"
    "fixture/UpdateUserSettings.yaml"

requestUpdateVoiceConnector :: UpdateVoiceConnector -> TestTree
requestUpdateVoiceConnector =
  req
    "UpdateVoiceConnector"
    "fixture/UpdateVoiceConnector.yaml"

requestUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroup -> TestTree
requestUpdateVoiceConnectorGroup =
  req
    "UpdateVoiceConnectorGroup"
    "fixture/UpdateVoiceConnectorGroup.yaml"

requestValidateE911Address :: ValidateE911Address -> TestTree
requestValidateE911Address =
  req
    "ValidateE911Address"
    "fixture/ValidateE911Address.yaml"

-- Responses

responseAssociatePhoneNumberWithUser :: AssociatePhoneNumberWithUserResponse -> TestTree
responseAssociatePhoneNumberWithUser =
  res
    "AssociatePhoneNumberWithUserResponse"
    "fixture/AssociatePhoneNumberWithUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumberWithUser)

responseAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnectorResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnector =
  res
    "AssociatePhoneNumbersWithVoiceConnectorResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumbersWithVoiceConnector)

responseAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroupResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnectorGroup =
  res
    "AssociatePhoneNumbersWithVoiceConnectorGroupResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumbersWithVoiceConnectorGroup)

responseAssociateSigninDelegateGroupsWithAccount :: AssociateSigninDelegateGroupsWithAccountResponse -> TestTree
responseAssociateSigninDelegateGroupsWithAccount =
  res
    "AssociateSigninDelegateGroupsWithAccountResponse"
    "fixture/AssociateSigninDelegateGroupsWithAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSigninDelegateGroupsWithAccount)

responseBatchCreateAttendee :: BatchCreateAttendeeResponse -> TestTree
responseBatchCreateAttendee =
  res
    "BatchCreateAttendeeResponse"
    "fixture/BatchCreateAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateAttendee)

responseBatchCreateChannelMembership :: BatchCreateChannelMembershipResponse -> TestTree
responseBatchCreateChannelMembership =
  res
    "BatchCreateChannelMembershipResponse"
    "fixture/BatchCreateChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateChannelMembership)

responseBatchCreateRoomMembership :: BatchCreateRoomMembershipResponse -> TestTree
responseBatchCreateRoomMembership =
  res
    "BatchCreateRoomMembershipResponse"
    "fixture/BatchCreateRoomMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateRoomMembership)

responseBatchDeletePhoneNumber :: BatchDeletePhoneNumberResponse -> TestTree
responseBatchDeletePhoneNumber =
  res
    "BatchDeletePhoneNumberResponse"
    "fixture/BatchDeletePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeletePhoneNumber)

responseBatchSuspendUser :: BatchSuspendUserResponse -> TestTree
responseBatchSuspendUser =
  res
    "BatchSuspendUserResponse"
    "fixture/BatchSuspendUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchSuspendUser)

responseBatchUnsuspendUser :: BatchUnsuspendUserResponse -> TestTree
responseBatchUnsuspendUser =
  res
    "BatchUnsuspendUserResponse"
    "fixture/BatchUnsuspendUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUnsuspendUser)

responseBatchUpdatePhoneNumber :: BatchUpdatePhoneNumberResponse -> TestTree
responseBatchUpdatePhoneNumber =
  res
    "BatchUpdatePhoneNumberResponse"
    "fixture/BatchUpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdatePhoneNumber)

responseBatchUpdateUser :: BatchUpdateUserResponse -> TestTree
responseBatchUpdateUser =
  res
    "BatchUpdateUserResponse"
    "fixture/BatchUpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateUser)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccount)

responseCreateAppInstance :: CreateAppInstanceResponse -> TestTree
responseCreateAppInstance =
  res
    "CreateAppInstanceResponse"
    "fixture/CreateAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstance)

responseCreateAppInstanceAdmin :: CreateAppInstanceAdminResponse -> TestTree
responseCreateAppInstanceAdmin =
  res
    "CreateAppInstanceAdminResponse"
    "fixture/CreateAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstanceAdmin)

responseCreateAppInstanceUser :: CreateAppInstanceUserResponse -> TestTree
responseCreateAppInstanceUser =
  res
    "CreateAppInstanceUserResponse"
    "fixture/CreateAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppInstanceUser)

responseCreateAttendee :: CreateAttendeeResponse -> TestTree
responseCreateAttendee =
  res
    "CreateAttendeeResponse"
    "fixture/CreateAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAttendee)

responseCreateBot :: CreateBotResponse -> TestTree
responseCreateBot =
  res
    "CreateBotResponse"
    "fixture/CreateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBot)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseCreateChannelBan :: CreateChannelBanResponse -> TestTree
responseCreateChannelBan =
  res
    "CreateChannelBanResponse"
    "fixture/CreateChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelBan)

responseCreateChannelMembership :: CreateChannelMembershipResponse -> TestTree
responseCreateChannelMembership =
  res
    "CreateChannelMembershipResponse"
    "fixture/CreateChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelMembership)

responseCreateChannelModerator :: CreateChannelModeratorResponse -> TestTree
responseCreateChannelModerator =
  res
    "CreateChannelModeratorResponse"
    "fixture/CreateChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannelModerator)

responseCreateMediaCapturePipeline :: CreateMediaCapturePipelineResponse -> TestTree
responseCreateMediaCapturePipeline =
  res
    "CreateMediaCapturePipelineResponse"
    "fixture/CreateMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMediaCapturePipeline)

responseCreateMeeting :: CreateMeetingResponse -> TestTree
responseCreateMeeting =
  res
    "CreateMeetingResponse"
    "fixture/CreateMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMeeting)

responseCreateMeetingDialOut :: CreateMeetingDialOutResponse -> TestTree
responseCreateMeetingDialOut =
  res
    "CreateMeetingDialOutResponse"
    "fixture/CreateMeetingDialOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMeetingDialOut)

responseCreateMeetingWithAttendees :: CreateMeetingWithAttendeesResponse -> TestTree
responseCreateMeetingWithAttendees =
  res
    "CreateMeetingWithAttendeesResponse"
    "fixture/CreateMeetingWithAttendeesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMeetingWithAttendees)

responseCreatePhoneNumberOrder :: CreatePhoneNumberOrderResponse -> TestTree
responseCreatePhoneNumberOrder =
  res
    "CreatePhoneNumberOrderResponse"
    "fixture/CreatePhoneNumberOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePhoneNumberOrder)

responseCreateProxySession :: CreateProxySessionResponse -> TestTree
responseCreateProxySession =
  res
    "CreateProxySessionResponse"
    "fixture/CreateProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProxySession)

responseCreateRoom :: CreateRoomResponse -> TestTree
responseCreateRoom =
  res
    "CreateRoomResponse"
    "fixture/CreateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoom)

responseCreateRoomMembership :: CreateRoomMembershipResponse -> TestTree
responseCreateRoomMembership =
  res
    "CreateRoomMembershipResponse"
    "fixture/CreateRoomMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoomMembership)

responseCreateSipMediaApplication :: CreateSipMediaApplicationResponse -> TestTree
responseCreateSipMediaApplication =
  res
    "CreateSipMediaApplicationResponse"
    "fixture/CreateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipMediaApplication)

responseCreateSipMediaApplicationCall :: CreateSipMediaApplicationCallResponse -> TestTree
responseCreateSipMediaApplicationCall =
  res
    "CreateSipMediaApplicationCallResponse"
    "fixture/CreateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipMediaApplicationCall)

responseCreateSipRule :: CreateSipRuleResponse -> TestTree
responseCreateSipRule =
  res
    "CreateSipRuleResponse"
    "fixture/CreateSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipRule)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseCreateVoiceConnector :: CreateVoiceConnectorResponse -> TestTree
responseCreateVoiceConnector =
  res
    "CreateVoiceConnectorResponse"
    "fixture/CreateVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceConnector)

responseCreateVoiceConnectorGroup :: CreateVoiceConnectorGroupResponse -> TestTree
responseCreateVoiceConnectorGroup =
  res
    "CreateVoiceConnectorGroupResponse"
    "fixture/CreateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceConnectorGroup)

responseDeleteAccount :: DeleteAccountResponse -> TestTree
responseDeleteAccount =
  res
    "DeleteAccountResponse"
    "fixture/DeleteAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccount)

responseDeleteAppInstance :: DeleteAppInstanceResponse -> TestTree
responseDeleteAppInstance =
  res
    "DeleteAppInstanceResponse"
    "fixture/DeleteAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstance)

responseDeleteAppInstanceAdmin :: DeleteAppInstanceAdminResponse -> TestTree
responseDeleteAppInstanceAdmin =
  res
    "DeleteAppInstanceAdminResponse"
    "fixture/DeleteAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceAdmin)

responseDeleteAppInstanceStreamingConfigurations :: DeleteAppInstanceStreamingConfigurationsResponse -> TestTree
responseDeleteAppInstanceStreamingConfigurations =
  res
    "DeleteAppInstanceStreamingConfigurationsResponse"
    "fixture/DeleteAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceStreamingConfigurations)

responseDeleteAppInstanceUser :: DeleteAppInstanceUserResponse -> TestTree
responseDeleteAppInstanceUser =
  res
    "DeleteAppInstanceUserResponse"
    "fixture/DeleteAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInstanceUser)

responseDeleteAttendee :: DeleteAttendeeResponse -> TestTree
responseDeleteAttendee =
  res
    "DeleteAttendeeResponse"
    "fixture/DeleteAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttendee)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeleteChannelBan :: DeleteChannelBanResponse -> TestTree
responseDeleteChannelBan =
  res
    "DeleteChannelBanResponse"
    "fixture/DeleteChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelBan)

responseDeleteChannelMembership :: DeleteChannelMembershipResponse -> TestTree
responseDeleteChannelMembership =
  res
    "DeleteChannelMembershipResponse"
    "fixture/DeleteChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMembership)

responseDeleteChannelMessage :: DeleteChannelMessageResponse -> TestTree
responseDeleteChannelMessage =
  res
    "DeleteChannelMessageResponse"
    "fixture/DeleteChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelMessage)

responseDeleteChannelModerator :: DeleteChannelModeratorResponse -> TestTree
responseDeleteChannelModerator =
  res
    "DeleteChannelModeratorResponse"
    "fixture/DeleteChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelModerator)

responseDeleteEventsConfiguration :: DeleteEventsConfigurationResponse -> TestTree
responseDeleteEventsConfiguration =
  res
    "DeleteEventsConfigurationResponse"
    "fixture/DeleteEventsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventsConfiguration)

responseDeleteMediaCapturePipeline :: DeleteMediaCapturePipelineResponse -> TestTree
responseDeleteMediaCapturePipeline =
  res
    "DeleteMediaCapturePipelineResponse"
    "fixture/DeleteMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMediaCapturePipeline)

responseDeleteMeeting :: DeleteMeetingResponse -> TestTree
responseDeleteMeeting =
  res
    "DeleteMeetingResponse"
    "fixture/DeleteMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMeeting)

responseDeletePhoneNumber :: DeletePhoneNumberResponse -> TestTree
responseDeletePhoneNumber =
  res
    "DeletePhoneNumberResponse"
    "fixture/DeletePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePhoneNumber)

responseDeleteProxySession :: DeleteProxySessionResponse -> TestTree
responseDeleteProxySession =
  res
    "DeleteProxySessionResponse"
    "fixture/DeleteProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProxySession)

responseDeleteRoom :: DeleteRoomResponse -> TestTree
responseDeleteRoom =
  res
    "DeleteRoomResponse"
    "fixture/DeleteRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoom)

responseDeleteRoomMembership :: DeleteRoomMembershipResponse -> TestTree
responseDeleteRoomMembership =
  res
    "DeleteRoomMembershipResponse"
    "fixture/DeleteRoomMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoomMembership)

responseDeleteSipMediaApplication :: DeleteSipMediaApplicationResponse -> TestTree
responseDeleteSipMediaApplication =
  res
    "DeleteSipMediaApplicationResponse"
    "fixture/DeleteSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSipMediaApplication)

responseDeleteSipRule :: DeleteSipRuleResponse -> TestTree
responseDeleteSipRule =
  res
    "DeleteSipRuleResponse"
    "fixture/DeleteSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSipRule)

responseDeleteVoiceConnector :: DeleteVoiceConnectorResponse -> TestTree
responseDeleteVoiceConnector =
  res
    "DeleteVoiceConnectorResponse"
    "fixture/DeleteVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnector)

responseDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorEmergencyCallingConfiguration =
  res
    "DeleteVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorEmergencyCallingConfiguration)

responseDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroupResponse -> TestTree
responseDeleteVoiceConnectorGroup =
  res
    "DeleteVoiceConnectorGroupResponse"
    "fixture/DeleteVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorGroup)

responseDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOriginationResponse -> TestTree
responseDeleteVoiceConnectorOrigination =
  res
    "DeleteVoiceConnectorOriginationResponse"
    "fixture/DeleteVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorOrigination)

responseDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxyResponse -> TestTree
responseDeleteVoiceConnectorProxy =
  res
    "DeleteVoiceConnectorProxyResponse"
    "fixture/DeleteVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorProxy)

responseDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorStreamingConfiguration =
  res
    "DeleteVoiceConnectorStreamingConfigurationResponse"
    "fixture/DeleteVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorStreamingConfiguration)

responseDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTerminationResponse -> TestTree
responseDeleteVoiceConnectorTermination =
  res
    "DeleteVoiceConnectorTerminationResponse"
    "fixture/DeleteVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorTermination)

responseDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentialsResponse -> TestTree
responseDeleteVoiceConnectorTerminationCredentials =
  res
    "DeleteVoiceConnectorTerminationCredentialsResponse"
    "fixture/DeleteVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorTerminationCredentials)

responseDescribeAppInstance :: DescribeAppInstanceResponse -> TestTree
responseDescribeAppInstance =
  res
    "DescribeAppInstanceResponse"
    "fixture/DescribeAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstance)

responseDescribeAppInstanceAdmin :: DescribeAppInstanceAdminResponse -> TestTree
responseDescribeAppInstanceAdmin =
  res
    "DescribeAppInstanceAdminResponse"
    "fixture/DescribeAppInstanceAdminResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceAdmin)

responseDescribeAppInstanceUser :: DescribeAppInstanceUserResponse -> TestTree
responseDescribeAppInstanceUser =
  res
    "DescribeAppInstanceUserResponse"
    "fixture/DescribeAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppInstanceUser)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseDescribeChannelBan :: DescribeChannelBanResponse -> TestTree
responseDescribeChannelBan =
  res
    "DescribeChannelBanResponse"
    "fixture/DescribeChannelBanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelBan)

responseDescribeChannelMembership :: DescribeChannelMembershipResponse -> TestTree
responseDescribeChannelMembership =
  res
    "DescribeChannelMembershipResponse"
    "fixture/DescribeChannelMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembership)

responseDescribeChannelMembershipForAppInstanceUser :: DescribeChannelMembershipForAppInstanceUserResponse -> TestTree
responseDescribeChannelMembershipForAppInstanceUser =
  res
    "DescribeChannelMembershipForAppInstanceUserResponse"
    "fixture/DescribeChannelMembershipForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelMembershipForAppInstanceUser)

responseDescribeChannelModeratedByAppInstanceUser :: DescribeChannelModeratedByAppInstanceUserResponse -> TestTree
responseDescribeChannelModeratedByAppInstanceUser =
  res
    "DescribeChannelModeratedByAppInstanceUserResponse"
    "fixture/DescribeChannelModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModeratedByAppInstanceUser)

responseDescribeChannelModerator :: DescribeChannelModeratorResponse -> TestTree
responseDescribeChannelModerator =
  res
    "DescribeChannelModeratorResponse"
    "fixture/DescribeChannelModeratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannelModerator)

responseDisassociatePhoneNumberFromUser :: DisassociatePhoneNumberFromUserResponse -> TestTree
responseDisassociatePhoneNumberFromUser =
  res
    "DisassociatePhoneNumberFromUserResponse"
    "fixture/DisassociatePhoneNumberFromUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumberFromUser)

responseDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnectorResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnector =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumbersFromVoiceConnector)

responseDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroupResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnectorGroup =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorGroupResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumbersFromVoiceConnectorGroup)

responseDisassociateSigninDelegateGroupsFromAccount :: DisassociateSigninDelegateGroupsFromAccountResponse -> TestTree
responseDisassociateSigninDelegateGroupsFromAccount =
  res
    "DisassociateSigninDelegateGroupsFromAccountResponse"
    "fixture/DisassociateSigninDelegateGroupsFromAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateSigninDelegateGroupsFromAccount)

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccount)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetAppInstanceRetentionSettings :: GetAppInstanceRetentionSettingsResponse -> TestTree
responseGetAppInstanceRetentionSettings =
  res
    "GetAppInstanceRetentionSettingsResponse"
    "fixture/GetAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppInstanceRetentionSettings)

responseGetAppInstanceStreamingConfigurations :: GetAppInstanceStreamingConfigurationsResponse -> TestTree
responseGetAppInstanceStreamingConfigurations =
  res
    "GetAppInstanceStreamingConfigurationsResponse"
    "fixture/GetAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppInstanceStreamingConfigurations)

responseGetAttendee :: GetAttendeeResponse -> TestTree
responseGetAttendee =
  res
    "GetAttendeeResponse"
    "fixture/GetAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttendee)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBot)

responseGetChannelMessage :: GetChannelMessageResponse -> TestTree
responseGetChannelMessage =
  res
    "GetChannelMessageResponse"
    "fixture/GetChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelMessage)

responseGetEventsConfiguration :: GetEventsConfigurationResponse -> TestTree
responseGetEventsConfiguration =
  res
    "GetEventsConfigurationResponse"
    "fixture/GetEventsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventsConfiguration)

responseGetGlobalSettings :: GetGlobalSettingsResponse -> TestTree
responseGetGlobalSettings =
  res
    "GetGlobalSettingsResponse"
    "fixture/GetGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGlobalSettings)

responseGetMediaCapturePipeline :: GetMediaCapturePipelineResponse -> TestTree
responseGetMediaCapturePipeline =
  res
    "GetMediaCapturePipelineResponse"
    "fixture/GetMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMediaCapturePipeline)

responseGetMeeting :: GetMeetingResponse -> TestTree
responseGetMeeting =
  res
    "GetMeetingResponse"
    "fixture/GetMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMeeting)

responseGetMessagingSessionEndpoint :: GetMessagingSessionEndpointResponse -> TestTree
responseGetMessagingSessionEndpoint =
  res
    "GetMessagingSessionEndpointResponse"
    "fixture/GetMessagingSessionEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMessagingSessionEndpoint)

responseGetPhoneNumber :: GetPhoneNumberResponse -> TestTree
responseGetPhoneNumber =
  res
    "GetPhoneNumberResponse"
    "fixture/GetPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumber)

responseGetPhoneNumberOrder :: GetPhoneNumberOrderResponse -> TestTree
responseGetPhoneNumberOrder =
  res
    "GetPhoneNumberOrderResponse"
    "fixture/GetPhoneNumberOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumberOrder)

responseGetPhoneNumberSettings :: GetPhoneNumberSettingsResponse -> TestTree
responseGetPhoneNumberSettings =
  res
    "GetPhoneNumberSettingsResponse"
    "fixture/GetPhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumberSettings)

responseGetProxySession :: GetProxySessionResponse -> TestTree
responseGetProxySession =
  res
    "GetProxySessionResponse"
    "fixture/GetProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProxySession)

responseGetRetentionSettings :: GetRetentionSettingsResponse -> TestTree
responseGetRetentionSettings =
  res
    "GetRetentionSettingsResponse"
    "fixture/GetRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRetentionSettings)

responseGetRoom :: GetRoomResponse -> TestTree
responseGetRoom =
  res
    "GetRoomResponse"
    "fixture/GetRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoom)

responseGetSipMediaApplication :: GetSipMediaApplicationResponse -> TestTree
responseGetSipMediaApplication =
  res
    "GetSipMediaApplicationResponse"
    "fixture/GetSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipMediaApplication)

responseGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfigurationResponse -> TestTree
responseGetSipMediaApplicationLoggingConfiguration =
  res
    "GetSipMediaApplicationLoggingConfigurationResponse"
    "fixture/GetSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipMediaApplicationLoggingConfiguration)

responseGetSipRule :: GetSipRuleResponse -> TestTree
responseGetSipRule =
  res
    "GetSipRuleResponse"
    "fixture/GetSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipRule)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUser)

responseGetUserSettings :: GetUserSettingsResponse -> TestTree
responseGetUserSettings =
  res
    "GetUserSettingsResponse"
    "fixture/GetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserSettings)

responseGetVoiceConnector :: GetVoiceConnectorResponse -> TestTree
responseGetVoiceConnector =
  res
    "GetVoiceConnectorResponse"
    "fixture/GetVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnector)

responseGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseGetVoiceConnectorEmergencyCallingConfiguration =
  res
    "GetVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/GetVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorEmergencyCallingConfiguration)

responseGetVoiceConnectorGroup :: GetVoiceConnectorGroupResponse -> TestTree
responseGetVoiceConnectorGroup =
  res
    "GetVoiceConnectorGroupResponse"
    "fixture/GetVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorGroup)

responseGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfigurationResponse -> TestTree
responseGetVoiceConnectorLoggingConfiguration =
  res
    "GetVoiceConnectorLoggingConfigurationResponse"
    "fixture/GetVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorLoggingConfiguration)

responseGetVoiceConnectorOrigination :: GetVoiceConnectorOriginationResponse -> TestTree
responseGetVoiceConnectorOrigination =
  res
    "GetVoiceConnectorOriginationResponse"
    "fixture/GetVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorOrigination)

responseGetVoiceConnectorProxy :: GetVoiceConnectorProxyResponse -> TestTree
responseGetVoiceConnectorProxy =
  res
    "GetVoiceConnectorProxyResponse"
    "fixture/GetVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorProxy)

responseGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfigurationResponse -> TestTree
responseGetVoiceConnectorStreamingConfiguration =
  res
    "GetVoiceConnectorStreamingConfigurationResponse"
    "fixture/GetVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorStreamingConfiguration)

responseGetVoiceConnectorTermination :: GetVoiceConnectorTerminationResponse -> TestTree
responseGetVoiceConnectorTermination =
  res
    "GetVoiceConnectorTerminationResponse"
    "fixture/GetVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorTermination)

responseGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealthResponse -> TestTree
responseGetVoiceConnectorTerminationHealth =
  res
    "GetVoiceConnectorTerminationHealthResponse"
    "fixture/GetVoiceConnectorTerminationHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorTerminationHealth)

responseInviteUsers :: InviteUsersResponse -> TestTree
responseInviteUsers =
  res
    "InviteUsersResponse"
    "fixture/InviteUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteUsers)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccounts)

responseListAppInstanceAdmins :: ListAppInstanceAdminsResponse -> TestTree
responseListAppInstanceAdmins =
  res
    "ListAppInstanceAdminsResponse"
    "fixture/ListAppInstanceAdminsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceAdmins)

responseListAppInstanceUsers :: ListAppInstanceUsersResponse -> TestTree
responseListAppInstanceUsers =
  res
    "ListAppInstanceUsersResponse"
    "fixture/ListAppInstanceUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstanceUsers)

responseListAppInstances :: ListAppInstancesResponse -> TestTree
responseListAppInstances =
  res
    "ListAppInstancesResponse"
    "fixture/ListAppInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInstances)

responseListAttendeeTags :: ListAttendeeTagsResponse -> TestTree
responseListAttendeeTags =
  res
    "ListAttendeeTagsResponse"
    "fixture/ListAttendeeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttendeeTags)

responseListAttendees :: ListAttendeesResponse -> TestTree
responseListAttendees =
  res
    "ListAttendeesResponse"
    "fixture/ListAttendeesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttendees)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBots)

responseListChannelBans :: ListChannelBansResponse -> TestTree
responseListChannelBans =
  res
    "ListChannelBansResponse"
    "fixture/ListChannelBansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelBans)

responseListChannelMemberships :: ListChannelMembershipsResponse -> TestTree
responseListChannelMemberships =
  res
    "ListChannelMembershipsResponse"
    "fixture/ListChannelMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMemberships)

responseListChannelMembershipsForAppInstanceUser :: ListChannelMembershipsForAppInstanceUserResponse -> TestTree
responseListChannelMembershipsForAppInstanceUser =
  res
    "ListChannelMembershipsForAppInstanceUserResponse"
    "fixture/ListChannelMembershipsForAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMembershipsForAppInstanceUser)

responseListChannelMessages :: ListChannelMessagesResponse -> TestTree
responseListChannelMessages =
  res
    "ListChannelMessagesResponse"
    "fixture/ListChannelMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelMessages)

responseListChannelModerators :: ListChannelModeratorsResponse -> TestTree
responseListChannelModerators =
  res
    "ListChannelModeratorsResponse"
    "fixture/ListChannelModeratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelModerators)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListChannelsModeratedByAppInstanceUser :: ListChannelsModeratedByAppInstanceUserResponse -> TestTree
responseListChannelsModeratedByAppInstanceUser =
  res
    "ListChannelsModeratedByAppInstanceUserResponse"
    "fixture/ListChannelsModeratedByAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannelsModeratedByAppInstanceUser)

responseListMediaCapturePipelines :: ListMediaCapturePipelinesResponse -> TestTree
responseListMediaCapturePipelines =
  res
    "ListMediaCapturePipelinesResponse"
    "fixture/ListMediaCapturePipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMediaCapturePipelines)

responseListMeetingTags :: ListMeetingTagsResponse -> TestTree
responseListMeetingTags =
  res
    "ListMeetingTagsResponse"
    "fixture/ListMeetingTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMeetingTags)

responseListMeetings :: ListMeetingsResponse -> TestTree
responseListMeetings =
  res
    "ListMeetingsResponse"
    "fixture/ListMeetingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMeetings)

responseListPhoneNumberOrders :: ListPhoneNumberOrdersResponse -> TestTree
responseListPhoneNumberOrders =
  res
    "ListPhoneNumberOrdersResponse"
    "fixture/ListPhoneNumberOrdersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumberOrders)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbers)

responseListProxySessions :: ListProxySessionsResponse -> TestTree
responseListProxySessions =
  res
    "ListProxySessionsResponse"
    "fixture/ListProxySessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProxySessions)

responseListRoomMemberships :: ListRoomMembershipsResponse -> TestTree
responseListRoomMemberships =
  res
    "ListRoomMembershipsResponse"
    "fixture/ListRoomMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoomMemberships)

responseListRooms :: ListRoomsResponse -> TestTree
responseListRooms =
  res
    "ListRoomsResponse"
    "fixture/ListRoomsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRooms)

responseListSipMediaApplications :: ListSipMediaApplicationsResponse -> TestTree
responseListSipMediaApplications =
  res
    "ListSipMediaApplicationsResponse"
    "fixture/ListSipMediaApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSipMediaApplications)

responseListSipRules :: ListSipRulesResponse -> TestTree
responseListSipRules =
  res
    "ListSipRulesResponse"
    "fixture/ListSipRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSipRules)

responseListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountriesResponse -> TestTree
responseListSupportedPhoneNumberCountries =
  res
    "ListSupportedPhoneNumberCountriesResponse"
    "fixture/ListSupportedPhoneNumberCountriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSupportedPhoneNumberCountries)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListVoiceConnectorGroups :: ListVoiceConnectorGroupsResponse -> TestTree
responseListVoiceConnectorGroups =
  res
    "ListVoiceConnectorGroupsResponse"
    "fixture/ListVoiceConnectorGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectorGroups)

responseListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentialsResponse -> TestTree
responseListVoiceConnectorTerminationCredentials =
  res
    "ListVoiceConnectorTerminationCredentialsResponse"
    "fixture/ListVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectorTerminationCredentials)

responseListVoiceConnectors :: ListVoiceConnectorsResponse -> TestTree
responseListVoiceConnectors =
  res
    "ListVoiceConnectorsResponse"
    "fixture/ListVoiceConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectors)

responseLogoutUser :: LogoutUserResponse -> TestTree
responseLogoutUser =
  res
    "LogoutUserResponse"
    "fixture/LogoutUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LogoutUser)

responsePutAppInstanceRetentionSettings :: PutAppInstanceRetentionSettingsResponse -> TestTree
responsePutAppInstanceRetentionSettings =
  res
    "PutAppInstanceRetentionSettingsResponse"
    "fixture/PutAppInstanceRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppInstanceRetentionSettings)

responsePutAppInstanceStreamingConfigurations :: PutAppInstanceStreamingConfigurationsResponse -> TestTree
responsePutAppInstanceStreamingConfigurations =
  res
    "PutAppInstanceStreamingConfigurationsResponse"
    "fixture/PutAppInstanceStreamingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAppInstanceStreamingConfigurations)

responsePutEventsConfiguration :: PutEventsConfigurationResponse -> TestTree
responsePutEventsConfiguration =
  res
    "PutEventsConfigurationResponse"
    "fixture/PutEventsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventsConfiguration)

responsePutRetentionSettings :: PutRetentionSettingsResponse -> TestTree
responsePutRetentionSettings =
  res
    "PutRetentionSettingsResponse"
    "fixture/PutRetentionSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionSettings)

responsePutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfigurationResponse -> TestTree
responsePutSipMediaApplicationLoggingConfiguration =
  res
    "PutSipMediaApplicationLoggingConfigurationResponse"
    "fixture/PutSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSipMediaApplicationLoggingConfiguration)

responsePutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responsePutVoiceConnectorEmergencyCallingConfiguration =
  res
    "PutVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/PutVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorEmergencyCallingConfiguration)

responsePutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfigurationResponse -> TestTree
responsePutVoiceConnectorLoggingConfiguration =
  res
    "PutVoiceConnectorLoggingConfigurationResponse"
    "fixture/PutVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorLoggingConfiguration)

responsePutVoiceConnectorOrigination :: PutVoiceConnectorOriginationResponse -> TestTree
responsePutVoiceConnectorOrigination =
  res
    "PutVoiceConnectorOriginationResponse"
    "fixture/PutVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorOrigination)

responsePutVoiceConnectorProxy :: PutVoiceConnectorProxyResponse -> TestTree
responsePutVoiceConnectorProxy =
  res
    "PutVoiceConnectorProxyResponse"
    "fixture/PutVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorProxy)

responsePutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfigurationResponse -> TestTree
responsePutVoiceConnectorStreamingConfiguration =
  res
    "PutVoiceConnectorStreamingConfigurationResponse"
    "fixture/PutVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorStreamingConfiguration)

responsePutVoiceConnectorTermination :: PutVoiceConnectorTerminationResponse -> TestTree
responsePutVoiceConnectorTermination =
  res
    "PutVoiceConnectorTerminationResponse"
    "fixture/PutVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorTermination)

responsePutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentialsResponse -> TestTree
responsePutVoiceConnectorTerminationCredentials =
  res
    "PutVoiceConnectorTerminationCredentialsResponse"
    "fixture/PutVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorTerminationCredentials)

responseRedactChannelMessage :: RedactChannelMessageResponse -> TestTree
responseRedactChannelMessage =
  res
    "RedactChannelMessageResponse"
    "fixture/RedactChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RedactChannelMessage)

responseRedactConversationMessage :: RedactConversationMessageResponse -> TestTree
responseRedactConversationMessage =
  res
    "RedactConversationMessageResponse"
    "fixture/RedactConversationMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RedactConversationMessage)

responseRedactRoomMessage :: RedactRoomMessageResponse -> TestTree
responseRedactRoomMessage =
  res
    "RedactRoomMessageResponse"
    "fixture/RedactRoomMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RedactRoomMessage)

responseRegenerateSecurityToken :: RegenerateSecurityTokenResponse -> TestTree
responseRegenerateSecurityToken =
  res
    "RegenerateSecurityTokenResponse"
    "fixture/RegenerateSecurityTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegenerateSecurityToken)

responseResetPersonalPIN :: ResetPersonalPINResponse -> TestTree
responseResetPersonalPIN =
  res
    "ResetPersonalPINResponse"
    "fixture/ResetPersonalPINResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetPersonalPIN)

responseRestorePhoneNumber :: RestorePhoneNumberResponse -> TestTree
responseRestorePhoneNumber =
  res
    "RestorePhoneNumberResponse"
    "fixture/RestorePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestorePhoneNumber)

responseSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbersResponse -> TestTree
responseSearchAvailablePhoneNumbers =
  res
    "SearchAvailablePhoneNumbersResponse"
    "fixture/SearchAvailablePhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAvailablePhoneNumbers)

responseSendChannelMessage :: SendChannelMessageResponse -> TestTree
responseSendChannelMessage =
  res
    "SendChannelMessageResponse"
    "fixture/SendChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendChannelMessage)

responseStartMeetingTranscription :: StartMeetingTranscriptionResponse -> TestTree
responseStartMeetingTranscription =
  res
    "StartMeetingTranscriptionResponse"
    "fixture/StartMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMeetingTranscription)

responseStopMeetingTranscription :: StopMeetingTranscriptionResponse -> TestTree
responseStopMeetingTranscription =
  res
    "StopMeetingTranscriptionResponse"
    "fixture/StopMeetingTranscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMeetingTranscription)

responseTagAttendee :: TagAttendeeResponse -> TestTree
responseTagAttendee =
  res
    "TagAttendeeResponse"
    "fixture/TagAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagAttendee)

responseTagMeeting :: TagMeetingResponse -> TestTree
responseTagMeeting =
  res
    "TagMeetingResponse"
    "fixture/TagMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagMeeting)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagAttendee :: UntagAttendeeResponse -> TestTree
responseUntagAttendee =
  res
    "UntagAttendeeResponse"
    "fixture/UntagAttendeeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagAttendee)

responseUntagMeeting :: UntagMeetingResponse -> TestTree
responseUntagMeeting =
  res
    "UntagMeetingResponse"
    "fixture/UntagMeetingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagMeeting)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccount :: UpdateAccountResponse -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccount)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseUpdateAppInstance :: UpdateAppInstanceResponse -> TestTree
responseUpdateAppInstance =
  res
    "UpdateAppInstanceResponse"
    "fixture/UpdateAppInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstance)

responseUpdateAppInstanceUser :: UpdateAppInstanceUserResponse -> TestTree
responseUpdateAppInstanceUser =
  res
    "UpdateAppInstanceUserResponse"
    "fixture/UpdateAppInstanceUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppInstanceUser)

responseUpdateBot :: UpdateBotResponse -> TestTree
responseUpdateBot =
  res
    "UpdateBotResponse"
    "fixture/UpdateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBot)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseUpdateChannelMessage :: UpdateChannelMessageResponse -> TestTree
responseUpdateChannelMessage =
  res
    "UpdateChannelMessageResponse"
    "fixture/UpdateChannelMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelMessage)

responseUpdateChannelReadMarker :: UpdateChannelReadMarkerResponse -> TestTree
responseUpdateChannelReadMarker =
  res
    "UpdateChannelReadMarkerResponse"
    "fixture/UpdateChannelReadMarkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelReadMarker)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalSettings)

responseUpdatePhoneNumber :: UpdatePhoneNumberResponse -> TestTree
responseUpdatePhoneNumber =
  res
    "UpdatePhoneNumberResponse"
    "fixture/UpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumber)

responseUpdatePhoneNumberSettings :: UpdatePhoneNumberSettingsResponse -> TestTree
responseUpdatePhoneNumberSettings =
  res
    "UpdatePhoneNumberSettingsResponse"
    "fixture/UpdatePhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumberSettings)

responseUpdateProxySession :: UpdateProxySessionResponse -> TestTree
responseUpdateProxySession =
  res
    "UpdateProxySessionResponse"
    "fixture/UpdateProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProxySession)

responseUpdateRoom :: UpdateRoomResponse -> TestTree
responseUpdateRoom =
  res
    "UpdateRoomResponse"
    "fixture/UpdateRoomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoom)

responseUpdateRoomMembership :: UpdateRoomMembershipResponse -> TestTree
responseUpdateRoomMembership =
  res
    "UpdateRoomMembershipResponse"
    "fixture/UpdateRoomMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoomMembership)

responseUpdateSipMediaApplication :: UpdateSipMediaApplicationResponse -> TestTree
responseUpdateSipMediaApplication =
  res
    "UpdateSipMediaApplicationResponse"
    "fixture/UpdateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipMediaApplication)

responseUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCallResponse -> TestTree
responseUpdateSipMediaApplicationCall =
  res
    "UpdateSipMediaApplicationCallResponse"
    "fixture/UpdateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipMediaApplicationCall)

responseUpdateSipRule :: UpdateSipRuleResponse -> TestTree
responseUpdateSipRule =
  res
    "UpdateSipRuleResponse"
    "fixture/UpdateSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipRule)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)

responseUpdateUserSettings :: UpdateUserSettingsResponse -> TestTree
responseUpdateUserSettings =
  res
    "UpdateUserSettingsResponse"
    "fixture/UpdateUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserSettings)

responseUpdateVoiceConnector :: UpdateVoiceConnectorResponse -> TestTree
responseUpdateVoiceConnector =
  res
    "UpdateVoiceConnectorResponse"
    "fixture/UpdateVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceConnector)

responseUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroupResponse -> TestTree
responseUpdateVoiceConnectorGroup =
  res
    "UpdateVoiceConnectorGroupResponse"
    "fixture/UpdateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceConnectorGroup)

responseValidateE911Address :: ValidateE911AddressResponse -> TestTree
responseValidateE911Address =
  res
    "ValidateE911AddressResponse"
    "fixture/ValidateE911AddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateE911Address)

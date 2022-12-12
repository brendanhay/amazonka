{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Lens
  ( -- * Operations

    -- ** AssociatePhoneNumberWithUser
    associatePhoneNumberWithUser_accountId,
    associatePhoneNumberWithUser_userId,
    associatePhoneNumberWithUser_e164PhoneNumber,
    associatePhoneNumberWithUserResponse_httpStatus,

    -- ** AssociatePhoneNumbersWithVoiceConnector
    associatePhoneNumbersWithVoiceConnector_forceAssociate,
    associatePhoneNumbersWithVoiceConnector_voiceConnectorId,
    associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorResponse_httpStatus,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate,
    associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId,
    associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus,

    -- ** AssociateSigninDelegateGroupsWithAccount
    associateSigninDelegateGroupsWithAccount_accountId,
    associateSigninDelegateGroupsWithAccount_signinDelegateGroups,
    associateSigninDelegateGroupsWithAccountResponse_httpStatus,

    -- ** BatchCreateAttendee
    batchCreateAttendee_meetingId,
    batchCreateAttendee_attendees,
    batchCreateAttendeeResponse_attendees,
    batchCreateAttendeeResponse_errors,
    batchCreateAttendeeResponse_httpStatus,

    -- ** BatchCreateChannelMembership
    batchCreateChannelMembership_chimeBearer,
    batchCreateChannelMembership_type,
    batchCreateChannelMembership_channelArn,
    batchCreateChannelMembership_memberArns,
    batchCreateChannelMembershipResponse_batchChannelMemberships,
    batchCreateChannelMembershipResponse_errors,
    batchCreateChannelMembershipResponse_httpStatus,

    -- ** BatchCreateRoomMembership
    batchCreateRoomMembership_accountId,
    batchCreateRoomMembership_roomId,
    batchCreateRoomMembership_membershipItemList,
    batchCreateRoomMembershipResponse_errors,
    batchCreateRoomMembershipResponse_httpStatus,

    -- ** BatchDeletePhoneNumber
    batchDeletePhoneNumber_phoneNumberIds,
    batchDeletePhoneNumberResponse_phoneNumberErrors,
    batchDeletePhoneNumberResponse_httpStatus,

    -- ** BatchSuspendUser
    batchSuspendUser_accountId,
    batchSuspendUser_userIdList,
    batchSuspendUserResponse_userErrors,
    batchSuspendUserResponse_httpStatus,

    -- ** BatchUnsuspendUser
    batchUnsuspendUser_accountId,
    batchUnsuspendUser_userIdList,
    batchUnsuspendUserResponse_userErrors,
    batchUnsuspendUserResponse_httpStatus,

    -- ** BatchUpdatePhoneNumber
    batchUpdatePhoneNumber_updatePhoneNumberRequestItems,
    batchUpdatePhoneNumberResponse_phoneNumberErrors,
    batchUpdatePhoneNumberResponse_httpStatus,

    -- ** BatchUpdateUser
    batchUpdateUser_accountId,
    batchUpdateUser_updateUserRequestItems,
    batchUpdateUserResponse_userErrors,
    batchUpdateUserResponse_httpStatus,

    -- ** CreateAccount
    createAccount_name,
    createAccountResponse_account,
    createAccountResponse_httpStatus,

    -- ** CreateAppInstance
    createAppInstance_metadata,
    createAppInstance_tags,
    createAppInstance_name,
    createAppInstance_clientRequestToken,
    createAppInstanceResponse_appInstanceArn,
    createAppInstanceResponse_httpStatus,

    -- ** CreateAppInstanceAdmin
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_httpStatus,

    -- ** CreateAppInstanceUser
    createAppInstanceUser_metadata,
    createAppInstanceUser_tags,
    createAppInstanceUser_appInstanceArn,
    createAppInstanceUser_appInstanceUserId,
    createAppInstanceUser_name,
    createAppInstanceUser_clientRequestToken,
    createAppInstanceUserResponse_appInstanceUserArn,
    createAppInstanceUserResponse_httpStatus,

    -- ** CreateAttendee
    createAttendee_tags,
    createAttendee_meetingId,
    createAttendee_externalUserId,
    createAttendeeResponse_attendee,
    createAttendeeResponse_httpStatus,

    -- ** CreateBot
    createBot_domain,
    createBot_displayName,
    createBot_accountId,
    createBotResponse_bot,
    createBotResponse_httpStatus,

    -- ** CreateChannel
    createChannel_chimeBearer,
    createChannel_metadata,
    createChannel_mode,
    createChannel_privacy,
    createChannel_tags,
    createChannel_appInstanceArn,
    createChannel_name,
    createChannel_clientRequestToken,
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,

    -- ** CreateChannelBan
    createChannelBan_chimeBearer,
    createChannelBan_channelArn,
    createChannelBan_memberArn,
    createChannelBanResponse_channelArn,
    createChannelBanResponse_member,
    createChannelBanResponse_httpStatus,

    -- ** CreateChannelMembership
    createChannelMembership_chimeBearer,
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_httpStatus,

    -- ** CreateChannelModerator
    createChannelModerator_chimeBearer,
    createChannelModerator_channelArn,
    createChannelModerator_channelModeratorArn,
    createChannelModeratorResponse_channelArn,
    createChannelModeratorResponse_channelModerator,
    createChannelModeratorResponse_httpStatus,

    -- ** CreateMediaCapturePipeline
    createMediaCapturePipeline_chimeSdkMeetingConfiguration,
    createMediaCapturePipeline_clientRequestToken,
    createMediaCapturePipeline_sourceType,
    createMediaCapturePipeline_sourceArn,
    createMediaCapturePipeline_sinkType,
    createMediaCapturePipeline_sinkArn,
    createMediaCapturePipelineResponse_mediaCapturePipeline,
    createMediaCapturePipelineResponse_httpStatus,

    -- ** CreateMeeting
    createMeeting_externalMeetingId,
    createMeeting_mediaRegion,
    createMeeting_meetingHostId,
    createMeeting_notificationsConfiguration,
    createMeeting_tags,
    createMeeting_clientRequestToken,
    createMeetingResponse_meeting,
    createMeetingResponse_httpStatus,

    -- ** CreateMeetingDialOut
    createMeetingDialOut_meetingId,
    createMeetingDialOut_fromPhoneNumber,
    createMeetingDialOut_toPhoneNumber,
    createMeetingDialOut_joinToken,
    createMeetingDialOutResponse_transactionId,
    createMeetingDialOutResponse_httpStatus,

    -- ** CreateMeetingWithAttendees
    createMeetingWithAttendees_attendees,
    createMeetingWithAttendees_externalMeetingId,
    createMeetingWithAttendees_mediaRegion,
    createMeetingWithAttendees_meetingHostId,
    createMeetingWithAttendees_notificationsConfiguration,
    createMeetingWithAttendees_tags,
    createMeetingWithAttendees_clientRequestToken,
    createMeetingWithAttendeesResponse_attendees,
    createMeetingWithAttendeesResponse_errors,
    createMeetingWithAttendeesResponse_meeting,
    createMeetingWithAttendeesResponse_httpStatus,

    -- ** CreatePhoneNumberOrder
    createPhoneNumberOrder_productType,
    createPhoneNumberOrder_e164PhoneNumbers,
    createPhoneNumberOrderResponse_phoneNumberOrder,
    createPhoneNumberOrderResponse_httpStatus,

    -- ** CreateProxySession
    createProxySession_expiryMinutes,
    createProxySession_geoMatchLevel,
    createProxySession_geoMatchParams,
    createProxySession_name,
    createProxySession_numberSelectionBehavior,
    createProxySession_participantPhoneNumbers,
    createProxySession_capabilities,
    createProxySession_voiceConnectorId,
    createProxySessionResponse_proxySession,
    createProxySessionResponse_httpStatus,

    -- ** CreateRoom
    createRoom_clientRequestToken,
    createRoom_accountId,
    createRoom_name,
    createRoomResponse_room,
    createRoomResponse_httpStatus,

    -- ** CreateRoomMembership
    createRoomMembership_role,
    createRoomMembership_accountId,
    createRoomMembership_roomId,
    createRoomMembership_memberId,
    createRoomMembershipResponse_roomMembership,
    createRoomMembershipResponse_httpStatus,

    -- ** CreateSipMediaApplication
    createSipMediaApplication_awsRegion,
    createSipMediaApplication_name,
    createSipMediaApplication_endpoints,
    createSipMediaApplicationResponse_sipMediaApplication,
    createSipMediaApplicationResponse_httpStatus,

    -- ** CreateSipMediaApplicationCall
    createSipMediaApplicationCall_sipHeaders,
    createSipMediaApplicationCall_fromPhoneNumber,
    createSipMediaApplicationCall_toPhoneNumber,
    createSipMediaApplicationCall_sipMediaApplicationId,
    createSipMediaApplicationCallResponse_sipMediaApplicationCall,
    createSipMediaApplicationCallResponse_httpStatus,

    -- ** CreateSipRule
    createSipRule_disabled,
    createSipRule_name,
    createSipRule_triggerType,
    createSipRule_triggerValue,
    createSipRule_targetApplications,
    createSipRuleResponse_sipRule,
    createSipRuleResponse_httpStatus,

    -- ** CreateUser
    createUser_email,
    createUser_userType,
    createUser_username,
    createUser_accountId,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** CreateVoiceConnector
    createVoiceConnector_awsRegion,
    createVoiceConnector_name,
    createVoiceConnector_requireEncryption,
    createVoiceConnectorResponse_voiceConnector,
    createVoiceConnectorResponse_httpStatus,

    -- ** CreateVoiceConnectorGroup
    createVoiceConnectorGroup_voiceConnectorItems,
    createVoiceConnectorGroup_name,
    createVoiceConnectorGroupResponse_voiceConnectorGroup,
    createVoiceConnectorGroupResponse_httpStatus,

    -- ** DeleteAccount
    deleteAccount_accountId,
    deleteAccountResponse_httpStatus,

    -- ** DeleteAppInstance
    deleteAppInstance_appInstanceArn,

    -- ** DeleteAppInstanceAdmin
    deleteAppInstanceAdmin_appInstanceAdminArn,
    deleteAppInstanceAdmin_appInstanceArn,

    -- ** DeleteAppInstanceStreamingConfigurations
    deleteAppInstanceStreamingConfigurations_appInstanceArn,

    -- ** DeleteAppInstanceUser
    deleteAppInstanceUser_appInstanceUserArn,

    -- ** DeleteAttendee
    deleteAttendee_meetingId,
    deleteAttendee_attendeeId,

    -- ** DeleteChannel
    deleteChannel_chimeBearer,
    deleteChannel_channelArn,

    -- ** DeleteChannelBan
    deleteChannelBan_chimeBearer,
    deleteChannelBan_channelArn,
    deleteChannelBan_memberArn,

    -- ** DeleteChannelMembership
    deleteChannelMembership_chimeBearer,
    deleteChannelMembership_channelArn,
    deleteChannelMembership_memberArn,

    -- ** DeleteChannelMessage
    deleteChannelMessage_chimeBearer,
    deleteChannelMessage_channelArn,
    deleteChannelMessage_messageId,

    -- ** DeleteChannelModerator
    deleteChannelModerator_chimeBearer,
    deleteChannelModerator_channelArn,
    deleteChannelModerator_channelModeratorArn,

    -- ** DeleteEventsConfiguration
    deleteEventsConfiguration_accountId,
    deleteEventsConfiguration_botId,

    -- ** DeleteMediaCapturePipeline
    deleteMediaCapturePipeline_mediaPipelineId,

    -- ** DeleteMeeting
    deleteMeeting_meetingId,

    -- ** DeletePhoneNumber
    deletePhoneNumber_phoneNumberId,

    -- ** DeleteProxySession
    deleteProxySession_voiceConnectorId,
    deleteProxySession_proxySessionId,

    -- ** DeleteRoom
    deleteRoom_accountId,
    deleteRoom_roomId,

    -- ** DeleteRoomMembership
    deleteRoomMembership_accountId,
    deleteRoomMembership_roomId,
    deleteRoomMembership_memberId,

    -- ** DeleteSipMediaApplication
    deleteSipMediaApplication_sipMediaApplicationId,

    -- ** DeleteSipRule
    deleteSipRule_sipRuleId,

    -- ** DeleteVoiceConnector
    deleteVoiceConnector_voiceConnectorId,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,

    -- ** DeleteVoiceConnectorGroup
    deleteVoiceConnectorGroup_voiceConnectorGroupId,

    -- ** DeleteVoiceConnectorOrigination
    deleteVoiceConnectorOrigination_voiceConnectorId,

    -- ** DeleteVoiceConnectorProxy
    deleteVoiceConnectorProxy_voiceConnectorId,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    deleteVoiceConnectorStreamingConfiguration_voiceConnectorId,

    -- ** DeleteVoiceConnectorTermination
    deleteVoiceConnectorTermination_voiceConnectorId,

    -- ** DeleteVoiceConnectorTerminationCredentials
    deleteVoiceConnectorTerminationCredentials_usernames,
    deleteVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** DescribeAppInstance
    describeAppInstance_appInstanceArn,
    describeAppInstanceResponse_appInstance,
    describeAppInstanceResponse_httpStatus,

    -- ** DescribeAppInstanceAdmin
    describeAppInstanceAdmin_appInstanceAdminArn,
    describeAppInstanceAdmin_appInstanceArn,
    describeAppInstanceAdminResponse_appInstanceAdmin,
    describeAppInstanceAdminResponse_httpStatus,

    -- ** DescribeAppInstanceUser
    describeAppInstanceUser_appInstanceUserArn,
    describeAppInstanceUserResponse_appInstanceUser,
    describeAppInstanceUserResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_chimeBearer,
    describeChannel_channelArn,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,

    -- ** DescribeChannelBan
    describeChannelBan_chimeBearer,
    describeChannelBan_channelArn,
    describeChannelBan_memberArn,
    describeChannelBanResponse_channelBan,
    describeChannelBanResponse_httpStatus,

    -- ** DescribeChannelMembership
    describeChannelMembership_chimeBearer,
    describeChannelMembership_channelArn,
    describeChannelMembership_memberArn,
    describeChannelMembershipResponse_channelMembership,
    describeChannelMembershipResponse_httpStatus,

    -- ** DescribeChannelMembershipForAppInstanceUser
    describeChannelMembershipForAppInstanceUser_chimeBearer,
    describeChannelMembershipForAppInstanceUser_channelArn,
    describeChannelMembershipForAppInstanceUser_appInstanceUserArn,
    describeChannelMembershipForAppInstanceUserResponse_channelMembership,
    describeChannelMembershipForAppInstanceUserResponse_httpStatus,

    -- ** DescribeChannelModeratedByAppInstanceUser
    describeChannelModeratedByAppInstanceUser_chimeBearer,
    describeChannelModeratedByAppInstanceUser_channelArn,
    describeChannelModeratedByAppInstanceUser_appInstanceUserArn,
    describeChannelModeratedByAppInstanceUserResponse_channel,
    describeChannelModeratedByAppInstanceUserResponse_httpStatus,

    -- ** DescribeChannelModerator
    describeChannelModerator_chimeBearer,
    describeChannelModerator_channelArn,
    describeChannelModerator_channelModeratorArn,
    describeChannelModeratorResponse_channelModerator,
    describeChannelModeratorResponse_httpStatus,

    -- ** DisassociatePhoneNumberFromUser
    disassociatePhoneNumberFromUser_accountId,
    disassociatePhoneNumberFromUser_userId,
    disassociatePhoneNumberFromUserResponse_httpStatus,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId,
    disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId,
    disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus,

    -- ** DisassociateSigninDelegateGroupsFromAccount
    disassociateSigninDelegateGroupsFromAccount_accountId,
    disassociateSigninDelegateGroupsFromAccount_groupNames,
    disassociateSigninDelegateGroupsFromAccountResponse_httpStatus,

    -- ** GetAccount
    getAccount_accountId,
    getAccountResponse_account,
    getAccountResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettings_accountId,
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** GetAppInstanceRetentionSettings
    getAppInstanceRetentionSettings_appInstanceArn,
    getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    getAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** GetAppInstanceStreamingConfigurations
    getAppInstanceStreamingConfigurations_appInstanceArn,
    getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations,
    getAppInstanceStreamingConfigurationsResponse_httpStatus,

    -- ** GetAttendee
    getAttendee_meetingId,
    getAttendee_attendeeId,
    getAttendeeResponse_attendee,
    getAttendeeResponse_httpStatus,

    -- ** GetBot
    getBot_accountId,
    getBot_botId,
    getBotResponse_bot,
    getBotResponse_httpStatus,

    -- ** GetChannelMessage
    getChannelMessage_chimeBearer,
    getChannelMessage_channelArn,
    getChannelMessage_messageId,
    getChannelMessageResponse_channelMessage,
    getChannelMessageResponse_httpStatus,

    -- ** GetEventsConfiguration
    getEventsConfiguration_accountId,
    getEventsConfiguration_botId,
    getEventsConfigurationResponse_eventsConfiguration,
    getEventsConfigurationResponse_httpStatus,

    -- ** GetGlobalSettings
    getGlobalSettingsResponse_businessCalling,
    getGlobalSettingsResponse_voiceConnector,
    getGlobalSettingsResponse_httpStatus,

    -- ** GetMediaCapturePipeline
    getMediaCapturePipeline_mediaPipelineId,
    getMediaCapturePipelineResponse_mediaCapturePipeline,
    getMediaCapturePipelineResponse_httpStatus,

    -- ** GetMeeting
    getMeeting_meetingId,
    getMeetingResponse_meeting,
    getMeetingResponse_httpStatus,

    -- ** GetMessagingSessionEndpoint
    getMessagingSessionEndpointResponse_endpoint,
    getMessagingSessionEndpointResponse_httpStatus,

    -- ** GetPhoneNumber
    getPhoneNumber_phoneNumberId,
    getPhoneNumberResponse_phoneNumber,
    getPhoneNumberResponse_httpStatus,

    -- ** GetPhoneNumberOrder
    getPhoneNumberOrder_phoneNumberOrderId,
    getPhoneNumberOrderResponse_phoneNumberOrder,
    getPhoneNumberOrderResponse_httpStatus,

    -- ** GetPhoneNumberSettings
    getPhoneNumberSettingsResponse_callingName,
    getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp,
    getPhoneNumberSettingsResponse_httpStatus,

    -- ** GetProxySession
    getProxySession_voiceConnectorId,
    getProxySession_proxySessionId,
    getProxySessionResponse_proxySession,
    getProxySessionResponse_httpStatus,

    -- ** GetRetentionSettings
    getRetentionSettings_accountId,
    getRetentionSettingsResponse_initiateDeletionTimestamp,
    getRetentionSettingsResponse_retentionSettings,
    getRetentionSettingsResponse_httpStatus,

    -- ** GetRoom
    getRoom_accountId,
    getRoom_roomId,
    getRoomResponse_room,
    getRoomResponse_httpStatus,

    -- ** GetSipMediaApplication
    getSipMediaApplication_sipMediaApplicationId,
    getSipMediaApplicationResponse_sipMediaApplication,
    getSipMediaApplicationResponse_httpStatus,

    -- ** GetSipMediaApplicationLoggingConfiguration
    getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    getSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** GetSipRule
    getSipRule_sipRuleId,
    getSipRuleResponse_sipRule,
    getSipRuleResponse_httpStatus,

    -- ** GetUser
    getUser_accountId,
    getUser_userId,
    getUserResponse_user,
    getUserResponse_httpStatus,

    -- ** GetUserSettings
    getUserSettings_accountId,
    getUserSettings_userId,
    getUserSettingsResponse_userSettings,
    getUserSettingsResponse_httpStatus,

    -- ** GetVoiceConnector
    getVoiceConnector_voiceConnectorId,
    getVoiceConnectorResponse_voiceConnector,
    getVoiceConnectorResponse_httpStatus,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    getVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    getVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    getVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorGroup
    getVoiceConnectorGroup_voiceConnectorGroupId,
    getVoiceConnectorGroupResponse_voiceConnectorGroup,
    getVoiceConnectorGroupResponse_httpStatus,

    -- ** GetVoiceConnectorLoggingConfiguration
    getVoiceConnectorLoggingConfiguration_voiceConnectorId,
    getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    getVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorOrigination
    getVoiceConnectorOrigination_voiceConnectorId,
    getVoiceConnectorOriginationResponse_origination,
    getVoiceConnectorOriginationResponse_httpStatus,

    -- ** GetVoiceConnectorProxy
    getVoiceConnectorProxy_voiceConnectorId,
    getVoiceConnectorProxyResponse_proxy,
    getVoiceConnectorProxyResponse_httpStatus,

    -- ** GetVoiceConnectorStreamingConfiguration
    getVoiceConnectorStreamingConfiguration_voiceConnectorId,
    getVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    getVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorTermination
    getVoiceConnectorTermination_voiceConnectorId,
    getVoiceConnectorTerminationResponse_termination,
    getVoiceConnectorTerminationResponse_httpStatus,

    -- ** GetVoiceConnectorTerminationHealth
    getVoiceConnectorTerminationHealth_voiceConnectorId,
    getVoiceConnectorTerminationHealthResponse_terminationHealth,
    getVoiceConnectorTerminationHealthResponse_httpStatus,

    -- ** InviteUsers
    inviteUsers_userType,
    inviteUsers_accountId,
    inviteUsers_userEmailList,
    inviteUsersResponse_invites,
    inviteUsersResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_maxResults,
    listAccounts_name,
    listAccounts_nextToken,
    listAccounts_userEmail,
    listAccountsResponse_accounts,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** ListAppInstanceAdmins
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_appInstanceArn,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_httpStatus,

    -- ** ListAppInstanceUsers
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_appInstanceArn,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_httpStatus,

    -- ** ListAppInstances
    listAppInstances_maxResults,
    listAppInstances_nextToken,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_httpStatus,

    -- ** ListAttendeeTags
    listAttendeeTags_meetingId,
    listAttendeeTags_attendeeId,
    listAttendeeTagsResponse_tags,
    listAttendeeTagsResponse_httpStatus,

    -- ** ListAttendees
    listAttendees_maxResults,
    listAttendees_nextToken,
    listAttendees_meetingId,
    listAttendeesResponse_attendees,
    listAttendeesResponse_nextToken,
    listAttendeesResponse_httpStatus,

    -- ** ListBots
    listBots_maxResults,
    listBots_nextToken,
    listBots_accountId,
    listBotsResponse_bots,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,

    -- ** ListChannelBans
    listChannelBans_chimeBearer,
    listChannelBans_maxResults,
    listChannelBans_nextToken,
    listChannelBans_channelArn,
    listChannelBansResponse_channelArn,
    listChannelBansResponse_channelBans,
    listChannelBansResponse_nextToken,
    listChannelBansResponse_httpStatus,

    -- ** ListChannelMemberships
    listChannelMemberships_chimeBearer,
    listChannelMemberships_maxResults,
    listChannelMemberships_nextToken,
    listChannelMemberships_type,
    listChannelMemberships_channelArn,
    listChannelMembershipsResponse_channelArn,
    listChannelMembershipsResponse_channelMemberships,
    listChannelMembershipsResponse_nextToken,
    listChannelMembershipsResponse_httpStatus,

    -- ** ListChannelMembershipsForAppInstanceUser
    listChannelMembershipsForAppInstanceUser_appInstanceUserArn,
    listChannelMembershipsForAppInstanceUser_chimeBearer,
    listChannelMembershipsForAppInstanceUser_maxResults,
    listChannelMembershipsForAppInstanceUser_nextToken,
    listChannelMembershipsForAppInstanceUserResponse_channelMemberships,
    listChannelMembershipsForAppInstanceUserResponse_nextToken,
    listChannelMembershipsForAppInstanceUserResponse_httpStatus,

    -- ** ListChannelMessages
    listChannelMessages_chimeBearer,
    listChannelMessages_maxResults,
    listChannelMessages_nextToken,
    listChannelMessages_notAfter,
    listChannelMessages_notBefore,
    listChannelMessages_sortOrder,
    listChannelMessages_channelArn,
    listChannelMessagesResponse_channelArn,
    listChannelMessagesResponse_channelMessages,
    listChannelMessagesResponse_nextToken,
    listChannelMessagesResponse_httpStatus,

    -- ** ListChannelModerators
    listChannelModerators_chimeBearer,
    listChannelModerators_maxResults,
    listChannelModerators_nextToken,
    listChannelModerators_channelArn,
    listChannelModeratorsResponse_channelArn,
    listChannelModeratorsResponse_channelModerators,
    listChannelModeratorsResponse_nextToken,
    listChannelModeratorsResponse_httpStatus,

    -- ** ListChannels
    listChannels_chimeBearer,
    listChannels_maxResults,
    listChannels_nextToken,
    listChannels_privacy,
    listChannels_appInstanceArn,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListChannelsModeratedByAppInstanceUser
    listChannelsModeratedByAppInstanceUser_appInstanceUserArn,
    listChannelsModeratedByAppInstanceUser_chimeBearer,
    listChannelsModeratedByAppInstanceUser_maxResults,
    listChannelsModeratedByAppInstanceUser_nextToken,
    listChannelsModeratedByAppInstanceUserResponse_channels,
    listChannelsModeratedByAppInstanceUserResponse_nextToken,
    listChannelsModeratedByAppInstanceUserResponse_httpStatus,

    -- ** ListMediaCapturePipelines
    listMediaCapturePipelines_maxResults,
    listMediaCapturePipelines_nextToken,
    listMediaCapturePipelinesResponse_mediaCapturePipelines,
    listMediaCapturePipelinesResponse_nextToken,
    listMediaCapturePipelinesResponse_httpStatus,

    -- ** ListMeetingTags
    listMeetingTags_meetingId,
    listMeetingTagsResponse_tags,
    listMeetingTagsResponse_httpStatus,

    -- ** ListMeetings
    listMeetings_maxResults,
    listMeetings_nextToken,
    listMeetingsResponse_meetings,
    listMeetingsResponse_nextToken,
    listMeetingsResponse_httpStatus,

    -- ** ListPhoneNumberOrders
    listPhoneNumberOrders_maxResults,
    listPhoneNumberOrders_nextToken,
    listPhoneNumberOrdersResponse_nextToken,
    listPhoneNumberOrdersResponse_phoneNumberOrders,
    listPhoneNumberOrdersResponse_httpStatus,

    -- ** ListPhoneNumbers
    listPhoneNumbers_filterName,
    listPhoneNumbers_filterValue,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_nextToken,
    listPhoneNumbers_productType,
    listPhoneNumbers_status,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumbers,
    listPhoneNumbersResponse_httpStatus,

    -- ** ListProxySessions
    listProxySessions_maxResults,
    listProxySessions_nextToken,
    listProxySessions_status,
    listProxySessions_voiceConnectorId,
    listProxySessionsResponse_nextToken,
    listProxySessionsResponse_proxySessions,
    listProxySessionsResponse_httpStatus,

    -- ** ListRoomMemberships
    listRoomMemberships_maxResults,
    listRoomMemberships_nextToken,
    listRoomMemberships_accountId,
    listRoomMemberships_roomId,
    listRoomMembershipsResponse_nextToken,
    listRoomMembershipsResponse_roomMemberships,
    listRoomMembershipsResponse_httpStatus,

    -- ** ListRooms
    listRooms_maxResults,
    listRooms_memberId,
    listRooms_nextToken,
    listRooms_accountId,
    listRoomsResponse_nextToken,
    listRoomsResponse_rooms,
    listRoomsResponse_httpStatus,

    -- ** ListSipMediaApplications
    listSipMediaApplications_maxResults,
    listSipMediaApplications_nextToken,
    listSipMediaApplicationsResponse_nextToken,
    listSipMediaApplicationsResponse_sipMediaApplications,
    listSipMediaApplicationsResponse_httpStatus,

    -- ** ListSipRules
    listSipRules_maxResults,
    listSipRules_nextToken,
    listSipRules_sipMediaApplicationId,
    listSipRulesResponse_nextToken,
    listSipRulesResponse_sipRules,
    listSipRulesResponse_httpStatus,

    -- ** ListSupportedPhoneNumberCountries
    listSupportedPhoneNumberCountries_productType,
    listSupportedPhoneNumberCountriesResponse_phoneNumberCountries,
    listSupportedPhoneNumberCountriesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsers
    listUsers_maxResults,
    listUsers_nextToken,
    listUsers_userEmail,
    listUsers_userType,
    listUsers_accountId,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** ListVoiceConnectorGroups
    listVoiceConnectorGroups_maxResults,
    listVoiceConnectorGroups_nextToken,
    listVoiceConnectorGroupsResponse_nextToken,
    listVoiceConnectorGroupsResponse_voiceConnectorGroups,
    listVoiceConnectorGroupsResponse_httpStatus,

    -- ** ListVoiceConnectorTerminationCredentials
    listVoiceConnectorTerminationCredentials_voiceConnectorId,
    listVoiceConnectorTerminationCredentialsResponse_usernames,
    listVoiceConnectorTerminationCredentialsResponse_httpStatus,

    -- ** ListVoiceConnectors
    listVoiceConnectors_maxResults,
    listVoiceConnectors_nextToken,
    listVoiceConnectorsResponse_nextToken,
    listVoiceConnectorsResponse_voiceConnectors,
    listVoiceConnectorsResponse_httpStatus,

    -- ** LogoutUser
    logoutUser_accountId,
    logoutUser_userId,
    logoutUserResponse_httpStatus,

    -- ** PutAppInstanceRetentionSettings
    putAppInstanceRetentionSettings_appInstanceArn,
    putAppInstanceRetentionSettings_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    putAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** PutAppInstanceStreamingConfigurations
    putAppInstanceStreamingConfigurations_appInstanceArn,
    putAppInstanceStreamingConfigurations_appInstanceStreamingConfigurations,
    putAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations,
    putAppInstanceStreamingConfigurationsResponse_httpStatus,

    -- ** PutEventsConfiguration
    putEventsConfiguration_lambdaFunctionArn,
    putEventsConfiguration_outboundEventsHTTPSEndpoint,
    putEventsConfiguration_accountId,
    putEventsConfiguration_botId,
    putEventsConfigurationResponse_eventsConfiguration,
    putEventsConfigurationResponse_httpStatus,

    -- ** PutRetentionSettings
    putRetentionSettings_accountId,
    putRetentionSettings_retentionSettings,
    putRetentionSettingsResponse_initiateDeletionTimestamp,
    putRetentionSettingsResponse_retentionSettings,
    putRetentionSettingsResponse_httpStatus,

    -- ** PutSipMediaApplicationLoggingConfiguration
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorLoggingConfiguration
    putVoiceConnectorLoggingConfiguration_voiceConnectorId,
    putVoiceConnectorLoggingConfiguration_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorOrigination
    putVoiceConnectorOrigination_voiceConnectorId,
    putVoiceConnectorOrigination_origination,
    putVoiceConnectorOriginationResponse_origination,
    putVoiceConnectorOriginationResponse_httpStatus,

    -- ** PutVoiceConnectorProxy
    putVoiceConnectorProxy_disabled,
    putVoiceConnectorProxy_fallBackPhoneNumber,
    putVoiceConnectorProxy_defaultSessionExpiryMinutes,
    putVoiceConnectorProxy_phoneNumberPoolCountries,
    putVoiceConnectorProxy_voiceConnectorId,
    putVoiceConnectorProxyResponse_proxy,
    putVoiceConnectorProxyResponse_httpStatus,

    -- ** PutVoiceConnectorStreamingConfiguration
    putVoiceConnectorStreamingConfiguration_voiceConnectorId,
    putVoiceConnectorStreamingConfiguration_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorTermination
    putVoiceConnectorTermination_voiceConnectorId,
    putVoiceConnectorTermination_termination,
    putVoiceConnectorTerminationResponse_termination,
    putVoiceConnectorTerminationResponse_httpStatus,

    -- ** PutVoiceConnectorTerminationCredentials
    putVoiceConnectorTerminationCredentials_credentials,
    putVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** RedactChannelMessage
    redactChannelMessage_chimeBearer,
    redactChannelMessage_channelArn,
    redactChannelMessage_messageId,
    redactChannelMessageResponse_channelArn,
    redactChannelMessageResponse_messageId,
    redactChannelMessageResponse_httpStatus,

    -- ** RedactConversationMessage
    redactConversationMessage_accountId,
    redactConversationMessage_conversationId,
    redactConversationMessage_messageId,
    redactConversationMessageResponse_httpStatus,

    -- ** RedactRoomMessage
    redactRoomMessage_accountId,
    redactRoomMessage_roomId,
    redactRoomMessage_messageId,
    redactRoomMessageResponse_httpStatus,

    -- ** RegenerateSecurityToken
    regenerateSecurityToken_accountId,
    regenerateSecurityToken_botId,
    regenerateSecurityTokenResponse_bot,
    regenerateSecurityTokenResponse_httpStatus,

    -- ** ResetPersonalPIN
    resetPersonalPIN_accountId,
    resetPersonalPIN_userId,
    resetPersonalPINResponse_user,
    resetPersonalPINResponse_httpStatus,

    -- ** RestorePhoneNumber
    restorePhoneNumber_phoneNumberId,
    restorePhoneNumberResponse_phoneNumber,
    restorePhoneNumberResponse_httpStatus,

    -- ** SearchAvailablePhoneNumbers
    searchAvailablePhoneNumbers_areaCode,
    searchAvailablePhoneNumbers_city,
    searchAvailablePhoneNumbers_country,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_phoneNumberType,
    searchAvailablePhoneNumbers_state,
    searchAvailablePhoneNumbers_tollFreePrefix,
    searchAvailablePhoneNumbersResponse_e164PhoneNumbers,
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_httpStatus,

    -- ** SendChannelMessage
    sendChannelMessage_chimeBearer,
    sendChannelMessage_metadata,
    sendChannelMessage_channelArn,
    sendChannelMessage_content,
    sendChannelMessage_type,
    sendChannelMessage_persistence,
    sendChannelMessage_clientRequestToken,
    sendChannelMessageResponse_channelArn,
    sendChannelMessageResponse_messageId,
    sendChannelMessageResponse_httpStatus,

    -- ** StartMeetingTranscription
    startMeetingTranscription_meetingId,
    startMeetingTranscription_transcriptionConfiguration,
    startMeetingTranscriptionResponse_httpStatus,

    -- ** StopMeetingTranscription
    stopMeetingTranscription_meetingId,
    stopMeetingTranscriptionResponse_httpStatus,

    -- ** TagAttendee
    tagAttendee_meetingId,
    tagAttendee_attendeeId,
    tagAttendee_tags,

    -- ** TagMeeting
    tagMeeting_meetingId,
    tagMeeting_tags,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** UntagAttendee
    untagAttendee_meetingId,
    untagAttendee_tagKeys,
    untagAttendee_attendeeId,

    -- ** UntagMeeting
    untagMeeting_meetingId,
    untagMeeting_tagKeys,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** UpdateAccount
    updateAccount_defaultLicense,
    updateAccount_name,
    updateAccount_accountId,
    updateAccountResponse_account,
    updateAccountResponse_httpStatus,

    -- ** UpdateAccountSettings
    updateAccountSettings_accountId,
    updateAccountSettings_accountSettings,
    updateAccountSettingsResponse_httpStatus,

    -- ** UpdateAppInstance
    updateAppInstance_metadata,
    updateAppInstance_appInstanceArn,
    updateAppInstance_name,
    updateAppInstanceResponse_appInstanceArn,
    updateAppInstanceResponse_httpStatus,

    -- ** UpdateAppInstanceUser
    updateAppInstanceUser_metadata,
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,

    -- ** UpdateBot
    updateBot_disabled,
    updateBot_accountId,
    updateBot_botId,
    updateBotResponse_bot,
    updateBotResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_chimeBearer,
    updateChannel_metadata,
    updateChannel_channelArn,
    updateChannel_name,
    updateChannel_mode,
    updateChannelResponse_channelArn,
    updateChannelResponse_httpStatus,

    -- ** UpdateChannelMessage
    updateChannelMessage_chimeBearer,
    updateChannelMessage_content,
    updateChannelMessage_metadata,
    updateChannelMessage_channelArn,
    updateChannelMessage_messageId,
    updateChannelMessageResponse_channelArn,
    updateChannelMessageResponse_messageId,
    updateChannelMessageResponse_httpStatus,

    -- ** UpdateChannelReadMarker
    updateChannelReadMarker_chimeBearer,
    updateChannelReadMarker_channelArn,
    updateChannelReadMarkerResponse_channelArn,
    updateChannelReadMarkerResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_businessCalling,
    updateGlobalSettings_voiceConnector,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_callingName,
    updatePhoneNumber_productType,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_httpStatus,

    -- ** UpdatePhoneNumberSettings
    updatePhoneNumberSettings_callingName,

    -- ** UpdateProxySession
    updateProxySession_expiryMinutes,
    updateProxySession_capabilities,
    updateProxySession_voiceConnectorId,
    updateProxySession_proxySessionId,
    updateProxySessionResponse_proxySession,
    updateProxySessionResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_name,
    updateRoom_accountId,
    updateRoom_roomId,
    updateRoomResponse_room,
    updateRoomResponse_httpStatus,

    -- ** UpdateRoomMembership
    updateRoomMembership_role,
    updateRoomMembership_accountId,
    updateRoomMembership_roomId,
    updateRoomMembership_memberId,
    updateRoomMembershipResponse_roomMembership,
    updateRoomMembershipResponse_httpStatus,

    -- ** UpdateSipMediaApplication
    updateSipMediaApplication_endpoints,
    updateSipMediaApplication_name,
    updateSipMediaApplication_sipMediaApplicationId,
    updateSipMediaApplicationResponse_sipMediaApplication,
    updateSipMediaApplicationResponse_httpStatus,

    -- ** UpdateSipMediaApplicationCall
    updateSipMediaApplicationCall_sipMediaApplicationId,
    updateSipMediaApplicationCall_transactionId,
    updateSipMediaApplicationCall_arguments,
    updateSipMediaApplicationCallResponse_sipMediaApplicationCall,
    updateSipMediaApplicationCallResponse_httpStatus,

    -- ** UpdateSipRule
    updateSipRule_disabled,
    updateSipRule_targetApplications,
    updateSipRule_sipRuleId,
    updateSipRule_name,
    updateSipRuleResponse_sipRule,
    updateSipRuleResponse_httpStatus,

    -- ** UpdateUser
    updateUser_alexaForBusinessMetadata,
    updateUser_licenseType,
    updateUser_userType,
    updateUser_accountId,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- ** UpdateUserSettings
    updateUserSettings_accountId,
    updateUserSettings_userId,
    updateUserSettings_userSettings,

    -- ** UpdateVoiceConnector
    updateVoiceConnector_voiceConnectorId,
    updateVoiceConnector_name,
    updateVoiceConnector_requireEncryption,
    updateVoiceConnectorResponse_voiceConnector,
    updateVoiceConnectorResponse_httpStatus,

    -- ** UpdateVoiceConnectorGroup
    updateVoiceConnectorGroup_voiceConnectorGroupId,
    updateVoiceConnectorGroup_name,
    updateVoiceConnectorGroup_voiceConnectorItems,
    updateVoiceConnectorGroupResponse_voiceConnectorGroup,
    updateVoiceConnectorGroupResponse_httpStatus,

    -- ** ValidateE911Address
    validateE911Address_awsAccountId,
    validateE911Address_streetNumber,
    validateE911Address_streetInfo,
    validateE911Address_city,
    validateE911Address_state,
    validateE911Address_country,
    validateE911Address_postalCode,
    validateE911AddressResponse_address,
    validateE911AddressResponse_addressExternalId,
    validateE911AddressResponse_candidateAddressList,
    validateE911AddressResponse_validationResult,
    validateE911AddressResponse_httpStatus,

    -- * Types

    -- ** Account
    account_accountStatus,
    account_accountType,
    account_createdTimestamp,
    account_defaultLicense,
    account_signinDelegateGroups,
    account_supportedLicenses,
    account_awsAccountId,
    account_accountId,
    account_name,

    -- ** AccountSettings
    accountSettings_disableRemoteControl,
    accountSettings_enableDialOut,

    -- ** Address
    address_city,
    address_country,
    address_postDirectional,
    address_postalCode,
    address_postalCodePlus4,
    address_preDirectional,
    address_state,
    address_streetName,
    address_streetNumber,
    address_streetSuffix,

    -- ** AlexaForBusinessMetadata
    alexaForBusinessMetadata_alexaForBusinessRoomArn,
    alexaForBusinessMetadata_isAlexaForBusinessEnabled,

    -- ** AppInstance
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,
    appInstance_metadata,
    appInstance_name,

    -- ** AppInstanceAdmin
    appInstanceAdmin_admin,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_createdTimestamp,

    -- ** AppInstanceAdminSummary
    appInstanceAdminSummary_admin,

    -- ** AppInstanceRetentionSettings
    appInstanceRetentionSettings_channelRetentionSettings,

    -- ** AppInstanceStreamingConfiguration
    appInstanceStreamingConfiguration_appInstanceDataType,
    appInstanceStreamingConfiguration_resourceArn,

    -- ** AppInstanceSummary
    appInstanceSummary_appInstanceArn,
    appInstanceSummary_metadata,
    appInstanceSummary_name,

    -- ** AppInstanceUser
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_metadata,
    appInstanceUser_name,

    -- ** AppInstanceUserMembershipSummary
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

    -- ** AppInstanceUserSummary
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_name,

    -- ** ArtifactsConfiguration
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- ** Attendee
    attendee_attendeeId,
    attendee_externalUserId,
    attendee_joinToken,

    -- ** AudioArtifactsConfiguration
    audioArtifactsConfiguration_muxType,

    -- ** BatchChannelMemberships
    batchChannelMemberships_channelArn,
    batchChannelMemberships_invitedBy,
    batchChannelMemberships_members,
    batchChannelMemberships_type,

    -- ** BatchCreateChannelMembershipError
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_errorMessage,
    batchCreateChannelMembershipError_memberArn,

    -- ** Bot
    bot_botEmail,
    bot_botId,
    bot_botType,
    bot_createdTimestamp,
    bot_disabled,
    bot_displayName,
    bot_securityToken,
    bot_updatedTimestamp,
    bot_userId,

    -- ** BusinessCallingSettings
    businessCallingSettings_cdrBucket,

    -- ** CandidateAddress
    candidateAddress_city,
    candidateAddress_country,
    candidateAddress_postalCode,
    candidateAddress_postalCodePlus4,
    candidateAddress_state,
    candidateAddress_streetInfo,
    candidateAddress_streetNumber,

    -- ** Channel
    channel_channelArn,
    channel_createdBy,
    channel_createdTimestamp,
    channel_lastMessageTimestamp,
    channel_lastUpdatedTimestamp,
    channel_metadata,
    channel_mode,
    channel_name,
    channel_privacy,

    -- ** ChannelBan
    channelBan_channelArn,
    channelBan_createdBy,
    channelBan_createdTimestamp,
    channelBan_member,

    -- ** ChannelBanSummary
    channelBanSummary_member,

    -- ** ChannelMembership
    channelMembership_channelArn,
    channelMembership_createdTimestamp,
    channelMembership_invitedBy,
    channelMembership_lastUpdatedTimestamp,
    channelMembership_member,
    channelMembership_type,

    -- ** ChannelMembershipForAppInstanceUserSummary
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,

    -- ** ChannelMembershipSummary
    channelMembershipSummary_member,

    -- ** ChannelMessage
    channelMessage_channelArn,
    channelMessage_content,
    channelMessage_createdTimestamp,
    channelMessage_lastEditedTimestamp,
    channelMessage_lastUpdatedTimestamp,
    channelMessage_messageId,
    channelMessage_metadata,
    channelMessage_persistence,
    channelMessage_redacted,
    channelMessage_sender,
    channelMessage_type,

    -- ** ChannelMessageSummary
    channelMessageSummary_content,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_lastEditedTimestamp,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_messageId,
    channelMessageSummary_metadata,
    channelMessageSummary_redacted,
    channelMessageSummary_sender,
    channelMessageSummary_type,

    -- ** ChannelModeratedByAppInstanceUserSummary
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- ** ChannelModerator
    channelModerator_channelArn,
    channelModerator_createdBy,
    channelModerator_createdTimestamp,
    channelModerator_moderator,

    -- ** ChannelModeratorSummary
    channelModeratorSummary_moderator,

    -- ** ChannelRetentionSettings
    channelRetentionSettings_retentionDays,

    -- ** ChannelSummary
    channelSummary_channelArn,
    channelSummary_lastMessageTimestamp,
    channelSummary_metadata,
    channelSummary_mode,
    channelSummary_name,
    channelSummary_privacy,

    -- ** ChimeSdkMeetingConfiguration
    chimeSdkMeetingConfiguration_artifactsConfiguration,
    chimeSdkMeetingConfiguration_sourceConfiguration,

    -- ** ContentArtifactsConfiguration
    contentArtifactsConfiguration_muxType,
    contentArtifactsConfiguration_state,

    -- ** ConversationRetentionSettings
    conversationRetentionSettings_retentionDays,

    -- ** CreateAttendeeError
    createAttendeeError_errorCode,
    createAttendeeError_errorMessage,
    createAttendeeError_externalUserId,

    -- ** CreateAttendeeRequestItem
    createAttendeeRequestItem_tags,
    createAttendeeRequestItem_externalUserId,

    -- ** Credential
    credential_password,
    credential_username,

    -- ** DNISEmergencyCallingConfiguration
    dNISEmergencyCallingConfiguration_testPhoneNumber,
    dNISEmergencyCallingConfiguration_emergencyPhoneNumber,
    dNISEmergencyCallingConfiguration_callingCountry,

    -- ** EmergencyCallingConfiguration
    emergencyCallingConfiguration_dnis,

    -- ** EngineTranscribeMedicalSettings
    engineTranscribeMedicalSettings_contentIdentificationType,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_vocabularyName,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- ** EngineTranscribeSettings
    engineTranscribeSettings_contentIdentificationType,
    engineTranscribeSettings_contentRedactionType,
    engineTranscribeSettings_enablePartialResultsStabilization,
    engineTranscribeSettings_languageModelName,
    engineTranscribeSettings_partialResultsStability,
    engineTranscribeSettings_piiEntityTypes,
    engineTranscribeSettings_region,
    engineTranscribeSettings_vocabularyFilterMethod,
    engineTranscribeSettings_vocabularyFilterName,
    engineTranscribeSettings_vocabularyName,
    engineTranscribeSettings_languageCode,

    -- ** EventsConfiguration
    eventsConfiguration_botId,
    eventsConfiguration_lambdaFunctionArn,
    eventsConfiguration_outboundEventsHTTPSEndpoint,

    -- ** GeoMatchParams
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** Invite
    invite_emailAddress,
    invite_emailStatus,
    invite_inviteId,
    invite_status,

    -- ** LoggingConfiguration
    loggingConfiguration_enableMediaMetricLogs,
    loggingConfiguration_enableSIPLogs,

    -- ** MediaCapturePipeline
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_createdTimestamp,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_sinkArn,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_status,
    mediaCapturePipeline_updatedTimestamp,

    -- ** MediaPlacement
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_screenDataUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_signalingUrl,
    mediaPlacement_turnControlUrl,

    -- ** Meeting
    meeting_externalMeetingId,
    meeting_mediaPlacement,
    meeting_mediaRegion,
    meeting_meetingId,

    -- ** MeetingNotificationConfiguration
    meetingNotificationConfiguration_snsTopicArn,
    meetingNotificationConfiguration_sqsQueueArn,

    -- ** Member
    member_accountId,
    member_email,
    member_fullName,
    member_memberId,
    member_memberType,

    -- ** MemberError
    memberError_errorCode,
    memberError_errorMessage,
    memberError_memberId,

    -- ** MembershipItem
    membershipItem_memberId,
    membershipItem_role,

    -- ** MessagingSessionEndpoint
    messagingSessionEndpoint_url,

    -- ** OrderedPhoneNumber
    orderedPhoneNumber_e164PhoneNumber,
    orderedPhoneNumber_status,

    -- ** Origination
    origination_disabled,
    origination_routes,

    -- ** OriginationRoute
    originationRoute_host,
    originationRoute_port,
    originationRoute_priority,
    originationRoute_protocol,
    originationRoute_weight,

    -- ** Participant
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- ** PhoneNumber
    phoneNumber_associations,
    phoneNumber_callingName,
    phoneNumber_callingNameStatus,
    phoneNumber_capabilities,
    phoneNumber_country,
    phoneNumber_createdTimestamp,
    phoneNumber_deletionTimestamp,
    phoneNumber_e164PhoneNumber,
    phoneNumber_phoneNumberId,
    phoneNumber_productType,
    phoneNumber_status,
    phoneNumber_type,
    phoneNumber_updatedTimestamp,

    -- ** PhoneNumberAssociation
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,
    phoneNumberAssociation_value,

    -- ** PhoneNumberCapabilities
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_outboundSMS,

    -- ** PhoneNumberCountry
    phoneNumberCountry_countryCode,
    phoneNumberCountry_supportedPhoneNumberTypes,

    -- ** PhoneNumberError
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,
    phoneNumberError_phoneNumberId,

    -- ** PhoneNumberOrder
    phoneNumberOrder_createdTimestamp,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_productType,
    phoneNumberOrder_status,
    phoneNumberOrder_updatedTimestamp,

    -- ** Proxy
    proxy_defaultSessionExpiryMinutes,
    proxy_disabled,
    proxy_fallBackPhoneNumber,
    proxy_phoneNumberCountries,

    -- ** ProxySession
    proxySession_capabilities,
    proxySession_createdTimestamp,
    proxySession_endedTimestamp,
    proxySession_expiryMinutes,
    proxySession_geoMatchLevel,
    proxySession_geoMatchParams,
    proxySession_name,
    proxySession_numberSelectionBehavior,
    proxySession_participants,
    proxySession_proxySessionId,
    proxySession_status,
    proxySession_updatedTimestamp,
    proxySession_voiceConnectorId,

    -- ** RetentionSettings
    retentionSettings_conversationRetentionSettings,
    retentionSettings_roomRetentionSettings,

    -- ** Room
    room_accountId,
    room_createdBy,
    room_createdTimestamp,
    room_name,
    room_roomId,
    room_updatedTimestamp,

    -- ** RoomMembership
    roomMembership_invitedBy,
    roomMembership_member,
    roomMembership_role,
    roomMembership_roomId,
    roomMembership_updatedTimestamp,

    -- ** RoomRetentionSettings
    roomRetentionSettings_retentionDays,

    -- ** SelectedVideoStreams
    selectedVideoStreams_attendeeIds,
    selectedVideoStreams_externalUserIds,

    -- ** SigninDelegateGroup
    signinDelegateGroup_groupName,

    -- ** SipMediaApplication
    sipMediaApplication_awsRegion,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_endpoints,
    sipMediaApplication_name,
    sipMediaApplication_sipMediaApplicationId,
    sipMediaApplication_updatedTimestamp,

    -- ** SipMediaApplicationCall
    sipMediaApplicationCall_transactionId,

    -- ** SipMediaApplicationEndpoint
    sipMediaApplicationEndpoint_lambdaArn,

    -- ** SipMediaApplicationLoggingConfiguration
    sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs,

    -- ** SipRule
    sipRule_createdTimestamp,
    sipRule_disabled,
    sipRule_name,
    sipRule_sipRuleId,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_triggerValue,
    sipRule_updatedTimestamp,

    -- ** SipRuleTargetApplication
    sipRuleTargetApplication_awsRegion,
    sipRuleTargetApplication_priority,
    sipRuleTargetApplication_sipMediaApplicationId,

    -- ** SourceConfiguration
    sourceConfiguration_selectedVideoStreams,

    -- ** StreamingConfiguration
    streamingConfiguration_disabled,
    streamingConfiguration_streamingNotificationTargets,
    streamingConfiguration_dataRetentionInHours,

    -- ** StreamingNotificationTarget
    streamingNotificationTarget_notificationTarget,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TelephonySettings
    telephonySettings_inboundCalling,
    telephonySettings_outboundCalling,
    telephonySettings_sms,

    -- ** Termination
    termination_callingRegions,
    termination_cidrAllowedList,
    termination_cpsLimit,
    termination_defaultPhoneNumber,
    termination_disabled,

    -- ** TerminationHealth
    terminationHealth_source,
    terminationHealth_timestamp,

    -- ** TranscriptionConfiguration
    transcriptionConfiguration_engineTranscribeMedicalSettings,
    transcriptionConfiguration_engineTranscribeSettings,

    -- ** UpdatePhoneNumberRequestItem
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- ** UpdateUserRequestItem
    updateUserRequestItem_alexaForBusinessMetadata,
    updateUserRequestItem_licenseType,
    updateUserRequestItem_userType,
    updateUserRequestItem_userId,

    -- ** User
    user_accountId,
    user_alexaForBusinessMetadata,
    user_displayName,
    user_invitedOn,
    user_licenseType,
    user_personalPIN,
    user_primaryEmail,
    user_primaryProvisionedNumber,
    user_registeredOn,
    user_userInvitationStatus,
    user_userRegistrationStatus,
    user_userType,
    user_userId,

    -- ** UserError
    userError_errorCode,
    userError_errorMessage,
    userError_userId,

    -- ** UserSettings
    userSettings_telephony,

    -- ** VideoArtifactsConfiguration
    videoArtifactsConfiguration_muxType,
    videoArtifactsConfiguration_state,

    -- ** VoiceConnector
    voiceConnector_awsRegion,
    voiceConnector_createdTimestamp,
    voiceConnector_name,
    voiceConnector_outboundHostName,
    voiceConnector_requireEncryption,
    voiceConnector_updatedTimestamp,
    voiceConnector_voiceConnectorArn,
    voiceConnector_voiceConnectorId,

    -- ** VoiceConnectorGroup
    voiceConnectorGroup_createdTimestamp,
    voiceConnectorGroup_name,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_voiceConnectorItems,

    -- ** VoiceConnectorItem
    voiceConnectorItem_voiceConnectorId,
    voiceConnectorItem_priority,

    -- ** VoiceConnectorSettings
    voiceConnectorSettings_cdrBucket,
  )
where

import Amazonka.Chime.AssociatePhoneNumberWithUser
import Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnector
import Amazonka.Chime.AssociatePhoneNumbersWithVoiceConnectorGroup
import Amazonka.Chime.AssociateSigninDelegateGroupsWithAccount
import Amazonka.Chime.BatchCreateAttendee
import Amazonka.Chime.BatchCreateChannelMembership
import Amazonka.Chime.BatchCreateRoomMembership
import Amazonka.Chime.BatchDeletePhoneNumber
import Amazonka.Chime.BatchSuspendUser
import Amazonka.Chime.BatchUnsuspendUser
import Amazonka.Chime.BatchUpdatePhoneNumber
import Amazonka.Chime.BatchUpdateUser
import Amazonka.Chime.CreateAccount
import Amazonka.Chime.CreateAppInstance
import Amazonka.Chime.CreateAppInstanceAdmin
import Amazonka.Chime.CreateAppInstanceUser
import Amazonka.Chime.CreateAttendee
import Amazonka.Chime.CreateBot
import Amazonka.Chime.CreateChannel
import Amazonka.Chime.CreateChannelBan
import Amazonka.Chime.CreateChannelMembership
import Amazonka.Chime.CreateChannelModerator
import Amazonka.Chime.CreateMediaCapturePipeline
import Amazonka.Chime.CreateMeeting
import Amazonka.Chime.CreateMeetingDialOut
import Amazonka.Chime.CreateMeetingWithAttendees
import Amazonka.Chime.CreatePhoneNumberOrder
import Amazonka.Chime.CreateProxySession
import Amazonka.Chime.CreateRoom
import Amazonka.Chime.CreateRoomMembership
import Amazonka.Chime.CreateSipMediaApplication
import Amazonka.Chime.CreateSipMediaApplicationCall
import Amazonka.Chime.CreateSipRule
import Amazonka.Chime.CreateUser
import Amazonka.Chime.CreateVoiceConnector
import Amazonka.Chime.CreateVoiceConnectorGroup
import Amazonka.Chime.DeleteAccount
import Amazonka.Chime.DeleteAppInstance
import Amazonka.Chime.DeleteAppInstanceAdmin
import Amazonka.Chime.DeleteAppInstanceStreamingConfigurations
import Amazonka.Chime.DeleteAppInstanceUser
import Amazonka.Chime.DeleteAttendee
import Amazonka.Chime.DeleteChannel
import Amazonka.Chime.DeleteChannelBan
import Amazonka.Chime.DeleteChannelMembership
import Amazonka.Chime.DeleteChannelMessage
import Amazonka.Chime.DeleteChannelModerator
import Amazonka.Chime.DeleteEventsConfiguration
import Amazonka.Chime.DeleteMediaCapturePipeline
import Amazonka.Chime.DeleteMeeting
import Amazonka.Chime.DeletePhoneNumber
import Amazonka.Chime.DeleteProxySession
import Amazonka.Chime.DeleteRoom
import Amazonka.Chime.DeleteRoomMembership
import Amazonka.Chime.DeleteSipMediaApplication
import Amazonka.Chime.DeleteSipRule
import Amazonka.Chime.DeleteVoiceConnector
import Amazonka.Chime.DeleteVoiceConnectorEmergencyCallingConfiguration
import Amazonka.Chime.DeleteVoiceConnectorGroup
import Amazonka.Chime.DeleteVoiceConnectorOrigination
import Amazonka.Chime.DeleteVoiceConnectorProxy
import Amazonka.Chime.DeleteVoiceConnectorStreamingConfiguration
import Amazonka.Chime.DeleteVoiceConnectorTermination
import Amazonka.Chime.DeleteVoiceConnectorTerminationCredentials
import Amazonka.Chime.DescribeAppInstance
import Amazonka.Chime.DescribeAppInstanceAdmin
import Amazonka.Chime.DescribeAppInstanceUser
import Amazonka.Chime.DescribeChannel
import Amazonka.Chime.DescribeChannelBan
import Amazonka.Chime.DescribeChannelMembership
import Amazonka.Chime.DescribeChannelMembershipForAppInstanceUser
import Amazonka.Chime.DescribeChannelModeratedByAppInstanceUser
import Amazonka.Chime.DescribeChannelModerator
import Amazonka.Chime.DisassociatePhoneNumberFromUser
import Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnector
import Amazonka.Chime.DisassociatePhoneNumbersFromVoiceConnectorGroup
import Amazonka.Chime.DisassociateSigninDelegateGroupsFromAccount
import Amazonka.Chime.GetAccount
import Amazonka.Chime.GetAccountSettings
import Amazonka.Chime.GetAppInstanceRetentionSettings
import Amazonka.Chime.GetAppInstanceStreamingConfigurations
import Amazonka.Chime.GetAttendee
import Amazonka.Chime.GetBot
import Amazonka.Chime.GetChannelMessage
import Amazonka.Chime.GetEventsConfiguration
import Amazonka.Chime.GetGlobalSettings
import Amazonka.Chime.GetMediaCapturePipeline
import Amazonka.Chime.GetMeeting
import Amazonka.Chime.GetMessagingSessionEndpoint
import Amazonka.Chime.GetPhoneNumber
import Amazonka.Chime.GetPhoneNumberOrder
import Amazonka.Chime.GetPhoneNumberSettings
import Amazonka.Chime.GetProxySession
import Amazonka.Chime.GetRetentionSettings
import Amazonka.Chime.GetRoom
import Amazonka.Chime.GetSipMediaApplication
import Amazonka.Chime.GetSipMediaApplicationLoggingConfiguration
import Amazonka.Chime.GetSipRule
import Amazonka.Chime.GetUser
import Amazonka.Chime.GetUserSettings
import Amazonka.Chime.GetVoiceConnector
import Amazonka.Chime.GetVoiceConnectorEmergencyCallingConfiguration
import Amazonka.Chime.GetVoiceConnectorGroup
import Amazonka.Chime.GetVoiceConnectorLoggingConfiguration
import Amazonka.Chime.GetVoiceConnectorOrigination
import Amazonka.Chime.GetVoiceConnectorProxy
import Amazonka.Chime.GetVoiceConnectorStreamingConfiguration
import Amazonka.Chime.GetVoiceConnectorTermination
import Amazonka.Chime.GetVoiceConnectorTerminationHealth
import Amazonka.Chime.InviteUsers
import Amazonka.Chime.ListAccounts
import Amazonka.Chime.ListAppInstanceAdmins
import Amazonka.Chime.ListAppInstanceUsers
import Amazonka.Chime.ListAppInstances
import Amazonka.Chime.ListAttendeeTags
import Amazonka.Chime.ListAttendees
import Amazonka.Chime.ListBots
import Amazonka.Chime.ListChannelBans
import Amazonka.Chime.ListChannelMemberships
import Amazonka.Chime.ListChannelMembershipsForAppInstanceUser
import Amazonka.Chime.ListChannelMessages
import Amazonka.Chime.ListChannelModerators
import Amazonka.Chime.ListChannels
import Amazonka.Chime.ListChannelsModeratedByAppInstanceUser
import Amazonka.Chime.ListMediaCapturePipelines
import Amazonka.Chime.ListMeetingTags
import Amazonka.Chime.ListMeetings
import Amazonka.Chime.ListPhoneNumberOrders
import Amazonka.Chime.ListPhoneNumbers
import Amazonka.Chime.ListProxySessions
import Amazonka.Chime.ListRoomMemberships
import Amazonka.Chime.ListRooms
import Amazonka.Chime.ListSipMediaApplications
import Amazonka.Chime.ListSipRules
import Amazonka.Chime.ListSupportedPhoneNumberCountries
import Amazonka.Chime.ListTagsForResource
import Amazonka.Chime.ListUsers
import Amazonka.Chime.ListVoiceConnectorGroups
import Amazonka.Chime.ListVoiceConnectorTerminationCredentials
import Amazonka.Chime.ListVoiceConnectors
import Amazonka.Chime.LogoutUser
import Amazonka.Chime.PutAppInstanceRetentionSettings
import Amazonka.Chime.PutAppInstanceStreamingConfigurations
import Amazonka.Chime.PutEventsConfiguration
import Amazonka.Chime.PutRetentionSettings
import Amazonka.Chime.PutSipMediaApplicationLoggingConfiguration
import Amazonka.Chime.PutVoiceConnectorEmergencyCallingConfiguration
import Amazonka.Chime.PutVoiceConnectorLoggingConfiguration
import Amazonka.Chime.PutVoiceConnectorOrigination
import Amazonka.Chime.PutVoiceConnectorProxy
import Amazonka.Chime.PutVoiceConnectorStreamingConfiguration
import Amazonka.Chime.PutVoiceConnectorTermination
import Amazonka.Chime.PutVoiceConnectorTerminationCredentials
import Amazonka.Chime.RedactChannelMessage
import Amazonka.Chime.RedactConversationMessage
import Amazonka.Chime.RedactRoomMessage
import Amazonka.Chime.RegenerateSecurityToken
import Amazonka.Chime.ResetPersonalPIN
import Amazonka.Chime.RestorePhoneNumber
import Amazonka.Chime.SearchAvailablePhoneNumbers
import Amazonka.Chime.SendChannelMessage
import Amazonka.Chime.StartMeetingTranscription
import Amazonka.Chime.StopMeetingTranscription
import Amazonka.Chime.TagAttendee
import Amazonka.Chime.TagMeeting
import Amazonka.Chime.TagResource
import Amazonka.Chime.Types.Account
import Amazonka.Chime.Types.AccountSettings
import Amazonka.Chime.Types.Address
import Amazonka.Chime.Types.AlexaForBusinessMetadata
import Amazonka.Chime.Types.AppInstance
import Amazonka.Chime.Types.AppInstanceAdmin
import Amazonka.Chime.Types.AppInstanceAdminSummary
import Amazonka.Chime.Types.AppInstanceRetentionSettings
import Amazonka.Chime.Types.AppInstanceStreamingConfiguration
import Amazonka.Chime.Types.AppInstanceSummary
import Amazonka.Chime.Types.AppInstanceUser
import Amazonka.Chime.Types.AppInstanceUserMembershipSummary
import Amazonka.Chime.Types.AppInstanceUserSummary
import Amazonka.Chime.Types.ArtifactsConfiguration
import Amazonka.Chime.Types.Attendee
import Amazonka.Chime.Types.AudioArtifactsConfiguration
import Amazonka.Chime.Types.BatchChannelMemberships
import Amazonka.Chime.Types.BatchCreateChannelMembershipError
import Amazonka.Chime.Types.Bot
import Amazonka.Chime.Types.BusinessCallingSettings
import Amazonka.Chime.Types.CandidateAddress
import Amazonka.Chime.Types.Channel
import Amazonka.Chime.Types.ChannelBan
import Amazonka.Chime.Types.ChannelBanSummary
import Amazonka.Chime.Types.ChannelMembership
import Amazonka.Chime.Types.ChannelMembershipForAppInstanceUserSummary
import Amazonka.Chime.Types.ChannelMembershipSummary
import Amazonka.Chime.Types.ChannelMessage
import Amazonka.Chime.Types.ChannelMessageSummary
import Amazonka.Chime.Types.ChannelModeratedByAppInstanceUserSummary
import Amazonka.Chime.Types.ChannelModerator
import Amazonka.Chime.Types.ChannelModeratorSummary
import Amazonka.Chime.Types.ChannelRetentionSettings
import Amazonka.Chime.Types.ChannelSummary
import Amazonka.Chime.Types.ChimeSdkMeetingConfiguration
import Amazonka.Chime.Types.ContentArtifactsConfiguration
import Amazonka.Chime.Types.ConversationRetentionSettings
import Amazonka.Chime.Types.CreateAttendeeError
import Amazonka.Chime.Types.CreateAttendeeRequestItem
import Amazonka.Chime.Types.Credential
import Amazonka.Chime.Types.DNISEmergencyCallingConfiguration
import Amazonka.Chime.Types.EmergencyCallingConfiguration
import Amazonka.Chime.Types.EngineTranscribeMedicalSettings
import Amazonka.Chime.Types.EngineTranscribeSettings
import Amazonka.Chime.Types.EventsConfiguration
import Amazonka.Chime.Types.GeoMatchParams
import Amazonka.Chime.Types.Identity
import Amazonka.Chime.Types.Invite
import Amazonka.Chime.Types.LoggingConfiguration
import Amazonka.Chime.Types.MediaCapturePipeline
import Amazonka.Chime.Types.MediaPlacement
import Amazonka.Chime.Types.Meeting
import Amazonka.Chime.Types.MeetingNotificationConfiguration
import Amazonka.Chime.Types.Member
import Amazonka.Chime.Types.MemberError
import Amazonka.Chime.Types.MembershipItem
import Amazonka.Chime.Types.MessagingSessionEndpoint
import Amazonka.Chime.Types.OrderedPhoneNumber
import Amazonka.Chime.Types.Origination
import Amazonka.Chime.Types.OriginationRoute
import Amazonka.Chime.Types.Participant
import Amazonka.Chime.Types.PhoneNumber
import Amazonka.Chime.Types.PhoneNumberAssociation
import Amazonka.Chime.Types.PhoneNumberCapabilities
import Amazonka.Chime.Types.PhoneNumberCountry
import Amazonka.Chime.Types.PhoneNumberError
import Amazonka.Chime.Types.PhoneNumberOrder
import Amazonka.Chime.Types.Proxy
import Amazonka.Chime.Types.ProxySession
import Amazonka.Chime.Types.RetentionSettings
import Amazonka.Chime.Types.Room
import Amazonka.Chime.Types.RoomMembership
import Amazonka.Chime.Types.RoomRetentionSettings
import Amazonka.Chime.Types.SelectedVideoStreams
import Amazonka.Chime.Types.SigninDelegateGroup
import Amazonka.Chime.Types.SipMediaApplication
import Amazonka.Chime.Types.SipMediaApplicationCall
import Amazonka.Chime.Types.SipMediaApplicationEndpoint
import Amazonka.Chime.Types.SipMediaApplicationLoggingConfiguration
import Amazonka.Chime.Types.SipRule
import Amazonka.Chime.Types.SipRuleTargetApplication
import Amazonka.Chime.Types.SourceConfiguration
import Amazonka.Chime.Types.StreamingConfiguration
import Amazonka.Chime.Types.StreamingNotificationTarget
import Amazonka.Chime.Types.Tag
import Amazonka.Chime.Types.TelephonySettings
import Amazonka.Chime.Types.Termination
import Amazonka.Chime.Types.TerminationHealth
import Amazonka.Chime.Types.TranscriptionConfiguration
import Amazonka.Chime.Types.UpdatePhoneNumberRequestItem
import Amazonka.Chime.Types.UpdateUserRequestItem
import Amazonka.Chime.Types.User
import Amazonka.Chime.Types.UserError
import Amazonka.Chime.Types.UserSettings
import Amazonka.Chime.Types.VideoArtifactsConfiguration
import Amazonka.Chime.Types.VoiceConnector
import Amazonka.Chime.Types.VoiceConnectorGroup
import Amazonka.Chime.Types.VoiceConnectorItem
import Amazonka.Chime.Types.VoiceConnectorSettings
import Amazonka.Chime.UntagAttendee
import Amazonka.Chime.UntagMeeting
import Amazonka.Chime.UntagResource
import Amazonka.Chime.UpdateAccount
import Amazonka.Chime.UpdateAccountSettings
import Amazonka.Chime.UpdateAppInstance
import Amazonka.Chime.UpdateAppInstanceUser
import Amazonka.Chime.UpdateBot
import Amazonka.Chime.UpdateChannel
import Amazonka.Chime.UpdateChannelMessage
import Amazonka.Chime.UpdateChannelReadMarker
import Amazonka.Chime.UpdateGlobalSettings
import Amazonka.Chime.UpdatePhoneNumber
import Amazonka.Chime.UpdatePhoneNumberSettings
import Amazonka.Chime.UpdateProxySession
import Amazonka.Chime.UpdateRoom
import Amazonka.Chime.UpdateRoomMembership
import Amazonka.Chime.UpdateSipMediaApplication
import Amazonka.Chime.UpdateSipMediaApplicationCall
import Amazonka.Chime.UpdateSipRule
import Amazonka.Chime.UpdateUser
import Amazonka.Chime.UpdateUserSettings
import Amazonka.Chime.UpdateVoiceConnector
import Amazonka.Chime.UpdateVoiceConnectorGroup
import Amazonka.Chime.ValidateE911Address

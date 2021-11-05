{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Lens
  ( -- * Operations

    -- ** DescribeChannelMembership
    describeChannelMembership_chimeBearer,
    describeChannelMembership_channelArn,
    describeChannelMembership_memberArn,
    describeChannelMembershipResponse_channelMembership,
    describeChannelMembershipResponse_httpStatus,

    -- ** CreateAppInstance
    createAppInstance_metadata,
    createAppInstance_tags,
    createAppInstance_name,
    createAppInstance_clientRequestToken,
    createAppInstanceResponse_appInstanceArn,
    createAppInstanceResponse_httpStatus,

    -- ** GetVoiceConnectorGroup
    getVoiceConnectorGroup_voiceConnectorGroupId,
    getVoiceConnectorGroupResponse_voiceConnectorGroup,
    getVoiceConnectorGroupResponse_httpStatus,

    -- ** ListVoiceConnectors
    listVoiceConnectors_nextToken,
    listVoiceConnectors_maxResults,
    listVoiceConnectorsResponse_nextToken,
    listVoiceConnectorsResponse_voiceConnectors,
    listVoiceConnectorsResponse_httpStatus,

    -- ** ListRoomMemberships
    listRoomMemberships_nextToken,
    listRoomMemberships_maxResults,
    listRoomMemberships_accountId,
    listRoomMemberships_roomId,
    listRoomMembershipsResponse_nextToken,
    listRoomMembershipsResponse_roomMemberships,
    listRoomMembershipsResponse_httpStatus,

    -- ** GetPhoneNumberSettings
    getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp,
    getPhoneNumberSettingsResponse_callingName,
    getPhoneNumberSettingsResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_businessCalling,
    updateGlobalSettings_voiceConnector,

    -- ** ListAttendees
    listAttendees_nextToken,
    listAttendees_maxResults,
    listAttendees_meetingId,
    listAttendeesResponse_attendees,
    listAttendeesResponse_nextToken,
    listAttendeesResponse_httpStatus,

    -- ** PutVoiceConnectorLoggingConfiguration
    putVoiceConnectorLoggingConfiguration_voiceConnectorId,
    putVoiceConnectorLoggingConfiguration_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorTermination
    getVoiceConnectorTermination_voiceConnectorId,
    getVoiceConnectorTerminationResponse_termination,
    getVoiceConnectorTerminationResponse_httpStatus,

    -- ** DeleteAttendee
    deleteAttendee_meetingId,
    deleteAttendee_attendeeId,

    -- ** GetVoiceConnectorProxy
    getVoiceConnectorProxy_voiceConnectorId,
    getVoiceConnectorProxyResponse_proxy,
    getVoiceConnectorProxyResponse_httpStatus,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,

    -- ** GetVoiceConnectorStreamingConfiguration
    getVoiceConnectorStreamingConfiguration_voiceConnectorId,
    getVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    getVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** UpdateSipMediaApplicationCall
    updateSipMediaApplicationCall_sipMediaApplicationId,
    updateSipMediaApplicationCall_transactionId,
    updateSipMediaApplicationCall_arguments,
    updateSipMediaApplicationCallResponse_sipMediaApplicationCall,
    updateSipMediaApplicationCallResponse_httpStatus,

    -- ** StopMeetingTranscription
    stopMeetingTranscription_meetingId,
    stopMeetingTranscriptionResponse_httpStatus,

    -- ** GetAppInstanceRetentionSettings
    getAppInstanceRetentionSettings_appInstanceArn,
    getAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    getAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    getAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** CreateMeetingWithAttendees
    createMeetingWithAttendees_mediaRegion,
    createMeetingWithAttendees_meetingHostId,
    createMeetingWithAttendees_attendees,
    createMeetingWithAttendees_notificationsConfiguration,
    createMeetingWithAttendees_externalMeetingId,
    createMeetingWithAttendees_tags,
    createMeetingWithAttendees_clientRequestToken,
    createMeetingWithAttendeesResponse_attendees,
    createMeetingWithAttendeesResponse_meeting,
    createMeetingWithAttendeesResponse_errors,
    createMeetingWithAttendeesResponse_httpStatus,

    -- ** ListChannels
    listChannels_privacy,
    listChannels_chimeBearer,
    listChannels_nextToken,
    listChannels_maxResults,
    listChannels_appInstanceArn,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** DisassociatePhoneNumberFromUser
    disassociatePhoneNumberFromUser_accountId,
    disassociatePhoneNumberFromUser_userId,
    disassociatePhoneNumberFromUserResponse_httpStatus,

    -- ** DisassociateSigninDelegateGroupsFromAccount
    disassociateSigninDelegateGroupsFromAccount_accountId,
    disassociateSigninDelegateGroupsFromAccount_groupNames,
    disassociateSigninDelegateGroupsFromAccountResponse_httpStatus,

    -- ** ResetPersonalPIN
    resetPersonalPIN_accountId,
    resetPersonalPIN_userId,
    resetPersonalPINResponse_user,
    resetPersonalPINResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_chimeBearer,
    deleteChannel_channelArn,

    -- ** UpdateChannel
    updateChannel_chimeBearer,
    updateChannel_metadata,
    updateChannel_channelArn,
    updateChannel_name,
    updateChannel_mode,
    updateChannelResponse_channelArn,
    updateChannelResponse_httpStatus,

    -- ** DescribeAppInstanceAdmin
    describeAppInstanceAdmin_appInstanceAdminArn,
    describeAppInstanceAdmin_appInstanceArn,
    describeAppInstanceAdminResponse_appInstanceAdmin,
    describeAppInstanceAdminResponse_httpStatus,

    -- ** CreateAttendee
    createAttendee_tags,
    createAttendee_meetingId,
    createAttendee_externalUserId,
    createAttendeeResponse_attendee,
    createAttendeeResponse_httpStatus,

    -- ** ListSupportedPhoneNumberCountries
    listSupportedPhoneNumberCountries_productType,
    listSupportedPhoneNumberCountriesResponse_phoneNumberCountries,
    listSupportedPhoneNumberCountriesResponse_httpStatus,

    -- ** DeleteSipRule
    deleteSipRule_sipRuleId,

    -- ** UpdateSipRule
    updateSipRule_disabled,
    updateSipRule_targetApplications,
    updateSipRule_sipRuleId,
    updateSipRule_name,
    updateSipRuleResponse_sipRule,
    updateSipRuleResponse_httpStatus,

    -- ** UpdateAccountSettings
    updateAccountSettings_accountId,
    updateAccountSettings_accountSettings,
    updateAccountSettingsResponse_httpStatus,

    -- ** DeleteVoiceConnectorOrigination
    deleteVoiceConnectorOrigination_voiceConnectorId,

    -- ** DeleteSipMediaApplication
    deleteSipMediaApplication_sipMediaApplicationId,

    -- ** UpdateSipMediaApplication
    updateSipMediaApplication_name,
    updateSipMediaApplication_endpoints,
    updateSipMediaApplication_sipMediaApplicationId,
    updateSipMediaApplicationResponse_sipMediaApplication,
    updateSipMediaApplicationResponse_httpStatus,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId,
    disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus,

    -- ** GetMessagingSessionEndpoint
    getMessagingSessionEndpointResponse_endpoint,
    getMessagingSessionEndpointResponse_httpStatus,

    -- ** PutVoiceConnectorOrigination
    putVoiceConnectorOrigination_voiceConnectorId,
    putVoiceConnectorOrigination_origination,
    putVoiceConnectorOriginationResponse_origination,
    putVoiceConnectorOriginationResponse_httpStatus,

    -- ** CreateAppInstanceUser
    createAppInstanceUser_metadata,
    createAppInstanceUser_tags,
    createAppInstanceUser_appInstanceArn,
    createAppInstanceUser_appInstanceUserId,
    createAppInstanceUser_name,
    createAppInstanceUser_clientRequestToken,
    createAppInstanceUserResponse_appInstanceUserArn,
    createAppInstanceUserResponse_httpStatus,

    -- ** ListAttendeeTags
    listAttendeeTags_meetingId,
    listAttendeeTags_attendeeId,
    listAttendeeTagsResponse_tags,
    listAttendeeTagsResponse_httpStatus,

    -- ** ListChannelsModeratedByAppInstanceUser
    listChannelsModeratedByAppInstanceUser_appInstanceUserArn,
    listChannelsModeratedByAppInstanceUser_chimeBearer,
    listChannelsModeratedByAppInstanceUser_nextToken,
    listChannelsModeratedByAppInstanceUser_maxResults,
    listChannelsModeratedByAppInstanceUserResponse_channels,
    listChannelsModeratedByAppInstanceUserResponse_nextToken,
    listChannelsModeratedByAppInstanceUserResponse_httpStatus,

    -- ** RedactChannelMessage
    redactChannelMessage_chimeBearer,
    redactChannelMessage_channelArn,
    redactChannelMessage_messageId,
    redactChannelMessageResponse_channelArn,
    redactChannelMessageResponse_messageId,
    redactChannelMessageResponse_httpStatus,

    -- ** PutRetentionSettings
    putRetentionSettings_accountId,
    putRetentionSettings_retentionSettings,
    putRetentionSettingsResponse_retentionSettings,
    putRetentionSettingsResponse_initiateDeletionTimestamp,
    putRetentionSettingsResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_userEmail,
    listUsers_maxResults,
    listUsers_userType,
    listUsers_accountId,
    listUsersResponse_users,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    deleteVoiceConnectorStreamingConfiguration_voiceConnectorId,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate,
    associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId,
    associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus,

    -- ** PutAppInstanceRetentionSettings
    putAppInstanceRetentionSettings_appInstanceArn,
    putAppInstanceRetentionSettings_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_appInstanceRetentionSettings,
    putAppInstanceRetentionSettingsResponse_initiateDeletionTimestamp,
    putAppInstanceRetentionSettingsResponse_httpStatus,

    -- ** GetVoiceConnectorLoggingConfiguration
    getVoiceConnectorLoggingConfiguration_voiceConnectorId,
    getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    getVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** ListBots
    listBots_nextToken,
    listBots_maxResults,
    listBots_accountId,
    listBotsResponse_bots,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,

    -- ** DeleteChannelMembership
    deleteChannelMembership_chimeBearer,
    deleteChannelMembership_channelArn,
    deleteChannelMembership_memberArn,

    -- ** PutVoiceConnectorStreamingConfiguration
    putVoiceConnectorStreamingConfiguration_voiceConnectorId,
    putVoiceConnectorStreamingConfiguration_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** ListChannelMemberships
    listChannelMemberships_chimeBearer,
    listChannelMemberships_nextToken,
    listChannelMemberships_type,
    listChannelMemberships_maxResults,
    listChannelMemberships_channelArn,
    listChannelMembershipsResponse_channelMemberships,
    listChannelMembershipsResponse_channelArn,
    listChannelMembershipsResponse_nextToken,
    listChannelMembershipsResponse_httpStatus,

    -- ** GetGlobalSettings
    getGlobalSettingsResponse_businessCalling,
    getGlobalSettingsResponse_voiceConnector,
    getGlobalSettingsResponse_httpStatus,

    -- ** DeleteMeeting
    deleteMeeting_meetingId,

    -- ** ListMeetings
    listMeetings_nextToken,
    listMeetings_maxResults,
    listMeetingsResponse_meetings,
    listMeetingsResponse_nextToken,
    listMeetingsResponse_httpStatus,

    -- ** GetAttendee
    getAttendee_meetingId,
    getAttendee_attendeeId,
    getAttendeeResponse_attendee,
    getAttendeeResponse_httpStatus,

    -- ** DeleteAccount
    deleteAccount_accountId,
    deleteAccountResponse_httpStatus,

    -- ** UpdateAccount
    updateAccount_defaultLicense,
    updateAccount_name,
    updateAccount_accountId,
    updateAccountResponse_account,
    updateAccountResponse_httpStatus,

    -- ** ListAccounts
    listAccounts_nextToken,
    listAccounts_name,
    listAccounts_userEmail,
    listAccounts_maxResults,
    listAccountsResponse_accounts,
    listAccountsResponse_nextToken,
    listAccountsResponse_httpStatus,

    -- ** UpdateBot
    updateBot_disabled,
    updateBot_accountId,
    updateBot_botId,
    updateBotResponse_bot,
    updateBotResponse_httpStatus,

    -- ** ListPhoneNumberOrders
    listPhoneNumberOrders_nextToken,
    listPhoneNumberOrders_maxResults,
    listPhoneNumberOrdersResponse_phoneNumberOrders,
    listPhoneNumberOrdersResponse_nextToken,
    listPhoneNumberOrdersResponse_httpStatus,

    -- ** SearchAvailablePhoneNumbers
    searchAvailablePhoneNumbers_phoneNumberType,
    searchAvailablePhoneNumbers_state,
    searchAvailablePhoneNumbers_tollFreePrefix,
    searchAvailablePhoneNumbers_country,
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_city,
    searchAvailablePhoneNumbers_areaCode,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbersResponse_e164PhoneNumbers,
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_httpStatus,

    -- ** CreateAppInstanceAdmin
    createAppInstanceAdmin_appInstanceAdminArn,
    createAppInstanceAdmin_appInstanceArn,
    createAppInstanceAdminResponse_appInstanceAdmin,
    createAppInstanceAdminResponse_appInstanceArn,
    createAppInstanceAdminResponse_httpStatus,

    -- ** TagMeeting
    tagMeeting_meetingId,
    tagMeeting_tags,

    -- ** ListVoiceConnectorGroups
    listVoiceConnectorGroups_nextToken,
    listVoiceConnectorGroups_maxResults,
    listVoiceConnectorGroupsResponse_voiceConnectorGroups,
    listVoiceConnectorGroupsResponse_nextToken,
    listVoiceConnectorGroupsResponse_httpStatus,

    -- ** LogoutUser
    logoutUser_accountId,
    logoutUser_userId,
    logoutUserResponse_httpStatus,

    -- ** ListVoiceConnectorTerminationCredentials
    listVoiceConnectorTerminationCredentials_voiceConnectorId,
    listVoiceConnectorTerminationCredentialsResponse_usernames,
    listVoiceConnectorTerminationCredentialsResponse_httpStatus,

    -- ** CreateMediaCapturePipeline
    createMediaCapturePipeline_chimeSdkMeetingConfiguration,
    createMediaCapturePipeline_clientRequestToken,
    createMediaCapturePipeline_sourceType,
    createMediaCapturePipeline_sourceArn,
    createMediaCapturePipeline_sinkType,
    createMediaCapturePipeline_sinkArn,
    createMediaCapturePipelineResponse_mediaCapturePipeline,
    createMediaCapturePipelineResponse_httpStatus,

    -- ** CreateProxySession
    createProxySession_numberSelectionBehavior,
    createProxySession_geoMatchParams,
    createProxySession_expiryMinutes,
    createProxySession_name,
    createProxySession_geoMatchLevel,
    createProxySession_participantPhoneNumbers,
    createProxySession_capabilities,
    createProxySession_voiceConnectorId,
    createProxySessionResponse_proxySession,
    createProxySessionResponse_httpStatus,

    -- ** DeleteEventsConfiguration
    deleteEventsConfiguration_accountId,
    deleteEventsConfiguration_botId,

    -- ** PutEventsConfiguration
    putEventsConfiguration_lambdaFunctionArn,
    putEventsConfiguration_outboundEventsHTTPSEndpoint,
    putEventsConfiguration_accountId,
    putEventsConfiguration_botId,
    putEventsConfigurationResponse_eventsConfiguration,
    putEventsConfigurationResponse_httpStatus,

    -- ** GetChannelMessage
    getChannelMessage_chimeBearer,
    getChannelMessage_channelArn,
    getChannelMessage_messageId,
    getChannelMessageResponse_channelMessage,
    getChannelMessageResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_name,
    updateRoom_accountId,
    updateRoom_roomId,
    updateRoomResponse_room,
    updateRoomResponse_httpStatus,

    -- ** DeleteRoom
    deleteRoom_accountId,
    deleteRoom_roomId,

    -- ** PutSipMediaApplicationLoggingConfiguration
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** DescribeChannelMembershipForAppInstanceUser
    describeChannelMembershipForAppInstanceUser_chimeBearer,
    describeChannelMembershipForAppInstanceUser_channelArn,
    describeChannelMembershipForAppInstanceUser_appInstanceUserArn,
    describeChannelMembershipForAppInstanceUserResponse_channelMembership,
    describeChannelMembershipForAppInstanceUserResponse_httpStatus,

    -- ** ListAppInstanceAdmins
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_appInstanceArn,
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_httpStatus,

    -- ** DeletePhoneNumber
    deletePhoneNumber_phoneNumberId,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_productType,
    updatePhoneNumber_callingName,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_httpStatus,

    -- ** ListPhoneNumbers
    listPhoneNumbers_status,
    listPhoneNumbers_filterName,
    listPhoneNumbers_productType,
    listPhoneNumbers_nextToken,
    listPhoneNumbers_filterValue,
    listPhoneNumbers_maxResults,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumbers,
    listPhoneNumbersResponse_httpStatus,

    -- ** CreateChannelModerator
    createChannelModerator_chimeBearer,
    createChannelModerator_channelArn,
    createChannelModerator_channelModeratorArn,
    createChannelModeratorResponse_channelModerator,
    createChannelModeratorResponse_channelArn,
    createChannelModeratorResponse_httpStatus,

    -- ** GetAppInstanceStreamingConfigurations
    getAppInstanceStreamingConfigurations_appInstanceArn,
    getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations,
    getAppInstanceStreamingConfigurationsResponse_httpStatus,

    -- ** ListAppInstances
    listAppInstances_nextToken,
    listAppInstances_maxResults,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_httpStatus,

    -- ** DescribeChannelModeratedByAppInstanceUser
    describeChannelModeratedByAppInstanceUser_chimeBearer,
    describeChannelModeratedByAppInstanceUser_channelArn,
    describeChannelModeratedByAppInstanceUser_appInstanceUserArn,
    describeChannelModeratedByAppInstanceUserResponse_channel,
    describeChannelModeratedByAppInstanceUserResponse_httpStatus,

    -- ** GetPhoneNumber
    getPhoneNumber_phoneNumberId,
    getPhoneNumberResponse_phoneNumber,
    getPhoneNumberResponse_httpStatus,

    -- ** GetEventsConfiguration
    getEventsConfiguration_accountId,
    getEventsConfiguration_botId,
    getEventsConfigurationResponse_eventsConfiguration,
    getEventsConfigurationResponse_httpStatus,

    -- ** GetSipMediaApplicationLoggingConfiguration
    getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    getSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** BatchUpdateUser
    batchUpdateUser_accountId,
    batchUpdateUser_updateUserRequestItems,
    batchUpdateUserResponse_userErrors,
    batchUpdateUserResponse_httpStatus,

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

    -- ** TagAttendee
    tagAttendee_meetingId,
    tagAttendee_attendeeId,
    tagAttendee_tags,

    -- ** UpdateVoiceConnector
    updateVoiceConnector_voiceConnectorId,
    updateVoiceConnector_name,
    updateVoiceConnector_requireEncryption,
    updateVoiceConnectorResponse_voiceConnector,
    updateVoiceConnectorResponse_httpStatus,

    -- ** DeleteVoiceConnector
    deleteVoiceConnector_voiceConnectorId,

    -- ** GetMediaCapturePipeline
    getMediaCapturePipeline_mediaPipelineId,
    getMediaCapturePipelineResponse_mediaCapturePipeline,
    getMediaCapturePipelineResponse_httpStatus,

    -- ** UpdateRoomMembership
    updateRoomMembership_role,
    updateRoomMembership_accountId,
    updateRoomMembership_roomId,
    updateRoomMembership_memberId,
    updateRoomMembershipResponse_roomMembership,
    updateRoomMembershipResponse_httpStatus,

    -- ** GetProxySession
    getProxySession_voiceConnectorId,
    getProxySession_proxySessionId,
    getProxySessionResponse_proxySession,
    getProxySessionResponse_httpStatus,

    -- ** DeleteRoomMembership
    deleteRoomMembership_accountId,
    deleteRoomMembership_roomId,
    deleteRoomMembership_memberId,

    -- ** DescribeAppInstanceUser
    describeAppInstanceUser_appInstanceUserArn,
    describeAppInstanceUserResponse_appInstanceUser,
    describeAppInstanceUserResponse_httpStatus,

    -- ** BatchUnsuspendUser
    batchUnsuspendUser_accountId,
    batchUnsuspendUser_userIdList,
    batchUnsuspendUserResponse_userErrors,
    batchUnsuspendUserResponse_httpStatus,

    -- ** DeleteChannelBan
    deleteChannelBan_chimeBearer,
    deleteChannelBan_channelArn,
    deleteChannelBan_memberArn,

    -- ** GetMeeting
    getMeeting_meetingId,
    getMeetingResponse_meeting,
    getMeetingResponse_httpStatus,

    -- ** RestorePhoneNumber
    restorePhoneNumber_phoneNumberId,
    restorePhoneNumberResponse_phoneNumber,
    restorePhoneNumberResponse_httpStatus,

    -- ** GetRetentionSettings
    getRetentionSettings_accountId,
    getRetentionSettingsResponse_retentionSettings,
    getRetentionSettingsResponse_initiateDeletionTimestamp,
    getRetentionSettingsResponse_httpStatus,

    -- ** GetBot
    getBot_accountId,
    getBot_botId,
    getBotResponse_bot,
    getBotResponse_httpStatus,

    -- ** GetUser
    getUser_accountId,
    getUser_userId,
    getUserResponse_user,
    getUserResponse_httpStatus,

    -- ** UntagAttendee
    untagAttendee_meetingId,
    untagAttendee_tagKeys,
    untagAttendee_attendeeId,

    -- ** StartMeetingTranscription
    startMeetingTranscription_meetingId,
    startMeetingTranscription_transcriptionConfiguration,
    startMeetingTranscriptionResponse_httpStatus,

    -- ** ListChannelBans
    listChannelBans_chimeBearer,
    listChannelBans_nextToken,
    listChannelBans_maxResults,
    listChannelBans_channelArn,
    listChannelBansResponse_channelArn,
    listChannelBansResponse_nextToken,
    listChannelBansResponse_channelBans,
    listChannelBansResponse_httpStatus,

    -- ** CreateChannel
    createChannel_mode,
    createChannel_privacy,
    createChannel_chimeBearer,
    createChannel_metadata,
    createChannel_tags,
    createChannel_appInstanceArn,
    createChannel_name,
    createChannel_clientRequestToken,
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,

    -- ** BatchSuspendUser
    batchSuspendUser_accountId,
    batchSuspendUser_userIdList,
    batchSuspendUserResponse_userErrors,
    batchSuspendUserResponse_httpStatus,

    -- ** GetAccount
    getAccount_accountId,
    getAccountResponse_account,
    getAccountResponse_httpStatus,

    -- ** DescribeChannelModerator
    describeChannelModerator_chimeBearer,
    describeChannelModerator_channelArn,
    describeChannelModerator_channelModeratorArn,
    describeChannelModeratorResponse_channelModerator,
    describeChannelModeratorResponse_httpStatus,

    -- ** AssociatePhoneNumbersWithVoiceConnector
    associatePhoneNumbersWithVoiceConnector_forceAssociate,
    associatePhoneNumbersWithVoiceConnector_voiceConnectorId,
    associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorResponse_httpStatus,

    -- ** GetPhoneNumberOrder
    getPhoneNumberOrder_phoneNumberOrderId,
    getPhoneNumberOrderResponse_phoneNumberOrder,
    getPhoneNumberOrderResponse_httpStatus,

    -- ** GetSipRule
    getSipRule_sipRuleId,
    getSipRuleResponse_sipRule,
    getSipRuleResponse_httpStatus,

    -- ** GetUserSettings
    getUserSettings_accountId,
    getUserSettings_userId,
    getUserSettingsResponse_userSettings,
    getUserSettingsResponse_httpStatus,

    -- ** GetSipMediaApplication
    getSipMediaApplication_sipMediaApplicationId,
    getSipMediaApplicationResponse_sipMediaApplication,
    getSipMediaApplicationResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettings_accountId,
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** CreateChannelBan
    createChannelBan_chimeBearer,
    createChannelBan_channelArn,
    createChannelBan_memberArn,
    createChannelBanResponse_channelArn,
    createChannelBanResponse_member,
    createChannelBanResponse_httpStatus,

    -- ** ListMeetingTags
    listMeetingTags_meetingId,
    listMeetingTagsResponse_tags,
    listMeetingTagsResponse_httpStatus,

    -- ** ListChannelMembershipsForAppInstanceUser
    listChannelMembershipsForAppInstanceUser_appInstanceUserArn,
    listChannelMembershipsForAppInstanceUser_chimeBearer,
    listChannelMembershipsForAppInstanceUser_nextToken,
    listChannelMembershipsForAppInstanceUser_maxResults,
    listChannelMembershipsForAppInstanceUserResponse_channelMemberships,
    listChannelMembershipsForAppInstanceUserResponse_nextToken,
    listChannelMembershipsForAppInstanceUserResponse_httpStatus,

    -- ** GetVoiceConnectorOrigination
    getVoiceConnectorOrigination_voiceConnectorId,
    getVoiceConnectorOriginationResponse_origination,
    getVoiceConnectorOriginationResponse_httpStatus,

    -- ** BatchUpdatePhoneNumber
    batchUpdatePhoneNumber_updatePhoneNumberRequestItems,
    batchUpdatePhoneNumberResponse_phoneNumberErrors,
    batchUpdatePhoneNumberResponse_httpStatus,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId,
    disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus,

    -- ** UpdateChannelReadMarker
    updateChannelReadMarker_chimeBearer,
    updateChannelReadMarker_channelArn,
    updateChannelReadMarkerResponse_channelArn,
    updateChannelReadMarkerResponse_httpStatus,

    -- ** CreateSipMediaApplicationCall
    createSipMediaApplicationCall_sipHeaders,
    createSipMediaApplicationCall_fromPhoneNumber,
    createSipMediaApplicationCall_toPhoneNumber,
    createSipMediaApplicationCall_sipMediaApplicationId,
    createSipMediaApplicationCallResponse_sipMediaApplicationCall,
    createSipMediaApplicationCallResponse_httpStatus,

    -- ** BatchDeletePhoneNumber
    batchDeletePhoneNumber_phoneNumberIds,
    batchDeletePhoneNumberResponse_phoneNumberErrors,
    batchDeletePhoneNumberResponse_httpStatus,

    -- ** ListSipMediaApplications
    listSipMediaApplications_nextToken,
    listSipMediaApplications_maxResults,
    listSipMediaApplicationsResponse_nextToken,
    listSipMediaApplicationsResponse_sipMediaApplications,
    listSipMediaApplicationsResponse_httpStatus,

    -- ** CreateMeeting
    createMeeting_mediaRegion,
    createMeeting_meetingHostId,
    createMeeting_notificationsConfiguration,
    createMeeting_externalMeetingId,
    createMeeting_tags,
    createMeeting_clientRequestToken,
    createMeetingResponse_meeting,
    createMeetingResponse_httpStatus,

    -- ** CreatePhoneNumberOrder
    createPhoneNumberOrder_productType,
    createPhoneNumberOrder_e164PhoneNumbers,
    createPhoneNumberOrderResponse_phoneNumberOrder,
    createPhoneNumberOrderResponse_httpStatus,

    -- ** ListSipRules
    listSipRules_nextToken,
    listSipRules_maxResults,
    listSipRules_sipMediaApplicationId,
    listSipRulesResponse_nextToken,
    listSipRulesResponse_sipRules,
    listSipRulesResponse_httpStatus,

    -- ** CreateBot
    createBot_domain,
    createBot_displayName,
    createBot_accountId,
    createBotResponse_bot,
    createBotResponse_httpStatus,

    -- ** UpdateUserSettings
    updateUserSettings_accountId,
    updateUserSettings_userId,
    updateUserSettings_userSettings,

    -- ** CreateUser
    createUser_email,
    createUser_username,
    createUser_userType,
    createUser_accountId,
    createUserResponse_user,
    createUserResponse_httpStatus,

    -- ** BatchCreateRoomMembership
    batchCreateRoomMembership_accountId,
    batchCreateRoomMembership_roomId,
    batchCreateRoomMembership_membershipItemList,
    batchCreateRoomMembershipResponse_errors,
    batchCreateRoomMembershipResponse_httpStatus,

    -- ** DescribeAppInstance
    describeAppInstance_appInstanceArn,
    describeAppInstanceResponse_appInstance,
    describeAppInstanceResponse_httpStatus,

    -- ** CreateAccount
    createAccount_name,
    createAccountResponse_account,
    createAccountResponse_httpStatus,

    -- ** CreateChannelMembership
    createChannelMembership_chimeBearer,
    createChannelMembership_channelArn,
    createChannelMembership_memberArn,
    createChannelMembership_type,
    createChannelMembershipResponse_channelArn,
    createChannelMembershipResponse_member,
    createChannelMembershipResponse_httpStatus,

    -- ** DeleteVoiceConnectorTermination
    deleteVoiceConnectorTermination_voiceConnectorId,

    -- ** AssociatePhoneNumberWithUser
    associatePhoneNumberWithUser_accountId,
    associatePhoneNumberWithUser_userId,
    associatePhoneNumberWithUser_e164PhoneNumber,
    associatePhoneNumberWithUserResponse_httpStatus,

    -- ** DeleteVoiceConnectorProxy
    deleteVoiceConnectorProxy_voiceConnectorId,

    -- ** CreateSipMediaApplication
    createSipMediaApplication_awsRegion,
    createSipMediaApplication_name,
    createSipMediaApplication_endpoints,
    createSipMediaApplicationResponse_sipMediaApplication,
    createSipMediaApplicationResponse_httpStatus,

    -- ** PutVoiceConnectorProxy
    putVoiceConnectorProxy_disabled,
    putVoiceConnectorProxy_fallBackPhoneNumber,
    putVoiceConnectorProxy_defaultSessionExpiryMinutes,
    putVoiceConnectorProxy_phoneNumberPoolCountries,
    putVoiceConnectorProxy_voiceConnectorId,
    putVoiceConnectorProxyResponse_proxy,
    putVoiceConnectorProxyResponse_httpStatus,

    -- ** UpdateUser
    updateUser_licenseType,
    updateUser_userType,
    updateUser_alexaForBusinessMetadata,
    updateUser_accountId,
    updateUser_userId,
    updateUserResponse_user,
    updateUserResponse_httpStatus,

    -- ** PutVoiceConnectorTermination
    putVoiceConnectorTermination_voiceConnectorId,
    putVoiceConnectorTermination_termination,
    putVoiceConnectorTerminationResponse_termination,
    putVoiceConnectorTerminationResponse_httpStatus,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    getVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    getVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    getVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorTerminationCredentials
    putVoiceConnectorTerminationCredentials_credentials,
    putVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** ListAppInstanceUsers
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_appInstanceArn,
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_httpStatus,

    -- ** AssociateSigninDelegateGroupsWithAccount
    associateSigninDelegateGroupsWithAccount_accountId,
    associateSigninDelegateGroupsWithAccount_signinDelegateGroups,
    associateSigninDelegateGroupsWithAccountResponse_httpStatus,

    -- ** CreateSipRule
    createSipRule_disabled,
    createSipRule_name,
    createSipRule_triggerType,
    createSipRule_triggerValue,
    createSipRule_targetApplications,
    createSipRuleResponse_sipRule,
    createSipRuleResponse_httpStatus,

    -- ** DeleteVoiceConnectorTerminationCredentials
    deleteVoiceConnectorTerminationCredentials_usernames,
    deleteVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** DeleteAppInstanceUser
    deleteAppInstanceUser_appInstanceUserArn,

    -- ** UpdateAppInstanceUser
    updateAppInstanceUser_metadata,
    updateAppInstanceUser_appInstanceUserArn,
    updateAppInstanceUser_name,
    updateAppInstanceUserResponse_appInstanceUserArn,
    updateAppInstanceUserResponse_httpStatus,

    -- ** UntagMeeting
    untagMeeting_meetingId,
    untagMeeting_tagKeys,

    -- ** UpdateVoiceConnectorGroup
    updateVoiceConnectorGroup_voiceConnectorGroupId,
    updateVoiceConnectorGroup_name,
    updateVoiceConnectorGroup_voiceConnectorItems,
    updateVoiceConnectorGroupResponse_voiceConnectorGroup,
    updateVoiceConnectorGroupResponse_httpStatus,

    -- ** RedactConversationMessage
    redactConversationMessage_accountId,
    redactConversationMessage_conversationId,
    redactConversationMessage_messageId,
    redactConversationMessageResponse_httpStatus,

    -- ** DeleteChannelModerator
    deleteChannelModerator_chimeBearer,
    deleteChannelModerator_channelArn,
    deleteChannelModerator_channelModeratorArn,

    -- ** DeleteVoiceConnectorGroup
    deleteVoiceConnectorGroup_voiceConnectorGroupId,

    -- ** DescribeChannelBan
    describeChannelBan_chimeBearer,
    describeChannelBan_channelArn,
    describeChannelBan_memberArn,
    describeChannelBanResponse_channelBan,
    describeChannelBanResponse_httpStatus,

    -- ** DeleteMediaCapturePipeline
    deleteMediaCapturePipeline_mediaPipelineId,

    -- ** UpdateProxySession
    updateProxySession_expiryMinutes,
    updateProxySession_capabilities,
    updateProxySession_voiceConnectorId,
    updateProxySession_proxySessionId,
    updateProxySessionResponse_proxySession,
    updateProxySessionResponse_httpStatus,

    -- ** DeleteProxySession
    deleteProxySession_voiceConnectorId,
    deleteProxySession_proxySessionId,

    -- ** GetVoiceConnectorTerminationHealth
    getVoiceConnectorTerminationHealth_voiceConnectorId,
    getVoiceConnectorTerminationHealthResponse_terminationHealth,
    getVoiceConnectorTerminationHealthResponse_httpStatus,

    -- ** CreateMeetingDialOut
    createMeetingDialOut_meetingId,
    createMeetingDialOut_fromPhoneNumber,
    createMeetingDialOut_toPhoneNumber,
    createMeetingDialOut_joinToken,
    createMeetingDialOutResponse_transactionId,
    createMeetingDialOutResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** ListProxySessions
    listProxySessions_status,
    listProxySessions_nextToken,
    listProxySessions_maxResults,
    listProxySessions_voiceConnectorId,
    listProxySessionsResponse_nextToken,
    listProxySessionsResponse_proxySessions,
    listProxySessionsResponse_httpStatus,

    -- ** ListMediaCapturePipelines
    listMediaCapturePipelines_nextToken,
    listMediaCapturePipelines_maxResults,
    listMediaCapturePipelinesResponse_nextToken,
    listMediaCapturePipelinesResponse_mediaCapturePipelines,
    listMediaCapturePipelinesResponse_httpStatus,

    -- ** UpdatePhoneNumberSettings
    updatePhoneNumberSettings_callingName,

    -- ** InviteUsers
    inviteUsers_userType,
    inviteUsers_accountId,
    inviteUsers_userEmailList,
    inviteUsersResponse_invites,
    inviteUsersResponse_httpStatus,

    -- ** CreateRoom
    createRoom_clientRequestToken,
    createRoom_accountId,
    createRoom_name,
    createRoomResponse_room,
    createRoomResponse_httpStatus,

    -- ** ListChannelModerators
    listChannelModerators_chimeBearer,
    listChannelModerators_nextToken,
    listChannelModerators_maxResults,
    listChannelModerators_channelArn,
    listChannelModeratorsResponse_channelArn,
    listChannelModeratorsResponse_nextToken,
    listChannelModeratorsResponse_channelModerators,
    listChannelModeratorsResponse_httpStatus,

    -- ** GetVoiceConnector
    getVoiceConnector_voiceConnectorId,
    getVoiceConnectorResponse_voiceConnector,
    getVoiceConnectorResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_chimeBearer,
    describeChannel_channelArn,
    describeChannelResponse_channel,
    describeChannelResponse_httpStatus,

    -- ** CreateVoiceConnectorGroup
    createVoiceConnectorGroup_voiceConnectorItems,
    createVoiceConnectorGroup_name,
    createVoiceConnectorGroupResponse_voiceConnectorGroup,
    createVoiceConnectorGroupResponse_httpStatus,

    -- ** DeleteAppInstanceStreamingConfigurations
    deleteAppInstanceStreamingConfigurations_appInstanceArn,

    -- ** ListRooms
    listRooms_memberId,
    listRooms_nextToken,
    listRooms_maxResults,
    listRooms_accountId,
    listRoomsResponse_rooms,
    listRoomsResponse_nextToken,
    listRoomsResponse_httpStatus,

    -- ** BatchCreateAttendee
    batchCreateAttendee_meetingId,
    batchCreateAttendee_attendees,
    batchCreateAttendeeResponse_attendees,
    batchCreateAttendeeResponse_errors,
    batchCreateAttendeeResponse_httpStatus,

    -- ** DeleteAppInstanceAdmin
    deleteAppInstanceAdmin_appInstanceAdminArn,
    deleteAppInstanceAdmin_appInstanceArn,

    -- ** PutAppInstanceStreamingConfigurations
    putAppInstanceStreamingConfigurations_appInstanceArn,
    putAppInstanceStreamingConfigurations_appInstanceStreamingConfigurations,
    putAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations,
    putAppInstanceStreamingConfigurationsResponse_httpStatus,

    -- ** RegenerateSecurityToken
    regenerateSecurityToken_accountId,
    regenerateSecurityToken_botId,
    regenerateSecurityTokenResponse_bot,
    regenerateSecurityTokenResponse_httpStatus,

    -- ** DeleteChannelMessage
    deleteChannelMessage_chimeBearer,
    deleteChannelMessage_channelArn,
    deleteChannelMessage_messageId,

    -- ** UpdateChannelMessage
    updateChannelMessage_chimeBearer,
    updateChannelMessage_content,
    updateChannelMessage_metadata,
    updateChannelMessage_channelArn,
    updateChannelMessage_messageId,
    updateChannelMessageResponse_channelArn,
    updateChannelMessageResponse_messageId,
    updateChannelMessageResponse_httpStatus,

    -- ** DeleteAppInstance
    deleteAppInstance_appInstanceArn,

    -- ** UpdateAppInstance
    updateAppInstance_metadata,
    updateAppInstance_appInstanceArn,
    updateAppInstance_name,
    updateAppInstanceResponse_appInstanceArn,
    updateAppInstanceResponse_httpStatus,

    -- ** CreateVoiceConnector
    createVoiceConnector_awsRegion,
    createVoiceConnector_name,
    createVoiceConnector_requireEncryption,
    createVoiceConnectorResponse_voiceConnector,
    createVoiceConnectorResponse_httpStatus,

    -- ** ListChannelMessages
    listChannelMessages_chimeBearer,
    listChannelMessages_nextToken,
    listChannelMessages_notBefore,
    listChannelMessages_sortOrder,
    listChannelMessages_maxResults,
    listChannelMessages_notAfter,
    listChannelMessages_channelArn,
    listChannelMessagesResponse_channelArn,
    listChannelMessagesResponse_nextToken,
    listChannelMessagesResponse_channelMessages,
    listChannelMessagesResponse_httpStatus,

    -- ** RedactRoomMessage
    redactRoomMessage_accountId,
    redactRoomMessage_roomId,
    redactRoomMessage_messageId,
    redactRoomMessageResponse_httpStatus,

    -- ** GetRoom
    getRoom_accountId,
    getRoom_roomId,
    getRoomResponse_room,
    getRoomResponse_httpStatus,

    -- ** CreateRoomMembership
    createRoomMembership_role,
    createRoomMembership_accountId,
    createRoomMembership_roomId,
    createRoomMembership_memberId,
    createRoomMembershipResponse_roomMembership,
    createRoomMembershipResponse_httpStatus,

    -- ** BatchCreateChannelMembership
    batchCreateChannelMembership_chimeBearer,
    batchCreateChannelMembership_type,
    batchCreateChannelMembership_channelArn,
    batchCreateChannelMembership_memberArns,
    batchCreateChannelMembershipResponse_errors,
    batchCreateChannelMembershipResponse_batchChannelMemberships,
    batchCreateChannelMembershipResponse_httpStatus,

    -- * Types

    -- ** Account
    account_signinDelegateGroups,
    account_accountStatus,
    account_defaultLicense,
    account_supportedLicenses,
    account_createdTimestamp,
    account_accountType,
    account_awsAccountId,
    account_accountId,
    account_name,

    -- ** AccountSettings
    accountSettings_enableDialOut,
    accountSettings_disableRemoteControl,

    -- ** AlexaForBusinessMetadata
    alexaForBusinessMetadata_alexaForBusinessRoomArn,
    alexaForBusinessMetadata_isAlexaForBusinessEnabled,

    -- ** AppInstance
    appInstance_name,
    appInstance_metadata,
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,

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
    appInstanceSummary_name,
    appInstanceSummary_metadata,
    appInstanceSummary_appInstanceArn,

    -- ** AppInstanceUser
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_name,
    appInstanceUser_metadata,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,

    -- ** AppInstanceUserMembershipSummary
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

    -- ** AppInstanceUserSummary
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_name,
    appInstanceUserSummary_metadata,

    -- ** ArtifactsConfiguration
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- ** Attendee
    attendee_attendeeId,
    attendee_joinToken,
    attendee_externalUserId,

    -- ** AudioArtifactsConfiguration
    audioArtifactsConfiguration_muxType,

    -- ** BatchChannelMemberships
    batchChannelMemberships_members,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_type,
    batchChannelMemberships_invitedBy,

    -- ** BatchCreateChannelMembershipError
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_memberArn,
    batchCreateChannelMembershipError_errorMessage,

    -- ** Bot
    bot_securityToken,
    bot_disabled,
    bot_updatedTimestamp,
    bot_userId,
    bot_botId,
    bot_displayName,
    bot_botEmail,
    bot_createdTimestamp,
    bot_botType,

    -- ** BusinessCallingSettings
    businessCallingSettings_cdrBucket,

    -- ** Channel
    channel_mode,
    channel_createdBy,
    channel_channelArn,
    channel_privacy,
    channel_lastMessageTimestamp,
    channel_name,
    channel_metadata,
    channel_createdTimestamp,
    channel_lastUpdatedTimestamp,

    -- ** ChannelBan
    channelBan_createdBy,
    channelBan_channelArn,
    channelBan_member,
    channelBan_createdTimestamp,

    -- ** ChannelBanSummary
    channelBanSummary_member,

    -- ** ChannelMembership
    channelMembership_channelArn,
    channelMembership_member,
    channelMembership_type,
    channelMembership_invitedBy,
    channelMembership_createdTimestamp,
    channelMembership_lastUpdatedTimestamp,

    -- ** ChannelMembershipForAppInstanceUserSummary
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,

    -- ** ChannelMembershipSummary
    channelMembershipSummary_member,

    -- ** ChannelMessage
    channelMessage_sender,
    channelMessage_channelArn,
    channelMessage_content,
    channelMessage_redacted,
    channelMessage_persistence,
    channelMessage_metadata,
    channelMessage_type,
    channelMessage_createdTimestamp,
    channelMessage_messageId,
    channelMessage_lastUpdatedTimestamp,
    channelMessage_lastEditedTimestamp,

    -- ** ChannelMessageSummary
    channelMessageSummary_sender,
    channelMessageSummary_content,
    channelMessageSummary_redacted,
    channelMessageSummary_metadata,
    channelMessageSummary_type,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_messageId,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_lastEditedTimestamp,

    -- ** ChannelModeratedByAppInstanceUserSummary
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- ** ChannelModerator
    channelModerator_createdBy,
    channelModerator_channelArn,
    channelModerator_createdTimestamp,
    channelModerator_moderator,

    -- ** ChannelModeratorSummary
    channelModeratorSummary_moderator,

    -- ** ChannelRetentionSettings
    channelRetentionSettings_retentionDays,

    -- ** ChannelSummary
    channelSummary_mode,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,

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
    credential_username,
    credential_password,

    -- ** DNISEmergencyCallingConfiguration
    dNISEmergencyCallingConfiguration_testPhoneNumber,
    dNISEmergencyCallingConfiguration_emergencyPhoneNumber,
    dNISEmergencyCallingConfiguration_callingCountry,

    -- ** EmergencyCallingConfiguration
    emergencyCallingConfiguration_dnis,

    -- ** EngineTranscribeMedicalSettings
    engineTranscribeMedicalSettings_vocabularyName,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- ** EngineTranscribeSettings
    engineTranscribeSettings_vocabularyName,
    engineTranscribeSettings_vocabularyFilterName,
    engineTranscribeSettings_vocabularyFilterMethod,
    engineTranscribeSettings_region,
    engineTranscribeSettings_languageCode,

    -- ** EventsConfiguration
    eventsConfiguration_lambdaFunctionArn,
    eventsConfiguration_botId,
    eventsConfiguration_outboundEventsHTTPSEndpoint,

    -- ** GeoMatchParams
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- ** Identity
    identity_arn,
    identity_name,

    -- ** Invite
    invite_status,
    invite_emailStatus,
    invite_inviteId,
    invite_emailAddress,

    -- ** LoggingConfiguration
    loggingConfiguration_enableSIPLogs,

    -- ** MediaCapturePipeline
    mediaCapturePipeline_status,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_updatedTimestamp,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_sinkArn,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_createdTimestamp,

    -- ** MediaPlacement
    mediaPlacement_screenDataUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_signalingUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_turnControlUrl,

    -- ** Meeting
    meeting_mediaRegion,
    meeting_mediaPlacement,
    meeting_externalMeetingId,
    meeting_meetingId,

    -- ** MeetingNotificationConfiguration
    meetingNotificationConfiguration_snsTopicArn,
    meetingNotificationConfiguration_sqsQueueArn,

    -- ** Member
    member_fullName,
    member_email,
    member_memberId,
    member_memberType,
    member_accountId,

    -- ** MemberError
    memberError_memberId,
    memberError_errorCode,
    memberError_errorMessage,

    -- ** MembershipItem
    membershipItem_memberId,
    membershipItem_role,

    -- ** MessagingSessionEndpoint
    messagingSessionEndpoint_url,

    -- ** OrderedPhoneNumber
    orderedPhoneNumber_status,
    orderedPhoneNumber_e164PhoneNumber,

    -- ** Origination
    origination_routes,
    origination_disabled,

    -- ** OriginationRoute
    originationRoute_priority,
    originationRoute_weight,
    originationRoute_protocol,
    originationRoute_host,
    originationRoute_port,

    -- ** Participant
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- ** PhoneNumber
    phoneNumber_status,
    phoneNumber_deletionTimestamp,
    phoneNumber_phoneNumberId,
    phoneNumber_country,
    phoneNumber_updatedTimestamp,
    phoneNumber_productType,
    phoneNumber_e164PhoneNumber,
    phoneNumber_associations,
    phoneNumber_callingName,
    phoneNumber_type,
    phoneNumber_createdTimestamp,
    phoneNumber_capabilities,
    phoneNumber_callingNameStatus,

    -- ** PhoneNumberAssociation
    phoneNumberAssociation_value,
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,

    -- ** PhoneNumberCapabilities
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundSMS,

    -- ** PhoneNumberCountry
    phoneNumberCountry_supportedPhoneNumberTypes,
    phoneNumberCountry_countryCode,

    -- ** PhoneNumberError
    phoneNumberError_phoneNumberId,
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,

    -- ** PhoneNumberOrder
    phoneNumberOrder_status,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_updatedTimestamp,
    phoneNumberOrder_productType,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_createdTimestamp,

    -- ** Proxy
    proxy_defaultSessionExpiryMinutes,
    proxy_disabled,
    proxy_fallBackPhoneNumber,
    proxy_phoneNumberCountries,

    -- ** ProxySession
    proxySession_status,
    proxySession_numberSelectionBehavior,
    proxySession_geoMatchParams,
    proxySession_expiryMinutes,
    proxySession_endedTimestamp,
    proxySession_updatedTimestamp,
    proxySession_participants,
    proxySession_name,
    proxySession_proxySessionId,
    proxySession_geoMatchLevel,
    proxySession_voiceConnectorId,
    proxySession_createdTimestamp,
    proxySession_capabilities,

    -- ** RetentionSettings
    retentionSettings_roomRetentionSettings,
    retentionSettings_conversationRetentionSettings,

    -- ** Room
    room_updatedTimestamp,
    room_createdBy,
    room_accountId,
    room_name,
    room_roomId,
    room_createdTimestamp,

    -- ** RoomMembership
    roomMembership_updatedTimestamp,
    roomMembership_role,
    roomMembership_roomId,
    roomMembership_member,
    roomMembership_invitedBy,

    -- ** RoomRetentionSettings
    roomRetentionSettings_retentionDays,

    -- ** SelectedVideoStreams
    selectedVideoStreams_attendeeIds,
    selectedVideoStreams_externalUserIds,

    -- ** SigninDelegateGroup
    signinDelegateGroup_groupName,

    -- ** SipMediaApplication
    sipMediaApplication_updatedTimestamp,
    sipMediaApplication_name,
    sipMediaApplication_awsRegion,
    sipMediaApplication_endpoints,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_sipMediaApplicationId,

    -- ** SipMediaApplicationCall
    sipMediaApplicationCall_transactionId,

    -- ** SipMediaApplicationEndpoint
    sipMediaApplicationEndpoint_lambdaArn,

    -- ** SipMediaApplicationLoggingConfiguration
    sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs,

    -- ** SipRule
    sipRule_disabled,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_updatedTimestamp,
    sipRule_name,
    sipRule_triggerValue,
    sipRule_createdTimestamp,
    sipRule_sipRuleId,

    -- ** SipRuleTargetApplication
    sipRuleTargetApplication_priority,
    sipRuleTargetApplication_awsRegion,
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
    termination_defaultPhoneNumber,
    termination_disabled,
    termination_callingRegions,
    termination_cpsLimit,
    termination_cidrAllowedList,

    -- ** TerminationHealth
    terminationHealth_source,
    terminationHealth_timestamp,

    -- ** TranscriptionConfiguration
    transcriptionConfiguration_engineTranscribeMedicalSettings,
    transcriptionConfiguration_engineTranscribeSettings,

    -- ** UpdatePhoneNumberRequestItem
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- ** UpdateUserRequestItem
    updateUserRequestItem_licenseType,
    updateUserRequestItem_userType,
    updateUserRequestItem_alexaForBusinessMetadata,
    updateUserRequestItem_userId,

    -- ** User
    user_userInvitationStatus,
    user_personalPIN,
    user_primaryProvisionedNumber,
    user_licenseType,
    user_registeredOn,
    user_accountId,
    user_userRegistrationStatus,
    user_invitedOn,
    user_displayName,
    user_primaryEmail,
    user_userType,
    user_alexaForBusinessMetadata,
    user_userId,

    -- ** UserError
    userError_userId,
    userError_errorCode,
    userError_errorMessage,

    -- ** UserSettings
    userSettings_telephony,

    -- ** VideoArtifactsConfiguration
    videoArtifactsConfiguration_muxType,
    videoArtifactsConfiguration_state,

    -- ** VoiceConnector
    voiceConnector_updatedTimestamp,
    voiceConnector_outboundHostName,
    voiceConnector_name,
    voiceConnector_requireEncryption,
    voiceConnector_awsRegion,
    voiceConnector_voiceConnectorId,
    voiceConnector_voiceConnectorArn,
    voiceConnector_createdTimestamp,

    -- ** VoiceConnectorGroup
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorItems,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_name,
    voiceConnectorGroup_createdTimestamp,

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

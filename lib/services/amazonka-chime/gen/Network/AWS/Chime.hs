{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Chime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime API (application programming interface) is designed for
-- developers to perform key tasks, such as creating and managing Amazon
-- Chime accounts, users, and Voice Connectors. This guide provides
-- detailed information about the Amazon Chime API, including operations,
-- types, inputs and outputs, and error codes. It also includes some
-- server-side API actions to use with the Amazon Chime SDK. For more
-- information about the Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
--
-- You can use an AWS SDK, the AWS Command Line Interface (AWS CLI), or the
-- REST API to make API calls. We recommend using an AWS SDK or the AWS
-- CLI. Each API operation includes links to information about using it
-- with a language-specific AWS SDK or the AWS CLI.
--
-- [Using an AWS SDK]
--     You don\'t need to write code to calculate a signature for request
--     authentication. The SDK clients authenticate your requests by using
--     access keys that you provide. For more information about AWS SDKs,
--     see the <http://aws.amazon.com/developer/ AWS Developer Center>.
--
-- [Using the AWS CLI]
--     Use your access keys with the AWS CLI to make API calls. For
--     information about setting up the AWS CLI, see
--     <https://docs.aws.amazon.com/cli/latest/userguide/installing.html Installing the AWS Command Line Interface>
--     in the /AWS Command Line Interface User Guide/. For a list of
--     available Amazon Chime commands, see the
--     <https://docs.aws.amazon.com/cli/latest/reference/chime/index.html Amazon Chime commands>
--     in the /AWS CLI Command Reference/.
--
-- [Using REST APIs]
--     If you use REST to make API calls, you must authenticate your
--     request by providing a signature. Amazon Chime supports signature
--     version 4. For more information, see
--     <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>
--     in the /Amazon Web Services General Reference/.
--
--     When making REST API calls, use the service name @chime@ and REST
--     endpoint @https:\/\/service.chime.aws.amazon.com@.
--
-- Administrative permissions are controlled using AWS Identity and Access
-- Management (IAM). For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ag/security-iam.html Identity and Access Management for Amazon Chime>
-- in the /Amazon Chime Administration Guide/.
module Network.AWS.Chime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeChannelMembership
    DescribeChannelMembership (DescribeChannelMembership'),
    newDescribeChannelMembership,
    DescribeChannelMembershipResponse (DescribeChannelMembershipResponse'),
    newDescribeChannelMembershipResponse,

    -- ** CreateAppInstance
    CreateAppInstance (CreateAppInstance'),
    newCreateAppInstance,
    CreateAppInstanceResponse (CreateAppInstanceResponse'),
    newCreateAppInstanceResponse,

    -- ** GetVoiceConnectorGroup
    GetVoiceConnectorGroup (GetVoiceConnectorGroup'),
    newGetVoiceConnectorGroup,
    GetVoiceConnectorGroupResponse (GetVoiceConnectorGroupResponse'),
    newGetVoiceConnectorGroupResponse,

    -- ** ListVoiceConnectors
    ListVoiceConnectors (ListVoiceConnectors'),
    newListVoiceConnectors,
    ListVoiceConnectorsResponse (ListVoiceConnectorsResponse'),
    newListVoiceConnectorsResponse,

    -- ** ListRoomMemberships
    ListRoomMemberships (ListRoomMemberships'),
    newListRoomMemberships,
    ListRoomMembershipsResponse (ListRoomMembershipsResponse'),
    newListRoomMembershipsResponse,

    -- ** GetPhoneNumberSettings
    GetPhoneNumberSettings (GetPhoneNumberSettings'),
    newGetPhoneNumberSettings,
    GetPhoneNumberSettingsResponse (GetPhoneNumberSettingsResponse'),
    newGetPhoneNumberSettingsResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** ListAttendees
    ListAttendees (ListAttendees'),
    newListAttendees,
    ListAttendeesResponse (ListAttendeesResponse'),
    newListAttendeesResponse,

    -- ** PutVoiceConnectorLoggingConfiguration
    PutVoiceConnectorLoggingConfiguration (PutVoiceConnectorLoggingConfiguration'),
    newPutVoiceConnectorLoggingConfiguration,
    PutVoiceConnectorLoggingConfigurationResponse (PutVoiceConnectorLoggingConfigurationResponse'),
    newPutVoiceConnectorLoggingConfigurationResponse,

    -- ** GetVoiceConnectorTermination
    GetVoiceConnectorTermination (GetVoiceConnectorTermination'),
    newGetVoiceConnectorTermination,
    GetVoiceConnectorTerminationResponse (GetVoiceConnectorTerminationResponse'),
    newGetVoiceConnectorTerminationResponse,

    -- ** DeleteAttendee
    DeleteAttendee (DeleteAttendee'),
    newDeleteAttendee,
    DeleteAttendeeResponse (DeleteAttendeeResponse'),
    newDeleteAttendeeResponse,

    -- ** GetVoiceConnectorProxy
    GetVoiceConnectorProxy (GetVoiceConnectorProxy'),
    newGetVoiceConnectorProxy,
    GetVoiceConnectorProxyResponse (GetVoiceConnectorProxyResponse'),
    newGetVoiceConnectorProxyResponse,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    DeleteVoiceConnectorEmergencyCallingConfiguration (DeleteVoiceConnectorEmergencyCallingConfiguration'),
    newDeleteVoiceConnectorEmergencyCallingConfiguration,
    DeleteVoiceConnectorEmergencyCallingConfigurationResponse (DeleteVoiceConnectorEmergencyCallingConfigurationResponse'),
    newDeleteVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** GetVoiceConnectorStreamingConfiguration
    GetVoiceConnectorStreamingConfiguration (GetVoiceConnectorStreamingConfiguration'),
    newGetVoiceConnectorStreamingConfiguration,
    GetVoiceConnectorStreamingConfigurationResponse (GetVoiceConnectorStreamingConfigurationResponse'),
    newGetVoiceConnectorStreamingConfigurationResponse,

    -- ** UpdateSipMediaApplicationCall
    UpdateSipMediaApplicationCall (UpdateSipMediaApplicationCall'),
    newUpdateSipMediaApplicationCall,
    UpdateSipMediaApplicationCallResponse (UpdateSipMediaApplicationCallResponse'),
    newUpdateSipMediaApplicationCallResponse,

    -- ** StopMeetingTranscription
    StopMeetingTranscription (StopMeetingTranscription'),
    newStopMeetingTranscription,
    StopMeetingTranscriptionResponse (StopMeetingTranscriptionResponse'),
    newStopMeetingTranscriptionResponse,

    -- ** GetAppInstanceRetentionSettings
    GetAppInstanceRetentionSettings (GetAppInstanceRetentionSettings'),
    newGetAppInstanceRetentionSettings,
    GetAppInstanceRetentionSettingsResponse (GetAppInstanceRetentionSettingsResponse'),
    newGetAppInstanceRetentionSettingsResponse,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    PutVoiceConnectorEmergencyCallingConfiguration (PutVoiceConnectorEmergencyCallingConfiguration'),
    newPutVoiceConnectorEmergencyCallingConfiguration,
    PutVoiceConnectorEmergencyCallingConfigurationResponse (PutVoiceConnectorEmergencyCallingConfigurationResponse'),
    newPutVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** CreateMeetingWithAttendees
    CreateMeetingWithAttendees (CreateMeetingWithAttendees'),
    newCreateMeetingWithAttendees,
    CreateMeetingWithAttendeesResponse (CreateMeetingWithAttendeesResponse'),
    newCreateMeetingWithAttendeesResponse,

    -- ** ListChannels
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** DisassociatePhoneNumberFromUser
    DisassociatePhoneNumberFromUser (DisassociatePhoneNumberFromUser'),
    newDisassociatePhoneNumberFromUser,
    DisassociatePhoneNumberFromUserResponse (DisassociatePhoneNumberFromUserResponse'),
    newDisassociatePhoneNumberFromUserResponse,

    -- ** DisassociateSigninDelegateGroupsFromAccount
    DisassociateSigninDelegateGroupsFromAccount (DisassociateSigninDelegateGroupsFromAccount'),
    newDisassociateSigninDelegateGroupsFromAccount,
    DisassociateSigninDelegateGroupsFromAccountResponse (DisassociateSigninDelegateGroupsFromAccountResponse'),
    newDisassociateSigninDelegateGroupsFromAccountResponse,

    -- ** ResetPersonalPIN
    ResetPersonalPIN (ResetPersonalPIN'),
    newResetPersonalPIN,
    ResetPersonalPINResponse (ResetPersonalPINResponse'),
    newResetPersonalPINResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** DescribeAppInstanceAdmin
    DescribeAppInstanceAdmin (DescribeAppInstanceAdmin'),
    newDescribeAppInstanceAdmin,
    DescribeAppInstanceAdminResponse (DescribeAppInstanceAdminResponse'),
    newDescribeAppInstanceAdminResponse,

    -- ** CreateAttendee
    CreateAttendee (CreateAttendee'),
    newCreateAttendee,
    CreateAttendeeResponse (CreateAttendeeResponse'),
    newCreateAttendeeResponse,

    -- ** ListSupportedPhoneNumberCountries
    ListSupportedPhoneNumberCountries (ListSupportedPhoneNumberCountries'),
    newListSupportedPhoneNumberCountries,
    ListSupportedPhoneNumberCountriesResponse (ListSupportedPhoneNumberCountriesResponse'),
    newListSupportedPhoneNumberCountriesResponse,

    -- ** DeleteSipRule
    DeleteSipRule (DeleteSipRule'),
    newDeleteSipRule,
    DeleteSipRuleResponse (DeleteSipRuleResponse'),
    newDeleteSipRuleResponse,

    -- ** UpdateSipRule
    UpdateSipRule (UpdateSipRule'),
    newUpdateSipRule,
    UpdateSipRuleResponse (UpdateSipRuleResponse'),
    newUpdateSipRuleResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** DeleteVoiceConnectorOrigination
    DeleteVoiceConnectorOrigination (DeleteVoiceConnectorOrigination'),
    newDeleteVoiceConnectorOrigination,
    DeleteVoiceConnectorOriginationResponse (DeleteVoiceConnectorOriginationResponse'),
    newDeleteVoiceConnectorOriginationResponse,

    -- ** DeleteSipMediaApplication
    DeleteSipMediaApplication (DeleteSipMediaApplication'),
    newDeleteSipMediaApplication,
    DeleteSipMediaApplicationResponse (DeleteSipMediaApplicationResponse'),
    newDeleteSipMediaApplicationResponse,

    -- ** UpdateSipMediaApplication
    UpdateSipMediaApplication (UpdateSipMediaApplication'),
    newUpdateSipMediaApplication,
    UpdateSipMediaApplicationResponse (UpdateSipMediaApplicationResponse'),
    newUpdateSipMediaApplicationResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    DisassociatePhoneNumbersFromVoiceConnector (DisassociatePhoneNumbersFromVoiceConnector'),
    newDisassociatePhoneNumbersFromVoiceConnector,
    DisassociatePhoneNumbersFromVoiceConnectorResponse (DisassociatePhoneNumbersFromVoiceConnectorResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorResponse,

    -- ** GetMessagingSessionEndpoint
    GetMessagingSessionEndpoint (GetMessagingSessionEndpoint'),
    newGetMessagingSessionEndpoint,
    GetMessagingSessionEndpointResponse (GetMessagingSessionEndpointResponse'),
    newGetMessagingSessionEndpointResponse,

    -- ** PutVoiceConnectorOrigination
    PutVoiceConnectorOrigination (PutVoiceConnectorOrigination'),
    newPutVoiceConnectorOrigination,
    PutVoiceConnectorOriginationResponse (PutVoiceConnectorOriginationResponse'),
    newPutVoiceConnectorOriginationResponse,

    -- ** CreateAppInstanceUser
    CreateAppInstanceUser (CreateAppInstanceUser'),
    newCreateAppInstanceUser,
    CreateAppInstanceUserResponse (CreateAppInstanceUserResponse'),
    newCreateAppInstanceUserResponse,

    -- ** ListAttendeeTags
    ListAttendeeTags (ListAttendeeTags'),
    newListAttendeeTags,
    ListAttendeeTagsResponse (ListAttendeeTagsResponse'),
    newListAttendeeTagsResponse,

    -- ** ListChannelsModeratedByAppInstanceUser
    ListChannelsModeratedByAppInstanceUser (ListChannelsModeratedByAppInstanceUser'),
    newListChannelsModeratedByAppInstanceUser,
    ListChannelsModeratedByAppInstanceUserResponse (ListChannelsModeratedByAppInstanceUserResponse'),
    newListChannelsModeratedByAppInstanceUserResponse,

    -- ** RedactChannelMessage
    RedactChannelMessage (RedactChannelMessage'),
    newRedactChannelMessage,
    RedactChannelMessageResponse (RedactChannelMessageResponse'),
    newRedactChannelMessageResponse,

    -- ** PutRetentionSettings
    PutRetentionSettings (PutRetentionSettings'),
    newPutRetentionSettings,
    PutRetentionSettingsResponse (PutRetentionSettingsResponse'),
    newPutRetentionSettingsResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    DeleteVoiceConnectorStreamingConfiguration (DeleteVoiceConnectorStreamingConfiguration'),
    newDeleteVoiceConnectorStreamingConfiguration,
    DeleteVoiceConnectorStreamingConfigurationResponse (DeleteVoiceConnectorStreamingConfigurationResponse'),
    newDeleteVoiceConnectorStreamingConfigurationResponse,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    AssociatePhoneNumbersWithVoiceConnectorGroup (AssociatePhoneNumbersWithVoiceConnectorGroup'),
    newAssociatePhoneNumbersWithVoiceConnectorGroup,
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse (AssociatePhoneNumbersWithVoiceConnectorGroupResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorGroupResponse,

    -- ** PutAppInstanceRetentionSettings
    PutAppInstanceRetentionSettings (PutAppInstanceRetentionSettings'),
    newPutAppInstanceRetentionSettings,
    PutAppInstanceRetentionSettingsResponse (PutAppInstanceRetentionSettingsResponse'),
    newPutAppInstanceRetentionSettingsResponse,

    -- ** GetVoiceConnectorLoggingConfiguration
    GetVoiceConnectorLoggingConfiguration (GetVoiceConnectorLoggingConfiguration'),
    newGetVoiceConnectorLoggingConfiguration,
    GetVoiceConnectorLoggingConfigurationResponse (GetVoiceConnectorLoggingConfigurationResponse'),
    newGetVoiceConnectorLoggingConfigurationResponse,

    -- ** ListBots
    ListBots (ListBots'),
    newListBots,
    ListBotsResponse (ListBotsResponse'),
    newListBotsResponse,

    -- ** DeleteChannelMembership
    DeleteChannelMembership (DeleteChannelMembership'),
    newDeleteChannelMembership,
    DeleteChannelMembershipResponse (DeleteChannelMembershipResponse'),
    newDeleteChannelMembershipResponse,

    -- ** PutVoiceConnectorStreamingConfiguration
    PutVoiceConnectorStreamingConfiguration (PutVoiceConnectorStreamingConfiguration'),
    newPutVoiceConnectorStreamingConfiguration,
    PutVoiceConnectorStreamingConfigurationResponse (PutVoiceConnectorStreamingConfigurationResponse'),
    newPutVoiceConnectorStreamingConfigurationResponse,

    -- ** ListChannelMemberships
    ListChannelMemberships (ListChannelMemberships'),
    newListChannelMemberships,
    ListChannelMembershipsResponse (ListChannelMembershipsResponse'),
    newListChannelMembershipsResponse,

    -- ** GetGlobalSettings
    GetGlobalSettings (GetGlobalSettings'),
    newGetGlobalSettings,
    GetGlobalSettingsResponse (GetGlobalSettingsResponse'),
    newGetGlobalSettingsResponse,

    -- ** DeleteMeeting
    DeleteMeeting (DeleteMeeting'),
    newDeleteMeeting,
    DeleteMeetingResponse (DeleteMeetingResponse'),
    newDeleteMeetingResponse,

    -- ** ListMeetings
    ListMeetings (ListMeetings'),
    newListMeetings,
    ListMeetingsResponse (ListMeetingsResponse'),
    newListMeetingsResponse,

    -- ** GetAttendee
    GetAttendee (GetAttendee'),
    newGetAttendee,
    GetAttendeeResponse (GetAttendeeResponse'),
    newGetAttendeeResponse,

    -- ** DeleteAccount
    DeleteAccount (DeleteAccount'),
    newDeleteAccount,
    DeleteAccountResponse (DeleteAccountResponse'),
    newDeleteAccountResponse,

    -- ** UpdateAccount
    UpdateAccount (UpdateAccount'),
    newUpdateAccount,
    UpdateAccountResponse (UpdateAccountResponse'),
    newUpdateAccountResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** UpdateBot
    UpdateBot (UpdateBot'),
    newUpdateBot,
    UpdateBotResponse (UpdateBotResponse'),
    newUpdateBotResponse,

    -- ** ListPhoneNumberOrders
    ListPhoneNumberOrders (ListPhoneNumberOrders'),
    newListPhoneNumberOrders,
    ListPhoneNumberOrdersResponse (ListPhoneNumberOrdersResponse'),
    newListPhoneNumberOrdersResponse,

    -- ** SearchAvailablePhoneNumbers
    SearchAvailablePhoneNumbers (SearchAvailablePhoneNumbers'),
    newSearchAvailablePhoneNumbers,
    SearchAvailablePhoneNumbersResponse (SearchAvailablePhoneNumbersResponse'),
    newSearchAvailablePhoneNumbersResponse,

    -- ** CreateAppInstanceAdmin
    CreateAppInstanceAdmin (CreateAppInstanceAdmin'),
    newCreateAppInstanceAdmin,
    CreateAppInstanceAdminResponse (CreateAppInstanceAdminResponse'),
    newCreateAppInstanceAdminResponse,

    -- ** TagMeeting
    TagMeeting (TagMeeting'),
    newTagMeeting,
    TagMeetingResponse (TagMeetingResponse'),
    newTagMeetingResponse,

    -- ** ListVoiceConnectorGroups
    ListVoiceConnectorGroups (ListVoiceConnectorGroups'),
    newListVoiceConnectorGroups,
    ListVoiceConnectorGroupsResponse (ListVoiceConnectorGroupsResponse'),
    newListVoiceConnectorGroupsResponse,

    -- ** LogoutUser
    LogoutUser (LogoutUser'),
    newLogoutUser,
    LogoutUserResponse (LogoutUserResponse'),
    newLogoutUserResponse,

    -- ** ListVoiceConnectorTerminationCredentials
    ListVoiceConnectorTerminationCredentials (ListVoiceConnectorTerminationCredentials'),
    newListVoiceConnectorTerminationCredentials,
    ListVoiceConnectorTerminationCredentialsResponse (ListVoiceConnectorTerminationCredentialsResponse'),
    newListVoiceConnectorTerminationCredentialsResponse,

    -- ** CreateMediaCapturePipeline
    CreateMediaCapturePipeline (CreateMediaCapturePipeline'),
    newCreateMediaCapturePipeline,
    CreateMediaCapturePipelineResponse (CreateMediaCapturePipelineResponse'),
    newCreateMediaCapturePipelineResponse,

    -- ** CreateProxySession
    CreateProxySession (CreateProxySession'),
    newCreateProxySession,
    CreateProxySessionResponse (CreateProxySessionResponse'),
    newCreateProxySessionResponse,

    -- ** DeleteEventsConfiguration
    DeleteEventsConfiguration (DeleteEventsConfiguration'),
    newDeleteEventsConfiguration,
    DeleteEventsConfigurationResponse (DeleteEventsConfigurationResponse'),
    newDeleteEventsConfigurationResponse,

    -- ** PutEventsConfiguration
    PutEventsConfiguration (PutEventsConfiguration'),
    newPutEventsConfiguration,
    PutEventsConfigurationResponse (PutEventsConfigurationResponse'),
    newPutEventsConfigurationResponse,

    -- ** GetChannelMessage
    GetChannelMessage (GetChannelMessage'),
    newGetChannelMessage,
    GetChannelMessageResponse (GetChannelMessageResponse'),
    newGetChannelMessageResponse,

    -- ** UpdateRoom
    UpdateRoom (UpdateRoom'),
    newUpdateRoom,
    UpdateRoomResponse (UpdateRoomResponse'),
    newUpdateRoomResponse,

    -- ** DeleteRoom
    DeleteRoom (DeleteRoom'),
    newDeleteRoom,
    DeleteRoomResponse (DeleteRoomResponse'),
    newDeleteRoomResponse,

    -- ** PutSipMediaApplicationLoggingConfiguration
    PutSipMediaApplicationLoggingConfiguration (PutSipMediaApplicationLoggingConfiguration'),
    newPutSipMediaApplicationLoggingConfiguration,
    PutSipMediaApplicationLoggingConfigurationResponse (PutSipMediaApplicationLoggingConfigurationResponse'),
    newPutSipMediaApplicationLoggingConfigurationResponse,

    -- ** DescribeChannelMembershipForAppInstanceUser
    DescribeChannelMembershipForAppInstanceUser (DescribeChannelMembershipForAppInstanceUser'),
    newDescribeChannelMembershipForAppInstanceUser,
    DescribeChannelMembershipForAppInstanceUserResponse (DescribeChannelMembershipForAppInstanceUserResponse'),
    newDescribeChannelMembershipForAppInstanceUserResponse,

    -- ** ListAppInstanceAdmins
    ListAppInstanceAdmins (ListAppInstanceAdmins'),
    newListAppInstanceAdmins,
    ListAppInstanceAdminsResponse (ListAppInstanceAdminsResponse'),
    newListAppInstanceAdminsResponse,

    -- ** DeletePhoneNumber
    DeletePhoneNumber (DeletePhoneNumber'),
    newDeletePhoneNumber,
    DeletePhoneNumberResponse (DeletePhoneNumberResponse'),
    newDeletePhoneNumberResponse,

    -- ** UpdatePhoneNumber
    UpdatePhoneNumber (UpdatePhoneNumber'),
    newUpdatePhoneNumber,
    UpdatePhoneNumberResponse (UpdatePhoneNumberResponse'),
    newUpdatePhoneNumberResponse,

    -- ** ListPhoneNumbers
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** CreateChannelModerator
    CreateChannelModerator (CreateChannelModerator'),
    newCreateChannelModerator,
    CreateChannelModeratorResponse (CreateChannelModeratorResponse'),
    newCreateChannelModeratorResponse,

    -- ** GetAppInstanceStreamingConfigurations
    GetAppInstanceStreamingConfigurations (GetAppInstanceStreamingConfigurations'),
    newGetAppInstanceStreamingConfigurations,
    GetAppInstanceStreamingConfigurationsResponse (GetAppInstanceStreamingConfigurationsResponse'),
    newGetAppInstanceStreamingConfigurationsResponse,

    -- ** ListAppInstances
    ListAppInstances (ListAppInstances'),
    newListAppInstances,
    ListAppInstancesResponse (ListAppInstancesResponse'),
    newListAppInstancesResponse,

    -- ** DescribeChannelModeratedByAppInstanceUser
    DescribeChannelModeratedByAppInstanceUser (DescribeChannelModeratedByAppInstanceUser'),
    newDescribeChannelModeratedByAppInstanceUser,
    DescribeChannelModeratedByAppInstanceUserResponse (DescribeChannelModeratedByAppInstanceUserResponse'),
    newDescribeChannelModeratedByAppInstanceUserResponse,

    -- ** GetPhoneNumber
    GetPhoneNumber (GetPhoneNumber'),
    newGetPhoneNumber,
    GetPhoneNumberResponse (GetPhoneNumberResponse'),
    newGetPhoneNumberResponse,

    -- ** GetEventsConfiguration
    GetEventsConfiguration (GetEventsConfiguration'),
    newGetEventsConfiguration,
    GetEventsConfigurationResponse (GetEventsConfigurationResponse'),
    newGetEventsConfigurationResponse,

    -- ** GetSipMediaApplicationLoggingConfiguration
    GetSipMediaApplicationLoggingConfiguration (GetSipMediaApplicationLoggingConfiguration'),
    newGetSipMediaApplicationLoggingConfiguration,
    GetSipMediaApplicationLoggingConfigurationResponse (GetSipMediaApplicationLoggingConfigurationResponse'),
    newGetSipMediaApplicationLoggingConfigurationResponse,

    -- ** BatchUpdateUser
    BatchUpdateUser (BatchUpdateUser'),
    newBatchUpdateUser,
    BatchUpdateUserResponse (BatchUpdateUserResponse'),
    newBatchUpdateUserResponse,

    -- ** SendChannelMessage
    SendChannelMessage (SendChannelMessage'),
    newSendChannelMessage,
    SendChannelMessageResponse (SendChannelMessageResponse'),
    newSendChannelMessageResponse,

    -- ** TagAttendee
    TagAttendee (TagAttendee'),
    newTagAttendee,
    TagAttendeeResponse (TagAttendeeResponse'),
    newTagAttendeeResponse,

    -- ** UpdateVoiceConnector
    UpdateVoiceConnector (UpdateVoiceConnector'),
    newUpdateVoiceConnector,
    UpdateVoiceConnectorResponse (UpdateVoiceConnectorResponse'),
    newUpdateVoiceConnectorResponse,

    -- ** DeleteVoiceConnector
    DeleteVoiceConnector (DeleteVoiceConnector'),
    newDeleteVoiceConnector,
    DeleteVoiceConnectorResponse (DeleteVoiceConnectorResponse'),
    newDeleteVoiceConnectorResponse,

    -- ** GetMediaCapturePipeline
    GetMediaCapturePipeline (GetMediaCapturePipeline'),
    newGetMediaCapturePipeline,
    GetMediaCapturePipelineResponse (GetMediaCapturePipelineResponse'),
    newGetMediaCapturePipelineResponse,

    -- ** UpdateRoomMembership
    UpdateRoomMembership (UpdateRoomMembership'),
    newUpdateRoomMembership,
    UpdateRoomMembershipResponse (UpdateRoomMembershipResponse'),
    newUpdateRoomMembershipResponse,

    -- ** GetProxySession
    GetProxySession (GetProxySession'),
    newGetProxySession,
    GetProxySessionResponse (GetProxySessionResponse'),
    newGetProxySessionResponse,

    -- ** DeleteRoomMembership
    DeleteRoomMembership (DeleteRoomMembership'),
    newDeleteRoomMembership,
    DeleteRoomMembershipResponse (DeleteRoomMembershipResponse'),
    newDeleteRoomMembershipResponse,

    -- ** DescribeAppInstanceUser
    DescribeAppInstanceUser (DescribeAppInstanceUser'),
    newDescribeAppInstanceUser,
    DescribeAppInstanceUserResponse (DescribeAppInstanceUserResponse'),
    newDescribeAppInstanceUserResponse,

    -- ** BatchUnsuspendUser
    BatchUnsuspendUser (BatchUnsuspendUser'),
    newBatchUnsuspendUser,
    BatchUnsuspendUserResponse (BatchUnsuspendUserResponse'),
    newBatchUnsuspendUserResponse,

    -- ** DeleteChannelBan
    DeleteChannelBan (DeleteChannelBan'),
    newDeleteChannelBan,
    DeleteChannelBanResponse (DeleteChannelBanResponse'),
    newDeleteChannelBanResponse,

    -- ** GetMeeting
    GetMeeting (GetMeeting'),
    newGetMeeting,
    GetMeetingResponse (GetMeetingResponse'),
    newGetMeetingResponse,

    -- ** RestorePhoneNumber
    RestorePhoneNumber (RestorePhoneNumber'),
    newRestorePhoneNumber,
    RestorePhoneNumberResponse (RestorePhoneNumberResponse'),
    newRestorePhoneNumberResponse,

    -- ** GetRetentionSettings
    GetRetentionSettings (GetRetentionSettings'),
    newGetRetentionSettings,
    GetRetentionSettingsResponse (GetRetentionSettingsResponse'),
    newGetRetentionSettingsResponse,

    -- ** GetBot
    GetBot (GetBot'),
    newGetBot,
    GetBotResponse (GetBotResponse'),
    newGetBotResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** UntagAttendee
    UntagAttendee (UntagAttendee'),
    newUntagAttendee,
    UntagAttendeeResponse (UntagAttendeeResponse'),
    newUntagAttendeeResponse,

    -- ** StartMeetingTranscription
    StartMeetingTranscription (StartMeetingTranscription'),
    newStartMeetingTranscription,
    StartMeetingTranscriptionResponse (StartMeetingTranscriptionResponse'),
    newStartMeetingTranscriptionResponse,

    -- ** ListChannelBans
    ListChannelBans (ListChannelBans'),
    newListChannelBans,
    ListChannelBansResponse (ListChannelBansResponse'),
    newListChannelBansResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** BatchSuspendUser
    BatchSuspendUser (BatchSuspendUser'),
    newBatchSuspendUser,
    BatchSuspendUserResponse (BatchSuspendUserResponse'),
    newBatchSuspendUserResponse,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    GetAccountResponse (GetAccountResponse'),
    newGetAccountResponse,

    -- ** DescribeChannelModerator
    DescribeChannelModerator (DescribeChannelModerator'),
    newDescribeChannelModerator,
    DescribeChannelModeratorResponse (DescribeChannelModeratorResponse'),
    newDescribeChannelModeratorResponse,

    -- ** AssociatePhoneNumbersWithVoiceConnector
    AssociatePhoneNumbersWithVoiceConnector (AssociatePhoneNumbersWithVoiceConnector'),
    newAssociatePhoneNumbersWithVoiceConnector,
    AssociatePhoneNumbersWithVoiceConnectorResponse (AssociatePhoneNumbersWithVoiceConnectorResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorResponse,

    -- ** GetPhoneNumberOrder
    GetPhoneNumberOrder (GetPhoneNumberOrder'),
    newGetPhoneNumberOrder,
    GetPhoneNumberOrderResponse (GetPhoneNumberOrderResponse'),
    newGetPhoneNumberOrderResponse,

    -- ** GetSipRule
    GetSipRule (GetSipRule'),
    newGetSipRule,
    GetSipRuleResponse (GetSipRuleResponse'),
    newGetSipRuleResponse,

    -- ** GetUserSettings
    GetUserSettings (GetUserSettings'),
    newGetUserSettings,
    GetUserSettingsResponse (GetUserSettingsResponse'),
    newGetUserSettingsResponse,

    -- ** GetSipMediaApplication
    GetSipMediaApplication (GetSipMediaApplication'),
    newGetSipMediaApplication,
    GetSipMediaApplicationResponse (GetSipMediaApplicationResponse'),
    newGetSipMediaApplicationResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** CreateChannelBan
    CreateChannelBan (CreateChannelBan'),
    newCreateChannelBan,
    CreateChannelBanResponse (CreateChannelBanResponse'),
    newCreateChannelBanResponse,

    -- ** ListMeetingTags
    ListMeetingTags (ListMeetingTags'),
    newListMeetingTags,
    ListMeetingTagsResponse (ListMeetingTagsResponse'),
    newListMeetingTagsResponse,

    -- ** ListChannelMembershipsForAppInstanceUser
    ListChannelMembershipsForAppInstanceUser (ListChannelMembershipsForAppInstanceUser'),
    newListChannelMembershipsForAppInstanceUser,
    ListChannelMembershipsForAppInstanceUserResponse (ListChannelMembershipsForAppInstanceUserResponse'),
    newListChannelMembershipsForAppInstanceUserResponse,

    -- ** GetVoiceConnectorOrigination
    GetVoiceConnectorOrigination (GetVoiceConnectorOrigination'),
    newGetVoiceConnectorOrigination,
    GetVoiceConnectorOriginationResponse (GetVoiceConnectorOriginationResponse'),
    newGetVoiceConnectorOriginationResponse,

    -- ** BatchUpdatePhoneNumber
    BatchUpdatePhoneNumber (BatchUpdatePhoneNumber'),
    newBatchUpdatePhoneNumber,
    BatchUpdatePhoneNumberResponse (BatchUpdatePhoneNumberResponse'),
    newBatchUpdatePhoneNumberResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    DisassociatePhoneNumbersFromVoiceConnectorGroup (DisassociatePhoneNumbersFromVoiceConnectorGroup'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroup,
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse (DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse,

    -- ** UpdateChannelReadMarker
    UpdateChannelReadMarker (UpdateChannelReadMarker'),
    newUpdateChannelReadMarker,
    UpdateChannelReadMarkerResponse (UpdateChannelReadMarkerResponse'),
    newUpdateChannelReadMarkerResponse,

    -- ** CreateSipMediaApplicationCall
    CreateSipMediaApplicationCall (CreateSipMediaApplicationCall'),
    newCreateSipMediaApplicationCall,
    CreateSipMediaApplicationCallResponse (CreateSipMediaApplicationCallResponse'),
    newCreateSipMediaApplicationCallResponse,

    -- ** BatchDeletePhoneNumber
    BatchDeletePhoneNumber (BatchDeletePhoneNumber'),
    newBatchDeletePhoneNumber,
    BatchDeletePhoneNumberResponse (BatchDeletePhoneNumberResponse'),
    newBatchDeletePhoneNumberResponse,

    -- ** ListSipMediaApplications
    ListSipMediaApplications (ListSipMediaApplications'),
    newListSipMediaApplications,
    ListSipMediaApplicationsResponse (ListSipMediaApplicationsResponse'),
    newListSipMediaApplicationsResponse,

    -- ** CreateMeeting
    CreateMeeting (CreateMeeting'),
    newCreateMeeting,
    CreateMeetingResponse (CreateMeetingResponse'),
    newCreateMeetingResponse,

    -- ** CreatePhoneNumberOrder
    CreatePhoneNumberOrder (CreatePhoneNumberOrder'),
    newCreatePhoneNumberOrder,
    CreatePhoneNumberOrderResponse (CreatePhoneNumberOrderResponse'),
    newCreatePhoneNumberOrderResponse,

    -- ** ListSipRules
    ListSipRules (ListSipRules'),
    newListSipRules,
    ListSipRulesResponse (ListSipRulesResponse'),
    newListSipRulesResponse,

    -- ** CreateBot
    CreateBot (CreateBot'),
    newCreateBot,
    CreateBotResponse (CreateBotResponse'),
    newCreateBotResponse,

    -- ** UpdateUserSettings
    UpdateUserSettings (UpdateUserSettings'),
    newUpdateUserSettings,
    UpdateUserSettingsResponse (UpdateUserSettingsResponse'),
    newUpdateUserSettingsResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** BatchCreateRoomMembership
    BatchCreateRoomMembership (BatchCreateRoomMembership'),
    newBatchCreateRoomMembership,
    BatchCreateRoomMembershipResponse (BatchCreateRoomMembershipResponse'),
    newBatchCreateRoomMembershipResponse,

    -- ** DescribeAppInstance
    DescribeAppInstance (DescribeAppInstance'),
    newDescribeAppInstance,
    DescribeAppInstanceResponse (DescribeAppInstanceResponse'),
    newDescribeAppInstanceResponse,

    -- ** CreateAccount
    CreateAccount (CreateAccount'),
    newCreateAccount,
    CreateAccountResponse (CreateAccountResponse'),
    newCreateAccountResponse,

    -- ** CreateChannelMembership
    CreateChannelMembership (CreateChannelMembership'),
    newCreateChannelMembership,
    CreateChannelMembershipResponse (CreateChannelMembershipResponse'),
    newCreateChannelMembershipResponse,

    -- ** DeleteVoiceConnectorTermination
    DeleteVoiceConnectorTermination (DeleteVoiceConnectorTermination'),
    newDeleteVoiceConnectorTermination,
    DeleteVoiceConnectorTerminationResponse (DeleteVoiceConnectorTerminationResponse'),
    newDeleteVoiceConnectorTerminationResponse,

    -- ** AssociatePhoneNumberWithUser
    AssociatePhoneNumberWithUser (AssociatePhoneNumberWithUser'),
    newAssociatePhoneNumberWithUser,
    AssociatePhoneNumberWithUserResponse (AssociatePhoneNumberWithUserResponse'),
    newAssociatePhoneNumberWithUserResponse,

    -- ** DeleteVoiceConnectorProxy
    DeleteVoiceConnectorProxy (DeleteVoiceConnectorProxy'),
    newDeleteVoiceConnectorProxy,
    DeleteVoiceConnectorProxyResponse (DeleteVoiceConnectorProxyResponse'),
    newDeleteVoiceConnectorProxyResponse,

    -- ** CreateSipMediaApplication
    CreateSipMediaApplication (CreateSipMediaApplication'),
    newCreateSipMediaApplication,
    CreateSipMediaApplicationResponse (CreateSipMediaApplicationResponse'),
    newCreateSipMediaApplicationResponse,

    -- ** PutVoiceConnectorProxy
    PutVoiceConnectorProxy (PutVoiceConnectorProxy'),
    newPutVoiceConnectorProxy,
    PutVoiceConnectorProxyResponse (PutVoiceConnectorProxyResponse'),
    newPutVoiceConnectorProxyResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** PutVoiceConnectorTermination
    PutVoiceConnectorTermination (PutVoiceConnectorTermination'),
    newPutVoiceConnectorTermination,
    PutVoiceConnectorTerminationResponse (PutVoiceConnectorTerminationResponse'),
    newPutVoiceConnectorTerminationResponse,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    GetVoiceConnectorEmergencyCallingConfiguration (GetVoiceConnectorEmergencyCallingConfiguration'),
    newGetVoiceConnectorEmergencyCallingConfiguration,
    GetVoiceConnectorEmergencyCallingConfigurationResponse (GetVoiceConnectorEmergencyCallingConfigurationResponse'),
    newGetVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** PutVoiceConnectorTerminationCredentials
    PutVoiceConnectorTerminationCredentials (PutVoiceConnectorTerminationCredentials'),
    newPutVoiceConnectorTerminationCredentials,
    PutVoiceConnectorTerminationCredentialsResponse (PutVoiceConnectorTerminationCredentialsResponse'),
    newPutVoiceConnectorTerminationCredentialsResponse,

    -- ** ListAppInstanceUsers
    ListAppInstanceUsers (ListAppInstanceUsers'),
    newListAppInstanceUsers,
    ListAppInstanceUsersResponse (ListAppInstanceUsersResponse'),
    newListAppInstanceUsersResponse,

    -- ** AssociateSigninDelegateGroupsWithAccount
    AssociateSigninDelegateGroupsWithAccount (AssociateSigninDelegateGroupsWithAccount'),
    newAssociateSigninDelegateGroupsWithAccount,
    AssociateSigninDelegateGroupsWithAccountResponse (AssociateSigninDelegateGroupsWithAccountResponse'),
    newAssociateSigninDelegateGroupsWithAccountResponse,

    -- ** CreateSipRule
    CreateSipRule (CreateSipRule'),
    newCreateSipRule,
    CreateSipRuleResponse (CreateSipRuleResponse'),
    newCreateSipRuleResponse,

    -- ** DeleteVoiceConnectorTerminationCredentials
    DeleteVoiceConnectorTerminationCredentials (DeleteVoiceConnectorTerminationCredentials'),
    newDeleteVoiceConnectorTerminationCredentials,
    DeleteVoiceConnectorTerminationCredentialsResponse (DeleteVoiceConnectorTerminationCredentialsResponse'),
    newDeleteVoiceConnectorTerminationCredentialsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteAppInstanceUser
    DeleteAppInstanceUser (DeleteAppInstanceUser'),
    newDeleteAppInstanceUser,
    DeleteAppInstanceUserResponse (DeleteAppInstanceUserResponse'),
    newDeleteAppInstanceUserResponse,

    -- ** UpdateAppInstanceUser
    UpdateAppInstanceUser (UpdateAppInstanceUser'),
    newUpdateAppInstanceUser,
    UpdateAppInstanceUserResponse (UpdateAppInstanceUserResponse'),
    newUpdateAppInstanceUserResponse,

    -- ** UntagMeeting
    UntagMeeting (UntagMeeting'),
    newUntagMeeting,
    UntagMeetingResponse (UntagMeetingResponse'),
    newUntagMeetingResponse,

    -- ** UpdateVoiceConnectorGroup
    UpdateVoiceConnectorGroup (UpdateVoiceConnectorGroup'),
    newUpdateVoiceConnectorGroup,
    UpdateVoiceConnectorGroupResponse (UpdateVoiceConnectorGroupResponse'),
    newUpdateVoiceConnectorGroupResponse,

    -- ** RedactConversationMessage
    RedactConversationMessage (RedactConversationMessage'),
    newRedactConversationMessage,
    RedactConversationMessageResponse (RedactConversationMessageResponse'),
    newRedactConversationMessageResponse,

    -- ** DeleteChannelModerator
    DeleteChannelModerator (DeleteChannelModerator'),
    newDeleteChannelModerator,
    DeleteChannelModeratorResponse (DeleteChannelModeratorResponse'),
    newDeleteChannelModeratorResponse,

    -- ** DeleteVoiceConnectorGroup
    DeleteVoiceConnectorGroup (DeleteVoiceConnectorGroup'),
    newDeleteVoiceConnectorGroup,
    DeleteVoiceConnectorGroupResponse (DeleteVoiceConnectorGroupResponse'),
    newDeleteVoiceConnectorGroupResponse,

    -- ** DescribeChannelBan
    DescribeChannelBan (DescribeChannelBan'),
    newDescribeChannelBan,
    DescribeChannelBanResponse (DescribeChannelBanResponse'),
    newDescribeChannelBanResponse,

    -- ** DeleteMediaCapturePipeline
    DeleteMediaCapturePipeline (DeleteMediaCapturePipeline'),
    newDeleteMediaCapturePipeline,
    DeleteMediaCapturePipelineResponse (DeleteMediaCapturePipelineResponse'),
    newDeleteMediaCapturePipelineResponse,

    -- ** UpdateProxySession
    UpdateProxySession (UpdateProxySession'),
    newUpdateProxySession,
    UpdateProxySessionResponse (UpdateProxySessionResponse'),
    newUpdateProxySessionResponse,

    -- ** DeleteProxySession
    DeleteProxySession (DeleteProxySession'),
    newDeleteProxySession,
    DeleteProxySessionResponse (DeleteProxySessionResponse'),
    newDeleteProxySessionResponse,

    -- ** GetVoiceConnectorTerminationHealth
    GetVoiceConnectorTerminationHealth (GetVoiceConnectorTerminationHealth'),
    newGetVoiceConnectorTerminationHealth,
    GetVoiceConnectorTerminationHealthResponse (GetVoiceConnectorTerminationHealthResponse'),
    newGetVoiceConnectorTerminationHealthResponse,

    -- ** CreateMeetingDialOut
    CreateMeetingDialOut (CreateMeetingDialOut'),
    newCreateMeetingDialOut,
    CreateMeetingDialOutResponse (CreateMeetingDialOutResponse'),
    newCreateMeetingDialOutResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListProxySessions
    ListProxySessions (ListProxySessions'),
    newListProxySessions,
    ListProxySessionsResponse (ListProxySessionsResponse'),
    newListProxySessionsResponse,

    -- ** ListMediaCapturePipelines
    ListMediaCapturePipelines (ListMediaCapturePipelines'),
    newListMediaCapturePipelines,
    ListMediaCapturePipelinesResponse (ListMediaCapturePipelinesResponse'),
    newListMediaCapturePipelinesResponse,

    -- ** UpdatePhoneNumberSettings
    UpdatePhoneNumberSettings (UpdatePhoneNumberSettings'),
    newUpdatePhoneNumberSettings,
    UpdatePhoneNumberSettingsResponse (UpdatePhoneNumberSettingsResponse'),
    newUpdatePhoneNumberSettingsResponse,

    -- ** InviteUsers
    InviteUsers (InviteUsers'),
    newInviteUsers,
    InviteUsersResponse (InviteUsersResponse'),
    newInviteUsersResponse,

    -- ** CreateRoom
    CreateRoom (CreateRoom'),
    newCreateRoom,
    CreateRoomResponse (CreateRoomResponse'),
    newCreateRoomResponse,

    -- ** ListChannelModerators
    ListChannelModerators (ListChannelModerators'),
    newListChannelModerators,
    ListChannelModeratorsResponse (ListChannelModeratorsResponse'),
    newListChannelModeratorsResponse,

    -- ** GetVoiceConnector
    GetVoiceConnector (GetVoiceConnector'),
    newGetVoiceConnector,
    GetVoiceConnectorResponse (GetVoiceConnectorResponse'),
    newGetVoiceConnectorResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** CreateVoiceConnectorGroup
    CreateVoiceConnectorGroup (CreateVoiceConnectorGroup'),
    newCreateVoiceConnectorGroup,
    CreateVoiceConnectorGroupResponse (CreateVoiceConnectorGroupResponse'),
    newCreateVoiceConnectorGroupResponse,

    -- ** DeleteAppInstanceStreamingConfigurations
    DeleteAppInstanceStreamingConfigurations (DeleteAppInstanceStreamingConfigurations'),
    newDeleteAppInstanceStreamingConfigurations,
    DeleteAppInstanceStreamingConfigurationsResponse (DeleteAppInstanceStreamingConfigurationsResponse'),
    newDeleteAppInstanceStreamingConfigurationsResponse,

    -- ** ListRooms
    ListRooms (ListRooms'),
    newListRooms,
    ListRoomsResponse (ListRoomsResponse'),
    newListRoomsResponse,

    -- ** BatchCreateAttendee
    BatchCreateAttendee (BatchCreateAttendee'),
    newBatchCreateAttendee,
    BatchCreateAttendeeResponse (BatchCreateAttendeeResponse'),
    newBatchCreateAttendeeResponse,

    -- ** DeleteAppInstanceAdmin
    DeleteAppInstanceAdmin (DeleteAppInstanceAdmin'),
    newDeleteAppInstanceAdmin,
    DeleteAppInstanceAdminResponse (DeleteAppInstanceAdminResponse'),
    newDeleteAppInstanceAdminResponse,

    -- ** PutAppInstanceStreamingConfigurations
    PutAppInstanceStreamingConfigurations (PutAppInstanceStreamingConfigurations'),
    newPutAppInstanceStreamingConfigurations,
    PutAppInstanceStreamingConfigurationsResponse (PutAppInstanceStreamingConfigurationsResponse'),
    newPutAppInstanceStreamingConfigurationsResponse,

    -- ** RegenerateSecurityToken
    RegenerateSecurityToken (RegenerateSecurityToken'),
    newRegenerateSecurityToken,
    RegenerateSecurityTokenResponse (RegenerateSecurityTokenResponse'),
    newRegenerateSecurityTokenResponse,

    -- ** DeleteChannelMessage
    DeleteChannelMessage (DeleteChannelMessage'),
    newDeleteChannelMessage,
    DeleteChannelMessageResponse (DeleteChannelMessageResponse'),
    newDeleteChannelMessageResponse,

    -- ** UpdateChannelMessage
    UpdateChannelMessage (UpdateChannelMessage'),
    newUpdateChannelMessage,
    UpdateChannelMessageResponse (UpdateChannelMessageResponse'),
    newUpdateChannelMessageResponse,

    -- ** DeleteAppInstance
    DeleteAppInstance (DeleteAppInstance'),
    newDeleteAppInstance,
    DeleteAppInstanceResponse (DeleteAppInstanceResponse'),
    newDeleteAppInstanceResponse,

    -- ** UpdateAppInstance
    UpdateAppInstance (UpdateAppInstance'),
    newUpdateAppInstance,
    UpdateAppInstanceResponse (UpdateAppInstanceResponse'),
    newUpdateAppInstanceResponse,

    -- ** CreateVoiceConnector
    CreateVoiceConnector (CreateVoiceConnector'),
    newCreateVoiceConnector,
    CreateVoiceConnectorResponse (CreateVoiceConnectorResponse'),
    newCreateVoiceConnectorResponse,

    -- ** ListChannelMessages
    ListChannelMessages (ListChannelMessages'),
    newListChannelMessages,
    ListChannelMessagesResponse (ListChannelMessagesResponse'),
    newListChannelMessagesResponse,

    -- ** RedactRoomMessage
    RedactRoomMessage (RedactRoomMessage'),
    newRedactRoomMessage,
    RedactRoomMessageResponse (RedactRoomMessageResponse'),
    newRedactRoomMessageResponse,

    -- ** GetRoom
    GetRoom (GetRoom'),
    newGetRoom,
    GetRoomResponse (GetRoomResponse'),
    newGetRoomResponse,

    -- ** CreateRoomMembership
    CreateRoomMembership (CreateRoomMembership'),
    newCreateRoomMembership,
    CreateRoomMembershipResponse (CreateRoomMembershipResponse'),
    newCreateRoomMembershipResponse,

    -- ** BatchCreateChannelMembership
    BatchCreateChannelMembership (BatchCreateChannelMembership'),
    newBatchCreateChannelMembership,
    BatchCreateChannelMembershipResponse (BatchCreateChannelMembershipResponse'),
    newBatchCreateChannelMembershipResponse,

    -- * Types

    -- ** AccountStatus
    AccountStatus (..),

    -- ** AccountType
    AccountType (..),

    -- ** AppInstanceDataType
    AppInstanceDataType (..),

    -- ** ArtifactsState
    ArtifactsState (..),

    -- ** AudioMuxType
    AudioMuxType (..),

    -- ** BotType
    BotType (..),

    -- ** CallingNameStatus
    CallingNameStatus (..),

    -- ** Capability
    Capability (..),

    -- ** ChannelMembershipType
    ChannelMembershipType (..),

    -- ** ChannelMessagePersistenceType
    ChannelMessagePersistenceType (..),

    -- ** ChannelMessageType
    ChannelMessageType (..),

    -- ** ChannelMode
    ChannelMode (..),

    -- ** ChannelPrivacy
    ChannelPrivacy (..),

    -- ** ContentMuxType
    ContentMuxType (..),

    -- ** EmailStatus
    EmailStatus (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** GeoMatchLevel
    GeoMatchLevel (..),

    -- ** InviteStatus
    InviteStatus (..),

    -- ** License
    License (..),

    -- ** MediaPipelineSinkType
    MediaPipelineSinkType (..),

    -- ** MediaPipelineSourceType
    MediaPipelineSourceType (..),

    -- ** MediaPipelineStatus
    MediaPipelineStatus (..),

    -- ** MemberType
    MemberType (..),

    -- ** NotificationTarget
    NotificationTarget (..),

    -- ** NumberSelectionBehavior
    NumberSelectionBehavior (..),

    -- ** OrderedPhoneNumberStatus
    OrderedPhoneNumberStatus (..),

    -- ** OriginationRouteProtocol
    OriginationRouteProtocol (..),

    -- ** PhoneNumberAssociationName
    PhoneNumberAssociationName (..),

    -- ** PhoneNumberOrderStatus
    PhoneNumberOrderStatus (..),

    -- ** PhoneNumberProductType
    PhoneNumberProductType (..),

    -- ** PhoneNumberStatus
    PhoneNumberStatus (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** ProxySessionStatus
    ProxySessionStatus (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** RoomMembershipRole
    RoomMembershipRole (..),

    -- ** SipRuleTriggerType
    SipRuleTriggerType (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** TranscribeLanguageCode
    TranscribeLanguageCode (..),

    -- ** TranscribeMedicalLanguageCode
    TranscribeMedicalLanguageCode (..),

    -- ** TranscribeMedicalRegion
    TranscribeMedicalRegion (..),

    -- ** TranscribeMedicalSpecialty
    TranscribeMedicalSpecialty (..),

    -- ** TranscribeMedicalType
    TranscribeMedicalType (..),

    -- ** TranscribeRegion
    TranscribeRegion (..),

    -- ** TranscribeVocabularyFilterMethod
    TranscribeVocabularyFilterMethod (..),

    -- ** UserType
    UserType (..),

    -- ** VideoMuxType
    VideoMuxType (..),

    -- ** VoiceConnectorAwsRegion
    VoiceConnectorAwsRegion (..),

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** AccountSettings
    AccountSettings (AccountSettings'),
    newAccountSettings,

    -- ** AlexaForBusinessMetadata
    AlexaForBusinessMetadata (AlexaForBusinessMetadata'),
    newAlexaForBusinessMetadata,

    -- ** AppInstance
    AppInstance (AppInstance'),
    newAppInstance,

    -- ** AppInstanceAdmin
    AppInstanceAdmin (AppInstanceAdmin'),
    newAppInstanceAdmin,

    -- ** AppInstanceAdminSummary
    AppInstanceAdminSummary (AppInstanceAdminSummary'),
    newAppInstanceAdminSummary,

    -- ** AppInstanceRetentionSettings
    AppInstanceRetentionSettings (AppInstanceRetentionSettings'),
    newAppInstanceRetentionSettings,

    -- ** AppInstanceStreamingConfiguration
    AppInstanceStreamingConfiguration (AppInstanceStreamingConfiguration'),
    newAppInstanceStreamingConfiguration,

    -- ** AppInstanceSummary
    AppInstanceSummary (AppInstanceSummary'),
    newAppInstanceSummary,

    -- ** AppInstanceUser
    AppInstanceUser (AppInstanceUser'),
    newAppInstanceUser,

    -- ** AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (AppInstanceUserMembershipSummary'),
    newAppInstanceUserMembershipSummary,

    -- ** AppInstanceUserSummary
    AppInstanceUserSummary (AppInstanceUserSummary'),
    newAppInstanceUserSummary,

    -- ** ArtifactsConfiguration
    ArtifactsConfiguration (ArtifactsConfiguration'),
    newArtifactsConfiguration,

    -- ** Attendee
    Attendee (Attendee'),
    newAttendee,

    -- ** AudioArtifactsConfiguration
    AudioArtifactsConfiguration (AudioArtifactsConfiguration'),
    newAudioArtifactsConfiguration,

    -- ** BatchChannelMemberships
    BatchChannelMemberships (BatchChannelMemberships'),
    newBatchChannelMemberships,

    -- ** BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (BatchCreateChannelMembershipError'),
    newBatchCreateChannelMembershipError,

    -- ** Bot
    Bot (Bot'),
    newBot,

    -- ** BusinessCallingSettings
    BusinessCallingSettings (BusinessCallingSettings'),
    newBusinessCallingSettings,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelBan
    ChannelBan (ChannelBan'),
    newChannelBan,

    -- ** ChannelBanSummary
    ChannelBanSummary (ChannelBanSummary'),
    newChannelBanSummary,

    -- ** ChannelMembership
    ChannelMembership (ChannelMembership'),
    newChannelMembership,

    -- ** ChannelMembershipForAppInstanceUserSummary
    ChannelMembershipForAppInstanceUserSummary (ChannelMembershipForAppInstanceUserSummary'),
    newChannelMembershipForAppInstanceUserSummary,

    -- ** ChannelMembershipSummary
    ChannelMembershipSummary (ChannelMembershipSummary'),
    newChannelMembershipSummary,

    -- ** ChannelMessage
    ChannelMessage (ChannelMessage'),
    newChannelMessage,

    -- ** ChannelMessageSummary
    ChannelMessageSummary (ChannelMessageSummary'),
    newChannelMessageSummary,

    -- ** ChannelModeratedByAppInstanceUserSummary
    ChannelModeratedByAppInstanceUserSummary (ChannelModeratedByAppInstanceUserSummary'),
    newChannelModeratedByAppInstanceUserSummary,

    -- ** ChannelModerator
    ChannelModerator (ChannelModerator'),
    newChannelModerator,

    -- ** ChannelModeratorSummary
    ChannelModeratorSummary (ChannelModeratorSummary'),
    newChannelModeratorSummary,

    -- ** ChannelRetentionSettings
    ChannelRetentionSettings (ChannelRetentionSettings'),
    newChannelRetentionSettings,

    -- ** ChannelSummary
    ChannelSummary (ChannelSummary'),
    newChannelSummary,

    -- ** ChimeSdkMeetingConfiguration
    ChimeSdkMeetingConfiguration (ChimeSdkMeetingConfiguration'),
    newChimeSdkMeetingConfiguration,

    -- ** ContentArtifactsConfiguration
    ContentArtifactsConfiguration (ContentArtifactsConfiguration'),
    newContentArtifactsConfiguration,

    -- ** ConversationRetentionSettings
    ConversationRetentionSettings (ConversationRetentionSettings'),
    newConversationRetentionSettings,

    -- ** CreateAttendeeError
    CreateAttendeeError (CreateAttendeeError'),
    newCreateAttendeeError,

    -- ** CreateAttendeeRequestItem
    CreateAttendeeRequestItem (CreateAttendeeRequestItem'),
    newCreateAttendeeRequestItem,

    -- ** Credential
    Credential (Credential'),
    newCredential,

    -- ** DNISEmergencyCallingConfiguration
    DNISEmergencyCallingConfiguration (DNISEmergencyCallingConfiguration'),
    newDNISEmergencyCallingConfiguration,

    -- ** EmergencyCallingConfiguration
    EmergencyCallingConfiguration (EmergencyCallingConfiguration'),
    newEmergencyCallingConfiguration,

    -- ** EngineTranscribeMedicalSettings
    EngineTranscribeMedicalSettings (EngineTranscribeMedicalSettings'),
    newEngineTranscribeMedicalSettings,

    -- ** EngineTranscribeSettings
    EngineTranscribeSettings (EngineTranscribeSettings'),
    newEngineTranscribeSettings,

    -- ** EventsConfiguration
    EventsConfiguration (EventsConfiguration'),
    newEventsConfiguration,

    -- ** GeoMatchParams
    GeoMatchParams (GeoMatchParams'),
    newGeoMatchParams,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** Invite
    Invite (Invite'),
    newInvite,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** MediaCapturePipeline
    MediaCapturePipeline (MediaCapturePipeline'),
    newMediaCapturePipeline,

    -- ** MediaPlacement
    MediaPlacement (MediaPlacement'),
    newMediaPlacement,

    -- ** Meeting
    Meeting (Meeting'),
    newMeeting,

    -- ** MeetingNotificationConfiguration
    MeetingNotificationConfiguration (MeetingNotificationConfiguration'),
    newMeetingNotificationConfiguration,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** MemberError
    MemberError (MemberError'),
    newMemberError,

    -- ** MembershipItem
    MembershipItem (MembershipItem'),
    newMembershipItem,

    -- ** MessagingSessionEndpoint
    MessagingSessionEndpoint (MessagingSessionEndpoint'),
    newMessagingSessionEndpoint,

    -- ** OrderedPhoneNumber
    OrderedPhoneNumber (OrderedPhoneNumber'),
    newOrderedPhoneNumber,

    -- ** Origination
    Origination (Origination'),
    newOrigination,

    -- ** OriginationRoute
    OriginationRoute (OriginationRoute'),
    newOriginationRoute,

    -- ** Participant
    Participant (Participant'),
    newParticipant,

    -- ** PhoneNumber
    PhoneNumber (PhoneNumber'),
    newPhoneNumber,

    -- ** PhoneNumberAssociation
    PhoneNumberAssociation (PhoneNumberAssociation'),
    newPhoneNumberAssociation,

    -- ** PhoneNumberCapabilities
    PhoneNumberCapabilities (PhoneNumberCapabilities'),
    newPhoneNumberCapabilities,

    -- ** PhoneNumberCountry
    PhoneNumberCountry (PhoneNumberCountry'),
    newPhoneNumberCountry,

    -- ** PhoneNumberError
    PhoneNumberError (PhoneNumberError'),
    newPhoneNumberError,

    -- ** PhoneNumberOrder
    PhoneNumberOrder (PhoneNumberOrder'),
    newPhoneNumberOrder,

    -- ** Proxy
    Proxy (Proxy'),
    newProxy,

    -- ** ProxySession
    ProxySession (ProxySession'),
    newProxySession,

    -- ** RetentionSettings
    RetentionSettings (RetentionSettings'),
    newRetentionSettings,

    -- ** Room
    Room (Room'),
    newRoom,

    -- ** RoomMembership
    RoomMembership (RoomMembership'),
    newRoomMembership,

    -- ** RoomRetentionSettings
    RoomRetentionSettings (RoomRetentionSettings'),
    newRoomRetentionSettings,

    -- ** SelectedVideoStreams
    SelectedVideoStreams (SelectedVideoStreams'),
    newSelectedVideoStreams,

    -- ** SigninDelegateGroup
    SigninDelegateGroup (SigninDelegateGroup'),
    newSigninDelegateGroup,

    -- ** SipMediaApplication
    SipMediaApplication (SipMediaApplication'),
    newSipMediaApplication,

    -- ** SipMediaApplicationCall
    SipMediaApplicationCall (SipMediaApplicationCall'),
    newSipMediaApplicationCall,

    -- ** SipMediaApplicationEndpoint
    SipMediaApplicationEndpoint (SipMediaApplicationEndpoint'),
    newSipMediaApplicationEndpoint,

    -- ** SipMediaApplicationLoggingConfiguration
    SipMediaApplicationLoggingConfiguration (SipMediaApplicationLoggingConfiguration'),
    newSipMediaApplicationLoggingConfiguration,

    -- ** SipRule
    SipRule (SipRule'),
    newSipRule,

    -- ** SipRuleTargetApplication
    SipRuleTargetApplication (SipRuleTargetApplication'),
    newSipRuleTargetApplication,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,

    -- ** StreamingConfiguration
    StreamingConfiguration (StreamingConfiguration'),
    newStreamingConfiguration,

    -- ** StreamingNotificationTarget
    StreamingNotificationTarget (StreamingNotificationTarget'),
    newStreamingNotificationTarget,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TelephonySettings
    TelephonySettings (TelephonySettings'),
    newTelephonySettings,

    -- ** Termination
    Termination (Termination'),
    newTermination,

    -- ** TerminationHealth
    TerminationHealth (TerminationHealth'),
    newTerminationHealth,

    -- ** TranscriptionConfiguration
    TranscriptionConfiguration (TranscriptionConfiguration'),
    newTranscriptionConfiguration,

    -- ** UpdatePhoneNumberRequestItem
    UpdatePhoneNumberRequestItem (UpdatePhoneNumberRequestItem'),
    newUpdatePhoneNumberRequestItem,

    -- ** UpdateUserRequestItem
    UpdateUserRequestItem (UpdateUserRequestItem'),
    newUpdateUserRequestItem,

    -- ** User
    User (User'),
    newUser,

    -- ** UserError
    UserError (UserError'),
    newUserError,

    -- ** UserSettings
    UserSettings (UserSettings'),
    newUserSettings,

    -- ** VideoArtifactsConfiguration
    VideoArtifactsConfiguration (VideoArtifactsConfiguration'),
    newVideoArtifactsConfiguration,

    -- ** VoiceConnector
    VoiceConnector (VoiceConnector'),
    newVoiceConnector,

    -- ** VoiceConnectorGroup
    VoiceConnectorGroup (VoiceConnectorGroup'),
    newVoiceConnectorGroup,

    -- ** VoiceConnectorItem
    VoiceConnectorItem (VoiceConnectorItem'),
    newVoiceConnectorItem,

    -- ** VoiceConnectorSettings
    VoiceConnectorSettings (VoiceConnectorSettings'),
    newVoiceConnectorSettings,
  )
where

import Network.AWS.Chime.AssociatePhoneNumberWithUser
import Network.AWS.Chime.AssociatePhoneNumbersWithVoiceConnector
import Network.AWS.Chime.AssociatePhoneNumbersWithVoiceConnectorGroup
import Network.AWS.Chime.AssociateSigninDelegateGroupsWithAccount
import Network.AWS.Chime.BatchCreateAttendee
import Network.AWS.Chime.BatchCreateChannelMembership
import Network.AWS.Chime.BatchCreateRoomMembership
import Network.AWS.Chime.BatchDeletePhoneNumber
import Network.AWS.Chime.BatchSuspendUser
import Network.AWS.Chime.BatchUnsuspendUser
import Network.AWS.Chime.BatchUpdatePhoneNumber
import Network.AWS.Chime.BatchUpdateUser
import Network.AWS.Chime.CreateAccount
import Network.AWS.Chime.CreateAppInstance
import Network.AWS.Chime.CreateAppInstanceAdmin
import Network.AWS.Chime.CreateAppInstanceUser
import Network.AWS.Chime.CreateAttendee
import Network.AWS.Chime.CreateBot
import Network.AWS.Chime.CreateChannel
import Network.AWS.Chime.CreateChannelBan
import Network.AWS.Chime.CreateChannelMembership
import Network.AWS.Chime.CreateChannelModerator
import Network.AWS.Chime.CreateMediaCapturePipeline
import Network.AWS.Chime.CreateMeeting
import Network.AWS.Chime.CreateMeetingDialOut
import Network.AWS.Chime.CreateMeetingWithAttendees
import Network.AWS.Chime.CreatePhoneNumberOrder
import Network.AWS.Chime.CreateProxySession
import Network.AWS.Chime.CreateRoom
import Network.AWS.Chime.CreateRoomMembership
import Network.AWS.Chime.CreateSipMediaApplication
import Network.AWS.Chime.CreateSipMediaApplicationCall
import Network.AWS.Chime.CreateSipRule
import Network.AWS.Chime.CreateUser
import Network.AWS.Chime.CreateVoiceConnector
import Network.AWS.Chime.CreateVoiceConnectorGroup
import Network.AWS.Chime.DeleteAccount
import Network.AWS.Chime.DeleteAppInstance
import Network.AWS.Chime.DeleteAppInstanceAdmin
import Network.AWS.Chime.DeleteAppInstanceStreamingConfigurations
import Network.AWS.Chime.DeleteAppInstanceUser
import Network.AWS.Chime.DeleteAttendee
import Network.AWS.Chime.DeleteChannel
import Network.AWS.Chime.DeleteChannelBan
import Network.AWS.Chime.DeleteChannelMembership
import Network.AWS.Chime.DeleteChannelMessage
import Network.AWS.Chime.DeleteChannelModerator
import Network.AWS.Chime.DeleteEventsConfiguration
import Network.AWS.Chime.DeleteMediaCapturePipeline
import Network.AWS.Chime.DeleteMeeting
import Network.AWS.Chime.DeletePhoneNumber
import Network.AWS.Chime.DeleteProxySession
import Network.AWS.Chime.DeleteRoom
import Network.AWS.Chime.DeleteRoomMembership
import Network.AWS.Chime.DeleteSipMediaApplication
import Network.AWS.Chime.DeleteSipRule
import Network.AWS.Chime.DeleteVoiceConnector
import Network.AWS.Chime.DeleteVoiceConnectorEmergencyCallingConfiguration
import Network.AWS.Chime.DeleteVoiceConnectorGroup
import Network.AWS.Chime.DeleteVoiceConnectorOrigination
import Network.AWS.Chime.DeleteVoiceConnectorProxy
import Network.AWS.Chime.DeleteVoiceConnectorStreamingConfiguration
import Network.AWS.Chime.DeleteVoiceConnectorTermination
import Network.AWS.Chime.DeleteVoiceConnectorTerminationCredentials
import Network.AWS.Chime.DescribeAppInstance
import Network.AWS.Chime.DescribeAppInstanceAdmin
import Network.AWS.Chime.DescribeAppInstanceUser
import Network.AWS.Chime.DescribeChannel
import Network.AWS.Chime.DescribeChannelBan
import Network.AWS.Chime.DescribeChannelMembership
import Network.AWS.Chime.DescribeChannelMembershipForAppInstanceUser
import Network.AWS.Chime.DescribeChannelModeratedByAppInstanceUser
import Network.AWS.Chime.DescribeChannelModerator
import Network.AWS.Chime.DisassociatePhoneNumberFromUser
import Network.AWS.Chime.DisassociatePhoneNumbersFromVoiceConnector
import Network.AWS.Chime.DisassociatePhoneNumbersFromVoiceConnectorGroup
import Network.AWS.Chime.DisassociateSigninDelegateGroupsFromAccount
import Network.AWS.Chime.GetAccount
import Network.AWS.Chime.GetAccountSettings
import Network.AWS.Chime.GetAppInstanceRetentionSettings
import Network.AWS.Chime.GetAppInstanceStreamingConfigurations
import Network.AWS.Chime.GetAttendee
import Network.AWS.Chime.GetBot
import Network.AWS.Chime.GetChannelMessage
import Network.AWS.Chime.GetEventsConfiguration
import Network.AWS.Chime.GetGlobalSettings
import Network.AWS.Chime.GetMediaCapturePipeline
import Network.AWS.Chime.GetMeeting
import Network.AWS.Chime.GetMessagingSessionEndpoint
import Network.AWS.Chime.GetPhoneNumber
import Network.AWS.Chime.GetPhoneNumberOrder
import Network.AWS.Chime.GetPhoneNumberSettings
import Network.AWS.Chime.GetProxySession
import Network.AWS.Chime.GetRetentionSettings
import Network.AWS.Chime.GetRoom
import Network.AWS.Chime.GetSipMediaApplication
import Network.AWS.Chime.GetSipMediaApplicationLoggingConfiguration
import Network.AWS.Chime.GetSipRule
import Network.AWS.Chime.GetUser
import Network.AWS.Chime.GetUserSettings
import Network.AWS.Chime.GetVoiceConnector
import Network.AWS.Chime.GetVoiceConnectorEmergencyCallingConfiguration
import Network.AWS.Chime.GetVoiceConnectorGroup
import Network.AWS.Chime.GetVoiceConnectorLoggingConfiguration
import Network.AWS.Chime.GetVoiceConnectorOrigination
import Network.AWS.Chime.GetVoiceConnectorProxy
import Network.AWS.Chime.GetVoiceConnectorStreamingConfiguration
import Network.AWS.Chime.GetVoiceConnectorTermination
import Network.AWS.Chime.GetVoiceConnectorTerminationHealth
import Network.AWS.Chime.InviteUsers
import Network.AWS.Chime.Lens
import Network.AWS.Chime.ListAccounts
import Network.AWS.Chime.ListAppInstanceAdmins
import Network.AWS.Chime.ListAppInstanceUsers
import Network.AWS.Chime.ListAppInstances
import Network.AWS.Chime.ListAttendeeTags
import Network.AWS.Chime.ListAttendees
import Network.AWS.Chime.ListBots
import Network.AWS.Chime.ListChannelBans
import Network.AWS.Chime.ListChannelMemberships
import Network.AWS.Chime.ListChannelMembershipsForAppInstanceUser
import Network.AWS.Chime.ListChannelMessages
import Network.AWS.Chime.ListChannelModerators
import Network.AWS.Chime.ListChannels
import Network.AWS.Chime.ListChannelsModeratedByAppInstanceUser
import Network.AWS.Chime.ListMediaCapturePipelines
import Network.AWS.Chime.ListMeetingTags
import Network.AWS.Chime.ListMeetings
import Network.AWS.Chime.ListPhoneNumberOrders
import Network.AWS.Chime.ListPhoneNumbers
import Network.AWS.Chime.ListProxySessions
import Network.AWS.Chime.ListRoomMemberships
import Network.AWS.Chime.ListRooms
import Network.AWS.Chime.ListSipMediaApplications
import Network.AWS.Chime.ListSipRules
import Network.AWS.Chime.ListSupportedPhoneNumberCountries
import Network.AWS.Chime.ListTagsForResource
import Network.AWS.Chime.ListUsers
import Network.AWS.Chime.ListVoiceConnectorGroups
import Network.AWS.Chime.ListVoiceConnectorTerminationCredentials
import Network.AWS.Chime.ListVoiceConnectors
import Network.AWS.Chime.LogoutUser
import Network.AWS.Chime.PutAppInstanceRetentionSettings
import Network.AWS.Chime.PutAppInstanceStreamingConfigurations
import Network.AWS.Chime.PutEventsConfiguration
import Network.AWS.Chime.PutRetentionSettings
import Network.AWS.Chime.PutSipMediaApplicationLoggingConfiguration
import Network.AWS.Chime.PutVoiceConnectorEmergencyCallingConfiguration
import Network.AWS.Chime.PutVoiceConnectorLoggingConfiguration
import Network.AWS.Chime.PutVoiceConnectorOrigination
import Network.AWS.Chime.PutVoiceConnectorProxy
import Network.AWS.Chime.PutVoiceConnectorStreamingConfiguration
import Network.AWS.Chime.PutVoiceConnectorTermination
import Network.AWS.Chime.PutVoiceConnectorTerminationCredentials
import Network.AWS.Chime.RedactChannelMessage
import Network.AWS.Chime.RedactConversationMessage
import Network.AWS.Chime.RedactRoomMessage
import Network.AWS.Chime.RegenerateSecurityToken
import Network.AWS.Chime.ResetPersonalPIN
import Network.AWS.Chime.RestorePhoneNumber
import Network.AWS.Chime.SearchAvailablePhoneNumbers
import Network.AWS.Chime.SendChannelMessage
import Network.AWS.Chime.StartMeetingTranscription
import Network.AWS.Chime.StopMeetingTranscription
import Network.AWS.Chime.TagAttendee
import Network.AWS.Chime.TagMeeting
import Network.AWS.Chime.TagResource
import Network.AWS.Chime.Types
import Network.AWS.Chime.UntagAttendee
import Network.AWS.Chime.UntagMeeting
import Network.AWS.Chime.UntagResource
import Network.AWS.Chime.UpdateAccount
import Network.AWS.Chime.UpdateAccountSettings
import Network.AWS.Chime.UpdateAppInstance
import Network.AWS.Chime.UpdateAppInstanceUser
import Network.AWS.Chime.UpdateBot
import Network.AWS.Chime.UpdateChannel
import Network.AWS.Chime.UpdateChannelMessage
import Network.AWS.Chime.UpdateChannelReadMarker
import Network.AWS.Chime.UpdateGlobalSettings
import Network.AWS.Chime.UpdatePhoneNumber
import Network.AWS.Chime.UpdatePhoneNumberSettings
import Network.AWS.Chime.UpdateProxySession
import Network.AWS.Chime.UpdateRoom
import Network.AWS.Chime.UpdateRoomMembership
import Network.AWS.Chime.UpdateSipMediaApplication
import Network.AWS.Chime.UpdateSipMediaApplicationCall
import Network.AWS.Chime.UpdateSipRule
import Network.AWS.Chime.UpdateUser
import Network.AWS.Chime.UpdateUserSettings
import Network.AWS.Chime.UpdateVoiceConnector
import Network.AWS.Chime.UpdateVoiceConnectorGroup
import Network.AWS.Chime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Chime'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

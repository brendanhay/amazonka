{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Chime
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- types, inputs and outputs, and error codes. It also includes API actions
-- for use with the Amazon Chime SDK, which developers use to build their
-- own communication applications. For more information about the Amazon
-- Chime SDK, see
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
module Amazonka.Chime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- ** UnprocessableEntityException
    _UnprocessableEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociatePhoneNumberWithUser
    AssociatePhoneNumberWithUser (AssociatePhoneNumberWithUser'),
    newAssociatePhoneNumberWithUser,
    AssociatePhoneNumberWithUserResponse (AssociatePhoneNumberWithUserResponse'),
    newAssociatePhoneNumberWithUserResponse,

    -- ** AssociatePhoneNumbersWithVoiceConnector
    AssociatePhoneNumbersWithVoiceConnector (AssociatePhoneNumbersWithVoiceConnector'),
    newAssociatePhoneNumbersWithVoiceConnector,
    AssociatePhoneNumbersWithVoiceConnectorResponse (AssociatePhoneNumbersWithVoiceConnectorResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorResponse,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    AssociatePhoneNumbersWithVoiceConnectorGroup (AssociatePhoneNumbersWithVoiceConnectorGroup'),
    newAssociatePhoneNumbersWithVoiceConnectorGroup,
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse (AssociatePhoneNumbersWithVoiceConnectorGroupResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorGroupResponse,

    -- ** AssociateSigninDelegateGroupsWithAccount
    AssociateSigninDelegateGroupsWithAccount (AssociateSigninDelegateGroupsWithAccount'),
    newAssociateSigninDelegateGroupsWithAccount,
    AssociateSigninDelegateGroupsWithAccountResponse (AssociateSigninDelegateGroupsWithAccountResponse'),
    newAssociateSigninDelegateGroupsWithAccountResponse,

    -- ** BatchCreateAttendee
    BatchCreateAttendee (BatchCreateAttendee'),
    newBatchCreateAttendee,
    BatchCreateAttendeeResponse (BatchCreateAttendeeResponse'),
    newBatchCreateAttendeeResponse,

    -- ** BatchCreateChannelMembership
    BatchCreateChannelMembership (BatchCreateChannelMembership'),
    newBatchCreateChannelMembership,
    BatchCreateChannelMembershipResponse (BatchCreateChannelMembershipResponse'),
    newBatchCreateChannelMembershipResponse,

    -- ** BatchCreateRoomMembership
    BatchCreateRoomMembership (BatchCreateRoomMembership'),
    newBatchCreateRoomMembership,
    BatchCreateRoomMembershipResponse (BatchCreateRoomMembershipResponse'),
    newBatchCreateRoomMembershipResponse,

    -- ** BatchDeletePhoneNumber
    BatchDeletePhoneNumber (BatchDeletePhoneNumber'),
    newBatchDeletePhoneNumber,
    BatchDeletePhoneNumberResponse (BatchDeletePhoneNumberResponse'),
    newBatchDeletePhoneNumberResponse,

    -- ** BatchSuspendUser
    BatchSuspendUser (BatchSuspendUser'),
    newBatchSuspendUser,
    BatchSuspendUserResponse (BatchSuspendUserResponse'),
    newBatchSuspendUserResponse,

    -- ** BatchUnsuspendUser
    BatchUnsuspendUser (BatchUnsuspendUser'),
    newBatchUnsuspendUser,
    BatchUnsuspendUserResponse (BatchUnsuspendUserResponse'),
    newBatchUnsuspendUserResponse,

    -- ** BatchUpdatePhoneNumber
    BatchUpdatePhoneNumber (BatchUpdatePhoneNumber'),
    newBatchUpdatePhoneNumber,
    BatchUpdatePhoneNumberResponse (BatchUpdatePhoneNumberResponse'),
    newBatchUpdatePhoneNumberResponse,

    -- ** BatchUpdateUser
    BatchUpdateUser (BatchUpdateUser'),
    newBatchUpdateUser,
    BatchUpdateUserResponse (BatchUpdateUserResponse'),
    newBatchUpdateUserResponse,

    -- ** CreateAccount
    CreateAccount (CreateAccount'),
    newCreateAccount,
    CreateAccountResponse (CreateAccountResponse'),
    newCreateAccountResponse,

    -- ** CreateAppInstance
    CreateAppInstance (CreateAppInstance'),
    newCreateAppInstance,
    CreateAppInstanceResponse (CreateAppInstanceResponse'),
    newCreateAppInstanceResponse,

    -- ** CreateAppInstanceAdmin
    CreateAppInstanceAdmin (CreateAppInstanceAdmin'),
    newCreateAppInstanceAdmin,
    CreateAppInstanceAdminResponse (CreateAppInstanceAdminResponse'),
    newCreateAppInstanceAdminResponse,

    -- ** CreateAppInstanceUser
    CreateAppInstanceUser (CreateAppInstanceUser'),
    newCreateAppInstanceUser,
    CreateAppInstanceUserResponse (CreateAppInstanceUserResponse'),
    newCreateAppInstanceUserResponse,

    -- ** CreateAttendee
    CreateAttendee (CreateAttendee'),
    newCreateAttendee,
    CreateAttendeeResponse (CreateAttendeeResponse'),
    newCreateAttendeeResponse,

    -- ** CreateBot
    CreateBot (CreateBot'),
    newCreateBot,
    CreateBotResponse (CreateBotResponse'),
    newCreateBotResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateChannelBan
    CreateChannelBan (CreateChannelBan'),
    newCreateChannelBan,
    CreateChannelBanResponse (CreateChannelBanResponse'),
    newCreateChannelBanResponse,

    -- ** CreateChannelMembership
    CreateChannelMembership (CreateChannelMembership'),
    newCreateChannelMembership,
    CreateChannelMembershipResponse (CreateChannelMembershipResponse'),
    newCreateChannelMembershipResponse,

    -- ** CreateChannelModerator
    CreateChannelModerator (CreateChannelModerator'),
    newCreateChannelModerator,
    CreateChannelModeratorResponse (CreateChannelModeratorResponse'),
    newCreateChannelModeratorResponse,

    -- ** CreateMediaCapturePipeline
    CreateMediaCapturePipeline (CreateMediaCapturePipeline'),
    newCreateMediaCapturePipeline,
    CreateMediaCapturePipelineResponse (CreateMediaCapturePipelineResponse'),
    newCreateMediaCapturePipelineResponse,

    -- ** CreateMeeting
    CreateMeeting (CreateMeeting'),
    newCreateMeeting,
    CreateMeetingResponse (CreateMeetingResponse'),
    newCreateMeetingResponse,

    -- ** CreateMeetingDialOut
    CreateMeetingDialOut (CreateMeetingDialOut'),
    newCreateMeetingDialOut,
    CreateMeetingDialOutResponse (CreateMeetingDialOutResponse'),
    newCreateMeetingDialOutResponse,

    -- ** CreateMeetingWithAttendees
    CreateMeetingWithAttendees (CreateMeetingWithAttendees'),
    newCreateMeetingWithAttendees,
    CreateMeetingWithAttendeesResponse (CreateMeetingWithAttendeesResponse'),
    newCreateMeetingWithAttendeesResponse,

    -- ** CreatePhoneNumberOrder
    CreatePhoneNumberOrder (CreatePhoneNumberOrder'),
    newCreatePhoneNumberOrder,
    CreatePhoneNumberOrderResponse (CreatePhoneNumberOrderResponse'),
    newCreatePhoneNumberOrderResponse,

    -- ** CreateProxySession
    CreateProxySession (CreateProxySession'),
    newCreateProxySession,
    CreateProxySessionResponse (CreateProxySessionResponse'),
    newCreateProxySessionResponse,

    -- ** CreateRoom
    CreateRoom (CreateRoom'),
    newCreateRoom,
    CreateRoomResponse (CreateRoomResponse'),
    newCreateRoomResponse,

    -- ** CreateRoomMembership
    CreateRoomMembership (CreateRoomMembership'),
    newCreateRoomMembership,
    CreateRoomMembershipResponse (CreateRoomMembershipResponse'),
    newCreateRoomMembershipResponse,

    -- ** CreateSipMediaApplication
    CreateSipMediaApplication (CreateSipMediaApplication'),
    newCreateSipMediaApplication,
    CreateSipMediaApplicationResponse (CreateSipMediaApplicationResponse'),
    newCreateSipMediaApplicationResponse,

    -- ** CreateSipMediaApplicationCall
    CreateSipMediaApplicationCall (CreateSipMediaApplicationCall'),
    newCreateSipMediaApplicationCall,
    CreateSipMediaApplicationCallResponse (CreateSipMediaApplicationCallResponse'),
    newCreateSipMediaApplicationCallResponse,

    -- ** CreateSipRule
    CreateSipRule (CreateSipRule'),
    newCreateSipRule,
    CreateSipRuleResponse (CreateSipRuleResponse'),
    newCreateSipRuleResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateVoiceConnector
    CreateVoiceConnector (CreateVoiceConnector'),
    newCreateVoiceConnector,
    CreateVoiceConnectorResponse (CreateVoiceConnectorResponse'),
    newCreateVoiceConnectorResponse,

    -- ** CreateVoiceConnectorGroup
    CreateVoiceConnectorGroup (CreateVoiceConnectorGroup'),
    newCreateVoiceConnectorGroup,
    CreateVoiceConnectorGroupResponse (CreateVoiceConnectorGroupResponse'),
    newCreateVoiceConnectorGroupResponse,

    -- ** DeleteAccount
    DeleteAccount (DeleteAccount'),
    newDeleteAccount,
    DeleteAccountResponse (DeleteAccountResponse'),
    newDeleteAccountResponse,

    -- ** DeleteAppInstance
    DeleteAppInstance (DeleteAppInstance'),
    newDeleteAppInstance,
    DeleteAppInstanceResponse (DeleteAppInstanceResponse'),
    newDeleteAppInstanceResponse,

    -- ** DeleteAppInstanceAdmin
    DeleteAppInstanceAdmin (DeleteAppInstanceAdmin'),
    newDeleteAppInstanceAdmin,
    DeleteAppInstanceAdminResponse (DeleteAppInstanceAdminResponse'),
    newDeleteAppInstanceAdminResponse,

    -- ** DeleteAppInstanceStreamingConfigurations
    DeleteAppInstanceStreamingConfigurations (DeleteAppInstanceStreamingConfigurations'),
    newDeleteAppInstanceStreamingConfigurations,
    DeleteAppInstanceStreamingConfigurationsResponse (DeleteAppInstanceStreamingConfigurationsResponse'),
    newDeleteAppInstanceStreamingConfigurationsResponse,

    -- ** DeleteAppInstanceUser
    DeleteAppInstanceUser (DeleteAppInstanceUser'),
    newDeleteAppInstanceUser,
    DeleteAppInstanceUserResponse (DeleteAppInstanceUserResponse'),
    newDeleteAppInstanceUserResponse,

    -- ** DeleteAttendee
    DeleteAttendee (DeleteAttendee'),
    newDeleteAttendee,
    DeleteAttendeeResponse (DeleteAttendeeResponse'),
    newDeleteAttendeeResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteChannelBan
    DeleteChannelBan (DeleteChannelBan'),
    newDeleteChannelBan,
    DeleteChannelBanResponse (DeleteChannelBanResponse'),
    newDeleteChannelBanResponse,

    -- ** DeleteChannelMembership
    DeleteChannelMembership (DeleteChannelMembership'),
    newDeleteChannelMembership,
    DeleteChannelMembershipResponse (DeleteChannelMembershipResponse'),
    newDeleteChannelMembershipResponse,

    -- ** DeleteChannelMessage
    DeleteChannelMessage (DeleteChannelMessage'),
    newDeleteChannelMessage,
    DeleteChannelMessageResponse (DeleteChannelMessageResponse'),
    newDeleteChannelMessageResponse,

    -- ** DeleteChannelModerator
    DeleteChannelModerator (DeleteChannelModerator'),
    newDeleteChannelModerator,
    DeleteChannelModeratorResponse (DeleteChannelModeratorResponse'),
    newDeleteChannelModeratorResponse,

    -- ** DeleteEventsConfiguration
    DeleteEventsConfiguration (DeleteEventsConfiguration'),
    newDeleteEventsConfiguration,
    DeleteEventsConfigurationResponse (DeleteEventsConfigurationResponse'),
    newDeleteEventsConfigurationResponse,

    -- ** DeleteMediaCapturePipeline
    DeleteMediaCapturePipeline (DeleteMediaCapturePipeline'),
    newDeleteMediaCapturePipeline,
    DeleteMediaCapturePipelineResponse (DeleteMediaCapturePipelineResponse'),
    newDeleteMediaCapturePipelineResponse,

    -- ** DeleteMeeting
    DeleteMeeting (DeleteMeeting'),
    newDeleteMeeting,
    DeleteMeetingResponse (DeleteMeetingResponse'),
    newDeleteMeetingResponse,

    -- ** DeletePhoneNumber
    DeletePhoneNumber (DeletePhoneNumber'),
    newDeletePhoneNumber,
    DeletePhoneNumberResponse (DeletePhoneNumberResponse'),
    newDeletePhoneNumberResponse,

    -- ** DeleteProxySession
    DeleteProxySession (DeleteProxySession'),
    newDeleteProxySession,
    DeleteProxySessionResponse (DeleteProxySessionResponse'),
    newDeleteProxySessionResponse,

    -- ** DeleteRoom
    DeleteRoom (DeleteRoom'),
    newDeleteRoom,
    DeleteRoomResponse (DeleteRoomResponse'),
    newDeleteRoomResponse,

    -- ** DeleteRoomMembership
    DeleteRoomMembership (DeleteRoomMembership'),
    newDeleteRoomMembership,
    DeleteRoomMembershipResponse (DeleteRoomMembershipResponse'),
    newDeleteRoomMembershipResponse,

    -- ** DeleteSipMediaApplication
    DeleteSipMediaApplication (DeleteSipMediaApplication'),
    newDeleteSipMediaApplication,
    DeleteSipMediaApplicationResponse (DeleteSipMediaApplicationResponse'),
    newDeleteSipMediaApplicationResponse,

    -- ** DeleteSipRule
    DeleteSipRule (DeleteSipRule'),
    newDeleteSipRule,
    DeleteSipRuleResponse (DeleteSipRuleResponse'),
    newDeleteSipRuleResponse,

    -- ** DeleteVoiceConnector
    DeleteVoiceConnector (DeleteVoiceConnector'),
    newDeleteVoiceConnector,
    DeleteVoiceConnectorResponse (DeleteVoiceConnectorResponse'),
    newDeleteVoiceConnectorResponse,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    DeleteVoiceConnectorEmergencyCallingConfiguration (DeleteVoiceConnectorEmergencyCallingConfiguration'),
    newDeleteVoiceConnectorEmergencyCallingConfiguration,
    DeleteVoiceConnectorEmergencyCallingConfigurationResponse (DeleteVoiceConnectorEmergencyCallingConfigurationResponse'),
    newDeleteVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** DeleteVoiceConnectorGroup
    DeleteVoiceConnectorGroup (DeleteVoiceConnectorGroup'),
    newDeleteVoiceConnectorGroup,
    DeleteVoiceConnectorGroupResponse (DeleteVoiceConnectorGroupResponse'),
    newDeleteVoiceConnectorGroupResponse,

    -- ** DeleteVoiceConnectorOrigination
    DeleteVoiceConnectorOrigination (DeleteVoiceConnectorOrigination'),
    newDeleteVoiceConnectorOrigination,
    DeleteVoiceConnectorOriginationResponse (DeleteVoiceConnectorOriginationResponse'),
    newDeleteVoiceConnectorOriginationResponse,

    -- ** DeleteVoiceConnectorProxy
    DeleteVoiceConnectorProxy (DeleteVoiceConnectorProxy'),
    newDeleteVoiceConnectorProxy,
    DeleteVoiceConnectorProxyResponse (DeleteVoiceConnectorProxyResponse'),
    newDeleteVoiceConnectorProxyResponse,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    DeleteVoiceConnectorStreamingConfiguration (DeleteVoiceConnectorStreamingConfiguration'),
    newDeleteVoiceConnectorStreamingConfiguration,
    DeleteVoiceConnectorStreamingConfigurationResponse (DeleteVoiceConnectorStreamingConfigurationResponse'),
    newDeleteVoiceConnectorStreamingConfigurationResponse,

    -- ** DeleteVoiceConnectorTermination
    DeleteVoiceConnectorTermination (DeleteVoiceConnectorTermination'),
    newDeleteVoiceConnectorTermination,
    DeleteVoiceConnectorTerminationResponse (DeleteVoiceConnectorTerminationResponse'),
    newDeleteVoiceConnectorTerminationResponse,

    -- ** DeleteVoiceConnectorTerminationCredentials
    DeleteVoiceConnectorTerminationCredentials (DeleteVoiceConnectorTerminationCredentials'),
    newDeleteVoiceConnectorTerminationCredentials,
    DeleteVoiceConnectorTerminationCredentialsResponse (DeleteVoiceConnectorTerminationCredentialsResponse'),
    newDeleteVoiceConnectorTerminationCredentialsResponse,

    -- ** DescribeAppInstance
    DescribeAppInstance (DescribeAppInstance'),
    newDescribeAppInstance,
    DescribeAppInstanceResponse (DescribeAppInstanceResponse'),
    newDescribeAppInstanceResponse,

    -- ** DescribeAppInstanceAdmin
    DescribeAppInstanceAdmin (DescribeAppInstanceAdmin'),
    newDescribeAppInstanceAdmin,
    DescribeAppInstanceAdminResponse (DescribeAppInstanceAdminResponse'),
    newDescribeAppInstanceAdminResponse,

    -- ** DescribeAppInstanceUser
    DescribeAppInstanceUser (DescribeAppInstanceUser'),
    newDescribeAppInstanceUser,
    DescribeAppInstanceUserResponse (DescribeAppInstanceUserResponse'),
    newDescribeAppInstanceUserResponse,

    -- ** DescribeChannel
    DescribeChannel (DescribeChannel'),
    newDescribeChannel,
    DescribeChannelResponse (DescribeChannelResponse'),
    newDescribeChannelResponse,

    -- ** DescribeChannelBan
    DescribeChannelBan (DescribeChannelBan'),
    newDescribeChannelBan,
    DescribeChannelBanResponse (DescribeChannelBanResponse'),
    newDescribeChannelBanResponse,

    -- ** DescribeChannelMembership
    DescribeChannelMembership (DescribeChannelMembership'),
    newDescribeChannelMembership,
    DescribeChannelMembershipResponse (DescribeChannelMembershipResponse'),
    newDescribeChannelMembershipResponse,

    -- ** DescribeChannelMembershipForAppInstanceUser
    DescribeChannelMembershipForAppInstanceUser (DescribeChannelMembershipForAppInstanceUser'),
    newDescribeChannelMembershipForAppInstanceUser,
    DescribeChannelMembershipForAppInstanceUserResponse (DescribeChannelMembershipForAppInstanceUserResponse'),
    newDescribeChannelMembershipForAppInstanceUserResponse,

    -- ** DescribeChannelModeratedByAppInstanceUser
    DescribeChannelModeratedByAppInstanceUser (DescribeChannelModeratedByAppInstanceUser'),
    newDescribeChannelModeratedByAppInstanceUser,
    DescribeChannelModeratedByAppInstanceUserResponse (DescribeChannelModeratedByAppInstanceUserResponse'),
    newDescribeChannelModeratedByAppInstanceUserResponse,

    -- ** DescribeChannelModerator
    DescribeChannelModerator (DescribeChannelModerator'),
    newDescribeChannelModerator,
    DescribeChannelModeratorResponse (DescribeChannelModeratorResponse'),
    newDescribeChannelModeratorResponse,

    -- ** DisassociatePhoneNumberFromUser
    DisassociatePhoneNumberFromUser (DisassociatePhoneNumberFromUser'),
    newDisassociatePhoneNumberFromUser,
    DisassociatePhoneNumberFromUserResponse (DisassociatePhoneNumberFromUserResponse'),
    newDisassociatePhoneNumberFromUserResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    DisassociatePhoneNumbersFromVoiceConnector (DisassociatePhoneNumbersFromVoiceConnector'),
    newDisassociatePhoneNumbersFromVoiceConnector,
    DisassociatePhoneNumbersFromVoiceConnectorResponse (DisassociatePhoneNumbersFromVoiceConnectorResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    DisassociatePhoneNumbersFromVoiceConnectorGroup (DisassociatePhoneNumbersFromVoiceConnectorGroup'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroup,
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse (DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse,

    -- ** DisassociateSigninDelegateGroupsFromAccount
    DisassociateSigninDelegateGroupsFromAccount (DisassociateSigninDelegateGroupsFromAccount'),
    newDisassociateSigninDelegateGroupsFromAccount,
    DisassociateSigninDelegateGroupsFromAccountResponse (DisassociateSigninDelegateGroupsFromAccountResponse'),
    newDisassociateSigninDelegateGroupsFromAccountResponse,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    GetAccountResponse (GetAccountResponse'),
    newGetAccountResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** GetAppInstanceRetentionSettings
    GetAppInstanceRetentionSettings (GetAppInstanceRetentionSettings'),
    newGetAppInstanceRetentionSettings,
    GetAppInstanceRetentionSettingsResponse (GetAppInstanceRetentionSettingsResponse'),
    newGetAppInstanceRetentionSettingsResponse,

    -- ** GetAppInstanceStreamingConfigurations
    GetAppInstanceStreamingConfigurations (GetAppInstanceStreamingConfigurations'),
    newGetAppInstanceStreamingConfigurations,
    GetAppInstanceStreamingConfigurationsResponse (GetAppInstanceStreamingConfigurationsResponse'),
    newGetAppInstanceStreamingConfigurationsResponse,

    -- ** GetAttendee
    GetAttendee (GetAttendee'),
    newGetAttendee,
    GetAttendeeResponse (GetAttendeeResponse'),
    newGetAttendeeResponse,

    -- ** GetBot
    GetBot (GetBot'),
    newGetBot,
    GetBotResponse (GetBotResponse'),
    newGetBotResponse,

    -- ** GetChannelMessage
    GetChannelMessage (GetChannelMessage'),
    newGetChannelMessage,
    GetChannelMessageResponse (GetChannelMessageResponse'),
    newGetChannelMessageResponse,

    -- ** GetEventsConfiguration
    GetEventsConfiguration (GetEventsConfiguration'),
    newGetEventsConfiguration,
    GetEventsConfigurationResponse (GetEventsConfigurationResponse'),
    newGetEventsConfigurationResponse,

    -- ** GetGlobalSettings
    GetGlobalSettings (GetGlobalSettings'),
    newGetGlobalSettings,
    GetGlobalSettingsResponse (GetGlobalSettingsResponse'),
    newGetGlobalSettingsResponse,

    -- ** GetMediaCapturePipeline
    GetMediaCapturePipeline (GetMediaCapturePipeline'),
    newGetMediaCapturePipeline,
    GetMediaCapturePipelineResponse (GetMediaCapturePipelineResponse'),
    newGetMediaCapturePipelineResponse,

    -- ** GetMeeting
    GetMeeting (GetMeeting'),
    newGetMeeting,
    GetMeetingResponse (GetMeetingResponse'),
    newGetMeetingResponse,

    -- ** GetMessagingSessionEndpoint
    GetMessagingSessionEndpoint (GetMessagingSessionEndpoint'),
    newGetMessagingSessionEndpoint,
    GetMessagingSessionEndpointResponse (GetMessagingSessionEndpointResponse'),
    newGetMessagingSessionEndpointResponse,

    -- ** GetPhoneNumber
    GetPhoneNumber (GetPhoneNumber'),
    newGetPhoneNumber,
    GetPhoneNumberResponse (GetPhoneNumberResponse'),
    newGetPhoneNumberResponse,

    -- ** GetPhoneNumberOrder
    GetPhoneNumberOrder (GetPhoneNumberOrder'),
    newGetPhoneNumberOrder,
    GetPhoneNumberOrderResponse (GetPhoneNumberOrderResponse'),
    newGetPhoneNumberOrderResponse,

    -- ** GetPhoneNumberSettings
    GetPhoneNumberSettings (GetPhoneNumberSettings'),
    newGetPhoneNumberSettings,
    GetPhoneNumberSettingsResponse (GetPhoneNumberSettingsResponse'),
    newGetPhoneNumberSettingsResponse,

    -- ** GetProxySession
    GetProxySession (GetProxySession'),
    newGetProxySession,
    GetProxySessionResponse (GetProxySessionResponse'),
    newGetProxySessionResponse,

    -- ** GetRetentionSettings
    GetRetentionSettings (GetRetentionSettings'),
    newGetRetentionSettings,
    GetRetentionSettingsResponse (GetRetentionSettingsResponse'),
    newGetRetentionSettingsResponse,

    -- ** GetRoom
    GetRoom (GetRoom'),
    newGetRoom,
    GetRoomResponse (GetRoomResponse'),
    newGetRoomResponse,

    -- ** GetSipMediaApplication
    GetSipMediaApplication (GetSipMediaApplication'),
    newGetSipMediaApplication,
    GetSipMediaApplicationResponse (GetSipMediaApplicationResponse'),
    newGetSipMediaApplicationResponse,

    -- ** GetSipMediaApplicationLoggingConfiguration
    GetSipMediaApplicationLoggingConfiguration (GetSipMediaApplicationLoggingConfiguration'),
    newGetSipMediaApplicationLoggingConfiguration,
    GetSipMediaApplicationLoggingConfigurationResponse (GetSipMediaApplicationLoggingConfigurationResponse'),
    newGetSipMediaApplicationLoggingConfigurationResponse,

    -- ** GetSipRule
    GetSipRule (GetSipRule'),
    newGetSipRule,
    GetSipRuleResponse (GetSipRuleResponse'),
    newGetSipRuleResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** GetUserSettings
    GetUserSettings (GetUserSettings'),
    newGetUserSettings,
    GetUserSettingsResponse (GetUserSettingsResponse'),
    newGetUserSettingsResponse,

    -- ** GetVoiceConnector
    GetVoiceConnector (GetVoiceConnector'),
    newGetVoiceConnector,
    GetVoiceConnectorResponse (GetVoiceConnectorResponse'),
    newGetVoiceConnectorResponse,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    GetVoiceConnectorEmergencyCallingConfiguration (GetVoiceConnectorEmergencyCallingConfiguration'),
    newGetVoiceConnectorEmergencyCallingConfiguration,
    GetVoiceConnectorEmergencyCallingConfigurationResponse (GetVoiceConnectorEmergencyCallingConfigurationResponse'),
    newGetVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** GetVoiceConnectorGroup
    GetVoiceConnectorGroup (GetVoiceConnectorGroup'),
    newGetVoiceConnectorGroup,
    GetVoiceConnectorGroupResponse (GetVoiceConnectorGroupResponse'),
    newGetVoiceConnectorGroupResponse,

    -- ** GetVoiceConnectorLoggingConfiguration
    GetVoiceConnectorLoggingConfiguration (GetVoiceConnectorLoggingConfiguration'),
    newGetVoiceConnectorLoggingConfiguration,
    GetVoiceConnectorLoggingConfigurationResponse (GetVoiceConnectorLoggingConfigurationResponse'),
    newGetVoiceConnectorLoggingConfigurationResponse,

    -- ** GetVoiceConnectorOrigination
    GetVoiceConnectorOrigination (GetVoiceConnectorOrigination'),
    newGetVoiceConnectorOrigination,
    GetVoiceConnectorOriginationResponse (GetVoiceConnectorOriginationResponse'),
    newGetVoiceConnectorOriginationResponse,

    -- ** GetVoiceConnectorProxy
    GetVoiceConnectorProxy (GetVoiceConnectorProxy'),
    newGetVoiceConnectorProxy,
    GetVoiceConnectorProxyResponse (GetVoiceConnectorProxyResponse'),
    newGetVoiceConnectorProxyResponse,

    -- ** GetVoiceConnectorStreamingConfiguration
    GetVoiceConnectorStreamingConfiguration (GetVoiceConnectorStreamingConfiguration'),
    newGetVoiceConnectorStreamingConfiguration,
    GetVoiceConnectorStreamingConfigurationResponse (GetVoiceConnectorStreamingConfigurationResponse'),
    newGetVoiceConnectorStreamingConfigurationResponse,

    -- ** GetVoiceConnectorTermination
    GetVoiceConnectorTermination (GetVoiceConnectorTermination'),
    newGetVoiceConnectorTermination,
    GetVoiceConnectorTerminationResponse (GetVoiceConnectorTerminationResponse'),
    newGetVoiceConnectorTerminationResponse,

    -- ** GetVoiceConnectorTerminationHealth
    GetVoiceConnectorTerminationHealth (GetVoiceConnectorTerminationHealth'),
    newGetVoiceConnectorTerminationHealth,
    GetVoiceConnectorTerminationHealthResponse (GetVoiceConnectorTerminationHealthResponse'),
    newGetVoiceConnectorTerminationHealthResponse,

    -- ** InviteUsers
    InviteUsers (InviteUsers'),
    newInviteUsers,
    InviteUsersResponse (InviteUsersResponse'),
    newInviteUsersResponse,

    -- ** ListAccounts (Paginated)
    ListAccounts (ListAccounts'),
    newListAccounts,
    ListAccountsResponse (ListAccountsResponse'),
    newListAccountsResponse,

    -- ** ListAppInstanceAdmins
    ListAppInstanceAdmins (ListAppInstanceAdmins'),
    newListAppInstanceAdmins,
    ListAppInstanceAdminsResponse (ListAppInstanceAdminsResponse'),
    newListAppInstanceAdminsResponse,

    -- ** ListAppInstanceUsers
    ListAppInstanceUsers (ListAppInstanceUsers'),
    newListAppInstanceUsers,
    ListAppInstanceUsersResponse (ListAppInstanceUsersResponse'),
    newListAppInstanceUsersResponse,

    -- ** ListAppInstances
    ListAppInstances (ListAppInstances'),
    newListAppInstances,
    ListAppInstancesResponse (ListAppInstancesResponse'),
    newListAppInstancesResponse,

    -- ** ListAttendeeTags
    ListAttendeeTags (ListAttendeeTags'),
    newListAttendeeTags,
    ListAttendeeTagsResponse (ListAttendeeTagsResponse'),
    newListAttendeeTagsResponse,

    -- ** ListAttendees
    ListAttendees (ListAttendees'),
    newListAttendees,
    ListAttendeesResponse (ListAttendeesResponse'),
    newListAttendeesResponse,

    -- ** ListBots
    ListBots (ListBots'),
    newListBots,
    ListBotsResponse (ListBotsResponse'),
    newListBotsResponse,

    -- ** ListChannelBans
    ListChannelBans (ListChannelBans'),
    newListChannelBans,
    ListChannelBansResponse (ListChannelBansResponse'),
    newListChannelBansResponse,

    -- ** ListChannelMemberships
    ListChannelMemberships (ListChannelMemberships'),
    newListChannelMemberships,
    ListChannelMembershipsResponse (ListChannelMembershipsResponse'),
    newListChannelMembershipsResponse,

    -- ** ListChannelMembershipsForAppInstanceUser
    ListChannelMembershipsForAppInstanceUser (ListChannelMembershipsForAppInstanceUser'),
    newListChannelMembershipsForAppInstanceUser,
    ListChannelMembershipsForAppInstanceUserResponse (ListChannelMembershipsForAppInstanceUserResponse'),
    newListChannelMembershipsForAppInstanceUserResponse,

    -- ** ListChannelMessages
    ListChannelMessages (ListChannelMessages'),
    newListChannelMessages,
    ListChannelMessagesResponse (ListChannelMessagesResponse'),
    newListChannelMessagesResponse,

    -- ** ListChannelModerators
    ListChannelModerators (ListChannelModerators'),
    newListChannelModerators,
    ListChannelModeratorsResponse (ListChannelModeratorsResponse'),
    newListChannelModeratorsResponse,

    -- ** ListChannels
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListChannelsModeratedByAppInstanceUser
    ListChannelsModeratedByAppInstanceUser (ListChannelsModeratedByAppInstanceUser'),
    newListChannelsModeratedByAppInstanceUser,
    ListChannelsModeratedByAppInstanceUserResponse (ListChannelsModeratedByAppInstanceUserResponse'),
    newListChannelsModeratedByAppInstanceUserResponse,

    -- ** ListMediaCapturePipelines
    ListMediaCapturePipelines (ListMediaCapturePipelines'),
    newListMediaCapturePipelines,
    ListMediaCapturePipelinesResponse (ListMediaCapturePipelinesResponse'),
    newListMediaCapturePipelinesResponse,

    -- ** ListMeetingTags
    ListMeetingTags (ListMeetingTags'),
    newListMeetingTags,
    ListMeetingTagsResponse (ListMeetingTagsResponse'),
    newListMeetingTagsResponse,

    -- ** ListMeetings
    ListMeetings (ListMeetings'),
    newListMeetings,
    ListMeetingsResponse (ListMeetingsResponse'),
    newListMeetingsResponse,

    -- ** ListPhoneNumberOrders
    ListPhoneNumberOrders (ListPhoneNumberOrders'),
    newListPhoneNumberOrders,
    ListPhoneNumberOrdersResponse (ListPhoneNumberOrdersResponse'),
    newListPhoneNumberOrdersResponse,

    -- ** ListPhoneNumbers
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** ListProxySessions
    ListProxySessions (ListProxySessions'),
    newListProxySessions,
    ListProxySessionsResponse (ListProxySessionsResponse'),
    newListProxySessionsResponse,

    -- ** ListRoomMemberships
    ListRoomMemberships (ListRoomMemberships'),
    newListRoomMemberships,
    ListRoomMembershipsResponse (ListRoomMembershipsResponse'),
    newListRoomMembershipsResponse,

    -- ** ListRooms
    ListRooms (ListRooms'),
    newListRooms,
    ListRoomsResponse (ListRoomsResponse'),
    newListRoomsResponse,

    -- ** ListSipMediaApplications
    ListSipMediaApplications (ListSipMediaApplications'),
    newListSipMediaApplications,
    ListSipMediaApplicationsResponse (ListSipMediaApplicationsResponse'),
    newListSipMediaApplicationsResponse,

    -- ** ListSipRules
    ListSipRules (ListSipRules'),
    newListSipRules,
    ListSipRulesResponse (ListSipRulesResponse'),
    newListSipRulesResponse,

    -- ** ListSupportedPhoneNumberCountries
    ListSupportedPhoneNumberCountries (ListSupportedPhoneNumberCountries'),
    newListSupportedPhoneNumberCountries,
    ListSupportedPhoneNumberCountriesResponse (ListSupportedPhoneNumberCountriesResponse'),
    newListSupportedPhoneNumberCountriesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** ListVoiceConnectorGroups
    ListVoiceConnectorGroups (ListVoiceConnectorGroups'),
    newListVoiceConnectorGroups,
    ListVoiceConnectorGroupsResponse (ListVoiceConnectorGroupsResponse'),
    newListVoiceConnectorGroupsResponse,

    -- ** ListVoiceConnectorTerminationCredentials
    ListVoiceConnectorTerminationCredentials (ListVoiceConnectorTerminationCredentials'),
    newListVoiceConnectorTerminationCredentials,
    ListVoiceConnectorTerminationCredentialsResponse (ListVoiceConnectorTerminationCredentialsResponse'),
    newListVoiceConnectorTerminationCredentialsResponse,

    -- ** ListVoiceConnectors
    ListVoiceConnectors (ListVoiceConnectors'),
    newListVoiceConnectors,
    ListVoiceConnectorsResponse (ListVoiceConnectorsResponse'),
    newListVoiceConnectorsResponse,

    -- ** LogoutUser
    LogoutUser (LogoutUser'),
    newLogoutUser,
    LogoutUserResponse (LogoutUserResponse'),
    newLogoutUserResponse,

    -- ** PutAppInstanceRetentionSettings
    PutAppInstanceRetentionSettings (PutAppInstanceRetentionSettings'),
    newPutAppInstanceRetentionSettings,
    PutAppInstanceRetentionSettingsResponse (PutAppInstanceRetentionSettingsResponse'),
    newPutAppInstanceRetentionSettingsResponse,

    -- ** PutAppInstanceStreamingConfigurations
    PutAppInstanceStreamingConfigurations (PutAppInstanceStreamingConfigurations'),
    newPutAppInstanceStreamingConfigurations,
    PutAppInstanceStreamingConfigurationsResponse (PutAppInstanceStreamingConfigurationsResponse'),
    newPutAppInstanceStreamingConfigurationsResponse,

    -- ** PutEventsConfiguration
    PutEventsConfiguration (PutEventsConfiguration'),
    newPutEventsConfiguration,
    PutEventsConfigurationResponse (PutEventsConfigurationResponse'),
    newPutEventsConfigurationResponse,

    -- ** PutRetentionSettings
    PutRetentionSettings (PutRetentionSettings'),
    newPutRetentionSettings,
    PutRetentionSettingsResponse (PutRetentionSettingsResponse'),
    newPutRetentionSettingsResponse,

    -- ** PutSipMediaApplicationLoggingConfiguration
    PutSipMediaApplicationLoggingConfiguration (PutSipMediaApplicationLoggingConfiguration'),
    newPutSipMediaApplicationLoggingConfiguration,
    PutSipMediaApplicationLoggingConfigurationResponse (PutSipMediaApplicationLoggingConfigurationResponse'),
    newPutSipMediaApplicationLoggingConfigurationResponse,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    PutVoiceConnectorEmergencyCallingConfiguration (PutVoiceConnectorEmergencyCallingConfiguration'),
    newPutVoiceConnectorEmergencyCallingConfiguration,
    PutVoiceConnectorEmergencyCallingConfigurationResponse (PutVoiceConnectorEmergencyCallingConfigurationResponse'),
    newPutVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** PutVoiceConnectorLoggingConfiguration
    PutVoiceConnectorLoggingConfiguration (PutVoiceConnectorLoggingConfiguration'),
    newPutVoiceConnectorLoggingConfiguration,
    PutVoiceConnectorLoggingConfigurationResponse (PutVoiceConnectorLoggingConfigurationResponse'),
    newPutVoiceConnectorLoggingConfigurationResponse,

    -- ** PutVoiceConnectorOrigination
    PutVoiceConnectorOrigination (PutVoiceConnectorOrigination'),
    newPutVoiceConnectorOrigination,
    PutVoiceConnectorOriginationResponse (PutVoiceConnectorOriginationResponse'),
    newPutVoiceConnectorOriginationResponse,

    -- ** PutVoiceConnectorProxy
    PutVoiceConnectorProxy (PutVoiceConnectorProxy'),
    newPutVoiceConnectorProxy,
    PutVoiceConnectorProxyResponse (PutVoiceConnectorProxyResponse'),
    newPutVoiceConnectorProxyResponse,

    -- ** PutVoiceConnectorStreamingConfiguration
    PutVoiceConnectorStreamingConfiguration (PutVoiceConnectorStreamingConfiguration'),
    newPutVoiceConnectorStreamingConfiguration,
    PutVoiceConnectorStreamingConfigurationResponse (PutVoiceConnectorStreamingConfigurationResponse'),
    newPutVoiceConnectorStreamingConfigurationResponse,

    -- ** PutVoiceConnectorTermination
    PutVoiceConnectorTermination (PutVoiceConnectorTermination'),
    newPutVoiceConnectorTermination,
    PutVoiceConnectorTerminationResponse (PutVoiceConnectorTerminationResponse'),
    newPutVoiceConnectorTerminationResponse,

    -- ** PutVoiceConnectorTerminationCredentials
    PutVoiceConnectorTerminationCredentials (PutVoiceConnectorTerminationCredentials'),
    newPutVoiceConnectorTerminationCredentials,
    PutVoiceConnectorTerminationCredentialsResponse (PutVoiceConnectorTerminationCredentialsResponse'),
    newPutVoiceConnectorTerminationCredentialsResponse,

    -- ** RedactChannelMessage
    RedactChannelMessage (RedactChannelMessage'),
    newRedactChannelMessage,
    RedactChannelMessageResponse (RedactChannelMessageResponse'),
    newRedactChannelMessageResponse,

    -- ** RedactConversationMessage
    RedactConversationMessage (RedactConversationMessage'),
    newRedactConversationMessage,
    RedactConversationMessageResponse (RedactConversationMessageResponse'),
    newRedactConversationMessageResponse,

    -- ** RedactRoomMessage
    RedactRoomMessage (RedactRoomMessage'),
    newRedactRoomMessage,
    RedactRoomMessageResponse (RedactRoomMessageResponse'),
    newRedactRoomMessageResponse,

    -- ** RegenerateSecurityToken
    RegenerateSecurityToken (RegenerateSecurityToken'),
    newRegenerateSecurityToken,
    RegenerateSecurityTokenResponse (RegenerateSecurityTokenResponse'),
    newRegenerateSecurityTokenResponse,

    -- ** ResetPersonalPIN
    ResetPersonalPIN (ResetPersonalPIN'),
    newResetPersonalPIN,
    ResetPersonalPINResponse (ResetPersonalPINResponse'),
    newResetPersonalPINResponse,

    -- ** RestorePhoneNumber
    RestorePhoneNumber (RestorePhoneNumber'),
    newRestorePhoneNumber,
    RestorePhoneNumberResponse (RestorePhoneNumberResponse'),
    newRestorePhoneNumberResponse,

    -- ** SearchAvailablePhoneNumbers
    SearchAvailablePhoneNumbers (SearchAvailablePhoneNumbers'),
    newSearchAvailablePhoneNumbers,
    SearchAvailablePhoneNumbersResponse (SearchAvailablePhoneNumbersResponse'),
    newSearchAvailablePhoneNumbersResponse,

    -- ** SendChannelMessage
    SendChannelMessage (SendChannelMessage'),
    newSendChannelMessage,
    SendChannelMessageResponse (SendChannelMessageResponse'),
    newSendChannelMessageResponse,

    -- ** StartMeetingTranscription
    StartMeetingTranscription (StartMeetingTranscription'),
    newStartMeetingTranscription,
    StartMeetingTranscriptionResponse (StartMeetingTranscriptionResponse'),
    newStartMeetingTranscriptionResponse,

    -- ** StopMeetingTranscription
    StopMeetingTranscription (StopMeetingTranscription'),
    newStopMeetingTranscription,
    StopMeetingTranscriptionResponse (StopMeetingTranscriptionResponse'),
    newStopMeetingTranscriptionResponse,

    -- ** TagAttendee
    TagAttendee (TagAttendee'),
    newTagAttendee,
    TagAttendeeResponse (TagAttendeeResponse'),
    newTagAttendeeResponse,

    -- ** TagMeeting
    TagMeeting (TagMeeting'),
    newTagMeeting,
    TagMeetingResponse (TagMeetingResponse'),
    newTagMeetingResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagAttendee
    UntagAttendee (UntagAttendee'),
    newUntagAttendee,
    UntagAttendeeResponse (UntagAttendeeResponse'),
    newUntagAttendeeResponse,

    -- ** UntagMeeting
    UntagMeeting (UntagMeeting'),
    newUntagMeeting,
    UntagMeetingResponse (UntagMeetingResponse'),
    newUntagMeetingResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccount
    UpdateAccount (UpdateAccount'),
    newUpdateAccount,
    UpdateAccountResponse (UpdateAccountResponse'),
    newUpdateAccountResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** UpdateAppInstance
    UpdateAppInstance (UpdateAppInstance'),
    newUpdateAppInstance,
    UpdateAppInstanceResponse (UpdateAppInstanceResponse'),
    newUpdateAppInstanceResponse,

    -- ** UpdateAppInstanceUser
    UpdateAppInstanceUser (UpdateAppInstanceUser'),
    newUpdateAppInstanceUser,
    UpdateAppInstanceUserResponse (UpdateAppInstanceUserResponse'),
    newUpdateAppInstanceUserResponse,

    -- ** UpdateBot
    UpdateBot (UpdateBot'),
    newUpdateBot,
    UpdateBotResponse (UpdateBotResponse'),
    newUpdateBotResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** UpdateChannelMessage
    UpdateChannelMessage (UpdateChannelMessage'),
    newUpdateChannelMessage,
    UpdateChannelMessageResponse (UpdateChannelMessageResponse'),
    newUpdateChannelMessageResponse,

    -- ** UpdateChannelReadMarker
    UpdateChannelReadMarker (UpdateChannelReadMarker'),
    newUpdateChannelReadMarker,
    UpdateChannelReadMarkerResponse (UpdateChannelReadMarkerResponse'),
    newUpdateChannelReadMarkerResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** UpdatePhoneNumber
    UpdatePhoneNumber (UpdatePhoneNumber'),
    newUpdatePhoneNumber,
    UpdatePhoneNumberResponse (UpdatePhoneNumberResponse'),
    newUpdatePhoneNumberResponse,

    -- ** UpdatePhoneNumberSettings
    UpdatePhoneNumberSettings (UpdatePhoneNumberSettings'),
    newUpdatePhoneNumberSettings,
    UpdatePhoneNumberSettingsResponse (UpdatePhoneNumberSettingsResponse'),
    newUpdatePhoneNumberSettingsResponse,

    -- ** UpdateProxySession
    UpdateProxySession (UpdateProxySession'),
    newUpdateProxySession,
    UpdateProxySessionResponse (UpdateProxySessionResponse'),
    newUpdateProxySessionResponse,

    -- ** UpdateRoom
    UpdateRoom (UpdateRoom'),
    newUpdateRoom,
    UpdateRoomResponse (UpdateRoomResponse'),
    newUpdateRoomResponse,

    -- ** UpdateRoomMembership
    UpdateRoomMembership (UpdateRoomMembership'),
    newUpdateRoomMembership,
    UpdateRoomMembershipResponse (UpdateRoomMembershipResponse'),
    newUpdateRoomMembershipResponse,

    -- ** UpdateSipMediaApplication
    UpdateSipMediaApplication (UpdateSipMediaApplication'),
    newUpdateSipMediaApplication,
    UpdateSipMediaApplicationResponse (UpdateSipMediaApplicationResponse'),
    newUpdateSipMediaApplicationResponse,

    -- ** UpdateSipMediaApplicationCall
    UpdateSipMediaApplicationCall (UpdateSipMediaApplicationCall'),
    newUpdateSipMediaApplicationCall,
    UpdateSipMediaApplicationCallResponse (UpdateSipMediaApplicationCallResponse'),
    newUpdateSipMediaApplicationCallResponse,

    -- ** UpdateSipRule
    UpdateSipRule (UpdateSipRule'),
    newUpdateSipRule,
    UpdateSipRuleResponse (UpdateSipRuleResponse'),
    newUpdateSipRuleResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- ** UpdateUserSettings
    UpdateUserSettings (UpdateUserSettings'),
    newUpdateUserSettings,
    UpdateUserSettingsResponse (UpdateUserSettingsResponse'),
    newUpdateUserSettingsResponse,

    -- ** UpdateVoiceConnector
    UpdateVoiceConnector (UpdateVoiceConnector'),
    newUpdateVoiceConnector,
    UpdateVoiceConnectorResponse (UpdateVoiceConnectorResponse'),
    newUpdateVoiceConnectorResponse,

    -- ** UpdateVoiceConnectorGroup
    UpdateVoiceConnectorGroup (UpdateVoiceConnectorGroup'),
    newUpdateVoiceConnectorGroup,
    UpdateVoiceConnectorGroupResponse (UpdateVoiceConnectorGroupResponse'),
    newUpdateVoiceConnectorGroupResponse,

    -- ** ValidateE911Address
    ValidateE911Address (ValidateE911Address'),
    newValidateE911Address,
    ValidateE911AddressResponse (ValidateE911AddressResponse'),
    newValidateE911AddressResponse,

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

    -- ** TranscribeContentIdentificationType
    TranscribeContentIdentificationType (..),

    -- ** TranscribeContentRedactionType
    TranscribeContentRedactionType (..),

    -- ** TranscribeLanguageCode
    TranscribeLanguageCode (..),

    -- ** TranscribeMedicalContentIdentificationType
    TranscribeMedicalContentIdentificationType (..),

    -- ** TranscribeMedicalLanguageCode
    TranscribeMedicalLanguageCode (..),

    -- ** TranscribeMedicalRegion
    TranscribeMedicalRegion (..),

    -- ** TranscribeMedicalSpecialty
    TranscribeMedicalSpecialty (..),

    -- ** TranscribeMedicalType
    TranscribeMedicalType (..),

    -- ** TranscribePartialResultsStability
    TranscribePartialResultsStability (..),

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

    -- ** Address
    Address (Address'),
    newAddress,

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

    -- ** CandidateAddress
    CandidateAddress (CandidateAddress'),
    newCandidateAddress,

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
import Amazonka.Chime.Lens
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
import Amazonka.Chime.Types
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
import Amazonka.Chime.Waiters

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

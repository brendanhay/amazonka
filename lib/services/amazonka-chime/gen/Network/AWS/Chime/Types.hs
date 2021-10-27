{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ThrottledClientException,
    _ResourceLimitExceededException,
    _UnprocessableEntityException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _ServiceFailureException,
    _UnauthorizedClientException,
    _ServiceUnavailableException,
    _BadRequestException,

    -- * AccountStatus
    AccountStatus (..),

    -- * AccountType
    AccountType (..),

    -- * AppInstanceDataType
    AppInstanceDataType (..),

    -- * ArtifactsState
    ArtifactsState (..),

    -- * AudioMuxType
    AudioMuxType (..),

    -- * BotType
    BotType (..),

    -- * CallingNameStatus
    CallingNameStatus (..),

    -- * Capability
    Capability (..),

    -- * ChannelMembershipType
    ChannelMembershipType (..),

    -- * ChannelMessagePersistenceType
    ChannelMessagePersistenceType (..),

    -- * ChannelMessageType
    ChannelMessageType (..),

    -- * ChannelMode
    ChannelMode (..),

    -- * ChannelPrivacy
    ChannelPrivacy (..),

    -- * ContentMuxType
    ContentMuxType (..),

    -- * EmailStatus
    EmailStatus (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * GeoMatchLevel
    GeoMatchLevel (..),

    -- * InviteStatus
    InviteStatus (..),

    -- * License
    License (..),

    -- * MediaPipelineSinkType
    MediaPipelineSinkType (..),

    -- * MediaPipelineSourceType
    MediaPipelineSourceType (..),

    -- * MediaPipelineStatus
    MediaPipelineStatus (..),

    -- * MemberType
    MemberType (..),

    -- * NotificationTarget
    NotificationTarget (..),

    -- * NumberSelectionBehavior
    NumberSelectionBehavior (..),

    -- * OrderedPhoneNumberStatus
    OrderedPhoneNumberStatus (..),

    -- * OriginationRouteProtocol
    OriginationRouteProtocol (..),

    -- * PhoneNumberAssociationName
    PhoneNumberAssociationName (..),

    -- * PhoneNumberOrderStatus
    PhoneNumberOrderStatus (..),

    -- * PhoneNumberProductType
    PhoneNumberProductType (..),

    -- * PhoneNumberStatus
    PhoneNumberStatus (..),

    -- * PhoneNumberType
    PhoneNumberType (..),

    -- * ProxySessionStatus
    ProxySessionStatus (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * RoomMembershipRole
    RoomMembershipRole (..),

    -- * SipRuleTriggerType
    SipRuleTriggerType (..),

    -- * SortOrder
    SortOrder (..),

    -- * TranscribeLanguageCode
    TranscribeLanguageCode (..),

    -- * TranscribeMedicalLanguageCode
    TranscribeMedicalLanguageCode (..),

    -- * TranscribeMedicalRegion
    TranscribeMedicalRegion (..),

    -- * TranscribeMedicalSpecialty
    TranscribeMedicalSpecialty (..),

    -- * TranscribeMedicalType
    TranscribeMedicalType (..),

    -- * TranscribeRegion
    TranscribeRegion (..),

    -- * TranscribeVocabularyFilterMethod
    TranscribeVocabularyFilterMethod (..),

    -- * UserType
    UserType (..),

    -- * VideoMuxType
    VideoMuxType (..),

    -- * VoiceConnectorAwsRegion
    VoiceConnectorAwsRegion (..),

    -- * Account
    Account (..),
    newAccount,
    account_signinDelegateGroups,
    account_accountStatus,
    account_defaultLicense,
    account_supportedLicenses,
    account_createdTimestamp,
    account_accountType,
    account_awsAccountId,
    account_accountId,
    account_name,

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_enableDialOut,
    accountSettings_disableRemoteControl,

    -- * AlexaForBusinessMetadata
    AlexaForBusinessMetadata (..),
    newAlexaForBusinessMetadata,
    alexaForBusinessMetadata_alexaForBusinessRoomArn,
    alexaForBusinessMetadata_isAlexaForBusinessEnabled,

    -- * AppInstance
    AppInstance (..),
    newAppInstance,
    appInstance_name,
    appInstance_metadata,
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,

    -- * AppInstanceAdmin
    AppInstanceAdmin (..),
    newAppInstanceAdmin,
    appInstanceAdmin_admin,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_createdTimestamp,

    -- * AppInstanceAdminSummary
    AppInstanceAdminSummary (..),
    newAppInstanceAdminSummary,
    appInstanceAdminSummary_admin,

    -- * AppInstanceRetentionSettings
    AppInstanceRetentionSettings (..),
    newAppInstanceRetentionSettings,
    appInstanceRetentionSettings_channelRetentionSettings,

    -- * AppInstanceStreamingConfiguration
    AppInstanceStreamingConfiguration (..),
    newAppInstanceStreamingConfiguration,
    appInstanceStreamingConfiguration_appInstanceDataType,
    appInstanceStreamingConfiguration_resourceArn,

    -- * AppInstanceSummary
    AppInstanceSummary (..),
    newAppInstanceSummary,
    appInstanceSummary_name,
    appInstanceSummary_metadata,
    appInstanceSummary_appInstanceArn,

    -- * AppInstanceUser
    AppInstanceUser (..),
    newAppInstanceUser,
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_name,
    appInstanceUser_metadata,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,

    -- * AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (..),
    newAppInstanceUserMembershipSummary,
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

    -- * AppInstanceUserSummary
    AppInstanceUserSummary (..),
    newAppInstanceUserSummary,
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_name,
    appInstanceUserSummary_metadata,

    -- * ArtifactsConfiguration
    ArtifactsConfiguration (..),
    newArtifactsConfiguration,
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- * Attendee
    Attendee (..),
    newAttendee,
    attendee_attendeeId,
    attendee_joinToken,
    attendee_externalUserId,

    -- * AudioArtifactsConfiguration
    AudioArtifactsConfiguration (..),
    newAudioArtifactsConfiguration,
    audioArtifactsConfiguration_muxType,

    -- * BatchChannelMemberships
    BatchChannelMemberships (..),
    newBatchChannelMemberships,
    batchChannelMemberships_members,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_type,
    batchChannelMemberships_invitedBy,

    -- * BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (..),
    newBatchCreateChannelMembershipError,
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_memberArn,
    batchCreateChannelMembershipError_errorMessage,

    -- * Bot
    Bot (..),
    newBot,
    bot_securityToken,
    bot_disabled,
    bot_updatedTimestamp,
    bot_userId,
    bot_botId,
    bot_displayName,
    bot_botEmail,
    bot_createdTimestamp,
    bot_botType,

    -- * BusinessCallingSettings
    BusinessCallingSettings (..),
    newBusinessCallingSettings,
    businessCallingSettings_cdrBucket,

    -- * Channel
    Channel (..),
    newChannel,
    channel_mode,
    channel_createdBy,
    channel_channelArn,
    channel_privacy,
    channel_lastMessageTimestamp,
    channel_name,
    channel_metadata,
    channel_createdTimestamp,
    channel_lastUpdatedTimestamp,

    -- * ChannelBan
    ChannelBan (..),
    newChannelBan,
    channelBan_createdBy,
    channelBan_channelArn,
    channelBan_member,
    channelBan_createdTimestamp,

    -- * ChannelBanSummary
    ChannelBanSummary (..),
    newChannelBanSummary,
    channelBanSummary_member,

    -- * ChannelMembership
    ChannelMembership (..),
    newChannelMembership,
    channelMembership_channelArn,
    channelMembership_member,
    channelMembership_type,
    channelMembership_invitedBy,
    channelMembership_createdTimestamp,
    channelMembership_lastUpdatedTimestamp,

    -- * ChannelMembershipForAppInstanceUserSummary
    ChannelMembershipForAppInstanceUserSummary (..),
    newChannelMembershipForAppInstanceUserSummary,
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,

    -- * ChannelMembershipSummary
    ChannelMembershipSummary (..),
    newChannelMembershipSummary,
    channelMembershipSummary_member,

    -- * ChannelMessage
    ChannelMessage (..),
    newChannelMessage,
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

    -- * ChannelMessageSummary
    ChannelMessageSummary (..),
    newChannelMessageSummary,
    channelMessageSummary_sender,
    channelMessageSummary_content,
    channelMessageSummary_redacted,
    channelMessageSummary_metadata,
    channelMessageSummary_type,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_messageId,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_lastEditedTimestamp,

    -- * ChannelModeratedByAppInstanceUserSummary
    ChannelModeratedByAppInstanceUserSummary (..),
    newChannelModeratedByAppInstanceUserSummary,
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- * ChannelModerator
    ChannelModerator (..),
    newChannelModerator,
    channelModerator_createdBy,
    channelModerator_channelArn,
    channelModerator_createdTimestamp,
    channelModerator_moderator,

    -- * ChannelModeratorSummary
    ChannelModeratorSummary (..),
    newChannelModeratorSummary,
    channelModeratorSummary_moderator,

    -- * ChannelRetentionSettings
    ChannelRetentionSettings (..),
    newChannelRetentionSettings,
    channelRetentionSettings_retentionDays,

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_mode,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,

    -- * ChimeSdkMeetingConfiguration
    ChimeSdkMeetingConfiguration (..),
    newChimeSdkMeetingConfiguration,
    chimeSdkMeetingConfiguration_artifactsConfiguration,
    chimeSdkMeetingConfiguration_sourceConfiguration,

    -- * ContentArtifactsConfiguration
    ContentArtifactsConfiguration (..),
    newContentArtifactsConfiguration,
    contentArtifactsConfiguration_muxType,
    contentArtifactsConfiguration_state,

    -- * ConversationRetentionSettings
    ConversationRetentionSettings (..),
    newConversationRetentionSettings,
    conversationRetentionSettings_retentionDays,

    -- * CreateAttendeeError
    CreateAttendeeError (..),
    newCreateAttendeeError,
    createAttendeeError_errorCode,
    createAttendeeError_errorMessage,
    createAttendeeError_externalUserId,

    -- * CreateAttendeeRequestItem
    CreateAttendeeRequestItem (..),
    newCreateAttendeeRequestItem,
    createAttendeeRequestItem_tags,
    createAttendeeRequestItem_externalUserId,

    -- * Credential
    Credential (..),
    newCredential,
    credential_username,
    credential_password,

    -- * DNISEmergencyCallingConfiguration
    DNISEmergencyCallingConfiguration (..),
    newDNISEmergencyCallingConfiguration,
    dNISEmergencyCallingConfiguration_testPhoneNumber,
    dNISEmergencyCallingConfiguration_emergencyPhoneNumber,
    dNISEmergencyCallingConfiguration_callingCountry,

    -- * EmergencyCallingConfiguration
    EmergencyCallingConfiguration (..),
    newEmergencyCallingConfiguration,
    emergencyCallingConfiguration_dnis,

    -- * EngineTranscribeMedicalSettings
    EngineTranscribeMedicalSettings (..),
    newEngineTranscribeMedicalSettings,
    engineTranscribeMedicalSettings_vocabularyName,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- * EngineTranscribeSettings
    EngineTranscribeSettings (..),
    newEngineTranscribeSettings,
    engineTranscribeSettings_vocabularyName,
    engineTranscribeSettings_vocabularyFilterName,
    engineTranscribeSettings_vocabularyFilterMethod,
    engineTranscribeSettings_region,
    engineTranscribeSettings_languageCode,

    -- * EventsConfiguration
    EventsConfiguration (..),
    newEventsConfiguration,
    eventsConfiguration_lambdaFunctionArn,
    eventsConfiguration_botId,
    eventsConfiguration_outboundEventsHTTPSEndpoint,

    -- * GeoMatchParams
    GeoMatchParams (..),
    newGeoMatchParams,
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_arn,
    identity_name,

    -- * Invite
    Invite (..),
    newInvite,
    invite_status,
    invite_emailStatus,
    invite_inviteId,
    invite_emailAddress,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_enableSIPLogs,

    -- * MediaCapturePipeline
    MediaCapturePipeline (..),
    newMediaCapturePipeline,
    mediaCapturePipeline_status,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_updatedTimestamp,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_sinkArn,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_createdTimestamp,

    -- * MediaPlacement
    MediaPlacement (..),
    newMediaPlacement,
    mediaPlacement_screenDataUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_signalingUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_turnControlUrl,

    -- * Meeting
    Meeting (..),
    newMeeting,
    meeting_mediaRegion,
    meeting_mediaPlacement,
    meeting_externalMeetingId,
    meeting_meetingId,

    -- * MeetingNotificationConfiguration
    MeetingNotificationConfiguration (..),
    newMeetingNotificationConfiguration,
    meetingNotificationConfiguration_snsTopicArn,
    meetingNotificationConfiguration_sqsQueueArn,

    -- * Member
    Member (..),
    newMember,
    member_fullName,
    member_email,
    member_memberId,
    member_memberType,
    member_accountId,

    -- * MemberError
    MemberError (..),
    newMemberError,
    memberError_memberId,
    memberError_errorCode,
    memberError_errorMessage,

    -- * MembershipItem
    MembershipItem (..),
    newMembershipItem,
    membershipItem_memberId,
    membershipItem_role,

    -- * MessagingSessionEndpoint
    MessagingSessionEndpoint (..),
    newMessagingSessionEndpoint,
    messagingSessionEndpoint_url,

    -- * OrderedPhoneNumber
    OrderedPhoneNumber (..),
    newOrderedPhoneNumber,
    orderedPhoneNumber_status,
    orderedPhoneNumber_e164PhoneNumber,

    -- * Origination
    Origination (..),
    newOrigination,
    origination_routes,
    origination_disabled,

    -- * OriginationRoute
    OriginationRoute (..),
    newOriginationRoute,
    originationRoute_priority,
    originationRoute_weight,
    originationRoute_protocol,
    originationRoute_host,
    originationRoute_port,

    -- * Participant
    Participant (..),
    newParticipant,
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- * PhoneNumber
    PhoneNumber (..),
    newPhoneNumber,
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

    -- * PhoneNumberAssociation
    PhoneNumberAssociation (..),
    newPhoneNumberAssociation,
    phoneNumberAssociation_value,
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,

    -- * PhoneNumberCapabilities
    PhoneNumberCapabilities (..),
    newPhoneNumberCapabilities,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundSMS,

    -- * PhoneNumberCountry
    PhoneNumberCountry (..),
    newPhoneNumberCountry,
    phoneNumberCountry_supportedPhoneNumberTypes,
    phoneNumberCountry_countryCode,

    -- * PhoneNumberError
    PhoneNumberError (..),
    newPhoneNumberError,
    phoneNumberError_phoneNumberId,
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,

    -- * PhoneNumberOrder
    PhoneNumberOrder (..),
    newPhoneNumberOrder,
    phoneNumberOrder_status,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_updatedTimestamp,
    phoneNumberOrder_productType,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_createdTimestamp,

    -- * Proxy
    Proxy (..),
    newProxy,
    proxy_defaultSessionExpiryMinutes,
    proxy_disabled,
    proxy_fallBackPhoneNumber,
    proxy_phoneNumberCountries,

    -- * ProxySession
    ProxySession (..),
    newProxySession,
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

    -- * RetentionSettings
    RetentionSettings (..),
    newRetentionSettings,
    retentionSettings_roomRetentionSettings,
    retentionSettings_conversationRetentionSettings,

    -- * Room
    Room (..),
    newRoom,
    room_updatedTimestamp,
    room_createdBy,
    room_accountId,
    room_name,
    room_roomId,
    room_createdTimestamp,

    -- * RoomMembership
    RoomMembership (..),
    newRoomMembership,
    roomMembership_updatedTimestamp,
    roomMembership_role,
    roomMembership_roomId,
    roomMembership_member,
    roomMembership_invitedBy,

    -- * RoomRetentionSettings
    RoomRetentionSettings (..),
    newRoomRetentionSettings,
    roomRetentionSettings_retentionDays,

    -- * SelectedVideoStreams
    SelectedVideoStreams (..),
    newSelectedVideoStreams,
    selectedVideoStreams_attendeeIds,
    selectedVideoStreams_externalUserIds,

    -- * SigninDelegateGroup
    SigninDelegateGroup (..),
    newSigninDelegateGroup,
    signinDelegateGroup_groupName,

    -- * SipMediaApplication
    SipMediaApplication (..),
    newSipMediaApplication,
    sipMediaApplication_updatedTimestamp,
    sipMediaApplication_name,
    sipMediaApplication_awsRegion,
    sipMediaApplication_endpoints,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_sipMediaApplicationId,

    -- * SipMediaApplicationCall
    SipMediaApplicationCall (..),
    newSipMediaApplicationCall,
    sipMediaApplicationCall_transactionId,

    -- * SipMediaApplicationEndpoint
    SipMediaApplicationEndpoint (..),
    newSipMediaApplicationEndpoint,
    sipMediaApplicationEndpoint_lambdaArn,

    -- * SipMediaApplicationLoggingConfiguration
    SipMediaApplicationLoggingConfiguration (..),
    newSipMediaApplicationLoggingConfiguration,
    sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs,

    -- * SipRule
    SipRule (..),
    newSipRule,
    sipRule_disabled,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_updatedTimestamp,
    sipRule_name,
    sipRule_triggerValue,
    sipRule_createdTimestamp,
    sipRule_sipRuleId,

    -- * SipRuleTargetApplication
    SipRuleTargetApplication (..),
    newSipRuleTargetApplication,
    sipRuleTargetApplication_priority,
    sipRuleTargetApplication_awsRegion,
    sipRuleTargetApplication_sipMediaApplicationId,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_selectedVideoStreams,

    -- * StreamingConfiguration
    StreamingConfiguration (..),
    newStreamingConfiguration,
    streamingConfiguration_disabled,
    streamingConfiguration_streamingNotificationTargets,
    streamingConfiguration_dataRetentionInHours,

    -- * StreamingNotificationTarget
    StreamingNotificationTarget (..),
    newStreamingNotificationTarget,
    streamingNotificationTarget_notificationTarget,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TelephonySettings
    TelephonySettings (..),
    newTelephonySettings,
    telephonySettings_inboundCalling,
    telephonySettings_outboundCalling,
    telephonySettings_sms,

    -- * Termination
    Termination (..),
    newTermination,
    termination_defaultPhoneNumber,
    termination_disabled,
    termination_callingRegions,
    termination_cpsLimit,
    termination_cidrAllowedList,

    -- * TerminationHealth
    TerminationHealth (..),
    newTerminationHealth,
    terminationHealth_source,
    terminationHealth_timestamp,

    -- * TranscriptionConfiguration
    TranscriptionConfiguration (..),
    newTranscriptionConfiguration,
    transcriptionConfiguration_engineTranscribeMedicalSettings,
    transcriptionConfiguration_engineTranscribeSettings,

    -- * UpdatePhoneNumberRequestItem
    UpdatePhoneNumberRequestItem (..),
    newUpdatePhoneNumberRequestItem,
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- * UpdateUserRequestItem
    UpdateUserRequestItem (..),
    newUpdateUserRequestItem,
    updateUserRequestItem_licenseType,
    updateUserRequestItem_userType,
    updateUserRequestItem_alexaForBusinessMetadata,
    updateUserRequestItem_userId,

    -- * User
    User (..),
    newUser,
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

    -- * UserError
    UserError (..),
    newUserError,
    userError_userId,
    userError_errorCode,
    userError_errorMessage,

    -- * UserSettings
    UserSettings (..),
    newUserSettings,
    userSettings_telephony,

    -- * VideoArtifactsConfiguration
    VideoArtifactsConfiguration (..),
    newVideoArtifactsConfiguration,
    videoArtifactsConfiguration_muxType,
    videoArtifactsConfiguration_state,

    -- * VoiceConnector
    VoiceConnector (..),
    newVoiceConnector,
    voiceConnector_updatedTimestamp,
    voiceConnector_outboundHostName,
    voiceConnector_name,
    voiceConnector_requireEncryption,
    voiceConnector_awsRegion,
    voiceConnector_voiceConnectorId,
    voiceConnector_voiceConnectorArn,
    voiceConnector_createdTimestamp,

    -- * VoiceConnectorGroup
    VoiceConnectorGroup (..),
    newVoiceConnectorGroup,
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorItems,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_name,
    voiceConnectorGroup_createdTimestamp,

    -- * VoiceConnectorItem
    VoiceConnectorItem (..),
    newVoiceConnectorItem,
    voiceConnectorItem_voiceConnectorId,
    voiceConnectorItem_priority,

    -- * VoiceConnectorSettings
    VoiceConnectorSettings (..),
    newVoiceConnectorSettings,
    voiceConnectorSettings_cdrBucket,
  )
where

import Network.AWS.Chime.Types.Account
import Network.AWS.Chime.Types.AccountSettings
import Network.AWS.Chime.Types.AccountStatus
import Network.AWS.Chime.Types.AccountType
import Network.AWS.Chime.Types.AlexaForBusinessMetadata
import Network.AWS.Chime.Types.AppInstance
import Network.AWS.Chime.Types.AppInstanceAdmin
import Network.AWS.Chime.Types.AppInstanceAdminSummary
import Network.AWS.Chime.Types.AppInstanceDataType
import Network.AWS.Chime.Types.AppInstanceRetentionSettings
import Network.AWS.Chime.Types.AppInstanceStreamingConfiguration
import Network.AWS.Chime.Types.AppInstanceSummary
import Network.AWS.Chime.Types.AppInstanceUser
import Network.AWS.Chime.Types.AppInstanceUserMembershipSummary
import Network.AWS.Chime.Types.AppInstanceUserSummary
import Network.AWS.Chime.Types.ArtifactsConfiguration
import Network.AWS.Chime.Types.ArtifactsState
import Network.AWS.Chime.Types.Attendee
import Network.AWS.Chime.Types.AudioArtifactsConfiguration
import Network.AWS.Chime.Types.AudioMuxType
import Network.AWS.Chime.Types.BatchChannelMemberships
import Network.AWS.Chime.Types.BatchCreateChannelMembershipError
import Network.AWS.Chime.Types.Bot
import Network.AWS.Chime.Types.BotType
import Network.AWS.Chime.Types.BusinessCallingSettings
import Network.AWS.Chime.Types.CallingNameStatus
import Network.AWS.Chime.Types.Capability
import Network.AWS.Chime.Types.Channel
import Network.AWS.Chime.Types.ChannelBan
import Network.AWS.Chime.Types.ChannelBanSummary
import Network.AWS.Chime.Types.ChannelMembership
import Network.AWS.Chime.Types.ChannelMembershipForAppInstanceUserSummary
import Network.AWS.Chime.Types.ChannelMembershipSummary
import Network.AWS.Chime.Types.ChannelMembershipType
import Network.AWS.Chime.Types.ChannelMessage
import Network.AWS.Chime.Types.ChannelMessagePersistenceType
import Network.AWS.Chime.Types.ChannelMessageSummary
import Network.AWS.Chime.Types.ChannelMessageType
import Network.AWS.Chime.Types.ChannelMode
import Network.AWS.Chime.Types.ChannelModeratedByAppInstanceUserSummary
import Network.AWS.Chime.Types.ChannelModerator
import Network.AWS.Chime.Types.ChannelModeratorSummary
import Network.AWS.Chime.Types.ChannelPrivacy
import Network.AWS.Chime.Types.ChannelRetentionSettings
import Network.AWS.Chime.Types.ChannelSummary
import Network.AWS.Chime.Types.ChimeSdkMeetingConfiguration
import Network.AWS.Chime.Types.ContentArtifactsConfiguration
import Network.AWS.Chime.Types.ContentMuxType
import Network.AWS.Chime.Types.ConversationRetentionSettings
import Network.AWS.Chime.Types.CreateAttendeeError
import Network.AWS.Chime.Types.CreateAttendeeRequestItem
import Network.AWS.Chime.Types.Credential
import Network.AWS.Chime.Types.DNISEmergencyCallingConfiguration
import Network.AWS.Chime.Types.EmailStatus
import Network.AWS.Chime.Types.EmergencyCallingConfiguration
import Network.AWS.Chime.Types.EngineTranscribeMedicalSettings
import Network.AWS.Chime.Types.EngineTranscribeSettings
import Network.AWS.Chime.Types.ErrorCode
import Network.AWS.Chime.Types.EventsConfiguration
import Network.AWS.Chime.Types.GeoMatchLevel
import Network.AWS.Chime.Types.GeoMatchParams
import Network.AWS.Chime.Types.Identity
import Network.AWS.Chime.Types.Invite
import Network.AWS.Chime.Types.InviteStatus
import Network.AWS.Chime.Types.License
import Network.AWS.Chime.Types.LoggingConfiguration
import Network.AWS.Chime.Types.MediaCapturePipeline
import Network.AWS.Chime.Types.MediaPipelineSinkType
import Network.AWS.Chime.Types.MediaPipelineSourceType
import Network.AWS.Chime.Types.MediaPipelineStatus
import Network.AWS.Chime.Types.MediaPlacement
import Network.AWS.Chime.Types.Meeting
import Network.AWS.Chime.Types.MeetingNotificationConfiguration
import Network.AWS.Chime.Types.Member
import Network.AWS.Chime.Types.MemberError
import Network.AWS.Chime.Types.MemberType
import Network.AWS.Chime.Types.MembershipItem
import Network.AWS.Chime.Types.MessagingSessionEndpoint
import Network.AWS.Chime.Types.NotificationTarget
import Network.AWS.Chime.Types.NumberSelectionBehavior
import Network.AWS.Chime.Types.OrderedPhoneNumber
import Network.AWS.Chime.Types.OrderedPhoneNumberStatus
import Network.AWS.Chime.Types.Origination
import Network.AWS.Chime.Types.OriginationRoute
import Network.AWS.Chime.Types.OriginationRouteProtocol
import Network.AWS.Chime.Types.Participant
import Network.AWS.Chime.Types.PhoneNumber
import Network.AWS.Chime.Types.PhoneNumberAssociation
import Network.AWS.Chime.Types.PhoneNumberAssociationName
import Network.AWS.Chime.Types.PhoneNumberCapabilities
import Network.AWS.Chime.Types.PhoneNumberCountry
import Network.AWS.Chime.Types.PhoneNumberError
import Network.AWS.Chime.Types.PhoneNumberOrder
import Network.AWS.Chime.Types.PhoneNumberOrderStatus
import Network.AWS.Chime.Types.PhoneNumberProductType
import Network.AWS.Chime.Types.PhoneNumberStatus
import Network.AWS.Chime.Types.PhoneNumberType
import Network.AWS.Chime.Types.Proxy
import Network.AWS.Chime.Types.ProxySession
import Network.AWS.Chime.Types.ProxySessionStatus
import Network.AWS.Chime.Types.RegistrationStatus
import Network.AWS.Chime.Types.RetentionSettings
import Network.AWS.Chime.Types.Room
import Network.AWS.Chime.Types.RoomMembership
import Network.AWS.Chime.Types.RoomMembershipRole
import Network.AWS.Chime.Types.RoomRetentionSettings
import Network.AWS.Chime.Types.SelectedVideoStreams
import Network.AWS.Chime.Types.SigninDelegateGroup
import Network.AWS.Chime.Types.SipMediaApplication
import Network.AWS.Chime.Types.SipMediaApplicationCall
import Network.AWS.Chime.Types.SipMediaApplicationEndpoint
import Network.AWS.Chime.Types.SipMediaApplicationLoggingConfiguration
import Network.AWS.Chime.Types.SipRule
import Network.AWS.Chime.Types.SipRuleTargetApplication
import Network.AWS.Chime.Types.SipRuleTriggerType
import Network.AWS.Chime.Types.SortOrder
import Network.AWS.Chime.Types.SourceConfiguration
import Network.AWS.Chime.Types.StreamingConfiguration
import Network.AWS.Chime.Types.StreamingNotificationTarget
import Network.AWS.Chime.Types.Tag
import Network.AWS.Chime.Types.TelephonySettings
import Network.AWS.Chime.Types.Termination
import Network.AWS.Chime.Types.TerminationHealth
import Network.AWS.Chime.Types.TranscribeLanguageCode
import Network.AWS.Chime.Types.TranscribeMedicalLanguageCode
import Network.AWS.Chime.Types.TranscribeMedicalRegion
import Network.AWS.Chime.Types.TranscribeMedicalSpecialty
import Network.AWS.Chime.Types.TranscribeMedicalType
import Network.AWS.Chime.Types.TranscribeRegion
import Network.AWS.Chime.Types.TranscribeVocabularyFilterMethod
import Network.AWS.Chime.Types.TranscriptionConfiguration
import Network.AWS.Chime.Types.UpdatePhoneNumberRequestItem
import Network.AWS.Chime.Types.UpdateUserRequestItem
import Network.AWS.Chime.Types.User
import Network.AWS.Chime.Types.UserError
import Network.AWS.Chime.Types.UserSettings
import Network.AWS.Chime.Types.UserType
import Network.AWS.Chime.Types.VideoArtifactsConfiguration
import Network.AWS.Chime.Types.VideoMuxType
import Network.AWS.Chime.Types.VoiceConnector
import Network.AWS.Chime.Types.VoiceConnectorAwsRegion
import Network.AWS.Chime.Types.VoiceConnectorGroup
import Network.AWS.Chime.Types.VoiceConnectorItem
import Network.AWS.Chime.Types.VoiceConnectorSettings
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-05-01@ of the Amazon Chime SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Chime",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "chime",
      Core._serviceSigningName = "chime",
      Core._serviceVersion = "2018-05-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Chime",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have permissions to perform the requested operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The client exceeded its request rate limit.
_ThrottledClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | The request exceeds the resource limit.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The request was well-formed but was unable to be followed due to
-- semantic errors.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The client is permanently forbidden from making the request.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | One or more of the resources in the request does not exist in the
-- system.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service encountered an unexpected error.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | The client is not currently authorized to make the request.
_UnauthorizedClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The input parameters don\'t match the service\'s restrictions.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

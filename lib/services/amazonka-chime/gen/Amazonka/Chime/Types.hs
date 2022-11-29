{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ThrottledClientException,
    _AccessDeniedException,
    _NotFoundException,
    _ServiceUnavailableException,
    _UnprocessableEntityException,
    _ResourceLimitExceededException,
    _ForbiddenException,
    _ConflictException,
    _BadRequestException,
    _UnauthorizedClientException,
    _ServiceFailureException,

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

    -- * TranscribeContentIdentificationType
    TranscribeContentIdentificationType (..),

    -- * TranscribeContentRedactionType
    TranscribeContentRedactionType (..),

    -- * TranscribeLanguageCode
    TranscribeLanguageCode (..),

    -- * TranscribeMedicalContentIdentificationType
    TranscribeMedicalContentIdentificationType (..),

    -- * TranscribeMedicalLanguageCode
    TranscribeMedicalLanguageCode (..),

    -- * TranscribeMedicalRegion
    TranscribeMedicalRegion (..),

    -- * TranscribeMedicalSpecialty
    TranscribeMedicalSpecialty (..),

    -- * TranscribeMedicalType
    TranscribeMedicalType (..),

    -- * TranscribePartialResultsStability
    TranscribePartialResultsStability (..),

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
    account_supportedLicenses,
    account_createdTimestamp,
    account_signinDelegateGroups,
    account_accountType,
    account_accountStatus,
    account_defaultLicense,
    account_awsAccountId,
    account_accountId,
    account_name,

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_disableRemoteControl,
    accountSettings_enableDialOut,

    -- * Address
    Address (..),
    newAddress,
    address_postalCode,
    address_streetSuffix,
    address_country,
    address_postDirectional,
    address_state,
    address_streetName,
    address_postalCodePlus4,
    address_city,
    address_preDirectional,
    address_streetNumber,

    -- * AlexaForBusinessMetadata
    AlexaForBusinessMetadata (..),
    newAlexaForBusinessMetadata,
    alexaForBusinessMetadata_alexaForBusinessRoomArn,
    alexaForBusinessMetadata_isAlexaForBusinessEnabled,

    -- * AppInstance
    AppInstance (..),
    newAppInstance,
    appInstance_lastUpdatedTimestamp,
    appInstance_name,
    appInstance_metadata,
    appInstance_createdTimestamp,
    appInstance_appInstanceArn,

    -- * AppInstanceAdmin
    AppInstanceAdmin (..),
    newAppInstanceAdmin,
    appInstanceAdmin_createdTimestamp,
    appInstanceAdmin_appInstanceArn,
    appInstanceAdmin_admin,

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
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_name,
    appInstanceUser_metadata,
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,

    -- * AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (..),
    newAppInstanceUserMembershipSummary,
    appInstanceUserMembershipSummary_type,
    appInstanceUserMembershipSummary_readMarkerTimestamp,

    -- * AppInstanceUserSummary
    AppInstanceUserSummary (..),
    newAppInstanceUserSummary,
    appInstanceUserSummary_name,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_appInstanceUserArn,

    -- * ArtifactsConfiguration
    ArtifactsConfiguration (..),
    newArtifactsConfiguration,
    artifactsConfiguration_audio,
    artifactsConfiguration_video,
    artifactsConfiguration_content,

    -- * Attendee
    Attendee (..),
    newAttendee,
    attendee_externalUserId,
    attendee_attendeeId,
    attendee_joinToken,

    -- * AudioArtifactsConfiguration
    AudioArtifactsConfiguration (..),
    newAudioArtifactsConfiguration,
    audioArtifactsConfiguration_muxType,

    -- * BatchChannelMemberships
    BatchChannelMemberships (..),
    newBatchChannelMemberships,
    batchChannelMemberships_type,
    batchChannelMemberships_members,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_invitedBy,

    -- * BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (..),
    newBatchCreateChannelMembershipError,
    batchCreateChannelMembershipError_memberArn,
    batchCreateChannelMembershipError_errorMessage,
    batchCreateChannelMembershipError_errorCode,

    -- * Bot
    Bot (..),
    newBot,
    bot_botEmail,
    bot_createdTimestamp,
    bot_updatedTimestamp,
    bot_displayName,
    bot_botId,
    bot_securityToken,
    bot_botType,
    bot_userId,
    bot_disabled,

    -- * BusinessCallingSettings
    BusinessCallingSettings (..),
    newBusinessCallingSettings,
    businessCallingSettings_cdrBucket,

    -- * CandidateAddress
    CandidateAddress (..),
    newCandidateAddress,
    candidateAddress_postalCode,
    candidateAddress_country,
    candidateAddress_state,
    candidateAddress_streetInfo,
    candidateAddress_postalCodePlus4,
    candidateAddress_city,
    candidateAddress_streetNumber,

    -- * Channel
    Channel (..),
    newChannel,
    channel_lastUpdatedTimestamp,
    channel_lastMessageTimestamp,
    channel_name,
    channel_metadata,
    channel_createdTimestamp,
    channel_channelArn,
    channel_privacy,
    channel_mode,
    channel_createdBy,

    -- * ChannelBan
    ChannelBan (..),
    newChannelBan,
    channelBan_member,
    channelBan_createdTimestamp,
    channelBan_channelArn,
    channelBan_createdBy,

    -- * ChannelBanSummary
    ChannelBanSummary (..),
    newChannelBanSummary,
    channelBanSummary_member,

    -- * ChannelMembership
    ChannelMembership (..),
    newChannelMembership,
    channelMembership_lastUpdatedTimestamp,
    channelMembership_member,
    channelMembership_type,
    channelMembership_createdTimestamp,
    channelMembership_channelArn,
    channelMembership_invitedBy,

    -- * ChannelMembershipForAppInstanceUserSummary
    ChannelMembershipForAppInstanceUserSummary (..),
    newChannelMembershipForAppInstanceUserSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,

    -- * ChannelMembershipSummary
    ChannelMembershipSummary (..),
    newChannelMembershipSummary,
    channelMembershipSummary_member,

    -- * ChannelMessage
    ChannelMessage (..),
    newChannelMessage,
    channelMessage_lastUpdatedTimestamp,
    channelMessage_type,
    channelMessage_metadata,
    channelMessage_createdTimestamp,
    channelMessage_redacted,
    channelMessage_channelArn,
    channelMessage_messageId,
    channelMessage_lastEditedTimestamp,
    channelMessage_sender,
    channelMessage_persistence,
    channelMessage_content,

    -- * ChannelMessageSummary
    ChannelMessageSummary (..),
    newChannelMessageSummary,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_type,
    channelMessageSummary_metadata,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_redacted,
    channelMessageSummary_messageId,
    channelMessageSummary_lastEditedTimestamp,
    channelMessageSummary_sender,
    channelMessageSummary_content,

    -- * ChannelModeratedByAppInstanceUserSummary
    ChannelModeratedByAppInstanceUserSummary (..),
    newChannelModeratedByAppInstanceUserSummary,
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- * ChannelModerator
    ChannelModerator (..),
    newChannelModerator,
    channelModerator_moderator,
    channelModerator_createdTimestamp,
    channelModerator_channelArn,
    channelModerator_createdBy,

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
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_mode,

    -- * ChimeSdkMeetingConfiguration
    ChimeSdkMeetingConfiguration (..),
    newChimeSdkMeetingConfiguration,
    chimeSdkMeetingConfiguration_sourceConfiguration,
    chimeSdkMeetingConfiguration_artifactsConfiguration,

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
    createAttendeeError_externalUserId,
    createAttendeeError_errorMessage,
    createAttendeeError_errorCode,

    -- * CreateAttendeeRequestItem
    CreateAttendeeRequestItem (..),
    newCreateAttendeeRequestItem,
    createAttendeeRequestItem_tags,
    createAttendeeRequestItem_externalUserId,

    -- * Credential
    Credential (..),
    newCredential,
    credential_password,
    credential_username,

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
    engineTranscribeMedicalSettings_contentIdentificationType,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- * EngineTranscribeSettings
    EngineTranscribeSettings (..),
    newEngineTranscribeSettings,
    engineTranscribeSettings_vocabularyFilterMethod,
    engineTranscribeSettings_vocabularyName,
    engineTranscribeSettings_contentIdentificationType,
    engineTranscribeSettings_enablePartialResultsStabilization,
    engineTranscribeSettings_languageModelName,
    engineTranscribeSettings_piiEntityTypes,
    engineTranscribeSettings_region,
    engineTranscribeSettings_vocabularyFilterName,
    engineTranscribeSettings_contentRedactionType,
    engineTranscribeSettings_partialResultsStability,
    engineTranscribeSettings_languageCode,

    -- * EventsConfiguration
    EventsConfiguration (..),
    newEventsConfiguration,
    eventsConfiguration_outboundEventsHTTPSEndpoint,
    eventsConfiguration_botId,
    eventsConfiguration_lambdaFunctionArn,

    -- * GeoMatchParams
    GeoMatchParams (..),
    newGeoMatchParams,
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_name,
    identity_arn,

    -- * Invite
    Invite (..),
    newInvite,
    invite_emailStatus,
    invite_status,
    invite_emailAddress,
    invite_inviteId,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_enableSIPLogs,
    loggingConfiguration_enableMediaMetricLogs,

    -- * MediaCapturePipeline
    MediaCapturePipeline (..),
    newMediaCapturePipeline,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_createdTimestamp,
    mediaCapturePipeline_updatedTimestamp,
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_status,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_sinkArn,

    -- * MediaPlacement
    MediaPlacement (..),
    newMediaPlacement,
    mediaPlacement_signalingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenDataUrl,
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_turnControlUrl,

    -- * Meeting
    Meeting (..),
    newMeeting,
    meeting_mediaRegion,
    meeting_externalMeetingId,
    meeting_mediaPlacement,
    meeting_meetingId,

    -- * MeetingNotificationConfiguration
    MeetingNotificationConfiguration (..),
    newMeetingNotificationConfiguration,
    meetingNotificationConfiguration_snsTopicArn,
    meetingNotificationConfiguration_sqsQueueArn,

    -- * Member
    Member (..),
    newMember,
    member_memberId,
    member_memberType,
    member_email,
    member_accountId,
    member_fullName,

    -- * MemberError
    MemberError (..),
    newMemberError,
    memberError_memberId,
    memberError_errorMessage,
    memberError_errorCode,

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
    orderedPhoneNumber_e164PhoneNumber,
    orderedPhoneNumber_status,

    -- * Origination
    Origination (..),
    newOrigination,
    origination_disabled,
    origination_routes,

    -- * OriginationRoute
    OriginationRoute (..),
    newOriginationRoute,
    originationRoute_port,
    originationRoute_host,
    originationRoute_priority,
    originationRoute_weight,
    originationRoute_protocol,

    -- * Participant
    Participant (..),
    newParticipant,
    participant_proxyPhoneNumber,
    participant_phoneNumber,

    -- * PhoneNumber
    PhoneNumber (..),
    newPhoneNumber,
    phoneNumber_type,
    phoneNumber_e164PhoneNumber,
    phoneNumber_productType,
    phoneNumber_country,
    phoneNumber_associations,
    phoneNumber_createdTimestamp,
    phoneNumber_updatedTimestamp,
    phoneNumber_status,
    phoneNumber_deletionTimestamp,
    phoneNumber_phoneNumberId,
    phoneNumber_capabilities,
    phoneNumber_callingNameStatus,
    phoneNumber_callingName,

    -- * PhoneNumberAssociation
    PhoneNumberAssociation (..),
    newPhoneNumberAssociation,
    phoneNumberAssociation_name,
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_value,

    -- * PhoneNumberCapabilities
    PhoneNumberCapabilities (..),
    newPhoneNumberCapabilities,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_outboundSMS,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_inboundMMS,

    -- * PhoneNumberCountry
    PhoneNumberCountry (..),
    newPhoneNumberCountry,
    phoneNumberCountry_countryCode,
    phoneNumberCountry_supportedPhoneNumberTypes,

    -- * PhoneNumberError
    PhoneNumberError (..),
    newPhoneNumberError,
    phoneNumberError_errorMessage,
    phoneNumberError_phoneNumberId,
    phoneNumberError_errorCode,

    -- * PhoneNumberOrder
    PhoneNumberOrder (..),
    newPhoneNumberOrder,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_productType,
    phoneNumberOrder_createdTimestamp,
    phoneNumberOrder_updatedTimestamp,
    phoneNumberOrder_status,
    phoneNumberOrder_orderedPhoneNumbers,

    -- * Proxy
    Proxy (..),
    newProxy,
    proxy_phoneNumberCountries,
    proxy_defaultSessionExpiryMinutes,
    proxy_fallBackPhoneNumber,
    proxy_disabled,

    -- * ProxySession
    ProxySession (..),
    newProxySession,
    proxySession_name,
    proxySession_geoMatchParams,
    proxySession_voiceConnectorId,
    proxySession_createdTimestamp,
    proxySession_updatedTimestamp,
    proxySession_proxySessionId,
    proxySession_status,
    proxySession_endedTimestamp,
    proxySession_expiryMinutes,
    proxySession_capabilities,
    proxySession_geoMatchLevel,
    proxySession_numberSelectionBehavior,
    proxySession_participants,

    -- * RetentionSettings
    RetentionSettings (..),
    newRetentionSettings,
    retentionSettings_roomRetentionSettings,
    retentionSettings_conversationRetentionSettings,

    -- * Room
    Room (..),
    newRoom,
    room_name,
    room_roomId,
    room_createdTimestamp,
    room_updatedTimestamp,
    room_accountId,
    room_createdBy,

    -- * RoomMembership
    RoomMembership (..),
    newRoomMembership,
    roomMembership_member,
    roomMembership_roomId,
    roomMembership_updatedTimestamp,
    roomMembership_invitedBy,
    roomMembership_role,

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
    sipMediaApplication_name,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_updatedTimestamp,
    sipMediaApplication_endpoints,
    sipMediaApplication_awsRegion,
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
    sipRule_sipRuleId,
    sipRule_name,
    sipRule_createdTimestamp,
    sipRule_updatedTimestamp,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_triggerValue,
    sipRule_disabled,

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
    streamingConfiguration_streamingNotificationTargets,
    streamingConfiguration_disabled,
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
    termination_cidrAllowedList,
    termination_cpsLimit,
    termination_defaultPhoneNumber,
    termination_disabled,
    termination_callingRegions,

    -- * TerminationHealth
    TerminationHealth (..),
    newTerminationHealth,
    terminationHealth_timestamp,
    terminationHealth_source,

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
    updateUserRequestItem_alexaForBusinessMetadata,
    updateUserRequestItem_userType,
    updateUserRequestItem_userId,

    -- * User
    User (..),
    newUser,
    user_personalPIN,
    user_invitedOn,
    user_licenseType,
    user_displayName,
    user_alexaForBusinessMetadata,
    user_userType,
    user_registeredOn,
    user_accountId,
    user_userRegistrationStatus,
    user_primaryEmail,
    user_primaryProvisionedNumber,
    user_userInvitationStatus,
    user_userId,

    -- * UserError
    UserError (..),
    newUserError,
    userError_errorMessage,
    userError_userId,
    userError_errorCode,

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
    voiceConnector_name,
    voiceConnector_voiceConnectorId,
    voiceConnector_requireEncryption,
    voiceConnector_createdTimestamp,
    voiceConnector_updatedTimestamp,
    voiceConnector_awsRegion,
    voiceConnector_voiceConnectorArn,
    voiceConnector_outboundHostName,

    -- * VoiceConnectorGroup
    VoiceConnectorGroup (..),
    newVoiceConnectorGroup,
    voiceConnectorGroup_name,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_createdTimestamp,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_voiceConnectorItems,

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

import Amazonka.Chime.Types.Account
import Amazonka.Chime.Types.AccountSettings
import Amazonka.Chime.Types.AccountStatus
import Amazonka.Chime.Types.AccountType
import Amazonka.Chime.Types.Address
import Amazonka.Chime.Types.AlexaForBusinessMetadata
import Amazonka.Chime.Types.AppInstance
import Amazonka.Chime.Types.AppInstanceAdmin
import Amazonka.Chime.Types.AppInstanceAdminSummary
import Amazonka.Chime.Types.AppInstanceDataType
import Amazonka.Chime.Types.AppInstanceRetentionSettings
import Amazonka.Chime.Types.AppInstanceStreamingConfiguration
import Amazonka.Chime.Types.AppInstanceSummary
import Amazonka.Chime.Types.AppInstanceUser
import Amazonka.Chime.Types.AppInstanceUserMembershipSummary
import Amazonka.Chime.Types.AppInstanceUserSummary
import Amazonka.Chime.Types.ArtifactsConfiguration
import Amazonka.Chime.Types.ArtifactsState
import Amazonka.Chime.Types.Attendee
import Amazonka.Chime.Types.AudioArtifactsConfiguration
import Amazonka.Chime.Types.AudioMuxType
import Amazonka.Chime.Types.BatchChannelMemberships
import Amazonka.Chime.Types.BatchCreateChannelMembershipError
import Amazonka.Chime.Types.Bot
import Amazonka.Chime.Types.BotType
import Amazonka.Chime.Types.BusinessCallingSettings
import Amazonka.Chime.Types.CallingNameStatus
import Amazonka.Chime.Types.CandidateAddress
import Amazonka.Chime.Types.Capability
import Amazonka.Chime.Types.Channel
import Amazonka.Chime.Types.ChannelBan
import Amazonka.Chime.Types.ChannelBanSummary
import Amazonka.Chime.Types.ChannelMembership
import Amazonka.Chime.Types.ChannelMembershipForAppInstanceUserSummary
import Amazonka.Chime.Types.ChannelMembershipSummary
import Amazonka.Chime.Types.ChannelMembershipType
import Amazonka.Chime.Types.ChannelMessage
import Amazonka.Chime.Types.ChannelMessagePersistenceType
import Amazonka.Chime.Types.ChannelMessageSummary
import Amazonka.Chime.Types.ChannelMessageType
import Amazonka.Chime.Types.ChannelMode
import Amazonka.Chime.Types.ChannelModeratedByAppInstanceUserSummary
import Amazonka.Chime.Types.ChannelModerator
import Amazonka.Chime.Types.ChannelModeratorSummary
import Amazonka.Chime.Types.ChannelPrivacy
import Amazonka.Chime.Types.ChannelRetentionSettings
import Amazonka.Chime.Types.ChannelSummary
import Amazonka.Chime.Types.ChimeSdkMeetingConfiguration
import Amazonka.Chime.Types.ContentArtifactsConfiguration
import Amazonka.Chime.Types.ContentMuxType
import Amazonka.Chime.Types.ConversationRetentionSettings
import Amazonka.Chime.Types.CreateAttendeeError
import Amazonka.Chime.Types.CreateAttendeeRequestItem
import Amazonka.Chime.Types.Credential
import Amazonka.Chime.Types.DNISEmergencyCallingConfiguration
import Amazonka.Chime.Types.EmailStatus
import Amazonka.Chime.Types.EmergencyCallingConfiguration
import Amazonka.Chime.Types.EngineTranscribeMedicalSettings
import Amazonka.Chime.Types.EngineTranscribeSettings
import Amazonka.Chime.Types.ErrorCode
import Amazonka.Chime.Types.EventsConfiguration
import Amazonka.Chime.Types.GeoMatchLevel
import Amazonka.Chime.Types.GeoMatchParams
import Amazonka.Chime.Types.Identity
import Amazonka.Chime.Types.Invite
import Amazonka.Chime.Types.InviteStatus
import Amazonka.Chime.Types.License
import Amazonka.Chime.Types.LoggingConfiguration
import Amazonka.Chime.Types.MediaCapturePipeline
import Amazonka.Chime.Types.MediaPipelineSinkType
import Amazonka.Chime.Types.MediaPipelineSourceType
import Amazonka.Chime.Types.MediaPipelineStatus
import Amazonka.Chime.Types.MediaPlacement
import Amazonka.Chime.Types.Meeting
import Amazonka.Chime.Types.MeetingNotificationConfiguration
import Amazonka.Chime.Types.Member
import Amazonka.Chime.Types.MemberError
import Amazonka.Chime.Types.MemberType
import Amazonka.Chime.Types.MembershipItem
import Amazonka.Chime.Types.MessagingSessionEndpoint
import Amazonka.Chime.Types.NotificationTarget
import Amazonka.Chime.Types.NumberSelectionBehavior
import Amazonka.Chime.Types.OrderedPhoneNumber
import Amazonka.Chime.Types.OrderedPhoneNumberStatus
import Amazonka.Chime.Types.Origination
import Amazonka.Chime.Types.OriginationRoute
import Amazonka.Chime.Types.OriginationRouteProtocol
import Amazonka.Chime.Types.Participant
import Amazonka.Chime.Types.PhoneNumber
import Amazonka.Chime.Types.PhoneNumberAssociation
import Amazonka.Chime.Types.PhoneNumberAssociationName
import Amazonka.Chime.Types.PhoneNumberCapabilities
import Amazonka.Chime.Types.PhoneNumberCountry
import Amazonka.Chime.Types.PhoneNumberError
import Amazonka.Chime.Types.PhoneNumberOrder
import Amazonka.Chime.Types.PhoneNumberOrderStatus
import Amazonka.Chime.Types.PhoneNumberProductType
import Amazonka.Chime.Types.PhoneNumberStatus
import Amazonka.Chime.Types.PhoneNumberType
import Amazonka.Chime.Types.Proxy
import Amazonka.Chime.Types.ProxySession
import Amazonka.Chime.Types.ProxySessionStatus
import Amazonka.Chime.Types.RegistrationStatus
import Amazonka.Chime.Types.RetentionSettings
import Amazonka.Chime.Types.Room
import Amazonka.Chime.Types.RoomMembership
import Amazonka.Chime.Types.RoomMembershipRole
import Amazonka.Chime.Types.RoomRetentionSettings
import Amazonka.Chime.Types.SelectedVideoStreams
import Amazonka.Chime.Types.SigninDelegateGroup
import Amazonka.Chime.Types.SipMediaApplication
import Amazonka.Chime.Types.SipMediaApplicationCall
import Amazonka.Chime.Types.SipMediaApplicationEndpoint
import Amazonka.Chime.Types.SipMediaApplicationLoggingConfiguration
import Amazonka.Chime.Types.SipRule
import Amazonka.Chime.Types.SipRuleTargetApplication
import Amazonka.Chime.Types.SipRuleTriggerType
import Amazonka.Chime.Types.SortOrder
import Amazonka.Chime.Types.SourceConfiguration
import Amazonka.Chime.Types.StreamingConfiguration
import Amazonka.Chime.Types.StreamingNotificationTarget
import Amazonka.Chime.Types.Tag
import Amazonka.Chime.Types.TelephonySettings
import Amazonka.Chime.Types.Termination
import Amazonka.Chime.Types.TerminationHealth
import Amazonka.Chime.Types.TranscribeContentIdentificationType
import Amazonka.Chime.Types.TranscribeContentRedactionType
import Amazonka.Chime.Types.TranscribeLanguageCode
import Amazonka.Chime.Types.TranscribeMedicalContentIdentificationType
import Amazonka.Chime.Types.TranscribeMedicalLanguageCode
import Amazonka.Chime.Types.TranscribeMedicalRegion
import Amazonka.Chime.Types.TranscribeMedicalSpecialty
import Amazonka.Chime.Types.TranscribeMedicalType
import Amazonka.Chime.Types.TranscribePartialResultsStability
import Amazonka.Chime.Types.TranscribeRegion
import Amazonka.Chime.Types.TranscribeVocabularyFilterMethod
import Amazonka.Chime.Types.TranscriptionConfiguration
import Amazonka.Chime.Types.UpdatePhoneNumberRequestItem
import Amazonka.Chime.Types.UpdateUserRequestItem
import Amazonka.Chime.Types.User
import Amazonka.Chime.Types.UserError
import Amazonka.Chime.Types.UserSettings
import Amazonka.Chime.Types.UserType
import Amazonka.Chime.Types.VideoArtifactsConfiguration
import Amazonka.Chime.Types.VideoMuxType
import Amazonka.Chime.Types.VoiceConnector
import Amazonka.Chime.Types.VoiceConnectorAwsRegion
import Amazonka.Chime.Types.VoiceConnectorGroup
import Amazonka.Chime.Types.VoiceConnectorItem
import Amazonka.Chime.Types.VoiceConnectorSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-01@ of the Amazon Chime SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Chime",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "chime",
      Core.signingName = "chime",
      Core.version = "2018-05-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Chime",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The client exceeded its request rate limit.
_ThrottledClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | You don\'t have permissions to perform the requested operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | One or more of the resources in the request does not exist in the
-- system.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request was well-formed but was unable to be followed due to
-- semantic errors.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | The request exceeds the resource limit.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The client is permanently forbidden from making the request.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The input parameters don\'t match the service\'s restrictions.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The client is not currently authorized to make the request.
_UnauthorizedClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401

-- | The service encountered an unexpected error.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

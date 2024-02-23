{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _ResourceLimitExceededException,
    _ServiceFailureException,
    _ServiceUnavailableException,
    _ThrottledClientException,
    _UnauthorizedClientException,
    _UnprocessableEntityException,

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
    account_accountStatus,
    account_accountType,
    account_createdTimestamp,
    account_defaultLicense,
    account_signinDelegateGroups,
    account_supportedLicenses,
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

    -- * AlexaForBusinessMetadata
    AlexaForBusinessMetadata (..),
    newAlexaForBusinessMetadata,
    alexaForBusinessMetadata_alexaForBusinessRoomArn,
    alexaForBusinessMetadata_isAlexaForBusinessEnabled,

    -- * AppInstance
    AppInstance (..),
    newAppInstance,
    appInstance_appInstanceArn,
    appInstance_createdTimestamp,
    appInstance_lastUpdatedTimestamp,
    appInstance_metadata,
    appInstance_name,

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
    appInstanceSummary_appInstanceArn,
    appInstanceSummary_metadata,
    appInstanceSummary_name,

    -- * AppInstanceUser
    AppInstanceUser (..),
    newAppInstanceUser,
    appInstanceUser_appInstanceUserArn,
    appInstanceUser_createdTimestamp,
    appInstanceUser_lastUpdatedTimestamp,
    appInstanceUser_metadata,
    appInstanceUser_name,

    -- * AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (..),
    newAppInstanceUserMembershipSummary,
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

    -- * AppInstanceUserSummary
    AppInstanceUserSummary (..),
    newAppInstanceUserSummary,
    appInstanceUserSummary_appInstanceUserArn,
    appInstanceUserSummary_metadata,
    appInstanceUserSummary_name,

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
    attendee_externalUserId,
    attendee_joinToken,

    -- * AudioArtifactsConfiguration
    AudioArtifactsConfiguration (..),
    newAudioArtifactsConfiguration,
    audioArtifactsConfiguration_muxType,

    -- * BatchChannelMemberships
    BatchChannelMemberships (..),
    newBatchChannelMemberships,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_invitedBy,
    batchChannelMemberships_members,
    batchChannelMemberships_type,

    -- * BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (..),
    newBatchCreateChannelMembershipError,
    batchCreateChannelMembershipError_errorCode,
    batchCreateChannelMembershipError_errorMessage,
    batchCreateChannelMembershipError_memberArn,

    -- * Bot
    Bot (..),
    newBot,
    bot_botEmail,
    bot_botId,
    bot_botType,
    bot_createdTimestamp,
    bot_disabled,
    bot_displayName,
    bot_securityToken,
    bot_updatedTimestamp,
    bot_userId,

    -- * BusinessCallingSettings
    BusinessCallingSettings (..),
    newBusinessCallingSettings,
    businessCallingSettings_cdrBucket,

    -- * CandidateAddress
    CandidateAddress (..),
    newCandidateAddress,
    candidateAddress_city,
    candidateAddress_country,
    candidateAddress_postalCode,
    candidateAddress_postalCodePlus4,
    candidateAddress_state,
    candidateAddress_streetInfo,
    candidateAddress_streetNumber,

    -- * Channel
    Channel (..),
    newChannel,
    channel_channelArn,
    channel_createdBy,
    channel_createdTimestamp,
    channel_lastMessageTimestamp,
    channel_lastUpdatedTimestamp,
    channel_metadata,
    channel_mode,
    channel_name,
    channel_privacy,

    -- * ChannelBan
    ChannelBan (..),
    newChannelBan,
    channelBan_channelArn,
    channelBan_createdBy,
    channelBan_createdTimestamp,
    channelBan_member,

    -- * ChannelBanSummary
    ChannelBanSummary (..),
    newChannelBanSummary,
    channelBanSummary_member,

    -- * ChannelMembership
    ChannelMembership (..),
    newChannelMembership,
    channelMembership_channelArn,
    channelMembership_createdTimestamp,
    channelMembership_invitedBy,
    channelMembership_lastUpdatedTimestamp,
    channelMembership_member,
    channelMembership_type,

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

    -- * ChannelMessageSummary
    ChannelMessageSummary (..),
    newChannelMessageSummary,
    channelMessageSummary_content,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_lastEditedTimestamp,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_messageId,
    channelMessageSummary_metadata,
    channelMessageSummary_redacted,
    channelMessageSummary_sender,
    channelMessageSummary_type,

    -- * ChannelModeratedByAppInstanceUserSummary
    ChannelModeratedByAppInstanceUserSummary (..),
    newChannelModeratedByAppInstanceUserSummary,
    channelModeratedByAppInstanceUserSummary_channelSummary,

    -- * ChannelModerator
    ChannelModerator (..),
    newChannelModerator,
    channelModerator_channelArn,
    channelModerator_createdBy,
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
    channelSummary_channelArn,
    channelSummary_lastMessageTimestamp,
    channelSummary_metadata,
    channelSummary_mode,
    channelSummary_name,
    channelSummary_privacy,

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
    engineTranscribeMedicalSettings_contentIdentificationType,
    engineTranscribeMedicalSettings_region,
    engineTranscribeMedicalSettings_vocabularyName,
    engineTranscribeMedicalSettings_languageCode,
    engineTranscribeMedicalSettings_specialty,
    engineTranscribeMedicalSettings_type,

    -- * EngineTranscribeSettings
    EngineTranscribeSettings (..),
    newEngineTranscribeSettings,
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

    -- * EventsConfiguration
    EventsConfiguration (..),
    newEventsConfiguration,
    eventsConfiguration_botId,
    eventsConfiguration_lambdaFunctionArn,
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
    invite_emailAddress,
    invite_emailStatus,
    invite_inviteId,
    invite_status,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_enableMediaMetricLogs,
    loggingConfiguration_enableSIPLogs,

    -- * MediaCapturePipeline
    MediaCapturePipeline (..),
    newMediaCapturePipeline,
    mediaCapturePipeline_chimeSdkMeetingConfiguration,
    mediaCapturePipeline_createdTimestamp,
    mediaCapturePipeline_mediaPipelineId,
    mediaCapturePipeline_sinkArn,
    mediaCapturePipeline_sinkType,
    mediaCapturePipeline_sourceArn,
    mediaCapturePipeline_sourceType,
    mediaCapturePipeline_status,
    mediaCapturePipeline_updatedTimestamp,

    -- * MediaPlacement
    MediaPlacement (..),
    newMediaPlacement,
    mediaPlacement_audioFallbackUrl,
    mediaPlacement_audioHostUrl,
    mediaPlacement_eventIngestionUrl,
    mediaPlacement_screenDataUrl,
    mediaPlacement_screenSharingUrl,
    mediaPlacement_screenViewingUrl,
    mediaPlacement_signalingUrl,
    mediaPlacement_turnControlUrl,

    -- * Meeting
    Meeting (..),
    newMeeting,
    meeting_externalMeetingId,
    meeting_mediaPlacement,
    meeting_mediaRegion,
    meeting_meetingId,

    -- * MeetingNotificationConfiguration
    MeetingNotificationConfiguration (..),
    newMeetingNotificationConfiguration,
    meetingNotificationConfiguration_snsTopicArn,
    meetingNotificationConfiguration_sqsQueueArn,

    -- * Member
    Member (..),
    newMember,
    member_accountId,
    member_email,
    member_fullName,
    member_memberId,
    member_memberType,

    -- * MemberError
    MemberError (..),
    newMemberError,
    memberError_errorCode,
    memberError_errorMessage,
    memberError_memberId,

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
    originationRoute_host,
    originationRoute_port,
    originationRoute_priority,
    originationRoute_protocol,
    originationRoute_weight,

    -- * Participant
    Participant (..),
    newParticipant,
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- * PhoneNumber
    PhoneNumber (..),
    newPhoneNumber,
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

    -- * PhoneNumberAssociation
    PhoneNumberAssociation (..),
    newPhoneNumberAssociation,
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,
    phoneNumberAssociation_value,

    -- * PhoneNumberCapabilities
    PhoneNumberCapabilities (..),
    newPhoneNumberCapabilities,
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_outboundSMS,

    -- * PhoneNumberCountry
    PhoneNumberCountry (..),
    newPhoneNumberCountry,
    phoneNumberCountry_countryCode,
    phoneNumberCountry_supportedPhoneNumberTypes,

    -- * PhoneNumberError
    PhoneNumberError (..),
    newPhoneNumberError,
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,
    phoneNumberError_phoneNumberId,

    -- * PhoneNumberOrder
    PhoneNumberOrder (..),
    newPhoneNumberOrder,
    phoneNumberOrder_createdTimestamp,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_productType,
    phoneNumberOrder_status,
    phoneNumberOrder_updatedTimestamp,

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

    -- * RetentionSettings
    RetentionSettings (..),
    newRetentionSettings,
    retentionSettings_conversationRetentionSettings,
    retentionSettings_roomRetentionSettings,

    -- * Room
    Room (..),
    newRoom,
    room_accountId,
    room_createdBy,
    room_createdTimestamp,
    room_name,
    room_roomId,
    room_updatedTimestamp,

    -- * RoomMembership
    RoomMembership (..),
    newRoomMembership,
    roomMembership_invitedBy,
    roomMembership_member,
    roomMembership_role,
    roomMembership_roomId,
    roomMembership_updatedTimestamp,

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
    sipMediaApplication_awsRegion,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_endpoints,
    sipMediaApplication_name,
    sipMediaApplication_sipMediaApplicationId,
    sipMediaApplication_updatedTimestamp,

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
    sipRule_createdTimestamp,
    sipRule_disabled,
    sipRule_name,
    sipRule_sipRuleId,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_triggerValue,
    sipRule_updatedTimestamp,

    -- * SipRuleTargetApplication
    SipRuleTargetApplication (..),
    newSipRuleTargetApplication,
    sipRuleTargetApplication_awsRegion,
    sipRuleTargetApplication_priority,
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
    termination_callingRegions,
    termination_cidrAllowedList,
    termination_cpsLimit,
    termination_defaultPhoneNumber,
    termination_disabled,

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
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- * UpdateUserRequestItem
    UpdateUserRequestItem (..),
    newUpdateUserRequestItem,
    updateUserRequestItem_alexaForBusinessMetadata,
    updateUserRequestItem_licenseType,
    updateUserRequestItem_userType,
    updateUserRequestItem_userId,

    -- * User
    User (..),
    newUser,
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

    -- * UserError
    UserError (..),
    newUserError,
    userError_errorCode,
    userError_errorMessage,
    userError_userId,

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
    voiceConnector_awsRegion,
    voiceConnector_createdTimestamp,
    voiceConnector_name,
    voiceConnector_outboundHostName,
    voiceConnector_requireEncryption,
    voiceConnector_updatedTimestamp,
    voiceConnector_voiceConnectorArn,
    voiceConnector_voiceConnectorId,

    -- * VoiceConnectorGroup
    VoiceConnectorGroup (..),
    newVoiceConnectorGroup,
    voiceConnectorGroup_createdTimestamp,
    voiceConnectorGroup_name,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorGroupArn,
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have permissions to perform the requested operation.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The input parameters don\'t match the service\'s restrictions.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The client is permanently forbidden from making the request.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | One or more of the resources in the request does not exist in the
-- system.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request exceeds the resource limit.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The service encountered an unexpected error.
_ServiceFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | The service is currently unavailable.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The client exceeded its request rate limit.
_ThrottledClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledClientException =
  Core._MatchServiceError
    defaultService
    "ThrottledClientException"
    Prelude.. Core.hasStatus 429

-- | The client is not currently authorized to make the request.
_UnauthorizedClientException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedClientException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedClientException"
    Prelude.. Core.hasStatus 401

-- | The request was well-formed but was unable to be followed due to
-- semantic errors.
_UnprocessableEntityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

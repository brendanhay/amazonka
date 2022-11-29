{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKMessaging.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ThrottledClientException,
    _NotFoundException,
    _ServiceUnavailableException,
    _ResourceLimitExceededException,
    _ForbiddenException,
    _ConflictException,
    _BadRequestException,
    _UnauthorizedClientException,
    _ServiceFailureException,

    -- * AllowNotifications
    AllowNotifications (..),

    -- * ChannelMembershipType
    ChannelMembershipType (..),

    -- * ChannelMessagePersistenceType
    ChannelMessagePersistenceType (..),

    -- * ChannelMessageStatus
    ChannelMessageStatus (..),

    -- * ChannelMessageType
    ChannelMessageType (..),

    -- * ChannelMode
    ChannelMode (..),

    -- * ChannelPrivacy
    ChannelPrivacy (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * FallbackAction
    FallbackAction (..),

    -- * InvocationType
    InvocationType (..),

    -- * PushNotificationType
    PushNotificationType (..),

    -- * SearchFieldKey
    SearchFieldKey (..),

    -- * SearchFieldOperator
    SearchFieldOperator (..),

    -- * SortOrder
    SortOrder (..),

    -- * AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (..),
    newAppInstanceUserMembershipSummary,
    appInstanceUserMembershipSummary_type,
    appInstanceUserMembershipSummary_subChannelId,
    appInstanceUserMembershipSummary_readMarkerTimestamp,

    -- * BatchChannelMemberships
    BatchChannelMemberships (..),
    newBatchChannelMemberships,
    batchChannelMemberships_type,
    batchChannelMemberships_members,
    batchChannelMemberships_subChannelId,
    batchChannelMemberships_channelArn,
    batchChannelMemberships_invitedBy,

    -- * BatchCreateChannelMembershipError
    BatchCreateChannelMembershipError (..),
    newBatchCreateChannelMembershipError,
    batchCreateChannelMembershipError_memberArn,
    batchCreateChannelMembershipError_errorMessage,
    batchCreateChannelMembershipError_errorCode,

    -- * Channel
    Channel (..),
    newChannel,
    channel_lastUpdatedTimestamp,
    channel_lastMessageTimestamp,
    channel_name,
    channel_elasticChannelConfiguration,
    channel_metadata,
    channel_createdTimestamp,
    channel_channelArn,
    channel_privacy,
    channel_mode,
    channel_createdBy,
    channel_channelFlowArn,

    -- * ChannelAssociatedWithFlowSummary
    ChannelAssociatedWithFlowSummary (..),
    newChannelAssociatedWithFlowSummary,
    channelAssociatedWithFlowSummary_name,
    channelAssociatedWithFlowSummary_metadata,
    channelAssociatedWithFlowSummary_channelArn,
    channelAssociatedWithFlowSummary_privacy,
    channelAssociatedWithFlowSummary_mode,

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

    -- * ChannelFlow
    ChannelFlow (..),
    newChannelFlow,
    channelFlow_lastUpdatedTimestamp,
    channelFlow_name,
    channelFlow_createdTimestamp,
    channelFlow_processors,
    channelFlow_channelFlowArn,

    -- * ChannelFlowSummary
    ChannelFlowSummary (..),
    newChannelFlowSummary,
    channelFlowSummary_name,
    channelFlowSummary_processors,
    channelFlowSummary_channelFlowArn,

    -- * ChannelMembership
    ChannelMembership (..),
    newChannelMembership,
    channelMembership_lastUpdatedTimestamp,
    channelMembership_member,
    channelMembership_type,
    channelMembership_createdTimestamp,
    channelMembership_subChannelId,
    channelMembership_channelArn,
    channelMembership_invitedBy,

    -- * ChannelMembershipForAppInstanceUserSummary
    ChannelMembershipForAppInstanceUserSummary (..),
    newChannelMembershipForAppInstanceUserSummary,
    channelMembershipForAppInstanceUserSummary_channelSummary,
    channelMembershipForAppInstanceUserSummary_appInstanceUserMembershipSummary,

    -- * ChannelMembershipPreferences
    ChannelMembershipPreferences (..),
    newChannelMembershipPreferences,
    channelMembershipPreferences_pushNotifications,

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
    channelMessage_subChannelId,
    channelMessage_redacted,
    channelMessage_channelArn,
    channelMessage_messageId,
    channelMessage_status,
    channelMessage_messageAttributes,
    channelMessage_lastEditedTimestamp,
    channelMessage_sender,
    channelMessage_persistence,
    channelMessage_content,

    -- * ChannelMessageCallback
    ChannelMessageCallback (..),
    newChannelMessageCallback,
    channelMessageCallback_metadata,
    channelMessageCallback_subChannelId,
    channelMessageCallback_messageAttributes,
    channelMessageCallback_pushNotification,
    channelMessageCallback_content,
    channelMessageCallback_messageId,

    -- * ChannelMessageStatusStructure
    ChannelMessageStatusStructure (..),
    newChannelMessageStatusStructure,
    channelMessageStatusStructure_detail,
    channelMessageStatusStructure_value,

    -- * ChannelMessageSummary
    ChannelMessageSummary (..),
    newChannelMessageSummary,
    channelMessageSummary_lastUpdatedTimestamp,
    channelMessageSummary_type,
    channelMessageSummary_metadata,
    channelMessageSummary_createdTimestamp,
    channelMessageSummary_redacted,
    channelMessageSummary_messageId,
    channelMessageSummary_status,
    channelMessageSummary_messageAttributes,
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

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_mode,

    -- * ElasticChannelConfiguration
    ElasticChannelConfiguration (..),
    newElasticChannelConfiguration,
    elasticChannelConfiguration_maximumSubChannels,
    elasticChannelConfiguration_targetMembershipsPerSubChannel,
    elasticChannelConfiguration_minimumMembershipPercentage,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_name,
    identity_arn,

    -- * LambdaConfiguration
    LambdaConfiguration (..),
    newLambdaConfiguration,
    lambdaConfiguration_resourceArn,
    lambdaConfiguration_invocationType,

    -- * MessageAttributeValue
    MessageAttributeValue (..),
    newMessageAttributeValue,
    messageAttributeValue_stringValues,

    -- * MessagingSessionEndpoint
    MessagingSessionEndpoint (..),
    newMessagingSessionEndpoint,
    messagingSessionEndpoint_url,

    -- * Processor
    Processor (..),
    newProcessor,
    processor_name,
    processor_configuration,
    processor_executionOrder,
    processor_fallbackAction,

    -- * ProcessorConfiguration
    ProcessorConfiguration (..),
    newProcessorConfiguration,
    processorConfiguration_lambda,

    -- * PushNotificationConfiguration
    PushNotificationConfiguration (..),
    newPushNotificationConfiguration,
    pushNotificationConfiguration_type,
    pushNotificationConfiguration_body,
    pushNotificationConfiguration_title,

    -- * PushNotificationPreferences
    PushNotificationPreferences (..),
    newPushNotificationPreferences,
    pushNotificationPreferences_filterRule,
    pushNotificationPreferences_allowNotifications,

    -- * SearchField
    SearchField (..),
    newSearchField,
    searchField_key,
    searchField_values,
    searchField_operator,

    -- * SubChannelSummary
    SubChannelSummary (..),
    newSubChannelSummary,
    subChannelSummary_membershipCount,
    subChannelSummary_subChannelId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Amazonka.ChimeSDKMessaging.Types.AllowNotifications
import Amazonka.ChimeSDKMessaging.Types.AppInstanceUserMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.BatchChannelMemberships
import Amazonka.ChimeSDKMessaging.Types.BatchCreateChannelMembershipError
import Amazonka.ChimeSDKMessaging.Types.Channel
import Amazonka.ChimeSDKMessaging.Types.ChannelAssociatedWithFlowSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelBan
import Amazonka.ChimeSDKMessaging.Types.ChannelBanSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelFlow
import Amazonka.ChimeSDKMessaging.Types.ChannelFlowSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMembership
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipForAppInstanceUserSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipPreferences
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMembershipType
import Amazonka.ChimeSDKMessaging.Types.ChannelMessage
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageCallback
import Amazonka.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatus
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelMessageType
import Amazonka.ChimeSDKMessaging.Types.ChannelMode
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratedByAppInstanceUserSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelModerator
import Amazonka.ChimeSDKMessaging.Types.ChannelModeratorSummary
import Amazonka.ChimeSDKMessaging.Types.ChannelPrivacy
import Amazonka.ChimeSDKMessaging.Types.ChannelSummary
import Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration
import Amazonka.ChimeSDKMessaging.Types.ErrorCode
import Amazonka.ChimeSDKMessaging.Types.FallbackAction
import Amazonka.ChimeSDKMessaging.Types.Identity
import Amazonka.ChimeSDKMessaging.Types.InvocationType
import Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
import Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue
import Amazonka.ChimeSDKMessaging.Types.MessagingSessionEndpoint
import Amazonka.ChimeSDKMessaging.Types.Processor
import Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
import Amazonka.ChimeSDKMessaging.Types.PushNotificationConfiguration
import Amazonka.ChimeSDKMessaging.Types.PushNotificationPreferences
import Amazonka.ChimeSDKMessaging.Types.PushNotificationType
import Amazonka.ChimeSDKMessaging.Types.SearchField
import Amazonka.ChimeSDKMessaging.Types.SearchFieldKey
import Amazonka.ChimeSDKMessaging.Types.SearchFieldOperator
import Amazonka.ChimeSDKMessaging.Types.SortOrder
import Amazonka.ChimeSDKMessaging.Types.SubChannelSummary
import Amazonka.ChimeSDKMessaging.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-05-15@ of the Amazon Chime SDK Messaging SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ChimeSDKMessaging",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "messaging-chime",
      Core.signingName = "chime",
      Core.version = "2021-05-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ChimeSDKMessaging",
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

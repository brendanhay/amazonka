{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKMessaging.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ThrottledClientException,
    _ResourceLimitExceededException,
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _ServiceFailureException,
    _UnauthorizedClientException,
    _ServiceUnavailableException,
    _BadRequestException,

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

    -- * SortOrder
    SortOrder (..),

    -- * AppInstanceUserMembershipSummary
    AppInstanceUserMembershipSummary (..),
    newAppInstanceUserMembershipSummary,
    appInstanceUserMembershipSummary_readMarkerTimestamp,
    appInstanceUserMembershipSummary_type,

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

    -- * Channel
    Channel (..),
    newChannel,
    channel_mode,
    channel_createdBy,
    channel_channelArn,
    channel_privacy,
    channel_channelFlowArn,
    channel_lastMessageTimestamp,
    channel_name,
    channel_metadata,
    channel_createdTimestamp,
    channel_lastUpdatedTimestamp,

    -- * ChannelAssociatedWithFlowSummary
    ChannelAssociatedWithFlowSummary (..),
    newChannelAssociatedWithFlowSummary,
    channelAssociatedWithFlowSummary_mode,
    channelAssociatedWithFlowSummary_channelArn,
    channelAssociatedWithFlowSummary_privacy,
    channelAssociatedWithFlowSummary_name,
    channelAssociatedWithFlowSummary_metadata,

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

    -- * ChannelFlow
    ChannelFlow (..),
    newChannelFlow,
    channelFlow_processors,
    channelFlow_channelFlowArn,
    channelFlow_name,
    channelFlow_createdTimestamp,
    channelFlow_lastUpdatedTimestamp,

    -- * ChannelFlowSummary
    ChannelFlowSummary (..),
    newChannelFlowSummary,
    channelFlowSummary_processors,
    channelFlowSummary_channelFlowArn,
    channelFlowSummary_name,

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
    channelMessage_status,
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

    -- * ChannelMessageCallback
    ChannelMessageCallback (..),
    newChannelMessageCallback,
    channelMessageCallback_content,
    channelMessageCallback_metadata,
    channelMessageCallback_messageId,

    -- * ChannelMessageStatusStructure
    ChannelMessageStatusStructure (..),
    newChannelMessageStatusStructure,
    channelMessageStatusStructure_value,
    channelMessageStatusStructure_detail,

    -- * ChannelMessageSummary
    ChannelMessageSummary (..),
    newChannelMessageSummary,
    channelMessageSummary_status,
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

    -- * ChannelSummary
    ChannelSummary (..),
    newChannelSummary,
    channelSummary_mode,
    channelSummary_channelArn,
    channelSummary_privacy,
    channelSummary_lastMessageTimestamp,
    channelSummary_name,
    channelSummary_metadata,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_arn,
    identity_name,

    -- * LambdaConfiguration
    LambdaConfiguration (..),
    newLambdaConfiguration,
    lambdaConfiguration_resourceArn,
    lambdaConfiguration_invocationType,

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

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

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
import Amazonka.ChimeSDKMessaging.Types.ErrorCode
import Amazonka.ChimeSDKMessaging.Types.FallbackAction
import Amazonka.ChimeSDKMessaging.Types.Identity
import Amazonka.ChimeSDKMessaging.Types.InvocationType
import Amazonka.ChimeSDKMessaging.Types.LambdaConfiguration
import Amazonka.ChimeSDKMessaging.Types.MessagingSessionEndpoint
import Amazonka.ChimeSDKMessaging.Types.Processor
import Amazonka.ChimeSDKMessaging.Types.ProcessorConfiguration
import Amazonka.ChimeSDKMessaging.Types.SortOrder
import Amazonka.ChimeSDKMessaging.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-05-15@ of the Amazon Chime SDK Messaging SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ChimeSDKMessaging",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "messaging-chime",
      Core._serviceSigningName = "chime",
      Core._serviceVersion = "2021-05-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ChimeSDKMessaging",
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

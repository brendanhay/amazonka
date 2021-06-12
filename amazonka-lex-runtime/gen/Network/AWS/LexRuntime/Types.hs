{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _UnsupportedMediaTypeException,
    _BadGatewayException,
    _NotAcceptableException,
    _LimitExceededException,
    _ConflictException,
    _DependencyFailedException,
    _InternalFailureException,
    _LoopDetectedException,
    _RequestTimeoutException,

    -- * ConfirmationStatus
    ConfirmationStatus (..),

    -- * ContentType
    ContentType (..),

    -- * DialogActionType
    DialogActionType (..),

    -- * DialogState
    DialogState (..),

    -- * FulfillmentState
    FulfillmentState (..),

    -- * MessageFormatType
    MessageFormatType (..),

    -- * ActiveContext
    ActiveContext (..),
    newActiveContext,
    activeContext_name,
    activeContext_timeToLive,
    activeContext_parameters,

    -- * ActiveContextTimeToLive
    ActiveContextTimeToLive (..),
    newActiveContextTimeToLive,
    activeContextTimeToLive_timeToLiveInSeconds,
    activeContextTimeToLive_turnsToLive,

    -- * Button
    Button (..),
    newButton,
    button_text,
    button_value,

    -- * DialogAction
    DialogAction (..),
    newDialogAction,
    dialogAction_message,
    dialogAction_intentName,
    dialogAction_messageFormat,
    dialogAction_fulfillmentState,
    dialogAction_slots,
    dialogAction_slotToElicit,
    dialogAction_type,

    -- * GenericAttachment
    GenericAttachment (..),
    newGenericAttachment,
    genericAttachment_title,
    genericAttachment_buttons,
    genericAttachment_attachmentLinkUrl,
    genericAttachment_imageUrl,
    genericAttachment_subTitle,

    -- * IntentConfidence
    IntentConfidence (..),
    newIntentConfidence,
    intentConfidence_score,

    -- * IntentSummary
    IntentSummary (..),
    newIntentSummary,
    intentSummary_intentName,
    intentSummary_fulfillmentState,
    intentSummary_slots,
    intentSummary_checkpointLabel,
    intentSummary_slotToElicit,
    intentSummary_confirmationStatus,
    intentSummary_dialogActionType,

    -- * PredictedIntent
    PredictedIntent (..),
    newPredictedIntent,
    predictedIntent_intentName,
    predictedIntent_slots,
    predictedIntent_nluIntentConfidence,

    -- * ResponseCard
    ResponseCard (..),
    newResponseCard,
    responseCard_contentType,
    responseCard_genericAttachments,
    responseCard_version,

    -- * SentimentResponse
    SentimentResponse (..),
    newSentimentResponse,
    sentimentResponse_sentimentScore,
    sentimentResponse_sentimentLabel,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ActiveContext
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import Network.AWS.LexRuntime.Types.Button
import Network.AWS.LexRuntime.Types.ConfirmationStatus
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.DialogAction
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.DialogState
import Network.AWS.LexRuntime.Types.FulfillmentState
import Network.AWS.LexRuntime.Types.GenericAttachment
import Network.AWS.LexRuntime.Types.IntentConfidence
import Network.AWS.LexRuntime.Types.IntentSummary
import Network.AWS.LexRuntime.Types.MessageFormatType
import Network.AWS.LexRuntime.Types.PredictedIntent
import Network.AWS.LexRuntime.Types.ResponseCard
import Network.AWS.LexRuntime.Types.SentimentResponse
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LexRuntime",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "runtime.lex",
      Core._serviceSigningName = "lex",
      Core._serviceVersion = "2016-11-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LexRuntime",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The resource (such as the Amazon Lex bot or an alias) that is referred
-- to is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | Request validation failed, there is no usable message in the context, or
-- the bot build failed, is still in progress, or contains unbuilt changes.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | The Content-Type header (@PostContent@ API) has an invalid value.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Core.. Core.hasStatus 415

-- | Either the Amazon Lex bot is still building, or one of the dependent
-- services (Amazon Polly, AWS Lambda) failed with an internal service
-- error.
_BadGatewayException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError
    defaultService
    "BadGatewayException"
    Core.. Core.hasStatus 502

-- | The accept header in the request does not have a valid value.
_NotAcceptableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAcceptableException =
  Core._MatchServiceError
    defaultService
    "NotAcceptableException"
    Core.. Core.hasStatus 406

-- | Exceeded a limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Core.. Core.hasStatus 429

-- | Two clients are using the same AWS account, Amazon Lex bot, and user ID.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Core.. Core.hasStatus 409

-- | One of the dependencies, such as AWS Lambda or Amazon Polly, threw an
-- exception. For example,
--
-- -   If Amazon Lex does not have sufficient permissions to call a Lambda
--     function.
--
-- -   If a Lambda function takes longer than 30 seconds to execute.
--
-- -   If a fulfillment Lambda function returns a @Delegate@ dialog action
--     without removing any slot values.
_DependencyFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyFailedException =
  Core._MatchServiceError
    defaultService
    "DependencyFailedException"
    Core.. Core.hasStatus 424

-- | Internal service error. Retry the call.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Core.. Core.hasStatus 500

-- | This exception is not used.
_LoopDetectedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LoopDetectedException =
  Core._MatchServiceError
    defaultService
    "LoopDetectedException"
    Core.. Core.hasStatus 508

-- | The input speech is too long.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Core.. Core.hasStatus 408

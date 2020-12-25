-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _NotAcceptableException,
    _DependencyFailedException,
    _UnsupportedMediaTypeException,
    _ConflictException,
    _NotFoundException,
    _RequestTimeoutException,
    _LoopDetectedException,
    _InternalFailureException,
    _BadGatewayException,
    _BadRequestException,
    _LimitExceededException,

    -- * ActiveContextName
    ActiveContextName (..),

    -- * SentimentResponse
    SentimentResponse (..),
    mkSentimentResponse,
    srSentimentLabel,
    srSentimentScore,

    -- * BotAlias
    BotAlias (..),

    -- * ResponseCard
    ResponseCard (..),
    mkResponseCard,
    rcContentType,
    rcGenericAttachments,
    rcVersion,

    -- * String
    String (..),

    -- * Text
    Text (..),

    -- * IntentConfidence
    IntentConfidence (..),
    mkIntentConfidence,
    icScore,

    -- * MessageFormatType
    MessageFormatType (..),

    -- * Accept
    Accept (..),

    -- * StringWithLength
    StringWithLength (..),

    -- * BotVersion
    BotVersion (..),

    -- * IntentName
    IntentName (..),

    -- * DialogActionType
    DialogActionType (..),

    -- * ButtonValueStringWithLength
    ButtonValueStringWithLength (..),

    -- * BotName
    BotName (..),

    -- * Button
    Button (..),
    mkButton,
    bText,
    bValue,

    -- * HttpContentType
    HttpContentType (..),

    -- * UserId
    UserId (..),

    -- * IntentSummaryCheckpointLabel
    IntentSummaryCheckpointLabel (..),

    -- * SentimentScore
    SentimentScore (..),

    -- * DialogState
    DialogState (..),

    -- * FulfillmentState
    FulfillmentState (..),

    -- * PredictedIntent
    PredictedIntent (..),
    mkPredictedIntent,
    piIntentName,
    piNluIntentConfidence,
    piSlots,

    -- * GenericAttachment
    GenericAttachment (..),
    mkGenericAttachment,
    gaAttachmentLinkUrl,
    gaButtons,
    gaImageUrl,
    gaSubTitle,
    gaTitle,

    -- * IntentSummary
    IntentSummary (..),
    mkIntentSummary,
    isDialogActionType,
    isCheckpointLabel,
    isConfirmationStatus,
    isFulfillmentState,
    isIntentName,
    isSlotToElicit,
    isSlots,

    -- * ConfirmationStatus
    ConfirmationStatus (..),

    -- * ActiveContext
    ActiveContext (..),
    mkActiveContext,
    acName,
    acTimeToLive,
    acParameters,

    -- * ActiveContextTimeToLive
    ActiveContextTimeToLive (..),
    mkActiveContextTimeToLive,
    acttlTimeToLiveInSeconds,
    acttlTurnsToLive,

    -- * ParameterName
    ParameterName (..),

    -- * DialogAction
    DialogAction (..),
    mkDialogAction,
    daType,
    daFulfillmentState,
    daIntentName,
    daMessage,
    daMessageFormat,
    daSlotToElicit,
    daSlots,

    -- * SentimentLabel
    SentimentLabel (..),

    -- * ContentType
    ContentType (..),

    -- * ActiveContexts
    ActiveContexts (..),

    -- * Message
    Message (..),

    -- * SessionAttributes
    SessionAttributes (..),

    -- * SessionId
    SessionId (..),

    -- * SlotToElicit
    SlotToElicit (..),

    -- * Slots
    Slots (..),

    -- * InputText
    InputText (..),

    -- * Version
    Version (..),

    -- * AlternativeIntents
    AlternativeIntents (..),

    -- * InputTranscript
    InputTranscript (..),

    -- * NluIntentConfidence
    NluIntentConfidence (..),

    -- * RequestAttributes
    RequestAttributes (..),

    -- * CheckpointLabelFilter
    CheckpointLabelFilter (..),

    -- * AttachmentLinkUrl
    AttachmentLinkUrl (..),

    -- * ImageUrl
    ImageUrl (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.Accept
import Network.AWS.LexRuntime.Types.ActiveContext
import Network.AWS.LexRuntime.Types.ActiveContextName
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import Network.AWS.LexRuntime.Types.ActiveContexts
import Network.AWS.LexRuntime.Types.AlternativeIntents
import Network.AWS.LexRuntime.Types.AttachmentLinkUrl
import Network.AWS.LexRuntime.Types.BotAlias
import Network.AWS.LexRuntime.Types.BotName
import Network.AWS.LexRuntime.Types.BotVersion
import Network.AWS.LexRuntime.Types.Button
import Network.AWS.LexRuntime.Types.ButtonValueStringWithLength
import Network.AWS.LexRuntime.Types.CheckpointLabelFilter
import Network.AWS.LexRuntime.Types.ConfirmationStatus
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.DialogAction
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.DialogState
import Network.AWS.LexRuntime.Types.FulfillmentState
import Network.AWS.LexRuntime.Types.GenericAttachment
import Network.AWS.LexRuntime.Types.HttpContentType
import Network.AWS.LexRuntime.Types.ImageUrl
import Network.AWS.LexRuntime.Types.InputText
import Network.AWS.LexRuntime.Types.InputTranscript
import Network.AWS.LexRuntime.Types.IntentConfidence
import Network.AWS.LexRuntime.Types.IntentName
import Network.AWS.LexRuntime.Types.IntentSummary
import Network.AWS.LexRuntime.Types.IntentSummaryCheckpointLabel
import Network.AWS.LexRuntime.Types.Message
import Network.AWS.LexRuntime.Types.MessageFormatType
import Network.AWS.LexRuntime.Types.NluIntentConfidence
import Network.AWS.LexRuntime.Types.ParameterName
import Network.AWS.LexRuntime.Types.PredictedIntent
import Network.AWS.LexRuntime.Types.RequestAttributes
import Network.AWS.LexRuntime.Types.ResponseCard
import Network.AWS.LexRuntime.Types.SentimentLabel
import Network.AWS.LexRuntime.Types.SentimentResponse
import Network.AWS.LexRuntime.Types.SentimentScore
import Network.AWS.LexRuntime.Types.SessionAttributes
import Network.AWS.LexRuntime.Types.SessionId
import Network.AWS.LexRuntime.Types.SlotToElicit
import Network.AWS.LexRuntime.Types.Slots
import Network.AWS.LexRuntime.Types.String
import Network.AWS.LexRuntime.Types.StringWithLength
import Network.AWS.LexRuntime.Types.Text
import Network.AWS.LexRuntime.Types.UserId
import Network.AWS.LexRuntime.Types.Version
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "LexRuntime",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "runtime.lex",
      Core._svcVersion = "2016-11-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "LexRuntime",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The accept header in the request does not have a valid value.
_NotAcceptableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAcceptableException =
  Core._MatchServiceError mkServiceConfig "NotAcceptableException"
    Core.. Core.hasStatues 406
{-# DEPRECATED _NotAcceptableException "Use generic-lens or generic-optics instead." #-}

-- | One of the dependencies, such as AWS Lambda or Amazon Polly, threw an exception. For example,
--
--
--     * If Amazon Lex does not have sufficient permissions to call a Lambda function.
--
--
--     * If a Lambda function takes longer than 30 seconds to execute.
--
--
--     * If a fulfillment Lambda function returns a @Delegate@ dialog action without removing any slot values.
_DependencyFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DependencyFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "DependencyFailedException"
    Core.. Core.hasStatues 424
{-# DEPRECATED _DependencyFailedException "Use generic-lens or generic-optics instead." #-}

-- | The Content-Type header (@PostContent@ API) has an invalid value.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedMediaTypeException"
    Core.. Core.hasStatues 415
{-# DEPRECATED _UnsupportedMediaTypeException "Use generic-lens or generic-optics instead." #-}

-- | Two clients are using the same AWS account, Amazon Lex bot, and user ID.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | The resource (such as the Amazon Lex bot or an alias) that is referred to is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The input speech is too long.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError mkServiceConfig "RequestTimeoutException"
    Core.. Core.hasStatues 408
{-# DEPRECATED _RequestTimeoutException "Use generic-lens or generic-optics instead." #-}

-- | This exception is not used.
_LoopDetectedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LoopDetectedException =
  Core._MatchServiceError mkServiceConfig "LoopDetectedException"
    Core.. Core.hasStatues 508
{-# DEPRECATED _LoopDetectedException "Use generic-lens or generic-optics instead." #-}

-- | Internal service error. Retry the call.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | Either the Amazon Lex bot is still building, or one of the dependent services (Amazon Polly, AWS Lambda) failed with an internal service error.
_BadGatewayException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError mkServiceConfig "BadGatewayException"
    Core.. Core.hasStatues 502
{-# DEPRECATED _BadGatewayException "Use generic-lens or generic-optics instead." #-}

-- | Request validation failed, there is no usable message in the context, or the bot build failed, is still in progress, or contains unbuilt changes.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}

-- | Exceeded a limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

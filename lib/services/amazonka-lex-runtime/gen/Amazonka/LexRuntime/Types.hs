{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexRuntime.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnsupportedMediaTypeException,
    _NotFoundException,
    _DependencyFailedException,
    _LimitExceededException,
    _LoopDetectedException,
    _ConflictException,
    _BadGatewayException,
    _BadRequestException,
    _RequestTimeoutException,
    _NotAcceptableException,
    _InternalFailureException,

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
    dialogAction_fulfillmentState,
    dialogAction_message,
    dialogAction_slotToElicit,
    dialogAction_messageFormat,
    dialogAction_intentName,
    dialogAction_slots,
    dialogAction_type,

    -- * GenericAttachment
    GenericAttachment (..),
    newGenericAttachment,
    genericAttachment_imageUrl,
    genericAttachment_subTitle,
    genericAttachment_buttons,
    genericAttachment_title,
    genericAttachment_attachmentLinkUrl,

    -- * IntentConfidence
    IntentConfidence (..),
    newIntentConfidence,
    intentConfidence_score,

    -- * IntentSummary
    IntentSummary (..),
    newIntentSummary,
    intentSummary_fulfillmentState,
    intentSummary_confirmationStatus,
    intentSummary_checkpointLabel,
    intentSummary_slotToElicit,
    intentSummary_intentName,
    intentSummary_slots,
    intentSummary_dialogActionType,

    -- * PredictedIntent
    PredictedIntent (..),
    newPredictedIntent,
    predictedIntent_intentName,
    predictedIntent_nluIntentConfidence,
    predictedIntent_slots,

    -- * ResponseCard
    ResponseCard (..),
    newResponseCard,
    responseCard_genericAttachments,
    responseCard_version,
    responseCard_contentType,

    -- * SentimentResponse
    SentimentResponse (..),
    newSentimentResponse,
    sentimentResponse_sentimentLabel,
    sentimentResponse_sentimentScore,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexRuntime.Types.ActiveContext
import Amazonka.LexRuntime.Types.ActiveContextTimeToLive
import Amazonka.LexRuntime.Types.Button
import Amazonka.LexRuntime.Types.ConfirmationStatus
import Amazonka.LexRuntime.Types.ContentType
import Amazonka.LexRuntime.Types.DialogAction
import Amazonka.LexRuntime.Types.DialogActionType
import Amazonka.LexRuntime.Types.DialogState
import Amazonka.LexRuntime.Types.FulfillmentState
import Amazonka.LexRuntime.Types.GenericAttachment
import Amazonka.LexRuntime.Types.IntentConfidence
import Amazonka.LexRuntime.Types.IntentSummary
import Amazonka.LexRuntime.Types.MessageFormatType
import Amazonka.LexRuntime.Types.PredictedIntent
import Amazonka.LexRuntime.Types.ResponseCard
import Amazonka.LexRuntime.Types.SentimentResponse
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LexRuntime",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "runtime.lex",
      Core.signingName = "lex",
      Core.version = "2016-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LexRuntime",
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

-- | The Content-Type header (@PostContent@ API) has an invalid value.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Prelude.. Core.hasStatus 415

-- | The resource (such as the Amazon Lex bot or an alias) that is referred
-- to is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

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
_DependencyFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyFailedException =
  Core._MatchServiceError
    defaultService
    "DependencyFailedException"
    Prelude.. Core.hasStatus 424

-- | Exceeded a limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | This exception is not used.
_LoopDetectedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LoopDetectedException =
  Core._MatchServiceError
    defaultService
    "LoopDetectedException"
    Prelude.. Core.hasStatus 508

-- | Two clients are using the same AWS account, Amazon Lex bot, and user ID.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Either the Amazon Lex bot is still building, or one of the dependent
-- services (Amazon Polly, AWS Lambda) failed with an internal service
-- error.
_BadGatewayException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError
    defaultService
    "BadGatewayException"
    Prelude.. Core.hasStatus 502

-- | Request validation failed, there is no usable message in the context, or
-- the bot build failed, is still in progress, or contains unbuilt changes.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The input speech is too long.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Core.hasStatus 408

-- | The accept header in the request does not have a valid value.
_NotAcceptableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAcceptableException =
  Core._MatchServiceError
    defaultService
    "NotAcceptableException"
    Prelude.. Core.hasStatus 406

-- | Internal service error. Retry the call.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

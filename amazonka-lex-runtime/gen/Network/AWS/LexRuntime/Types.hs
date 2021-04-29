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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "LexRuntime",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "runtime.lex",
      Prelude._svcVersion = "2016-11-28",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "LexRuntime",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource (such as the Amazon Lex bot or an alias) that is referred
-- to is not found.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | Request validation failed, there is no usable message in the context, or
-- the bot build failed, is still in progress, or contains unbuilt changes.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The Content-Type header (@PostContent@ API) has an invalid value.
_UnsupportedMediaTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedMediaTypeException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedMediaTypeException"
    Prelude.. Prelude.hasStatus 415

-- | Either the Amazon Lex bot is still building, or one of the dependent
-- services (Amazon Polly, AWS Lambda) failed with an internal service
-- error.
_BadGatewayException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadGatewayException =
  Prelude._MatchServiceError
    defaultService
    "BadGatewayException"
    Prelude.. Prelude.hasStatus 502

-- | The accept header in the request does not have a valid value.
_NotAcceptableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAcceptableException =
  Prelude._MatchServiceError
    defaultService
    "NotAcceptableException"
    Prelude.. Prelude.hasStatus 406

-- | Exceeded a limit.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 429

-- | Two clients are using the same AWS account, Amazon Lex bot, and user ID.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Prelude.hasStatus 409

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
_DependencyFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DependencyFailedException =
  Prelude._MatchServiceError
    defaultService
    "DependencyFailedException"
    Prelude.. Prelude.hasStatus 424

-- | Internal service error. Retry the call.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Prelude.hasStatus 500

-- | This exception is not used.
_LoopDetectedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LoopDetectedException =
  Prelude._MatchServiceError
    defaultService
    "LoopDetectedException"
    Prelude.. Prelude.hasStatus 508

-- | The input speech is too long.
_RequestTimeoutException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestTimeoutException =
  Prelude._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Prelude.hasStatus 408

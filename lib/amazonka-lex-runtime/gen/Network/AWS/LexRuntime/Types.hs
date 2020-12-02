{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types
  ( -- * Service Configuration
    lexRuntime,

    -- * Errors

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
    ActiveContext,
    activeContext,
    acName,
    acTimeToLive,
    acParameters,

    -- * ActiveContextTimeToLive
    ActiveContextTimeToLive,
    activeContextTimeToLive,
    acttlTurnsToLive,
    acttlTimeToLiveInSeconds,

    -- * Button
    Button,
    button,
    bText,
    bValue,

    -- * DialogAction
    DialogAction,
    dialogAction,
    daSlots,
    daIntentName,
    daFulfillmentState,
    daMessageFormat,
    daMessage,
    daSlotToElicit,
    daType,

    -- * GenericAttachment
    GenericAttachment,
    genericAttachment,
    gaButtons,
    gaSubTitle,
    gaImageURL,
    gaAttachmentLinkURL,
    gaTitle,

    -- * IntentConfidence
    IntentConfidence,
    intentConfidence,
    icScore,

    -- * IntentSummary
    IntentSummary,
    intentSummary,
    isCheckpointLabel,
    isSlots,
    isIntentName,
    isFulfillmentState,
    isConfirmationStatus,
    isSlotToElicit,
    isDialogActionType,

    -- * PredictedIntent
    PredictedIntent,
    predictedIntent,
    piNluIntentConfidence,
    piSlots,
    piIntentName,

    -- * ResponseCard
    ResponseCard,
    responseCard,
    rcGenericAttachments,
    rcVersion,
    rcContentType,

    -- * SentimentResponse
    SentimentResponse,
    sentimentResponse,
    sSentimentScore,
    sSentimentLabel,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
lexRuntime :: Service
lexRuntime =
  Service
    { _svcAbbrev = "LexRuntime",
      _svcSigner = v4,
      _svcPrefix = "runtime.lex",
      _svcVersion = "2016-11-28",
      _svcEndpoint = defaultEndpoint lexRuntime,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "LexRuntime",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

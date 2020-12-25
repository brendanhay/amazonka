{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lex provides both build and runtime endpoints. Each endpoint provides a set of operations (API). Your conversational bot uses the runtime API to understand user utterances (user input text or voice). For example, suppose a user says "I want pizza", your bot sends this input to Amazon Lex using the runtime API. Amazon Lex recognizes that the user request is for the OrderPizza intent (one of the intents defined in the bot). Then Amazon Lex engages in user conversation on behalf of the bot to elicit required information (slot values, such as pizza size and crust type), and then performs fulfillment activity (that you configured when you created the bot). You use the build-time API to create and manage your Amazon Lex bot. For a list of build-time operations, see the build-time API, .
module Network.AWS.LexRuntime
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** NotAcceptableException
    _NotAcceptableException,

    -- ** DependencyFailedException
    _DependencyFailedException,

    -- ** UnsupportedMediaTypeException
    _UnsupportedMediaTypeException,

    -- ** ConflictException
    _ConflictException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** RequestTimeoutException
    _RequestTimeoutException,

    -- ** LoopDetectedException
    _LoopDetectedException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** BadGatewayException
    _BadGatewayException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutSession
    module Network.AWS.LexRuntime.PutSession,

    -- ** DeleteSession
    module Network.AWS.LexRuntime.DeleteSession,

    -- ** PostText
    module Network.AWS.LexRuntime.PostText,

    -- ** PostContent
    module Network.AWS.LexRuntime.PostContent,

    -- ** GetSession
    module Network.AWS.LexRuntime.GetSession,

    -- * Types

    -- ** ActiveContextName
    ActiveContextName (..),

    -- ** SentimentResponse
    SentimentResponse (..),
    mkSentimentResponse,
    srSentimentLabel,
    srSentimentScore,

    -- ** BotAlias
    BotAlias (..),

    -- ** ResponseCard
    ResponseCard (..),
    mkResponseCard,
    rcContentType,
    rcGenericAttachments,
    rcVersion,

    -- ** String
    String (..),

    -- ** Text
    Text (..),

    -- ** IntentConfidence
    IntentConfidence (..),
    mkIntentConfidence,
    icScore,

    -- ** MessageFormatType
    MessageFormatType (..),

    -- ** Accept
    Accept (..),

    -- ** StringWithLength
    StringWithLength (..),

    -- ** BotVersion
    BotVersion (..),

    -- ** IntentName
    IntentName (..),

    -- ** DialogActionType
    DialogActionType (..),

    -- ** ButtonValueStringWithLength
    ButtonValueStringWithLength (..),

    -- ** BotName
    BotName (..),

    -- ** Button
    Button (..),
    mkButton,
    bText,
    bValue,

    -- ** HttpContentType
    HttpContentType (..),

    -- ** UserId
    UserId (..),

    -- ** IntentSummaryCheckpointLabel
    IntentSummaryCheckpointLabel (..),

    -- ** SentimentScore
    SentimentScore (..),

    -- ** DialogState
    DialogState (..),

    -- ** FulfillmentState
    FulfillmentState (..),

    -- ** PredictedIntent
    PredictedIntent (..),
    mkPredictedIntent,
    piIntentName,
    piNluIntentConfidence,
    piSlots,

    -- ** GenericAttachment
    GenericAttachment (..),
    mkGenericAttachment,
    gaAttachmentLinkUrl,
    gaButtons,
    gaImageUrl,
    gaSubTitle,
    gaTitle,

    -- ** IntentSummary
    IntentSummary (..),
    mkIntentSummary,
    isDialogActionType,
    isCheckpointLabel,
    isConfirmationStatus,
    isFulfillmentState,
    isIntentName,
    isSlotToElicit,
    isSlots,

    -- ** ConfirmationStatus
    ConfirmationStatus (..),

    -- ** ActiveContext
    ActiveContext (..),
    mkActiveContext,
    acName,
    acTimeToLive,
    acParameters,

    -- ** ActiveContextTimeToLive
    ActiveContextTimeToLive (..),
    mkActiveContextTimeToLive,
    acttlTimeToLiveInSeconds,
    acttlTurnsToLive,

    -- ** ParameterName
    ParameterName (..),

    -- ** DialogAction
    DialogAction (..),
    mkDialogAction,
    daType,
    daFulfillmentState,
    daIntentName,
    daMessage,
    daMessageFormat,
    daSlotToElicit,
    daSlots,

    -- ** SentimentLabel
    SentimentLabel (..),

    -- ** ContentType
    ContentType (..),

    -- ** ActiveContexts
    ActiveContexts (..),

    -- ** Message
    Message (..),

    -- ** SessionAttributes
    SessionAttributes (..),

    -- ** SessionId
    SessionId (..),

    -- ** SlotToElicit
    SlotToElicit (..),

    -- ** Slots
    Slots (..),

    -- ** InputText
    InputText (..),

    -- ** Version
    Version (..),

    -- ** AlternativeIntents
    AlternativeIntents (..),

    -- ** InputTranscript
    InputTranscript (..),

    -- ** NluIntentConfidence
    NluIntentConfidence (..),

    -- ** RequestAttributes
    RequestAttributes (..),

    -- ** CheckpointLabelFilter
    CheckpointLabelFilter (..),

    -- ** AttachmentLinkUrl
    AttachmentLinkUrl (..),

    -- ** ImageUrl
    ImageUrl (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.LexRuntime.DeleteSession
import Network.AWS.LexRuntime.GetSession
import Network.AWS.LexRuntime.PostContent
import Network.AWS.LexRuntime.PostText
import Network.AWS.LexRuntime.PutSession
import Network.AWS.LexRuntime.Types
import Network.AWS.LexRuntime.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LexRuntime'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

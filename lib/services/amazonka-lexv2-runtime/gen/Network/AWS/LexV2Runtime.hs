{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.LexV2Runtime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-07@ of the AWS service descriptions, licensed under Apache 2.0.
module Network.AWS.LexV2Runtime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DependencyFailedException
    _DependencyFailedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** BadGatewayException
    _BadGatewayException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutSession
    PutSession (PutSession'),
    newPutSession,
    PutSessionResponse (PutSessionResponse'),
    newPutSessionResponse,

    -- ** RecognizeUtterance
    RecognizeUtterance (RecognizeUtterance'),
    newRecognizeUtterance,
    RecognizeUtteranceResponse (RecognizeUtteranceResponse'),
    newRecognizeUtteranceResponse,

    -- ** DeleteSession
    DeleteSession (DeleteSession'),
    newDeleteSession,
    DeleteSessionResponse (DeleteSessionResponse'),
    newDeleteSessionResponse,

    -- ** StartConversation
    StartConversation (StartConversation'),
    newStartConversation,
    StartConversationResponse (StartConversationResponse'),
    newStartConversationResponse,

    -- ** GetSession
    GetSession (GetSession'),
    newGetSession,
    GetSessionResponse (GetSessionResponse'),
    newGetSessionResponse,

    -- ** RecognizeText
    RecognizeText (RecognizeText'),
    newRecognizeText,
    RecognizeTextResponse (RecognizeTextResponse'),
    newRecognizeTextResponse,

    -- * Types

    -- ** ConfirmationState
    ConfirmationState (..),

    -- ** ConversationMode
    ConversationMode (..),

    -- ** DialogActionType
    DialogActionType (..),

    -- ** InputMode
    InputMode (..),

    -- ** IntentState
    IntentState (..),

    -- ** MessageContentType
    MessageContentType (..),

    -- ** PlaybackInterruptionReason
    PlaybackInterruptionReason (..),

    -- ** SentimentType
    SentimentType (..),

    -- ** Shape
    Shape (..),

    -- ** ActiveContext
    ActiveContext (ActiveContext'),
    newActiveContext,

    -- ** ActiveContextTimeToLive
    ActiveContextTimeToLive (ActiveContextTimeToLive'),
    newActiveContextTimeToLive,

    -- ** AudioInputEvent
    AudioInputEvent (AudioInputEvent'),
    newAudioInputEvent,

    -- ** AudioResponseEvent
    AudioResponseEvent (AudioResponseEvent'),
    newAudioResponseEvent,

    -- ** Button
    Button (Button'),
    newButton,

    -- ** ConfidenceScore
    ConfidenceScore (ConfidenceScore'),
    newConfidenceScore,

    -- ** ConfigurationEvent
    ConfigurationEvent (ConfigurationEvent'),
    newConfigurationEvent,

    -- ** DTMFInputEvent
    DTMFInputEvent (DTMFInputEvent'),
    newDTMFInputEvent,

    -- ** DialogAction
    DialogAction (DialogAction'),
    newDialogAction,

    -- ** DisconnectionEvent
    DisconnectionEvent (DisconnectionEvent'),
    newDisconnectionEvent,

    -- ** HeartbeatEvent
    HeartbeatEvent (HeartbeatEvent'),
    newHeartbeatEvent,

    -- ** ImageResponseCard
    ImageResponseCard (ImageResponseCard'),
    newImageResponseCard,

    -- ** Intent
    Intent (Intent'),
    newIntent,

    -- ** IntentResultEvent
    IntentResultEvent (IntentResultEvent'),
    newIntentResultEvent,

    -- ** Interpretation
    Interpretation (Interpretation'),
    newInterpretation,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** PlaybackCompletionEvent
    PlaybackCompletionEvent (PlaybackCompletionEvent'),
    newPlaybackCompletionEvent,

    -- ** PlaybackInterruptionEvent
    PlaybackInterruptionEvent (PlaybackInterruptionEvent'),
    newPlaybackInterruptionEvent,

    -- ** SentimentResponse
    SentimentResponse (SentimentResponse'),
    newSentimentResponse,

    -- ** SentimentScore
    SentimentScore (SentimentScore'),
    newSentimentScore,

    -- ** SessionState
    SessionState (SessionState'),
    newSessionState,

    -- ** Slot
    Slot (Slot'),
    newSlot,

    -- ** StartConversationRequestEventStream
    StartConversationRequestEventStream (StartConversationRequestEventStream'),
    newStartConversationRequestEventStream,

    -- ** StartConversationResponseEventStream
    StartConversationResponseEventStream (StartConversationResponseEventStream'),
    newStartConversationResponseEventStream,

    -- ** TextInputEvent
    TextInputEvent (TextInputEvent'),
    newTextInputEvent,

    -- ** TextResponseEvent
    TextResponseEvent (TextResponseEvent'),
    newTextResponseEvent,

    -- ** TranscriptEvent
    TranscriptEvent (TranscriptEvent'),
    newTranscriptEvent,

    -- ** Value
    Value (Value'),
    newValue,
  )
where

import Network.AWS.LexV2Runtime.DeleteSession
import Network.AWS.LexV2Runtime.GetSession
import Network.AWS.LexV2Runtime.Lens
import Network.AWS.LexV2Runtime.PutSession
import Network.AWS.LexV2Runtime.RecognizeText
import Network.AWS.LexV2Runtime.RecognizeUtterance
import Network.AWS.LexV2Runtime.StartConversation
import Network.AWS.LexV2Runtime.Types
import Network.AWS.LexV2Runtime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LexV2Runtime'.

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

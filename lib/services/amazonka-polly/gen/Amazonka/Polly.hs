{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Polly
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Polly is a web service that makes it easy to synthesize speech
-- from text.
--
-- The Amazon Polly service provides API operations for synthesizing
-- high-quality speech from plain text and Speech Synthesis Markup Language
-- (SSML), along with managing pronunciations lexicons that enable you to
-- get the best results for your application domain.
module Amazonka.Polly
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** EngineNotSupportedException
    _EngineNotSupportedException,

    -- ** InvalidLexiconException
    _InvalidLexiconException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidS3BucketException
    _InvalidS3BucketException,

    -- ** InvalidS3KeyException
    _InvalidS3KeyException,

    -- ** InvalidSampleRateException
    _InvalidSampleRateException,

    -- ** InvalidSnsTopicArnException
    _InvalidSnsTopicArnException,

    -- ** InvalidSsmlException
    _InvalidSsmlException,

    -- ** InvalidTaskIdException
    _InvalidTaskIdException,

    -- ** LanguageNotSupportedException
    _LanguageNotSupportedException,

    -- ** LexiconNotFoundException
    _LexiconNotFoundException,

    -- ** LexiconSizeExceededException
    _LexiconSizeExceededException,

    -- ** MarksNotSupportedForFormatException
    _MarksNotSupportedForFormatException,

    -- ** MaxLexemeLengthExceededException
    _MaxLexemeLengthExceededException,

    -- ** MaxLexiconsNumberExceededException
    _MaxLexiconsNumberExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** SsmlMarksNotSupportedForTextTypeException
    _SsmlMarksNotSupportedForTextTypeException,

    -- ** SynthesisTaskNotFoundException
    _SynthesisTaskNotFoundException,

    -- ** TextLengthExceededException
    _TextLengthExceededException,

    -- ** UnsupportedPlsAlphabetException
    _UnsupportedPlsAlphabetException,

    -- ** UnsupportedPlsLanguageException
    _UnsupportedPlsLanguageException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteLexicon
    DeleteLexicon (DeleteLexicon'),
    newDeleteLexicon,
    DeleteLexiconResponse (DeleteLexiconResponse'),
    newDeleteLexiconResponse,

    -- ** DescribeVoices (Paginated)
    DescribeVoices (DescribeVoices'),
    newDescribeVoices,
    DescribeVoicesResponse (DescribeVoicesResponse'),
    newDescribeVoicesResponse,

    -- ** GetLexicon
    GetLexicon (GetLexicon'),
    newGetLexicon,
    GetLexiconResponse (GetLexiconResponse'),
    newGetLexiconResponse,

    -- ** GetSpeechSynthesisTask
    GetSpeechSynthesisTask (GetSpeechSynthesisTask'),
    newGetSpeechSynthesisTask,
    GetSpeechSynthesisTaskResponse (GetSpeechSynthesisTaskResponse'),
    newGetSpeechSynthesisTaskResponse,

    -- ** ListLexicons (Paginated)
    ListLexicons (ListLexicons'),
    newListLexicons,
    ListLexiconsResponse (ListLexiconsResponse'),
    newListLexiconsResponse,

    -- ** ListSpeechSynthesisTasks (Paginated)
    ListSpeechSynthesisTasks (ListSpeechSynthesisTasks'),
    newListSpeechSynthesisTasks,
    ListSpeechSynthesisTasksResponse (ListSpeechSynthesisTasksResponse'),
    newListSpeechSynthesisTasksResponse,

    -- ** PutLexicon
    PutLexicon (PutLexicon'),
    newPutLexicon,
    PutLexiconResponse (PutLexiconResponse'),
    newPutLexiconResponse,

    -- ** StartSpeechSynthesisTask
    StartSpeechSynthesisTask (StartSpeechSynthesisTask'),
    newStartSpeechSynthesisTask,
    StartSpeechSynthesisTaskResponse (StartSpeechSynthesisTaskResponse'),
    newStartSpeechSynthesisTaskResponse,

    -- ** SynthesizeSpeech
    SynthesizeSpeech (SynthesizeSpeech'),
    newSynthesizeSpeech,
    SynthesizeSpeechResponse (SynthesizeSpeechResponse'),
    newSynthesizeSpeechResponse,

    -- * Types

    -- ** Engine
    Engine (..),

    -- ** Gender
    Gender (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** OutputFormat
    OutputFormat (..),

    -- ** SpeechMarkType
    SpeechMarkType (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** TextType
    TextType (..),

    -- ** VoiceId
    VoiceId (..),

    -- ** Lexicon
    Lexicon (Lexicon'),
    newLexicon,

    -- ** LexiconAttributes
    LexiconAttributes (LexiconAttributes'),
    newLexiconAttributes,

    -- ** LexiconDescription
    LexiconDescription (LexiconDescription'),
    newLexiconDescription,

    -- ** SynthesisTask
    SynthesisTask (SynthesisTask'),
    newSynthesisTask,

    -- ** Voice
    Voice (Voice'),
    newVoice,
  )
where

import Amazonka.Polly.DeleteLexicon
import Amazonka.Polly.DescribeVoices
import Amazonka.Polly.GetLexicon
import Amazonka.Polly.GetSpeechSynthesisTask
import Amazonka.Polly.Lens
import Amazonka.Polly.ListLexicons
import Amazonka.Polly.ListSpeechSynthesisTasks
import Amazonka.Polly.PutLexicon
import Amazonka.Polly.StartSpeechSynthesisTask
import Amazonka.Polly.SynthesizeSpeech
import Amazonka.Polly.Types
import Amazonka.Polly.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Polly'.

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

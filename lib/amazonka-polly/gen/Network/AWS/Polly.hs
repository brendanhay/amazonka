{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Polly is a web service that makes it easy to synthesize speech from text.
--
-- The Amazon Polly service provides API operations for synthesizing high-quality speech from plain text and Speech Synthesis Markup Language (SSML), along with managing pronunciations lexicons that enable you to get the best results for your application domain.
module Network.AWS.Polly
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidSnsTopicArnException
    _InvalidSnsTopicArnException,

    -- ** UnsupportedPlsLanguageException
    _UnsupportedPlsLanguageException,

    -- ** InvalidSsmlException
    _InvalidSsmlException,

    -- ** InvalidSampleRateException
    _InvalidSampleRateException,

    -- ** EngineNotSupportedException
    _EngineNotSupportedException,

    -- ** MaxLexiconsNumberExceededException
    _MaxLexiconsNumberExceededException,

    -- ** TextLengthExceededException
    _TextLengthExceededException,

    -- ** MaxLexemeLengthExceededException
    _MaxLexemeLengthExceededException,

    -- ** InvalidTaskIdException
    _InvalidTaskIdException,

    -- ** InvalidLexiconException
    _InvalidLexiconException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** UnsupportedPlsAlphabetException
    _UnsupportedPlsAlphabetException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** MarksNotSupportedForFormatException
    _MarksNotSupportedForFormatException,

    -- ** SynthesisTaskNotFoundException
    _SynthesisTaskNotFoundException,

    -- ** SsmlMarksNotSupportedForTextTypeException
    _SsmlMarksNotSupportedForTextTypeException,

    -- ** InvalidS3BucketException
    _InvalidS3BucketException,

    -- ** LexiconSizeExceededException
    _LexiconSizeExceededException,

    -- ** LanguageNotSupportedException
    _LanguageNotSupportedException,

    -- ** LexiconNotFoundException
    _LexiconNotFoundException,

    -- ** InvalidS3KeyException
    _InvalidS3KeyException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetLexicon
    module Network.AWS.Polly.GetLexicon,

    -- ** GetSpeechSynthesisTask
    module Network.AWS.Polly.GetSpeechSynthesisTask,

    -- ** DescribeVoices (Paginated)
    module Network.AWS.Polly.DescribeVoices,

    -- ** ListLexicons (Paginated)
    module Network.AWS.Polly.ListLexicons,

    -- ** SynthesizeSpeech
    module Network.AWS.Polly.SynthesizeSpeech,

    -- ** ListSpeechSynthesisTasks (Paginated)
    module Network.AWS.Polly.ListSpeechSynthesisTasks,

    -- ** PutLexicon
    module Network.AWS.Polly.PutLexicon,

    -- ** DeleteLexicon
    module Network.AWS.Polly.DeleteLexicon,

    -- ** StartSpeechSynthesisTask
    module Network.AWS.Polly.StartSpeechSynthesisTask,

    -- * Types

    -- ** LanguageCode
    LanguageCode (..),

    -- ** SnsTopicArn
    SnsTopicArn (..),

    -- ** LexiconName
    LexiconName (..),

    -- ** LexiconAttributes
    LexiconAttributes (..),
    mkLexiconAttributes,
    laAlphabet,
    laLanguageCode,
    laLastModified,
    laLexemesCount,
    laLexiconArn,
    laSize,

    -- ** LanguageName
    LanguageName (..),

    -- ** Text
    Text (..),

    -- ** TaskStatusReason
    TaskStatusReason (..),

    -- ** OutputS3KeyPrefix
    OutputS3KeyPrefix (..),

    -- ** TaskId
    TaskId (..),

    -- ** Engine
    Engine (..),

    -- ** NextToken
    NextToken (..),

    -- ** LexiconDescription
    LexiconDescription (..),
    mkLexiconDescription,
    ldAttributes,
    ldName,

    -- ** SampleRate
    SampleRate (..),

    -- ** OutputFormat
    OutputFormat (..),

    -- ** LexiconArn
    LexiconArn (..),

    -- ** Alphabet
    Alphabet (..),

    -- ** Gender
    Gender (..),

    -- ** Lexicon
    Lexicon (..),
    mkLexicon,
    lContent,
    lName,

    -- ** TextType
    TextType (..),

    -- ** SpeechMarkType
    SpeechMarkType (..),

    -- ** VoiceId
    VoiceId (..),

    -- ** Voice
    Voice (..),
    mkVoice,
    vAdditionalLanguageCodes,
    vGender,
    vId,
    vLanguageCode,
    vLanguageName,
    vName,
    vSupportedEngines,

    -- ** OutputS3BucketName
    OutputS3BucketName (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** SynthesisTask
    SynthesisTask (..),
    mkSynthesisTask,
    stCreationTime,
    stEngine,
    stLanguageCode,
    stLexiconNames,
    stOutputFormat,
    stOutputUri,
    stRequestCharacters,
    stSampleRate,
    stSnsTopicArn,
    stSpeechMarkTypes,
    stTaskId,
    stTaskStatus,
    stTaskStatusReason,
    stTextType,
    stVoiceId,

    -- ** ContentType
    ContentType (..),

    -- ** OutputUri
    OutputUri (..),

    -- ** Content
    Content (..),

    -- ** Name
    Name (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.Polly.DeleteLexicon
import Network.AWS.Polly.DescribeVoices
import Network.AWS.Polly.GetLexicon
import Network.AWS.Polly.GetSpeechSynthesisTask
import Network.AWS.Polly.ListLexicons
import Network.AWS.Polly.ListSpeechSynthesisTasks
import Network.AWS.Polly.PutLexicon
import Network.AWS.Polly.StartSpeechSynthesisTask
import Network.AWS.Polly.SynthesizeSpeech
import Network.AWS.Polly.Types
import Network.AWS.Polly.Waiters
import qualified Network.AWS.Prelude as Lude

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

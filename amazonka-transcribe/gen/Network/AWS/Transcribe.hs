{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Operations and objects for transcribing speech to text.
--
--
module Network.AWS.Transcribe
    (
    -- * Service Configuration
      transcribe

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetVocabulary
    , module Network.AWS.Transcribe.GetVocabulary

    -- ** GetTranscriptionJob
    , module Network.AWS.Transcribe.GetTranscriptionJob

    -- ** DeleteVocabulary
    , module Network.AWS.Transcribe.DeleteVocabulary

    -- ** UpdateVocabulary
    , module Network.AWS.Transcribe.UpdateVocabulary

    -- ** ListTranscriptionJobs
    , module Network.AWS.Transcribe.ListTranscriptionJobs

    -- ** ListVocabularies
    , module Network.AWS.Transcribe.ListVocabularies

    -- ** CreateVocabulary
    , module Network.AWS.Transcribe.CreateVocabulary

    -- ** StartTranscriptionJob
    , module Network.AWS.Transcribe.StartTranscriptionJob

    -- * Types

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** MediaFormat
    , MediaFormat (..)

    -- ** TranscriptionJobStatus
    , TranscriptionJobStatus (..)

    -- ** VocabularyState
    , VocabularyState (..)

    -- ** Media
    , Media
    , media
    , mMediaFileURI

    -- ** Settings
    , Settings
    , settings
    , sVocabularyName
    , sMaxSpeakerLabels
    , sShowSpeakerLabels

    -- ** Transcript
    , Transcript
    , transcript
    , tTranscriptFileURI

    -- ** TranscriptionJob
    , TranscriptionJob
    , transcriptionJob
    , tjCreationTime
    , tjFailureReason
    , tjLanguageCode
    , tjSettings
    , tjCompletionTime
    , tjMedia
    , tjMediaFormat
    , tjTranscriptionJobStatus
    , tjTranscriptionJobName
    , tjTranscript
    , tjMediaSampleRateHertz

    -- ** TranscriptionJobSummary
    , TranscriptionJobSummary
    , transcriptionJobSummary
    , tjsCreationTime
    , tjsFailureReason
    , tjsLanguageCode
    , tjsCompletionTime
    , tjsTranscriptionJobStatus
    , tjsTranscriptionJobName

    -- ** VocabularyInfo
    , VocabularyInfo
    , vocabularyInfo
    , viLanguageCode
    , viVocabularyName
    , viLastModifiedTime
    , viVocabularyState
    ) where

import Network.AWS.Transcribe.CreateVocabulary
import Network.AWS.Transcribe.DeleteVocabulary
import Network.AWS.Transcribe.GetTranscriptionJob
import Network.AWS.Transcribe.GetVocabulary
import Network.AWS.Transcribe.ListTranscriptionJobs
import Network.AWS.Transcribe.ListVocabularies
import Network.AWS.Transcribe.StartTranscriptionJob
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.UpdateVocabulary
import Network.AWS.Transcribe.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Transcribe'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

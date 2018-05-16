{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.
--
--
module Network.AWS.Comprehend
    (
    -- * Service Configuration
      comprehend

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerException
    , _InternalServerException

    -- ** BatchSizeLimitExceededException
    , _BatchSizeLimitExceededException

    -- ** UnsupportedLanguageException
    , _UnsupportedLanguageException

    -- ** JobNotFoundException
    , _JobNotFoundException

    -- ** InvalidFilterException
    , _InvalidFilterException

    -- ** TextSizeLimitExceededException
    , _TextSizeLimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDetectSentiment
    , module Network.AWS.Comprehend.BatchDetectSentiment

    -- ** StartTopicsDetectionJob
    , module Network.AWS.Comprehend.StartTopicsDetectionJob

    -- ** BatchDetectKeyPhrases
    , module Network.AWS.Comprehend.BatchDetectKeyPhrases

    -- ** DetectSentiment
    , module Network.AWS.Comprehend.DetectSentiment

    -- ** BatchDetectEntities
    , module Network.AWS.Comprehend.BatchDetectEntities

    -- ** DetectDominantLanguage
    , module Network.AWS.Comprehend.DetectDominantLanguage

    -- ** DescribeTopicsDetectionJob
    , module Network.AWS.Comprehend.DescribeTopicsDetectionJob

    -- ** DetectEntities
    , module Network.AWS.Comprehend.DetectEntities

    -- ** ListTopicsDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListTopicsDetectionJobs

    -- ** BatchDetectDominantLanguage
    , module Network.AWS.Comprehend.BatchDetectDominantLanguage

    -- ** DetectKeyPhrases
    , module Network.AWS.Comprehend.DetectKeyPhrases

    -- * Types

    -- ** EntityType
    , EntityType (..)

    -- ** InputFormat
    , InputFormat (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** SentimentType
    , SentimentType (..)

    -- ** BatchDetectDominantLanguageItemResult
    , BatchDetectDominantLanguageItemResult
    , batchDetectDominantLanguageItemResult
    , bddlirLanguages
    , bddlirIndex

    -- ** BatchDetectEntitiesItemResult
    , BatchDetectEntitiesItemResult
    , batchDetectEntitiesItemResult
    , bdeirEntities
    , bdeirIndex

    -- ** BatchDetectKeyPhrasesItemResult
    , BatchDetectKeyPhrasesItemResult
    , batchDetectKeyPhrasesItemResult
    , bdkpirIndex
    , bdkpirKeyPhrases

    -- ** BatchDetectSentimentItemResult
    , BatchDetectSentimentItemResult
    , batchDetectSentimentItemResult
    , bdsirSentiment
    , bdsirSentimentScore
    , bdsirIndex

    -- ** BatchItemError
    , BatchItemError
    , batchItemError
    , bieErrorCode
    , bieErrorMessage
    , bieIndex

    -- ** DominantLanguage
    , DominantLanguage
    , dominantLanguage
    , dlLanguageCode
    , dlScore

    -- ** Entity
    , Entity
    , entity
    , eBeginOffset
    , eText
    , eScore
    , eEndOffset
    , eType

    -- ** InputDataConfig
    , InputDataConfig
    , inputDataConfig
    , idcInputFormat
    , idcS3URI

    -- ** KeyPhrase
    , KeyPhrase
    , keyPhrase
    , kpBeginOffset
    , kpText
    , kpScore
    , kpEndOffset

    -- ** OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcS3URI

    -- ** SentimentScore
    , SentimentScore
    , sentimentScore
    , ssMixed
    , ssNegative
    , ssNeutral
    , ssPositive

    -- ** TopicsDetectionJobFilter
    , TopicsDetectionJobFilter
    , topicsDetectionJobFilter
    , tdjfSubmitTimeAfter
    , tdjfSubmitTimeBefore
    , tdjfJobName
    , tdjfJobStatus

    -- ** TopicsDetectionJobProperties
    , TopicsDetectionJobProperties
    , topicsDetectionJobProperties
    , tdjpJobId
    , tdjpJobName
    , tdjpInputDataConfig
    , tdjpEndTime
    , tdjpOutputDataConfig
    , tdjpNumberOfTopics
    , tdjpJobStatus
    , tdjpMessage
    , tdjpSubmitTime
    ) where

import Network.AWS.Comprehend.BatchDetectDominantLanguage
import Network.AWS.Comprehend.BatchDetectEntities
import Network.AWS.Comprehend.BatchDetectKeyPhrases
import Network.AWS.Comprehend.BatchDetectSentiment
import Network.AWS.Comprehend.DescribeTopicsDetectionJob
import Network.AWS.Comprehend.DetectDominantLanguage
import Network.AWS.Comprehend.DetectEntities
import Network.AWS.Comprehend.DetectKeyPhrases
import Network.AWS.Comprehend.DetectSentiment
import Network.AWS.Comprehend.ListTopicsDetectionJobs
import Network.AWS.Comprehend.StartTopicsDetectionJob
import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Comprehend'.
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

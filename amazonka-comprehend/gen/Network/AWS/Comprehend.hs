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

    -- ** ResourceUnavailableException
    , _ResourceUnavailableException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ResourceLimitExceededException
    , _ResourceLimitExceededException

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

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** TextSizeLimitExceededException
    , _TextSizeLimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDetectSentiment
    , module Network.AWS.Comprehend.BatchDetectSentiment

    -- ** DeleteEntityRecognizer
    , module Network.AWS.Comprehend.DeleteEntityRecognizer

    -- ** DescribeKeyPhrasesDetectionJob
    , module Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob

    -- ** ListEntitiesDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListEntitiesDetectionJobs

    -- ** StartSentimentDetectionJob
    , module Network.AWS.Comprehend.StartSentimentDetectionJob

    -- ** BatchDetectSyntax
    , module Network.AWS.Comprehend.BatchDetectSyntax

    -- ** StartTopicsDetectionJob
    , module Network.AWS.Comprehend.StartTopicsDetectionJob

    -- ** BatchDetectKeyPhrases
    , module Network.AWS.Comprehend.BatchDetectKeyPhrases

    -- ** DescribeSentimentDetectionJob
    , module Network.AWS.Comprehend.DescribeSentimentDetectionJob

    -- ** StartEntitiesDetectionJob
    , module Network.AWS.Comprehend.StartEntitiesDetectionJob

    -- ** DescribeEntityRecognizer
    , module Network.AWS.Comprehend.DescribeEntityRecognizer

    -- ** DetectSentiment
    , module Network.AWS.Comprehend.DetectSentiment

    -- ** StartDominantLanguageDetectionJob
    , module Network.AWS.Comprehend.StartDominantLanguageDetectionJob

    -- ** StopTrainingDocumentClassifier
    , module Network.AWS.Comprehend.StopTrainingDocumentClassifier

    -- ** DescribeDocumentClassificationJob
    , module Network.AWS.Comprehend.DescribeDocumentClassificationJob

    -- ** BatchDetectEntities
    , module Network.AWS.Comprehend.BatchDetectEntities

    -- ** CreateEntityRecognizer
    , module Network.AWS.Comprehend.CreateEntityRecognizer

    -- ** StopKeyPhrasesDetectionJob
    , module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob

    -- ** CreateDocumentClassifier
    , module Network.AWS.Comprehend.CreateDocumentClassifier

    -- ** ListEntityRecognizers (Paginated)
    , module Network.AWS.Comprehend.ListEntityRecognizers

    -- ** StopSentimentDetectionJob
    , module Network.AWS.Comprehend.StopSentimentDetectionJob

    -- ** DetectDominantLanguage
    , module Network.AWS.Comprehend.DetectDominantLanguage

    -- ** DescribeTopicsDetectionJob
    , module Network.AWS.Comprehend.DescribeTopicsDetectionJob

    -- ** ListDocumentClassificationJobs (Paginated)
    , module Network.AWS.Comprehend.ListDocumentClassificationJobs

    -- ** DetectEntities
    , module Network.AWS.Comprehend.DetectEntities

    -- ** DescribeDocumentClassifier
    , module Network.AWS.Comprehend.DescribeDocumentClassifier

    -- ** DescribeDominantLanguageDetectionJob
    , module Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob

    -- ** StopEntitiesDetectionJob
    , module Network.AWS.Comprehend.StopEntitiesDetectionJob

    -- ** StopTrainingEntityRecognizer
    , module Network.AWS.Comprehend.StopTrainingEntityRecognizer

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs

    -- ** DescribeEntitiesDetectionJob
    , module Network.AWS.Comprehend.DescribeEntitiesDetectionJob

    -- ** StopDominantLanguageDetectionJob
    , module Network.AWS.Comprehend.StopDominantLanguageDetectionJob

    -- ** ListTopicsDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListTopicsDetectionJobs

    -- ** BatchDetectDominantLanguage
    , module Network.AWS.Comprehend.BatchDetectDominantLanguage

    -- ** StartDocumentClassificationJob
    , module Network.AWS.Comprehend.StartDocumentClassificationJob

    -- ** DetectKeyPhrases
    , module Network.AWS.Comprehend.DetectKeyPhrases

    -- ** DetectSyntax
    , module Network.AWS.Comprehend.DetectSyntax

    -- ** ListSentimentDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListSentimentDetectionJobs

    -- ** DeleteDocumentClassifier
    , module Network.AWS.Comprehend.DeleteDocumentClassifier

    -- ** ListDominantLanguageDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs

    -- ** StartKeyPhrasesDetectionJob
    , module Network.AWS.Comprehend.StartKeyPhrasesDetectionJob

    -- ** ListDocumentClassifiers (Paginated)
    , module Network.AWS.Comprehend.ListDocumentClassifiers

    -- * Types

    -- ** EntityType
    , EntityType (..)

    -- ** InputFormat
    , InputFormat (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** ModelStatus
    , ModelStatus (..)

    -- ** PartOfSpeechTagType
    , PartOfSpeechTagType (..)

    -- ** SentimentType
    , SentimentType (..)

    -- ** SyntaxLanguageCode
    , SyntaxLanguageCode (..)

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
    , bSentiment
    , bSentimentScore
    , bIndex

    -- ** BatchDetectSyntaxItemResult
    , BatchDetectSyntaxItemResult
    , batchDetectSyntaxItemResult
    , bdsirIndex
    , bdsirSyntaxTokens

    -- ** BatchItemError
    , BatchItemError
    , batchItemError
    , bieErrorCode
    , bieErrorMessage
    , bieIndex

    -- ** ClassifierEvaluationMetrics
    , ClassifierEvaluationMetrics
    , classifierEvaluationMetrics
    , cemRecall
    , cemPrecision
    , cemF1Score
    , cemAccuracy

    -- ** ClassifierMetadata
    , ClassifierMetadata
    , classifierMetadata
    , cmNumberOfLabels
    , cmEvaluationMetrics
    , cmNumberOfTrainedDocuments
    , cmNumberOfTestDocuments

    -- ** DocumentClassificationJobFilter
    , DocumentClassificationJobFilter
    , documentClassificationJobFilter
    , dcjfSubmitTimeAfter
    , dcjfSubmitTimeBefore
    , dcjfJobName
    , dcjfJobStatus

    -- ** DocumentClassificationJobProperties
    , DocumentClassificationJobProperties
    , documentClassificationJobProperties
    , dcjpJobId
    , dcjpDocumentClassifierARN
    , dcjpJobName
    , dcjpInputDataConfig
    , dcjpEndTime
    , dcjpOutputDataConfig
    , dcjpDataAccessRoleARN
    , dcjpJobStatus
    , dcjpMessage
    , dcjpSubmitTime

    -- ** DocumentClassifierFilter
    , DocumentClassifierFilter
    , documentClassifierFilter
    , dcfStatus
    , dcfSubmitTimeAfter
    , dcfSubmitTimeBefore

    -- ** DocumentClassifierInputDataConfig
    , DocumentClassifierInputDataConfig
    , documentClassifierInputDataConfig
    , dcidcS3URI

    -- ** DocumentClassifierProperties
    , DocumentClassifierProperties
    , documentClassifierProperties
    , dcpStatus
    , dcpLanguageCode
    , dcpClassifierMetadata
    , dcpTrainingEndTime
    , dcpDocumentClassifierARN
    , dcpInputDataConfig
    , dcpEndTime
    , dcpTrainingStartTime
    , dcpDataAccessRoleARN
    , dcpMessage
    , dcpSubmitTime

    -- ** DominantLanguage
    , DominantLanguage
    , dominantLanguage
    , dlLanguageCode
    , dlScore

    -- ** DominantLanguageDetectionJobFilter
    , DominantLanguageDetectionJobFilter
    , dominantLanguageDetectionJobFilter
    , dldjfSubmitTimeAfter
    , dldjfSubmitTimeBefore
    , dldjfJobName
    , dldjfJobStatus

    -- ** DominantLanguageDetectionJobProperties
    , DominantLanguageDetectionJobProperties
    , dominantLanguageDetectionJobProperties
    , dldjpJobId
    , dldjpJobName
    , dldjpInputDataConfig
    , dldjpEndTime
    , dldjpOutputDataConfig
    , dldjpDataAccessRoleARN
    , dldjpJobStatus
    , dldjpMessage
    , dldjpSubmitTime

    -- ** EntitiesDetectionJobFilter
    , EntitiesDetectionJobFilter
    , entitiesDetectionJobFilter
    , edjfSubmitTimeAfter
    , edjfSubmitTimeBefore
    , edjfJobName
    , edjfJobStatus

    -- ** EntitiesDetectionJobProperties
    , EntitiesDetectionJobProperties
    , entitiesDetectionJobProperties
    , edjpLanguageCode
    , edjpJobId
    , edjpEntityRecognizerARN
    , edjpJobName
    , edjpInputDataConfig
    , edjpEndTime
    , edjpOutputDataConfig
    , edjpDataAccessRoleARN
    , edjpJobStatus
    , edjpMessage
    , edjpSubmitTime

    -- ** Entity
    , Entity
    , entity
    , eBeginOffset
    , eText
    , eScore
    , eEndOffset
    , eType

    -- ** EntityRecognizerAnnotations
    , EntityRecognizerAnnotations
    , entityRecognizerAnnotations
    , eraS3URI

    -- ** EntityRecognizerDocuments
    , EntityRecognizerDocuments
    , entityRecognizerDocuments
    , erdS3URI

    -- ** EntityRecognizerEntityList
    , EntityRecognizerEntityList
    , entityRecognizerEntityList
    , erelS3URI

    -- ** EntityRecognizerEvaluationMetrics
    , EntityRecognizerEvaluationMetrics
    , entityRecognizerEvaluationMetrics
    , eremRecall
    , eremPrecision
    , eremF1Score

    -- ** EntityRecognizerFilter
    , EntityRecognizerFilter
    , entityRecognizerFilter
    , erfStatus
    , erfSubmitTimeAfter
    , erfSubmitTimeBefore

    -- ** EntityRecognizerInputDataConfig
    , EntityRecognizerInputDataConfig
    , entityRecognizerInputDataConfig
    , eridcAnnotations
    , eridcEntityList
    , eridcEntityTypes
    , eridcDocuments

    -- ** EntityRecognizerMetadata
    , EntityRecognizerMetadata
    , entityRecognizerMetadata
    , ermEntityTypes
    , ermEvaluationMetrics
    , ermNumberOfTrainedDocuments
    , ermNumberOfTestDocuments

    -- ** EntityRecognizerMetadataEntityTypesListItem
    , EntityRecognizerMetadataEntityTypesListItem
    , entityRecognizerMetadataEntityTypesListItem
    , ermetliType

    -- ** EntityRecognizerProperties
    , EntityRecognizerProperties
    , entityRecognizerProperties
    , erpStatus
    , erpLanguageCode
    , erpTrainingEndTime
    , erpEntityRecognizerARN
    , erpInputDataConfig
    , erpEndTime
    , erpTrainingStartTime
    , erpDataAccessRoleARN
    , erpRecognizerMetadata
    , erpMessage
    , erpSubmitTime

    -- ** EntityTypesListItem
    , EntityTypesListItem
    , entityTypesListItem
    , etliType

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

    -- ** KeyPhrasesDetectionJobFilter
    , KeyPhrasesDetectionJobFilter
    , keyPhrasesDetectionJobFilter
    , kpdjfSubmitTimeAfter
    , kpdjfSubmitTimeBefore
    , kpdjfJobName
    , kpdjfJobStatus

    -- ** KeyPhrasesDetectionJobProperties
    , KeyPhrasesDetectionJobProperties
    , keyPhrasesDetectionJobProperties
    , kpdjpLanguageCode
    , kpdjpJobId
    , kpdjpJobName
    , kpdjpInputDataConfig
    , kpdjpEndTime
    , kpdjpOutputDataConfig
    , kpdjpDataAccessRoleARN
    , kpdjpJobStatus
    , kpdjpMessage
    , kpdjpSubmitTime

    -- ** OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcS3URI

    -- ** PartOfSpeechTag
    , PartOfSpeechTag
    , partOfSpeechTag
    , postTag
    , postScore

    -- ** SentimentDetectionJobFilter
    , SentimentDetectionJobFilter
    , sentimentDetectionJobFilter
    , sdjfSubmitTimeAfter
    , sdjfSubmitTimeBefore
    , sdjfJobName
    , sdjfJobStatus

    -- ** SentimentDetectionJobProperties
    , SentimentDetectionJobProperties
    , sentimentDetectionJobProperties
    , sdjpLanguageCode
    , sdjpJobId
    , sdjpJobName
    , sdjpInputDataConfig
    , sdjpEndTime
    , sdjpOutputDataConfig
    , sdjpDataAccessRoleARN
    , sdjpJobStatus
    , sdjpMessage
    , sdjpSubmitTime

    -- ** SentimentScore
    , SentimentScore
    , sentimentScore
    , ssMixed
    , ssNegative
    , ssNeutral
    , ssPositive

    -- ** SyntaxToken
    , SyntaxToken
    , syntaxToken
    , stBeginOffset
    , stText
    , stTokenId
    , stEndOffset
    , stPartOfSpeech

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
import Network.AWS.Comprehend.BatchDetectSyntax
import Network.AWS.Comprehend.CreateDocumentClassifier
import Network.AWS.Comprehend.CreateEntityRecognizer
import Network.AWS.Comprehend.DeleteDocumentClassifier
import Network.AWS.Comprehend.DeleteEntityRecognizer
import Network.AWS.Comprehend.DescribeDocumentClassificationJob
import Network.AWS.Comprehend.DescribeDocumentClassifier
import Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
import Network.AWS.Comprehend.DescribeEntitiesDetectionJob
import Network.AWS.Comprehend.DescribeEntityRecognizer
import Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
import Network.AWS.Comprehend.DescribeSentimentDetectionJob
import Network.AWS.Comprehend.DescribeTopicsDetectionJob
import Network.AWS.Comprehend.DetectDominantLanguage
import Network.AWS.Comprehend.DetectEntities
import Network.AWS.Comprehend.DetectKeyPhrases
import Network.AWS.Comprehend.DetectSentiment
import Network.AWS.Comprehend.DetectSyntax
import Network.AWS.Comprehend.ListDocumentClassificationJobs
import Network.AWS.Comprehend.ListDocumentClassifiers
import Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
import Network.AWS.Comprehend.ListEntitiesDetectionJobs
import Network.AWS.Comprehend.ListEntityRecognizers
import Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
import Network.AWS.Comprehend.ListSentimentDetectionJobs
import Network.AWS.Comprehend.ListTopicsDetectionJobs
import Network.AWS.Comprehend.StartDocumentClassificationJob
import Network.AWS.Comprehend.StartDominantLanguageDetectionJob
import Network.AWS.Comprehend.StartEntitiesDetectionJob
import Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
import Network.AWS.Comprehend.StartSentimentDetectionJob
import Network.AWS.Comprehend.StartTopicsDetectionJob
import Network.AWS.Comprehend.StopDominantLanguageDetectionJob
import Network.AWS.Comprehend.StopEntitiesDetectionJob
import Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
import Network.AWS.Comprehend.StopSentimentDetectionJob
import Network.AWS.Comprehend.StopTrainingDocumentClassifier
import Network.AWS.Comprehend.StopTrainingEntityRecognizer
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

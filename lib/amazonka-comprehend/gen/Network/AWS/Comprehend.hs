{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.
module Network.AWS.Comprehend
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ResourceUnavailableException
    , _ResourceUnavailableException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ResourceLimitExceededException
    , _ResourceLimitExceededException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InternalServerException
    , _InternalServerException

    -- ** BatchSizeLimitExceededException
    , _BatchSizeLimitExceededException

    -- ** UnsupportedLanguageException
    , _UnsupportedLanguageException

    -- ** JobNotFoundException
    , _JobNotFoundException

    -- ** TooManyTagKeysException
    , _TooManyTagKeysException

    -- ** InvalidFilterException
    , _InvalidFilterException

    -- ** KmsKeyValidationException
    , _KmsKeyValidationException

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

    -- ** CreateEndpoint 
    , module Network.AWS.Comprehend.CreateEndpoint

    -- ** StopEventsDetectionJob 
    , module Network.AWS.Comprehend.StopEventsDetectionJob

    -- ** StartSentimentDetectionJob 
    , module Network.AWS.Comprehend.StartSentimentDetectionJob

    -- ** BatchDetectSyntax 
    , module Network.AWS.Comprehend.BatchDetectSyntax

    -- ** StartTopicsDetectionJob 
    , module Network.AWS.Comprehend.StartTopicsDetectionJob

    -- ** DescribeEventsDetectionJob 
    , module Network.AWS.Comprehend.DescribeEventsDetectionJob

    -- ** DeleteEndpoint 
    , module Network.AWS.Comprehend.DeleteEndpoint

    -- ** UpdateEndpoint 
    , module Network.AWS.Comprehend.UpdateEndpoint

    -- ** ListTagsForResource 
    , module Network.AWS.Comprehend.ListTagsForResource

    -- ** BatchDetectKeyPhrases 
    , module Network.AWS.Comprehend.BatchDetectKeyPhrases

    -- ** DescribeSentimentDetectionJob 
    , module Network.AWS.Comprehend.DescribeSentimentDetectionJob

    -- ** StartEntitiesDetectionJob 
    , module Network.AWS.Comprehend.StartEntitiesDetectionJob

    -- ** StopPiiEntitiesDetectionJob 
    , module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob

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

    -- ** ListEventsDetectionJobs 
    , module Network.AWS.Comprehend.ListEventsDetectionJobs

    -- ** BatchDetectEntities 
    , module Network.AWS.Comprehend.BatchDetectEntities

    -- ** CreateEntityRecognizer 
    , module Network.AWS.Comprehend.CreateEntityRecognizer

    -- ** StopKeyPhrasesDetectionJob 
    , module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob

    -- ** CreateDocumentClassifier 
    , module Network.AWS.Comprehend.CreateDocumentClassifier

    -- ** ListPiiEntitiesDetectionJobs 
    , module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs

    -- ** ListEntityRecognizers (Paginated)
    , module Network.AWS.Comprehend.ListEntityRecognizers

    -- ** StopSentimentDetectionJob 
    , module Network.AWS.Comprehend.StopSentimentDetectionJob

    -- ** DetectDominantLanguage 
    , module Network.AWS.Comprehend.DetectDominantLanguage

    -- ** ClassifyDocument 
    , module Network.AWS.Comprehend.ClassifyDocument

    -- ** StartEventsDetectionJob 
    , module Network.AWS.Comprehend.StartEventsDetectionJob

    -- ** DescribeTopicsDetectionJob 
    , module Network.AWS.Comprehend.DescribeTopicsDetectionJob

    -- ** ListDocumentClassificationJobs (Paginated)
    , module Network.AWS.Comprehend.ListDocumentClassificationJobs

    -- ** DetectPiiEntities 
    , module Network.AWS.Comprehend.DetectPiiEntities

    -- ** ListEndpoints 
    , module Network.AWS.Comprehend.ListEndpoints

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

    -- ** StartPiiEntitiesDetectionJob 
    , module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs

    -- ** DescribeEntitiesDetectionJob 
    , module Network.AWS.Comprehend.DescribeEntitiesDetectionJob

    -- ** StopDominantLanguageDetectionJob 
    , module Network.AWS.Comprehend.StopDominantLanguageDetectionJob

    -- ** TagResource 
    , module Network.AWS.Comprehend.TagResource

    -- ** DescribePiiEntitiesDetectionJob 
    , module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob

    -- ** ListTopicsDetectionJobs (Paginated)
    , module Network.AWS.Comprehend.ListTopicsDetectionJobs

    -- ** UntagResource 
    , module Network.AWS.Comprehend.UntagResource

    -- ** BatchDetectDominantLanguage 
    , module Network.AWS.Comprehend.BatchDetectDominantLanguage

    -- ** StartDocumentClassificationJob 
    , module Network.AWS.Comprehend.StartDocumentClassificationJob

    -- ** DetectKeyPhrases 
    , module Network.AWS.Comprehend.DetectKeyPhrases

    -- ** DetectSyntax 
    , module Network.AWS.Comprehend.DetectSyntax

    -- ** DescribeEndpoint 
    , module Network.AWS.Comprehend.DescribeEndpoint

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

    -- ** EntityRecognizerEntityList
    , EntityRecognizerEntityList (..)
    , mkEntityRecognizerEntityList
    , erelS3Uri

    -- ** IamRoleArn
    , IamRoleArn (..)

    -- ** EntityRecognizerEvaluationMetrics
    , EntityRecognizerEvaluationMetrics (..)
    , mkEntityRecognizerEvaluationMetrics
    , eremF1Score
    , eremPrecision
    , eremRecall

    -- ** PartOfSpeechTag
    , PartOfSpeechTag (..)
    , mkPartOfSpeechTag
    , postScore
    , postTag

    -- ** EntityRecognizerProperties
    , EntityRecognizerProperties (..)
    , mkEntityRecognizerProperties
    , erpDataAccessRoleArn
    , erpEndTime
    , erpEntityRecognizerArn
    , erpInputDataConfig
    , erpLanguageCode
    , erpMessage
    , erpRecognizerMetadata
    , erpStatus
    , erpSubmitTime
    , erpTrainingEndTime
    , erpTrainingStartTime
    , erpVolumeKmsKeyId
    , erpVpcConfig

    -- ** PiiEntitiesDetectionJobProperties
    , PiiEntitiesDetectionJobProperties (..)
    , mkPiiEntitiesDetectionJobProperties
    , pedjpDataAccessRoleArn
    , pedjpEndTime
    , pedjpInputDataConfig
    , pedjpJobId
    , pedjpJobName
    , pedjpJobStatus
    , pedjpLanguageCode
    , pedjpMessage
    , pedjpMode
    , pedjpOutputDataConfig
    , pedjpRedactionConfig
    , pedjpSubmitTime

    -- ** EventsDetectionJobFilter
    , EventsDetectionJobFilter (..)
    , mkEventsDetectionJobFilter
    , edjfJobName
    , edjfJobStatus
    , edjfSubmitTimeAfter
    , edjfSubmitTimeBefore

    -- ** AnyLengthString
    , AnyLengthString (..)

    -- ** AttributeNamesListItem
    , AttributeNamesListItem (..)

    -- ** BatchDetectDominantLanguageItemResult
    , BatchDetectDominantLanguageItemResult (..)
    , mkBatchDetectDominantLanguageItemResult
    , bddlirIndex
    , bddlirLanguages

    -- ** CustomerInputString
    , CustomerInputString (..)

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** EndpointProperties
    , EndpointProperties (..)
    , mkEndpointProperties
    , epCreationTime
    , epCurrentInferenceUnits
    , epDesiredInferenceUnits
    , epEndpointArn
    , epLastModifiedTime
    , epMessage
    , epModelArn
    , epStatus

    -- ** KeyPhrase
    , KeyPhrase (..)
    , mkKeyPhrase
    , kpBeginOffset
    , kpEndOffset
    , kpScore
    , kpText

    -- ** SyntaxToken
    , SyntaxToken (..)
    , mkSyntaxToken
    , stBeginOffset
    , stEndOffset
    , stPartOfSpeech
    , stText
    , stTokenId

    -- ** BatchDetectEntitiesItemResult
    , BatchDetectEntitiesItemResult (..)
    , mkBatchDetectEntitiesItemResult
    , bdeirEntities
    , bdeirIndex

    -- ** ClassifierMetadata
    , ClassifierMetadata (..)
    , mkClassifierMetadata
    , cmEvaluationMetrics
    , cmNumberOfLabels
    , cmNumberOfTestDocuments
    , cmNumberOfTrainedDocuments

    -- ** JobId
    , JobId (..)

    -- ** DocumentClassifierArn
    , DocumentClassifierArn (..)

    -- ** MaskCharacter
    , MaskCharacter (..)

    -- ** EntityRecognizerFilter
    , EntityRecognizerFilter (..)
    , mkEntityRecognizerFilter
    , erfStatus
    , erfSubmitTimeAfter
    , erfSubmitTimeBefore

    -- ** PiiEntitiesDetectionJobFilter
    , PiiEntitiesDetectionJobFilter (..)
    , mkPiiEntitiesDetectionJobFilter
    , pedjfJobName
    , pedjfJobStatus
    , pedjfSubmitTimeAfter
    , pedjfSubmitTimeBefore

    -- ** PiiOutputDataConfig
    , PiiOutputDataConfig (..)
    , mkPiiOutputDataConfig
    , podcS3Uri
    , podcKmsKeyId

    -- ** ComprehendEndpointName
    , ComprehendEndpointName (..)

    -- ** DominantLanguage
    , DominantLanguage (..)
    , mkDominantLanguage
    , dlLanguageCode
    , dlScore

    -- ** EntityRecognizerArn
    , EntityRecognizerArn (..)

    -- ** EntityRecognizerMetadata
    , EntityRecognizerMetadata (..)
    , mkEntityRecognizerMetadata
    , ermEntityTypes
    , ermEvaluationMetrics
    , ermNumberOfTestDocuments
    , ermNumberOfTrainedDocuments

    -- ** JobName
    , JobName (..)

    -- ** KeyPhrasesDetectionJobProperties
    , KeyPhrasesDetectionJobProperties (..)
    , mkKeyPhrasesDetectionJobProperties
    , kpdjpDataAccessRoleArn
    , kpdjpEndTime
    , kpdjpInputDataConfig
    , kpdjpJobId
    , kpdjpJobName
    , kpdjpJobStatus
    , kpdjpLanguageCode
    , kpdjpMessage
    , kpdjpOutputDataConfig
    , kpdjpSubmitTime
    , kpdjpVolumeKmsKeyId
    , kpdjpVpcConfig

    -- ** EntityType
    , EntityType (..)

    -- ** DocumentClassifierMode
    , DocumentClassifierMode (..)

    -- ** ComprehendModelArn
    , ComprehendModelArn (..)

    -- ** PiiEntity
    , PiiEntity (..)
    , mkPiiEntity
    , peBeginOffset
    , peEndOffset
    , peScore
    , peType

    -- ** DocumentClassifierDataFormat
    , DocumentClassifierDataFormat (..)

    -- ** EndpointFilter
    , EndpointFilter (..)
    , mkEndpointFilter
    , efCreationTimeAfter
    , efCreationTimeBefore
    , efModelArn
    , efStatus

    -- ** BatchDetectSyntaxItemResult
    , BatchDetectSyntaxItemResult (..)
    , mkBatchDetectSyntaxItemResult
    , bdsirIndex
    , bdsirSyntaxTokens

    -- ** ClientRequestTokenString
    , ClientRequestTokenString (..)

    -- ** DocumentClassificationJobFilter
    , DocumentClassificationJobFilter (..)
    , mkDocumentClassificationJobFilter
    , dcjfJobName
    , dcjfJobStatus
    , dcjfSubmitTimeAfter
    , dcjfSubmitTimeBefore

    -- ** EntityRecognizerDataFormat
    , EntityRecognizerDataFormat (..)

    -- ** SyntaxLanguageCode
    , SyntaxLanguageCode (..)

    -- ** SentimentDetectionJobProperties
    , SentimentDetectionJobProperties (..)
    , mkSentimentDetectionJobProperties
    , sdjpDataAccessRoleArn
    , sdjpEndTime
    , sdjpInputDataConfig
    , sdjpJobId
    , sdjpJobName
    , sdjpJobStatus
    , sdjpLanguageCode
    , sdjpMessage
    , sdjpOutputDataConfig
    , sdjpSubmitTime
    , sdjpVolumeKmsKeyId
    , sdjpVpcConfig

    -- ** SubnetId
    , SubnetId (..)

    -- ** BatchDetectKeyPhrasesItemResult
    , BatchDetectKeyPhrasesItemResult (..)
    , mkBatchDetectKeyPhrasesItemResult
    , bdkpirIndex
    , bdkpirKeyPhrases

    -- ** EntityTypesEvaluationMetrics
    , EntityTypesEvaluationMetrics (..)
    , mkEntityTypesEvaluationMetrics
    , etemF1Score
    , etemPrecision
    , etemRecall

    -- ** PiiEntitiesDetectionMode
    , PiiEntitiesDetectionMode (..)

    -- ** BatchItemError
    , BatchItemError (..)
    , mkBatchItemError
    , bieErrorCode
    , bieErrorMessage
    , bieIndex

    -- ** KeyPhrasesDetectionJobFilter
    , KeyPhrasesDetectionJobFilter (..)
    , mkKeyPhrasesDetectionJobFilter
    , kpdjfJobName
    , kpdjfJobStatus
    , kpdjfSubmitTimeAfter
    , kpdjfSubmitTimeBefore

    -- ** DocumentClass
    , DocumentClass (..)
    , mkDocumentClass
    , dcName
    , dcScore

    -- ** EntityRecognizerAnnotations
    , EntityRecognizerAnnotations (..)
    , mkEntityRecognizerAnnotations
    , eraS3Uri

    -- ** DocumentLabel
    , DocumentLabel (..)
    , mkDocumentLabel
    , dName
    , dScore

    -- ** SecurityGroupId
    , SecurityGroupId (..)

    -- ** EntitiesDetectionJobProperties
    , EntitiesDetectionJobProperties (..)
    , mkEntitiesDetectionJobProperties
    , eDataAccessRoleArn
    , eEndTime
    , eEntityRecognizerArn
    , eInputDataConfig
    , eJobId
    , eJobName
    , eJobStatus
    , eLanguageCode
    , eMessage
    , eOutputDataConfig
    , eSubmitTime
    , eVolumeKmsKeyId
    , eVpcConfig

    -- ** ComprehendArn
    , ComprehendArn (..)

    -- ** PartOfSpeechTagType
    , PartOfSpeechTagType (..)

    -- ** EntityTypeName
    , EntityTypeName (..)

    -- ** InputDataConfig
    , InputDataConfig (..)
    , mkInputDataConfig
    , idcS3Uri
    , idcInputFormat

    -- ** DocumentClassifierInputDataConfig
    , DocumentClassifierInputDataConfig (..)
    , mkDocumentClassifierInputDataConfig
    , dcidcAugmentedManifests
    , dcidcDataFormat
    , dcidcLabelDelimiter
    , dcidcS3Uri

    -- ** VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnets

    -- ** EntityTypesListItem
    , EntityTypesListItem (..)
    , mkEntityTypesListItem
    , etliType

    -- ** RedactionConfig
    , RedactionConfig (..)
    , mkRedactionConfig
    , rcMaskCharacter
    , rcMaskMode
    , rcPiiEntityTypes

    -- ** SentimentType
    , SentimentType (..)

    -- ** KmsKeyId
    , KmsKeyId (..)

    -- ** EntityRecognizerMetadataEntityTypesListItem
    , EntityRecognizerMetadataEntityTypesListItem (..)
    , mkEntityRecognizerMetadataEntityTypesListItem
    , ermetliEvaluationMetrics
    , ermetliNumberOfTrainMentions
    , ermetliType

    -- ** ComprehendEndpointArn
    , ComprehendEndpointArn (..)

    -- ** SentimentDetectionJobFilter
    , SentimentDetectionJobFilter (..)
    , mkSentimentDetectionJobFilter
    , sdjfJobName
    , sdjfJobStatus
    , sdjfSubmitTimeAfter
    , sdjfSubmitTimeBefore

    -- ** SentimentScore
    , SentimentScore (..)
    , mkSentimentScore
    , ssMixed
    , ssNegative
    , ssNeutral
    , ssPositive

    -- ** DocumentClassificationJobProperties
    , DocumentClassificationJobProperties (..)
    , mkDocumentClassificationJobProperties
    , dcjpDataAccessRoleArn
    , dcjpDocumentClassifierArn
    , dcjpEndTime
    , dcjpInputDataConfig
    , dcjpJobId
    , dcjpJobName
    , dcjpJobStatus
    , dcjpMessage
    , dcjpOutputDataConfig
    , dcjpSubmitTime
    , dcjpVolumeKmsKeyId
    , dcjpVpcConfig

    -- ** EndpointStatus
    , EndpointStatus (..)

    -- ** TopicsDetectionJobFilter
    , TopicsDetectionJobFilter (..)
    , mkTopicsDetectionJobFilter
    , tdjfJobName
    , tdjfJobStatus
    , tdjfSubmitTimeAfter
    , tdjfSubmitTimeBefore

    -- ** InputFormat
    , InputFormat (..)

    -- ** EntitiesDetectionJobFilter
    , EntitiesDetectionJobFilter (..)
    , mkEntitiesDetectionJobFilter
    , edjffJobName
    , edjffJobStatus
    , edjffSubmitTimeAfter
    , edjffSubmitTimeBefore

    -- ** OutputDataConfig
    , OutputDataConfig (..)
    , mkOutputDataConfig
    , odcS3Uri
    , odcKmsKeyId

    -- ** PiiEntitiesDetectionMaskMode
    , PiiEntitiesDetectionMaskMode (..)

    -- ** LabelDelimiter
    , LabelDelimiter (..)

    -- ** DocumentClassifierOutputDataConfig
    , DocumentClassifierOutputDataConfig (..)
    , mkDocumentClassifierOutputDataConfig
    , dcodcKmsKeyId
    , dcodcS3Uri

    -- ** TagKey
    , TagKey (..)

    -- ** DocumentClassifierFilter
    , DocumentClassifierFilter (..)
    , mkDocumentClassifierFilter
    , dcfStatus
    , dcfSubmitTimeAfter
    , dcfSubmitTimeBefore

    -- ** DominantLanguageDetectionJobFilter
    , DominantLanguageDetectionJobFilter (..)
    , mkDominantLanguageDetectionJobFilter
    , dldjfJobName
    , dldjfJobStatus
    , dldjfSubmitTimeAfter
    , dldjfSubmitTimeBefore

    -- ** BatchDetectSentimentItemResult
    , BatchDetectSentimentItemResult (..)
    , mkBatchDetectSentimentItemResult
    , bIndex
    , bSentiment
    , bSentimentScore

    -- ** JobStatus
    , JobStatus (..)

    -- ** TopicsDetectionJobProperties
    , TopicsDetectionJobProperties (..)
    , mkTopicsDetectionJobProperties
    , tdjpDataAccessRoleArn
    , tdjpEndTime
    , tdjpInputDataConfig
    , tdjpJobId
    , tdjpJobName
    , tdjpJobStatus
    , tdjpMessage
    , tdjpNumberOfTopics
    , tdjpOutputDataConfig
    , tdjpSubmitTime
    , tdjpVolumeKmsKeyId
    , tdjpVpcConfig

    -- ** AugmentedManifestsListItem
    , AugmentedManifestsListItem (..)
    , mkAugmentedManifestsListItem
    , amliS3Uri
    , amliAttributeNames

    -- ** Entity
    , Entity (..)
    , mkEntity
    , eBeginOffset
    , eEndOffset
    , eScore
    , eText
    , eType

    -- ** PiiEntityType
    , PiiEntityType (..)

    -- ** ModelStatus
    , ModelStatus (..)

    -- ** S3Uri
    , S3Uri (..)

    -- ** EventsDetectionJobProperties
    , EventsDetectionJobProperties (..)
    , mkEventsDetectionJobProperties
    , edjpDataAccessRoleArn
    , edjpEndTime
    , edjpInputDataConfig
    , edjpJobId
    , edjpJobName
    , edjpJobStatus
    , edjpLanguageCode
    , edjpMessage
    , edjpOutputDataConfig
    , edjpSubmitTime
    , edjpTargetEventTypes

    -- ** ClassifierEvaluationMetrics
    , ClassifierEvaluationMetrics (..)
    , mkClassifierEvaluationMetrics
    , cemAccuracy
    , cemF1Score
    , cemHammingLoss
    , cemMicroF1Score
    , cemMicroPrecision
    , cemMicroRecall
    , cemPrecision
    , cemRecall

    -- ** EventTypeString
    , EventTypeString (..)

    -- ** DominantLanguageDetectionJobProperties
    , DominantLanguageDetectionJobProperties (..)
    , mkDominantLanguageDetectionJobProperties
    , dldjpDataAccessRoleArn
    , dldjpEndTime
    , dldjpInputDataConfig
    , dldjpJobId
    , dldjpJobName
    , dldjpJobStatus
    , dldjpMessage
    , dldjpOutputDataConfig
    , dldjpSubmitTime
    , dldjpVolumeKmsKeyId
    , dldjpVpcConfig

    -- ** EntityRecognizerInputDataConfig
    , EntityRecognizerInputDataConfig (..)
    , mkEntityRecognizerInputDataConfig
    , eridcEntityTypes
    , eridcAnnotations
    , eridcAugmentedManifests
    , eridcDataFormat
    , eridcDocuments
    , eridcEntityList

    -- ** EntityRecognizerDocuments
    , EntityRecognizerDocuments (..)
    , mkEntityRecognizerDocuments
    , erdS3Uri

    -- ** DocumentClassifierProperties
    , DocumentClassifierProperties (..)
    , mkDocumentClassifierProperties
    , dcpClassifierMetadata
    , dcpDataAccessRoleArn
    , dcpDocumentClassifierArn
    , dcpEndTime
    , dcpInputDataConfig
    , dcpLanguageCode
    , dcpMessage
    , dcpMode
    , dcpOutputDataConfig
    , dcpStatus
    , dcpSubmitTime
    , dcpTrainingEndTime
    , dcpTrainingStartTime
    , dcpVolumeKmsKeyId
    , dcpVpcConfig

    -- ** DataAccessRoleArn
    , DataAccessRoleArn (..)

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** VolumeKmsKeyId
    , VolumeKmsKeyId (..)

    -- ** EndpointArn
    , EndpointArn (..)

    -- ** Message
    , Message (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** ModelArn
    , ModelArn (..)

    -- ** DocumentClassifierName
    , DocumentClassifierName (..)

    -- ** RecognizerName
    , RecognizerName (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Waiters
import Network.AWS.Comprehend.BatchDetectSentiment
import Network.AWS.Comprehend.DeleteEntityRecognizer
import Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
import Network.AWS.Comprehend.ListEntitiesDetectionJobs
import Network.AWS.Comprehend.CreateEndpoint
import Network.AWS.Comprehend.StopEventsDetectionJob
import Network.AWS.Comprehend.StartSentimentDetectionJob
import Network.AWS.Comprehend.BatchDetectSyntax
import Network.AWS.Comprehend.StartTopicsDetectionJob
import Network.AWS.Comprehend.DescribeEventsDetectionJob
import Network.AWS.Comprehend.DeleteEndpoint
import Network.AWS.Comprehend.UpdateEndpoint
import Network.AWS.Comprehend.ListTagsForResource
import Network.AWS.Comprehend.BatchDetectKeyPhrases
import Network.AWS.Comprehend.DescribeSentimentDetectionJob
import Network.AWS.Comprehend.StartEntitiesDetectionJob
import Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
import Network.AWS.Comprehend.DescribeEntityRecognizer
import Network.AWS.Comprehend.DetectSentiment
import Network.AWS.Comprehend.StartDominantLanguageDetectionJob
import Network.AWS.Comprehend.StopTrainingDocumentClassifier
import Network.AWS.Comprehend.DescribeDocumentClassificationJob
import Network.AWS.Comprehend.ListEventsDetectionJobs
import Network.AWS.Comprehend.BatchDetectEntities
import Network.AWS.Comprehend.CreateEntityRecognizer
import Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
import Network.AWS.Comprehend.CreateDocumentClassifier
import Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
import Network.AWS.Comprehend.ListEntityRecognizers
import Network.AWS.Comprehend.StopSentimentDetectionJob
import Network.AWS.Comprehend.DetectDominantLanguage
import Network.AWS.Comprehend.ClassifyDocument
import Network.AWS.Comprehend.StartEventsDetectionJob
import Network.AWS.Comprehend.DescribeTopicsDetectionJob
import Network.AWS.Comprehend.ListDocumentClassificationJobs
import Network.AWS.Comprehend.DetectPiiEntities
import Network.AWS.Comprehend.ListEndpoints
import Network.AWS.Comprehend.DetectEntities
import Network.AWS.Comprehend.DescribeDocumentClassifier
import Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
import Network.AWS.Comprehend.StopEntitiesDetectionJob
import Network.AWS.Comprehend.StopTrainingEntityRecognizer
import Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
import Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
import Network.AWS.Comprehend.DescribeEntitiesDetectionJob
import Network.AWS.Comprehend.StopDominantLanguageDetectionJob
import Network.AWS.Comprehend.TagResource
import Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
import Network.AWS.Comprehend.ListTopicsDetectionJobs
import Network.AWS.Comprehend.UntagResource
import Network.AWS.Comprehend.BatchDetectDominantLanguage
import Network.AWS.Comprehend.StartDocumentClassificationJob
import Network.AWS.Comprehend.DetectKeyPhrases
import Network.AWS.Comprehend.DetectSyntax
import Network.AWS.Comprehend.DescribeEndpoint
import Network.AWS.Comprehend.ListSentimentDetectionJobs
import Network.AWS.Comprehend.DeleteDocumentClassifier
import Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
import Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
import Network.AWS.Comprehend.ListDocumentClassifiers
import qualified Network.AWS.Prelude as Lude

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

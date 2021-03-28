{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the Amazon Rekognition API reference.
module Network.AWS.Rekognition
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** VideoTooLargeException
    , _VideoTooLargeException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidImageFormatException
    , _InvalidImageFormatException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** InvalidS3ObjectException
    , _InvalidS3ObjectException

    -- ** ProvisionedThroughputExceededException
    , _ProvisionedThroughputExceededException

    -- ** ImageTooLargeException
    , _ImageTooLargeException

    -- ** ServiceQuotaExceededException
    , _ServiceQuotaExceededException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServerError
    , _InternalServerError

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** ResourceNotReadyException
    , _ResourceNotReadyException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** HumanLoopQuotaExceededException
    , _HumanLoopQuotaExceededException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** ProjectVersionRunning
    , mkProjectVersionRunning

    -- ** ProjectVersionTrainingCompleted
    , mkProjectVersionTrainingCompleted

    -- * Operations
    -- $operations

    -- ** DetectProtectiveEquipment 
    , module Network.AWS.Rekognition.DetectProtectiveEquipment

    -- ** DeleteProject 
    , module Network.AWS.Rekognition.DeleteProject

    -- ** StartCelebrityRecognition 
    , module Network.AWS.Rekognition.StartCelebrityRecognition

    -- ** GetPersonTracking 
    , module Network.AWS.Rekognition.GetPersonTracking

    -- ** GetTextDetection 
    , module Network.AWS.Rekognition.GetTextDetection

    -- ** StartSegmentDetection 
    , module Network.AWS.Rekognition.StartSegmentDetection

    -- ** ListCollections (Paginated)
    , module Network.AWS.Rekognition.ListCollections

    -- ** StartProjectVersion 
    , module Network.AWS.Rekognition.StartProjectVersion

    -- ** DeleteCollection 
    , module Network.AWS.Rekognition.DeleteCollection

    -- ** CreateCollection 
    , module Network.AWS.Rekognition.CreateCollection

    -- ** StopStreamProcessor 
    , module Network.AWS.Rekognition.StopStreamProcessor

    -- ** DetectLabels 
    , module Network.AWS.Rekognition.DetectLabels

    -- ** StartContentModeration 
    , module Network.AWS.Rekognition.StartContentModeration

    -- ** SearchFacesByImage 
    , module Network.AWS.Rekognition.SearchFacesByImage

    -- ** ListStreamProcessors (Paginated)
    , module Network.AWS.Rekognition.ListStreamProcessors

    -- ** DescribeCollection 
    , module Network.AWS.Rekognition.DescribeCollection

    -- ** DeleteProjectVersion 
    , module Network.AWS.Rekognition.DeleteProjectVersion

    -- ** DescribeProjectVersions (Paginated)
    , module Network.AWS.Rekognition.DescribeProjectVersions

    -- ** RecognizeCelebrities 
    , module Network.AWS.Rekognition.RecognizeCelebrities

    -- ** DetectCustomLabels 
    , module Network.AWS.Rekognition.DetectCustomLabels

    -- ** GetFaceSearch 
    , module Network.AWS.Rekognition.GetFaceSearch

    -- ** StartLabelDetection 
    , module Network.AWS.Rekognition.StartLabelDetection

    -- ** SearchFaces 
    , module Network.AWS.Rekognition.SearchFaces

    -- ** IndexFaces 
    , module Network.AWS.Rekognition.IndexFaces

    -- ** GetLabelDetection 
    , module Network.AWS.Rekognition.GetLabelDetection

    -- ** StopProjectVersion 
    , module Network.AWS.Rekognition.StopProjectVersion

    -- ** DescribeStreamProcessor 
    , module Network.AWS.Rekognition.DescribeStreamProcessor

    -- ** StartFaceSearch 
    , module Network.AWS.Rekognition.StartFaceSearch

    -- ** StartTextDetection 
    , module Network.AWS.Rekognition.StartTextDetection

    -- ** StartPersonTracking 
    , module Network.AWS.Rekognition.StartPersonTracking

    -- ** GetCelebrityRecognition 
    , module Network.AWS.Rekognition.GetCelebrityRecognition

    -- ** StartStreamProcessor 
    , module Network.AWS.Rekognition.StartStreamProcessor

    -- ** DetectText 
    , module Network.AWS.Rekognition.DetectText

    -- ** GetSegmentDetection 
    , module Network.AWS.Rekognition.GetSegmentDetection

    -- ** CompareFaces 
    , module Network.AWS.Rekognition.CompareFaces

    -- ** DetectFaces 
    , module Network.AWS.Rekognition.DetectFaces

    -- ** GetFaceDetection 
    , module Network.AWS.Rekognition.GetFaceDetection

    -- ** ListFaces (Paginated)
    , module Network.AWS.Rekognition.ListFaces

    -- ** CreateProjectVersion 
    , module Network.AWS.Rekognition.CreateProjectVersion

    -- ** DescribeProjects (Paginated)
    , module Network.AWS.Rekognition.DescribeProjects

    -- ** GetContentModeration 
    , module Network.AWS.Rekognition.GetContentModeration

    -- ** DeleteFaces 
    , module Network.AWS.Rekognition.DeleteFaces

    -- ** GetCelebrityInfo 
    , module Network.AWS.Rekognition.GetCelebrityInfo

    -- ** DeleteStreamProcessor 
    , module Network.AWS.Rekognition.DeleteStreamProcessor

    -- ** DetectModerationLabels 
    , module Network.AWS.Rekognition.DetectModerationLabels

    -- ** CreateStreamProcessor 
    , module Network.AWS.Rekognition.CreateStreamProcessor

    -- ** StartFaceDetection 
    , module Network.AWS.Rekognition.StartFaceDetection

    -- ** CreateProject 
    , module Network.AWS.Rekognition.CreateProject

    -- * Types

    -- ** ProtectiveEquipmentBodyPart
    , ProtectiveEquipmentBodyPart (..)
    , mkProtectiveEquipmentBodyPart
    , pebpConfidence
    , pebpEquipmentDetections
    , pebpName

    -- ** Parent
    , Parent (..)
    , mkParent
    , pName

    -- ** FaceId
    , FaceId (..)

    -- ** SegmentDetection
    , SegmentDetection (..)
    , mkSegmentDetection
    , sdDurationMillis
    , sdDurationSMPTE
    , sdEndTimecodeSMPTE
    , sdEndTimestampMillis
    , sdShotSegment
    , sdStartTimecodeSMPTE
    , sdStartTimestampMillis
    , sdTechnicalCueSegment
    , sdType

    -- ** StreamProcessorInput
    , StreamProcessorInput (..)
    , mkStreamProcessorInput
    , spiKinesisVideoStream

    -- ** KinesisVideoArn
    , KinesisVideoArn (..)

    -- ** FaceRecord
    , FaceRecord (..)
    , mkFaceRecord
    , frFace
    , frFaceDetail

    -- ** AgeRange
    , AgeRange (..)
    , mkAgeRange
    , arHigh
    , arLow

    -- ** Attribute
    , Attribute (..)

    -- ** Summary
    , Summary (..)
    , mkSummary
    , sS3Object

    -- ** Sunglasses
    , Sunglasses (..)
    , mkSunglasses
    , sfConfidence
    , sfValue

    -- ** CelebrityRecognition
    , CelebrityRecognition (..)
    , mkCelebrityRecognition
    , crCelebrity
    , crTimestamp

    -- ** GenderType
    , GenderType (..)

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** MouthOpen
    , MouthOpen (..)
    , mkMouthOpen
    , moConfidence
    , moValue

    -- ** StartSegmentDetectionFilters
    , StartSegmentDetectionFilters (..)
    , mkStartSegmentDetectionFilters
    , ssdfShotFilter
    , ssdfTechnicalCueFilter

    -- ** S3ObjectVersion
    , S3ObjectVersion (..)

    -- ** BoundingBox
    , BoundingBox (..)
    , mkBoundingBox
    , bbHeight
    , bbLeft
    , bbTop
    , bbWidth

    -- ** ExternalImageId
    , ExternalImageId (..)

    -- ** EvaluationResult
    , EvaluationResult (..)
    , mkEvaluationResult
    , erF1Score
    , erSummary

    -- ** StartTechnicalCueDetectionFilter
    , StartTechnicalCueDetectionFilter (..)
    , mkStartTechnicalCueDetectionFilter
    , stcdfMinSegmentConfidence

    -- ** BodyPart
    , BodyPart (..)

    -- ** Image
    , Image (..)
    , mkImage
    , iBytes
    , iS3Object

    -- ** S3KeyPrefix
    , S3KeyPrefix (..)

    -- ** SNSTopicArn
    , SNSTopicArn (..)

    -- ** ModerationLabel
    , ModerationLabel (..)
    , mkModerationLabel
    , mlConfidence
    , mlName
    , mlParentName

    -- ** ComparedFace
    , ComparedFace (..)
    , mkComparedFace
    , cfBoundingBox
    , cfConfidence
    , cfLandmarks
    , cfPose
    , cfQuality

    -- ** Landmark
    , Landmark (..)
    , mkLandmark
    , lType
    , lX
    , lY

    -- ** ImageQuality
    , ImageQuality (..)
    , mkImageQuality
    , iqBrightness
    , iqSharpness

    -- ** JobId
    , JobId (..)

    -- ** VideoJobStatus
    , VideoJobStatus (..)

    -- ** QualityFilter
    , QualityFilter (..)

    -- ** VersionName
    , VersionName (..)

    -- ** S3Object
    , S3Object (..)
    , mkS3Object
    , soBucket
    , soName
    , soVersion

    -- ** SegmentTypeInfo
    , SegmentTypeInfo (..)
    , mkSegmentTypeInfo
    , stiModelVersion
    , stiType

    -- ** JobTag
    , JobTag (..)

    -- ** StreamProcessorName
    , StreamProcessorName (..)

    -- ** GroundTruthManifest
    , GroundTruthManifest (..)
    , mkGroundTruthManifest
    , gtmS3Object

    -- ** StreamProcessorOutput
    , StreamProcessorOutput (..)
    , mkStreamProcessorOutput
    , spoKinesisDataStream

    -- ** UnindexedFace
    , UnindexedFace (..)
    , mkUnindexedFace
    , ufFaceDetail
    , ufReasons

    -- ** Asset
    , Asset (..)
    , mkAsset
    , aGroundTruthManifest

    -- ** FaceDetail
    , FaceDetail (..)
    , mkFaceDetail
    , fdAgeRange
    , fdBeard
    , fdBoundingBox
    , fdConfidence
    , fdEmotions
    , fdEyeglasses
    , fdEyesOpen
    , fdGender
    , fdLandmarks
    , fdMouthOpen
    , fdMustache
    , fdPose
    , fdQuality
    , fdSmile
    , fdSunglasses

    -- ** HumanLoopActivationOutput
    , HumanLoopActivationOutput (..)
    , mkHumanLoopActivationOutput
    , hlaoHumanLoopActivationConditionsEvaluationResults
    , hlaoHumanLoopActivationReasons
    , hlaoHumanLoopArn

    -- ** TechnicalCueType
    , TechnicalCueType (..)

    -- ** CollectionId
    , CollectionId (..)

    -- ** PersonTrackingSortBy
    , PersonTrackingSortBy (..)

    -- ** TechnicalCueSegment
    , TechnicalCueSegment (..)
    , mkTechnicalCueSegment
    , tcsConfidence
    , tcsType

    -- ** ProjectVersionStatus
    , ProjectVersionStatus (..)

    -- ** SegmentType
    , SegmentType (..)

    -- ** HumanLoopActivationReason
    , HumanLoopActivationReason (..)

    -- ** Url
    , Url (..)

    -- ** Emotion
    , Emotion (..)
    , mkEmotion
    , eConfidence
    , eType

    -- ** Celebrity
    , Celebrity (..)
    , mkCelebrity
    , cFace
    , cId
    , cMatchConfidence
    , cName
    , cUrls

    -- ** NotificationChannel
    , NotificationChannel (..)
    , mkNotificationChannel
    , ncSNSTopicArn
    , ncRoleArn

    -- ** CompareFacesMatch
    , CompareFacesMatch (..)
    , mkCompareFacesMatch
    , cfmFace
    , cfmSimilarity

    -- ** StreamProcessorStatus
    , StreamProcessorStatus (..)

    -- ** HumanLoopArn
    , HumanLoopArn (..)

    -- ** TestingData
    , TestingData (..)
    , mkTestingData
    , tdAssets
    , tdAutoCreate

    -- ** ContentClassifier
    , ContentClassifier (..)

    -- ** TextTypes
    , TextTypes (..)

    -- ** HumanLoopConfig
    , HumanLoopConfig (..)
    , mkHumanLoopConfig
    , hlcHumanLoopName
    , hlcFlowDefinitionArn
    , hlcDataAttributes

    -- ** ProjectStatus
    , ProjectStatus (..)

    -- ** CoversBodyPart
    , CoversBodyPart (..)
    , mkCoversBodyPart
    , cbpConfidence
    , cbpValue

    -- ** LabelDetection
    , LabelDetection (..)
    , mkLabelDetection
    , ldLabel
    , ldTimestamp

    -- ** Pose
    , Pose (..)
    , mkPose
    , pPitch
    , pRoll
    , pYaw

    -- ** Video
    , Video (..)
    , mkVideo
    , vS3Object

    -- ** KinesisDataStream
    , KinesisDataStream (..)
    , mkKinesisDataStream
    , kdsArn

    -- ** Reason
    , Reason (..)

    -- ** VideoMetadata
    , VideoMetadata (..)
    , mkVideoMetadata
    , vmCodec
    , vmDurationMillis
    , vmFormat
    , vmFrameHeight
    , vmFrameRate
    , vmFrameWidth

    -- ** ContentModerationSortBy
    , ContentModerationSortBy (..)

    -- ** EmotionName
    , EmotionName (..)

    -- ** Point
    , Point (..)
    , mkPoint
    , pX
    , pY

    -- ** HumanLoopName
    , HumanLoopName (..)

    -- ** PersonDetail
    , PersonDetail (..)
    , mkPersonDetail
    , pdBoundingBox
    , pdFace
    , pdIndex

    -- ** HumanLoopDataAttributes
    , HumanLoopDataAttributes (..)
    , mkHumanLoopDataAttributes
    , hldaContentClassifiers

    -- ** StreamProcessorSettings
    , StreamProcessorSettings (..)
    , mkStreamProcessorSettings
    , spsFaceSearch

    -- ** ProtectiveEquipmentSummary
    , ProtectiveEquipmentSummary (..)
    , mkProtectiveEquipmentSummary
    , pesPersonsIndeterminate
    , pesPersonsWithRequiredEquipment
    , pesPersonsWithoutRequiredEquipment

    -- ** StartShotDetectionFilter
    , StartShotDetectionFilter (..)
    , mkStartShotDetectionFilter
    , ssdfMinSegmentConfidence

    -- ** TestingDataResult
    , TestingDataResult (..)
    , mkTestingDataResult
    , tdrInput
    , tdrOutput
    , tdrValidation

    -- ** ProtectiveEquipmentSummarizationAttributes
    , ProtectiveEquipmentSummarizationAttributes (..)
    , mkProtectiveEquipmentSummarizationAttributes
    , pesaMinConfidence
    , pesaRequiredEquipmentTypes

    -- ** StreamProcessorArn
    , StreamProcessorArn (..)

    -- ** StatusMessage
    , StatusMessage (..)

    -- ** Gender
    , Gender (..)
    , mkGender
    , gConfidence
    , gValue

    -- ** RegionOfInterest
    , RegionOfInterest (..)
    , mkRegionOfInterest
    , roiBoundingBox

    -- ** DetectionFilter
    , DetectionFilter (..)
    , mkDetectionFilter
    , dfMinBoundingBoxHeight
    , dfMinBoundingBoxWidth
    , dfMinConfidence

    -- ** FaceSearchSettings
    , FaceSearchSettings (..)
    , mkFaceSearchSettings
    , fssCollectionId
    , fssFaceMatchThreshold

    -- ** CelebrityDetail
    , CelebrityDetail (..)
    , mkCelebrityDetail
    , cdBoundingBox
    , cdConfidence
    , cdFace
    , cdId
    , cdName
    , cdUrls

    -- ** LandmarkType
    , LandmarkType (..)

    -- ** FlowDefinitionArn
    , FlowDefinitionArn (..)

    -- ** ContentModerationDetection
    , ContentModerationDetection (..)
    , mkContentModerationDetection
    , cmdModerationLabel
    , cmdTimestamp

    -- ** ImageId
    , ImageId (..)

    -- ** PersonDetection
    , PersonDetection (..)
    , mkPersonDetection
    , pdPerson
    , pdTimestamp

    -- ** FaceMatch
    , FaceMatch (..)
    , mkFaceMatch
    , fmFace
    , fmSimilarity

    -- ** TextDetection
    , TextDetection (..)
    , mkTextDetection
    , tdConfidence
    , tdDetectedText
    , tdGeometry
    , tdId
    , tdParentId
    , tdType

    -- ** ProjectName
    , ProjectName (..)

    -- ** Geometry
    , Geometry (..)
    , mkGeometry
    , gBoundingBox
    , gPolygon

    -- ** ProjectVersionArn
    , ProjectVersionArn (..)

    -- ** StreamProcessor
    , StreamProcessor (..)
    , mkStreamProcessor
    , spName
    , spStatus

    -- ** OutputConfig
    , OutputConfig (..)
    , mkOutputConfig
    , ocS3Bucket
    , ocS3KeyPrefix

    -- ** EyeOpen
    , EyeOpen (..)
    , mkEyeOpen
    , eoConfidence
    , eoValue

    -- ** EquipmentDetection
    , EquipmentDetection (..)
    , mkEquipmentDetection
    , edBoundingBox
    , edConfidence
    , edCoversBodyPart
    , edType

    -- ** KinesisVideoStream
    , KinesisVideoStream (..)
    , mkKinesisVideoStream
    , kvsArn

    -- ** CustomLabel
    , CustomLabel (..)
    , mkCustomLabel
    , clConfidence
    , clGeometry
    , clName

    -- ** Eyeglasses
    , Eyeglasses (..)
    , mkEyeglasses
    , efConfidence
    , efValue

    -- ** CelebrityRecognitionSortBy
    , CelebrityRecognitionSortBy (..)

    -- ** Beard
    , Beard (..)
    , mkBeard
    , bConfidence
    , bValue

    -- ** Mustache
    , Mustache (..)
    , mkMustache
    , mConfidence
    , mValue

    -- ** ExtendedPaginationToken
    , ExtendedPaginationToken (..)

    -- ** TrainingData
    , TrainingData (..)
    , mkTrainingData
    , tAssets

    -- ** ComparedSourceImageFace
    , ComparedSourceImageFace (..)
    , mkComparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

    -- ** ProjectArn
    , ProjectArn (..)

    -- ** LabelDetectionSortBy
    , LabelDetectionSortBy (..)

    -- ** OrientationCorrection
    , OrientationCorrection (..)

    -- ** HumanLoopActivationConditionsEvaluationResults
    , HumanLoopActivationConditionsEvaluationResults (..)

    -- ** ShotSegment
    , ShotSegment (..)
    , mkShotSegment
    , ssConfidence
    , ssIndex

    -- ** AudioMetadata
    , AudioMetadata (..)
    , mkAudioMetadata
    , amCodec
    , amDurationMillis
    , amNumberOfChannels
    , amSampleRate

    -- ** ValidationData
    , ValidationData (..)
    , mkValidationData
    , vdAssets

    -- ** TextDetectionResult
    , TextDetectionResult (..)
    , mkTextDetectionResult
    , tdrTextDetection
    , tdrTimestamp

    -- ** ProjectDescription
    , ProjectDescription (..)
    , mkProjectDescription
    , pdCreationTimestamp
    , pdProjectArn
    , pdStatus

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** ProtectiveEquipmentType
    , ProtectiveEquipmentType (..)

    -- ** FaceSearchSortBy
    , FaceSearchSortBy (..)

    -- ** Smile
    , Smile (..)
    , mkSmile
    , sConfidence
    , sValue

    -- ** PersonMatch
    , PersonMatch (..)
    , mkPersonMatch
    , pmFaceMatches
    , pmPerson
    , pmTimestamp

    -- ** Label
    , Label (..)
    , mkLabel
    , lConfidence
    , lInstances
    , lName
    , lParents

    -- ** FaceDetection
    , FaceDetection (..)
    , mkFaceDetection
    , fdFace
    , fdTimestamp

    -- ** S3Bucket
    , S3Bucket (..)

    -- ** FaceAttributes
    , FaceAttributes (..)

    -- ** ProtectiveEquipmentPerson
    , ProtectiveEquipmentPerson (..)
    , mkProtectiveEquipmentPerson
    , pepBodyParts
    , pepBoundingBox
    , pepConfidence
    , pepId

    -- ** DetectTextFilters
    , DetectTextFilters (..)
    , mkDetectTextFilters
    , dtfRegionsOfInterest
    , dtfWordFilter

    -- ** ProjectVersionDescription
    , ProjectVersionDescription (..)
    , mkProjectVersionDescription
    , pvdBillableTrainingTimeInSeconds
    , pvdCreationTimestamp
    , pvdEvaluationResult
    , pvdManifestSummary
    , pvdMinInferenceUnits
    , pvdOutputConfig
    , pvdProjectVersionArn
    , pvdStatus
    , pvdStatusMessage
    , pvdTestingDataResult
    , pvdTrainingDataResult
    , pvdTrainingEndTimestamp

    -- ** Instance
    , Instance (..)
    , mkInstance
    , iBoundingBox
    , iConfidence

    -- ** TrainingDataResult
    , TrainingDataResult (..)
    , mkTrainingDataResult
    , tInput
    , tOutput
    , tValidation

    -- ** Face
    , Face (..)
    , mkFace
    , fBoundingBox
    , fConfidence
    , fExternalImageId
    , fFaceId
    , fImageId

    -- ** StartTextDetectionFilters
    , StartTextDetectionFilters (..)
    , mkStartTextDetectionFilters
    , stdfRegionsOfInterest
    , stdfWordFilter

    -- ** RoleArn
    , RoleArn (..)

    -- ** NextToken
    , NextToken (..)

    -- ** DurationSMPTE
    , DurationSMPTE (..)

    -- ** EndTimecodeSMPTE
    , EndTimecodeSMPTE (..)

    -- ** StartTimecodeSMPTE
    , StartTimecodeSMPTE (..)

    -- ** Bucket
    , Bucket (..)

    -- ** Name
    , Name (..)

    -- ** Id
    , Id (..)

    -- ** Arn
    , Arn (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Waiters
import Network.AWS.Rekognition.DetectProtectiveEquipment
import Network.AWS.Rekognition.DeleteProject
import Network.AWS.Rekognition.StartCelebrityRecognition
import Network.AWS.Rekognition.GetPersonTracking
import Network.AWS.Rekognition.GetTextDetection
import Network.AWS.Rekognition.StartSegmentDetection
import Network.AWS.Rekognition.ListCollections
import Network.AWS.Rekognition.StartProjectVersion
import Network.AWS.Rekognition.DeleteCollection
import Network.AWS.Rekognition.CreateCollection
import Network.AWS.Rekognition.StopStreamProcessor
import Network.AWS.Rekognition.DetectLabels
import Network.AWS.Rekognition.StartContentModeration
import Network.AWS.Rekognition.SearchFacesByImage
import Network.AWS.Rekognition.ListStreamProcessors
import Network.AWS.Rekognition.DescribeCollection
import Network.AWS.Rekognition.DeleteProjectVersion
import Network.AWS.Rekognition.DescribeProjectVersions
import Network.AWS.Rekognition.RecognizeCelebrities
import Network.AWS.Rekognition.DetectCustomLabels
import Network.AWS.Rekognition.GetFaceSearch
import Network.AWS.Rekognition.StartLabelDetection
import Network.AWS.Rekognition.SearchFaces
import Network.AWS.Rekognition.IndexFaces
import Network.AWS.Rekognition.GetLabelDetection
import Network.AWS.Rekognition.StopProjectVersion
import Network.AWS.Rekognition.DescribeStreamProcessor
import Network.AWS.Rekognition.StartFaceSearch
import Network.AWS.Rekognition.StartTextDetection
import Network.AWS.Rekognition.StartPersonTracking
import Network.AWS.Rekognition.GetCelebrityRecognition
import Network.AWS.Rekognition.StartStreamProcessor
import Network.AWS.Rekognition.DetectText
import Network.AWS.Rekognition.GetSegmentDetection
import Network.AWS.Rekognition.CompareFaces
import Network.AWS.Rekognition.DetectFaces
import Network.AWS.Rekognition.GetFaceDetection
import Network.AWS.Rekognition.ListFaces
import Network.AWS.Rekognition.CreateProjectVersion
import Network.AWS.Rekognition.DescribeProjects
import Network.AWS.Rekognition.GetContentModeration
import Network.AWS.Rekognition.DeleteFaces
import Network.AWS.Rekognition.GetCelebrityInfo
import Network.AWS.Rekognition.DeleteStreamProcessor
import Network.AWS.Rekognition.DetectModerationLabels
import Network.AWS.Rekognition.CreateStreamProcessor
import Network.AWS.Rekognition.StartFaceDetection
import Network.AWS.Rekognition.CreateProject
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Rekognition'.
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

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _AccessDeniedException
    , _VideoTooLargeException
    , _InvalidParameterException
    , _InvalidImageFormatException
    , _ResourceAlreadyExistsException
    , _InvalidS3ObjectException
    , _ProvisionedThroughputExceededException
    , _ImageTooLargeException
    , _ServiceQuotaExceededException
    , _ThrottlingException
    , _InternalServerError
    , _IdempotentParameterMismatchException
    , _ResourceNotReadyException
    , _ResourceNotFoundException
    , _HumanLoopQuotaExceededException
    , _InvalidPaginationTokenException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ProtectiveEquipmentBodyPart
    , ProtectiveEquipmentBodyPart (..)
    , mkProtectiveEquipmentBodyPart
    , pebpConfidence
    , pebpEquipmentDetections
    , pebpName

    -- * Parent
    , Parent (..)
    , mkParent
    , pName

    -- * FaceId
    , FaceId (..)

    -- * SegmentDetection
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

    -- * StreamProcessorInput
    , StreamProcessorInput (..)
    , mkStreamProcessorInput
    , spiKinesisVideoStream

    -- * KinesisVideoArn
    , KinesisVideoArn (..)

    -- * FaceRecord
    , FaceRecord (..)
    , mkFaceRecord
    , frFace
    , frFaceDetail

    -- * AgeRange
    , AgeRange (..)
    , mkAgeRange
    , arHigh
    , arLow

    -- * Attribute
    , Attribute (..)

    -- * Summary
    , Summary (..)
    , mkSummary
    , sS3Object

    -- * Sunglasses
    , Sunglasses (..)
    , mkSunglasses
    , sfConfidence
    , sfValue

    -- * CelebrityRecognition
    , CelebrityRecognition (..)
    , mkCelebrityRecognition
    , crCelebrity
    , crTimestamp

    -- * GenderType
    , GenderType (..)

    -- * PaginationToken
    , PaginationToken (..)

    -- * MouthOpen
    , MouthOpen (..)
    , mkMouthOpen
    , moConfidence
    , moValue

    -- * StartSegmentDetectionFilters
    , StartSegmentDetectionFilters (..)
    , mkStartSegmentDetectionFilters
    , ssdfShotFilter
    , ssdfTechnicalCueFilter

    -- * S3ObjectVersion
    , S3ObjectVersion (..)

    -- * BoundingBox
    , BoundingBox (..)
    , mkBoundingBox
    , bbHeight
    , bbLeft
    , bbTop
    , bbWidth

    -- * ExternalImageId
    , ExternalImageId (..)

    -- * EvaluationResult
    , EvaluationResult (..)
    , mkEvaluationResult
    , erF1Score
    , erSummary

    -- * StartTechnicalCueDetectionFilter
    , StartTechnicalCueDetectionFilter (..)
    , mkStartTechnicalCueDetectionFilter
    , stcdfMinSegmentConfidence

    -- * BodyPart
    , BodyPart (..)

    -- * Image
    , Image (..)
    , mkImage
    , iBytes
    , iS3Object

    -- * S3KeyPrefix
    , S3KeyPrefix (..)

    -- * SNSTopicArn
    , SNSTopicArn (..)

    -- * ModerationLabel
    , ModerationLabel (..)
    , mkModerationLabel
    , mlConfidence
    , mlName
    , mlParentName

    -- * ComparedFace
    , ComparedFace (..)
    , mkComparedFace
    , cfBoundingBox
    , cfConfidence
    , cfLandmarks
    , cfPose
    , cfQuality

    -- * Landmark
    , Landmark (..)
    , mkLandmark
    , lType
    , lX
    , lY

    -- * ImageQuality
    , ImageQuality (..)
    , mkImageQuality
    , iqBrightness
    , iqSharpness

    -- * JobId
    , JobId (..)

    -- * VideoJobStatus
    , VideoJobStatus (..)

    -- * QualityFilter
    , QualityFilter (..)

    -- * VersionName
    , VersionName (..)

    -- * S3Object
    , S3Object (..)
    , mkS3Object
    , soBucket
    , soName
    , soVersion

    -- * SegmentTypeInfo
    , SegmentTypeInfo (..)
    , mkSegmentTypeInfo
    , stiModelVersion
    , stiType

    -- * JobTag
    , JobTag (..)

    -- * StreamProcessorName
    , StreamProcessorName (..)

    -- * GroundTruthManifest
    , GroundTruthManifest (..)
    , mkGroundTruthManifest
    , gtmS3Object

    -- * StreamProcessorOutput
    , StreamProcessorOutput (..)
    , mkStreamProcessorOutput
    , spoKinesisDataStream

    -- * UnindexedFace
    , UnindexedFace (..)
    , mkUnindexedFace
    , ufFaceDetail
    , ufReasons

    -- * Asset
    , Asset (..)
    , mkAsset
    , aGroundTruthManifest

    -- * FaceDetail
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

    -- * HumanLoopActivationOutput
    , HumanLoopActivationOutput (..)
    , mkHumanLoopActivationOutput
    , hlaoHumanLoopActivationConditionsEvaluationResults
    , hlaoHumanLoopActivationReasons
    , hlaoHumanLoopArn

    -- * TechnicalCueType
    , TechnicalCueType (..)

    -- * CollectionId
    , CollectionId (..)

    -- * PersonTrackingSortBy
    , PersonTrackingSortBy (..)

    -- * TechnicalCueSegment
    , TechnicalCueSegment (..)
    , mkTechnicalCueSegment
    , tcsConfidence
    , tcsType

    -- * ProjectVersionStatus
    , ProjectVersionStatus (..)

    -- * SegmentType
    , SegmentType (..)

    -- * HumanLoopActivationReason
    , HumanLoopActivationReason (..)

    -- * Url
    , Url (..)

    -- * Emotion
    , Emotion (..)
    , mkEmotion
    , eConfidence
    , eType

    -- * Celebrity
    , Celebrity (..)
    , mkCelebrity
    , cFace
    , cId
    , cMatchConfidence
    , cName
    , cUrls

    -- * NotificationChannel
    , NotificationChannel (..)
    , mkNotificationChannel
    , ncSNSTopicArn
    , ncRoleArn

    -- * CompareFacesMatch
    , CompareFacesMatch (..)
    , mkCompareFacesMatch
    , cfmFace
    , cfmSimilarity

    -- * StreamProcessorStatus
    , StreamProcessorStatus (..)

    -- * HumanLoopArn
    , HumanLoopArn (..)

    -- * TestingData
    , TestingData (..)
    , mkTestingData
    , tdAssets
    , tdAutoCreate

    -- * ContentClassifier
    , ContentClassifier (..)

    -- * TextTypes
    , TextTypes (..)

    -- * HumanLoopConfig
    , HumanLoopConfig (..)
    , mkHumanLoopConfig
    , hlcHumanLoopName
    , hlcFlowDefinitionArn
    , hlcDataAttributes

    -- * ProjectStatus
    , ProjectStatus (..)

    -- * CoversBodyPart
    , CoversBodyPart (..)
    , mkCoversBodyPart
    , cbpConfidence
    , cbpValue

    -- * LabelDetection
    , LabelDetection (..)
    , mkLabelDetection
    , ldLabel
    , ldTimestamp

    -- * Pose
    , Pose (..)
    , mkPose
    , pPitch
    , pRoll
    , pYaw

    -- * Video
    , Video (..)
    , mkVideo
    , vS3Object

    -- * KinesisDataStream
    , KinesisDataStream (..)
    , mkKinesisDataStream
    , kdsArn

    -- * Reason
    , Reason (..)

    -- * VideoMetadata
    , VideoMetadata (..)
    , mkVideoMetadata
    , vmCodec
    , vmDurationMillis
    , vmFormat
    , vmFrameHeight
    , vmFrameRate
    , vmFrameWidth

    -- * ContentModerationSortBy
    , ContentModerationSortBy (..)

    -- * EmotionName
    , EmotionName (..)

    -- * Point
    , Point (..)
    , mkPoint
    , pX
    , pY

    -- * HumanLoopName
    , HumanLoopName (..)

    -- * PersonDetail
    , PersonDetail (..)
    , mkPersonDetail
    , pdBoundingBox
    , pdFace
    , pdIndex

    -- * HumanLoopDataAttributes
    , HumanLoopDataAttributes (..)
    , mkHumanLoopDataAttributes
    , hldaContentClassifiers

    -- * StreamProcessorSettings
    , StreamProcessorSettings (..)
    , mkStreamProcessorSettings
    , spsFaceSearch

    -- * ProtectiveEquipmentSummary
    , ProtectiveEquipmentSummary (..)
    , mkProtectiveEquipmentSummary
    , pesPersonsIndeterminate
    , pesPersonsWithRequiredEquipment
    , pesPersonsWithoutRequiredEquipment

    -- * StartShotDetectionFilter
    , StartShotDetectionFilter (..)
    , mkStartShotDetectionFilter
    , ssdfMinSegmentConfidence

    -- * TestingDataResult
    , TestingDataResult (..)
    , mkTestingDataResult
    , tdrInput
    , tdrOutput
    , tdrValidation

    -- * ProtectiveEquipmentSummarizationAttributes
    , ProtectiveEquipmentSummarizationAttributes (..)
    , mkProtectiveEquipmentSummarizationAttributes
    , pesaMinConfidence
    , pesaRequiredEquipmentTypes

    -- * StreamProcessorArn
    , StreamProcessorArn (..)

    -- * StatusMessage
    , StatusMessage (..)

    -- * Gender
    , Gender (..)
    , mkGender
    , gConfidence
    , gValue

    -- * RegionOfInterest
    , RegionOfInterest (..)
    , mkRegionOfInterest
    , roiBoundingBox

    -- * DetectionFilter
    , DetectionFilter (..)
    , mkDetectionFilter
    , dfMinBoundingBoxHeight
    , dfMinBoundingBoxWidth
    , dfMinConfidence

    -- * FaceSearchSettings
    , FaceSearchSettings (..)
    , mkFaceSearchSettings
    , fssCollectionId
    , fssFaceMatchThreshold

    -- * CelebrityDetail
    , CelebrityDetail (..)
    , mkCelebrityDetail
    , cdBoundingBox
    , cdConfidence
    , cdFace
    , cdId
    , cdName
    , cdUrls

    -- * LandmarkType
    , LandmarkType (..)

    -- * FlowDefinitionArn
    , FlowDefinitionArn (..)

    -- * ContentModerationDetection
    , ContentModerationDetection (..)
    , mkContentModerationDetection
    , cmdModerationLabel
    , cmdTimestamp

    -- * ImageId
    , ImageId (..)

    -- * PersonDetection
    , PersonDetection (..)
    , mkPersonDetection
    , pdPerson
    , pdTimestamp

    -- * FaceMatch
    , FaceMatch (..)
    , mkFaceMatch
    , fmFace
    , fmSimilarity

    -- * TextDetection
    , TextDetection (..)
    , mkTextDetection
    , tdConfidence
    , tdDetectedText
    , tdGeometry
    , tdId
    , tdParentId
    , tdType

    -- * ProjectName
    , ProjectName (..)

    -- * Geometry
    , Geometry (..)
    , mkGeometry
    , gBoundingBox
    , gPolygon

    -- * ProjectVersionArn
    , ProjectVersionArn (..)

    -- * StreamProcessor
    , StreamProcessor (..)
    , mkStreamProcessor
    , spName
    , spStatus

    -- * OutputConfig
    , OutputConfig (..)
    , mkOutputConfig
    , ocS3Bucket
    , ocS3KeyPrefix

    -- * EyeOpen
    , EyeOpen (..)
    , mkEyeOpen
    , eoConfidence
    , eoValue

    -- * EquipmentDetection
    , EquipmentDetection (..)
    , mkEquipmentDetection
    , edBoundingBox
    , edConfidence
    , edCoversBodyPart
    , edType

    -- * KinesisVideoStream
    , KinesisVideoStream (..)
    , mkKinesisVideoStream
    , kvsArn

    -- * CustomLabel
    , CustomLabel (..)
    , mkCustomLabel
    , clConfidence
    , clGeometry
    , clName

    -- * Eyeglasses
    , Eyeglasses (..)
    , mkEyeglasses
    , efConfidence
    , efValue

    -- * CelebrityRecognitionSortBy
    , CelebrityRecognitionSortBy (..)

    -- * Beard
    , Beard (..)
    , mkBeard
    , bConfidence
    , bValue

    -- * Mustache
    , Mustache (..)
    , mkMustache
    , mConfidence
    , mValue

    -- * ExtendedPaginationToken
    , ExtendedPaginationToken (..)

    -- * TrainingData
    , TrainingData (..)
    , mkTrainingData
    , tAssets

    -- * ComparedSourceImageFace
    , ComparedSourceImageFace (..)
    , mkComparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

    -- * ProjectArn
    , ProjectArn (..)

    -- * LabelDetectionSortBy
    , LabelDetectionSortBy (..)

    -- * OrientationCorrection
    , OrientationCorrection (..)

    -- * HumanLoopActivationConditionsEvaluationResults
    , HumanLoopActivationConditionsEvaluationResults (..)

    -- * ShotSegment
    , ShotSegment (..)
    , mkShotSegment
    , ssConfidence
    , ssIndex

    -- * AudioMetadata
    , AudioMetadata (..)
    , mkAudioMetadata
    , amCodec
    , amDurationMillis
    , amNumberOfChannels
    , amSampleRate

    -- * ValidationData
    , ValidationData (..)
    , mkValidationData
    , vdAssets

    -- * TextDetectionResult
    , TextDetectionResult (..)
    , mkTextDetectionResult
    , tdrTextDetection
    , tdrTimestamp

    -- * ProjectDescription
    , ProjectDescription (..)
    , mkProjectDescription
    , pdCreationTimestamp
    , pdProjectArn
    , pdStatus

    -- * ClientRequestToken
    , ClientRequestToken (..)

    -- * ProtectiveEquipmentType
    , ProtectiveEquipmentType (..)

    -- * FaceSearchSortBy
    , FaceSearchSortBy (..)

    -- * Smile
    , Smile (..)
    , mkSmile
    , sConfidence
    , sValue

    -- * PersonMatch
    , PersonMatch (..)
    , mkPersonMatch
    , pmFaceMatches
    , pmPerson
    , pmTimestamp

    -- * Label
    , Label (..)
    , mkLabel
    , lConfidence
    , lInstances
    , lName
    , lParents

    -- * FaceDetection
    , FaceDetection (..)
    , mkFaceDetection
    , fdFace
    , fdTimestamp

    -- * S3Bucket
    , S3Bucket (..)

    -- * FaceAttributes
    , FaceAttributes (..)

    -- * ProtectiveEquipmentPerson
    , ProtectiveEquipmentPerson (..)
    , mkProtectiveEquipmentPerson
    , pepBodyParts
    , pepBoundingBox
    , pepConfidence
    , pepId

    -- * DetectTextFilters
    , DetectTextFilters (..)
    , mkDetectTextFilters
    , dtfRegionsOfInterest
    , dtfWordFilter

    -- * ProjectVersionDescription
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

    -- * Instance
    , Instance (..)
    , mkInstance
    , iBoundingBox
    , iConfidence

    -- * TrainingDataResult
    , TrainingDataResult (..)
    , mkTrainingDataResult
    , tInput
    , tOutput
    , tValidation

    -- * Face
    , Face (..)
    , mkFace
    , fBoundingBox
    , fConfidence
    , fExternalImageId
    , fFaceId
    , fImageId

    -- * StartTextDetectionFilters
    , StartTextDetectionFilters (..)
    , mkStartTextDetectionFilters
    , stdfRegionsOfInterest
    , stdfWordFilter

    -- * RoleArn
    , RoleArn (..)

    -- * NextToken
    , NextToken (..)

    -- * DurationSMPTE
    , DurationSMPTE (..)

    -- * EndTimecodeSMPTE
    , EndTimecodeSMPTE (..)

    -- * StartTimecodeSMPTE
    , StartTimecodeSMPTE (..)

    -- * Bucket
    , Bucket (..)

    -- * Name
    , Name (..)

    -- * Id
    , Id (..)

    -- * Arn
    , Arn (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Rekognition.Types.ProtectiveEquipmentBodyPart
  
import Network.AWS.Rekognition.Types.Parent
  
import Network.AWS.Rekognition.Types.FaceId
  
import Network.AWS.Rekognition.Types.SegmentDetection
  
import Network.AWS.Rekognition.Types.StreamProcessorInput
  
import Network.AWS.Rekognition.Types.KinesisVideoArn
  
import Network.AWS.Rekognition.Types.FaceRecord
  
import Network.AWS.Rekognition.Types.AgeRange
  
import Network.AWS.Rekognition.Types.Attribute
  
import Network.AWS.Rekognition.Types.Summary
  
import Network.AWS.Rekognition.Types.Sunglasses
  
  
import Network.AWS.Rekognition.Types.CelebrityRecognition
  
import Network.AWS.Rekognition.Types.GenderType
  
import Network.AWS.Rekognition.Types.PaginationToken
  
import Network.AWS.Rekognition.Types.MouthOpen
  
import Network.AWS.Rekognition.Types.StartSegmentDetectionFilters
  
import Network.AWS.Rekognition.Types.S3ObjectVersion
  
import Network.AWS.Rekognition.Types.BoundingBox
  
  
  
import Network.AWS.Rekognition.Types.ExternalImageId
  
import Network.AWS.Rekognition.Types.EvaluationResult
  
import Network.AWS.Rekognition.Types.StartTechnicalCueDetectionFilter
  
import Network.AWS.Rekognition.Types.BodyPart
  
import Network.AWS.Rekognition.Types.Image
  
import Network.AWS.Rekognition.Types.S3KeyPrefix
  
import Network.AWS.Rekognition.Types.SNSTopicArn
  
import Network.AWS.Rekognition.Types.ModerationLabel
  
import Network.AWS.Rekognition.Types.ComparedFace
  
import Network.AWS.Rekognition.Types.Landmark
  
  
import Network.AWS.Rekognition.Types.ImageQuality
  
import Network.AWS.Rekognition.Types.JobId
  
import Network.AWS.Rekognition.Types.VideoJobStatus
  
import Network.AWS.Rekognition.Types.QualityFilter
  
import Network.AWS.Rekognition.Types.VersionName
  
import Network.AWS.Rekognition.Types.S3Object
  
import Network.AWS.Rekognition.Types.SegmentTypeInfo
  
import Network.AWS.Rekognition.Types.JobTag
  
import Network.AWS.Rekognition.Types.StreamProcessorName
  
import Network.AWS.Rekognition.Types.GroundTruthManifest
  
import Network.AWS.Rekognition.Types.StreamProcessorOutput
  
import Network.AWS.Rekognition.Types.UnindexedFace
  
import Network.AWS.Rekognition.Types.Asset
  
import Network.AWS.Rekognition.Types.FaceDetail
  
import Network.AWS.Rekognition.Types.HumanLoopActivationOutput
  
import Network.AWS.Rekognition.Types.TechnicalCueType
  
import Network.AWS.Rekognition.Types.CollectionId
  
import Network.AWS.Rekognition.Types.PersonTrackingSortBy
  
import Network.AWS.Rekognition.Types.TechnicalCueSegment
  
import Network.AWS.Rekognition.Types.ProjectVersionStatus
  
import Network.AWS.Rekognition.Types.SegmentType
  
  
import Network.AWS.Rekognition.Types.HumanLoopActivationReason
  
  
import Network.AWS.Rekognition.Types.Url
  
import Network.AWS.Rekognition.Types.Emotion
  
import Network.AWS.Rekognition.Types.Celebrity
  
import Network.AWS.Rekognition.Types.NotificationChannel
  
import Network.AWS.Rekognition.Types.CompareFacesMatch
  
  
import Network.AWS.Rekognition.Types.StreamProcessorStatus
  
import Network.AWS.Rekognition.Types.HumanLoopArn
  
import Network.AWS.Rekognition.Types.TestingData
  
import Network.AWS.Rekognition.Types.ContentClassifier
  
import Network.AWS.Rekognition.Types.TextTypes
  
  
import Network.AWS.Rekognition.Types.HumanLoopConfig
  
import Network.AWS.Rekognition.Types.ProjectStatus
  
import Network.AWS.Rekognition.Types.CoversBodyPart
  
import Network.AWS.Rekognition.Types.LabelDetection
  
import Network.AWS.Rekognition.Types.Pose
  
import Network.AWS.Rekognition.Types.Video
  
import Network.AWS.Rekognition.Types.KinesisDataStream
  
import Network.AWS.Rekognition.Types.Reason
  
import Network.AWS.Rekognition.Types.VideoMetadata
  
import Network.AWS.Rekognition.Types.ContentModerationSortBy
  
import Network.AWS.Rekognition.Types.EmotionName
  
import Network.AWS.Rekognition.Types.Point
  
import Network.AWS.Rekognition.Types.HumanLoopName
  
import Network.AWS.Rekognition.Types.PersonDetail
  
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes
  
import Network.AWS.Rekognition.Types.StreamProcessorSettings
  
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummary
  
import Network.AWS.Rekognition.Types.StartShotDetectionFilter
  
import Network.AWS.Rekognition.Types.TestingDataResult
  
import Network.AWS.Rekognition.Types.ProtectiveEquipmentSummarizationAttributes
  
import Network.AWS.Rekognition.Types.StreamProcessorArn
  
import Network.AWS.Rekognition.Types.StatusMessage
  
import Network.AWS.Rekognition.Types.Gender
  
  
import Network.AWS.Rekognition.Types.RegionOfInterest
  
import Network.AWS.Rekognition.Types.DetectionFilter
  
import Network.AWS.Rekognition.Types.FaceSearchSettings
  
import Network.AWS.Rekognition.Types.CelebrityDetail
  
import Network.AWS.Rekognition.Types.LandmarkType
  
import Network.AWS.Rekognition.Types.FlowDefinitionArn
  
import Network.AWS.Rekognition.Types.ContentModerationDetection
  
import Network.AWS.Rekognition.Types.ImageId
  
import Network.AWS.Rekognition.Types.PersonDetection
  
  
import Network.AWS.Rekognition.Types.FaceMatch
  
import Network.AWS.Rekognition.Types.TextDetection
  
  
import Network.AWS.Rekognition.Types.ProjectName
  
import Network.AWS.Rekognition.Types.Geometry
  
import Network.AWS.Rekognition.Types.ProjectVersionArn
  
import Network.AWS.Rekognition.Types.StreamProcessor
  
import Network.AWS.Rekognition.Types.OutputConfig
  
import Network.AWS.Rekognition.Types.EyeOpen
  
import Network.AWS.Rekognition.Types.EquipmentDetection
  
import Network.AWS.Rekognition.Types.KinesisVideoStream
  
import Network.AWS.Rekognition.Types.CustomLabel
  
import Network.AWS.Rekognition.Types.Eyeglasses
  
import Network.AWS.Rekognition.Types.CelebrityRecognitionSortBy
  
import Network.AWS.Rekognition.Types.Beard
  
import Network.AWS.Rekognition.Types.Mustache
  
import Network.AWS.Rekognition.Types.ExtendedPaginationToken
  
import Network.AWS.Rekognition.Types.TrainingData
  
import Network.AWS.Rekognition.Types.ComparedSourceImageFace
  
import Network.AWS.Rekognition.Types.ProjectArn
  
import Network.AWS.Rekognition.Types.LabelDetectionSortBy
  
import Network.AWS.Rekognition.Types.OrientationCorrection
  
import Network.AWS.Rekognition.Types.HumanLoopActivationConditionsEvaluationResults
  
import Network.AWS.Rekognition.Types.ShotSegment
  
import Network.AWS.Rekognition.Types.AudioMetadata
  
import Network.AWS.Rekognition.Types.ValidationData
  
  
import Network.AWS.Rekognition.Types.TextDetectionResult
  
  
import Network.AWS.Rekognition.Types.ProjectDescription
  
import Network.AWS.Rekognition.Types.ClientRequestToken
  
import Network.AWS.Rekognition.Types.ProtectiveEquipmentType
  
import Network.AWS.Rekognition.Types.FaceSearchSortBy
  
import Network.AWS.Rekognition.Types.Smile
  
import Network.AWS.Rekognition.Types.PersonMatch
  
import Network.AWS.Rekognition.Types.Label
  
import Network.AWS.Rekognition.Types.FaceDetection
  
import Network.AWS.Rekognition.Types.S3Bucket
  
import Network.AWS.Rekognition.Types.FaceAttributes
  
import Network.AWS.Rekognition.Types.ProtectiveEquipmentPerson
  
  
import Network.AWS.Rekognition.Types.DetectTextFilters
  
  
import Network.AWS.Rekognition.Types.ProjectVersionDescription
  
import Network.AWS.Rekognition.Types.Instance
  
import Network.AWS.Rekognition.Types.TrainingDataResult
  
import Network.AWS.Rekognition.Types.Face
  
  
import Network.AWS.Rekognition.Types.StartTextDetectionFilters
  
  
  
import Network.AWS.Rekognition.Types.RoleArn
  
import Network.AWS.Rekognition.Types.NextToken
  
import Network.AWS.Rekognition.Types.DurationSMPTE
  
import Network.AWS.Rekognition.Types.EndTimecodeSMPTE
  
import Network.AWS.Rekognition.Types.StartTimecodeSMPTE
  
import Network.AWS.Rekognition.Types.Bucket
  
import Network.AWS.Rekognition.Types.Name
  
import Network.AWS.Rekognition.Types.Id
  
import Network.AWS.Rekognition.Types.Arn
  

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Rekognition",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "rekognition",
                 Core._svcVersion = "2016-06-27", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Rekognition",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The file size or duration of the supplied media is too large. The maximum file size is 10GB. The maximum duration is 6 hours. 
_VideoTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VideoTooLargeException
  = Core._MatchServiceError mkServiceConfig "VideoTooLargeException"
{-# INLINEABLE _VideoTooLargeException #-}
{-# DEPRECATED _VideoTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | Input parameter violated a constraint. Validate your parameter before calling the API operation again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The provided image format is not supported. 
_InvalidImageFormatException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidImageFormatException
  = Core._MatchServiceError mkServiceConfig
      "InvalidImageFormatException"
{-# INLINEABLE _InvalidImageFormatException #-}
{-# DEPRECATED _InvalidImageFormatException "Use generic-lens or generic-optics instead"  #-}

-- | A collection with the specified ID already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsException"
{-# INLINEABLE _ResourceAlreadyExistsException #-}
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Amazon Rekognition is unable to access the S3 object specified in the request.
_InvalidS3ObjectException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3ObjectException
  = Core._MatchServiceError mkServiceConfig
      "InvalidS3ObjectException"
{-# INLINEABLE _InvalidS3ObjectException #-}
{-# DEPRECATED _InvalidS3ObjectException "Use generic-lens or generic-optics instead"  #-}

-- | The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException
  = Core._MatchServiceError mkServiceConfig
      "ProvisionedThroughputExceededException"
{-# INLINEABLE _ProvisionedThroughputExceededException #-}
{-# DEPRECATED _ProvisionedThroughputExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The input image size exceeds the allowed limit. For more information, see Limits in Amazon Rekognition in the Amazon Rekognition Developer Guide. 
_ImageTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ImageTooLargeException
  = Core._MatchServiceError mkServiceConfig "ImageTooLargeException"
{-# INLINEABLE _ImageTooLargeException #-}
{-# DEPRECATED _ImageTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | 
--
-- The size of the collection exceeds the allowed limit. For more information, see Limits in Amazon Rekognition in the Amazon Rekognition Developer Guide. 
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException
  = Core._MatchServiceError mkServiceConfig
      "ServiceQuotaExceededException"
{-# INLINEABLE _ServiceQuotaExceededException #-}
{-# DEPRECATED _ServiceQuotaExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Amazon Rekognition is temporarily unable to process the request. Try your call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | Amazon Rekognition experienced a service issue. Try your call again.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError
  = Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# INLINEABLE _InternalServerError #-}
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead"  #-}

-- | A @ClientRequestToken@ input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException
  = Core._MatchServiceError mkServiceConfig
      "IdempotentParameterMismatchException"
{-# INLINEABLE _IdempotentParameterMismatchException #-}
{-# DEPRECATED _IdempotentParameterMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | The requested resource isn't ready. For example, this exception occurs when you call @DetectCustomLabels@ with a model version that isn't deployed. 
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotReadyException"
{-# INLINEABLE _ResourceNotReadyException #-}
{-# DEPRECATED _ResourceNotReadyException "Use generic-lens or generic-optics instead"  #-}

-- | The collection specified in the request cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The number of in-progress human reviews you have has exceeded the number allowed.
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HumanLoopQuotaExceededException
  = Core._MatchServiceError mkServiceConfig
      "HumanLoopQuotaExceededException"
{-# INLINEABLE _HumanLoopQuotaExceededException #-}
{-# DEPRECATED _HumanLoopQuotaExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Pagination token in the request is not valid.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPaginationTokenException"
{-# INLINEABLE _InvalidPaginationTokenException #-}
{-# DEPRECATED _InvalidPaginationTokenException "Use generic-lens or generic-optics instead"  #-}

-- | An Amazon Rekognition service limit was exceeded. For example, if you start too many Amazon Rekognition Video jobs concurrently, calls to start operations (@StartLabelDetection@ , for example) will raise a @LimitExceededException@ exception (HTTP status code: 400) until the number of concurrently running jobs is below the Amazon Rekognition service limit. 
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource is already being used.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}

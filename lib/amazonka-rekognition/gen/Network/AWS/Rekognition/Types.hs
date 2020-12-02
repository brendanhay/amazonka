{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types
    (
    -- * Service Configuration
      rekognition

    -- * Errors
    , _AccessDeniedException
    , _VideoTooLargeException
    , _InvalidParameterException
    , _InvalidImageFormatException
    , _ResourceAlreadyExistsException
    , _InvalidS3ObjectException
    , _ProvisionedThroughputExceededException
    , _ImageTooLargeException
    , _ThrottlingException
    , _InternalServerError
    , _IdempotentParameterMismatchException
    , _ResourceNotFoundException
    , _InvalidPaginationTokenException
    , _LimitExceededException
    , _ResourceInUseException

    -- * Attribute
    , Attribute (..)

    -- * CelebrityRecognitionSortBy
    , CelebrityRecognitionSortBy (..)

    -- * ContentModerationSortBy
    , ContentModerationSortBy (..)

    -- * EmotionName
    , EmotionName (..)

    -- * FaceAttributes
    , FaceAttributes (..)

    -- * FaceSearchSortBy
    , FaceSearchSortBy (..)

    -- * GenderType
    , GenderType (..)

    -- * LabelDetectionSortBy
    , LabelDetectionSortBy (..)

    -- * LandmarkType
    , LandmarkType (..)

    -- * OrientationCorrection
    , OrientationCorrection (..)

    -- * PersonTrackingSortBy
    , PersonTrackingSortBy (..)

    -- * StreamProcessorStatus
    , StreamProcessorStatus (..)

    -- * TextTypes
    , TextTypes (..)

    -- * VideoJobStatus
    , VideoJobStatus (..)

    -- * AgeRange
    , AgeRange
    , ageRange
    , arLow
    , arHigh

    -- * Beard
    , Beard
    , beard
    , bValue
    , bConfidence

    -- * BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- * Celebrity
    , Celebrity
    , celebrity
    , cMatchConfidence
    , cURLs
    , cName
    , cId
    , cFace

    -- * CelebrityDetail
    , CelebrityDetail
    , celebrityDetail
    , cdBoundingBox
    , cdURLs
    , cdConfidence
    , cdName
    , cdId
    , cdFace

    -- * CelebrityRecognition
    , CelebrityRecognition
    , celebrityRecognition
    , crCelebrity
    , crTimestamp

    -- * CompareFacesMatch
    , CompareFacesMatch
    , compareFacesMatch
    , cfmSimilarity
    , cfmFace

    -- * ComparedFace
    , ComparedFace
    , comparedFace
    , cfBoundingBox
    , cfPose
    , cfConfidence
    , cfQuality
    , cfLandmarks

    -- * ComparedSourceImageFace
    , ComparedSourceImageFace
    , comparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

    -- * ContentModerationDetection
    , ContentModerationDetection
    , contentModerationDetection
    , cmdModerationLabel
    , cmdTimestamp

    -- * Emotion
    , Emotion
    , emotion
    , eConfidence
    , eType

    -- * EyeOpen
    , EyeOpen
    , eyeOpen
    , eoValue
    , eoConfidence

    -- * Eyeglasses
    , Eyeglasses
    , eyeglasses
    , eyeValue
    , eyeConfidence

    -- * Face
    , Face
    , face
    , fFaceId
    , fBoundingBox
    , fExternalImageId
    , fConfidence
    , fImageId

    -- * FaceDetail
    , FaceDetail
    , faceDetail
    , fdAgeRange
    , fdSunglasses
    , fdMouthOpen
    , fdBoundingBox
    , fdEmotions
    , fdEyesOpen
    , fdPose
    , fdConfidence
    , fdGender
    , fdQuality
    , fdEyeglasses
    , fdBeard
    , fdMustache
    , fdSmile
    , fdLandmarks

    -- * FaceDetection
    , FaceDetection
    , faceDetection
    , fdTimestamp
    , fdFace

    -- * FaceMatch
    , FaceMatch
    , faceMatch
    , fmSimilarity
    , fmFace

    -- * FaceRecord
    , FaceRecord
    , faceRecord
    , frFaceDetail
    , frFace

    -- * FaceSearchSettings
    , FaceSearchSettings
    , faceSearchSettings
    , fssFaceMatchThreshold
    , fssCollectionId

    -- * Gender
    , Gender
    , gender
    , gValue
    , gConfidence

    -- * Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- * Image
    , Image
    , image
    , iS3Object
    , iBytes

    -- * ImageQuality
    , ImageQuality
    , imageQuality
    , iqSharpness
    , iqBrightness

    -- * KinesisDataStream
    , KinesisDataStream
    , kinesisDataStream
    , kdsARN

    -- * KinesisVideoStream
    , KinesisVideoStream
    , kinesisVideoStream
    , kvsARN

    -- * Label
    , Label
    , label
    , lConfidence
    , lName

    -- * LabelDetection
    , LabelDetection
    , labelDetection
    , ldLabel
    , ldTimestamp

    -- * Landmark
    , Landmark
    , landmark
    , lType
    , lX
    , lY

    -- * ModerationLabel
    , ModerationLabel
    , moderationLabel
    , mlConfidence
    , mlName
    , mlParentName

    -- * MouthOpen
    , MouthOpen
    , mouthOpen
    , moValue
    , moConfidence

    -- * Mustache
    , Mustache
    , mustache
    , mValue
    , mConfidence

    -- * NotificationChannel
    , NotificationChannel
    , notificationChannel
    , ncSNSTopicARN
    , ncRoleARN

    -- * PersonDetail
    , PersonDetail
    , personDetail
    , pdBoundingBox
    , pdIndex
    , pdFace

    -- * PersonDetection
    , PersonDetection
    , personDetection
    , pdPerson
    , pdTimestamp

    -- * PersonMatch
    , PersonMatch
    , personMatch
    , pmFaceMatches
    , pmPerson
    , pmTimestamp

    -- * Point
    , Point
    , point
    , pX
    , pY

    -- * Pose
    , Pose
    , pose
    , pYaw
    , pRoll
    , pPitch

    -- * S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion

    -- * Smile
    , Smile
    , smile
    , smiValue
    , smiConfidence

    -- * StreamProcessor
    , StreamProcessor
    , streamProcessor
    , spStatus
    , spName

    -- * StreamProcessorInput
    , StreamProcessorInput
    , streamProcessorInput
    , spiKinesisVideoStream

    -- * StreamProcessorOutput
    , StreamProcessorOutput
    , streamProcessorOutput
    , spoKinesisDataStream

    -- * StreamProcessorSettings
    , StreamProcessorSettings
    , streamProcessorSettings
    , spsFaceSearch

    -- * Sunglasses
    , Sunglasses
    , sunglasses
    , sValue
    , sConfidence

    -- * TextDetection
    , TextDetection
    , textDetection
    , tdDetectedText
    , tdConfidence
    , tdGeometry
    , tdId
    , tdType
    , tdParentId

    -- * Video
    , Video
    , video
    , vS3Object

    -- * VideoMetadata
    , VideoMetadata
    , videoMetadata
    , vmFrameRate
    , vmFormat
    , vmCodec
    , vmFrameHeight
    , vmDurationMillis
    , vmFrameWidth
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Rekognition.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2016-06-27@ of the Amazon Rekognition SDK configuration.
rekognition :: Service
rekognition =
  Service
    { _svcAbbrev = "Rekognition"
    , _svcSigner = v4
    , _svcPrefix = "rekognition"
    , _svcVersion = "2016-06-27"
    , _svcEndpoint = defaultEndpoint rekognition
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Rekognition"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You are not authorized to perform the action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError rekognition "AccessDeniedException"


-- | The file size or duration of the supplied media is too large. The maximum file size is 8GB. The maximum duration is 2 hours.
--
--
_VideoTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_VideoTooLargeException =
  _MatchServiceError rekognition "VideoTooLargeException"


-- | Input parameter violated a constraint. Validate your parameter before calling the API operation again.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError rekognition "InvalidParameterException"


-- | The provided image format is not supported.
--
--
_InvalidImageFormatException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidImageFormatException =
  _MatchServiceError rekognition "InvalidImageFormatException"


-- | A collection with the specified ID already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError rekognition "ResourceAlreadyExistsException"


-- | Amazon Rekognition is unable to access the S3 object specified in the request.
--
--
_InvalidS3ObjectException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ObjectException =
  _MatchServiceError rekognition "InvalidS3ObjectException"


-- | The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
  _MatchServiceError rekognition "ProvisionedThroughputExceededException"


-- | The input image size exceeds the allowed limit. For more information, see 'limits' .
--
--
_ImageTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_ImageTooLargeException =
  _MatchServiceError rekognition "ImageTooLargeException"


-- | Amazon Rekognition is temporarily unable to process the request. Try your call again.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException = _MatchServiceError rekognition "ThrottlingException"


-- | Amazon Rekognition experienced a service issue. Try your call again.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError rekognition "InternalServerError"


-- | A @ClientRequestToken@ input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException =
  _MatchServiceError rekognition "IdempotentParameterMismatchException"


-- | The collection specified in the request cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError rekognition "ResourceNotFoundException"


-- | Pagination token in the request is not valid.
--
--
_InvalidPaginationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPaginationTokenException =
  _MatchServiceError rekognition "InvalidPaginationTokenException"


-- | An Amazon Rekognition service limit was exceeded. For example, if you start too many Rekognition Video jobs concurrently, calls to start operations (@StartLabelDetection@ , for example) will raise a @LimitExceededException@ exception (HTTP status code: 400) until the number of concurrently running jobs is below the Amazon Rekognition service limit.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError rekognition "LimitExceededException"


-- |
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError rekognition "ResourceInUseException"


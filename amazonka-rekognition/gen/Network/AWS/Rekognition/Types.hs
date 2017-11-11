{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types
-- Copyright   : (c) 2013-2017 Brendan Hay
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
    , _InvalidParameterException
    , _InvalidImageFormatException
    , _ResourceAlreadyExistsException
    , _InvalidS3ObjectException
    , _ProvisionedThroughputExceededException
    , _ImageTooLargeException
    , _ThrottlingException
    , _InternalServerError
    , _ResourceNotFoundException
    , _InvalidPaginationTokenException

    -- * Attribute
    , Attribute (..)

    -- * EmotionName
    , EmotionName (..)

    -- * GenderType
    , GenderType (..)

    -- * LandmarkType
    , LandmarkType (..)

    -- * OrientationCorrection
    , OrientationCorrection (..)

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

    -- * Gender
    , Gender
    , gender
    , gValue
    , gConfidence

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

    -- * Label
    , Label
    , label
    , lConfidence
    , lName

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

    -- * Sunglasses
    , Sunglasses
    , sunglasses
    , sValue
    , sConfidence
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


-- | Collection specified in the request is not found.
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


{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the Amazon Rekognition API reference.
--
--
module Network.AWS.Rekognition
    (
    -- * Service Configuration
      rekognition

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

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServerError
    , _InternalServerError

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartCelebrityRecognition
    , module Network.AWS.Rekognition.StartCelebrityRecognition

    -- ** GetPersonTracking
    , module Network.AWS.Rekognition.GetPersonTracking

    -- ** ListCollections (Paginated)
    , module Network.AWS.Rekognition.ListCollections

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

    -- ** RecognizeCelebrities
    , module Network.AWS.Rekognition.RecognizeCelebrities

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

    -- ** DescribeStreamProcessor
    , module Network.AWS.Rekognition.DescribeStreamProcessor

    -- ** StartFaceSearch
    , module Network.AWS.Rekognition.StartFaceSearch

    -- ** StartPersonTracking
    , module Network.AWS.Rekognition.StartPersonTracking

    -- ** GetCelebrityRecognition
    , module Network.AWS.Rekognition.GetCelebrityRecognition

    -- ** StartStreamProcessor
    , module Network.AWS.Rekognition.StartStreamProcessor

    -- ** DetectText
    , module Network.AWS.Rekognition.DetectText

    -- ** CompareFaces
    , module Network.AWS.Rekognition.CompareFaces

    -- ** DetectFaces
    , module Network.AWS.Rekognition.DetectFaces

    -- ** GetFaceDetection
    , module Network.AWS.Rekognition.GetFaceDetection

    -- ** ListFaces (Paginated)
    , module Network.AWS.Rekognition.ListFaces

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

    -- * Types

    -- ** Attribute
    , Attribute (..)

    -- ** CelebrityRecognitionSortBy
    , CelebrityRecognitionSortBy (..)

    -- ** ContentModerationSortBy
    , ContentModerationSortBy (..)

    -- ** EmotionName
    , EmotionName (..)

    -- ** FaceAttributes
    , FaceAttributes (..)

    -- ** FaceSearchSortBy
    , FaceSearchSortBy (..)

    -- ** GenderType
    , GenderType (..)

    -- ** LabelDetectionSortBy
    , LabelDetectionSortBy (..)

    -- ** LandmarkType
    , LandmarkType (..)

    -- ** OrientationCorrection
    , OrientationCorrection (..)

    -- ** PersonTrackingSortBy
    , PersonTrackingSortBy (..)

    -- ** StreamProcessorStatus
    , StreamProcessorStatus (..)

    -- ** TextTypes
    , TextTypes (..)

    -- ** VideoJobStatus
    , VideoJobStatus (..)

    -- ** AgeRange
    , AgeRange
    , ageRange
    , arLow
    , arHigh

    -- ** Beard
    , Beard
    , beard
    , bValue
    , bConfidence

    -- ** BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- ** Celebrity
    , Celebrity
    , celebrity
    , cMatchConfidence
    , cURLs
    , cName
    , cId
    , cFace

    -- ** CelebrityDetail
    , CelebrityDetail
    , celebrityDetail
    , cdBoundingBox
    , cdURLs
    , cdConfidence
    , cdName
    , cdId
    , cdFace

    -- ** CelebrityRecognition
    , CelebrityRecognition
    , celebrityRecognition
    , crCelebrity
    , crTimestamp

    -- ** CompareFacesMatch
    , CompareFacesMatch
    , compareFacesMatch
    , cfmSimilarity
    , cfmFace

    -- ** ComparedFace
    , ComparedFace
    , comparedFace
    , cfBoundingBox
    , cfPose
    , cfConfidence
    , cfQuality
    , cfLandmarks

    -- ** ComparedSourceImageFace
    , ComparedSourceImageFace
    , comparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

    -- ** ContentModerationDetection
    , ContentModerationDetection
    , contentModerationDetection
    , cmdModerationLabel
    , cmdTimestamp

    -- ** Emotion
    , Emotion
    , emotion
    , eConfidence
    , eType

    -- ** EyeOpen
    , EyeOpen
    , eyeOpen
    , eoValue
    , eoConfidence

    -- ** Eyeglasses
    , Eyeglasses
    , eyeglasses
    , eyeValue
    , eyeConfidence

    -- ** Face
    , Face
    , face
    , fFaceId
    , fBoundingBox
    , fExternalImageId
    , fConfidence
    , fImageId

    -- ** FaceDetail
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

    -- ** FaceDetection
    , FaceDetection
    , faceDetection
    , fdTimestamp
    , fdFace

    -- ** FaceMatch
    , FaceMatch
    , faceMatch
    , fmSimilarity
    , fmFace

    -- ** FaceRecord
    , FaceRecord
    , faceRecord
    , frFaceDetail
    , frFace

    -- ** FaceSearchSettings
    , FaceSearchSettings
    , faceSearchSettings
    , fssFaceMatchThreshold
    , fssCollectionId

    -- ** Gender
    , Gender
    , gender
    , gValue
    , gConfidence

    -- ** Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- ** Image
    , Image
    , image
    , iS3Object
    , iBytes

    -- ** ImageQuality
    , ImageQuality
    , imageQuality
    , iqSharpness
    , iqBrightness

    -- ** KinesisDataStream
    , KinesisDataStream
    , kinesisDataStream
    , kdsARN

    -- ** KinesisVideoStream
    , KinesisVideoStream
    , kinesisVideoStream
    , kvsARN

    -- ** Label
    , Label
    , label
    , lConfidence
    , lName

    -- ** LabelDetection
    , LabelDetection
    , labelDetection
    , ldLabel
    , ldTimestamp

    -- ** Landmark
    , Landmark
    , landmark
    , lType
    , lX
    , lY

    -- ** ModerationLabel
    , ModerationLabel
    , moderationLabel
    , mlConfidence
    , mlName
    , mlParentName

    -- ** MouthOpen
    , MouthOpen
    , mouthOpen
    , moValue
    , moConfidence

    -- ** Mustache
    , Mustache
    , mustache
    , mValue
    , mConfidence

    -- ** NotificationChannel
    , NotificationChannel
    , notificationChannel
    , ncSNSTopicARN
    , ncRoleARN

    -- ** PersonDetail
    , PersonDetail
    , personDetail
    , pdBoundingBox
    , pdIndex
    , pdFace

    -- ** PersonDetection
    , PersonDetection
    , personDetection
    , pdPerson
    , pdTimestamp

    -- ** PersonMatch
    , PersonMatch
    , personMatch
    , pmFaceMatches
    , pmPerson
    , pmTimestamp

    -- ** Point
    , Point
    , point
    , pX
    , pY

    -- ** Pose
    , Pose
    , pose
    , pYaw
    , pRoll
    , pPitch

    -- ** S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion

    -- ** Smile
    , Smile
    , smile
    , smiValue
    , smiConfidence

    -- ** StreamProcessor
    , StreamProcessor
    , streamProcessor
    , spStatus
    , spName

    -- ** StreamProcessorInput
    , StreamProcessorInput
    , streamProcessorInput
    , spiKinesisVideoStream

    -- ** StreamProcessorOutput
    , StreamProcessorOutput
    , streamProcessorOutput
    , spoKinesisDataStream

    -- ** StreamProcessorSettings
    , StreamProcessorSettings
    , streamProcessorSettings
    , spsFaceSearch

    -- ** Sunglasses
    , Sunglasses
    , sunglasses
    , sValue
    , sConfidence

    -- ** TextDetection
    , TextDetection
    , textDetection
    , tdDetectedText
    , tdConfidence
    , tdGeometry
    , tdId
    , tdType
    , tdParentId

    -- ** Video
    , Video
    , video
    , vS3Object

    -- ** VideoMetadata
    , VideoMetadata
    , videoMetadata
    , vmFrameRate
    , vmFormat
    , vmCodec
    , vmFrameHeight
    , vmDurationMillis
    , vmFrameWidth
    ) where

import Network.AWS.Rekognition.CompareFaces
import Network.AWS.Rekognition.CreateCollection
import Network.AWS.Rekognition.CreateStreamProcessor
import Network.AWS.Rekognition.DeleteCollection
import Network.AWS.Rekognition.DeleteFaces
import Network.AWS.Rekognition.DeleteStreamProcessor
import Network.AWS.Rekognition.DescribeStreamProcessor
import Network.AWS.Rekognition.DetectFaces
import Network.AWS.Rekognition.DetectLabels
import Network.AWS.Rekognition.DetectModerationLabels
import Network.AWS.Rekognition.DetectText
import Network.AWS.Rekognition.GetCelebrityInfo
import Network.AWS.Rekognition.GetCelebrityRecognition
import Network.AWS.Rekognition.GetContentModeration
import Network.AWS.Rekognition.GetFaceDetection
import Network.AWS.Rekognition.GetFaceSearch
import Network.AWS.Rekognition.GetLabelDetection
import Network.AWS.Rekognition.GetPersonTracking
import Network.AWS.Rekognition.IndexFaces
import Network.AWS.Rekognition.ListCollections
import Network.AWS.Rekognition.ListFaces
import Network.AWS.Rekognition.ListStreamProcessors
import Network.AWS.Rekognition.RecognizeCelebrities
import Network.AWS.Rekognition.SearchFaces
import Network.AWS.Rekognition.SearchFacesByImage
import Network.AWS.Rekognition.StartCelebrityRecognition
import Network.AWS.Rekognition.StartContentModeration
import Network.AWS.Rekognition.StartFaceDetection
import Network.AWS.Rekognition.StartFaceSearch
import Network.AWS.Rekognition.StartLabelDetection
import Network.AWS.Rekognition.StartPersonTracking
import Network.AWS.Rekognition.StartStreamProcessor
import Network.AWS.Rekognition.StopStreamProcessor
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Waiters

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

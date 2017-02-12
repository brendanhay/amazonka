{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidPaginationTokenException
    , _InvalidPaginationTokenException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListCollections (Paginated)
    , module Network.AWS.Rekognition.ListCollections

    -- ** DeleteCollection
    , module Network.AWS.Rekognition.DeleteCollection

    -- ** CreateCollection
    , module Network.AWS.Rekognition.CreateCollection

    -- ** DetectLabels
    , module Network.AWS.Rekognition.DetectLabels

    -- ** SearchFacesByImage
    , module Network.AWS.Rekognition.SearchFacesByImage

    -- ** SearchFaces
    , module Network.AWS.Rekognition.SearchFaces

    -- ** IndexFaces
    , module Network.AWS.Rekognition.IndexFaces

    -- ** CompareFaces
    , module Network.AWS.Rekognition.CompareFaces

    -- ** DetectFaces
    , module Network.AWS.Rekognition.DetectFaces

    -- ** ListFaces (Paginated)
    , module Network.AWS.Rekognition.ListFaces

    -- ** DeleteFaces
    , module Network.AWS.Rekognition.DeleteFaces

    -- * Types

    -- ** Attribute
    , Attribute (..)

    -- ** EmotionName
    , EmotionName (..)

    -- ** GenderType
    , GenderType (..)

    -- ** LandmarkType
    , LandmarkType (..)

    -- ** OrientationCorrection
    , OrientationCorrection (..)

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

    -- ** CompareFacesMatch
    , CompareFacesMatch
    , compareFacesMatch
    , cfmSimilarity
    , cfmFace

    -- ** ComparedFace
    , ComparedFace
    , comparedFace
    , cfBoundingBox
    , cfConfidence

    -- ** ComparedSourceImageFace
    , ComparedSourceImageFace
    , comparedSourceImageFace
    , csifBoundingBox
    , csifConfidence

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

    -- ** Gender
    , Gender
    , gender
    , gValue
    , gConfidence

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

    -- ** Label
    , Label
    , label
    , lConfidence
    , lName

    -- ** Landmark
    , Landmark
    , landmark
    , lType
    , lX
    , lY

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

    -- ** Sunglasses
    , Sunglasses
    , sunglasses
    , sValue
    , sConfidence
    ) where

import           Network.AWS.Rekognition.CompareFaces
import           Network.AWS.Rekognition.CreateCollection
import           Network.AWS.Rekognition.DeleteCollection
import           Network.AWS.Rekognition.DeleteFaces
import           Network.AWS.Rekognition.DetectFaces
import           Network.AWS.Rekognition.DetectLabels
import           Network.AWS.Rekognition.IndexFaces
import           Network.AWS.Rekognition.ListCollections
import           Network.AWS.Rekognition.ListFaces
import           Network.AWS.Rekognition.SearchFaces
import           Network.AWS.Rekognition.SearchFacesByImage
import           Network.AWS.Rekognition.Types
import           Network.AWS.Rekognition.Waiters

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

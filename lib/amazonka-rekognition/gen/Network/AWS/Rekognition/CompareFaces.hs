{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CompareFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Compares a face in the /source/ input image with each of the 100 largest faces detected in the /target/ input image.
--
--
-- You pass the input and target images either as base64-encoded image bytes or as a references to images in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- In response, the operation returns an array of face matches ordered by similarity score in descending order. For each face match, the response provides a bounding box of the face, facial landmarks, pose details (pitch, role, and yaw), quality (brightness and sharpness), and confidence value (indicating the level of confidence that the bounding box contains a face). The response also provides a similarity score, which indicates how closely the faces match.
--
-- @CompareFaces@ also returns an array of faces that don't match the source image. For each face, it returns a bounding box, confidence value, landmarks, pose details, and quality. The response also returns information about the face in the source image, including the bounding box of the face and confidence value.
--
-- If the image doesn't contain Exif metadata, @CompareFaces@ returns orientation information for the source and target images. Use these values to display the images with the correct image orientation.
--
-- If no faces are detected in the source or target images, @CompareFaces@ returns an @InvalidParameterException@ error.
--
-- For an example, see 'faces-compare-images' .
--
-- This operation requires permissions to perform the @rekognition:CompareFaces@ action.
--
module Network.AWS.Rekognition.CompareFaces
    (
    -- * Creating a Request
      compareFaces
    , CompareFaces
    -- * Request Lenses
    , cfSimilarityThreshold
    , cfSourceImage
    , cfTargetImage

    -- * Destructuring the Response
    , compareFacesResponse
    , CompareFacesResponse
    -- * Response Lenses
    , cfrsFaceMatches
    , cfrsUnmatchedFaces
    , cfrsTargetImageOrientationCorrection
    , cfrsSourceImageOrientationCorrection
    , cfrsSourceImageFace
    , cfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'compareFaces' smart constructor.
data CompareFaces = CompareFaces'
  { _cfSimilarityThreshold :: !(Maybe Double)
  , _cfSourceImage         :: !Image
  , _cfTargetImage         :: !Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompareFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfSimilarityThreshold' - The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
--
-- * 'cfSourceImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
--
-- * 'cfTargetImage' - The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
compareFaces
    :: Image -- ^ 'cfSourceImage'
    -> Image -- ^ 'cfTargetImage'
    -> CompareFaces
compareFaces pSourceImage_ pTargetImage_ =
  CompareFaces'
    { _cfSimilarityThreshold = Nothing
    , _cfSourceImage = pSourceImage_
    , _cfTargetImage = pTargetImage_
    }


-- | The minimum level of confidence in the face matches that a match must meet to be included in the @FaceMatches@ array.
cfSimilarityThreshold :: Lens' CompareFaces (Maybe Double)
cfSimilarityThreshold = lens _cfSimilarityThreshold (\ s a -> s{_cfSimilarityThreshold = a})

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
cfSourceImage :: Lens' CompareFaces Image
cfSourceImage = lens _cfSourceImage (\ s a -> s{_cfSourceImage = a})

-- | The target image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
cfTargetImage :: Lens' CompareFaces Image
cfTargetImage = lens _cfTargetImage (\ s a -> s{_cfTargetImage = a})

instance AWSRequest CompareFaces where
        type Rs CompareFaces = CompareFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 CompareFacesResponse' <$>
                   (x .?> "FaceMatches" .!@ mempty) <*>
                     (x .?> "UnmatchedFaces" .!@ mempty)
                     <*> (x .?> "TargetImageOrientationCorrection")
                     <*> (x .?> "SourceImageOrientationCorrection")
                     <*> (x .?> "SourceImageFace")
                     <*> (pure (fromEnum s)))

instance Hashable CompareFaces where

instance NFData CompareFaces where

instance ToHeaders CompareFaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.CompareFaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CompareFaces where
        toJSON CompareFaces'{..}
          = object
              (catMaybes
                 [("SimilarityThreshold" .=) <$>
                    _cfSimilarityThreshold,
                  Just ("SourceImage" .= _cfSourceImage),
                  Just ("TargetImage" .= _cfTargetImage)])

instance ToPath CompareFaces where
        toPath = const "/"

instance ToQuery CompareFaces where
        toQuery = const mempty

-- | /See:/ 'compareFacesResponse' smart constructor.
data CompareFacesResponse = CompareFacesResponse'
  { _cfrsFaceMatches                      :: !(Maybe [CompareFacesMatch])
  , _cfrsUnmatchedFaces                   :: !(Maybe [ComparedFace])
  , _cfrsTargetImageOrientationCorrection :: !(Maybe OrientationCorrection)
  , _cfrsSourceImageOrientationCorrection :: !(Maybe OrientationCorrection)
  , _cfrsSourceImageFace                  :: !(Maybe ComparedSourceImageFace)
  , _cfrsResponseStatus                   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompareFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFaceMatches' - An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
--
-- * 'cfrsUnmatchedFaces' - An array of faces in the target image that did not match the source image face.
--
-- * 'cfrsTargetImageOrientationCorrection' - The orientation of the target image (in counterclockwise direction). If your application displays the target image, you can use this value to correct the orientation of the image. The bounding box coordinates returned in @FaceMatches@ and @UnmatchedFaces@ represent face locations before the image orientation is corrected.
--
-- * 'cfrsSourceImageOrientationCorrection' - The orientation of the source image (counterclockwise direction). If your application displays the source image, you can use this value to correct image orientation. The bounding box coordinates returned in @SourceImageFace@ represent the location of the face before the image orientation is corrected.
--
-- * 'cfrsSourceImageFace' - The face in the source image that was used for comparison.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
compareFacesResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CompareFacesResponse
compareFacesResponse pResponseStatus_ =
  CompareFacesResponse'
    { _cfrsFaceMatches = Nothing
    , _cfrsUnmatchedFaces = Nothing
    , _cfrsTargetImageOrientationCorrection = Nothing
    , _cfrsSourceImageOrientationCorrection = Nothing
    , _cfrsSourceImageFace = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }


-- | An array of faces in the target image that match the source image face. Each @CompareFacesMatch@ object provides the bounding box, the confidence level that the bounding box contains a face, and the similarity score for the face in the bounding box and the face in the source image.
cfrsFaceMatches :: Lens' CompareFacesResponse [CompareFacesMatch]
cfrsFaceMatches = lens _cfrsFaceMatches (\ s a -> s{_cfrsFaceMatches = a}) . _Default . _Coerce

-- | An array of faces in the target image that did not match the source image face.
cfrsUnmatchedFaces :: Lens' CompareFacesResponse [ComparedFace]
cfrsUnmatchedFaces = lens _cfrsUnmatchedFaces (\ s a -> s{_cfrsUnmatchedFaces = a}) . _Default . _Coerce

-- | The orientation of the target image (in counterclockwise direction). If your application displays the target image, you can use this value to correct the orientation of the image. The bounding box coordinates returned in @FaceMatches@ and @UnmatchedFaces@ represent face locations before the image orientation is corrected.
cfrsTargetImageOrientationCorrection :: Lens' CompareFacesResponse (Maybe OrientationCorrection)
cfrsTargetImageOrientationCorrection = lens _cfrsTargetImageOrientationCorrection (\ s a -> s{_cfrsTargetImageOrientationCorrection = a})

-- | The orientation of the source image (counterclockwise direction). If your application displays the source image, you can use this value to correct image orientation. The bounding box coordinates returned in @SourceImageFace@ represent the location of the face before the image orientation is corrected.
cfrsSourceImageOrientationCorrection :: Lens' CompareFacesResponse (Maybe OrientationCorrection)
cfrsSourceImageOrientationCorrection = lens _cfrsSourceImageOrientationCorrection (\ s a -> s{_cfrsSourceImageOrientationCorrection = a})

-- | The face in the source image that was used for comparison.
cfrsSourceImageFace :: Lens' CompareFacesResponse (Maybe ComparedSourceImageFace)
cfrsSourceImageFace = lens _cfrsSourceImageFace (\ s a -> s{_cfrsSourceImageFace = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CompareFacesResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CompareFacesResponse where

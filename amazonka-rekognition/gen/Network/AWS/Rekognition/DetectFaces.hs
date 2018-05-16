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
-- Module      : Network.AWS.Rekognition.DetectFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces within an image that is provided as input.
--
--
-- @DetectFaces@ detects the 100 largest faces in the image. For each face detected, the operation returns face details including a bounding box of the face, a confidence value (that the bounding box contains a face), and a fixed set of attributes such as facial landmarks (for example, coordinates of eye and mouth), gender, presence of beard, sunglasses, etc.
--
-- The face-detection algorithm is most effective on frontal faces. For non-frontal or obscured faces, the algorithm may not detect the faces or might detect faces with lower confidence.
--
-- You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- For an example, see 'procedure-detecting-faces-in-images' .
--
-- This operation requires permissions to perform the @rekognition:DetectFaces@ action.
--
module Network.AWS.Rekognition.DetectFaces
    (
    -- * Creating a Request
      detectFaces
    , DetectFaces
    -- * Request Lenses
    , dfAttributes
    , dfImage

    -- * Destructuring the Response
    , detectFacesResponse
    , DetectFacesResponse
    -- * Response Lenses
    , dfrsOrientationCorrection
    , dfrsFaceDetails
    , dfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectFaces' smart constructor.
data DetectFaces = DetectFaces'
  { _dfAttributes :: !(Maybe [Attribute])
  , _dfImage      :: !Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfAttributes' - An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned but the operation will take longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
--
-- * 'dfImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
detectFaces
    :: Image -- ^ 'dfImage'
    -> DetectFaces
detectFaces pImage_ = DetectFaces' {_dfAttributes = Nothing, _dfImage = pImage_}


-- | An array of facial attributes you want to be returned. This can be the default list of attributes or all attributes. If you don't specify a value for @Attributes@ or if you specify @["DEFAULT"]@ , the API returns the following subset of facial attributes: @BoundingBox@ , @Confidence@ , @Pose@ , @Quality@ and @Landmarks@ . If you provide @["ALL"]@ , all facial attributes are returned but the operation will take longer to complete. If you provide both, @["ALL", "DEFAULT"]@ , the service uses a logical AND operator to determine which attributes to return (in this case, all attributes).
dfAttributes :: Lens' DetectFaces [Attribute]
dfAttributes = lens _dfAttributes (\ s a -> s{_dfAttributes = a}) . _Default . _Coerce

-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
dfImage :: Lens' DetectFaces Image
dfImage = lens _dfImage (\ s a -> s{_dfImage = a})

instance AWSRequest DetectFaces where
        type Rs DetectFaces = DetectFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DetectFacesResponse' <$>
                   (x .?> "OrientationCorrection") <*>
                     (x .?> "FaceDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DetectFaces where

instance NFData DetectFaces where

instance ToHeaders DetectFaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DetectFaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectFaces where
        toJSON DetectFaces'{..}
          = object
              (catMaybes
                 [("Attributes" .=) <$> _dfAttributes,
                  Just ("Image" .= _dfImage)])

instance ToPath DetectFaces where
        toPath = const "/"

instance ToQuery DetectFaces where
        toQuery = const mempty

-- | /See:/ 'detectFacesResponse' smart constructor.
data DetectFacesResponse = DetectFacesResponse'
  { _dfrsOrientationCorrection :: !(Maybe OrientationCorrection)
  , _dfrsFaceDetails           :: !(Maybe [FaceDetail])
  , _dfrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsOrientationCorrection' - The orientation of the input image (counter-clockwise direction). If your application displays the image, you can use this value to correct image orientation. The bounding box coordinates returned in @FaceDetails@ represent face locations before the image orientation is corrected.
--
-- * 'dfrsFaceDetails' - Details of each face found in the image.
--
-- * 'dfrsResponseStatus' - -- | The response status code.
detectFacesResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DetectFacesResponse
detectFacesResponse pResponseStatus_ =
  DetectFacesResponse'
    { _dfrsOrientationCorrection = Nothing
    , _dfrsFaceDetails = Nothing
    , _dfrsResponseStatus = pResponseStatus_
    }


-- | The orientation of the input image (counter-clockwise direction). If your application displays the image, you can use this value to correct image orientation. The bounding box coordinates returned in @FaceDetails@ represent face locations before the image orientation is corrected.
dfrsOrientationCorrection :: Lens' DetectFacesResponse (Maybe OrientationCorrection)
dfrsOrientationCorrection = lens _dfrsOrientationCorrection (\ s a -> s{_dfrsOrientationCorrection = a})

-- | Details of each face found in the image.
dfrsFaceDetails :: Lens' DetectFacesResponse [FaceDetail]
dfrsFaceDetails = lens _dfrsFaceDetails (\ s a -> s{_dfrsFaceDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
dfrsResponseStatus :: Lens' DetectFacesResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DetectFacesResponse where

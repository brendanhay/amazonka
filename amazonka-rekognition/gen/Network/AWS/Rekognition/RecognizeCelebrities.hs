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
-- Module      : Network.AWS.Rekognition.RecognizeCelebrities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of celebrities recognized in the input image. For more information, see 'celebrities' .
--
--
-- @RecognizeCelebrities@ returns the 100 largest faces in the image. It lists recognized celebrities in the @CelebrityFaces@ array and unrecognized faces in the @UnrecognizedFaces@ array. @RecognizeCelebrities@ doesn't return celebrities whose faces are not amongst the largest 100 faces in the image.
--
-- For each celebrity recognized, the @RecognizeCelebrities@ returns a @Celebrity@ object. The @Celebrity@ object contains the celebrity name, ID, URL links to additional information, match confidence, and a @ComparedFace@ object that you can use to locate the celebrity's face on the image.
--
-- Rekognition does not retain information about which images a celebrity has been recognized in. Your application must store this information and use the @Celebrity@ ID property as a unique identifier for the celebrity. If you don't store the celebrity name or additional information URLs returned by @RecognizeCelebrities@ , you will need the ID to identify the celebrity in a call to the operation.
--
-- You pass the imput image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file.
--
-- For an example, see 'celebrities-procedure-image' .
--
-- This operation requires permissions to perform the @rekognition:RecognizeCelebrities@ operation.
--
module Network.AWS.Rekognition.RecognizeCelebrities
    (
    -- * Creating a Request
      recognizeCelebrities
    , RecognizeCelebrities
    -- * Request Lenses
    , rcImage

    -- * Destructuring the Response
    , recognizeCelebritiesResponse
    , RecognizeCelebritiesResponse
    -- * Response Lenses
    , rcrsCelebrityFaces
    , rcrsOrientationCorrection
    , rcrsUnrecognizedFaces
    , rcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'recognizeCelebrities' smart constructor.
newtype RecognizeCelebrities = RecognizeCelebrities'
  { _rcImage :: Image
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecognizeCelebrities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcImage' - The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
recognizeCelebrities
    :: Image -- ^ 'rcImage'
    -> RecognizeCelebrities
recognizeCelebrities pImage_ = RecognizeCelebrities' {_rcImage = pImage_}


-- | The input image as base64-encoded bytes or an S3 object. If you use the AWS CLI to call Amazon Rekognition operations, passing base64-encoded image bytes is not supported.
rcImage :: Lens' RecognizeCelebrities Image
rcImage = lens _rcImage (\ s a -> s{_rcImage = a})

instance AWSRequest RecognizeCelebrities where
        type Rs RecognizeCelebrities =
             RecognizeCelebritiesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 RecognizeCelebritiesResponse' <$>
                   (x .?> "CelebrityFaces" .!@ mempty) <*>
                     (x .?> "OrientationCorrection")
                     <*> (x .?> "UnrecognizedFaces" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable RecognizeCelebrities where

instance NFData RecognizeCelebrities where

instance ToHeaders RecognizeCelebrities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.RecognizeCelebrities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RecognizeCelebrities where
        toJSON RecognizeCelebrities'{..}
          = object (catMaybes [Just ("Image" .= _rcImage)])

instance ToPath RecognizeCelebrities where
        toPath = const "/"

instance ToQuery RecognizeCelebrities where
        toQuery = const mempty

-- | /See:/ 'recognizeCelebritiesResponse' smart constructor.
data RecognizeCelebritiesResponse = RecognizeCelebritiesResponse'
  { _rcrsCelebrityFaces        :: !(Maybe [Celebrity])
  , _rcrsOrientationCorrection :: !(Maybe OrientationCorrection)
  , _rcrsUnrecognizedFaces     :: !(Maybe [ComparedFace])
  , _rcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecognizeCelebritiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCelebrityFaces' - Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 15 celebrities in an image.
--
-- * 'rcrsOrientationCorrection' - The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
--
-- * 'rcrsUnrecognizedFaces' - Details about each unrecognized face in the image.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
recognizeCelebritiesResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> RecognizeCelebritiesResponse
recognizeCelebritiesResponse pResponseStatus_ =
  RecognizeCelebritiesResponse'
    { _rcrsCelebrityFaces = Nothing
    , _rcrsOrientationCorrection = Nothing
    , _rcrsUnrecognizedFaces = Nothing
    , _rcrsResponseStatus = pResponseStatus_
    }


-- | Details about each celebrity found in the image. Amazon Rekognition can detect a maximum of 15 celebrities in an image.
rcrsCelebrityFaces :: Lens' RecognizeCelebritiesResponse [Celebrity]
rcrsCelebrityFaces = lens _rcrsCelebrityFaces (\ s a -> s{_rcrsCelebrityFaces = a}) . _Default . _Coerce

-- | The orientation of the input image (counterclockwise direction). If your application displays the image, you can use this value to correct the orientation. The bounding box coordinates returned in @CelebrityFaces@ and @UnrecognizedFaces@ represent face locations before the image orientation is corrected.
rcrsOrientationCorrection :: Lens' RecognizeCelebritiesResponse (Maybe OrientationCorrection)
rcrsOrientationCorrection = lens _rcrsOrientationCorrection (\ s a -> s{_rcrsOrientationCorrection = a})

-- | Details about each unrecognized face in the image.
rcrsUnrecognizedFaces :: Lens' RecognizeCelebritiesResponse [ComparedFace]
rcrsUnrecognizedFaces = lens _rcrsUnrecognizedFaces (\ s a -> s{_rcrsUnrecognizedFaces = a}) . _Default . _Coerce

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RecognizeCelebritiesResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData RecognizeCelebritiesResponse where

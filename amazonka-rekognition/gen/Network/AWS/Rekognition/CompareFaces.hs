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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Compares a face in the /source/ input image with each face detected in the /target/ input image.
--
--
-- In response, the operation returns an array of face matches ordered by similarity score with the highest similarity scores first. For each face match, the response provides a bounding box of the face and @confidence@ value (indicating the level of confidence that the bounding box contains a face). The response also provides a @similarity@ score, which indicates how closely the faces match.
--
-- In addition to the face matches, the response returns information about the face in the source image, including the bounding box of the face and confidence value.
--
-- For an example, see 'get-started-exercise-compare-faces'
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
    , cfrsSourceImageFace
    , cfrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Rekognition.Types
import           Network.AWS.Rekognition.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'compareFaces' smart constructor.
data CompareFaces = CompareFaces'
    { _cfSimilarityThreshold :: !(Maybe Double)
    , _cfSourceImage         :: !Image
    , _cfTargetImage         :: !Image
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompareFaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfSimilarityThreshold' - The minimum level of confidence in the match you want included in the result.
--
-- * 'cfSourceImage' - Source image either as bytes or an S3 object
--
-- * 'cfTargetImage' - Target image either as bytes or an S3 object
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

-- | The minimum level of confidence in the match you want included in the result.
cfSimilarityThreshold :: Lens' CompareFaces (Maybe Double)
cfSimilarityThreshold = lens _cfSimilarityThreshold (\ s a -> s{_cfSimilarityThreshold = a});

-- | Source image either as bytes or an S3 object
cfSourceImage :: Lens' CompareFaces Image
cfSourceImage = lens _cfSourceImage (\ s a -> s{_cfSourceImage = a});

-- | Target image either as bytes or an S3 object
cfTargetImage :: Lens' CompareFaces Image
cfTargetImage = lens _cfTargetImage (\ s a -> s{_cfTargetImage = a});

instance AWSRequest CompareFaces where
        type Rs CompareFaces = CompareFacesResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 CompareFacesResponse' <$>
                   (x .?> "FaceMatches" .!@ mempty) <*>
                     (x .?> "SourceImageFace")
                     <*> (pure (fromEnum s)))

instance Hashable CompareFaces

instance NFData CompareFaces

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
    { _cfrsFaceMatches     :: !(Maybe [CompareFacesMatch])
    , _cfrsSourceImageFace :: !(Maybe ComparedSourceImageFace)
    , _cfrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompareFacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFaceMatches' - Provides an array of @CompareFacesMatch@ objects. Each object provides the bounding box, confidence that the bounding box contains a face, and the similarity between the face in the bounding box and the face in the source image.
--
-- * 'cfrsSourceImageFace' - The face from the source image that was used for comparison.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
compareFacesResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CompareFacesResponse
compareFacesResponse pResponseStatus_ =
    CompareFacesResponse'
    { _cfrsFaceMatches = Nothing
    , _cfrsSourceImageFace = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }

-- | Provides an array of @CompareFacesMatch@ objects. Each object provides the bounding box, confidence that the bounding box contains a face, and the similarity between the face in the bounding box and the face in the source image.
cfrsFaceMatches :: Lens' CompareFacesResponse [CompareFacesMatch]
cfrsFaceMatches = lens _cfrsFaceMatches (\ s a -> s{_cfrsFaceMatches = a}) . _Default . _Coerce;

-- | The face from the source image that was used for comparison.
cfrsSourceImageFace :: Lens' CompareFacesResponse (Maybe ComparedSourceImageFace)
cfrsSourceImageFace = lens _cfrsSourceImageFace (\ s a -> s{_cfrsSourceImageFace = a});

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CompareFacesResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a});

instance NFData CompareFacesResponse

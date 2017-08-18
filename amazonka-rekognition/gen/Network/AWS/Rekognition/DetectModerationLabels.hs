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
-- Module      : Network.AWS.Rekognition.DetectModerationLabels
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects explicit or suggestive adult content in a specified JPEG or PNG format image. Use @DetectModerationLabels@ to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.
--
--
-- To filter images, use the labels returned by @DetectModerationLabels@ to determine which types of content are appropriate. For information about moderation labels, see 'image-moderation' .
--
module Network.AWS.Rekognition.DetectModerationLabels
    (
    -- * Creating a Request
      detectModerationLabels
    , DetectModerationLabels
    -- * Request Lenses
    , dmlMinConfidence
    , dmlImage

    -- * Destructuring the Response
    , detectModerationLabelsResponse
    , DetectModerationLabelsResponse
    -- * Response Lenses
    , dmlrsModerationLabels
    , dmlrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Rekognition.Types
import           Network.AWS.Rekognition.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detectModerationLabels' smart constructor.
data DetectModerationLabels = DetectModerationLabels'
    { _dmlMinConfidence :: !(Maybe Double)
    , _dmlImage         :: !Image
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetectModerationLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlMinConfidence' - Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
--
-- * 'dmlImage' - The input image as bytes or an S3 object.
detectModerationLabels
    :: Image -- ^ 'dmlImage'
    -> DetectModerationLabels
detectModerationLabels pImage_ =
    DetectModerationLabels'
    { _dmlMinConfidence = Nothing
    , _dmlImage = pImage_
    }

-- | Specifies the minimum confidence level for the labels to return. Amazon Rekognition doesn't return any labels with a confidence level lower than this specified value. If you don't specify @MinConfidence@ , the operation returns labels with confidence values greater than or equal to 50 percent.
dmlMinConfidence :: Lens' DetectModerationLabels (Maybe Double)
dmlMinConfidence = lens _dmlMinConfidence (\ s a -> s{_dmlMinConfidence = a});

-- | The input image as bytes or an S3 object.
dmlImage :: Lens' DetectModerationLabels Image
dmlImage = lens _dmlImage (\ s a -> s{_dmlImage = a});

instance AWSRequest DetectModerationLabels where
        type Rs DetectModerationLabels =
             DetectModerationLabelsResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 DetectModerationLabelsResponse' <$>
                   (x .?> "ModerationLabels" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetectModerationLabels

instance NFData DetectModerationLabels

instance ToHeaders DetectModerationLabels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DetectModerationLabels" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectModerationLabels where
        toJSON DetectModerationLabels'{..}
          = object
              (catMaybes
                 [("MinConfidence" .=) <$> _dmlMinConfidence,
                  Just ("Image" .= _dmlImage)])

instance ToPath DetectModerationLabels where
        toPath = const "/"

instance ToQuery DetectModerationLabels where
        toQuery = const mempty

-- | /See:/ 'detectModerationLabelsResponse' smart constructor.
data DetectModerationLabelsResponse = DetectModerationLabelsResponse'
    { _dmlrsModerationLabels :: !(Maybe [ModerationLabel])
    , _dmlrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetectModerationLabelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlrsModerationLabels' - An array of labels for explicit or suggestive adult content found in the image. The list includes the top-level label and each child label detected in the image. This is useful for filtering specific categories of content.
--
-- * 'dmlrsResponseStatus' - -- | The response status code.
detectModerationLabelsResponse
    :: Int -- ^ 'dmlrsResponseStatus'
    -> DetectModerationLabelsResponse
detectModerationLabelsResponse pResponseStatus_ =
    DetectModerationLabelsResponse'
    { _dmlrsModerationLabels = Nothing
    , _dmlrsResponseStatus = pResponseStatus_
    }

-- | An array of labels for explicit or suggestive adult content found in the image. The list includes the top-level label and each child label detected in the image. This is useful for filtering specific categories of content.
dmlrsModerationLabels :: Lens' DetectModerationLabelsResponse [ModerationLabel]
dmlrsModerationLabels = lens _dmlrsModerationLabels (\ s a -> s{_dmlrsModerationLabels = a}) . _Default . _Coerce;

-- | -- | The response status code.
dmlrsResponseStatus :: Lens' DetectModerationLabelsResponse Int
dmlrsResponseStatus = lens _dmlrsResponseStatus (\ s a -> s{_dmlrsResponseStatus = a});

instance NFData DetectModerationLabelsResponse

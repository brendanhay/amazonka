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
-- Module      : Network.AWS.Rekognition.StartFaceSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.
--
--
-- The video must be stored in an Amazon S3 bucket. Use 'Video' to specify the bucket name and the filename of the video. @StartFaceSearch@ returns a job identifier (@JobId@ ) which you use to get the search results once the search has completed. When searching is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in @NotificationChannel@ . To get the search results, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartFaceSearch@ . For more information, see 'collections-search-person' .
--
module Network.AWS.Rekognition.StartFaceSearch
    (
    -- * Creating a Request
      startFaceSearch
    , StartFaceSearch
    -- * Request Lenses
    , sfsFaceMatchThreshold
    , sfsJobTag
    , sfsNotificationChannel
    , sfsClientRequestToken
    , sfsVideo
    , sfsCollectionId

    -- * Destructuring the Response
    , startFaceSearchResponse
    , StartFaceSearchResponse
    -- * Response Lenses
    , sfsrsJobId
    , sfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startFaceSearch' smart constructor.
data StartFaceSearch = StartFaceSearch'
  { _sfsFaceMatchThreshold  :: !(Maybe Double)
  , _sfsJobTag              :: !(Maybe Text)
  , _sfsNotificationChannel :: !(Maybe NotificationChannel)
  , _sfsClientRequestToken  :: !(Maybe Text)
  , _sfsVideo               :: !Video
  , _sfsCollectionId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFaceSearch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfsFaceMatchThreshold' - The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%.
--
-- * 'sfsJobTag' - Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
--
-- * 'sfsNotificationChannel' - The ARN of the Amazon SNS topic to which you want Rekognition Video to publish the completion status of the search.
--
-- * 'sfsClientRequestToken' - Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
--
-- * 'sfsVideo' - The video you want to search. The video must be stored in an Amazon S3 bucket.
--
-- * 'sfsCollectionId' - ID of the collection that contains the faces you want to search for.
startFaceSearch
    :: Video -- ^ 'sfsVideo'
    -> Text -- ^ 'sfsCollectionId'
    -> StartFaceSearch
startFaceSearch pVideo_ pCollectionId_ =
  StartFaceSearch'
    { _sfsFaceMatchThreshold = Nothing
    , _sfsJobTag = Nothing
    , _sfsNotificationChannel = Nothing
    , _sfsClientRequestToken = Nothing
    , _sfsVideo = pVideo_
    , _sfsCollectionId = pCollectionId_
    }


-- | The minimum confidence in the person match to return. For example, don't return any matches where confidence in matches is less than 70%.
sfsFaceMatchThreshold :: Lens' StartFaceSearch (Maybe Double)
sfsFaceMatchThreshold = lens _sfsFaceMatchThreshold (\ s a -> s{_sfsFaceMatchThreshold = a})

-- | Unique identifier you specify to identify the job in the completion status published to the Amazon Simple Notification Service topic.
sfsJobTag :: Lens' StartFaceSearch (Maybe Text)
sfsJobTag = lens _sfsJobTag (\ s a -> s{_sfsJobTag = a})

-- | The ARN of the Amazon SNS topic to which you want Rekognition Video to publish the completion status of the search.
sfsNotificationChannel :: Lens' StartFaceSearch (Maybe NotificationChannel)
sfsNotificationChannel = lens _sfsNotificationChannel (\ s a -> s{_sfsNotificationChannel = a})

-- | Idempotent token used to identify the start request. If you use the same token with multiple @StartFaceSearch@ requests, the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same job from being accidently started more than once.
sfsClientRequestToken :: Lens' StartFaceSearch (Maybe Text)
sfsClientRequestToken = lens _sfsClientRequestToken (\ s a -> s{_sfsClientRequestToken = a})

-- | The video you want to search. The video must be stored in an Amazon S3 bucket.
sfsVideo :: Lens' StartFaceSearch Video
sfsVideo = lens _sfsVideo (\ s a -> s{_sfsVideo = a})

-- | ID of the collection that contains the faces you want to search for.
sfsCollectionId :: Lens' StartFaceSearch Text
sfsCollectionId = lens _sfsCollectionId (\ s a -> s{_sfsCollectionId = a})

instance AWSRequest StartFaceSearch where
        type Rs StartFaceSearch = StartFaceSearchResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 StartFaceSearchResponse' <$>
                   (x .?> "JobId") <*> (pure (fromEnum s)))

instance Hashable StartFaceSearch where

instance NFData StartFaceSearch where

instance ToHeaders StartFaceSearch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.StartFaceSearch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartFaceSearch where
        toJSON StartFaceSearch'{..}
          = object
              (catMaybes
                 [("FaceMatchThreshold" .=) <$>
                    _sfsFaceMatchThreshold,
                  ("JobTag" .=) <$> _sfsJobTag,
                  ("NotificationChannel" .=) <$>
                    _sfsNotificationChannel,
                  ("ClientRequestToken" .=) <$> _sfsClientRequestToken,
                  Just ("Video" .= _sfsVideo),
                  Just ("CollectionId" .= _sfsCollectionId)])

instance ToPath StartFaceSearch where
        toPath = const "/"

instance ToQuery StartFaceSearch where
        toQuery = const mempty

-- | /See:/ 'startFaceSearchResponse' smart constructor.
data StartFaceSearchResponse = StartFaceSearchResponse'
  { _sfsrsJobId          :: !(Maybe Text)
  , _sfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFaceSearchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfsrsJobId' - The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ .
--
-- * 'sfsrsResponseStatus' - -- | The response status code.
startFaceSearchResponse
    :: Int -- ^ 'sfsrsResponseStatus'
    -> StartFaceSearchResponse
startFaceSearchResponse pResponseStatus_ =
  StartFaceSearchResponse'
    {_sfsrsJobId = Nothing, _sfsrsResponseStatus = pResponseStatus_}


-- | The identifier for the search job. Use @JobId@ to identify the job in a subsequent call to @GetFaceSearch@ .
sfsrsJobId :: Lens' StartFaceSearchResponse (Maybe Text)
sfsrsJobId = lens _sfsrsJobId (\ s a -> s{_sfsrsJobId = a})

-- | -- | The response status code.
sfsrsResponseStatus :: Lens' StartFaceSearchResponse Int
sfsrsResponseStatus = lens _sfsrsResponseStatus (\ s a -> s{_sfsrsResponseStatus = a})

instance NFData StartFaceSearchResponse where

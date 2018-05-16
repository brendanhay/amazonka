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
-- Module      : Network.AWS.Rekognition.GetContentModeration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the content moderation analysis results for a Rekognition Video analysis started by .
--
--
-- Content moderation analysis of a video is an asynchronous operation. You start analysis by calling . which returns a job identifier (@JobId@ ). When analysis finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartContentModeration@ . To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetCelebrityDetection@ and pass the job identifier (@JobId@ ) from the initial call to @StartCelebrityDetection@ . For more information, see 'video' .
--
-- @GetContentModeration@ returns detected content moderation labels, and the time they are detected, in an array, @ModerationLabels@ , of objects.
--
-- By default, the moderated labels are returned sorted by time, in milliseconds from the start of the video. You can also sort them by moderated label by specifying @NAME@ for the @SortBy@ input parameter.
--
-- Since video analysis can return a large number of results, use the @MaxResults@ parameter to limit the number of labels returned in a single call to @GetContentModeration@ . If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetContentModeration@ and populate the @NextToken@ request parameter with the value of @NextToken@ returned from the previous call to @GetContentModeration@ .
--
-- For more information, see 'moderation' .
--
module Network.AWS.Rekognition.GetContentModeration
    (
    -- * Creating a Request
      getContentModeration
    , GetContentModeration
    -- * Request Lenses
    , gcmNextToken
    , gcmMaxResults
    , gcmSortBy
    , gcmJobId

    -- * Destructuring the Response
    , getContentModerationResponse
    , GetContentModerationResponse
    -- * Response Lenses
    , gcmrsNextToken
    , gcmrsVideoMetadata
    , gcmrsStatusMessage
    , gcmrsJobStatus
    , gcmrsModerationLabels
    , gcmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContentModeration' smart constructor.
data GetContentModeration = GetContentModeration'
  { _gcmNextToken  :: !(Maybe Text)
  , _gcmMaxResults :: !(Maybe Nat)
  , _gcmSortBy     :: !(Maybe ContentModerationSortBy)
  , _gcmJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContentModeration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmNextToken' - If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of content moderation labels.
--
-- * 'gcmMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- * 'gcmSortBy' - Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- * 'gcmJobId' - The identifier for the content moderation job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
getContentModeration
    :: Text -- ^ 'gcmJobId'
    -> GetContentModeration
getContentModeration pJobId_ =
  GetContentModeration'
    { _gcmNextToken = Nothing
    , _gcmMaxResults = Nothing
    , _gcmSortBy = Nothing
    , _gcmJobId = pJobId_
    }


-- | If the previous response was incomplete (because there is more data to retrieve), Amazon Rekognition returns a pagination token in the response. You can use this pagination token to retrieve the next set of content moderation labels.
gcmNextToken :: Lens' GetContentModeration (Maybe Text)
gcmNextToken = lens _gcmNextToken (\ s a -> s{_gcmNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
gcmMaxResults :: Lens' GetContentModeration (Maybe Natural)
gcmMaxResults = lens _gcmMaxResults (\ s a -> s{_gcmMaxResults = a}) . mapping _Nat

-- | Sort to use for elements in the @ModerationLabelDetections@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
gcmSortBy :: Lens' GetContentModeration (Maybe ContentModerationSortBy)
gcmSortBy = lens _gcmSortBy (\ s a -> s{_gcmSortBy = a})

-- | The identifier for the content moderation job. Use @JobId@ to identify the job in a subsequent call to @GetContentModeration@ .
gcmJobId :: Lens' GetContentModeration Text
gcmJobId = lens _gcmJobId (\ s a -> s{_gcmJobId = a})

instance AWSRequest GetContentModeration where
        type Rs GetContentModeration =
             GetContentModerationResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetContentModerationResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "VideoMetadata") <*>
                     (x .?> "StatusMessage")
                     <*> (x .?> "JobStatus")
                     <*> (x .?> "ModerationLabels" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetContentModeration where

instance NFData GetContentModeration where

instance ToHeaders GetContentModeration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetContentModeration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetContentModeration where
        toJSON GetContentModeration'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gcmNextToken,
                  ("MaxResults" .=) <$> _gcmMaxResults,
                  ("SortBy" .=) <$> _gcmSortBy,
                  Just ("JobId" .= _gcmJobId)])

instance ToPath GetContentModeration where
        toPath = const "/"

instance ToQuery GetContentModeration where
        toQuery = const mempty

-- | /See:/ 'getContentModerationResponse' smart constructor.
data GetContentModerationResponse = GetContentModerationResponse'
  { _gcmrsNextToken        :: !(Maybe Text)
  , _gcmrsVideoMetadata    :: !(Maybe VideoMetadata)
  , _gcmrsStatusMessage    :: !(Maybe Text)
  , _gcmrsJobStatus        :: !(Maybe VideoJobStatus)
  , _gcmrsModerationLabels :: !(Maybe [ContentModerationDetection])
  , _gcmrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContentModerationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmrsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of moderation labels.
--
-- * 'gcmrsVideoMetadata' - Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ .
--
-- * 'gcmrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gcmrsJobStatus' - The current status of the content moderation job.
--
-- * 'gcmrsModerationLabels' - The detected moderation labels and the time(s) they were detected.
--
-- * 'gcmrsResponseStatus' - -- | The response status code.
getContentModerationResponse
    :: Int -- ^ 'gcmrsResponseStatus'
    -> GetContentModerationResponse
getContentModerationResponse pResponseStatus_ =
  GetContentModerationResponse'
    { _gcmrsNextToken = Nothing
    , _gcmrsVideoMetadata = Nothing
    , _gcmrsStatusMessage = Nothing
    , _gcmrsJobStatus = Nothing
    , _gcmrsModerationLabels = Nothing
    , _gcmrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of moderation labels.
gcmrsNextToken :: Lens' GetContentModerationResponse (Maybe Text)
gcmrsNextToken = lens _gcmrsNextToken (\ s a -> s{_gcmrsNextToken = a})

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from @GetContentModeration@ .
gcmrsVideoMetadata :: Lens' GetContentModerationResponse (Maybe VideoMetadata)
gcmrsVideoMetadata = lens _gcmrsVideoMetadata (\ s a -> s{_gcmrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gcmrsStatusMessage :: Lens' GetContentModerationResponse (Maybe Text)
gcmrsStatusMessage = lens _gcmrsStatusMessage (\ s a -> s{_gcmrsStatusMessage = a})

-- | The current status of the content moderation job.
gcmrsJobStatus :: Lens' GetContentModerationResponse (Maybe VideoJobStatus)
gcmrsJobStatus = lens _gcmrsJobStatus (\ s a -> s{_gcmrsJobStatus = a})

-- | The detected moderation labels and the time(s) they were detected.
gcmrsModerationLabels :: Lens' GetContentModerationResponse [ContentModerationDetection]
gcmrsModerationLabels = lens _gcmrsModerationLabels (\ s a -> s{_gcmrsModerationLabels = a}) . _Default . _Coerce

-- | -- | The response status code.
gcmrsResponseStatus :: Lens' GetContentModerationResponse Int
gcmrsResponseStatus = lens _gcmrsResponseStatus (\ s a -> s{_gcmrsResponseStatus = a})

instance NFData GetContentModerationResponse where

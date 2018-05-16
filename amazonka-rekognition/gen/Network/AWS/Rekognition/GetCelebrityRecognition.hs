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
-- Module      : Network.AWS.Rekognition.GetCelebrityRecognition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the celebrity recognition results for a Rekognition Video analysis started by .
--
--
-- Celebrity recognition in a video is an asynchronous operation. Analysis is started by a call to which returns a job identifier (@JobId@ ). When the celebrity recognition operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartCelebrityRecognition@ . To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetCelebrityDetection@ and pass the job identifier (@JobId@ ) from the initial call to @StartCelebrityDetection@ . For more information, see 'video' .
--
-- @GetCelebrityRecognition@ returns detected celebrities and the time(s) they are detected in an array (@Celebrities@ ) of objects. Each @CelebrityRecognition@ contains information about the celebrity in a object and the time, @Timestamp@ , the celebrity was detected.
--
-- By default, the @Celebrities@ array is sorted by time (milliseconds from the start of the video). You can also sort the array by celebrity by specifying the value @ID@ in the @SortBy@ input parameter.
--
-- The @CelebrityDetail@ object includes the celebrity identifer and additional information urls. If you don't store the additional information urls, you can get them later by calling with the celebrity identifer.
--
-- No information is returned for faces not recognized as celebrities.
--
-- Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetCelebrityDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetCelebrityRecognition@ .
--
module Network.AWS.Rekognition.GetCelebrityRecognition
    (
    -- * Creating a Request
      getCelebrityRecognition
    , GetCelebrityRecognition
    -- * Request Lenses
    , gcrNextToken
    , gcrMaxResults
    , gcrSortBy
    , gcrJobId

    -- * Destructuring the Response
    , getCelebrityRecognitionResponse
    , GetCelebrityRecognitionResponse
    -- * Response Lenses
    , gcrrsNextToken
    , gcrrsVideoMetadata
    , gcrrsStatusMessage
    , gcrrsCelebrities
    , gcrrsJobStatus
    , gcrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCelebrityRecognition' smart constructor.
data GetCelebrityRecognition = GetCelebrityRecognition'
  { _gcrNextToken  :: !(Maybe Text)
  , _gcrMaxResults :: !(Maybe Nat)
  , _gcrSortBy     :: !(Maybe CelebrityRecognitionSortBy)
  , _gcrJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCelebrityRecognition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrNextToken' - If the previous response was incomplete (because there is more recognized celebrities to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities.
--
-- * 'gcrMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- * 'gcrSortBy' - Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
--
-- * 'gcrJobId' - Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
getCelebrityRecognition
    :: Text -- ^ 'gcrJobId'
    -> GetCelebrityRecognition
getCelebrityRecognition pJobId_ =
  GetCelebrityRecognition'
    { _gcrNextToken = Nothing
    , _gcrMaxResults = Nothing
    , _gcrSortBy = Nothing
    , _gcrJobId = pJobId_
    }


-- | If the previous response was incomplete (because there is more recognized celebrities to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of celebrities.
gcrNextToken :: Lens' GetCelebrityRecognition (Maybe Text)
gcrNextToken = lens _gcrNextToken (\ s a -> s{_gcrNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
gcrMaxResults :: Lens' GetCelebrityRecognition (Maybe Natural)
gcrMaxResults = lens _gcrMaxResults (\ s a -> s{_gcrMaxResults = a}) . mapping _Nat

-- | Sort to use for celebrities returned in @Celebrities@ field. Specify @ID@ to sort by the celebrity identifier, specify @TIMESTAMP@ to sort by the time the celebrity was recognized.
gcrSortBy :: Lens' GetCelebrityRecognition (Maybe CelebrityRecognitionSortBy)
gcrSortBy = lens _gcrSortBy (\ s a -> s{_gcrSortBy = a})

-- | Job identifier for the required celebrity recognition analysis. You can get the job identifer from a call to @StartCelebrityRecognition@ .
gcrJobId :: Lens' GetCelebrityRecognition Text
gcrJobId = lens _gcrJobId (\ s a -> s{_gcrJobId = a})

instance AWSRequest GetCelebrityRecognition where
        type Rs GetCelebrityRecognition =
             GetCelebrityRecognitionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetCelebrityRecognitionResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "VideoMetadata") <*>
                     (x .?> "StatusMessage")
                     <*> (x .?> "Celebrities" .!@ mempty)
                     <*> (x .?> "JobStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetCelebrityRecognition where

instance NFData GetCelebrityRecognition where

instance ToHeaders GetCelebrityRecognition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetCelebrityRecognition" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCelebrityRecognition where
        toJSON GetCelebrityRecognition'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gcrNextToken,
                  ("MaxResults" .=) <$> _gcrMaxResults,
                  ("SortBy" .=) <$> _gcrSortBy,
                  Just ("JobId" .= _gcrJobId)])

instance ToPath GetCelebrityRecognition where
        toPath = const "/"

instance ToQuery GetCelebrityRecognition where
        toQuery = const mempty

-- | /See:/ 'getCelebrityRecognitionResponse' smart constructor.
data GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse'
  { _gcrrsNextToken      :: !(Maybe Text)
  , _gcrrsVideoMetadata  :: !(Maybe VideoMetadata)
  , _gcrrsStatusMessage  :: !(Maybe Text)
  , _gcrrsCelebrities    :: !(Maybe [CelebrityRecognition])
  , _gcrrsJobStatus      :: !(Maybe VideoJobStatus)
  , _gcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCelebrityRecognitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrrsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
--
-- * 'gcrrsVideoMetadata' - Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
--
-- * 'gcrrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gcrrsCelebrities' - Array of celebrities recognized in the video.
--
-- * 'gcrrsJobStatus' - The current status of the celebrity recognition job.
--
-- * 'gcrrsResponseStatus' - -- | The response status code.
getCelebrityRecognitionResponse
    :: Int -- ^ 'gcrrsResponseStatus'
    -> GetCelebrityRecognitionResponse
getCelebrityRecognitionResponse pResponseStatus_ =
  GetCelebrityRecognitionResponse'
    { _gcrrsNextToken = Nothing
    , _gcrrsVideoMetadata = Nothing
    , _gcrrsStatusMessage = Nothing
    , _gcrrsCelebrities = Nothing
    , _gcrrsJobStatus = Nothing
    , _gcrrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of celebrities.
gcrrsNextToken :: Lens' GetCelebrityRecognitionResponse (Maybe Text)
gcrrsNextToken = lens _gcrrsNextToken (\ s a -> s{_gcrrsNextToken = a})

-- | Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
gcrrsVideoMetadata :: Lens' GetCelebrityRecognitionResponse (Maybe VideoMetadata)
gcrrsVideoMetadata = lens _gcrrsVideoMetadata (\ s a -> s{_gcrrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gcrrsStatusMessage :: Lens' GetCelebrityRecognitionResponse (Maybe Text)
gcrrsStatusMessage = lens _gcrrsStatusMessage (\ s a -> s{_gcrrsStatusMessage = a})

-- | Array of celebrities recognized in the video.
gcrrsCelebrities :: Lens' GetCelebrityRecognitionResponse [CelebrityRecognition]
gcrrsCelebrities = lens _gcrrsCelebrities (\ s a -> s{_gcrrsCelebrities = a}) . _Default . _Coerce

-- | The current status of the celebrity recognition job.
gcrrsJobStatus :: Lens' GetCelebrityRecognitionResponse (Maybe VideoJobStatus)
gcrrsJobStatus = lens _gcrrsJobStatus (\ s a -> s{_gcrrsJobStatus = a})

-- | -- | The response status code.
gcrrsResponseStatus :: Lens' GetCelebrityRecognitionResponse Int
gcrrsResponseStatus = lens _gcrrsResponseStatus (\ s a -> s{_gcrrsResponseStatus = a})

instance NFData GetCelebrityRecognitionResponse where

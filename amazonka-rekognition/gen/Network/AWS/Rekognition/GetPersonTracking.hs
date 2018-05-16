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
-- Module      : Network.AWS.Rekognition.GetPersonTracking
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the person tracking results of a Rekognition Video analysis started by .
--
--
-- The person detection operation is started by a call to @StartPersonTracking@ which returns a job identifier (@JobId@ ). When the person detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartPersonTracking@ .
--
-- To get the results of the person tracking operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartPersonTracking@ .
--
-- @GetPersonTracking@ returns an array, @Persons@ , of tracked persons and the time(s) they were tracked in the video.
--
-- By default, the array is sorted by the time(s) a person is tracked in the video. You can sort by tracked persons by specifying @INDEX@ for the @SortBy@ input parameter.
--
-- Use the @MaxResults@ parameter to limit the number of items returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetPersonTracking@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetPersonTracking@ .
--
module Network.AWS.Rekognition.GetPersonTracking
    (
    -- * Creating a Request
      getPersonTracking
    , GetPersonTracking
    -- * Request Lenses
    , gptNextToken
    , gptMaxResults
    , gptSortBy
    , gptJobId

    -- * Destructuring the Response
    , getPersonTrackingResponse
    , GetPersonTrackingResponse
    -- * Response Lenses
    , gptrsNextToken
    , gptrsVideoMetadata
    , gptrsStatusMessage
    , gptrsJobStatus
    , gptrsPersons
    , gptrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPersonTracking' smart constructor.
data GetPersonTracking = GetPersonTracking'
  { _gptNextToken  :: !(Maybe Text)
  , _gptMaxResults :: !(Maybe Nat)
  , _gptSortBy     :: !(Maybe PersonTrackingSortBy)
  , _gptJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPersonTracking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gptNextToken' - If the previous response was incomplete (because there are more persons to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons.
--
-- * 'gptMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- * 'gptSortBy' - Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- * 'gptJobId' - The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ .
getPersonTracking
    :: Text -- ^ 'gptJobId'
    -> GetPersonTracking
getPersonTracking pJobId_ =
  GetPersonTracking'
    { _gptNextToken = Nothing
    , _gptMaxResults = Nothing
    , _gptSortBy = Nothing
    , _gptJobId = pJobId_
    }


-- | If the previous response was incomplete (because there are more persons to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of persons.
gptNextToken :: Lens' GetPersonTracking (Maybe Text)
gptNextToken = lens _gptNextToken (\ s a -> s{_gptNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
gptMaxResults :: Lens' GetPersonTracking (Maybe Natural)
gptMaxResults = lens _gptMaxResults (\ s a -> s{_gptMaxResults = a}) . mapping _Nat

-- | Sort to use for elements in the @Persons@ array. Use @TIMESTAMP@ to sort array elements by the time persons are detected. Use @INDEX@ to sort by the tracked persons. If you sort by @INDEX@ , the array elements for each person are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
gptSortBy :: Lens' GetPersonTracking (Maybe PersonTrackingSortBy)
gptSortBy = lens _gptSortBy (\ s a -> s{_gptSortBy = a})

-- | The identifier for a job that tracks persons in a video. You get the @JobId@ from a call to @StartPersonTracking@ .
gptJobId :: Lens' GetPersonTracking Text
gptJobId = lens _gptJobId (\ s a -> s{_gptJobId = a})

instance AWSRequest GetPersonTracking where
        type Rs GetPersonTracking = GetPersonTrackingResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetPersonTrackingResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "VideoMetadata") <*>
                     (x .?> "StatusMessage")
                     <*> (x .?> "JobStatus")
                     <*> (x .?> "Persons" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetPersonTracking where

instance NFData GetPersonTracking where

instance ToHeaders GetPersonTracking where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetPersonTracking" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPersonTracking where
        toJSON GetPersonTracking'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gptNextToken,
                  ("MaxResults" .=) <$> _gptMaxResults,
                  ("SortBy" .=) <$> _gptSortBy,
                  Just ("JobId" .= _gptJobId)])

instance ToPath GetPersonTracking where
        toPath = const "/"

instance ToQuery GetPersonTracking where
        toQuery = const mempty

-- | /See:/ 'getPersonTrackingResponse' smart constructor.
data GetPersonTrackingResponse = GetPersonTrackingResponse'
  { _gptrsNextToken      :: !(Maybe Text)
  , _gptrsVideoMetadata  :: !(Maybe VideoMetadata)
  , _gptrsStatusMessage  :: !(Maybe Text)
  , _gptrsJobStatus      :: !(Maybe VideoJobStatus)
  , _gptrsPersons        :: !(Maybe [PersonDetection])
  , _gptrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPersonTrackingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gptrsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons.
--
-- * 'gptrsVideoMetadata' - Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
--
-- * 'gptrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gptrsJobStatus' - The current status of the person tracking job.
--
-- * 'gptrsPersons' - An array of the persons detected in the video and the times they are tracked throughout the video. An array element will exist for each time the person is tracked.
--
-- * 'gptrsResponseStatus' - -- | The response status code.
getPersonTrackingResponse
    :: Int -- ^ 'gptrsResponseStatus'
    -> GetPersonTrackingResponse
getPersonTrackingResponse pResponseStatus_ =
  GetPersonTrackingResponse'
    { _gptrsNextToken = Nothing
    , _gptrsVideoMetadata = Nothing
    , _gptrsStatusMessage = Nothing
    , _gptrsJobStatus = Nothing
    , _gptrsPersons = Nothing
    , _gptrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of persons.
gptrsNextToken :: Lens' GetPersonTrackingResponse (Maybe Text)
gptrsNextToken = lens _gptrsNextToken (\ s a -> s{_gptrsNextToken = a})

-- | Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
gptrsVideoMetadata :: Lens' GetPersonTrackingResponse (Maybe VideoMetadata)
gptrsVideoMetadata = lens _gptrsVideoMetadata (\ s a -> s{_gptrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gptrsStatusMessage :: Lens' GetPersonTrackingResponse (Maybe Text)
gptrsStatusMessage = lens _gptrsStatusMessage (\ s a -> s{_gptrsStatusMessage = a})

-- | The current status of the person tracking job.
gptrsJobStatus :: Lens' GetPersonTrackingResponse (Maybe VideoJobStatus)
gptrsJobStatus = lens _gptrsJobStatus (\ s a -> s{_gptrsJobStatus = a})

-- | An array of the persons detected in the video and the times they are tracked throughout the video. An array element will exist for each time the person is tracked.
gptrsPersons :: Lens' GetPersonTrackingResponse [PersonDetection]
gptrsPersons = lens _gptrsPersons (\ s a -> s{_gptrsPersons = a}) . _Default . _Coerce

-- | -- | The response status code.
gptrsResponseStatus :: Lens' GetPersonTrackingResponse Int
gptrsResponseStatus = lens _gptrsResponseStatus (\ s a -> s{_gptrsResponseStatus = a})

instance NFData GetPersonTrackingResponse where

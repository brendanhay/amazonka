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
-- Module      : Network.AWS.Rekognition.GetFaceSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the face search results for Rekognition Video face search started by . The search returns faces in a collection that match the faces of persons detected in a video. It also includes the time(s) that faces are matched in the video.
--
--
-- Face search in a video is an asynchronous operation. You start face search by calling to which returns a job identifier (@JobId@ ). When the search operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartFaceSearch@ . To get the search results, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call @GetFaceSearch@ and pass the job identifier (@JobId@ ) from the initial call to @StartFaceSearch@ . For more information, see 'collections' .
--
-- The search results are retured in an array, @Persons@ , of objects. Each@PersonMatch@ element contains details about the matching faces in the input collection, person information (facial attributes, bounding boxes, and person identifer) for the matched person, and the time the person was matched in the video.
--
-- By default, the @Persons@ array is sorted by the time, in milliseconds from the start of the video, persons are matched. You can also sort by persons by specifying @INDEX@ for the @SORTBY@ input parameter.
--
module Network.AWS.Rekognition.GetFaceSearch
    (
    -- * Creating a Request
      getFaceSearch
    , GetFaceSearch
    -- * Request Lenses
    , gfsNextToken
    , gfsMaxResults
    , gfsSortBy
    , gfsJobId

    -- * Destructuring the Response
    , getFaceSearchResponse
    , GetFaceSearchResponse
    -- * Response Lenses
    , gfsrsNextToken
    , gfsrsVideoMetadata
    , gfsrsStatusMessage
    , gfsrsJobStatus
    , gfsrsPersons
    , gfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFaceSearch' smart constructor.
data GetFaceSearch = GetFaceSearch'
  { _gfsNextToken  :: !(Maybe Text)
  , _gfsMaxResults :: !(Maybe Nat)
  , _gfsSortBy     :: !(Maybe FaceSearchSortBy)
  , _gfsJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFaceSearch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsNextToken' - If the previous response was incomplete (because there is more search results to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
--
-- * 'gfsMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- * 'gfsSortBy' - Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
--
-- * 'gfsJobId' - The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
getFaceSearch
    :: Text -- ^ 'gfsJobId'
    -> GetFaceSearch
getFaceSearch pJobId_ =
  GetFaceSearch'
    { _gfsNextToken = Nothing
    , _gfsMaxResults = Nothing
    , _gfsSortBy = Nothing
    , _gfsJobId = pJobId_
    }


-- | If the previous response was incomplete (because there is more search results to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of search results.
gfsNextToken :: Lens' GetFaceSearch (Maybe Text)
gfsNextToken = lens _gfsNextToken (\ s a -> s{_gfsNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
gfsMaxResults :: Lens' GetFaceSearch (Maybe Natural)
gfsMaxResults = lens _gfsMaxResults (\ s a -> s{_gfsMaxResults = a}) . mapping _Nat

-- | Sort to use for grouping faces in the response. Use @TIMESTAMP@ to group faces by the time that they are recognized. Use @INDEX@ to sort by recognized faces.
gfsSortBy :: Lens' GetFaceSearch (Maybe FaceSearchSortBy)
gfsSortBy = lens _gfsSortBy (\ s a -> s{_gfsSortBy = a})

-- | The job identifer for the search request. You get the job identifier from an initial call to @StartFaceSearch@ .
gfsJobId :: Lens' GetFaceSearch Text
gfsJobId = lens _gfsJobId (\ s a -> s{_gfsJobId = a})

instance AWSRequest GetFaceSearch where
        type Rs GetFaceSearch = GetFaceSearchResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetFaceSearchResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "VideoMetadata") <*>
                     (x .?> "StatusMessage")
                     <*> (x .?> "JobStatus")
                     <*> (x .?> "Persons" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetFaceSearch where

instance NFData GetFaceSearch where

instance ToHeaders GetFaceSearch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetFaceSearch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetFaceSearch where
        toJSON GetFaceSearch'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gfsNextToken,
                  ("MaxResults" .=) <$> _gfsMaxResults,
                  ("SortBy" .=) <$> _gfsSortBy,
                  Just ("JobId" .= _gfsJobId)])

instance ToPath GetFaceSearch where
        toPath = const "/"

instance ToQuery GetFaceSearch where
        toQuery = const mempty

-- | /See:/ 'getFaceSearchResponse' smart constructor.
data GetFaceSearchResponse = GetFaceSearchResponse'
  { _gfsrsNextToken      :: !(Maybe Text)
  , _gfsrsVideoMetadata  :: !(Maybe VideoMetadata)
  , _gfsrsStatusMessage  :: !(Maybe Text)
  , _gfsrsJobStatus      :: !(Maybe VideoJobStatus)
  , _gfsrsPersons        :: !(Maybe [PersonMatch])
  , _gfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFaceSearchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsrsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
--
-- * 'gfsrsVideoMetadata' - Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
--
-- * 'gfsrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gfsrsJobStatus' - The current status of the face search job.
--
-- * 'gfsrsPersons' - An array of persons, , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
--
-- * 'gfsrsResponseStatus' - -- | The response status code.
getFaceSearchResponse
    :: Int -- ^ 'gfsrsResponseStatus'
    -> GetFaceSearchResponse
getFaceSearchResponse pResponseStatus_ =
  GetFaceSearchResponse'
    { _gfsrsNextToken = Nothing
    , _gfsrsVideoMetadata = Nothing
    , _gfsrsStatusMessage = Nothing
    , _gfsrsJobStatus = Nothing
    , _gfsrsPersons = Nothing
    , _gfsrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of search results.
gfsrsNextToken :: Lens' GetFaceSearchResponse (Maybe Text)
gfsrsNextToken = lens _gfsrsNextToken (\ s a -> s{_gfsrsNextToken = a})

-- | Information about a video that Amazon Rekognition analyzed. @Videometadata@ is returned in every page of paginated responses from a Rekognition Video operation.
gfsrsVideoMetadata :: Lens' GetFaceSearchResponse (Maybe VideoMetadata)
gfsrsVideoMetadata = lens _gfsrsVideoMetadata (\ s a -> s{_gfsrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gfsrsStatusMessage :: Lens' GetFaceSearchResponse (Maybe Text)
gfsrsStatusMessage = lens _gfsrsStatusMessage (\ s a -> s{_gfsrsStatusMessage = a})

-- | The current status of the face search job.
gfsrsJobStatus :: Lens' GetFaceSearchResponse (Maybe VideoJobStatus)
gfsrsJobStatus = lens _gfsrsJobStatus (\ s a -> s{_gfsrsJobStatus = a})

-- | An array of persons, , in the video whose face(s) match the face(s) in an Amazon Rekognition collection. It also includes time information for when persons are matched in the video. You specify the input collection in an initial call to @StartFaceSearch@ . Each @Persons@ element includes a time the person was matched, face match details (@FaceMatches@ ) for matching faces in the collection, and person information (@Person@ ) for the matched person.
gfsrsPersons :: Lens' GetFaceSearchResponse [PersonMatch]
gfsrsPersons = lens _gfsrsPersons (\ s a -> s{_gfsrsPersons = a}) . _Default . _Coerce

-- | -- | The response status code.
gfsrsResponseStatus :: Lens' GetFaceSearchResponse Int
gfsrsResponseStatus = lens _gfsrsResponseStatus (\ s a -> s{_gfsrsResponseStatus = a})

instance NFData GetFaceSearchResponse where

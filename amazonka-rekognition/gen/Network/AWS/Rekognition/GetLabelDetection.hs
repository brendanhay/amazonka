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
-- Module      : Network.AWS.Rekognition.GetLabelDetection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the label detection results of a Rekognition Video analysis started by .
--
--
-- The label detection operation is started by a call to which returns a job identifier (@JobId@ ). When the label detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to @StartlabelDetection@ . To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is @SUCCEEDED@ . If so, call and pass the job identifier (@JobId@ ) from the initial call to @StartLabelDetection@ .
--
-- @GetLabelDetection@ returns an array of detected labels (@Labels@ ) sorted by the time the labels were detected. You can also sort by the label name by specifying @NAME@ for the @SortBy@ input parameter.
--
-- The labels returned include the label name, the percentage confidence in the accuracy of the detected label, and the time the label was detected in the video.
--
-- Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in @MaxResults@ , the value of @NextToken@ in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call @GetlabelDetection@ and populate the @NextToken@ request parameter with the token value returned from the previous call to @GetLabelDetection@ .
--
module Network.AWS.Rekognition.GetLabelDetection
    (
    -- * Creating a Request
      getLabelDetection
    , GetLabelDetection
    -- * Request Lenses
    , gldNextToken
    , gldMaxResults
    , gldSortBy
    , gldJobId

    -- * Destructuring the Response
    , getLabelDetectionResponse
    , GetLabelDetectionResponse
    -- * Response Lenses
    , gldrsNextToken
    , gldrsVideoMetadata
    , gldrsStatusMessage
    , gldrsLabels
    , gldrsJobStatus
    , gldrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLabelDetection' smart constructor.
data GetLabelDetection = GetLabelDetection'
  { _gldNextToken  :: !(Maybe Text)
  , _gldMaxResults :: !(Maybe Nat)
  , _gldSortBy     :: !(Maybe LabelDetectionSortBy)
  , _gldJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLabelDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldNextToken' - If the previous response was incomplete (because there are more labels to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
--
-- * 'gldMaxResults' - Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
--
-- * 'gldSortBy' - Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
--
-- * 'gldJobId' - Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
getLabelDetection
    :: Text -- ^ 'gldJobId'
    -> GetLabelDetection
getLabelDetection pJobId_ =
  GetLabelDetection'
    { _gldNextToken = Nothing
    , _gldMaxResults = Nothing
    , _gldSortBy = Nothing
    , _gldJobId = pJobId_
    }


-- | If the previous response was incomplete (because there are more labels to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of labels.
gldNextToken :: Lens' GetLabelDetection (Maybe Text)
gldNextToken = lens _gldNextToken (\ s a -> s{_gldNextToken = a})

-- | Maximum number of results to return per paginated call. The largest value you can specify is 1000. If you specify a value greater than 1000, a maximum of 1000 results is returned. The default value is 1000.
gldMaxResults :: Lens' GetLabelDetection (Maybe Natural)
gldMaxResults = lens _gldMaxResults (\ s a -> s{_gldMaxResults = a}) . mapping _Nat

-- | Sort to use for elements in the @Labels@ array. Use @TIMESTAMP@ to sort array elements by the time labels are detected. Use @NAME@ to alphabetically group elements for a label together. Within each label group, the array element are sorted by detection confidence. The default sort is by @TIMESTAMP@ .
gldSortBy :: Lens' GetLabelDetection (Maybe LabelDetectionSortBy)
gldSortBy = lens _gldSortBy (\ s a -> s{_gldSortBy = a})

-- | Job identifier for the label detection operation for which you want results returned. You get the job identifer from an initial call to @StartlabelDetection@ .
gldJobId :: Lens' GetLabelDetection Text
gldJobId = lens _gldJobId (\ s a -> s{_gldJobId = a})

instance AWSRequest GetLabelDetection where
        type Rs GetLabelDetection = GetLabelDetectionResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 GetLabelDetectionResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "VideoMetadata") <*>
                     (x .?> "StatusMessage")
                     <*> (x .?> "Labels" .!@ mempty)
                     <*> (x .?> "JobStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetLabelDetection where

instance NFData GetLabelDetection where

instance ToHeaders GetLabelDetection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.GetLabelDetection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetLabelDetection where
        toJSON GetLabelDetection'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gldNextToken,
                  ("MaxResults" .=) <$> _gldMaxResults,
                  ("SortBy" .=) <$> _gldSortBy,
                  Just ("JobId" .= _gldJobId)])

instance ToPath GetLabelDetection where
        toPath = const "/"

instance ToQuery GetLabelDetection where
        toQuery = const mempty

-- | /See:/ 'getLabelDetectionResponse' smart constructor.
data GetLabelDetectionResponse = GetLabelDetectionResponse'
  { _gldrsNextToken      :: !(Maybe Text)
  , _gldrsVideoMetadata  :: !(Maybe VideoMetadata)
  , _gldrsStatusMessage  :: !(Maybe Text)
  , _gldrsLabels         :: !(Maybe [LabelDetection])
  , _gldrsJobStatus      :: !(Maybe VideoJobStatus)
  , _gldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLabelDetectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldrsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
--
-- * 'gldrsVideoMetadata' - Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
--
-- * 'gldrsStatusMessage' - If the job fails, @StatusMessage@ provides a descriptive error message.
--
-- * 'gldrsLabels' - An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
--
-- * 'gldrsJobStatus' - The current status of the label detection job.
--
-- * 'gldrsResponseStatus' - -- | The response status code.
getLabelDetectionResponse
    :: Int -- ^ 'gldrsResponseStatus'
    -> GetLabelDetectionResponse
getLabelDetectionResponse pResponseStatus_ =
  GetLabelDetectionResponse'
    { _gldrsNextToken = Nothing
    , _gldrsVideoMetadata = Nothing
    , _gldrsStatusMessage = Nothing
    , _gldrsLabels = Nothing
    , _gldrsJobStatus = Nothing
    , _gldrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of labels.
gldrsNextToken :: Lens' GetLabelDetectionResponse (Maybe Text)
gldrsNextToken = lens _gldrsNextToken (\ s a -> s{_gldrsNextToken = a})

-- | Information about a video that Rekognition Video analyzed. @Videometadata@ is returned in every page of paginated responses from a Amazon Rekognition video operation.
gldrsVideoMetadata :: Lens' GetLabelDetectionResponse (Maybe VideoMetadata)
gldrsVideoMetadata = lens _gldrsVideoMetadata (\ s a -> s{_gldrsVideoMetadata = a})

-- | If the job fails, @StatusMessage@ provides a descriptive error message.
gldrsStatusMessage :: Lens' GetLabelDetectionResponse (Maybe Text)
gldrsStatusMessage = lens _gldrsStatusMessage (\ s a -> s{_gldrsStatusMessage = a})

-- | An array of labels detected in the video. Each element contains the detected label and the time, in milliseconds from the start of the video, that the label was detected.
gldrsLabels :: Lens' GetLabelDetectionResponse [LabelDetection]
gldrsLabels = lens _gldrsLabels (\ s a -> s{_gldrsLabels = a}) . _Default . _Coerce

-- | The current status of the label detection job.
gldrsJobStatus :: Lens' GetLabelDetectionResponse (Maybe VideoJobStatus)
gldrsJobStatus = lens _gldrsJobStatus (\ s a -> s{_gldrsJobStatus = a})

-- | -- | The response status code.
gldrsResponseStatus :: Lens' GetLabelDetectionResponse Int
gldrsResponseStatus = lens _gldrsResponseStatus (\ s a -> s{_gldrsResponseStatus = a})

instance NFData GetLabelDetectionResponse where

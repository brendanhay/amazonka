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
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByPipeline operation gets a list of the jobs currently in a pipeline.
--
--
-- Elastic Transcoder returns all of the jobs currently in the specified pipeline. The response body contains one element for each job that satisfies the search criteria.
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListJobsByPipeline
    (
    -- * Creating a Request
      listJobsByPipeline
    , ListJobsByPipeline
    -- * Request Lenses
    , ljbpAscending
    , ljbpPageToken
    , ljbpPipelineId

    -- * Destructuring the Response
    , listJobsByPipelineResponse
    , ListJobsByPipelineResponse
    -- * Response Lenses
    , ljbprsNextPageToken
    , ljbprsJobs
    , ljbprsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @ListJobsByPipelineRequest@ structure.
--
--
--
-- /See:/ 'listJobsByPipeline' smart constructor.
data ListJobsByPipeline = ListJobsByPipeline'
  { _ljbpAscending  :: !(Maybe Text)
  , _ljbpPageToken  :: !(Maybe Text)
  , _ljbpPipelineId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsByPipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljbpAscending' - To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
--
-- * 'ljbpPageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- * 'ljbpPipelineId' - The ID of the pipeline for which you want to get job information.
listJobsByPipeline
    :: Text -- ^ 'ljbpPipelineId'
    -> ListJobsByPipeline
listJobsByPipeline pPipelineId_ =
  ListJobsByPipeline'
    { _ljbpAscending = Nothing
    , _ljbpPageToken = Nothing
    , _ljbpPipelineId = pPipelineId_
    }


-- | To list jobs in chronological order by the date and time that they were submitted, enter @true@ . To list jobs in reverse chronological order, enter @false@ .
ljbpAscending :: Lens' ListJobsByPipeline (Maybe Text)
ljbpAscending = lens _ljbpAscending (\ s a -> s{_ljbpAscending = a})

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
ljbpPageToken :: Lens' ListJobsByPipeline (Maybe Text)
ljbpPageToken = lens _ljbpPageToken (\ s a -> s{_ljbpPageToken = a})

-- | The ID of the pipeline for which you want to get job information.
ljbpPipelineId :: Lens' ListJobsByPipeline Text
ljbpPipelineId = lens _ljbpPipelineId (\ s a -> s{_ljbpPipelineId = a})

instance AWSPager ListJobsByPipeline where
        page rq rs
          | stop (rs ^. ljbprsNextPageToken) = Nothing
          | stop (rs ^. ljbprsJobs) = Nothing
          | otherwise =
            Just $ rq &
              ljbpPageToken .~ rs ^. ljbprsNextPageToken

instance AWSRequest ListJobsByPipeline where
        type Rs ListJobsByPipeline =
             ListJobsByPipelineResponse
        request = get elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsByPipelineResponse' <$>
                   (x .?> "NextPageToken") <*> (x .?> "Jobs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListJobsByPipeline where

instance NFData ListJobsByPipeline where

instance ToHeaders ListJobsByPipeline where
        toHeaders = const mempty

instance ToPath ListJobsByPipeline where
        toPath ListJobsByPipeline'{..}
          = mconcat
              ["/2012-09-25/jobsByPipeline/", toBS _ljbpPipelineId]

instance ToQuery ListJobsByPipeline where
        toQuery ListJobsByPipeline'{..}
          = mconcat
              ["Ascending" =: _ljbpAscending,
               "PageToken" =: _ljbpPageToken]

-- | The @ListJobsByPipelineResponse@ structure.
--
--
--
-- /See:/ 'listJobsByPipelineResponse' smart constructor.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
  { _ljbprsNextPageToken  :: !(Maybe Text)
  , _ljbprsJobs           :: !(Maybe [Job'])
  , _ljbprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsByPipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljbprsNextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- * 'ljbprsJobs' - An array of @Job@ objects that are in the specified pipeline.
--
-- * 'ljbprsResponseStatus' - -- | The response status code.
listJobsByPipelineResponse
    :: Int -- ^ 'ljbprsResponseStatus'
    -> ListJobsByPipelineResponse
listJobsByPipelineResponse pResponseStatus_ =
  ListJobsByPipelineResponse'
    { _ljbprsNextPageToken = Nothing
    , _ljbprsJobs = Nothing
    , _ljbprsResponseStatus = pResponseStatus_
    }


-- | A value that you use to access the second and subsequent pages of results, if any. When the jobs in the specified pipeline fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
ljbprsNextPageToken :: Lens' ListJobsByPipelineResponse (Maybe Text)
ljbprsNextPageToken = lens _ljbprsNextPageToken (\ s a -> s{_ljbprsNextPageToken = a})

-- | An array of @Job@ objects that are in the specified pipeline.
ljbprsJobs :: Lens' ListJobsByPipelineResponse [Job']
ljbprsJobs = lens _ljbprsJobs (\ s a -> s{_ljbprsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
ljbprsResponseStatus :: Lens' ListJobsByPipelineResponse Int
ljbprsResponseStatus = lens _ljbprsResponseStatus (\ s a -> s{_ljbprsResponseStatus = a})

instance NFData ListJobsByPipelineResponse where

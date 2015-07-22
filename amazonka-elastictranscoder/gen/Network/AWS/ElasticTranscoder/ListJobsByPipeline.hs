{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByPipeline operation gets a list of the jobs currently in a
-- pipeline.
--
-- Elastic Transcoder returns all of the jobs currently in the specified
-- pipeline. The response body contains one element for each job that
-- satisfies the search criteria.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListJobsByPipeline.html>
module Network.AWS.ElasticTranscoder.ListJobsByPipeline
    (
    -- * Request
      ListJobsByPipeline
    -- ** Request constructor
    , listJobsByPipeline
    -- ** Request lenses
    , ljbprqAscending
    , ljbprqPageToken
    , ljbprqPipelineId

    -- * Response
    , ListJobsByPipelineResponse
    -- ** Response constructor
    , listJobsByPipelineResponse
    -- ** Response lenses
    , ljbprsNextPageToken
    , ljbprsJobs
    , ljbprsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ListJobsByPipelineRequest@ structure.
--
-- /See:/ 'listJobsByPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbprqAscending'
--
-- * 'ljbprqPageToken'
--
-- * 'ljbprqPipelineId'
data ListJobsByPipeline = ListJobsByPipeline'
    { _ljbprqAscending  :: !(Maybe Text)
    , _ljbprqPageToken  :: !(Maybe Text)
    , _ljbprqPipelineId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsByPipeline' smart constructor.
listJobsByPipeline :: Text -> ListJobsByPipeline
listJobsByPipeline pPipelineId =
    ListJobsByPipeline'
    { _ljbprqAscending = Nothing
    , _ljbprqPageToken = Nothing
    , _ljbprqPipelineId = pPipelineId
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
ljbprqAscending :: Lens' ListJobsByPipeline (Maybe Text)
ljbprqAscending = lens _ljbprqAscending (\ s a -> s{_ljbprqAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
ljbprqPageToken :: Lens' ListJobsByPipeline (Maybe Text)
ljbprqPageToken = lens _ljbprqPageToken (\ s a -> s{_ljbprqPageToken = a});

-- | The ID of the pipeline for which you want to get job information.
ljbprqPipelineId :: Lens' ListJobsByPipeline Text
ljbprqPipelineId = lens _ljbprqPipelineId (\ s a -> s{_ljbprqPipelineId = a});

instance AWSPager ListJobsByPipeline where
        page rq rs
          | stop (rs ^. ljbprsNextPageToken) = Nothing
          | stop (rs ^. ljbprsJobs) = Nothing
          | otherwise =
            Just $ rq &
              ljbprqPageToken .~ rs ^. ljbprsNextPageToken

instance AWSRequest ListJobsByPipeline where
        type Sv ListJobsByPipeline = ElasticTranscoder
        type Rs ListJobsByPipeline =
             ListJobsByPipelineResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsByPipelineResponse' <$>
                   (x .?> "NextPageToken") <*> (x .?> "Jobs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListJobsByPipeline where
        toHeaders = const mempty

instance ToPath ListJobsByPipeline where
        toPath ListJobsByPipeline'{..}
          = mconcat
              ["/2012-09-25/jobsByPipeline/",
               toText _ljbprqPipelineId]

instance ToQuery ListJobsByPipeline where
        toQuery ListJobsByPipeline'{..}
          = mconcat
              ["Ascending" =: _ljbprqAscending,
               "PageToken" =: _ljbprqPageToken]

-- | The @ListJobsByPipelineResponse@ structure.
--
-- /See:/ 'listJobsByPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbprsNextPageToken'
--
-- * 'ljbprsJobs'
--
-- * 'ljbprsStatus'
data ListJobsByPipelineResponse = ListJobsByPipelineResponse'
    { _ljbprsNextPageToken :: !(Maybe Text)
    , _ljbprsJobs          :: !(Maybe [Job'])
    , _ljbprsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsByPipelineResponse' smart constructor.
listJobsByPipelineResponse :: Int -> ListJobsByPipelineResponse
listJobsByPipelineResponse pStatus =
    ListJobsByPipelineResponse'
    { _ljbprsNextPageToken = Nothing
    , _ljbprsJobs = Nothing
    , _ljbprsStatus = pStatus
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
ljbprsNextPageToken :: Lens' ListJobsByPipelineResponse (Maybe Text)
ljbprsNextPageToken = lens _ljbprsNextPageToken (\ s a -> s{_ljbprsNextPageToken = a});

-- | An array of @Job@ objects that are in the specified pipeline.
ljbprsJobs :: Lens' ListJobsByPipelineResponse [Job']
ljbprsJobs = lens _ljbprsJobs (\ s a -> s{_ljbprsJobs = a}) . _Default;

-- | FIXME: Undocumented member.
ljbprsStatus :: Lens' ListJobsByPipelineResponse Int
ljbprsStatus = lens _ljbprsStatus (\ s a -> s{_ljbprsStatus = a});

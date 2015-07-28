{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListJobsByStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The ListJobsByStatus operation gets a list of jobs that have a specified
-- status. The response body contains one element for each job that
-- satisfies the search criteria.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListJobsByStatus.html>
module Network.AWS.ElasticTranscoder.ListJobsByStatus
    (
    -- * Request
      ListJobsByStatus
    -- ** Request constructor
    , listJobsByStatus
    -- ** Request lenses
    , ljbsAscending
    , ljbsPageToken
    , ljbsStatus

    -- * Response
    , ListJobsByStatusResponse
    -- ** Response constructor
    , listJobsByStatusResponse
    -- ** Response lenses
    , ljbsrsNextPageToken
    , ljbsrsJobs
    , ljbsrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ListJobsByStatusRequest@ structure.
--
-- /See:/ 'listJobsByStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsAscending'
--
-- * 'ljbsPageToken'
--
-- * 'ljbsStatus'
data ListJobsByStatus = ListJobsByStatus'
    { _ljbsAscending :: !(Maybe Text)
    , _ljbsPageToken :: !(Maybe Text)
    , _ljbsStatus    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsByStatus' smart constructor.
listJobsByStatus :: Text -> ListJobsByStatus
listJobsByStatus pStatus_ =
    ListJobsByStatus'
    { _ljbsAscending = Nothing
    , _ljbsPageToken = Nothing
    , _ljbsStatus = pStatus_
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter @true@. To list jobs in reverse chronological order,
-- enter @false@.
ljbsAscending :: Lens' ListJobsByStatus (Maybe Text)
ljbsAscending = lens _ljbsAscending (\ s a -> s{_ljbsAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
ljbsPageToken :: Lens' ListJobsByStatus (Maybe Text)
ljbsPageToken = lens _ljbsPageToken (\ s a -> s{_ljbsPageToken = a});

-- | To get information about all of the jobs associated with the current AWS
-- account that have a given status, specify the following status:
-- @Submitted@, @Progressing@, @Complete@, @Canceled@, or @Error@.
ljbsStatus :: Lens' ListJobsByStatus Text
ljbsStatus = lens _ljbsStatus (\ s a -> s{_ljbsStatus = a});

instance AWSPager ListJobsByStatus where
        page rq rs
          | stop (rs ^. ljbsrsNextPageToken) = Nothing
          | stop (rs ^. ljbsrsJobs) = Nothing
          | otherwise =
            Just $ rq &
              ljbsPageToken .~ rs ^. ljbsrsNextPageToken

instance AWSRequest ListJobsByStatus where
        type Sv ListJobsByStatus = ElasticTranscoder
        type Rs ListJobsByStatus = ListJobsByStatusResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsByStatusResponse' <$>
                   (x .?> "NextPageToken") <*> (x .?> "Jobs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListJobsByStatus where
        toHeaders = const mempty

instance ToPath ListJobsByStatus where
        toPath ListJobsByStatus'{..}
          = mconcat
              ["/2012-09-25/jobsByStatus/", toPath _ljbsStatus]

instance ToQuery ListJobsByStatus where
        toQuery ListJobsByStatus'{..}
          = mconcat
              ["Ascending" =: _ljbsAscending,
               "PageToken" =: _ljbsPageToken]

-- | The @ListJobsByStatusResponse@ structure.
--
-- /See:/ 'listJobsByStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsrsNextPageToken'
--
-- * 'ljbsrsJobs'
--
-- * 'ljbsrsStatus'
data ListJobsByStatusResponse = ListJobsByStatusResponse'
    { _ljbsrsNextPageToken :: !(Maybe Text)
    , _ljbsrsJobs          :: !(Maybe [Job'])
    , _ljbsrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListJobsByStatusResponse' smart constructor.
listJobsByStatusResponse :: Int -> ListJobsByStatusResponse
listJobsByStatusResponse pStatus_ =
    ListJobsByStatusResponse'
    { _ljbsrsNextPageToken = Nothing
    , _ljbsrsJobs = Nothing
    , _ljbsrsStatus = pStatus_
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
ljbsrsNextPageToken :: Lens' ListJobsByStatusResponse (Maybe Text)
ljbsrsNextPageToken = lens _ljbsrsNextPageToken (\ s a -> s{_ljbsrsNextPageToken = a});

-- | An array of @Job@ objects that have the specified status.
ljbsrsJobs :: Lens' ListJobsByStatusResponse [Job']
ljbsrsJobs = lens _ljbsrsJobs (\ s a -> s{_ljbsrsJobs = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
ljbsrsStatus :: Lens' ListJobsByStatusResponse Int
ljbsrsStatus = lens _ljbsrsStatus (\ s a -> s{_ljbsrsStatus = a});

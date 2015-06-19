{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.ListJobsByStatus
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The ListJobsByStatus operation gets a list of jobs that have a specified
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
    , ljbsrNextPageToken
    , ljbsrJobs
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobsByStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsAscending'
--
-- * 'ljbsPageToken'
--
-- * 'ljbsStatus'
data ListJobsByStatus = ListJobsByStatus'{_ljbsAscending :: Maybe Text, _ljbsPageToken :: Maybe Text, _ljbsStatus :: Text} deriving (Eq, Read, Show)

-- | 'ListJobsByStatus' smart constructor.
listJobsByStatus :: Text -> ListJobsByStatus
listJobsByStatus pStatus = ListJobsByStatus'{_ljbsAscending = Nothing, _ljbsPageToken = Nothing, _ljbsStatus = pStatus};

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

instance AWSRequest ListJobsByStatus where
        type Sv ListJobsByStatus = ElasticTranscoder
        type Rs ListJobsByStatus = ListJobsByStatusResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsByStatusResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Jobs" .!@ mempty))

instance ToHeaders ListJobsByStatus where
        toHeaders = const mempty

instance ToPath ListJobsByStatus where
        toPath ListJobsByStatus'{..}
          = mconcat
              ["/2012-09-25/jobsByStatus/", toText _ljbsStatus]

instance ToQuery ListJobsByStatus where
        toQuery ListJobsByStatus'{..}
          = mconcat
              ["Ascending" =: _ljbsAscending,
               "PageToken" =: _ljbsPageToken]

-- | /See:/ 'listJobsByStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsrNextPageToken'
--
-- * 'ljbsrJobs'
data ListJobsByStatusResponse = ListJobsByStatusResponse'{_ljbsrNextPageToken :: Maybe Text, _ljbsrJobs :: Maybe [Job']} deriving (Eq, Read, Show)

-- | 'ListJobsByStatusResponse' smart constructor.
listJobsByStatusResponse :: ListJobsByStatusResponse
listJobsByStatusResponse = ListJobsByStatusResponse'{_ljbsrNextPageToken = Nothing, _ljbsrJobs = Nothing};

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you\'ve reached the last page of results, the value of
-- @NextPageToken@ is @null@.
ljbsrNextPageToken :: Lens' ListJobsByStatusResponse (Maybe Text)
ljbsrNextPageToken = lens _ljbsrNextPageToken (\ s a -> s{_ljbsrNextPageToken = a});

-- | An array of @Job@ objects that have the specified status.
ljbsrJobs :: Lens' ListJobsByStatusResponse [Job']
ljbsrJobs = lens _ljbsrJobs (\ s a -> s{_ljbsrJobs = a}) . _Default;

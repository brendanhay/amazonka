{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.ElasticTranscoder.ListJobsByStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListJobsByStatus operation gets a list of jobs that have a specified
-- status. The response body contains one element for each job that satisfies
-- the search criteria.
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
    , ljbsrJobs
    , ljbsrNextPageToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

data ListJobsByStatus = ListJobsByStatus
    { _ljbsAscending :: Maybe Text
    , _ljbsPageToken :: Maybe Text
    , _ljbsStatus    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListJobsByStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsAscending' @::@ 'Maybe' 'Text'
--
-- * 'ljbsPageToken' @::@ 'Maybe' 'Text'
--
-- * 'ljbsStatus' @::@ 'Text'
--
listJobsByStatus :: Text -- ^ 'ljbsStatus'
                 -> ListJobsByStatus
listJobsByStatus p1 = ListJobsByStatus
    { _ljbsStatus    = p1
    , _ljbsAscending = Nothing
    , _ljbsPageToken = Nothing
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter true. To list jobs in reverse chronological order, enter
-- false.
ljbsAscending :: Lens' ListJobsByStatus (Maybe Text)
ljbsAscending = lens _ljbsAscending (\s a -> s { _ljbsAscending = a })

-- | When Elastic Transcoder returns more than one page of results, use
-- pageToken in subsequent GET requests to get each successive page of
-- results.
ljbsPageToken :: Lens' ListJobsByStatus (Maybe Text)
ljbsPageToken = lens _ljbsPageToken (\s a -> s { _ljbsPageToken = a })

-- | To get information about all of the jobs associated with the current AWS
-- account that have a given status, specify the following status:
-- Submitted, Progressing, Complete, Canceled, or Error.
ljbsStatus :: Lens' ListJobsByStatus Text
ljbsStatus = lens _ljbsStatus (\s a -> s { _ljbsStatus = a })

instance ToPath ListJobsByStatus where
    toPath ListJobsByStatus{..} = mconcat
        [ "/2012-09-25/jobsByStatus/"
        , toText _ljbsStatus
        ]

instance ToQuery ListJobsByStatus where
    toQuery ListJobsByStatus{..} = mconcat
        [ "Ascending" =? _ljbsAscending
        , "PageToken" =? _ljbsPageToken
        ]

instance ToHeaders ListJobsByStatus

data ListJobsByStatusResponse = ListJobsByStatusResponse
    { _ljbsrJobs          :: [Job']
    , _ljbsrNextPageToken :: Maybe Job'
    } deriving (Eq, Show, Generic)

-- | 'ListJobsByStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbsrJobs' @::@ ['Job'']
--
-- * 'ljbsrNextPageToken' @::@ 'Maybe' 'Job''
--
listJobsByStatusResponse :: ListJobsByStatusResponse
listJobsByStatusResponse = ListJobsByStatusResponse
    { _ljbsrJobs          = mempty
    , _ljbsrNextPageToken = Nothing
    }

-- | An array of Job objects that have the specified status.
ljbsrJobs :: Lens' ListJobsByStatusResponse [Job']
ljbsrJobs = lens _ljbsrJobs (\s a -> s { _ljbsrJobs = a })

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the jobs in the specified pipeline fit on one page
-- or when you've reached the last page of results, the value of
-- NextPageToken is null.
ljbsrNextPageToken :: Lens' ListJobsByStatusResponse (Maybe Job')
ljbsrNextPageToken =
    lens _ljbsrNextPageToken (\s a -> s { _ljbsrNextPageToken = a })

instance AWSRequest ListJobsByStatus where
    type Sv ListJobsByStatus = ElasticTranscoder
    type Rs ListJobsByStatus = ListJobsByStatusResponse

    request  = get
    response = jsonResponse $ \h o -> ListJobsByStatusResponse
        <$> o .: "Jobs"
        <*> o .: "NextPageToken"

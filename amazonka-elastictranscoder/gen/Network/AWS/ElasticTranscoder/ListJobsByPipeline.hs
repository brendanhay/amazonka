{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.ListJobsByPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The ListJobsByPipeline operation gets a list of the jobs currently in a
-- pipeline.
--
-- Elastic Transcoder returns all of the jobs currently in the specified
-- pipeline. The response body contains one element for each job that satisfies
-- the search criteria.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListJobsByPipeline.html>
module Network.AWS.ElasticTranscoder.ListJobsByPipeline
    (
    -- * Request
      ListJobsByPipeline
    -- ** Request constructor
    , listJobsByPipeline
    -- ** Request lenses
    , ljbpAscending
    , ljbpPageToken
    , ljbpPipelineId

    -- * Response
    , ListJobsByPipelineResponse
    -- ** Response constructor
    , listJobsByPipelineResponse
    -- ** Response lenses
    , ljbprJobs
    , ljbprNextPageToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

data ListJobsByPipeline = ListJobsByPipeline
    { _ljbpAscending  :: Maybe Text
    , _ljbpPageToken  :: Maybe Text
    , _ljbpPipelineId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListJobsByPipeline' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbpAscending' @::@ 'Maybe' 'Text'
--
-- * 'ljbpPageToken' @::@ 'Maybe' 'Text'
--
-- * 'ljbpPipelineId' @::@ 'Text'
--
listJobsByPipeline :: Text -- ^ 'ljbpPipelineId'
                   -> ListJobsByPipeline
listJobsByPipeline p1 = ListJobsByPipeline
    { _ljbpPipelineId = p1
    , _ljbpAscending  = Nothing
    , _ljbpPageToken  = Nothing
    }

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter 'true'. To list jobs in reverse chronological order, enter 'false'.
ljbpAscending :: Lens' ListJobsByPipeline (Maybe Text)
ljbpAscending = lens _ljbpAscending (\s a -> s { _ljbpAscending = a })

-- | When Elastic Transcoder returns more than one page of results, use 'pageToken'
-- in subsequent 'GET' requests to get each successive page of results.
ljbpPageToken :: Lens' ListJobsByPipeline (Maybe Text)
ljbpPageToken = lens _ljbpPageToken (\s a -> s { _ljbpPageToken = a })

-- | The ID of the pipeline for which you want to get job information.
ljbpPipelineId :: Lens' ListJobsByPipeline Text
ljbpPipelineId = lens _ljbpPipelineId (\s a -> s { _ljbpPipelineId = a })

data ListJobsByPipelineResponse = ListJobsByPipelineResponse
    { _ljbprJobs          :: List "Jobs" Job'
    , _ljbprNextPageToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListJobsByPipelineResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljbprJobs' @::@ ['Job'']
--
-- * 'ljbprNextPageToken' @::@ 'Maybe' 'Text'
--
listJobsByPipelineResponse :: ListJobsByPipelineResponse
listJobsByPipelineResponse = ListJobsByPipelineResponse
    { _ljbprJobs          = mempty
    , _ljbprNextPageToken = Nothing
    }

-- | An array of 'Job' objects that are in the specified pipeline.
ljbprJobs :: Lens' ListJobsByPipelineResponse [Job']
ljbprJobs = lens _ljbprJobs (\s a -> s { _ljbprJobs = a }) . _List

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the jobs in the specified pipeline fit on one page or when
-- you've reached the last page of results, the value of 'NextPageToken' is 'null'.
ljbprNextPageToken :: Lens' ListJobsByPipelineResponse (Maybe Text)
ljbprNextPageToken =
    lens _ljbprNextPageToken (\s a -> s { _ljbprNextPageToken = a })

instance ToPath ListJobsByPipeline where
    toPath ListJobsByPipeline{..} = mconcat
        [ "/2012-09-25/jobsByPipeline/"
        , toText _ljbpPipelineId
        ]

instance ToQuery ListJobsByPipeline where
    toQuery ListJobsByPipeline{..} = mconcat
        [ "Ascending" =? _ljbpAscending
        , "PageToken" =? _ljbpPageToken
        ]

instance ToHeaders ListJobsByPipeline

instance ToJSON ListJobsByPipeline where
    toJSON = const (toJSON Empty)

instance AWSRequest ListJobsByPipeline where
    type Sv ListJobsByPipeline = ElasticTranscoder
    type Rs ListJobsByPipeline = ListJobsByPipelineResponse

    request  = get
    response = jsonResponse

instance FromJSON ListJobsByPipelineResponse where
    parseJSON = withObject "ListJobsByPipelineResponse" $ \o -> ListJobsByPipelineResponse
        <$> o .:? "Jobs" .!= mempty
        <*> o .:? "NextPageToken"

instance AWSPager ListJobsByPipeline where
    page rq rs
        | stop (rs ^. ljbprNextPageToken) = Nothing
        | otherwise = (\x -> rq & ljbpPageToken ?~ x)
            <$> (rs ^. ljbprNextPageToken)

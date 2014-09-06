{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ListJobsByPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListJobsByPipeline operation gets a list of the jobs currently in a
-- pipeline. Elastic Transcoder returns all of the jobs currently in the
-- specified pipeline. The response body contains one element for each job
-- that satisfies the search criteria. GET
-- /2012-09-25/jobsByPipeline/1111111111111-abcde1?Ascending=true HTTP/1.1
-- Content-Type: charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Jobs":[ { "Id":"3333333333333-abcde3", "Input":{
-- "AspectRatio":"auto", "Container":"mp4", "FrameRate":"auto",
-- "Interlaced":"auto", "Key":"cooking/lasagna.mp4", "Resolution":"auto" },
-- "Outputs ":[ { "Id":"1" "Key":"cooking/lasagna-KindleFireHD.mp4",
-- "PresetId":"5555555555555-abcde5", "Rotate":"0", "Status":"Submitted",
-- "StatusDetail":"Job has been received.",
-- "ThumbnailPattern":"cooking/lasagna-{count}-KindleFireHD",
-- "Duration":"1003", "Width":"1280", "Height":"720" }, { "Id":"2"
-- "Key":"cooking/lasagna-iPhone4s.mp4", "PresetId":"1351620000000-100020",
-- "Rotate":"0", "Status":"Submitted", "StatusDetail":"Job has been
-- received.", "ThumbnailPattern":"cooking/lasagna-{count}-iPhone4s",
-- "Duration":"1003", "Width":"1920", "Height":"1080" } ], "Output":{
-- "Key":"cooking/lasagna-KindleFireHD.mp4",
-- "PresetId":"1351620000000-100080", "Rotate":"0", "Status":"Submitted",
-- "StatusDetail":"Job has been received.",
-- "ThumbnailPattern":"cooking/lasagna-{count}-KindleFireHD" },
-- "PipelineId":"1111111111111-abcde1" }, { "Id":"4444444444444-abcde4",
-- "Input":{ "AspectRatio":"auto", "Container":"mp4", "FrameRate":"auto",
-- "Interlaced":"auto", "Key":"cooking/baked-ziti.mp4", "Resolution":"auto" },
-- "Outputs":[ { "Id":"1" "Key":"cooking/baked-ziti-KindleFireHD.mp4",
-- "PresetId":"1351620000000-100080", "Rotate":"0", "Status":"Complete",
-- "StatusDetail":"",
-- "ThumbnailPattern":"cooking/baked-ziti-{count}-KindleFireHD",
-- "Duration":"596", "Width":"1280", "Height":"720" } ], "Output":{
-- "Key":"cooking/baked-ziti-KindleFireHD.mp4",
-- "PresetId":"1351620000000-100080", "Rotate":"0", "Status":"Complete",
-- "StatusDetail":"",
-- "ThumbnailPattern":"cooking/baked-ziti-{count}-KindleFireHD" },
-- "PipelineId":"1111111111111-abcde1" } ], "NextPageToken":null.
module Network.AWS.ElasticTranscoder.V2012_09_25.ListJobsByPipeline
    (
    -- * Request
      ListJobsByPipeline
    -- ** Request constructor
    , mkListJobsByPipeline
    -- ** Request lenses
    , ljbpPipelineId
    , ljbpAscending
    , ljbpPageToken

    -- * Response
    , ListJobsByPipelineResponse
    -- ** Response lenses
    , ljbprsJobs
    , ljbprsNextPageToken
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The ListJobsByPipelineRequest structure.
data ListJobsByPipeline = ListJobsByPipeline
    { _ljbpPipelineId :: Text
    , _ljbpAscending :: Maybe Text
    , _ljbpPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListJobsByPipeline' request.
mkListJobsByPipeline :: Text -- ^ 'ljbpPipelineId'
                     -> ListJobsByPipeline
mkListJobsByPipeline p1 = ListJobsByPipeline
    { _ljbpPipelineId = p1
    , _ljbpAscending = Nothing
    , _ljbpPageToken = Nothing
    }
{-# INLINE mkListJobsByPipeline #-}

-- | The ID of the pipeline for which you want to get job information.
ljbpPipelineId :: Lens' ListJobsByPipeline Text
ljbpPipelineId = lens _ljbpPipelineId (\s a -> s { _ljbpPipelineId = a })
{-# INLINE ljbpPipelineId #-}

-- | To list jobs in chronological order by the date and time that they were
-- submitted, enter true. To list jobs in reverse chronological order, enter
-- false.
ljbpAscending :: Lens' ListJobsByPipeline (Maybe Text)
ljbpAscending = lens _ljbpAscending (\s a -> s { _ljbpAscending = a })
{-# INLINE ljbpAscending #-}

-- | When Elastic Transcoder returns more than one page of results, use
-- pageToken in subsequent GET requests to get each successive page of
-- results.
ljbpPageToken :: Lens' ListJobsByPipeline (Maybe Text)
ljbpPageToken = lens _ljbpPageToken (\s a -> s { _ljbpPageToken = a })
{-# INLINE ljbpPageToken #-}

instance ToPath ListJobsByPipeline where
    toPath ListJobsByPipeline{..} = mconcat
        [ "/2012-09-25/jobsByPipeline/"
        , toBS _ljbpPipelineId
        ]

instance ToQuery ListJobsByPipeline where
    toQuery ListJobsByPipeline{..} = mconcat
        [ "Ascending" =? _ljbpAscending
        , "PageToken" =? _ljbpPageToken
        ]

instance ToHeaders ListJobsByPipeline

instance ToJSON ListJobsByPipeline

-- | The ListJobsByPipelineResponse structure.
data ListJobsByPipelineResponse = ListJobsByPipelineResponse
    { _ljbprsJobs :: [Job]
    , _ljbprsNextPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | An array of Job objects that are in the specified pipeline.
ljbprsJobs :: Lens' ListJobsByPipelineResponse [Job]
ljbprsJobs = lens _ljbprsJobs (\s a -> s { _ljbprsJobs = a })
{-# INLINE ljbprsJobs #-}

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the jobs in the specified pipeline fit on one page or when
-- you've reached the last page of results, the value of NextPageToken is
-- null.
ljbprsNextPageToken :: Lens' ListJobsByPipelineResponse (Maybe Text)
ljbprsNextPageToken =
    lens _ljbprsNextPageToken (\s a -> s { _ljbprsNextPageToken = a })
{-# INLINE ljbprsNextPageToken #-}

instance FromJSON ListJobsByPipelineResponse

instance AWSRequest ListJobsByPipeline where
    type Sv ListJobsByPipeline = ElasticTranscoder
    type Rs ListJobsByPipeline = ListJobsByPipelineResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListJobsByPipeline where
    next rq rs = (\x -> rq { _ljbpPageToken = Just x })
        <$> (_ljbprsNextPageToken rs)

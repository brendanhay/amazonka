{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ListJobsByStatus
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
-- the search criteria. GET /2012-09-25/jobsByStatus/Complete?Ascending=true
-- HTTP/1.1 Content-Type: charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Jobs":[ { "Id":"3333333333333-abcde3", "Input":{
-- "AspectRatio":"auto", "Container":"mp4", "FrameRate":"auto",
-- "Interlaced":"auto", "Key":"cooking/lasagna.mp4", "Resolution":"auto" },
-- "Output":{ "Duration":"1003", "Height":"720", "Id":"1",
-- "Key":"mp4/lasagna-kindlefirehd.mp4", "PresetId":"1351620000000-100080",
-- "Rotate":"0", "Status":"Complete", "StatusDetail":"",
-- "ThumbnailPattern":"mp4/thumbnails/lasagna-{count}", "Width":"1280" },
-- "Outputs":[ { "Duration":"1003", "Height":"720", "Id":"1",
-- "Key":"mp4/lasagna-kindlefirehd.mp4", "PresetId":"1351620000000-100080",
-- "Rotate":"0", "Status":"Complete", "StatusDetail":"",
-- "ThumbnailPattern":"mp4/thumbnails/lasagna-{count}", "Width":"1280" }, {
-- "Duration":"1003", "Height":"640", "Id":"2", "Key":"iphone/lasagna-1024k",
-- "PresetId":"1351620000000-987654", "Rotate":"0", "SegmentDuration":"5",
-- "Status":"Complete", "StatusDetail":"",
-- "ThumbnailPattern":"iphone/th1024k/lasagna-{count}", "Width":"1136" }, ],
-- "PipelineId":"1111111111111-abcde1", "Playlists":[ { "Format":"HLSv3",
-- "Name":"playlist-iPhone-lasagna.m3u8", "OutputKeys":[
-- "iphone/lasagna-1024k", "iphone/lasagna-512k" ] } ], "Status":"Complete" },
-- { "Id":"4444444444444-abcde4", "Input":{ "AspectRatio":"auto",
-- "Container":"mp4", "FrameRate":"auto", "Interlaced":"auto",
-- "Key":"cooking/spaghetti.mp4", "Resolution":"auto" }, "Output":{
-- "Duration":"1003", "Height":"640", "Id":"3", "Key":"iphone/spaghetti-512k",
-- "PresetId":"1351620000000-456789", "Rotate":"0", "SegmentDuration":"5",
-- "Status":"Complete", "StatusDetail":"",
-- "ThumbnailPattern":"iphone/th512k/spaghetti-{count}", "Width":"1136" },
-- "Outputs":[ { "Duration":"1003", "Height":"640", "Id":"3",
-- "Key":"iphone/spaghetti-512k", "PresetId":"1351620000000-456789",
-- "Rotate":"0", "SegmentDuration":"5", "Status":"Complete",
-- "StatusDetail":"", "ThumbnailPattern":"iphone/th512k/spaghetti-{count}",
-- "Width":"1136" } ], "Playlists":[ { "Format":"HLSv3",
-- "Name":"playlist-iPhone-spaghetti.m3u8", "OutputKeys":[
-- "iphone/spaghetti-512k" ] } ], "Status":"Complete" } ],
-- "NextPageToken":null }.
module Network.AWS.ElasticTranscoder.V2012_09_25.ListJobsByStatus
    (
    -- * Request
      ListJobsByStatus
    -- ** Request constructor
    , mkListJobsByStatus
    -- ** Request lenses
    , ljbsStatus
    , ljbsAscending
    , ljbsPageToken

    -- * Response
    , ListJobsByStatusResponse
    -- ** Response lenses
    , ljbsrsJobs
    , ljbsrsNextPageToken
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The ListJobsByStatusRequest structure.
data ListJobsByStatus = ListJobsByStatus
    { _ljbsStatus :: Text
    , _ljbsAscending :: Maybe Text
    , _ljbsPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListJobsByStatus' request.
mkListJobsByStatus :: Text -- ^ 'ljbsStatus'
                   -> ListJobsByStatus
mkListJobsByStatus p1 = ListJobsByStatus
    { _ljbsStatus = p1
    , _ljbsAscending = Nothing
    , _ljbsPageToken = Nothing
    }

-- | To get information about all of the jobs associated with the current AWS
-- account that have a given status, specify the following status: Submitted,
-- Progressing, Complete, Canceled, or Error.
ljbsStatus :: Lens' ListJobsByStatus Text
ljbsStatus = lens _ljbsStatus (\s a -> s { _ljbsStatus = a })

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

instance ToPath ListJobsByStatus where
    toPath ListJobsByStatus{..} = mconcat
        [ "/2012-09-25/jobsByStatus/"
        , toBS _ljbsStatus
        ]

instance ToQuery ListJobsByStatus where
    toQuery ListJobsByStatus{..} = mconcat
        [ "Ascending" =? _ljbsAscending
        , "PageToken" =? _ljbsPageToken
        ]

instance ToHeaders ListJobsByStatus

instance ToJSON ListJobsByStatus

-- | The ListJobsByStatusResponse structure.
data ListJobsByStatusResponse = ListJobsByStatusResponse
    { _ljbsrsJobs :: [Job]
    , _ljbsrsNextPageToken :: Maybe Text
    } deriving (Show, Generic)

-- | An array of Job objects that have the specified status.
ljbsrsJobs :: Lens' ListJobsByStatusResponse [Job]
ljbsrsJobs = lens _ljbsrsJobs (\s a -> s { _ljbsrsJobs = a })

-- | A value that you use to access the second and subsequent pages of results,
-- if any. When the jobs in the specified pipeline fit on one page or when
-- you've reached the last page of results, the value of NextPageToken is
-- null.
ljbsrsNextPageToken :: Lens' ListJobsByStatusResponse (Maybe Text)
ljbsrsNextPageToken =
    lens _ljbsrsNextPageToken (\s a -> s { _ljbsrsNextPageToken = a })

instance FromJSON ListJobsByStatusResponse

instance AWSRequest ListJobsByStatus where
    type Sv ListJobsByStatus = ElasticTranscoder
    type Rs ListJobsByStatus = ListJobsByStatusResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListJobsByStatus where
    next rq rs = (\x -> rq & ljbsPageToken ?~ x)
        <$> (rs ^. ljbsrsNextPageToken)

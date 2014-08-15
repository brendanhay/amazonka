{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.ElasticTranscoder.V2012_09_25.ListJobsByPipeline where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'ListJobsByPipeline' request.
listJobsByPipeline :: Text -- ^ '_ljbprPipelineId'
                   -> ListJobsByPipeline
listJobsByPipeline p1 = ListJobsByPipeline
    { _ljbprPipelineId = p1
    , _ljbprAscending = Nothing
    , _ljbprPageToken = Nothing
    }

data ListJobsByPipeline = ListJobsByPipeline
    { _ljbprPipelineId :: Text
      -- ^ The ID of the pipeline for which you want to get job information.
    , _ljbprAscending :: Maybe Text
      -- ^ To list jobs in chronological order by the date and time that
      -- they were submitted, enter true. To list jobs in reverse
      -- chronological order, enter false.
    , _ljbprPageToken :: Maybe Text
      -- ^ When Elastic Transcoder returns more than one page of results,
      -- use pageToken in subsequent GET requests to get each successive
      -- page of results.
    } deriving (Show, Generic)

makeLenses ''ListJobsByPipeline

instance ToPath ListJobsByPipeline where
    toPath ListJobsByPipeline{..} = mconcat
        [ "/2012-09-25/jobsByPipeline/"
        , toBS _ljbprPipelineId
        ]

instance ToQuery ListJobsByPipeline where
    toQuery ListJobsByPipeline{..} = mconcat
        [ "Ascending" =? _ljbprAscending
        , "PageToken" =? _ljbprPageToken
        ]

instance ToHeaders ListJobsByPipeline

instance ToJSON ListJobsByPipeline

data ListJobsByPipelineResponse = ListJobsByPipelineResponse
    { _ljbpsNextPageToken :: Maybe Text
      -- ^ A value that you use to access the second and subsequent pages of
      -- results, if any. When the jobs in the specified pipeline fit on
      -- one page or when you've reached the last page of results, the
      -- value of NextPageToken is null.
    , _ljbpsJobs :: [Job]
      -- ^ An array of Job objects that are in the specified pipeline.
    } deriving (Show, Generic)

makeLenses ''ListJobsByPipelineResponse

instance AWSRequest ListJobsByPipeline where
    type Sv ListJobsByPipeline = ElasticTranscoder
    type Rs ListJobsByPipeline = ListJobsByPipelineResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListJobsByPipeline where
    next rq rs = (\x -> rq { _ljbprPageToken = Just x })
        <$> (_ljbpsNextPageToken rs)

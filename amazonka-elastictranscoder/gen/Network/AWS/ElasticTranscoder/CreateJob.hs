{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | When you create a job, Elastic Transcoder returns JSON data that includes
-- the values that you specified plus information about the job that is
-- created. If you have specified more than one output for your jobs (for
-- example, one output for the Kindle Fire and another output for the Apple
-- iPhone 4s), you currently must use the Elastic Transcoder API to list the
-- jobs (as opposed to the AWS Console). CreateJob Example POST
-- /2012-09-25/jobs HTTP/1.1 Content-Type: application/json; charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Input":{
-- "Key":"recipes/lasagna.mp4", "FrameRate":"auto", "Resolution":"auto",
-- "AspectRatio":"auto", "Interlaced":"auto", "Container":"mp4" },
-- "OutputKeyPrefix":"recipes/", "Outputs":[ {
-- "Key":"mp4/lasagna-kindlefirehd.mp4",
-- "ThumbnailPattern":"mp4/thumbnails/lasagna-{count}", "Rotate":"0",
-- "PresetId":"1351620000000-100080" }, { "Key":"iphone/lasagna-1024k",
-- "ThumbnailPattern":"iphone/th1024k/lasagna-{count}", "Rotate":"0",
-- "PresetId":"1351620000000-987654", "SegmentDuration":"5" }, {
-- "Key":"iphone/lasagna-512k",
-- "ThumbnailPattern":"iphone/th512k/lasagna-{count}", "Rotate":"0",
-- "PresetId":"1351620000000-456789", "Watermarks":[ {
-- "InputKey":"logo/128x64.png", "PresetWatermarkId":"company logo 128x64" }
-- ], "SegmentDuration":"5" } ], "Playlists": [ { "Format": "HLSv3", "Name":
-- "playlist-iPhone-lasagna.m3u8", "OutputKeys": [ "iphone/lasagna-1024k",
-- "iphone/lasagna-512k" ] } ], "PipelineId":"1111111111111-abcde1" } Status:
-- 201 Created x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9
-- Content-Type: application/json Content-Length:
-- [number-of-characters-in-response] Date: Mon, 14 Jan 2013 06:01:47 GMT {
-- "Job":{ "Id":"3333333333333-abcde3" "Input":{ "AspectRatio":"auto",
-- "Container":"mp4", "FrameRate":"auto", "Interlaced":"auto",
-- "Key":"cooking/lasagna.mp4", "Resolution":"auto" }, "Output":{
-- "Duration":"1003", "Height":"720", "Id":"1",
-- "Key":"mp4/lasagna-kindlefirehd.mp4", "PresetId":"1351620000000-100080",
-- "Rotate":"0", "Status":"Progressing", "StatusDetail":"",
-- "ThumbnailPattern":"mp4/thumbnails/lasagna-{count}", "Width":"1280" },
-- "Outputs":[ { "Duration":"1003", "Height":"720", "Id":"1",
-- "Key":"mp4/lasagna-kindlefirehd.mp4", "PresetId":"1351620000000-100080",
-- "Rotate":"0", "Status":"Progressing", "StatusDetail":"",
-- "ThumbnailPattern":"mp4/thumbnails/lasagna-{count}", "Width":"1280" }, {
-- "Duration":"1003", "Height":"640", "Id":"2", "Key":"iphone/lasagna-1024k",
-- "PresetId":"1351620000000-987654", "Rotate":"0", "SegmentDuration":"5",
-- "Status":"Progressing", "StatusDetail":"",
-- "ThumbnailPattern":"iphone/th1024k/lasagna-{count}", "Width":"1136" }, {
-- "Duration":"1003", "Height":"640", "Id":"3", "Key":"iphone/lasagna-512k",
-- "PresetId":"1351620000000-456789", "Watermarks":[ {
-- "InputKey":"logo/128x64.png", "PresetWatermarkId":"company logo 128x64" }
-- ], "Rotate":"0", "SegmentDuration":"5", "Status":"Complete",
-- "StatusDetail":"", "ThumbnailPattern":"iphone/th512k/lasagna-{count}",
-- "Width":"1136" } ], "PipelineId":"1111111111111-abcde1", "Playlists":[ {
-- "Format":"HLSv3", "Name":"playlist-iPhone-lasagna.m3u8", "OutputKeys": [
-- "iphone/lasagna-1024k", "iphone/lasagna-512k" ] } ], "Status":"Progressing"
-- }.
module Network.AWS.ElasticTranscoder.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cj1PipelineId
    , cj1Input
    , cj1Output
    , cj1Outputs
    , cj1OutputKeyPrefix
    , cj1Playlists

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrrJob
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The CreateJobRequest structure.
data CreateJob = CreateJob
    { _cj1PipelineId :: Text
    , _cj1Input :: JobInput
    , _cj1Output :: Maybe CreateJobOutput
    , _cj1Outputs :: [CreateJobOutput]
    , _cj1OutputKeyPrefix :: Maybe Text
    , _cj1Playlists :: [CreateJobPlaylist]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJob' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
-- * @Input ::@ @JobInput@
--
-- * @Output ::@ @Maybe CreateJobOutput@
--
-- * @Outputs ::@ @[CreateJobOutput]@
--
-- * @OutputKeyPrefix ::@ @Maybe Text@
--
-- * @Playlists ::@ @[CreateJobPlaylist]@
--
createJob :: Text -- ^ 'cj1PipelineId'
          -> JobInput -- ^ 'cj1Input'
          -> CreateJob
createJob p1 p2 = CreateJob
    { _cj1PipelineId = p1
    , _cj1Input = p2
    , _cj1Output = Nothing
    , _cj1Outputs = mempty
    , _cj1OutputKeyPrefix = Nothing
    , _cj1Playlists = mempty
    }

-- | The Id of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the Amazon
-- S3 bucket from which Elastic Transcoder gets the files to transcode and the
-- bucket into which Elastic Transcoder puts the transcoded files.
cj1PipelineId :: Lens' CreateJob Text
cj1PipelineId = lens _cj1PipelineId (\s a -> s { _cj1PipelineId = a })

-- | A section of the request body that provides information about the file that
-- is being transcoded.
cj1Input :: Lens' CreateJob JobInput
cj1Input = lens _cj1Input (\s a -> s { _cj1Input = a })

-- | The CreateJobOutput structure.
cj1Output :: Lens' CreateJob (Maybe CreateJobOutput)
cj1Output = lens _cj1Output (\s a -> s { _cj1Output = a })

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the Outputs syntax
-- instead of the Output syntax.
cj1Outputs :: Lens' CreateJob [CreateJobOutput]
cj1Outputs = lens _cj1Outputs (\s a -> s { _cj1Outputs = a })

-- | The value, if any, that you want Elastic Transcoder to prepend to the names
-- of all files that this job creates, including output files, thumbnails, and
-- playlists.
cj1OutputKeyPrefix :: Lens' CreateJob (Maybe Text)
cj1OutputKeyPrefix =
    lens _cj1OutputKeyPrefix (\s a -> s { _cj1OutputKeyPrefix = a })

-- | If you specify a preset in PresetId for which the value of Container is ts
-- (MPEG-TS), Playlists contains information about the master playlists that
-- you want Elastic Transcoder to create. We recommend that you create only
-- one master playlist. The maximum number of master playlists in a job is 30.
cj1Playlists :: Lens' CreateJob [CreateJobPlaylist]
cj1Playlists = lens _cj1Playlists (\s a -> s { _cj1Playlists = a })

instance ToPath CreateJob

instance ToQuery CreateJob

instance ToHeaders CreateJob

instance ToJSON CreateJob

-- | The CreateJobResponse structure.
newtype CreateJobResponse = CreateJobResponse
    { _cjrrJob :: Maybe Job
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJobResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Job ::@ @Maybe Job@
--
createJobResponse :: CreateJobResponse
createJobResponse = CreateJobResponse
    { _cjrrJob = Nothing
    }

-- | A section of the response body that provides information about the job that
-- is created.
cjrrJob :: Lens' CreateJobResponse (Maybe Job)
cjrrJob = lens _cjrrJob (\s a -> s { _cjrrJob = a })

instance FromJSON CreateJobResponse

instance AWSRequest CreateJob where
    type Sv CreateJob = ElasticTranscoder
    type Rs CreateJob = CreateJobResponse

    request = get
    response _ = jsonResponse

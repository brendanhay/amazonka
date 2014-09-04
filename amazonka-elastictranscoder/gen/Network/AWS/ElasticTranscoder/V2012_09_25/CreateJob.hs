{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.CreateJob
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
module Network.AWS.ElasticTranscoder.V2012_09_25.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , mkCreateJobRequest
    -- ** Request lenses
    , cjtPipelineId
    , cjtInput
    , cjtOutput
    , cjtOutputs
    , cjtOutputKeyPrefix
    , cjtPlaylists

    -- * Response
    , CreateJobResponse
    -- ** Response lenses
    , cjwJob
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJob' request.
mkCreateJobRequest :: Text -- ^ 'cjtPipelineId'
                   -> JobInput -- ^ 'cjtInput'
                   -> CreateJob
mkCreateJobRequest p1 p2 = CreateJob
    { _cjtPipelineId = p1
    , _cjtInput = p2
    , _cjtOutput = Nothing
    , _cjtOutputs = mempty
    , _cjtOutputKeyPrefix = Nothing
    , _cjtPlaylists = mempty
    }
{-# INLINE mkCreateJobRequest #-}

data CreateJob = CreateJob
    { _cjtPipelineId :: Text
      -- ^ The Id of the pipeline that you want Elastic Transcoder to use
      -- for transcoding. The pipeline determines several settings,
      -- including the Amazon S3 bucket from which Elastic Transcoder gets
      -- the files to transcode and the bucket into which Elastic
      -- Transcoder puts the transcoded files.
    , _cjtInput :: JobInput
      -- ^ A section of the request body that provides information about the
      -- file that is being transcoded.
    , _cjtOutput :: Maybe CreateJobOutput
      -- ^ The CreateJobOutput structure.
    , _cjtOutputs :: [CreateJobOutput]
      -- ^ A section of the request body that provides information about the
      -- transcoded (target) files. We recommend that you use the Outputs
      -- syntax instead of the Output syntax.
    , _cjtOutputKeyPrefix :: Maybe Text
      -- ^ The value, if any, that you want Elastic Transcoder to prepend to
      -- the names of all files that this job creates, including output
      -- files, thumbnails, and playlists.
    , _cjtPlaylists :: [CreateJobPlaylist]
      -- ^ If you specify a preset in PresetId for which the value of
      -- Container is ts (MPEG-TS), Playlists contains information about
      -- the master playlists that you want Elastic Transcoder to create.
      -- We recommend that you create only one master playlist. The
      -- maximum number of master playlists in a job is 30.
    } deriving (Show, Generic)

-- | The Id of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the Amazon
-- S3 bucket from which Elastic Transcoder gets the files to transcode and the
-- bucket into which Elastic Transcoder puts the transcoded files.
cjtPipelineId :: Lens' CreateJob (Text)
cjtPipelineId = lens _cjtPipelineId (\s a -> s { _cjtPipelineId = a })
{-# INLINE cjtPipelineId #-}

-- | A section of the request body that provides information about the file that
-- is being transcoded.
cjtInput :: Lens' CreateJob (JobInput)
cjtInput = lens _cjtInput (\s a -> s { _cjtInput = a })
{-# INLINE cjtInput #-}

-- | The CreateJobOutput structure.
cjtOutput :: Lens' CreateJob (Maybe CreateJobOutput)
cjtOutput = lens _cjtOutput (\s a -> s { _cjtOutput = a })
{-# INLINE cjtOutput #-}

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the Outputs syntax
-- instead of the Output syntax.
cjtOutputs :: Lens' CreateJob ([CreateJobOutput])
cjtOutputs = lens _cjtOutputs (\s a -> s { _cjtOutputs = a })
{-# INLINE cjtOutputs #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names
-- of all files that this job creates, including output files, thumbnails, and
-- playlists.
cjtOutputKeyPrefix :: Lens' CreateJob (Maybe Text)
cjtOutputKeyPrefix = lens _cjtOutputKeyPrefix (\s a -> s { _cjtOutputKeyPrefix = a })
{-# INLINE cjtOutputKeyPrefix #-}

-- | If you specify a preset in PresetId for which the value of Container is ts
-- (MPEG-TS), Playlists contains information about the master playlists that
-- you want Elastic Transcoder to create. We recommend that you create only
-- one master playlist. The maximum number of master playlists in a job is 30.
cjtPlaylists :: Lens' CreateJob ([CreateJobPlaylist])
cjtPlaylists = lens _cjtPlaylists (\s a -> s { _cjtPlaylists = a })
{-# INLINE cjtPlaylists #-}

instance ToPath CreateJob where
    toPath = const "/2012-09-25/jobs"

instance ToQuery CreateJob

instance ToHeaders CreateJob

instance ToJSON CreateJob

newtype CreateJobResponse = CreateJobResponse
    { _cjwJob :: Maybe Job
      -- ^ A section of the response body that provides information about
      -- the job that is created.
    } deriving (Show, Generic)

-- | A section of the response body that provides information about the job that
-- is created.
cjwJob :: Lens' CreateJobResponse (Maybe Job)
cjwJob = lens _cjwJob (\s a -> s { _cjwJob = a })
{-# INLINE cjwJob #-}

instance FromJSON CreateJobResponse

instance AWSRequest CreateJob where
    type Sv CreateJob = ElasticTranscoder
    type Rs CreateJob = CreateJobResponse

    request = post
    response _ = jsonResponse

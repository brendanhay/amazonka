{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.ElasticTranscoder" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.ElasticTranscoder
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.ElasticTranscoder.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.ElasticTranscoder.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.ElasticTranscoder.Monadic
    (
    -- * CancelJob
    -- $CancelJob
      cancelJob
    , cancelJobCatch

    -- * CreateJob
    -- $CreateJob
    , createJob
    , createJobCatch

    -- * CreatePipeline
    -- $CreatePipeline
    , createPipeline
    , createPipelineCatch

    -- * CreatePreset
    -- $CreatePreset
    , createPreset
    , createPresetCatch

    -- * DeletePipeline
    -- $DeletePipeline
    , deletePipeline
    , deletePipelineCatch

    -- * DeletePreset
    -- $DeletePreset
    , deletePreset
    , deletePresetCatch

    -- * ListJobsByPipeline
    -- $ListJobsByPipeline
    , listJobsByPipeline
    , listJobsByPipelineCatch

    -- * ListJobsByStatus
    -- $ListJobsByStatus
    , listJobsByStatus
    , listJobsByStatusCatch

    -- * ListPipelines
    -- $ListPipelines
    , listPipelines
    , listPipelinesCatch

    -- * ListPresets
    -- $ListPresets
    , listPresets
    , listPresetsCatch

    -- * ReadJob
    -- $ReadJob
    , readJob
    , readJobCatch

    -- * ReadPipeline
    -- $ReadPipeline
    , readPipeline
    , readPipelineCatch

    -- * ReadPreset
    -- $ReadPreset
    , readPreset
    , readPresetCatch

    -- * TestRole
    -- $TestRole
    , testRole
    , testRoleCatch

    -- * UpdatePipeline
    -- $UpdatePipeline
    , updatePipeline
    , updatePipelineCatch

    -- * UpdatePipelineNotifications
    -- $UpdatePipelineNotifications
    , updatePipelineNotifications
    , updatePipelineNotificationsCatch

    -- * UpdatePipelineStatus
    -- $UpdatePipelineStatus
    , updatePipelineStatus
    , updatePipelineStatusCatch

    -- * Re-exported
    , module Network.AWS.ElasticTranscoder

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.ElasticTranscoder

type ServiceEr = Er ElasticTranscoder

-- $CancelJob
-- The CancelJob operation cancels an unfinished job. You can only cancel a
-- job that has a status of Submitted. To prevent a pipeline from starting to
-- process a job while you're getting the job identifier, use
-- UpdatePipelineStatus to temporarily pause the pipeline. DELETE
-- /2012-09-25/jobs/3333333333333-abcde3 HTTP/1.1 Content-Type: charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Success":"true" }.
--
-- See: 'Network.AWS.ElasticTranscoder.CancelJob'

cancelJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'cjId'
    -> m CancelJobResponse
cancelJob p1 =
    send (mkCancelJob p1)

cancelJobCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cjId'
    -> m (Either ServiceEr CancelJobResponse)
cancelJobCatch p1 =
    sendCatch (mkCancelJob p1)

-- $CreateJob
-- When you create a job, Elastic Transcoder returns JSON data that includes
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
--
-- See: 'Network.AWS.ElasticTranscoder.CreateJob'

createJob :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'cj1PipelineId'
    -> JobInput -- ^ 'cj1Input'
    -> State CreateJob a
    -> m CreateJobResponse
createJob p1 p2 s =
    send $ (mkCreateJob p1 p2) &~ s

createJobCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cj1PipelineId'
    -> JobInput -- ^ 'cj1Input'
    -> State CreateJob a
    -> m (Either ServiceEr CreateJobResponse)
createJobCatch p1 p2 s =
    sendCatch $ (mkCreateJob p1 p2) &~ s

-- $CreatePipeline
-- The CreatePipeline operation creates a pipeline with settings that you
-- specify. POST /2012-09-25/pipelines HTTP/1.1 Content-Type:
-- application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Name":"Default",
-- "InputBucket":"salesoffice.example.com-source",
-- "OutputBucket":"salesoffice.example.com-public-promos",
-- "Role":"arn:aws:iam::123456789012:role/transcode-service",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" } "ContentConfig":{
-- "Bucket": "My-S3-bucket", "Permissions":[ { "GranteeType":"Email",
-- "Grantee": "marketing-promos@example.com", "Access":[ "Read" ] } ],
-- "StorageClass":"Standard" }, "ThumbnailConfig":{ "Bucket":"My-S3-bucket",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos@example.com", "Access":[ "Read" ] } ],
-- "StorageClass":"Standard" } } Status: 201 Created x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Pipeline":{ Id":"1111111111111-abcde1",
-- "InputBucket":"salesoffice.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ET_Errors", "Progressing":"",
-- "Warning":"" }, "ContentConfig":{
-- "Bucket":"salesoffice.example.com-public-promos", "Permissions":[ {
-- "GranteeType":"Email", "Grantee":"marketing-promos@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"Standard" }, "ThumbnailConfig":{
-- "Bucket":"salesoffice.example.com-public-promos-thumbnails",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos@example.com", "Access":[ "FullControl" ] } ],
-- "StorageClass":"ReducedRedundancy" }, "Status":"Active" } }.
--
-- See: 'Network.AWS.ElasticTranscoder.CreatePipeline'

createPipeline :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cpName'
    -> Text -- ^ 'cpInputBucket'
    -> Text -- ^ 'cpRole'
    -> State CreatePipeline a
    -> m CreatePipelineResponse
createPipeline p1 p2 p4 s =
    send $ (mkCreatePipeline p1 p2 p4) &~ s

createPipelineCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cpName'
    -> Text -- ^ 'cpInputBucket'
    -> Text -- ^ 'cpRole'
    -> State CreatePipeline a
    -> m (Either ServiceEr CreatePipelineResponse)
createPipelineCatch p1 p2 p4 s =
    sendCatch $ (mkCreatePipeline p1 p2 p4) &~ s

-- $CreatePreset
-- The CreatePreset operation creates a preset with settings that you specify.
-- Elastic Transcoder checks the CreatePreset settings to ensure that they
-- meet Elastic Transcoder requirements and to determine whether they comply
-- with H.264 standards. If your settings are not valid for Elastic
-- Transcoder, Elastic Transcoder returns an HTTP 400 response
-- (ValidationException) and does not create the preset. If the settings are
-- valid for Elastic Transcoder but aren't strictly compliant with the H.264
-- standard, Elastic Transcoder creates the preset and returns a warning
-- message in the response. This helps you determine whether your settings
-- comply with the H.264 standard while giving you greater flexibility with
-- respect to the video that Elastic Transcoder produces. Elastic Transcoder
-- uses the H.264 video-compression format. For more information, see the
-- International Telecommunication Union publication Recommendation ITU-T
-- H.264: Advanced video coding for generic audiovisual services. POST
-- /2012-09-25/presets HTTP/1.1 Content-Type: application/json; charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Name":"DefaultPreset",
-- "Description":"Use for published videos", "Container":"mp4", "Audio":{
-- "Codec":"AAC", "SampleRate":"44100", "BitRate":"96", "Channels":"2" },
-- "Video":{ "Codec":"H.264", "CodecOptions":{ "Profile":"main",
-- "Level":"2.2", "MaxReferenceFrames":"3", "MaxBitRate":"", "BufferSize":""
-- }, "KeyframesMaxDist":"240", "FixedGOP":"false", "BitRate":"1600",
-- "FrameRate":"30", "MaxWidth": "auto", "MaxHeight": "auto", "SizingPolicy":
-- "Fit", "PaddingPolicy": "NoPad", "DisplayAspectRatio": "16:9",
-- "Watermarks":[ { "Id":"company logo", "MaxWidth":"20%", "MaxHeight":"20%",
-- "SizingPolicy":"ShrinkToFit", "HorizontalAlign":"Right",
-- "HorizontalOffset":"10px", "VerticalAlign":"Bottom",
-- "VerticalOffset":"10px", "Opacity":"55.5", "Target":"Content" } ]},
-- "Thumbnails":{ "Format":"png", "Interval":"120", "MaxWidth": "auto,
-- "MaxHeight": "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad" } }
-- Status: 201 Created x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9
-- Content-Type: application/json Content-Length:
-- [number-of-characters-in-response] Date: Mon, 14 Jan 2013 06:01:47 GMT {
-- "Preset":{ "Audio":{ "BitRate":"96", "Channels":"2", "Codec":"AAC",
-- "SampleRate":"44100" }, "Container":"mp4", "Description":"Use for published
-- videos", "Id":"5555555555555-abcde5", "Name":"DefaultPreset",
-- "Thumbnails":{ "Format":"png", "Interval":"120", "MaxWidth": "auto,
-- "MaxHeight": "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad" },
-- "Type":"Custom", "Video":{ "Codec":"H.264", "CodecOptions":{
-- "Profile":"main", "Level":"2.2", "MaxReferenceFrames":"3", "MaxBitRate":"",
-- "BufferSize":"" }, "KeyframesMaxDist":"240", "FixedGOP":"false",
-- "BitRate":"1600", "FrameRate":"30", "MaxWidth": "auto", "MaxHeight":
-- "auto", "SizingPolicy": "Fit", "PaddingPolicy": "NoPad",
-- "DisplayAspectRatio": "16:9", "Watermarks":[ { "Id":"company logo",
-- "MaxWidth":"20%", "MaxHeight":"20%", "SizingPolicy":"ShrinkToFit",
-- "HorizontalAlign":"Right", "HorizontalOffset":"10px",
-- "VerticalAlign":"Bottom", "VerticalOffset":"10px", "Opacity":"55.5",
-- "Target":"Content" } ] } }, "Warning":"" }.
--
-- See: 'Network.AWS.ElasticTranscoder.CreatePreset'

createPreset :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'cp1Name'
    -> Text -- ^ 'cp1Container'
    -> State CreatePreset a
    -> m CreatePresetResponse
createPreset p1 p3 s =
    send $ (mkCreatePreset p1 p3) &~ s

createPresetCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cp1Name'
    -> Text -- ^ 'cp1Container'
    -> State CreatePreset a
    -> m (Either ServiceEr CreatePresetResponse)
createPresetCatch p1 p3 s =
    sendCatch $ (mkCreatePreset p1 p3) &~ s

-- $DeletePipeline
-- The DeletePipeline operation removes a pipeline. You can only delete a
-- pipeline that has never been used or that is not currently in use (doesn't
-- contain any active jobs). If the pipeline is currently in use,
-- DeletePipeline returns an error. DELETE
-- /2012-09-25/pipelines/1111111111111-abcde1 HTTP/1.1 Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Success":"true" }.
--
-- See: 'Network.AWS.ElasticTranscoder.DeletePipeline'

deletePipeline :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'dpId'
    -> m DeletePipelineResponse
deletePipeline p1 =
    send (mkDeletePipeline p1)

deletePipelineCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dpId'
    -> m (Either ServiceEr DeletePipelineResponse)
deletePipelineCatch p1 =
    sendCatch (mkDeletePipeline p1)

-- $DeletePreset
-- The DeletePreset operation removes a preset that you've added in an AWS
-- region. You can't delete the default presets that are included with Elastic
-- Transcoder. DELETE /2012-09-25/pipelines/5555555555555-abcde5 HTTP/1.1
-- Content-Type: charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Success":"true" }.
--
-- See: 'Network.AWS.ElasticTranscoder.DeletePreset'

deletePreset :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'dp1Id'
    -> m DeletePresetResponse
deletePreset p1 =
    send (mkDeletePreset p1)

deletePresetCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dp1Id'
    -> m (Either ServiceEr DeletePresetResponse)
deletePresetCatch p1 =
    sendCatch (mkDeletePreset p1)

-- $ListJobsByPipeline
-- The ListJobsByPipeline operation gets a list of the jobs currently in a
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
--
-- See: 'Network.AWS.ElasticTranscoder.ListJobsByPipeline'

listJobsByPipeline :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'ljbpPipelineId'
    -> State ListJobsByPipeline a
    -> Source m ListJobsByPipelineResponse
listJobsByPipeline p1 s =
    paginate $ (mkListJobsByPipeline p1) &~ s

listJobsByPipelineCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'ljbpPipelineId'
    -> State ListJobsByPipeline a
    -> Source m (Either ServiceEr ListJobsByPipelineResponse)
listJobsByPipelineCatch p1 s =
    paginateCatch $ (mkListJobsByPipeline p1) &~ s

-- $ListJobsByStatus
-- The ListJobsByStatus operation gets a list of jobs that have a specified
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
--
-- See: 'Network.AWS.ElasticTranscoder.ListJobsByStatus'

listJobsByStatus :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ljbsStatus'
    -> State ListJobsByStatus a
    -> Source m ListJobsByStatusResponse
listJobsByStatus p1 s =
    paginate $ (mkListJobsByStatus p1) &~ s

listJobsByStatusCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'ljbsStatus'
    -> State ListJobsByStatus a
    -> Source m (Either ServiceEr ListJobsByStatusResponse)
listJobsByStatusCatch p1 s =
    paginateCatch $ (mkListJobsByStatus p1) &~ s

-- $ListPipelines
-- The ListPipelines operation gets a list of the pipelines associated with
-- the current AWS account. GET /2012-09-25/pipelines HTTP/1.1 Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Pipelines":[ { "Id":"1111111111111-abcde1",
-- "Name":"Tokyo-Default",
-- "InputBucket":"salesoffice-tokyo.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" },
-- "ContentConfig":{ "Bucket":"salesoffice-tokyo.example.com-public-promos",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-tokyo@example.com", "Access":[ "FullControl" ]
-- } ], "StorageClass":"Standard" }, "ThumbnailConfig":{
-- "Bucket":"salesoffice-tokyo.example.com-public-promos-thumbnails",
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-tokyo@example.com", "Access":[ "FullControl" ]
-- } ], "StorageClass":"ReducedRedundancy" }, "Status":"Active" }, {
-- "Id":"2222222222222-abcde2", "Name":"Amsterdam-Default",
-- "InputBucket":"salesoffice-amsterdam.example.com-source",
-- "Role":"arn:aws:iam::123456789012:role/Elastic_Transcoder_Default_Role",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" },
-- "ContentConfig":{
-- "Bucket":"salesoffice-amsterdam.example.com-public-promos", "Permissions":[
-- { "GranteeType":"Email",
-- "Grantee":"marketing-promos-amsterdam@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"Standard" }, nails",
-- "ThumbnailConfig":{
-- "Bucket":"salesoffice-amsterdam.example.com-public-promos-thumb
-- "Permissions":[ { "GranteeType":"Email",
-- "Grantee":"marketing-promos-amsterdam@example.com", "Access":[
-- "FullControl" ] } ], "StorageClass":"ReducedRedundancy" },
-- "Status":"Active" } ] }.
--
-- See: 'Network.AWS.ElasticTranscoder.ListPipelines'

listPipelines :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => State ListPipelines a
    -> Source m ListPipelinesResponse
listPipelines s =
    paginate (mkListPipelines &~ s)

listPipelinesCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => State ListPipelines a
    -> Source m (Either ServiceEr ListPipelinesResponse)
listPipelinesCatch s =
    paginateCatch (mkListPipelines &~ s)

-- $ListPresets
-- The ListPresets operation gets a list of the default presets included with
-- Elastic Transcoder and the presets that you've added in an AWS region. GET
-- /2012-09-25/presets HTTP/1.1 Content-Type: charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Presets":[ { "Audio":{ "BitRate":"96", "Channels":"2",
-- "Codec":"AAC", "SampleRate":"44100" }, "Container":"mp4",
-- "Description":"Use for published videos", "Id":"5555555555555-abcde5",
-- "Name":"DefaultPreset", "Thumbnails":{ "Format":"png", "Interval":"120",
-- "MaxHeight":"auto", "MaxWidth":"auto", "PaddingPolicy":"Pad",
-- "SizingPolicy":"Fit" }, "Type":"Custom", "Video":{ "BitRate":"1600",
-- "Codec":"H.264", "CodecOptions":{ "Level":"2.2", "MaxReferenceFrames":"3",
-- "Profile":"main", "MaxBitRate":"", "BufferSize":"" },
-- "DisplayAspectRatio":"auto", "FixedGOP":"false", "FrameRate":"30",
-- "KeyframesMaxDist":"240", "MaxHeight":"auto", "MaxWidth":"auto",
-- "PaddingPolicy":"Pad", "SizingPolicy":"Fit" } }, {...} ] }.
--
-- See: 'Network.AWS.ElasticTranscoder.ListPresets'

listPresets :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => State ListPresets a
    -> Source m ListPresetsResponse
listPresets s =
    paginate (mkListPresets &~ s)

listPresetsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => State ListPresets a
    -> Source m (Either ServiceEr ListPresetsResponse)
listPresetsCatch s =
    paginateCatch (mkListPresets &~ s)

-- $ReadJob
-- The ReadJob operation returns detailed information about a job. GET
-- /2012-09-25/jobs/3333333333333-abcde3 HTTP/1.1 Content-Type: charset=UTF-8
-- Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Job":{ "Id":"3333333333333-abcde3", "Input":{
-- "AspectRatio":"auto", "Container":"mp4", "FrameRate":"auto",
-- "Interlaced":"auto", "Key":"cooking/lasagna.mp4", "Resolution":"auto" },
-- "Output":{ "Key":"", "PresetId":"5555555555555-abcde5", "Rotate":"0",
-- "Status":"Submitted", "StatusDetail":"", "ThumbnailPattern":"{count}" },
-- "PipelineId":"1111111111111-abcde1" } }.
--
-- See: 'Network.AWS.ElasticTranscoder.ReadJob'

readJob :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           )
    => Text -- ^ 'rjId'
    -> m ReadJobResponse
readJob p1 =
    send (mkReadJob p1)

readJobCatch :: ( MonadCatch m
                , MonadResource m
                , MonadReader Env m
                )
    => Text -- ^ 'rjId'
    -> m (Either ServiceEr ReadJobResponse)
readJobCatch p1 =
    sendCatch (mkReadJob p1)

-- $ReadPipeline
-- The ReadPipeline operation gets detailed information about a pipeline. GET
-- /2012-09-25/pipelines/1111111111111-abcde1 HTTP/1.1 Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Status: 200 OK x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Pipeline":{ "Id":"1111111111111-abcde1",
-- "InputBucket":"salesoffice.example.com-source", "Name":"Default",
-- "Notifications":{ "Completed":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors", "Progressing":"",
-- "Warning":"" }, "OutputBucket":"salesoffice.example.com-public-promos",
-- "Role":"arn:aws:iam::123456789012:role/transcode-service",
-- "Status":"Active" } }.
--
-- See: 'Network.AWS.ElasticTranscoder.ReadPipeline'

readPipeline :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => Text -- ^ 'rpId'
    -> m ReadPipelineResponse
readPipeline p1 =
    send (mkReadPipeline p1)

readPipelineCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => Text -- ^ 'rpId'
    -> m (Either ServiceEr ReadPipelineResponse)
readPipelineCatch p1 =
    sendCatch (mkReadPipeline p1)

-- $ReadPreset
-- The ReadPreset operation gets detailed information about a preset. GET
-- /2012-09-25/presets/5555555555555-abcde5 HTTP/1.1 Content-Type:
-- application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] Status: 200 OK Content-Type:
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] { "Preset":{ "Audio":{ "Codec":"AAC",
-- "SampleRate":"44100", "BitRate":96, "Channels":2 }, "Container":"mp4",
-- "Description":"Use for published videos", "Id":"5555555555555-abcde5",
-- "Name":"DefaultPreset", "Thumbnails":{ "Format":"png", "Interval":"120",
-- "MaxWidth": "auto", "MaxHeight": "auto", "SizingPolicy": "Fill",
-- "PaddingPolicy": "Pad" }, "Type":"Custom", "Video":{ "MaxWidth": "auto",
-- "MaxHeight": "auto", "SizingPolicy": "Fill", "PaddingPolicy": "Pad",
-- "DisplayAspectRatio": "auto", "BitRate":"1600", "Codec":"H.264",
-- "CodecOptions":{ "Level":"2.2", "MaxReferenceFrames":"3", "Profile":"main",
-- "MaxBitRate":"", "BufferSize":"" }, "FixedGOP":"false", "FrameRate":"30",
-- "KeyframesMaxDist":"240" } } }.
--
-- See: 'Network.AWS.ElasticTranscoder.ReadPreset'

readPreset :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => Text -- ^ 'rp1Id'
    -> m ReadPresetResponse
readPreset p1 =
    send (mkReadPreset p1)

readPresetCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => Text -- ^ 'rp1Id'
    -> m (Either ServiceEr ReadPresetResponse)
readPresetCatch p1 =
    sendCatch (mkReadPreset p1)

-- $TestRole
-- The TestRole operation tests the IAM role used to create the pipeline. The
-- TestRole action lets you determine whether the IAM role you are using has
-- sufficient permissions to let Elastic Transcoder perform tasks associated
-- with the transcoding process. The action attempts to assume the specified
-- IAM role, checks read access to the input and output buckets, and tries to
-- send a test notification to Amazon SNS topics that you specify. POST
-- /2012-09-25/roleTests HTTP/1.1 Content-Type: application/json;
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] {
-- "InputBucket":"salesoffice.example.com-source",
-- "OutputBucket":"salesoffice.example.com-public-promos",
-- "Role":"arn:aws:iam::123456789012:role/transcode-service", "Topics":
-- ["arn:aws:sns:us-east-1:111222333444:ETS_Errors",
-- "arn:aws:sns:us-east-1:111222333444:ETS_Progressing"] } Status: 200 OK
-- x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type:
-- application/json Content-Length: [number-of-characters-in-response] Date:
-- Mon, 14 Jan 2013 06:01:47 GMT { "Messages":[ "The role
-- arn:aws:iam::123456789012:role/transcode-service does not have access to
-- the bucket: salesoffice.example.com-source", "The role
-- arn:aws:iam::123456789012:role/transcode-service does not have access to
-- the topic: arn:aws:sns:us-east-1:111222333444:ETS_Errors" ], "Success":
-- "false" }.
--
-- See: 'Network.AWS.ElasticTranscoder.TestRole'

testRole :: ( MonadCatch m
            , MonadResource m
            , MonadError AWS.Error m
            , MonadReader Env m
            )
    => Text -- ^ 'trRole'
    -> Text -- ^ 'trInputBucket'
    -> Text -- ^ 'trOutputBucket'
    -> [Text] -- ^ 'trTopics'
    -> m TestRoleResponse
testRole p1 p2 p3 p4 =
    send (mkTestRole p1 p2 p3 p4)

testRoleCatch :: ( MonadCatch m
                 , MonadResource m
                 , MonadReader Env m
                 )
    => Text -- ^ 'trRole'
    -> Text -- ^ 'trInputBucket'
    -> Text -- ^ 'trOutputBucket'
    -> [Text] -- ^ 'trTopics'
    -> m (Either ServiceEr TestRoleResponse)
testRoleCatch p1 p2 p3 p4 =
    sendCatch (mkTestRole p1 p2 p3 p4)

-- $UpdatePipeline
-- Use the UpdatePipeline operation to update settings for a pipeline. When
-- you change pipeline settings, your changes take effect immediately. Jobs
-- that you have already submitted and that Elastic Transcoder has not started
-- to process are affected in addition to jobs that you submit after you
-- change settings.
--
-- See: 'Network.AWS.ElasticTranscoder.UpdatePipeline'

updatePipeline :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'upId'
    -> State UpdatePipeline a
    -> m UpdatePipelineResponse
updatePipeline p1 s =
    send $ (mkUpdatePipeline p1) &~ s

updatePipelineCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'upId'
    -> State UpdatePipeline a
    -> m (Either ServiceEr UpdatePipelineResponse)
updatePipelineCatch p1 s =
    sendCatch $ (mkUpdatePipeline p1) &~ s

-- $UpdatePipelineNotifications
-- With the UpdatePipelineNotifications operation, you can update Amazon
-- Simple Notification Service (Amazon SNS) notifications for a pipeline. When
-- you update notifications for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request. POST
-- /2012-09-25/pipelines/1111111111111-abcde1/notifications HTTP/1.1
-- Content-Type: application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Id":"1111111111111-abcde1",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" } } Status: 202
-- Accepted x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9
-- Content-Type: application/json Content-Length:
-- [number-of-characters-in-response] Date: Mon, 14 Jan 2013 06:01:47 GMT {
-- "Id":"1111111111111-abcde1", "Notifications":{ "Completed":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors", "Progressing":"",
-- "Warning":"" } }.
--
-- See: 'Network.AWS.ElasticTranscoder.UpdatePipelineNotifications'

updatePipelineNotifications :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'upnId'
    -> Notifications -- ^ 'upnNotifications'
    -> m UpdatePipelineNotificationsResponse
updatePipelineNotifications p1 p2 =
    send (mkUpdatePipelineNotifications p1 p2)

updatePipelineNotificationsCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'upnId'
    -> Notifications -- ^ 'upnNotifications'
    -> m (Either ServiceEr UpdatePipelineNotificationsResponse)
updatePipelineNotificationsCatch p1 p2 =
    sendCatch (mkUpdatePipelineNotifications p1 p2)

-- $UpdatePipelineStatus
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so
-- that the pipeline stops or restarts the processing of jobs. Changing the
-- pipeline status is useful if you want to cancel one or more jobs. You can't
-- cancel jobs after Elastic Transcoder has started processing them; if you
-- pause the pipeline to which you submitted the jobs, you have more time to
-- get the job IDs for the jobs that you want to cancel, and to send a
-- CancelJob request. POST /2012-09-25/pipelines/1111111111111-abcde1/status
-- HTTP/1.1 Content-Type: application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Id":"1111111111111-abcde1",
-- "Status":"Active" } Status: 202 Accepted x-amzn-RequestId:
-- c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type: application/json
-- Content-Length: [number-of-characters-in-response] Date: Mon, 14 Jan 2013
-- 06:01:47 GMT { "Id":"1111111111111-abcde1", "Status":"Active" }.
--
-- See: 'Network.AWS.ElasticTranscoder.UpdatePipelineStatus'

updatePipelineStatus :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'upsId'
    -> Text -- ^ 'upsStatus'
    -> m UpdatePipelineStatusResponse
updatePipelineStatus p1 p2 =
    send (mkUpdatePipelineStatus p1 p2)

updatePipelineStatusCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'upsId'
    -> Text -- ^ 'upsStatus'
    -> m (Either ServiceEr UpdatePipelineStatusResponse)
updatePipelineStatusCatch p1 p2 =
    sendCatch (mkUpdatePipelineStatus p1 p2)

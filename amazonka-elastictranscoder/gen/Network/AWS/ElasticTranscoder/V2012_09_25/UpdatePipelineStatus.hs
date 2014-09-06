{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipelineStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The UpdatePipelineStatus operation pauses or reactivates a pipeline, so
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
module Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipelineStatus
    (
    -- * Request
      UpdatePipelineStatus
    -- ** Request constructor
    , mkUpdatePipelineStatus
    -- ** Request lenses
    , upsId
    , upsStatus

    -- * Response
    , UpdatePipelineStatusResponse
    -- ** Response lenses
    , upsrsPipeline
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The UpdatePipelineStatusRequest structure.
data UpdatePipelineStatus = UpdatePipelineStatus
    { _upsId :: Text
    , _upsStatus :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdatePipelineStatus' request.
mkUpdatePipelineStatus :: Text -- ^ 'upsId'
                       -> Text -- ^ 'upsStatus'
                       -> UpdatePipelineStatus
mkUpdatePipelineStatus p1 p2 = UpdatePipelineStatus
    { _upsId = p1
    , _upsStatus = p2
    }
{-# INLINE mkUpdatePipelineStatus #-}

-- | The identifier of the pipeline to update.
upsId :: Lens' UpdatePipelineStatus Text
upsId = lens _upsId (\s a -> s { _upsId = a })
{-# INLINE upsId #-}

-- | The desired status of the pipeline: Active: The pipeline is processing
-- jobs. Paused: The pipeline is not currently processing jobs.
upsStatus :: Lens' UpdatePipelineStatus Text
upsStatus = lens _upsStatus (\s a -> s { _upsStatus = a })
{-# INLINE upsStatus #-}

instance ToPath UpdatePipelineStatus where
    toPath UpdatePipelineStatus{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toBS _upsId
        , "/status"
        ]

instance ToQuery UpdatePipelineStatus

instance ToHeaders UpdatePipelineStatus

instance ToJSON UpdatePipelineStatus

-- | When you update status for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request.
newtype UpdatePipelineStatusResponse = UpdatePipelineStatusResponse
    { _upsrsPipeline :: Maybe Pipeline
    } deriving (Show, Generic)

-- | A section of the response body that provides information about the
-- pipeline.
upsrsPipeline :: Lens' UpdatePipelineStatusResponse (Maybe Pipeline)
upsrsPipeline = lens _upsrsPipeline (\s a -> s { _upsrsPipeline = a })
{-# INLINE upsrsPipeline #-}

instance FromJSON UpdatePipelineStatusResponse

instance AWSRequest UpdatePipelineStatus where
    type Sv UpdatePipelineStatus = ElasticTranscoder
    type Rs UpdatePipelineStatus = UpdatePipelineStatusResponse

    request = post
    response _ = jsonResponse

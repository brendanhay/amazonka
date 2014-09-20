{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
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
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
    (
    -- * Request
      UpdatePipelineStatus
    -- ** Request constructor
    , updatePipelineStatus
    -- ** Request lenses
    , upsId
    , upsStatus

    -- * Response
    , UpdatePipelineStatusResponse
    -- ** Response constructor
    , updatePipelineStatusResponse
    -- ** Response lenses
    , upsrPipeline
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The UpdatePipelineStatusRequest structure.
data UpdatePipelineStatus = UpdatePipelineStatus
    { _upsId :: Text
    , _upsStatus :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdatePipelineStatus' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @Status ::@ @Text@
--
updatePipelineStatus :: Text -- ^ 'upsId'
                     -> Text -- ^ 'upsStatus'
                     -> UpdatePipelineStatus
updatePipelineStatus p1 p2 = UpdatePipelineStatus
    { _upsId = p1
    , _upsStatus = p2
    }

-- | The identifier of the pipeline to update.
upsId :: Lens' UpdatePipelineStatus Text
upsId = lens _upsId (\s a -> s { _upsId = a })

-- | The desired status of the pipeline: Active: The pipeline is processing
-- jobs. Paused: The pipeline is not currently processing jobs.
upsStatus :: Lens' UpdatePipelineStatus Text
upsStatus = lens _upsStatus (\s a -> s { _upsStatus = a })

instance ToPath UpdatePipelineStatus

instance ToQuery UpdatePipelineStatus

instance ToHeaders UpdatePipelineStatus

instance ToJSON UpdatePipelineStatus

-- | When you update status for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request.
newtype UpdatePipelineStatusResponse = UpdatePipelineStatusResponse
    { _upsrPipeline :: Maybe Pipeline
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdatePipelineStatusResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Pipeline ::@ @Maybe Pipeline@
--
updatePipelineStatusResponse :: UpdatePipelineStatusResponse
updatePipelineStatusResponse = UpdatePipelineStatusResponse
    { _upsrPipeline = Nothing
    }

-- | A section of the response body that provides information about the
-- pipeline.
upsrPipeline :: Lens' UpdatePipelineStatusResponse (Maybe Pipeline)
upsrPipeline = lens _upsrPipeline (\s a -> s { _upsrPipeline = a })

instance FromJSON UpdatePipelineStatusResponse

instance AWSRequest UpdatePipelineStatus where
    type Sv UpdatePipelineStatus = ElasticTranscoder
    type Rs UpdatePipelineStatus = UpdatePipelineStatusResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.ReadPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ReadPipeline operation gets detailed information about a pipeline. GET
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
module Network.AWS.ElasticTranscoder.V2012_09_25.ReadPipeline where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data ReadPipeline = ReadPipeline
    { _rptId :: Text
      -- ^ The identifier of the pipeline to read.
    } deriving (Show, Generic)

makeLenses ''ReadPipeline

instance ToPath ReadPipeline where
    toPath ReadPipeline{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toBS _rptId
        ]

instance ToQuery ReadPipeline

instance ToHeaders ReadPipeline

instance ToJSON ReadPipeline

data ReadPipelineResponse = ReadPipelineResponse
    { _rpuPipeline :: Maybe Pipeline
      -- ^ A section of the response body that provides information about
      -- the pipeline.
    } deriving (Show, Generic)

makeLenses ''ReadPipelineResponse

instance FromJSON ReadPipelineResponse

instance AWSRequest ReadPipeline where
    type Sv ReadPipeline = ElasticTranscoder
    type Rs ReadPipeline = ReadPipelineResponse

    request = get
    response _ = jsonResponse

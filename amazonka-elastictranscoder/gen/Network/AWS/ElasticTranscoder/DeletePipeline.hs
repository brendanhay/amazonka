{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.DeletePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeletePipeline operation removes a pipeline. You can only delete a
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
module Network.AWS.ElasticTranscoder.DeletePipeline
    (
    -- * Request
      DeletePipeline
    -- ** Request constructor
    , deletePipeline
    -- ** Request lenses
    , dpId

    -- * Response
    , DeletePipelineResponse
    -- ** Response constructor
    , deletePipelineResponse
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The DeletePipelineRequest structure.
newtype DeletePipeline = DeletePipeline
    { _dpId :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePipeline' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
deletePipeline :: Text -- ^ 'dpId'
               -> DeletePipeline
deletePipeline p1 = DeletePipeline
    { _dpId = p1
    }

-- | The identifier of the pipeline that you want to delete.
dpId :: Lens' DeletePipeline Text
dpId = lens _dpId (\s a -> s { _dpId = a })

instance ToPath DeletePipeline

instance ToQuery DeletePipeline

instance ToHeaders DeletePipeline

instance ToJSON DeletePipeline

-- | The DeletePipelineResponse structure.
data DeletePipelineResponse = DeletePipelineResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePipelineResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deletePipelineResponse :: DeletePipelineResponse
deletePipelineResponse = DeletePipelineResponse

instance AWSRequest DeletePipeline where
    type Sv DeletePipeline = ElasticTranscoder
    type Rs DeletePipeline = DeletePipelineResponse

    request = get
    response _ = nullaryResponse DeletePipelineResponse

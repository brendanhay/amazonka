{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Validates a pipeline and initiates processing. If the pipeline does not
-- pass validation, activation fails. Call this action to start processing
-- pipeline tasks of a pipeline you've created using the CreatePipeline and
-- PutPipelineDefinition actions. A pipeline cannot be modified after it has
-- been successfully activated. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.ActivatePipeline
-- Content-Length: 39 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} HTTP/1.1 200 x-amzn-RequestId:
-- ee19d5bf-074e-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 2 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {}.
module Network.AWS.DataPipeline
    (
    -- * Request
      ActivatePipeline
    -- ** Request constructor
    , mkActivatePipeline
    -- ** Request lenses
    , apPipelineId

    -- * Response
    , ActivatePipelineResponse
    -- ** Response constructor
    , mkActivatePipelineResponse
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input of the ActivatePipeline action.
newtype ActivatePipeline = ActivatePipeline
    { _apPipelineId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ActivatePipeline' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
mkActivatePipeline :: Text -- ^ 'apPipelineId'
                   -> ActivatePipeline
mkActivatePipeline p1 = ActivatePipeline
    { _apPipelineId = p1
    }

-- | The identifier of the pipeline to activate.
apPipelineId :: Lens' ActivatePipeline Text
apPipelineId = lens _apPipelineId (\s a -> s { _apPipelineId = a })

instance ToPath ActivatePipeline

instance ToQuery ActivatePipeline

instance ToHeaders ActivatePipeline

instance ToJSON ActivatePipeline

-- | Contains the output from the ActivatePipeline action.
data ActivatePipelineResponse = ActivatePipelineResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ActivatePipelineResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkActivatePipelineResponse :: ActivatePipelineResponse
mkActivatePipelineResponse = ActivatePipelineResponse

instance AWSRequest ActivatePipeline where
    type Sv ActivatePipeline = DataPipeline
    type Rs ActivatePipeline = ActivatePipelineResponse

    request = get
    response _ = nullaryResponse ActivatePipelineResponse

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.ActivatePipeline
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
module Network.AWS.DataPipeline.V2012_10_29.ActivatePipeline where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude

data ActivatePipeline = ActivatePipeline
    { _apiPipelineId :: Text
      -- ^ The identifier of the pipeline to activate.
    } deriving (Generic)

makeLenses ''ActivatePipeline

instance ToPath ActivatePipeline

instance ToQuery ActivatePipeline

instance ToHeaders ActivatePipeline

instance ToJSON ActivatePipeline

data ActivatePipelineResponse = ActivatePipelineResponse
    deriving (Eq, Show, Generic)

makeLenses ''ActivatePipelineResponse

instance AWSRequest ActivatePipeline where
    type Sv ActivatePipeline = DataPipeline
    type Rs ActivatePipeline = ActivatePipelineResponse

    request = get
    response _ _ = return (Right ActivatePipelineResponse)

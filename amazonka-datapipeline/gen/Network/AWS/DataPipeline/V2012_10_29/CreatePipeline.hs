{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.CreatePipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new empty pipeline. When this action succeeds, you can then use
-- the PutPipelineDefinition action to populate the pipeline. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.CreatePipeline Content-Length: 91 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"name": "myPipeline", "uniqueId":
-- "123456789", "description": "This is my first pipeline"} HTTP/1.1 200
-- x-amzn-RequestId: b16911ce-0774-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 40 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineId": "df-06372391ZG65EXAMPLE"}.
module Network.AWS.DataPipeline.V2012_10_29.CreatePipeline where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'CreatePipeline' request.
createPipeline :: Text -- ^ '_cpiUniqueId'
               -> Text -- ^ '_cpiName'
               -> CreatePipeline
createPipeline p1 p2 = CreatePipeline
    { _cpiUniqueId = p1
    , _cpiName = p2
    , _cpiDescription = Nothing
    }

data CreatePipeline = CreatePipeline
    { _cpiUniqueId :: Text
      -- ^ A unique identifier that you specify. This identifier is not the
      -- same as the pipeline identifier assigned by AWS Data Pipeline.
      -- You are responsible for defining the format and ensuring the
      -- uniqueness of this identifier. You use this parameter to ensure
      -- idempotency during repeated calls to CreatePipeline. For example,
      -- if the first call to CreatePipeline does not return a clear
      -- success, you can pass in the same unique identifier and pipeline
      -- name combination on a subsequent call to CreatePipeline.
      -- CreatePipeline ensures that if a pipeline already exists with the
      -- same name and unique identifier, a new pipeline will not be
      -- created. Instead, you'll receive the pipeline identifier from the
      -- previous attempt. The uniqueness of the name and unique
      -- identifier combination is scoped to the AWS account or IAM user
      -- credentials.
    , _cpiName :: Text
      -- ^ The name of the new pipeline. You can use the same name for
      -- multiple pipelines associated with your AWS account, because AWS
      -- Data Pipeline assigns each new pipeline a unique pipeline
      -- identifier.
    , _cpiDescription :: Maybe Text
      -- ^ The description of the new pipeline.
    } deriving (Show, Generic)

makeLenses ''CreatePipeline

instance ToPath CreatePipeline

instance ToQuery CreatePipeline

instance ToHeaders CreatePipeline

instance ToJSON CreatePipeline

data CreatePipelineResponse = CreatePipelineResponse
    { _cpoPipelineId :: Text
      -- ^ The ID that AWS Data Pipeline assigns the newly created pipeline.
      -- The ID is a string of the form: df-06372391ZG65EXAMPLE.
    } deriving (Show, Generic)

makeLenses ''CreatePipelineResponse

instance FromJSON CreatePipelineResponse

instance AWSRequest CreatePipeline where
    type Sv CreatePipeline = DataPipeline
    type Rs CreatePipeline = CreatePipelineResponse

    request = get
    response _ = jsonResponse

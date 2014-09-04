{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.GetPipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the definition of the specified pipeline. You can call
-- GetPipelineDefinition to retrieve the pipeline definition you provided
-- using PutPipelineDefinition. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.GetPipelineDefinition
-- Content-Length: 40 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- e28309e5-0776-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 890 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineObjects": [ {"fields": [ {"key": "workerGroup",
-- "stringValue": "workerGroup"} ], "id": "Default", "name": "Default"},
-- {"fields": [ {"key": "startDateTime", "stringValue":
-- "2012-09-25T17:00:00"}, {"key": "type", "stringValue": "Schedule"}, {"key":
-- "period", "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-09-25T18:00:00"} ], "id": "Schedule", "name": "Schedule"}, {"fields":
-- [ {"key": "schedule", "refValue": "Schedule"}, {"key": "command",
-- "stringValue": "echo hello"}, {"key": "parent", "refValue": "Default"},
-- {"key": "type", "stringValue": "ShellCommandActivity"} ], "id": "SayHello",
-- "name": "SayHello"} ] }.
module Network.AWS.DataPipeline.V2012_10_29.GetPipelineDefinition
    (
    -- * Request
      GetPipelineDefinition
    -- ** Request constructor
    , mkGetPipelineDefinitionInput
    -- ** Request lenses
    , gpdiPipelineId
    , gpdiVersion

    -- * Response
    , GetPipelineDefinitionResponse
    -- ** Response lenses
    , gpdoPipelineObjects
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetPipelineDefinition' request.
mkGetPipelineDefinitionInput :: Text -- ^ 'gpdiPipelineId'
                             -> GetPipelineDefinition
mkGetPipelineDefinitionInput p1 = GetPipelineDefinition
    { _gpdiPipelineId = p1
    , _gpdiVersion = Nothing
    }
{-# INLINE mkGetPipelineDefinitionInput #-}

data GetPipelineDefinition = GetPipelineDefinition
    { _gpdiPipelineId :: Text
      -- ^ The identifier of the pipeline.
    , _gpdiVersion :: Maybe Text
      -- ^ The version of the pipeline definition to retrieve. This
      -- parameter accepts the values latest (default) and active. Where
      -- latest indicates the last definition saved to the pipeline and
      -- active indicates the last definition of the pipeline that was
      -- activated.
    } deriving (Show, Generic)

-- | The identifier of the pipeline.
gpdiPipelineId :: Lens' GetPipelineDefinition (Text)
gpdiPipelineId = lens _gpdiPipelineId (\s a -> s { _gpdiPipelineId = a })
{-# INLINE gpdiPipelineId #-}

-- | The version of the pipeline definition to retrieve. This parameter accepts
-- the values latest (default) and active. Where latest indicates the last
-- definition saved to the pipeline and active indicates the last definition
-- of the pipeline that was activated.
gpdiVersion :: Lens' GetPipelineDefinition (Maybe Text)
gpdiVersion = lens _gpdiVersion (\s a -> s { _gpdiVersion = a })
{-# INLINE gpdiVersion #-}

instance ToPath GetPipelineDefinition

instance ToQuery GetPipelineDefinition

instance ToHeaders GetPipelineDefinition

instance ToJSON GetPipelineDefinition

newtype GetPipelineDefinitionResponse = GetPipelineDefinitionResponse
    { _gpdoPipelineObjects :: [PipelineObject]
      -- ^ An array of objects defined in the pipeline.
    } deriving (Show, Generic)

-- | An array of objects defined in the pipeline.
gpdoPipelineObjects :: Lens' GetPipelineDefinitionResponse ([PipelineObject])
gpdoPipelineObjects = lens _gpdoPipelineObjects (\s a -> s { _gpdoPipelineObjects = a })
{-# INLINE gpdoPipelineObjects #-}

instance FromJSON GetPipelineDefinitionResponse

instance AWSRequest GetPipelineDefinition where
    type Sv GetPipelineDefinition = DataPipeline
    type Rs GetPipelineDefinition = GetPipelineDefinitionResponse

    request = get
    response _ = jsonResponse

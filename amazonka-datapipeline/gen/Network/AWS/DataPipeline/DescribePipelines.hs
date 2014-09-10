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

-- | Retrieve metadata about one or more pipelines. The information retrieved
-- includes the name of the pipeline, the pipeline identifier, its current
-- state, and the user account that owns the pipeline. Using account
-- credentials, you can retrieve metadata about pipelines that you or your IAM
-- users have created. If you are using an IAM user account, you can retrieve
-- metadata about only those pipelines you have read permission for. To
-- retrieve the full pipeline definition instead of metadata about the
-- pipeline, call the GetPipelineDefinition action. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.DescribePipelines Content-Length: 70 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineIds": ["df-08785951KAKJEXAMPLE"] }
-- x-amzn-RequestId: 02870eb7-0736-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 767 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineDescriptionList": [ {"description": "This is my
-- first pipeline", "fields": [ {"key": "@pipelineState", "stringValue":
-- "SCHEDULED"}, {"key": "description", "stringValue": "This is my first
-- pipeline"}, {"key": "name", "stringValue": "myPipeline"}, {"key":
-- "@creationTime", "stringValue": "2012-12-13T01:24:06"}, {"key": "@id",
-- "stringValue": "df-0937003356ZJEXAMPLE"}, {"key": "@sphere", "stringValue":
-- "PIPELINE"}, {"key": "@version", "stringValue": "1"}, {"key": "@userId",
-- "stringValue": "924374875933"}, {"key": "@accountId", "stringValue":
-- "924374875933"}, {"key": "uniqueId", "stringValue": "1234567890"} ],
-- "name": "myPipeline", "pipelineId": "df-0937003356ZJEXAMPLE"} ] }.
module Network.AWS.DataPipeline
    (
    -- * Request
      DescribePipelines
    -- ** Request constructor
    , mkDescribePipelines
    -- ** Request lenses
    , dp1PipelineIds

    -- * Response
    , DescribePipelinesResponse
    -- ** Response constructor
    , mkDescribePipelinesResponse
    -- ** Response lenses
    , dprPipelineDescriptionList
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input to the DescribePipelines action.
newtype DescribePipelines = DescribePipelines
    { _dp1PipelineIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePipelines' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineIds ::@ @[Text]@
--
mkDescribePipelines :: [Text] -- ^ 'dp1PipelineIds'
                    -> DescribePipelines
mkDescribePipelines p1 = DescribePipelines
    { _dp1PipelineIds = p1
    }

-- | Identifiers of the pipelines to describe. You can pass as many as 25
-- identifiers in a single call to DescribePipelines. You can obtain pipeline
-- identifiers by calling ListPipelines.
dp1PipelineIds :: Lens' DescribePipelines [Text]
dp1PipelineIds = lens _dp1PipelineIds (\s a -> s { _dp1PipelineIds = a })

instance ToPath DescribePipelines

instance ToQuery DescribePipelines

instance ToHeaders DescribePipelines

instance ToJSON DescribePipelines

-- | Contains the output from the DescribePipelines action.
newtype DescribePipelinesResponse = DescribePipelinesResponse
    { _dprPipelineDescriptionList :: [PipelineDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribePipelinesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineDescriptionList ::@ @[PipelineDescription]@
--
mkDescribePipelinesResponse :: [PipelineDescription] -- ^ 'dprPipelineDescriptionList'
                            -> DescribePipelinesResponse
mkDescribePipelinesResponse p1 = DescribePipelinesResponse
    { _dprPipelineDescriptionList = p1
    }

-- | An array of descriptions returned for the specified pipelines.
dprPipelineDescriptionList :: Lens' DescribePipelinesResponse [PipelineDescription]
dprPipelineDescriptionList =
    lens _dprPipelineDescriptionList
         (\s a -> s { _dprPipelineDescriptionList = a })

instance FromJSON DescribePipelinesResponse

instance AWSRequest DescribePipelines where
    type Sv DescribePipelines = DataPipeline
    type Rs DescribePipelines = DescribePipelinesResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Evaluates a string in the context of a specified object. A task runner can
-- use this action to evaluate SQL queries stored in Amazon S3. POST /
-- HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.DescribePipelines Content-Length: 164 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-08785951KAKJEXAMPLE",
-- "objectId": "Schedule", "expression": "Transform started at
-- #{startDateTime} and finished at #{endDateTime}"} x-amzn-RequestId:
-- 02870eb7-0736-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 103 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"evaluatedExpression": "Transform started at
-- 2012-12-12T00:00:00 and finished at 2012-12-21T18:00:00"}.
module Network.AWS.DataPipeline.EvaluateExpression
    (
    -- * Request
      EvaluateExpression
    -- ** Request constructor
    , evaluateExpression
    -- ** Request lenses
    , eePipelineId
    , eeObjectId
    , eeExpression

    -- * Response
    , EvaluateExpressionResponse
    -- ** Response constructor
    , evaluateExpressionResponse
    -- ** Response lenses
    , eerEvaluatedExpression
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input for the EvaluateExpression action.
data EvaluateExpression = EvaluateExpression
    { _eePipelineId :: Text
    , _eeObjectId :: Text
    , _eeExpression :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EvaluateExpression' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
-- * @ObjectId ::@ @Text@
--
-- * @Expression ::@ @Text@
--
evaluateExpression :: Text -- ^ 'eePipelineId'
                   -> Text -- ^ 'eeObjectId'
                   -> Text -- ^ 'eeExpression'
                   -> EvaluateExpression
evaluateExpression p1 p2 p3 = EvaluateExpression
    { _eePipelineId = p1
    , _eeObjectId = p2
    , _eeExpression = p3
    }

-- | The identifier of the pipeline.
eePipelineId :: Lens' EvaluateExpression Text
eePipelineId = lens _eePipelineId (\s a -> s { _eePipelineId = a })

-- | The identifier of the object.
eeObjectId :: Lens' EvaluateExpression Text
eeObjectId = lens _eeObjectId (\s a -> s { _eeObjectId = a })

-- | The expression to evaluate.
eeExpression :: Lens' EvaluateExpression Text
eeExpression = lens _eeExpression (\s a -> s { _eeExpression = a })

instance ToPath EvaluateExpression

instance ToQuery EvaluateExpression

instance ToHeaders EvaluateExpression

instance ToJSON EvaluateExpression

-- | Contains the output from the EvaluateExpression action.
newtype EvaluateExpressionResponse = EvaluateExpressionResponse
    { _eerEvaluatedExpression :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EvaluateExpressionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EvaluatedExpression ::@ @Text@
--
evaluateExpressionResponse :: Text -- ^ 'eerEvaluatedExpression'
                           -> EvaluateExpressionResponse
evaluateExpressionResponse p1 = EvaluateExpressionResponse
    { _eerEvaluatedExpression = p1
    }

-- | The evaluated expression.
eerEvaluatedExpression :: Lens' EvaluateExpressionResponse Text
eerEvaluatedExpression =
    lens _eerEvaluatedExpression (\s a -> s { _eerEvaluatedExpression = a })

instance FromJSON EvaluateExpressionResponse

instance AWSRequest EvaluateExpression where
    type Sv EvaluateExpression = DataPipeline
    type Rs EvaluateExpression = EvaluateExpressionResponse

    request = get
    response _ = jsonResponse

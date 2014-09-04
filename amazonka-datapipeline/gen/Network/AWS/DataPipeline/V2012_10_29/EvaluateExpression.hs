{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.EvaluateExpression
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
module Network.AWS.DataPipeline.V2012_10_29.EvaluateExpression
    (
    -- * Request
      EvaluateExpression
    -- ** Request constructor
    , evaluateExpression
    -- ** Request lenses
    , eeiPipelineId
    , eeiObjectId
    , eeiExpression

    -- * Response
    , EvaluateExpressionResponse
    -- ** Response lenses
    , eeoEvaluatedExpression
    ) where

import           Network.AWS.DataPipeline.V2012_10_29.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'EvaluateExpression' request.
evaluateExpression :: Text -- ^ 'eeiPipelineId'
                   -> Text -- ^ 'eeiObjectId'
                   -> Text -- ^ 'eeiExpression'
                   -> EvaluateExpression
evaluateExpression p1 p2 p3 = EvaluateExpression
    { _eeiPipelineId = p1
    , _eeiObjectId = p2
    , _eeiExpression = p3
    }
{-# INLINE evaluateExpression #-}

data EvaluateExpression = EvaluateExpression
    { _eeiPipelineId :: Text
      -- ^ The identifier of the pipeline.
    , _eeiObjectId :: Text
      -- ^ The identifier of the object.
    , _eeiExpression :: Text
      -- ^ The expression to evaluate.
    } deriving (Show, Generic)

-- | The identifier of the pipeline.
eeiPipelineId :: Lens' EvaluateExpression (Text)
eeiPipelineId f x =
    f (_eeiPipelineId x)
        <&> \y -> x { _eeiPipelineId = y }
{-# INLINE eeiPipelineId #-}

-- | The identifier of the object.
eeiObjectId :: Lens' EvaluateExpression (Text)
eeiObjectId f x =
    f (_eeiObjectId x)
        <&> \y -> x { _eeiObjectId = y }
{-# INLINE eeiObjectId #-}

-- | The expression to evaluate.
eeiExpression :: Lens' EvaluateExpression (Text)
eeiExpression f x =
    f (_eeiExpression x)
        <&> \y -> x { _eeiExpression = y }
{-# INLINE eeiExpression #-}

instance ToPath EvaluateExpression

instance ToQuery EvaluateExpression

instance ToHeaders EvaluateExpression

instance ToJSON EvaluateExpression

data EvaluateExpressionResponse = EvaluateExpressionResponse
    { _eeoEvaluatedExpression :: Text
      -- ^ The evaluated expression.
    } deriving (Show, Generic)

-- | The evaluated expression.
eeoEvaluatedExpression :: Lens' EvaluateExpressionResponse (Text)
eeoEvaluatedExpression f x =
    f (_eeoEvaluatedExpression x)
        <&> \y -> x { _eeoEvaluatedExpression = y }
{-# INLINE eeoEvaluatedExpression #-}

instance FromJSON EvaluateExpressionResponse

instance AWSRequest EvaluateExpression where
    type Sv EvaluateExpression = DataPipeline
    type Rs EvaluateExpression = EvaluateExpressionResponse

    request = get
    response _ = jsonResponse

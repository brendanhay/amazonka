{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.DataPipeline.V2012_10_29.EvaluateExpression where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.V2012_10_29.Types
import Network.AWS.Prelude

data EvaluateExpression = EvaluateExpression
    { _eeiPipelineId :: Text
      -- ^ The identifier of the pipeline.
    , _eeiObjectId :: Text
      -- ^ The identifier of the object.
    , _eeiExpression :: Text
      -- ^ The expression to evaluate.
    } deriving (Show, Generic)

makeLenses ''EvaluateExpression

instance ToPath EvaluateExpression

instance ToQuery EvaluateExpression

instance ToHeaders EvaluateExpression

instance ToJSON EvaluateExpression

data EvaluateExpressionResponse = EvaluateExpressionResponse
    { _eeoEvaluatedExpression :: Text
      -- ^ The evaluated expression.
    } deriving (Show, Generic)

makeLenses ''EvaluateExpressionResponse

instance FromJSON EvaluateExpressionResponse

instance AWSRequest EvaluateExpression where
    type Sv EvaluateExpression = DataPipeline
    type Rs EvaluateExpression = EvaluateExpressionResponse

    request = get
    response _ = jsonResponse

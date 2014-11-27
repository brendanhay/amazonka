{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Evaluates a string in the context of a specified object. A task runner can
-- use this action to evaluate SQL queries stored in Amazon S3.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_EvaluateExpression.html>
module Network.AWS.DataPipeline.EvaluateExpression
    (
    -- * Request
      EvaluateExpression
    -- ** Request constructor
    , evaluateExpression
    -- ** Request lenses
    , eeExpression
    , eeObjectId
    , eePipelineId

    -- * Response
    , EvaluateExpressionResponse
    -- ** Response constructor
    , evaluateExpressionResponse
    -- ** Response lenses
    , eerEvaluatedExpression
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data EvaluateExpression = EvaluateExpression
    { _eeExpression :: Text
    , _eeObjectId   :: Text
    , _eePipelineId :: Text
    } deriving (Eq, Ord, Show)

-- | 'EvaluateExpression' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eeExpression' @::@ 'Text'
--
-- * 'eeObjectId' @::@ 'Text'
--
-- * 'eePipelineId' @::@ 'Text'
--
evaluateExpression :: Text -- ^ 'eePipelineId'
                   -> Text -- ^ 'eeObjectId'
                   -> Text -- ^ 'eeExpression'
                   -> EvaluateExpression
evaluateExpression p1 p2 p3 = EvaluateExpression
    { _eePipelineId = p1
    , _eeObjectId   = p2
    , _eeExpression = p3
    }

-- | The expression to evaluate.
eeExpression :: Lens' EvaluateExpression Text
eeExpression = lens _eeExpression (\s a -> s { _eeExpression = a })

-- | The identifier of the object.
eeObjectId :: Lens' EvaluateExpression Text
eeObjectId = lens _eeObjectId (\s a -> s { _eeObjectId = a })

-- | The identifier of the pipeline.
eePipelineId :: Lens' EvaluateExpression Text
eePipelineId = lens _eePipelineId (\s a -> s { _eePipelineId = a })

newtype EvaluateExpressionResponse = EvaluateExpressionResponse
    { _eerEvaluatedExpression :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'EvaluateExpressionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eerEvaluatedExpression' @::@ 'Text'
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

instance ToPath EvaluateExpression where
    toPath = const "/"

instance ToQuery EvaluateExpression where
    toQuery = const mempty

instance ToHeaders EvaluateExpression

instance ToJSON EvaluateExpression where
    toJSON EvaluateExpression{..} = object
        [ "pipelineId" .= _eePipelineId
        , "objectId"   .= _eeObjectId
        , "expression" .= _eeExpression
        ]

instance AWSRequest EvaluateExpression where
    type Sv EvaluateExpression = DataPipeline
    type Rs EvaluateExpression = EvaluateExpressionResponse

    request  = post "EvaluateExpression"
    response = jsonResponse

instance FromJSON EvaluateExpressionResponse where
    parseJSON = withObject "EvaluateExpressionResponse" $ \o -> EvaluateExpressionResponse
        <$> o .:  "evaluatedExpression"

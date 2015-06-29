{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Task runners call @EvaluateExpression@ to evaluate a string in the
-- context of the specified object. For example, a task runner can evaluate
-- SQL queries stored in Amazon S3.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_EvaluateExpression.html>
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
    , eerStatus
    , eerEvaluatedExpression
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for EvaluateExpression.
--
-- /See:/ 'evaluateExpression' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eePipelineId'
--
-- * 'eeObjectId'
--
-- * 'eeExpression'
data EvaluateExpression = EvaluateExpression'
    { _eePipelineId :: !Text
    , _eeObjectId   :: !Text
    , _eeExpression :: !Text
    } deriving (Eq,Read,Show)

-- | 'EvaluateExpression' smart constructor.
evaluateExpression :: Text -> Text -> Text -> EvaluateExpression
evaluateExpression pPipelineId pObjectId pExpression =
    EvaluateExpression'
    { _eePipelineId = pPipelineId
    , _eeObjectId = pObjectId
    , _eeExpression = pExpression
    }

-- | The ID of the pipeline.
eePipelineId :: Lens' EvaluateExpression Text
eePipelineId = lens _eePipelineId (\ s a -> s{_eePipelineId = a});

-- | The ID of the object.
eeObjectId :: Lens' EvaluateExpression Text
eeObjectId = lens _eeObjectId (\ s a -> s{_eeObjectId = a});

-- | The expression to evaluate.
eeExpression :: Lens' EvaluateExpression Text
eeExpression = lens _eeExpression (\ s a -> s{_eeExpression = a});

instance AWSRequest EvaluateExpression where
        type Sv EvaluateExpression = DataPipeline
        type Rs EvaluateExpression =
             EvaluateExpressionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EvaluateExpressionResponse' <$>
                   (pure s) <*> (x .:> "evaluatedExpression"))

instance ToHeaders EvaluateExpression where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.EvaluateExpression" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EvaluateExpression where
        toJSON EvaluateExpression'{..}
          = object
              ["pipelineId" .= _eePipelineId,
               "objectId" .= _eeObjectId,
               "expression" .= _eeExpression]

instance ToPath EvaluateExpression where
        toPath = const "/"

instance ToQuery EvaluateExpression where
        toQuery = const mempty

-- | Contains the output of EvaluateExpression.
--
-- /See:/ 'evaluateExpressionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eerStatus'
--
-- * 'eerEvaluatedExpression'
data EvaluateExpressionResponse = EvaluateExpressionResponse'
    { _eerStatus              :: !Status
    , _eerEvaluatedExpression :: !Text
    } deriving (Eq,Show)

-- | 'EvaluateExpressionResponse' smart constructor.
evaluateExpressionResponse :: Status -> Text -> EvaluateExpressionResponse
evaluateExpressionResponse pStatus pEvaluatedExpression =
    EvaluateExpressionResponse'
    { _eerStatus = pStatus
    , _eerEvaluatedExpression = pEvaluatedExpression
    }

-- | FIXME: Undocumented member.
eerStatus :: Lens' EvaluateExpressionResponse Status
eerStatus = lens _eerStatus (\ s a -> s{_eerStatus = a});

-- | The evaluated expression.
eerEvaluatedExpression :: Lens' EvaluateExpressionResponse Text
eerEvaluatedExpression = lens _eerEvaluatedExpression (\ s a -> s{_eerEvaluatedExpression = a});

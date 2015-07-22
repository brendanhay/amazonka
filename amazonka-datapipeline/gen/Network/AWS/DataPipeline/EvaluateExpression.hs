{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @EvaluateExpression@ to evaluate a string in the
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
    , eerqPipelineId
    , eerqObjectId
    , eerqExpression

    -- * Response
    , EvaluateExpressionResponse
    -- ** Response constructor
    , evaluateExpressionResponse
    -- ** Response lenses
    , eersStatus
    , eersEvaluatedExpression
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
-- * 'eerqPipelineId'
--
-- * 'eerqObjectId'
--
-- * 'eerqExpression'
data EvaluateExpression = EvaluateExpression'
    { _eerqPipelineId :: !Text
    , _eerqObjectId   :: !Text
    , _eerqExpression :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EvaluateExpression' smart constructor.
evaluateExpression :: Text -> Text -> Text -> EvaluateExpression
evaluateExpression pPipelineId pObjectId pExpression =
    EvaluateExpression'
    { _eerqPipelineId = pPipelineId
    , _eerqObjectId = pObjectId
    , _eerqExpression = pExpression
    }

-- | The ID of the pipeline.
eerqPipelineId :: Lens' EvaluateExpression Text
eerqPipelineId = lens _eerqPipelineId (\ s a -> s{_eerqPipelineId = a});

-- | The ID of the object.
eerqObjectId :: Lens' EvaluateExpression Text
eerqObjectId = lens _eerqObjectId (\ s a -> s{_eerqObjectId = a});

-- | The expression to evaluate.
eerqExpression :: Lens' EvaluateExpression Text
eerqExpression = lens _eerqExpression (\ s a -> s{_eerqExpression = a});

instance AWSRequest EvaluateExpression where
        type Sv EvaluateExpression = DataPipeline
        type Rs EvaluateExpression =
             EvaluateExpressionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EvaluateExpressionResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "evaluatedExpression"))

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
              ["pipelineId" .= _eerqPipelineId,
               "objectId" .= _eerqObjectId,
               "expression" .= _eerqExpression]

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
-- * 'eersStatus'
--
-- * 'eersEvaluatedExpression'
data EvaluateExpressionResponse = EvaluateExpressionResponse'
    { _eersStatus              :: !Int
    , _eersEvaluatedExpression :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EvaluateExpressionResponse' smart constructor.
evaluateExpressionResponse :: Int -> Text -> EvaluateExpressionResponse
evaluateExpressionResponse pStatus pEvaluatedExpression =
    EvaluateExpressionResponse'
    { _eersStatus = pStatus
    , _eersEvaluatedExpression = pEvaluatedExpression
    }

-- | FIXME: Undocumented member.
eersStatus :: Lens' EvaluateExpressionResponse Int
eersStatus = lens _eersStatus (\ s a -> s{_eersStatus = a});

-- | The evaluated expression.
eersEvaluatedExpression :: Lens' EvaluateExpressionResponse Text
eersEvaluatedExpression = lens _eersEvaluatedExpression (\ s a -> s{_eersEvaluatedExpression = a});

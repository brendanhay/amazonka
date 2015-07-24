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
    , eePipelineId
    , eeObjectId
    , eeExpression

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
-- * 'eePipelineId'
--
-- * 'eeObjectId'
--
-- * 'eeExpression'
data EvaluateExpression = EvaluateExpression'
    { _eePipelineId :: !Text
    , _eeObjectId   :: !Text
    , _eeExpression :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EvaluateExpression' smart constructor.
evaluateExpression :: Text -> Text -> Text -> EvaluateExpression
evaluateExpression pPipelineId_ pObjectId_ pExpression_ =
    EvaluateExpression'
    { _eePipelineId = pPipelineId_
    , _eeObjectId = pObjectId_
    , _eeExpression = pExpression_
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
        request = postJSON "EvaluateExpression"
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
-- * 'eersStatus'
--
-- * 'eersEvaluatedExpression'
data EvaluateExpressionResponse = EvaluateExpressionResponse'
    { _eersStatus              :: !Int
    , _eersEvaluatedExpression :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EvaluateExpressionResponse' smart constructor.
evaluateExpressionResponse :: Int -> Text -> EvaluateExpressionResponse
evaluateExpressionResponse pStatus_ pEvaluatedExpression_ =
    EvaluateExpressionResponse'
    { _eersStatus = pStatus_
    , _eersEvaluatedExpression = pEvaluatedExpression_
    }

-- | FIXME: Undocumented member.
eersStatus :: Lens' EvaluateExpressionResponse Int
eersStatus = lens _eersStatus (\ s a -> s{_eersStatus = a});

-- | The evaluated expression.
eersEvaluatedExpression :: Lens' EvaluateExpressionResponse Text
eersEvaluatedExpression = lens _eersEvaluatedExpression (\ s a -> s{_eersEvaluatedExpression = a});

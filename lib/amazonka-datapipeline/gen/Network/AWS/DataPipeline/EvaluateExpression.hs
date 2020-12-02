{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @EvaluateExpression@ to evaluate a string in the context of the specified object. For example, a task runner can evaluate SQL queries stored in Amazon S3.
--
--
module Network.AWS.DataPipeline.EvaluateExpression
    (
    -- * Creating a Request
      evaluateExpression
    , EvaluateExpression
    -- * Request Lenses
    , eePipelineId
    , eeObjectId
    , eeExpression

    -- * Destructuring the Response
    , evaluateExpressionResponse
    , EvaluateExpressionResponse
    -- * Response Lenses
    , eersResponseStatus
    , eersEvaluatedExpression
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for EvaluateExpression.
--
--
--
-- /See:/ 'evaluateExpression' smart constructor.
data EvaluateExpression = EvaluateExpression'
  { _eePipelineId :: !Text
  , _eeObjectId   :: !Text
  , _eeExpression :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EvaluateExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eePipelineId' - The ID of the pipeline.
--
-- * 'eeObjectId' - The ID of the object.
--
-- * 'eeExpression' - The expression to evaluate.
evaluateExpression
    :: Text -- ^ 'eePipelineId'
    -> Text -- ^ 'eeObjectId'
    -> Text -- ^ 'eeExpression'
    -> EvaluateExpression
evaluateExpression pPipelineId_ pObjectId_ pExpression_ =
  EvaluateExpression'
    { _eePipelineId = pPipelineId_
    , _eeObjectId = pObjectId_
    , _eeExpression = pExpression_
    }


-- | The ID of the pipeline.
eePipelineId :: Lens' EvaluateExpression Text
eePipelineId = lens _eePipelineId (\ s a -> s{_eePipelineId = a})

-- | The ID of the object.
eeObjectId :: Lens' EvaluateExpression Text
eeObjectId = lens _eeObjectId (\ s a -> s{_eeObjectId = a})

-- | The expression to evaluate.
eeExpression :: Lens' EvaluateExpression Text
eeExpression = lens _eeExpression (\ s a -> s{_eeExpression = a})

instance AWSRequest EvaluateExpression where
        type Rs EvaluateExpression =
             EvaluateExpressionResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 EvaluateExpressionResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "evaluatedExpression"))

instance Hashable EvaluateExpression where

instance NFData EvaluateExpression where

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
              (catMaybes
                 [Just ("pipelineId" .= _eePipelineId),
                  Just ("objectId" .= _eeObjectId),
                  Just ("expression" .= _eeExpression)])

instance ToPath EvaluateExpression where
        toPath = const "/"

instance ToQuery EvaluateExpression where
        toQuery = const mempty

-- | Contains the output of EvaluateExpression.
--
--
--
-- /See:/ 'evaluateExpressionResponse' smart constructor.
data EvaluateExpressionResponse = EvaluateExpressionResponse'
  { _eersResponseStatus      :: !Int
  , _eersEvaluatedExpression :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EvaluateExpressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eersResponseStatus' - -- | The response status code.
--
-- * 'eersEvaluatedExpression' - The evaluated expression.
evaluateExpressionResponse
    :: Int -- ^ 'eersResponseStatus'
    -> Text -- ^ 'eersEvaluatedExpression'
    -> EvaluateExpressionResponse
evaluateExpressionResponse pResponseStatus_ pEvaluatedExpression_ =
  EvaluateExpressionResponse'
    { _eersResponseStatus = pResponseStatus_
    , _eersEvaluatedExpression = pEvaluatedExpression_
    }


-- | -- | The response status code.
eersResponseStatus :: Lens' EvaluateExpressionResponse Int
eersResponseStatus = lens _eersResponseStatus (\ s a -> s{_eersResponseStatus = a})

-- | The evaluated expression.
eersEvaluatedExpression :: Lens' EvaluateExpressionResponse Text
eersEvaluatedExpression = lens _eersEvaluatedExpression (\ s a -> s{_eersEvaluatedExpression = a})

instance NFData EvaluateExpressionResponse where

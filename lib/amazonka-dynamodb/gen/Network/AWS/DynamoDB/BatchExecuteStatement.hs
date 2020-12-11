{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchExecuteStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows you to perform batch reads and writes on data stored in DynamoDB, using PartiQL.
module Network.AWS.DynamoDB.BatchExecuteStatement
  ( -- * Creating a request
    BatchExecuteStatement (..),
    mkBatchExecuteStatement,

    -- ** Request lenses
    besStatements,

    -- * Destructuring the response
    BatchExecuteStatementResponse (..),
    mkBatchExecuteStatementResponse,

    -- ** Response lenses
    besrsResponses,
    besrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchExecuteStatement' smart constructor.
newtype BatchExecuteStatement = BatchExecuteStatement'
  { statements ::
      Lude.NonEmpty BatchStatementRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchExecuteStatement' with the minimum fields required to make a request.
--
-- * 'statements' - The list of PartiQL statements representing the batch to run.
mkBatchExecuteStatement ::
  -- | 'statements'
  Lude.NonEmpty BatchStatementRequest ->
  BatchExecuteStatement
mkBatchExecuteStatement pStatements_ =
  BatchExecuteStatement' {statements = pStatements_}

-- | The list of PartiQL statements representing the batch to run.
--
-- /Note:/ Consider using 'statements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besStatements :: Lens.Lens' BatchExecuteStatement (Lude.NonEmpty BatchStatementRequest)
besStatements = Lens.lens (statements :: BatchExecuteStatement -> Lude.NonEmpty BatchStatementRequest) (\s a -> s {statements = a} :: BatchExecuteStatement)
{-# DEPRECATED besStatements "Use generic-lens or generic-optics with 'statements' instead." #-}

instance Lude.AWSRequest BatchExecuteStatement where
  type Rs BatchExecuteStatement = BatchExecuteStatementResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Lude.<$> (x Lude..?> "Responses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchExecuteStatement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.BatchExecuteStatement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Statements" Lude..= statements)])

instance Lude.ToPath BatchExecuteStatement where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchExecuteStatement where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { responses ::
      Lude.Maybe
        [BatchStatementResponse],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchExecuteStatementResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'responses' - The response to each PartiQL statement in the batch.
mkBatchExecuteStatementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchExecuteStatementResponse
mkBatchExecuteStatementResponse pResponseStatus_ =
  BatchExecuteStatementResponse'
    { responses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The response to each PartiQL statement in the batch.
--
-- /Note:/ Consider using 'responses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrsResponses :: Lens.Lens' BatchExecuteStatementResponse (Lude.Maybe [BatchStatementResponse])
besrsResponses = Lens.lens (responses :: BatchExecuteStatementResponse -> Lude.Maybe [BatchStatementResponse]) (\s a -> s {responses = a} :: BatchExecuteStatementResponse)
{-# DEPRECATED besrsResponses "Use generic-lens or generic-optics with 'responses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
besrsResponseStatus :: Lens.Lens' BatchExecuteStatementResponse Lude.Int
besrsResponseStatus = Lens.lens (responseStatus :: BatchExecuteStatementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchExecuteStatementResponse)
{-# DEPRECATED besrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

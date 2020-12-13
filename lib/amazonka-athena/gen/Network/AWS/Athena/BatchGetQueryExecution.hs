{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.BatchGetQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single query execution or a list of up to 50 query executions, which you provide as an array of query execution ID strings. Requires you to have access to the workgroup in which the queries ran. To get a list of query execution IDs, use 'ListQueryExecutionsInput$WorkGroup' . Query executions differ from named (saved) queries. Use 'BatchGetNamedQueryInput' to get details about named queries.
module Network.AWS.Athena.BatchGetQueryExecution
  ( -- * Creating a request
    BatchGetQueryExecution (..),
    mkBatchGetQueryExecution,

    -- ** Request lenses
    bgqeQueryExecutionIds,

    -- * Destructuring the response
    BatchGetQueryExecutionResponse (..),
    mkBatchGetQueryExecutionResponse,

    -- ** Response lenses
    bgqersUnprocessedQueryExecutionIds,
    bgqersQueryExecutions,
    bgqersResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetQueryExecution' smart constructor.
newtype BatchGetQueryExecution = BatchGetQueryExecution'
  { -- | An array of query execution IDs.
    queryExecutionIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetQueryExecution' with the minimum fields required to make a request.
--
-- * 'queryExecutionIds' - An array of query execution IDs.
mkBatchGetQueryExecution ::
  -- | 'queryExecutionIds'
  Lude.NonEmpty Lude.Text ->
  BatchGetQueryExecution
mkBatchGetQueryExecution pQueryExecutionIds_ =
  BatchGetQueryExecution' {queryExecutionIds = pQueryExecutionIds_}

-- | An array of query execution IDs.
--
-- /Note:/ Consider using 'queryExecutionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqeQueryExecutionIds :: Lens.Lens' BatchGetQueryExecution (Lude.NonEmpty Lude.Text)
bgqeQueryExecutionIds = Lens.lens (queryExecutionIds :: BatchGetQueryExecution -> Lude.NonEmpty Lude.Text) (\s a -> s {queryExecutionIds = a} :: BatchGetQueryExecution)
{-# DEPRECATED bgqeQueryExecutionIds "Use generic-lens or generic-optics with 'queryExecutionIds' instead." #-}

instance Lude.AWSRequest BatchGetQueryExecution where
  type Rs BatchGetQueryExecution = BatchGetQueryExecutionResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetQueryExecutionResponse'
            Lude.<$> (x Lude..?> "UnprocessedQueryExecutionIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "QueryExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetQueryExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.BatchGetQueryExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetQueryExecution where
  toJSON BatchGetQueryExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QueryExecutionIds" Lude..= queryExecutionIds)]
      )

instance Lude.ToPath BatchGetQueryExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetQueryExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetQueryExecutionResponse' smart constructor.
data BatchGetQueryExecutionResponse = BatchGetQueryExecutionResponse'
  { -- | Information about the query executions that failed to run.
    unprocessedQueryExecutionIds :: Lude.Maybe [UnprocessedQueryExecutionId],
    -- | Information about a query execution.
    queryExecutions :: Lude.Maybe [QueryExecution],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetQueryExecutionResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedQueryExecutionIds' - Information about the query executions that failed to run.
-- * 'queryExecutions' - Information about a query execution.
-- * 'responseStatus' - The response status code.
mkBatchGetQueryExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetQueryExecutionResponse
mkBatchGetQueryExecutionResponse pResponseStatus_ =
  BatchGetQueryExecutionResponse'
    { unprocessedQueryExecutionIds =
        Lude.Nothing,
      queryExecutions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the query executions that failed to run.
--
-- /Note:/ Consider using 'unprocessedQueryExecutionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqersUnprocessedQueryExecutionIds :: Lens.Lens' BatchGetQueryExecutionResponse (Lude.Maybe [UnprocessedQueryExecutionId])
bgqersUnprocessedQueryExecutionIds = Lens.lens (unprocessedQueryExecutionIds :: BatchGetQueryExecutionResponse -> Lude.Maybe [UnprocessedQueryExecutionId]) (\s a -> s {unprocessedQueryExecutionIds = a} :: BatchGetQueryExecutionResponse)
{-# DEPRECATED bgqersUnprocessedQueryExecutionIds "Use generic-lens or generic-optics with 'unprocessedQueryExecutionIds' instead." #-}

-- | Information about a query execution.
--
-- /Note:/ Consider using 'queryExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqersQueryExecutions :: Lens.Lens' BatchGetQueryExecutionResponse (Lude.Maybe [QueryExecution])
bgqersQueryExecutions = Lens.lens (queryExecutions :: BatchGetQueryExecutionResponse -> Lude.Maybe [QueryExecution]) (\s a -> s {queryExecutions = a} :: BatchGetQueryExecutionResponse)
{-# DEPRECATED bgqersQueryExecutions "Use generic-lens or generic-optics with 'queryExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgqersResponseStatus :: Lens.Lens' BatchGetQueryExecutionResponse Lude.Int
bgqersResponseStatus = Lens.lens (responseStatus :: BatchGetQueryExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetQueryExecutionResponse)
{-# DEPRECATED bgqersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

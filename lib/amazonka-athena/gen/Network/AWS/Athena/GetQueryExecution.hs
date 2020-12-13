{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single execution of a query if you have access to the workgroup in which the query ran. Each time a query executes, information about the query execution is saved with a unique ID.
module Network.AWS.Athena.GetQueryExecution
  ( -- * Creating a request
    GetQueryExecution (..),
    mkGetQueryExecution,

    -- ** Request lenses
    gqeQueryExecutionId,

    -- * Destructuring the response
    GetQueryExecutionResponse (..),
    mkGetQueryExecutionResponse,

    -- ** Response lenses
    gqersQueryExecution,
    gqersResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetQueryExecution' smart constructor.
newtype GetQueryExecution = GetQueryExecution'
  { -- | The unique ID of the query execution.
    queryExecutionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueryExecution' with the minimum fields required to make a request.
--
-- * 'queryExecutionId' - The unique ID of the query execution.
mkGetQueryExecution ::
  -- | 'queryExecutionId'
  Lude.Text ->
  GetQueryExecution
mkGetQueryExecution pQueryExecutionId_ =
  GetQueryExecution' {queryExecutionId = pQueryExecutionId_}

-- | The unique ID of the query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqeQueryExecutionId :: Lens.Lens' GetQueryExecution Lude.Text
gqeQueryExecutionId = Lens.lens (queryExecutionId :: GetQueryExecution -> Lude.Text) (\s a -> s {queryExecutionId = a} :: GetQueryExecution)
{-# DEPRECATED gqeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

instance Lude.AWSRequest GetQueryExecution where
  type Rs GetQueryExecution = GetQueryExecutionResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetQueryExecutionResponse'
            Lude.<$> (x Lude..?> "QueryExecution")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueryExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.GetQueryExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetQueryExecution where
  toJSON GetQueryExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QueryExecutionId" Lude..= queryExecutionId)]
      )

instance Lude.ToPath GetQueryExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQueryExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { -- | Information about the query execution.
    queryExecution :: Lude.Maybe QueryExecution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueryExecutionResponse' with the minimum fields required to make a request.
--
-- * 'queryExecution' - Information about the query execution.
-- * 'responseStatus' - The response status code.
mkGetQueryExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQueryExecutionResponse
mkGetQueryExecutionResponse pResponseStatus_ =
  GetQueryExecutionResponse'
    { queryExecution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the query execution.
--
-- /Note:/ Consider using 'queryExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqersQueryExecution :: Lens.Lens' GetQueryExecutionResponse (Lude.Maybe QueryExecution)
gqersQueryExecution = Lens.lens (queryExecution :: GetQueryExecutionResponse -> Lude.Maybe QueryExecution) (\s a -> s {queryExecution = a} :: GetQueryExecutionResponse)
{-# DEPRECATED gqersQueryExecution "Use generic-lens or generic-optics with 'queryExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqersResponseStatus :: Lens.Lens' GetQueryExecutionResponse Lude.Int
gqersResponseStatus = Lens.lens (responseStatus :: GetQueryExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueryExecutionResponse)
{-# DEPRECATED gqersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.StopQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a query execution. Requires you to have access to the workgroup in which the query ran.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.StopQueryExecution
  ( -- * Creating a request
    StopQueryExecution (..),
    mkStopQueryExecution,

    -- ** Request lenses
    sqeQueryExecutionId,

    -- * Destructuring the response
    StopQueryExecutionResponse (..),
    mkStopQueryExecutionResponse,

    -- ** Response lenses
    sqersResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopQueryExecution' smart constructor.
newtype StopQueryExecution = StopQueryExecution'
  { -- | The unique ID of the query execution to stop.
    queryExecutionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopQueryExecution' with the minimum fields required to make a request.
--
-- * 'queryExecutionId' - The unique ID of the query execution to stop.
mkStopQueryExecution ::
  -- | 'queryExecutionId'
  Lude.Text ->
  StopQueryExecution
mkStopQueryExecution pQueryExecutionId_ =
  StopQueryExecution' {queryExecutionId = pQueryExecutionId_}

-- | The unique ID of the query execution to stop.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryExecutionId :: Lens.Lens' StopQueryExecution Lude.Text
sqeQueryExecutionId = Lens.lens (queryExecutionId :: StopQueryExecution -> Lude.Text) (\s a -> s {queryExecutionId = a} :: StopQueryExecution)
{-# DEPRECATED sqeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

instance Lude.AWSRequest StopQueryExecution where
  type Rs StopQueryExecution = StopQueryExecutionResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopQueryExecutionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopQueryExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.StopQueryExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopQueryExecution where
  toJSON StopQueryExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QueryExecutionId" Lude..= queryExecutionId)]
      )

instance Lude.ToPath StopQueryExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StopQueryExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopQueryExecutionResponse' smart constructor.
newtype StopQueryExecutionResponse = StopQueryExecutionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopQueryExecutionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopQueryExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopQueryExecutionResponse
mkStopQueryExecutionResponse pResponseStatus_ =
  StopQueryExecutionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqersResponseStatus :: Lens.Lens' StopQueryExecutionResponse Lude.Int
sqersResponseStatus = Lens.lens (responseStatus :: StopQueryExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopQueryExecutionResponse)
{-# DEPRECATED sqersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

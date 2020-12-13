{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.StopQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a CloudWatch Logs Insights query that is in progress. If the query has already ended, the operation returns an error indicating that the specified query is not running.
module Network.AWS.CloudWatchLogs.StopQuery
  ( -- * Creating a request
    StopQuery (..),
    mkStopQuery,

    -- ** Request lenses
    sqQueryId,

    -- * Destructuring the response
    StopQueryResponse (..),
    mkStopQueryResponse,

    -- ** Response lenses
    sqrsSuccess,
    sqrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopQuery' smart constructor.
newtype StopQuery = StopQuery'
  { -- | The ID number of the query to stop. To find this ID number, use @DescribeQueries@ .
    queryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopQuery' with the minimum fields required to make a request.
--
-- * 'queryId' - The ID number of the query to stop. To find this ID number, use @DescribeQueries@ .
mkStopQuery ::
  -- | 'queryId'
  Lude.Text ->
  StopQuery
mkStopQuery pQueryId_ = StopQuery' {queryId = pQueryId_}

-- | The ID number of the query to stop. To find this ID number, use @DescribeQueries@ .
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqQueryId :: Lens.Lens' StopQuery Lude.Text
sqQueryId = Lens.lens (queryId :: StopQuery -> Lude.Text) (\s a -> s {queryId = a} :: StopQuery)
{-# DEPRECATED sqQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

instance Lude.AWSRequest StopQuery where
  type Rs StopQuery = StopQueryResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopQueryResponse'
            Lude.<$> (x Lude..?> "success") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.StopQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopQuery where
  toJSON StopQuery' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("queryId" Lude..= queryId)])

instance Lude.ToPath StopQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery StopQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopQueryResponse' smart constructor.
data StopQueryResponse = StopQueryResponse'
  { -- | This is true if the query was stopped by the @StopQuery@ operation.
    success :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopQueryResponse' with the minimum fields required to make a request.
--
-- * 'success' - This is true if the query was stopped by the @StopQuery@ operation.
-- * 'responseStatus' - The response status code.
mkStopQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopQueryResponse
mkStopQueryResponse pResponseStatus_ =
  StopQueryResponse'
    { success = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This is true if the query was stopped by the @StopQuery@ operation.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqrsSuccess :: Lens.Lens' StopQueryResponse (Lude.Maybe Lude.Bool)
sqrsSuccess = Lens.lens (success :: StopQueryResponse -> Lude.Maybe Lude.Bool) (\s a -> s {success = a} :: StopQueryResponse)
{-# DEPRECATED sqrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqrsResponseStatus :: Lens.Lens' StopQueryResponse Lude.Int
sqrsResponseStatus = Lens.lens (responseStatus :: StopQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopQueryResponse)
{-# DEPRECATED sqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

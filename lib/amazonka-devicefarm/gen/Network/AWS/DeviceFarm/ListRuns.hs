{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs, given an AWS Device Farm project ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRuns
  ( -- * Creating a request
    ListRuns (..),
    mkListRuns,

    -- ** Request lenses
    lrArn,
    lrNextToken,

    -- * Destructuring the response
    ListRunsResponse (..),
    mkListRunsResponse,

    -- ** Response lenses
    lrrsRuns,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list runs operation.
--
-- /See:/ 'mkListRuns' smart constructor.
data ListRuns = ListRuns'
  { -- | The Amazon Resource Name (ARN) of the project for which you want to list runs.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRuns' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project for which you want to list runs.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListRuns ::
  -- | 'arn'
  Lude.Text ->
  ListRuns
mkListRuns pArn_ = ListRuns' {arn = pArn_, nextToken = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the project for which you want to list runs.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrArn :: Lens.Lens' ListRuns Lude.Text
lrArn = Lens.lens (arn :: ListRuns -> Lude.Text) (\s a -> s {arn = a} :: ListRuns)
{-# DEPRECATED lrArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRuns (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRuns)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListRuns where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsRuns) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListRuns where
  type Rs ListRuns = ListRunsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRunsResponse'
            Lude.<$> (x Lude..?> "runs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRuns where
  toJSON ListRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRuns where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list runs request.
--
-- /See:/ 'mkListRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
  { -- | Information about the runs.
    runs :: Lude.Maybe [Run],
    -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRunsResponse' with the minimum fields required to make a request.
--
-- * 'runs' - Information about the runs.
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRunsResponse
mkListRunsResponse pResponseStatus_ =
  ListRunsResponse'
    { runs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the runs.
--
-- /Note:/ Consider using 'runs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRuns :: Lens.Lens' ListRunsResponse (Lude.Maybe [Run])
lrrsRuns = Lens.lens (runs :: ListRunsResponse -> Lude.Maybe [Run]) (\s a -> s {runs = a} :: ListRunsResponse)
{-# DEPRECATED lrrsRuns "Use generic-lens or generic-optics with 'runs' instead." #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRunsResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRunsResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRunsResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRunsResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

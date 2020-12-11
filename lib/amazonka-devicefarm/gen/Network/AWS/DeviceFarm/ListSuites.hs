{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about test suites for a given job.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSuites
  ( -- * Creating a request
    ListSuites (..),
    mkListSuites,

    -- ** Request lenses
    lssNextToken,
    lssArn,

    -- * Destructuring the response
    ListSuitesResponse (..),
    mkListSuitesResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsSuites,
    lsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the list suites operation.
--
-- /See:/ 'mkListSuites' smart constructor.
data ListSuites = ListSuites'
  { nextToken :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSuites' with the minimum fields required to make a request.
--
-- * 'arn' - The job's Amazon Resource Name (ARN).
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListSuites ::
  -- | 'arn'
  Lude.Text ->
  ListSuites
mkListSuites pArn_ =
  ListSuites' {nextToken = Lude.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssNextToken :: Lens.Lens' ListSuites (Lude.Maybe Lude.Text)
lssNextToken = Lens.lens (nextToken :: ListSuites -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSuites)
{-# DEPRECATED lssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The job's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssArn :: Lens.Lens' ListSuites Lude.Text
lssArn = Lens.lens (arn :: ListSuites -> Lude.Text) (\s a -> s {arn = a} :: ListSuites)
{-# DEPRECATED lssArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Page.AWSPager ListSuites where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsSuites) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lssNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListSuites where
  type Rs ListSuites = ListSuitesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSuitesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "suites" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSuites where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListSuites" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSuites where
  toJSON ListSuites' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath ListSuites where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSuites where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list suites request.
--
-- /See:/ 'mkListSuitesResponse' smart constructor.
data ListSuitesResponse = ListSuitesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    suites :: Lude.Maybe [Suite],
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

-- | Creates a value of 'ListSuitesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
-- * 'suites' - Information about the suites.
mkListSuitesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSuitesResponse
mkListSuitesResponse pResponseStatus_ =
  ListSuitesResponse'
    { nextToken = Lude.Nothing,
      suites = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListSuitesResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListSuitesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSuitesResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the suites.
--
-- /Note:/ Consider using 'suites' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsSuites :: Lens.Lens' ListSuitesResponse (Lude.Maybe [Suite])
lsrsSuites = Lens.lens (suites :: ListSuitesResponse -> Lude.Maybe [Suite]) (\s a -> s {suites = a} :: ListSuitesResponse)
{-# DEPRECATED lsrsSuites "Use generic-lens or generic-optics with 'suites' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListSuitesResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListSuitesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSuitesResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

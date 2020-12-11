{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all load balancers in an account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetLoadBalancers
  ( -- * Creating a request
    GetLoadBalancers (..),
    mkGetLoadBalancers,

    -- ** Request lenses
    glbPageToken,

    -- * Destructuring the response
    GetLoadBalancersResponse (..),
    mkGetLoadBalancersResponse,

    -- ** Response lenses
    glbsrsNextPageToken,
    glbsrsLoadBalancers,
    glbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoadBalancers' smart constructor.
newtype GetLoadBalancers = GetLoadBalancers'
  { pageToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoadBalancers' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancers@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetLoadBalancers ::
  GetLoadBalancers
mkGetLoadBalancers = GetLoadBalancers' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancers@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbPageToken :: Lens.Lens' GetLoadBalancers (Lude.Maybe Lude.Text)
glbPageToken = Lens.lens (pageToken :: GetLoadBalancers -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetLoadBalancers)
{-# DEPRECATED glbPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetLoadBalancers where
  page rq rs
    | Page.stop (rs Lens.^. glbsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. glbsrsLoadBalancers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& glbPageToken Lens..~ rs Lens.^. glbsrsNextPageToken

instance Lude.AWSRequest GetLoadBalancers where
  type Rs GetLoadBalancers = GetLoadBalancersResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoadBalancersResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "loadBalancers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoadBalancers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetLoadBalancers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLoadBalancers where
  toJSON GetLoadBalancers' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLoadBalancers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLoadBalancersResponse' smart constructor.
data GetLoadBalancersResponse = GetLoadBalancersResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    loadBalancers ::
      Lude.Maybe [LoadBalancer],
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

-- | Creates a value of 'GetLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancers' - An array of LoadBalancer objects describing your load balancers.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetLoadBalancers@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoadBalancersResponse
mkGetLoadBalancersResponse pResponseStatus_ =
  GetLoadBalancersResponse'
    { nextPageToken = Lude.Nothing,
      loadBalancers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetLoadBalancers@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbsrsNextPageToken :: Lens.Lens' GetLoadBalancersResponse (Lude.Maybe Lude.Text)
glbsrsNextPageToken = Lens.lens (nextPageToken :: GetLoadBalancersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetLoadBalancersResponse)
{-# DEPRECATED glbsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of LoadBalancer objects describing your load balancers.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbsrsLoadBalancers :: Lens.Lens' GetLoadBalancersResponse (Lude.Maybe [LoadBalancer])
glbsrsLoadBalancers = Lens.lens (loadBalancers :: GetLoadBalancersResponse -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: GetLoadBalancersResponse)
{-# DEPRECATED glbsrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbsrsResponseStatus :: Lens.Lens' GetLoadBalancersResponse Lude.Int
glbsrsResponseStatus = Lens.lens (responseStatus :: GetLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoadBalancersResponse)
{-# DEPRECATED glbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetStaticIPs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all static IPs in the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetStaticIPs
  ( -- * Creating a request
    GetStaticIPs (..),
    mkGetStaticIPs,

    -- ** Request lenses
    gsiPageToken,

    -- * Destructuring the response
    GetStaticIPsResponse (..),
    mkGetStaticIPsResponse,

    -- ** Response lenses
    gsiprsNextPageToken,
    gsiprsStaticIPs,
    gsiprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetStaticIPs' smart constructor.
newtype GetStaticIPs = GetStaticIPs'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetStaticIps@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStaticIPs' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetStaticIPs ::
  GetStaticIPs
mkGetStaticIPs = GetStaticIPs' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiPageToken :: Lens.Lens' GetStaticIPs (Lude.Maybe Lude.Text)
gsiPageToken = Lens.lens (pageToken :: GetStaticIPs -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetStaticIPs)
{-# DEPRECATED gsiPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetStaticIPs where
  page rq rs
    | Page.stop (rs Lens.^. gsiprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gsiprsStaticIPs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gsiPageToken Lens..~ rs Lens.^. gsiprsNextPageToken

instance Lude.AWSRequest GetStaticIPs where
  type Rs GetStaticIPs = GetStaticIPsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetStaticIPsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "staticIps" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStaticIPs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetStaticIps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetStaticIPs where
  toJSON GetStaticIPs' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetStaticIPs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetStaticIPs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetStaticIPsResponse' smart constructor.
data GetStaticIPsResponse = GetStaticIPsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetStaticIps@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of key-value pairs containing information about your get static IPs request.
    staticIPs :: Lude.Maybe [StaticIP],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStaticIPsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetStaticIps@ request and specify the next page token using the @pageToken@ parameter.
-- * 'staticIPs' - An array of key-value pairs containing information about your get static IPs request.
-- * 'responseStatus' - The response status code.
mkGetStaticIPsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStaticIPsResponse
mkGetStaticIPsResponse pResponseStatus_ =
  GetStaticIPsResponse'
    { nextPageToken = Lude.Nothing,
      staticIPs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetStaticIps@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiprsNextPageToken :: Lens.Lens' GetStaticIPsResponse (Lude.Maybe Lude.Text)
gsiprsNextPageToken = Lens.lens (nextPageToken :: GetStaticIPsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetStaticIPsResponse)
{-# DEPRECATED gsiprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs containing information about your get static IPs request.
--
-- /Note:/ Consider using 'staticIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiprsStaticIPs :: Lens.Lens' GetStaticIPsResponse (Lude.Maybe [StaticIP])
gsiprsStaticIPs = Lens.lens (staticIPs :: GetStaticIPsResponse -> Lude.Maybe [StaticIP]) (\s a -> s {staticIPs = a} :: GetStaticIPsResponse)
{-# DEPRECATED gsiprsStaticIPs "Use generic-lens or generic-optics with 'staticIPs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiprsResponseStatus :: Lens.Lens' GetStaticIPsResponse Lude.Int
gsiprsResponseStatus = Lens.lens (responseStatus :: GetStaticIPsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStaticIPsResponse)
{-# DEPRECATED gsiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available for purchase. A bundle describes the specs for your virtual private server (or /instance/ ).
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBundles
  ( -- * Creating a request
    GetBundles (..),
    mkGetBundles,

    -- ** Request lenses
    gbsIncludeInactive,
    gbsPageToken,

    -- * Destructuring the response
    GetBundlesResponse (..),
    mkGetBundlesResponse,

    -- ** Response lenses
    gbrsNextPageToken,
    gbrsBundles,
    gbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBundles' smart constructor.
data GetBundles = GetBundles'
  { includeInactive ::
      Lude.Maybe Lude.Bool,
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBundles' with the minimum fields required to make a request.
--
-- * 'includeInactive' - A Boolean value that indicates whether to include inactive bundle results in your request.
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetBundles ::
  GetBundles
mkGetBundles =
  GetBundles'
    { includeInactive = Lude.Nothing,
      pageToken = Lude.Nothing
    }

-- | A Boolean value that indicates whether to include inactive bundle results in your request.
--
-- /Note:/ Consider using 'includeInactive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsIncludeInactive :: Lens.Lens' GetBundles (Lude.Maybe Lude.Bool)
gbsIncludeInactive = Lens.lens (includeInactive :: GetBundles -> Lude.Maybe Lude.Bool) (\s a -> s {includeInactive = a} :: GetBundles)
{-# DEPRECATED gbsIncludeInactive "Use generic-lens or generic-optics with 'includeInactive' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsPageToken :: Lens.Lens' GetBundles (Lude.Maybe Lude.Text)
gbsPageToken = Lens.lens (pageToken :: GetBundles -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetBundles)
{-# DEPRECATED gbsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetBundles where
  page rq rs
    | Page.stop (rs Lens.^. gbrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbrsBundles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbsPageToken Lens..~ rs Lens.^. gbrsNextPageToken

instance Lude.AWSRequest GetBundles where
  type Rs GetBundles = GetBundlesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBundlesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "bundles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBundles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetBundles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBundles where
  toJSON GetBundles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeInactive" Lude..=) Lude.<$> includeInactive,
            ("pageToken" Lude..=) Lude.<$> pageToken
          ]
      )

instance Lude.ToPath GetBundles where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBundles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBundlesResponse' smart constructor.
data GetBundlesResponse = GetBundlesResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    bundles :: Lude.Maybe [Bundle],
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

-- | Creates a value of 'GetBundlesResponse' with the minimum fields required to make a request.
--
-- * 'bundles' - An array of key-value pairs that contains information about the available bundles.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBundles@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetBundlesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBundlesResponse
mkGetBundlesResponse pResponseStatus_ =
  GetBundlesResponse'
    { nextPageToken = Lude.Nothing,
      bundles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBundles@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsNextPageToken :: Lens.Lens' GetBundlesResponse (Lude.Maybe Lude.Text)
gbrsNextPageToken = Lens.lens (nextPageToken :: GetBundlesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetBundlesResponse)
{-# DEPRECATED gbrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs that contains information about the available bundles.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsBundles :: Lens.Lens' GetBundlesResponse (Lude.Maybe [Bundle])
gbrsBundles = Lens.lens (bundles :: GetBundlesResponse -> Lude.Maybe [Bundle]) (\s a -> s {bundles = a} :: GetBundlesResponse)
{-# DEPRECATED gbrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrsResponseStatus :: Lens.Lens' GetBundlesResponse Lude.Int
gbrsResponseStatus = Lens.lens (responseStatus :: GetBundlesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBundlesResponse)
{-# DEPRECATED gbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

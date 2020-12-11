{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.ListBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all available bundles.
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListBundles
  ( -- * Creating a request
    ListBundles (..),
    mkListBundles,

    -- ** Request lenses
    lbNextToken,
    lbMaxResults,

    -- * Destructuring the response
    ListBundlesResponse (..),
    mkListBundlesResponse,

    -- ** Response lenses
    lbrsBundleList,
    lbrsNextToken,
    lbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure to request all available bundles.
--
-- /See:/ 'mkListBundles' smart constructor.
data ListBundles = ListBundles'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBundles' with the minimum fields required to make a request.
--
-- * 'maxResults' - Maximum number of records to list in a single response.
-- * 'nextToken' - Pagination token. Set to null to start listing bundles from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more bundles.
mkListBundles ::
  ListBundles
mkListBundles =
  ListBundles' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | Pagination token. Set to null to start listing bundles from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more bundles.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBundles (Lude.Maybe Lude.Text)
lbNextToken = Lens.lens (nextToken :: ListBundles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBundles)
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of records to list in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbMaxResults :: Lens.Lens' ListBundles (Lude.Maybe Lude.Int)
lbMaxResults = Lens.lens (maxResults :: ListBundles -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListBundles)
{-# DEPRECATED lbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBundles where
  page rq rs
    | Page.stop (rs Lens.^. lbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsBundleList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbNextToken Lens..~ rs Lens.^. lbrsNextToken

instance Lude.AWSRequest ListBundles where
  type Rs ListBundles = ListBundlesResponse
  request = Req.get mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBundlesResponse'
            Lude.<$> (x Lude..?> "bundleList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBundles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListBundles where
  toPath = Lude.const "/bundles"

instance Lude.ToQuery ListBundles where
  toQuery ListBundles' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Result structure contains a list of all available bundles with details.
--
-- /See:/ 'mkListBundlesResponse' smart constructor.
data ListBundlesResponse = ListBundlesResponse'
  { bundleList ::
      Lude.Maybe [BundleDetails],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListBundlesResponse' with the minimum fields required to make a request.
--
-- * 'bundleList' - A list of bundles.
-- * 'nextToken' - Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries.
-- * 'responseStatus' - The response status code.
mkListBundlesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBundlesResponse
mkListBundlesResponse pResponseStatus_ =
  ListBundlesResponse'
    { bundleList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of bundles.
--
-- /Note:/ Consider using 'bundleList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsBundleList :: Lens.Lens' ListBundlesResponse (Lude.Maybe [BundleDetails])
lbrsBundleList = Lens.lens (bundleList :: ListBundlesResponse -> Lude.Maybe [BundleDetails]) (\s a -> s {bundleList = a} :: ListBundlesResponse)
{-# DEPRECATED lbrsBundleList "Use generic-lens or generic-optics with 'bundleList' instead." #-}

-- | Pagination token. If non-null pagination token is returned in a result, then pass its value in another request to fetch more entries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBundlesResponse (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBundlesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBundlesResponse)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBundlesResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBundlesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBundlesResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

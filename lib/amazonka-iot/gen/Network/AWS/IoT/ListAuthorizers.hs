{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuthorizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the authorizers registered in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuthorizers
  ( -- * Creating a request
    ListAuthorizers (..),
    mkListAuthorizers,

    -- ** Request lenses
    laStatus,
    laMarker,
    laAscendingOrder,
    laPageSize,

    -- * Destructuring the response
    ListAuthorizersResponse (..),
    mkListAuthorizersResponse,

    -- ** Response lenses
    larsAuthorizers,
    larsNextMarker,
    larsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuthorizers' smart constructor.
data ListAuthorizers = ListAuthorizers'
  { -- | The status of the list authorizers request.
    status :: Lude.Maybe AuthorizerStatus,
    -- | A marker used to get the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | Return the list of authorizers in ascending alphabetical order.
    ascendingOrder :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return at one time.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuthorizers' with the minimum fields required to make a request.
--
-- * 'status' - The status of the list authorizers request.
-- * 'marker' - A marker used to get the next set of results.
-- * 'ascendingOrder' - Return the list of authorizers in ascending alphabetical order.
-- * 'pageSize' - The maximum number of results to return at one time.
mkListAuthorizers ::
  ListAuthorizers
mkListAuthorizers =
  ListAuthorizers'
    { status = Lude.Nothing,
      marker = Lude.Nothing,
      ascendingOrder = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The status of the list authorizers request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laStatus :: Lens.Lens' ListAuthorizers (Lude.Maybe AuthorizerStatus)
laStatus = Lens.lens (status :: ListAuthorizers -> Lude.Maybe AuthorizerStatus) (\s a -> s {status = a} :: ListAuthorizers)
{-# DEPRECATED laStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAuthorizers (Lude.Maybe Lude.Text)
laMarker = Lens.lens (marker :: ListAuthorizers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAuthorizers)
{-# DEPRECATED laMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Return the list of authorizers in ascending alphabetical order.
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAscendingOrder :: Lens.Lens' ListAuthorizers (Lude.Maybe Lude.Bool)
laAscendingOrder = Lens.lens (ascendingOrder :: ListAuthorizers -> Lude.Maybe Lude.Bool) (\s a -> s {ascendingOrder = a} :: ListAuthorizers)
{-# DEPRECATED laAscendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laPageSize :: Lens.Lens' ListAuthorizers (Lude.Maybe Lude.Natural)
laPageSize = Lens.lens (pageSize :: ListAuthorizers -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: ListAuthorizers)
{-# DEPRECATED laPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager ListAuthorizers where
  page rq rs
    | Page.stop (rs Lens.^. larsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAuthorizers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laMarker Lens..~ rs Lens.^. larsNextMarker

instance Lude.AWSRequest ListAuthorizers where
  type Rs ListAuthorizers = ListAuthorizersResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuthorizersResponse'
            Lude.<$> (x Lude..?> "authorizers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuthorizers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAuthorizers where
  toPath = Lude.const "/authorizers/"

instance Lude.ToQuery ListAuthorizers where
  toQuery ListAuthorizers' {..} =
    Lude.mconcat
      [ "status" Lude.=: status,
        "marker" Lude.=: marker,
        "isAscendingOrder" Lude.=: ascendingOrder,
        "pageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkListAuthorizersResponse' smart constructor.
data ListAuthorizersResponse = ListAuthorizersResponse'
  { -- | The authorizers.
    authorizers :: Lude.Maybe [AuthorizerSummary],
    -- | A marker used to get the next set of results.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuthorizersResponse' with the minimum fields required to make a request.
--
-- * 'authorizers' - The authorizers.
-- * 'nextMarker' - A marker used to get the next set of results.
-- * 'responseStatus' - The response status code.
mkListAuthorizersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuthorizersResponse
mkListAuthorizersResponse pResponseStatus_ =
  ListAuthorizersResponse'
    { authorizers = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The authorizers.
--
-- /Note:/ Consider using 'authorizers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAuthorizers :: Lens.Lens' ListAuthorizersResponse (Lude.Maybe [AuthorizerSummary])
larsAuthorizers = Lens.lens (authorizers :: ListAuthorizersResponse -> Lude.Maybe [AuthorizerSummary]) (\s a -> s {authorizers = a} :: ListAuthorizersResponse)
{-# DEPRECATED larsAuthorizers "Use generic-lens or generic-optics with 'authorizers' instead." #-}

-- | A marker used to get the next set of results.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextMarker :: Lens.Lens' ListAuthorizersResponse (Lude.Maybe Lude.Text)
larsNextMarker = Lens.lens (nextMarker :: ListAuthorizersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListAuthorizersResponse)
{-# DEPRECATED larsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAuthorizersResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAuthorizersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuthorizersResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

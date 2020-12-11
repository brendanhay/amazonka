{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListTapePools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists custom tape pools. You specify custom tape pools to list by specifying one or more custom tape pool Amazon Resource Names (ARNs). If you don't specify a custom tape pool ARN, the operation lists all custom tape pools.
--
-- This operation supports pagination. You can optionally specify the @Limit@ parameter in the body to limit the number of tape pools in the response. If the number of tape pools returned in the response is truncated, the response includes a @Marker@ element that you can use in your subsequent request to retrieve the next set of tape pools.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapePools
  ( -- * Creating a request
    ListTapePools (..),
    mkListTapePools,

    -- ** Request lenses
    ltpPoolARNs,
    ltpMarker,
    ltpLimit,

    -- * Destructuring the response
    ListTapePoolsResponse (..),
    mkListTapePoolsResponse,

    -- ** Response lenses
    ltprsPoolInfos,
    ltprsMarker,
    ltprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkListTapePools' smart constructor.
data ListTapePools = ListTapePools'
  { poolARNs ::
      Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTapePools' with the minimum fields required to make a request.
--
-- * 'limit' - An optional number limit for the tape pools in the list returned by this call.
-- * 'marker' - A string that indicates the position at which to begin the returned list of tape pools.
-- * 'poolARNs' - The Amazon Resource Name (ARN) of each of the custom tape pools you want to list. If you don't specify a custom tape pool ARN, the response lists all custom tape pools.
mkListTapePools ::
  ListTapePools
mkListTapePools =
  ListTapePools'
    { poolARNs = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of each of the custom tape pools you want to list. If you don't specify a custom tape pool ARN, the response lists all custom tape pools.
--
-- /Note:/ Consider using 'poolARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpPoolARNs :: Lens.Lens' ListTapePools (Lude.Maybe [Lude.Text])
ltpPoolARNs = Lens.lens (poolARNs :: ListTapePools -> Lude.Maybe [Lude.Text]) (\s a -> s {poolARNs = a} :: ListTapePools)
{-# DEPRECATED ltpPoolARNs "Use generic-lens or generic-optics with 'poolARNs' instead." #-}

-- | A string that indicates the position at which to begin the returned list of tape pools.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpMarker :: Lens.Lens' ListTapePools (Lude.Maybe Lude.Text)
ltpMarker = Lens.lens (marker :: ListTapePools -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTapePools)
{-# DEPRECATED ltpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An optional number limit for the tape pools in the list returned by this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpLimit :: Lens.Lens' ListTapePools (Lude.Maybe Lude.Natural)
ltpLimit = Lens.lens (limit :: ListTapePools -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTapePools)
{-# DEPRECATED ltpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListTapePools where
  page rq rs
    | Page.stop (rs Lens.^. ltprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ltprsPoolInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ltpMarker Lens..~ rs Lens.^. ltprsMarker

instance Lude.AWSRequest ListTapePools where
  type Rs ListTapePools = ListTapePoolsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTapePoolsResponse'
            Lude.<$> (x Lude..?> "PoolInfos" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTapePools where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListTapePools" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTapePools where
  toJSON ListTapePools' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PoolARNs" Lude..=) Lude.<$> poolARNs,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListTapePools where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTapePools where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTapePoolsResponse' smart constructor.
data ListTapePoolsResponse = ListTapePoolsResponse'
  { poolInfos ::
      Lude.Maybe [PoolInfo],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTapePoolsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A string that indicates the position at which to begin the returned list of tape pools. Use the marker in your next request to continue pagination of tape pools. If there are no more tape pools to list, this element does not appear in the response body.
-- * 'poolInfos' - An array of @PoolInfo@ objects, where each object describes a single custom tape pool. If there are no custom tape pools, the @PoolInfos@ is an empty array.
-- * 'responseStatus' - The response status code.
mkListTapePoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTapePoolsResponse
mkListTapePoolsResponse pResponseStatus_ =
  ListTapePoolsResponse'
    { poolInfos = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @PoolInfo@ objects, where each object describes a single custom tape pool. If there are no custom tape pools, the @PoolInfos@ is an empty array.
--
-- /Note:/ Consider using 'poolInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsPoolInfos :: Lens.Lens' ListTapePoolsResponse (Lude.Maybe [PoolInfo])
ltprsPoolInfos = Lens.lens (poolInfos :: ListTapePoolsResponse -> Lude.Maybe [PoolInfo]) (\s a -> s {poolInfos = a} :: ListTapePoolsResponse)
{-# DEPRECATED ltprsPoolInfos "Use generic-lens or generic-optics with 'poolInfos' instead." #-}

-- | A string that indicates the position at which to begin the returned list of tape pools. Use the marker in your next request to continue pagination of tape pools. If there are no more tape pools to list, this element does not appear in the response body.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsMarker :: Lens.Lens' ListTapePoolsResponse (Lude.Maybe Lude.Text)
ltprsMarker = Lens.lens (marker :: ListTapePoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTapePoolsResponse)
{-# DEPRECATED ltprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsResponseStatus :: Lens.Lens' ListTapePoolsResponse Lude.Int
ltprsResponseStatus = Lens.lens (responseStatus :: ListTapePoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTapePoolsResponse)
{-# DEPRECATED ltprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

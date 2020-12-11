{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists gateways owned by an AWS account in an AWS Region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).
--
-- By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.
-- If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListGateways
  ( -- * Creating a request
    ListGateways (..),
    mkListGateways,

    -- ** Request lenses
    lgMarker,
    lgLimit,

    -- * Destructuring the response
    ListGatewaysResponse (..),
    mkListGatewaysResponse,

    -- ** Response lenses
    lgrsMarker,
    lgrsGateways,
    lgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing zero or more of the following fields:
--
--
--     * 'ListGatewaysInput$Limit'
--
--
--     * 'ListGatewaysInput$Marker'
--
--
--
-- /See:/ 'mkListGateways' smart constructor.
data ListGateways = ListGateways'
  { marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListGateways' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies that the list of gateways returned be limited to the specified number of items.
-- * 'marker' - An opaque string that indicates the position at which to begin the returned list of gateways.
mkListGateways ::
  ListGateways
mkListGateways =
  ListGateways' {marker = Lude.Nothing, limit = Lude.Nothing}

-- | An opaque string that indicates the position at which to begin the returned list of gateways.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMarker :: Lens.Lens' ListGateways (Lude.Maybe Lude.Text)
lgMarker = Lens.lens (marker :: ListGateways -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGateways)
{-# DEPRECATED lgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the list of gateways returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLimit :: Lens.Lens' ListGateways (Lude.Maybe Lude.Natural)
lgLimit = Lens.lens (limit :: ListGateways -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListGateways)
{-# DEPRECATED lgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListGateways where
  page rq rs
    | Page.stop (rs Lens.^. lgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lgrsGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lgMarker Lens..~ rs Lens.^. lgrsMarker

instance Lude.AWSRequest ListGateways where
  type Rs ListGateways = ListGatewaysResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Gateways" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGateways where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListGateways" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGateways where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    gateways :: Lude.Maybe [GatewayInfo],
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

-- | Creates a value of 'ListGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'gateways' - An array of 'GatewayInfo' objects.
-- * 'marker' - Use the marker in your next request to fetch the next set of gateways in the list. If there are no more gateways to list, this field does not appear in the response.
-- * 'responseStatus' - The response status code.
mkListGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGatewaysResponse
mkListGatewaysResponse pResponseStatus_ =
  ListGatewaysResponse'
    { marker = Lude.Nothing,
      gateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Use the marker in your next request to fetch the next set of gateways in the list. If there are no more gateways to list, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsMarker :: Lens.Lens' ListGatewaysResponse (Lude.Maybe Lude.Text)
lgrsMarker = Lens.lens (marker :: ListGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of 'GatewayInfo' objects.
--
-- /Note:/ Consider using 'gateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGateways :: Lens.Lens' ListGatewaysResponse (Lude.Maybe [GatewayInfo])
lgrsGateways = Lens.lens (gateways :: ListGatewaysResponse -> Lude.Maybe [GatewayInfo]) (\s a -> s {gateways = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsGateways "Use generic-lens or generic-optics with 'gateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGatewaysResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

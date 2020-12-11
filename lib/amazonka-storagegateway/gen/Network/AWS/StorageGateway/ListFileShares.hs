{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the file shares for a specific file gateway, or the list of file shares that belong to the calling user account. This operation is only supported for file gateways.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListFileShares
  ( -- * Creating a request
    ListFileShares (..),
    mkListFileShares,

    -- ** Request lenses
    lfsGatewayARN,
    lfsMarker,
    lfsLimit,

    -- * Destructuring the response
    ListFileSharesResponse (..),
    mkListFileSharesResponse,

    -- ** Response lenses
    lfsrsFileShareInfoList,
    lfsrsMarker,
    lfsrsNextMarker,
    lfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | ListFileShareInput
--
-- /See:/ 'mkListFileShares' smart constructor.
data ListFileShares = ListFileShares'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListFileShares' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
-- * 'limit' - The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
-- * 'marker' - Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
mkListFileShares ::
  ListFileShares
mkListFileShares =
  ListFileShares'
    { gatewayARN = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway whose file shares you want to list. If this field is not present, all file shares under your account are listed.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsGatewayARN :: Lens.Lens' ListFileShares (Lude.Maybe Lude.Text)
lfsGatewayARN = Lens.lens (gatewayARN :: ListFileShares -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListFileShares)
{-# DEPRECATED lfsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Opaque pagination token returned from a previous ListFileShares operation. If present, @Marker@ specifies where to continue the list from after a previous call to ListFileShares. Optional.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsMarker :: Lens.Lens' ListFileShares (Lude.Maybe Lude.Text)
lfsMarker = Lens.lens (marker :: ListFileShares -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFileShares)
{-# DEPRECATED lfsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of file shares to return in the response. The value must be an integer with a value greater than zero. Optional.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsLimit :: Lens.Lens' ListFileShares (Lude.Maybe Lude.Natural)
lfsLimit = Lens.lens (limit :: ListFileShares -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListFileShares)
{-# DEPRECATED lfsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListFileShares where
  page rq rs
    | Page.stop (rs Lens.^. lfsrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lfsrsFileShareInfoList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfsMarker Lens..~ rs Lens.^. lfsrsNextMarker

instance Lude.AWSRequest ListFileShares where
  type Rs ListFileShares = ListFileSharesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFileSharesResponse'
            Lude.<$> (x Lude..?> "FileShareInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFileShares where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListFileShares" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListFileShares where
  toJSON ListFileShares' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GatewayARN" Lude..=) Lude.<$> gatewayARN,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListFileShares where
  toPath = Lude.const "/"

instance Lude.ToQuery ListFileShares where
  toQuery = Lude.const Lude.mempty

-- | ListFileShareOutput
--
-- /See:/ 'mkListFileSharesResponse' smart constructor.
data ListFileSharesResponse = ListFileSharesResponse'
  { fileShareInfoList ::
      Lude.Maybe [FileShareInfo],
    marker :: Lude.Maybe Lude.Text,
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListFileSharesResponse' with the minimum fields required to make a request.
--
-- * 'fileShareInfoList' - An array of information about the file gateway's file shares.
-- * 'marker' - If the request includes @Marker@ , the response returns that value in this field.
-- * 'nextMarker' - If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
-- * 'responseStatus' - The response status code.
mkListFileSharesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFileSharesResponse
mkListFileSharesResponse pResponseStatus_ =
  ListFileSharesResponse'
    { fileShareInfoList = Lude.Nothing,
      marker = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of information about the file gateway's file shares.
--
-- /Note:/ Consider using 'fileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrsFileShareInfoList :: Lens.Lens' ListFileSharesResponse (Lude.Maybe [FileShareInfo])
lfsrsFileShareInfoList = Lens.lens (fileShareInfoList :: ListFileSharesResponse -> Lude.Maybe [FileShareInfo]) (\s a -> s {fileShareInfoList = a} :: ListFileSharesResponse)
{-# DEPRECATED lfsrsFileShareInfoList "Use generic-lens or generic-optics with 'fileShareInfoList' instead." #-}

-- | If the request includes @Marker@ , the response returns that value in this field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrsMarker :: Lens.Lens' ListFileSharesResponse (Lude.Maybe Lude.Text)
lfsrsMarker = Lens.lens (marker :: ListFileSharesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFileSharesResponse)
{-# DEPRECATED lfsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | If a value is present, there are more file shares to return. In a subsequent request, use @NextMarker@ as the value for @Marker@ to retrieve the next set of file shares.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrsNextMarker :: Lens.Lens' ListFileSharesResponse (Lude.Maybe Lude.Text)
lfsrsNextMarker = Lens.lens (nextMarker :: ListFileSharesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListFileSharesResponse)
{-# DEPRECATED lfsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfsrsResponseStatus :: Lens.Lens' ListFileSharesResponse Lude.Int
lfsrsResponseStatus = Lens.lens (responseStatus :: ListFileSharesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFileSharesResponse)
{-# DEPRECATED lfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

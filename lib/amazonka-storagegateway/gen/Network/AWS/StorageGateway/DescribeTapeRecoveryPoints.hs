{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of virtual tape recovery points that are available for the specified tape gateway.
--
-- A recovery point is a point-in-time view of a virtual tape at which all the data on the virtual tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway. This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
  ( -- * Creating a request
    DescribeTapeRecoveryPoints (..),
    mkDescribeTapeRecoveryPoints,

    -- ** Request lenses
    dtrpMarker,
    dtrpLimit,
    dtrpGatewayARN,

    -- * Destructuring the response
    DescribeTapeRecoveryPointsResponse (..),
    mkDescribeTapeRecoveryPointsResponse,

    -- ** Response lenses
    dtrprsTapeRecoveryPointInfos,
    dtrprsGatewayARN,
    dtrprsMarker,
    dtrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DescribeTapeRecoveryPointsInput
--
-- /See:/ 'mkDescribeTapeRecoveryPoints' smart constructor.
data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints'
  { marker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    gatewayARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTapeRecoveryPoints' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'limit' - Specifies that the number of virtual tape recovery points that are described be limited to the specified number.
-- * 'marker' - An opaque string that indicates the position at which to begin describing the virtual tape recovery points.
mkDescribeTapeRecoveryPoints ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeTapeRecoveryPoints
mkDescribeTapeRecoveryPoints pGatewayARN_ =
  DescribeTapeRecoveryPoints'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | An opaque string that indicates the position at which to begin describing the virtual tape recovery points.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrpMarker :: Lens.Lens' DescribeTapeRecoveryPoints (Lude.Maybe Lude.Text)
dtrpMarker = Lens.lens (marker :: DescribeTapeRecoveryPoints -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTapeRecoveryPoints)
{-# DEPRECATED dtrpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the number of virtual tape recovery points that are described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrpLimit :: Lens.Lens' DescribeTapeRecoveryPoints (Lude.Maybe Lude.Natural)
dtrpLimit = Lens.lens (limit :: DescribeTapeRecoveryPoints -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeTapeRecoveryPoints)
{-# DEPRECATED dtrpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrpGatewayARN :: Lens.Lens' DescribeTapeRecoveryPoints Lude.Text
dtrpGatewayARN = Lens.lens (gatewayARN :: DescribeTapeRecoveryPoints -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeTapeRecoveryPoints)
{-# DEPRECATED dtrpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Page.AWSPager DescribeTapeRecoveryPoints where
  page rq rs
    | Page.stop (rs Lens.^. dtrprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrprsTapeRecoveryPointInfos) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtrpMarker Lens..~ rs Lens.^. dtrprsMarker

instance Lude.AWSRequest DescribeTapeRecoveryPoints where
  type
    Rs DescribeTapeRecoveryPoints =
      DescribeTapeRecoveryPointsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTapeRecoveryPointsResponse'
            Lude.<$> (x Lude..?> "TapeRecoveryPointInfos" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTapeRecoveryPoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeTapeRecoveryPoints" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTapeRecoveryPoints where
  toJSON DescribeTapeRecoveryPoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath DescribeTapeRecoveryPoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTapeRecoveryPoints where
  toQuery = Lude.const Lude.mempty

-- | DescribeTapeRecoveryPointsOutput
--
-- /See:/ 'mkDescribeTapeRecoveryPointsResponse' smart constructor.
data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse'
  { tapeRecoveryPointInfos ::
      Lude.Maybe
        [TapeRecoveryPointInfo],
    gatewayARN ::
      Lude.Maybe Lude.Text,
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTapeRecoveryPointsResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'marker' - An opaque string that indicates the position at which the virtual tape recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual tape recovery points in the list. If there are no more recovery points to describe, this field does not appear in the response.
-- * 'responseStatus' - The response status code.
-- * 'tapeRecoveryPointInfos' - An array of TapeRecoveryPointInfos that are available for the specified gateway.
mkDescribeTapeRecoveryPointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTapeRecoveryPointsResponse
mkDescribeTapeRecoveryPointsResponse pResponseStatus_ =
  DescribeTapeRecoveryPointsResponse'
    { tapeRecoveryPointInfos =
        Lude.Nothing,
      gatewayARN = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of TapeRecoveryPointInfos that are available for the specified gateway.
--
-- /Note:/ Consider using 'tapeRecoveryPointInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrprsTapeRecoveryPointInfos :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Lude.Maybe [TapeRecoveryPointInfo])
dtrprsTapeRecoveryPointInfos = Lens.lens (tapeRecoveryPointInfos :: DescribeTapeRecoveryPointsResponse -> Lude.Maybe [TapeRecoveryPointInfo]) (\s a -> s {tapeRecoveryPointInfos = a} :: DescribeTapeRecoveryPointsResponse)
{-# DEPRECATED dtrprsTapeRecoveryPointInfos "Use generic-lens or generic-optics with 'tapeRecoveryPointInfos' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrprsGatewayARN :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Lude.Maybe Lude.Text)
dtrprsGatewayARN = Lens.lens (gatewayARN :: DescribeTapeRecoveryPointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeTapeRecoveryPointsResponse)
{-# DEPRECATED dtrprsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An opaque string that indicates the position at which the virtual tape recovery points that were listed for description ended.
--
-- Use this marker in your next request to list the next set of virtual tape recovery points in the list. If there are no more recovery points to describe, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrprsMarker :: Lens.Lens' DescribeTapeRecoveryPointsResponse (Lude.Maybe Lude.Text)
dtrprsMarker = Lens.lens (marker :: DescribeTapeRecoveryPointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTapeRecoveryPointsResponse)
{-# DEPRECATED dtrprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrprsResponseStatus :: Lens.Lens' DescribeTapeRecoveryPointsResponse Lude.Int
dtrprsResponseStatus = Lens.lens (responseStatus :: DescribeTapeRecoveryPointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTapeRecoveryPointsResponse)
{-# DEPRECATED dtrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

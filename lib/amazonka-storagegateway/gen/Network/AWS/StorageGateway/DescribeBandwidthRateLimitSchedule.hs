{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the bandwidth rate limit schedule of a gateway. By default, gateways do not have bandwidth rate limit schedules, which means no bandwidth rate limiting is in effect. This operation is supported only in the volume and tape gateway types.
--
-- This operation returns information about a gateway's bandwidth rate limit schedule. A bandwidth rate limit schedule consists of one or more bandwidth rate limit intervals. A bandwidth rate limit interval defines a period of time on one or more days of the week, during which bandwidth rate limits are specified for uploading, downloading, or both.
-- A bandwidth rate limit interval consists of one or more days of the week, a start hour and minute, an ending hour and minute, and bandwidth rate limits for uploading and downloading
-- If no bandwidth rate limit schedule intervals are set for the gateway, this operation returns an empty response. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
  ( -- * Creating a request
    DescribeBandwidthRateLimitSchedule (..),
    mkDescribeBandwidthRateLimitSchedule,

    -- ** Request lenses
    dbrlsGatewayARN,

    -- * Destructuring the response
    DescribeBandwidthRateLimitScheduleResponse (..),
    mkDescribeBandwidthRateLimitScheduleResponse,

    -- ** Response lenses
    dbrlsrsGatewayARN,
    dbrlsrsBandwidthRateLimitIntervals,
    dbrlsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeBandwidthRateLimitSchedule' smart constructor.
newtype DescribeBandwidthRateLimitSchedule = DescribeBandwidthRateLimitSchedule'
  { gatewayARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBandwidthRateLimitSchedule' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkDescribeBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeBandwidthRateLimitSchedule
mkDescribeBandwidthRateLimitSchedule pGatewayARN_ =
  DescribeBandwidthRateLimitSchedule' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitSchedule Lude.Text
dbrlsGatewayARN = Lens.lens (gatewayARN :: DescribeBandwidthRateLimitSchedule -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitSchedule)
{-# DEPRECATED dbrlsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeBandwidthRateLimitSchedule where
  type
    Rs DescribeBandwidthRateLimitSchedule =
      DescribeBandwidthRateLimitScheduleResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitScheduleResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "BandwidthRateLimitIntervals" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBandwidthRateLimitSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeBandwidthRateLimitSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBandwidthRateLimitSchedule where
  toJSON DescribeBandwidthRateLimitSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeBandwidthRateLimitSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBandwidthRateLimitSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBandwidthRateLimitScheduleResponse' smart constructor.
data DescribeBandwidthRateLimitScheduleResponse = DescribeBandwidthRateLimitScheduleResponse'
  { gatewayARN ::
      Lude.Maybe
        Lude.Text,
    bandwidthRateLimitIntervals ::
      Lude.Maybe
        [BandwidthRateLimitInterval],
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

-- | Creates a value of 'DescribeBandwidthRateLimitScheduleResponse' with the minimum fields required to make a request.
--
-- * 'bandwidthRateLimitIntervals' - An array that contains the bandwidth rate limit intervals for a tape or volume gateway.
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBandwidthRateLimitScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBandwidthRateLimitScheduleResponse
mkDescribeBandwidthRateLimitScheduleResponse pResponseStatus_ =
  DescribeBandwidthRateLimitScheduleResponse'
    { gatewayARN =
        Lude.Nothing,
      bandwidthRateLimitIntervals = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Lude.Maybe Lude.Text)
dbrlsrsGatewayARN = Lens.lens (gatewayARN :: DescribeBandwidthRateLimitScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitScheduleResponse)
{-# DEPRECATED dbrlsrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array that contains the bandwidth rate limit intervals for a tape or volume gateway.
--
-- /Note:/ Consider using 'bandwidthRateLimitIntervals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrsBandwidthRateLimitIntervals :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Lude.Maybe [BandwidthRateLimitInterval])
dbrlsrsBandwidthRateLimitIntervals = Lens.lens (bandwidthRateLimitIntervals :: DescribeBandwidthRateLimitScheduleResponse -> Lude.Maybe [BandwidthRateLimitInterval]) (\s a -> s {bandwidthRateLimitIntervals = a} :: DescribeBandwidthRateLimitScheduleResponse)
{-# DEPRECATED dbrlsrsBandwidthRateLimitIntervals "Use generic-lens or generic-optics with 'bandwidthRateLimitIntervals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlsrsResponseStatus :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse Lude.Int
dbrlsrsResponseStatus = Lens.lens (responseStatus :: DescribeBandwidthRateLimitScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBandwidthRateLimitScheduleResponse)
{-# DEPRECATED dbrlsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

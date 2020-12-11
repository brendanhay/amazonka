{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limit schedule for a specified gateway. By default, gateways do not have bandwidth rate limit schedules, which means no bandwidth rate limiting is in effect. Use this to initiate or update a gateway's bandwidth rate limit schedule. This operation is supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
  ( -- * Creating a request
    UpdateBandwidthRateLimitSchedule (..),
    mkUpdateBandwidthRateLimitSchedule,

    -- ** Request lenses
    ubrlsGatewayARN,
    ubrlsBandwidthRateLimitIntervals,

    -- * Destructuring the response
    UpdateBandwidthRateLimitScheduleResponse (..),
    mkUpdateBandwidthRateLimitScheduleResponse,

    -- ** Response lenses
    ubrlsrsGatewayARN,
    ubrlsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { gatewayARN ::
      Lude.Text,
    bandwidthRateLimitIntervals ::
      [BandwidthRateLimitInterval]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBandwidthRateLimitSchedule' with the minimum fields required to make a request.
--
-- * 'bandwidthRateLimitIntervals' - An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
-- * 'gatewayARN' - Undocumented field.
mkUpdateBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Lude.Text ->
  UpdateBandwidthRateLimitSchedule
mkUpdateBandwidthRateLimitSchedule pGatewayARN_ =
  UpdateBandwidthRateLimitSchedule'
    { gatewayARN = pGatewayARN_,
      bandwidthRateLimitIntervals = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsGatewayARN :: Lens.Lens' UpdateBandwidthRateLimitSchedule Lude.Text
ubrlsGatewayARN = Lens.lens (gatewayARN :: UpdateBandwidthRateLimitSchedule -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitSchedule)
{-# DEPRECATED ubrlsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array containing bandwidth rate limit schedule intervals for a gateway. When no bandwidth rate limit intervals have been scheduled, the array is empty.
--
-- /Note:/ Consider using 'bandwidthRateLimitIntervals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsBandwidthRateLimitIntervals :: Lens.Lens' UpdateBandwidthRateLimitSchedule [BandwidthRateLimitInterval]
ubrlsBandwidthRateLimitIntervals = Lens.lens (bandwidthRateLimitIntervals :: UpdateBandwidthRateLimitSchedule -> [BandwidthRateLimitInterval]) (\s a -> s {bandwidthRateLimitIntervals = a} :: UpdateBandwidthRateLimitSchedule)
{-# DEPRECATED ubrlsBandwidthRateLimitIntervals "Use generic-lens or generic-optics with 'bandwidthRateLimitIntervals' instead." #-}

instance Lude.AWSRequest UpdateBandwidthRateLimitSchedule where
  type
    Rs UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBandwidthRateLimitSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBandwidthRateLimitSchedule where
  toJSON UpdateBandwidthRateLimitSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just
              ( "BandwidthRateLimitIntervals"
                  Lude..= bandwidthRateLimitIntervals
              )
          ]
      )

instance Lude.ToPath UpdateBandwidthRateLimitSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBandwidthRateLimitSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { gatewayARN ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'UpdateBandwidthRateLimitScheduleResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateBandwidthRateLimitScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBandwidthRateLimitScheduleResponse
mkUpdateBandwidthRateLimitScheduleResponse pResponseStatus_ =
  UpdateBandwidthRateLimitScheduleResponse'
    { gatewayARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsrsGatewayARN :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse (Lude.Maybe Lude.Text)
ubrlsrsGatewayARN = Lens.lens (gatewayARN :: UpdateBandwidthRateLimitScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitScheduleResponse)
{-# DEPRECATED ubrlsrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlsrsResponseStatus :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse Lude.Int
ubrlsrsResponseStatus = Lens.lens (responseStatus :: UpdateBandwidthRateLimitScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBandwidthRateLimitScheduleResponse)
{-# DEPRECATED ubrlsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

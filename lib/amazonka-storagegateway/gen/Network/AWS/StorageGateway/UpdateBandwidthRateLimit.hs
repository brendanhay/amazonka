{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limits of a gateway. You can update both the upload and download bandwidth rate limit or specify only one of the two. If you don't set a bandwidth rate limit, the existing rate limit remains. This operation is supported for the stored volume, cached volume, and tape gateway types.
--
-- By default, a gateway's bandwidth rate limits are not set. If you don't set any limit, the gateway does not have any limitations on its bandwidth usage and could potentially use the maximum available bandwidth.
-- To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
  ( -- * Creating a request
    UpdateBandwidthRateLimit (..),
    mkUpdateBandwidthRateLimit,

    -- ** Request lenses
    ubrlAverageUploadRateLimitInBitsPerSec,
    ubrlAverageDownloadRateLimitInBitsPerSec,
    ubrlGatewayARN,

    -- * Destructuring the response
    UpdateBandwidthRateLimitResponse (..),
    mkUpdateBandwidthRateLimitResponse,

    -- ** Response lenses
    ubrlrsGatewayARN,
    ubrlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec'
--
--
--     * 'UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec'
--
--
--
-- /See:/ 'mkUpdateBandwidthRateLimit' smart constructor.
data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit'
  { averageUploadRateLimitInBitsPerSec ::
      Lude.Maybe Lude.Natural,
    averageDownloadRateLimitInBitsPerSec ::
      Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'UpdateBandwidthRateLimit' with the minimum fields required to make a request.
--
-- * 'averageDownloadRateLimitInBitsPerSec' - The average download bandwidth rate limit in bits per second.
-- * 'averageUploadRateLimitInBitsPerSec' - The average upload bandwidth rate limit in bits per second.
-- * 'gatewayARN' - Undocumented field.
mkUpdateBandwidthRateLimit ::
  -- | 'gatewayARN'
  Lude.Text ->
  UpdateBandwidthRateLimit
mkUpdateBandwidthRateLimit pGatewayARN_ =
  UpdateBandwidthRateLimit'
    { averageUploadRateLimitInBitsPerSec =
        Lude.Nothing,
      averageDownloadRateLimitInBitsPerSec = Lude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | The average upload bandwidth rate limit in bits per second.
--
-- /Note:/ Consider using 'averageUploadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlAverageUploadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Lude.Maybe Lude.Natural)
ubrlAverageUploadRateLimitInBitsPerSec = Lens.lens (averageUploadRateLimitInBitsPerSec :: UpdateBandwidthRateLimit -> Lude.Maybe Lude.Natural) (\s a -> s {averageUploadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)
{-# DEPRECATED ubrlAverageUploadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageUploadRateLimitInBitsPerSec' instead." #-}

-- | The average download bandwidth rate limit in bits per second.
--
-- /Note:/ Consider using 'averageDownloadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlAverageDownloadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Lude.Maybe Lude.Natural)
ubrlAverageDownloadRateLimitInBitsPerSec = Lens.lens (averageDownloadRateLimitInBitsPerSec :: UpdateBandwidthRateLimit -> Lude.Maybe Lude.Natural) (\s a -> s {averageDownloadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)
{-# DEPRECATED ubrlAverageDownloadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageDownloadRateLimitInBitsPerSec' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlGatewayARN :: Lens.Lens' UpdateBandwidthRateLimit Lude.Text
ubrlGatewayARN = Lens.lens (gatewayARN :: UpdateBandwidthRateLimit -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateBandwidthRateLimit)
{-# DEPRECATED ubrlGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest UpdateBandwidthRateLimit where
  type Rs UpdateBandwidthRateLimit = UpdateBandwidthRateLimitResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBandwidthRateLimit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBandwidthRateLimit where
  toJSON UpdateBandwidthRateLimit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AverageUploadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageUploadRateLimitInBitsPerSec,
            ("AverageDownloadRateLimitInBitsPerSec" Lude..=)
              Lude.<$> averageDownloadRateLimitInBitsPerSec,
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath UpdateBandwidthRateLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBandwidthRateLimit where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose throttle information was updated.
--
-- /See:/ 'mkUpdateBandwidthRateLimitResponse' smart constructor.
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
  { gatewayARN ::
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

-- | Creates a value of 'UpdateBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateBandwidthRateLimitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBandwidthRateLimitResponse
mkUpdateBandwidthRateLimitResponse pResponseStatus_ =
  UpdateBandwidthRateLimitResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlrsGatewayARN :: Lens.Lens' UpdateBandwidthRateLimitResponse (Lude.Maybe Lude.Text)
ubrlrsGatewayARN = Lens.lens (gatewayARN :: UpdateBandwidthRateLimitResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitResponse)
{-# DEPRECATED ubrlrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrlrsResponseStatus :: Lens.Lens' UpdateBandwidthRateLimitResponse Lude.Int
ubrlrsResponseStatus = Lens.lens (responseStatus :: UpdateBandwidthRateLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBandwidthRateLimitResponse)
{-# DEPRECATED ubrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

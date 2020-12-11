{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeBandwidthRateLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the bandwidth rate limits of a gateway. By default, these limits are not set, which means no bandwidth rate limiting is in effect. This operation is supported for the stored volume, cached volume, and tape gateway types.
--
-- This operation only returns a value for a bandwidth rate limit only if the limit is set. If no limits are set for the gateway, then this operation returns only the gateway ARN in the response body. To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeBandwidthRateLimit
  ( -- * Creating a request
    DescribeBandwidthRateLimit (..),
    mkDescribeBandwidthRateLimit,

    -- ** Request lenses
    dbrlGatewayARN,

    -- * Destructuring the response
    DescribeBandwidthRateLimitResponse (..),
    mkDescribeBandwidthRateLimitResponse,

    -- ** Response lenses
    dbrlrsGatewayARN,
    dbrlrsAverageUploadRateLimitInBitsPerSec,
    dbrlrsAverageDownloadRateLimitInBitsPerSec,
    dbrlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkDescribeBandwidthRateLimit' smart constructor.
newtype DescribeBandwidthRateLimit = DescribeBandwidthRateLimit'
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

-- | Creates a value of 'DescribeBandwidthRateLimit' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkDescribeBandwidthRateLimit ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeBandwidthRateLimit
mkDescribeBandwidthRateLimit pGatewayARN_ =
  DescribeBandwidthRateLimit' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlGatewayARN :: Lens.Lens' DescribeBandwidthRateLimit Lude.Text
dbrlGatewayARN = Lens.lens (gatewayARN :: DescribeBandwidthRateLimit -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeBandwidthRateLimit)
{-# DEPRECATED dbrlGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeBandwidthRateLimit where
  type
    Rs DescribeBandwidthRateLimit =
      DescribeBandwidthRateLimitResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "AverageUploadRateLimitInBitsPerSec")
            Lude.<*> (x Lude..?> "AverageDownloadRateLimitInBitsPerSec")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBandwidthRateLimit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeBandwidthRateLimit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBandwidthRateLimit where
  toJSON DescribeBandwidthRateLimit' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeBandwidthRateLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBandwidthRateLimit where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeBandwidthRateLimitResponse' smart constructor.
data DescribeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
    averageUploadRateLimitInBitsPerSec ::
      Lude.Maybe
        Lude.Natural,
    averageDownloadRateLimitInBitsPerSec ::
      Lude.Maybe
        Lude.Natural,
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

-- | Creates a value of 'DescribeBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- * 'averageDownloadRateLimitInBitsPerSec' - The average download bandwidth rate limit in bits per second. This field does not appear in the response if the download rate limit is not set.
-- * 'averageUploadRateLimitInBitsPerSec' - The average upload bandwidth rate limit in bits per second. This field does not appear in the response if the upload rate limit is not set.
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeBandwidthRateLimitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBandwidthRateLimitResponse
mkDescribeBandwidthRateLimitResponse pResponseStatus_ =
  DescribeBandwidthRateLimitResponse'
    { gatewayARN = Lude.Nothing,
      averageUploadRateLimitInBitsPerSec = Lude.Nothing,
      averageDownloadRateLimitInBitsPerSec = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitResponse (Lude.Maybe Lude.Text)
dbrlrsGatewayARN = Lens.lens (gatewayARN :: DescribeBandwidthRateLimitResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitResponse)
{-# DEPRECATED dbrlrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The average upload bandwidth rate limit in bits per second. This field does not appear in the response if the upload rate limit is not set.
--
-- /Note:/ Consider using 'averageUploadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrsAverageUploadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Lude.Maybe Lude.Natural)
dbrlrsAverageUploadRateLimitInBitsPerSec = Lens.lens (averageUploadRateLimitInBitsPerSec :: DescribeBandwidthRateLimitResponse -> Lude.Maybe Lude.Natural) (\s a -> s {averageUploadRateLimitInBitsPerSec = a} :: DescribeBandwidthRateLimitResponse)
{-# DEPRECATED dbrlrsAverageUploadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageUploadRateLimitInBitsPerSec' instead." #-}

-- | The average download bandwidth rate limit in bits per second. This field does not appear in the response if the download rate limit is not set.
--
-- /Note:/ Consider using 'averageDownloadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrsAverageDownloadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Lude.Maybe Lude.Natural)
dbrlrsAverageDownloadRateLimitInBitsPerSec = Lens.lens (averageDownloadRateLimitInBitsPerSec :: DescribeBandwidthRateLimitResponse -> Lude.Maybe Lude.Natural) (\s a -> s {averageDownloadRateLimitInBitsPerSec = a} :: DescribeBandwidthRateLimitResponse)
{-# DEPRECATED dbrlrsAverageDownloadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageDownloadRateLimitInBitsPerSec' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrsResponseStatus :: Lens.Lens' DescribeBandwidthRateLimitResponse Lude.Int
dbrlrsResponseStatus = Lens.lens (responseStatus :: DescribeBandwidthRateLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBandwidthRateLimitResponse)
{-# DEPRECATED dbrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

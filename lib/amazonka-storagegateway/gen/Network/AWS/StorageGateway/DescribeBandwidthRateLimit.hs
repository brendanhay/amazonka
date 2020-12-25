{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dbrlrrsAverageDownloadRateLimitInBitsPerSec,
    dbrlrrsAverageUploadRateLimitInBitsPerSec,
    dbrlrrsGatewayARN,
    dbrlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkDescribeBandwidthRateLimit' smart constructor.
newtype DescribeBandwidthRateLimit = DescribeBandwidthRateLimit'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBandwidthRateLimit' value with any optional fields omitted.
mkDescribeBandwidthRateLimit ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeBandwidthRateLimit
mkDescribeBandwidthRateLimit gatewayARN =
  DescribeBandwidthRateLimit' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlGatewayARN :: Lens.Lens' DescribeBandwidthRateLimit Types.GatewayARN
dbrlGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dbrlGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DescribeBandwidthRateLimit where
  toJSON DescribeBandwidthRateLimit {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeBandwidthRateLimit where
  type
    Rs DescribeBandwidthRateLimit =
      DescribeBandwidthRateLimitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.DescribeBandwidthRateLimit"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitResponse'
            Core.<$> (x Core..:? "AverageDownloadRateLimitInBitsPerSec")
            Core.<*> (x Core..:? "AverageUploadRateLimitInBitsPerSec")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeBandwidthRateLimitResponse' smart constructor.
data DescribeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse'
  { -- | The average download bandwidth rate limit in bits per second. This field does not appear in the response if the download rate limit is not set.
    averageDownloadRateLimitInBitsPerSec :: Core.Maybe Core.Natural,
    -- | The average upload bandwidth rate limit in bits per second. This field does not appear in the response if the upload rate limit is not set.
    averageUploadRateLimitInBitsPerSec :: Core.Maybe Core.Natural,
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBandwidthRateLimitResponse' value with any optional fields omitted.
mkDescribeBandwidthRateLimitResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBandwidthRateLimitResponse
mkDescribeBandwidthRateLimitResponse responseStatus =
  DescribeBandwidthRateLimitResponse'
    { averageDownloadRateLimitInBitsPerSec =
        Core.Nothing,
      averageUploadRateLimitInBitsPerSec = Core.Nothing,
      gatewayARN = Core.Nothing,
      responseStatus
    }

-- | The average download bandwidth rate limit in bits per second. This field does not appear in the response if the download rate limit is not set.
--
-- /Note:/ Consider using 'averageDownloadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrrsAverageDownloadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Core.Maybe Core.Natural)
dbrlrrsAverageDownloadRateLimitInBitsPerSec = Lens.field @"averageDownloadRateLimitInBitsPerSec"
{-# DEPRECATED dbrlrrsAverageDownloadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageDownloadRateLimitInBitsPerSec' instead." #-}

-- | The average upload bandwidth rate limit in bits per second. This field does not appear in the response if the upload rate limit is not set.
--
-- /Note:/ Consider using 'averageUploadRateLimitInBitsPerSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrrsAverageUploadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Core.Maybe Core.Natural)
dbrlrrsAverageUploadRateLimitInBitsPerSec = Lens.field @"averageUploadRateLimitInBitsPerSec"
{-# DEPRECATED dbrlrrsAverageUploadRateLimitInBitsPerSec "Use generic-lens or generic-optics with 'averageUploadRateLimitInBitsPerSec' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrrsGatewayARN :: Lens.Lens' DescribeBandwidthRateLimitResponse (Core.Maybe Types.GatewayARN)
dbrlrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dbrlrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlrrsResponseStatus :: Lens.Lens' DescribeBandwidthRateLimitResponse Core.Int
dbrlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

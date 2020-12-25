{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RetrieveTapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an archived virtual tape from the virtual tape shelf (VTS) to a tape gateway. Virtual tapes archived in the VTS are not associated with any gateway. However after a tape is retrieved, it is associated with a gateway, even though it is also listed in the VTS, that is, archive. This operation is only supported in the tape gateway type.
--
-- Once a tape is successfully retrieved to a gateway, it cannot be retrieved again to another gateway. You must archive the tape again before you can retrieve it to another gateway. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.RetrieveTapeArchive
  ( -- * Creating a request
    RetrieveTapeArchive (..),
    mkRetrieveTapeArchive,

    -- ** Request lenses
    rtaTapeARN,
    rtaGatewayARN,

    -- * Destructuring the response
    RetrieveTapeArchiveResponse (..),
    mkRetrieveTapeArchiveResponse,

    -- ** Response lenses
    rtarrsTapeARN,
    rtarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | RetrieveTapeArchiveInput
--
-- /See:/ 'mkRetrieveTapeArchive' smart constructor.
data RetrieveTapeArchive = RetrieveTapeArchive'
  { -- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve from the virtual tape shelf (VTS).
    tapeARN :: Types.TapeARN,
    -- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the virtual tape to. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    --
    -- You retrieve archived virtual tapes to only one gateway and the gateway must be a tape gateway.
    gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveTapeArchive' value with any optional fields omitted.
mkRetrieveTapeArchive ::
  -- | 'tapeARN'
  Types.TapeARN ->
  -- | 'gatewayARN'
  Types.GatewayARN ->
  RetrieveTapeArchive
mkRetrieveTapeArchive tapeARN gatewayARN =
  RetrieveTapeArchive' {tapeARN, gatewayARN}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaTapeARN :: Lens.Lens' RetrieveTapeArchive Types.TapeARN
rtaTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED rtaTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the virtual tape to. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway must be a tape gateway.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaGatewayARN :: Lens.Lens' RetrieveTapeArchive Types.GatewayARN
rtaGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED rtaGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON RetrieveTapeArchive where
  toJSON RetrieveTapeArchive {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TapeARN" Core..= tapeARN),
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.AWSRequest RetrieveTapeArchive where
  type Rs RetrieveTapeArchive = RetrieveTapeArchiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.RetrieveTapeArchive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveTapeArchiveResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | RetrieveTapeArchiveOutput
--
-- /See:/ 'mkRetrieveTapeArchiveResponse' smart constructor.
data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetrieveTapeArchiveResponse' value with any optional fields omitted.
mkRetrieveTapeArchiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RetrieveTapeArchiveResponse
mkRetrieveTapeArchiveResponse responseStatus =
  RetrieveTapeArchiveResponse'
    { tapeARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarrsTapeARN :: Lens.Lens' RetrieveTapeArchiveResponse (Core.Maybe Types.TapeARN)
rtarrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED rtarrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarrsResponseStatus :: Lens.Lens' RetrieveTapeArchiveResponse Core.Int
rtarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

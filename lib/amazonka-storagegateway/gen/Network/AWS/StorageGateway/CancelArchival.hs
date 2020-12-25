{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelArchival
  ( -- * Creating a request
    CancelArchival (..),
    mkCancelArchival,

    -- ** Request lenses
    caGatewayARN,
    caTapeARN,

    -- * Destructuring the response
    CancelArchivalResponse (..),
    mkCancelArchivalResponse,

    -- ** Response lenses
    carrsTapeARN,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CancelArchivalInput
--
-- /See:/ 'mkCancelArchival' smart constructor.
data CancelArchival = CancelArchival'
  { gatewayARN :: Types.GatewayARN,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
    tapeARN :: Types.TapeARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelArchival' value with any optional fields omitted.
mkCancelArchival ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'tapeARN'
  Types.TapeARN ->
  CancelArchival
mkCancelArchival gatewayARN tapeARN =
  CancelArchival' {gatewayARN, tapeARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caGatewayARN :: Lens.Lens' CancelArchival Types.GatewayARN
caGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED caGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTapeARN :: Lens.Lens' CancelArchival Types.TapeARN
caTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED caTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

instance Core.FromJSON CancelArchival where
  toJSON CancelArchival {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.AWSRequest CancelArchival where
  type Rs CancelArchival = CancelArchivalResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CancelArchival")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelArchivalResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | CancelArchivalOutput
--
-- /See:/ 'mkCancelArchivalResponse' smart constructor.
data CancelArchivalResponse = CancelArchivalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelArchivalResponse' value with any optional fields omitted.
mkCancelArchivalResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelArchivalResponse
mkCancelArchivalResponse responseStatus =
  CancelArchivalResponse' {tapeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsTapeARN :: Lens.Lens' CancelArchivalResponse (Core.Maybe Types.TapeARN)
carrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED carrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CancelArchivalResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

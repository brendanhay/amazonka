{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelRetrieval
  ( -- * Creating a request
    CancelRetrieval (..),
    mkCancelRetrieval,

    -- ** Request lenses
    crGatewayARN,
    crTapeARN,

    -- * Destructuring the response
    CancelRetrievalResponse (..),
    mkCancelRetrievalResponse,

    -- ** Response lenses
    crrrsTapeARN,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CancelRetrievalInput
--
-- /See:/ 'mkCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { gatewayARN :: Types.GatewayARN,
    -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
    tapeARN :: Types.TapeARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRetrieval' value with any optional fields omitted.
mkCancelRetrieval ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'tapeARN'
  Types.TapeARN ->
  CancelRetrieval
mkCancelRetrieval gatewayARN tapeARN =
  CancelRetrieval' {gatewayARN, tapeARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayARN :: Lens.Lens' CancelRetrieval Types.GatewayARN
crGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED crGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTapeARN :: Lens.Lens' CancelRetrieval Types.TapeARN
crTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED crTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

instance Core.FromJSON CancelRetrieval where
  toJSON CancelRetrieval {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN)
          ]
      )

instance Core.AWSRequest CancelRetrieval where
  type Rs CancelRetrieval = CancelRetrievalResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CancelRetrieval")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRetrievalResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | CancelRetrievalOutput
--
-- /See:/ 'mkCancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRetrievalResponse' value with any optional fields omitted.
mkCancelRetrievalResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelRetrievalResponse
mkCancelRetrievalResponse responseStatus =
  CancelRetrievalResponse' {tapeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsTapeARN :: Lens.Lens' CancelRetrievalResponse (Core.Maybe Types.TapeARN)
crrrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED crrrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CancelRetrievalResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

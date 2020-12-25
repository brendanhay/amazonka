{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTape
  ( -- * Creating a request
    DeleteTape (..),
    mkDeleteTape,

    -- ** Request lenses
    dtfGatewayARN,
    dtfTapeARN,
    dtfBypassGovernanceRetention,

    -- * Destructuring the response
    DeleteTapeResponse (..),
    mkDeleteTapeResponse,

    -- ** Response lenses
    dtrfrsTapeARN,
    dtrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DeleteTapeInput
--
-- /See:/ 'mkDeleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { -- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Types.GatewayARN,
    -- | The Amazon Resource Name (ARN) of the virtual tape to delete.
    tapeARN :: Types.TapeARN,
    -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
    bypassGovernanceRetention :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTape' value with any optional fields omitted.
mkDeleteTape ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'tapeARN'
  Types.TapeARN ->
  DeleteTape
mkDeleteTape gatewayARN tapeARN =
  DeleteTape'
    { gatewayARN,
      tapeARN,
      bypassGovernanceRetention = Core.Nothing
    }

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfGatewayARN :: Lens.Lens' DeleteTape Types.GatewayARN
dtfGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dtfGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfTapeARN :: Lens.Lens' DeleteTape Types.TapeARN
dtfTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED dtfTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfBypassGovernanceRetention :: Lens.Lens' DeleteTape (Core.Maybe Core.Bool)
dtfBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# DEPRECATED dtfBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

instance Core.FromJSON DeleteTape where
  toJSON DeleteTape {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("TapeARN" Core..= tapeARN),
            ("BypassGovernanceRetention" Core..=)
              Core.<$> bypassGovernanceRetention
          ]
      )

instance Core.AWSRequest DeleteTape where
  type Rs DeleteTape = DeleteTapeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.DeleteTape")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapeResponse'
            Core.<$> (x Core..:? "TapeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | DeleteTapeOutput
--
-- /See:/ 'mkDeleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted virtual tape.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapeResponse' value with any optional fields omitted.
mkDeleteTapeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTapeResponse
mkDeleteTapeResponse responseStatus =
  DeleteTapeResponse' {tapeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsTapeARN :: Lens.Lens' DeleteTapeResponse (Core.Maybe Types.TapeARN)
dtrfrsTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED dtrfrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeleteTapeResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

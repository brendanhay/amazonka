{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Controls whether the shares on a gateway are visible in a net view or browse list.
module Network.AWS.StorageGateway.UpdateSMBFileShareVisibility
  ( -- * Creating a request
    UpdateSMBFileShareVisibility (..),
    mkUpdateSMBFileShareVisibility,

    -- ** Request lenses
    usmbfsvGatewayARN,
    usmbfsvFileSharesVisible,

    -- * Destructuring the response
    UpdateSMBFileShareVisibilityResponse (..),
    mkUpdateSMBFileShareVisibilityResponse,

    -- ** Response lenses
    usmbfsvrrsGatewayARN,
    usmbfsvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateSMBFileShareVisibility' smart constructor.
data UpdateSMBFileShareVisibility = UpdateSMBFileShareVisibility'
  { gatewayARN :: Types.GatewayARN,
    -- | The shares on this gateway appear when listing shares.
    fileSharesVisible :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShareVisibility' value with any optional fields omitted.
mkUpdateSMBFileShareVisibility ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'fileSharesVisible'
  Core.Bool ->
  UpdateSMBFileShareVisibility
mkUpdateSMBFileShareVisibility gatewayARN fileSharesVisible =
  UpdateSMBFileShareVisibility' {gatewayARN, fileSharesVisible}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvGatewayARN :: Lens.Lens' UpdateSMBFileShareVisibility Types.GatewayARN
usmbfsvGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED usmbfsvGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The shares on this gateway appear when listing shares.
--
-- /Note:/ Consider using 'fileSharesVisible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvFileSharesVisible :: Lens.Lens' UpdateSMBFileShareVisibility Core.Bool
usmbfsvFileSharesVisible = Lens.field @"fileSharesVisible"
{-# DEPRECATED usmbfsvFileSharesVisible "Use generic-lens or generic-optics with 'fileSharesVisible' instead." #-}

instance Core.FromJSON UpdateSMBFileShareVisibility where
  toJSON UpdateSMBFileShareVisibility {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("FileSharesVisible" Core..= fileSharesVisible)
          ]
      )

instance Core.AWSRequest UpdateSMBFileShareVisibility where
  type
    Rs UpdateSMBFileShareVisibility =
      UpdateSMBFileShareVisibilityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateSMBFileShareVisibility"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareVisibilityResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSMBFileShareVisibilityResponse' smart constructor.
data UpdateSMBFileShareVisibilityResponse = UpdateSMBFileShareVisibilityResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShareVisibilityResponse' value with any optional fields omitted.
mkUpdateSMBFileShareVisibilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSMBFileShareVisibilityResponse
mkUpdateSMBFileShareVisibilityResponse responseStatus =
  UpdateSMBFileShareVisibilityResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvrrsGatewayARN :: Lens.Lens' UpdateSMBFileShareVisibilityResponse (Core.Maybe Types.GatewayARN)
usmbfsvrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED usmbfsvrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsvrrsResponseStatus :: Lens.Lens' UpdateSMBFileShareVisibilityResponse Core.Int
usmbfsvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usmbfsvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

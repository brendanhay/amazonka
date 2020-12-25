{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the attributes of the specified link aggregation group (LAG).
--
-- You can update the following attributes:
--
--     * The name of the LAG.
--
--
--     * The value for the minimum number of connections that must be operational for the LAG itself to be operational.
--
--
-- When you create a LAG, the default value for the minimum number of operational connections is zero (0). If you update this value and the number of operational connections falls below the specified value, the LAG automatically goes down to avoid over-utilization of the remaining connections. Adjust this value with care, as it could force the LAG down if it is set higher than the current number of operational connections.
module Network.AWS.DirectConnect.UpdateLag
  ( -- * Creating a request
    UpdateLag (..),
    mkUpdateLag,

    -- ** Request lenses
    ulLagId,
    ulLagName,
    ulMinimumLinks,

    -- * Destructuring the response
    Types.Lag (..),
    Types.mkLag,

    -- ** Response lenses
    Types.lfAllowsHostedConnections,
    Types.lfAwsDevice,
    Types.lfAwsDeviceV2,
    Types.lfConnections,
    Types.lfConnectionsBandwidth,
    Types.lfHasLogicalRedundancy,
    Types.lfJumboFrameCapable,
    Types.lfLagId,
    Types.lfLagName,
    Types.lfLagState,
    Types.lfLocation,
    Types.lfMinimumLinks,
    Types.lfNumberOfConnections,
    Types.lfOwnerAccount,
    Types.lfProviderName,
    Types.lfRegion,
    Types.lfTags,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLag' smart constructor.
data UpdateLag = UpdateLag'
  { -- | The ID of the LAG.
    lagId :: Types.LagId,
    -- | The name of the LAG.
    lagName :: Core.Maybe Types.LagName,
    -- | The minimum number of physical connections that must be operational for the LAG itself to be operational.
    minimumLinks :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLag' value with any optional fields omitted.
mkUpdateLag ::
  -- | 'lagId'
  Types.LagId ->
  UpdateLag
mkUpdateLag lagId =
  UpdateLag'
    { lagId,
      lagName = Core.Nothing,
      minimumLinks = Core.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLagId :: Lens.Lens' UpdateLag Types.LagId
ulLagId = Lens.field @"lagId"
{-# DEPRECATED ulLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulLagName :: Lens.Lens' UpdateLag (Core.Maybe Types.LagName)
ulLagName = Lens.field @"lagName"
{-# DEPRECATED ulLagName "Use generic-lens or generic-optics with 'lagName' instead." #-}

-- | The minimum number of physical connections that must be operational for the LAG itself to be operational.
--
-- /Note:/ Consider using 'minimumLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulMinimumLinks :: Lens.Lens' UpdateLag (Core.Maybe Core.Int)
ulMinimumLinks = Lens.field @"minimumLinks"
{-# DEPRECATED ulMinimumLinks "Use generic-lens or generic-optics with 'minimumLinks' instead." #-}

instance Core.FromJSON UpdateLag where
  toJSON UpdateLag {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("lagId" Core..= lagId),
            ("lagName" Core..=) Core.<$> lagName,
            ("minimumLinks" Core..=) Core.<$> minimumLinks
          ]
      )

instance Core.AWSRequest UpdateLag where
  type Rs UpdateLag = Types.Lag
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.UpdateLag")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

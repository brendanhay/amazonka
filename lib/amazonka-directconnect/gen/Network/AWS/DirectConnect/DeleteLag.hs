{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.
module Network.AWS.DirectConnect.DeleteLag
  ( -- * Creating a request
    DeleteLag (..),
    mkDeleteLag,

    -- ** Request lenses
    dLagId,

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

-- | /See:/ 'mkDeleteLag' smart constructor.
newtype DeleteLag = DeleteLag'
  { -- | The ID of the LAG.
    lagId :: Types.LagId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLag' value with any optional fields omitted.
mkDeleteLag ::
  -- | 'lagId'
  Types.LagId ->
  DeleteLag
mkDeleteLag lagId = DeleteLag' {lagId}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLagId :: Lens.Lens' DeleteLag Types.LagId
dLagId = Lens.field @"lagId"
{-# DEPRECATED dLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

instance Core.FromJSON DeleteLag where
  toJSON DeleteLag {..} =
    Core.object (Core.catMaybes [Core.Just ("lagId" Core..= lagId)])

instance Core.AWSRequest DeleteLag where
  type Rs DeleteLag = Types.Lag
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DeleteLag")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

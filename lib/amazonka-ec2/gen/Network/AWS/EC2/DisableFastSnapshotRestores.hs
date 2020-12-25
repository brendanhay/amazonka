{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables fast snapshot restores for the specified snapshots in the specified Availability Zones.
module Network.AWS.EC2.DisableFastSnapshotRestores
  ( -- * Creating a request
    DisableFastSnapshotRestores (..),
    mkDisableFastSnapshotRestores,

    -- ** Request lenses
    dfsrsAvailabilityZones,
    dfsrsSourceSnapshotIds,
    dfsrsDryRun,

    -- * Destructuring the response
    DisableFastSnapshotRestoresResponse (..),
    mkDisableFastSnapshotRestoresResponse,

    -- ** Response lenses
    dfsrrfrsSuccessful,
    dfsrrfrsUnsuccessful,
    dfsrrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableFastSnapshotRestores' smart constructor.
data DisableFastSnapshotRestores = DisableFastSnapshotRestores'
  { -- | One or more Availability Zones. For example, @us-east-2a@ .
    availabilityZones :: [Types.String],
    -- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
    sourceSnapshotIds :: [Types.SnapshotId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableFastSnapshotRestores' value with any optional fields omitted.
mkDisableFastSnapshotRestores ::
  DisableFastSnapshotRestores
mkDisableFastSnapshotRestores =
  DisableFastSnapshotRestores'
    { availabilityZones = Core.mempty,
      sourceSnapshotIds = Core.mempty,
      dryRun = Core.Nothing
    }

-- | One or more Availability Zones. For example, @us-east-2a@ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsAvailabilityZones :: Lens.Lens' DisableFastSnapshotRestores [Types.String]
dfsrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dfsrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'sourceSnapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsSourceSnapshotIds :: Lens.Lens' DisableFastSnapshotRestores [Types.SnapshotId]
dfsrsSourceSnapshotIds = Lens.field @"sourceSnapshotIds"
{-# DEPRECATED dfsrsSourceSnapshotIds "Use generic-lens or generic-optics with 'sourceSnapshotIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsDryRun :: Lens.Lens' DisableFastSnapshotRestores (Core.Maybe Core.Bool)
dfsrsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dfsrsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DisableFastSnapshotRestores where
  type
    Rs DisableFastSnapshotRestores =
      DisableFastSnapshotRestoresResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DisableFastSnapshotRestores")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "AvailabilityZone" availabilityZones)
                Core.<> (Core.toQueryList "SourceSnapshotId" sourceSnapshotIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DisableFastSnapshotRestoresResponse'
            Core.<$> (x Core..@? "successful" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { -- | Information about the snapshots for which fast snapshot restores were successfully disabled.
    successful :: Core.Maybe [Types.DisableFastSnapshotRestoreSuccessItem],
    -- | Information about the snapshots for which fast snapshot restores could not be disabled.
    unsuccessful :: Core.Maybe [Types.DisableFastSnapshotRestoreErrorItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DisableFastSnapshotRestoresResponse' value with any optional fields omitted.
mkDisableFastSnapshotRestoresResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableFastSnapshotRestoresResponse
mkDisableFastSnapshotRestoresResponse responseStatus =
  DisableFastSnapshotRestoresResponse'
    { successful = Core.Nothing,
      unsuccessful = Core.Nothing,
      responseStatus
    }

-- | Information about the snapshots for which fast snapshot restores were successfully disabled.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsSuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Core.Maybe [Types.DisableFastSnapshotRestoreSuccessItem])
dfsrrfrsSuccessful = Lens.field @"successful"
{-# DEPRECATED dfsrrfrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | Information about the snapshots for which fast snapshot restores could not be disabled.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsUnsuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Core.Maybe [Types.DisableFastSnapshotRestoreErrorItem])
dfsrrfrsUnsuccessful = Lens.field @"unsuccessful"
{-# DEPRECATED dfsrrfrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsResponseStatus :: Lens.Lens' DisableFastSnapshotRestoresResponse Core.Int
dfsrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfsrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

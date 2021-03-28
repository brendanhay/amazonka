{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisableFastSnapshotRestores (..)
    , mkDisableFastSnapshotRestores
    -- ** Request lenses
    , dfsrsAvailabilityZones
    , dfsrsSourceSnapshotIds
    , dfsrsDryRun

    -- * Destructuring the response
    , DisableFastSnapshotRestoresResponse (..)
    , mkDisableFastSnapshotRestoresResponse
    -- ** Response lenses
    , dfsrrfrsSuccessful
    , dfsrrfrsUnsuccessful
    , dfsrrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableFastSnapshotRestores' smart constructor.
data DisableFastSnapshotRestores = DisableFastSnapshotRestores'
  { availabilityZones :: [Core.Text]
    -- ^ One or more Availability Zones. For example, @us-east-2a@ .
  , sourceSnapshotIds :: [Types.SnapshotId]
    -- ^ The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableFastSnapshotRestores' value with any optional fields omitted.
mkDisableFastSnapshotRestores
    :: DisableFastSnapshotRestores
mkDisableFastSnapshotRestores
  = DisableFastSnapshotRestores'{availabilityZones = Core.mempty,
                                 sourceSnapshotIds = Core.mempty, dryRun = Core.Nothing}

-- | One or more Availability Zones. For example, @us-east-2a@ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsAvailabilityZones :: Lens.Lens' DisableFastSnapshotRestores [Core.Text]
dfsrsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dfsrsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
--
-- /Note:/ Consider using 'sourceSnapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsSourceSnapshotIds :: Lens.Lens' DisableFastSnapshotRestores [Types.SnapshotId]
dfsrsSourceSnapshotIds = Lens.field @"sourceSnapshotIds"
{-# INLINEABLE dfsrsSourceSnapshotIds #-}
{-# DEPRECATED sourceSnapshotIds "Use generic-lens or generic-optics with 'sourceSnapshotIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsDryRun :: Lens.Lens' DisableFastSnapshotRestores (Core.Maybe Core.Bool)
dfsrsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dfsrsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisableFastSnapshotRestores where
        toQuery DisableFastSnapshotRestores{..}
          = Core.toQueryPair "Action"
              ("DisableFastSnapshotRestores" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "AvailabilityZone" availabilityZones
              Core.<> Core.toQueryList "SourceSnapshotId" sourceSnapshotIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisableFastSnapshotRestores where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableFastSnapshotRestores where
        type Rs DisableFastSnapshotRestores =
             DisableFastSnapshotRestoresResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DisableFastSnapshotRestoresResponse' Core.<$>
                   (x Core..@? "successful" Core..<@> Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { successful :: Core.Maybe [Types.DisableFastSnapshotRestoreSuccessItem]
    -- ^ Information about the snapshots for which fast snapshot restores were successfully disabled.
  , unsuccessful :: Core.Maybe [Types.DisableFastSnapshotRestoreErrorItem]
    -- ^ Information about the snapshots for which fast snapshot restores could not be disabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DisableFastSnapshotRestoresResponse' value with any optional fields omitted.
mkDisableFastSnapshotRestoresResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableFastSnapshotRestoresResponse
mkDisableFastSnapshotRestoresResponse responseStatus
  = DisableFastSnapshotRestoresResponse'{successful = Core.Nothing,
                                         unsuccessful = Core.Nothing, responseStatus}

-- | Information about the snapshots for which fast snapshot restores were successfully disabled.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsSuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Core.Maybe [Types.DisableFastSnapshotRestoreSuccessItem])
dfsrrfrsSuccessful = Lens.field @"successful"
{-# INLINEABLE dfsrrfrsSuccessful #-}
{-# DEPRECATED successful "Use generic-lens or generic-optics with 'successful' instead"  #-}

-- | Information about the snapshots for which fast snapshot restores could not be disabled.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsUnsuccessful :: Lens.Lens' DisableFastSnapshotRestoresResponse (Core.Maybe [Types.DisableFastSnapshotRestoreErrorItem])
dfsrrfrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE dfsrrfrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrrfrsResponseStatus :: Lens.Lens' DisableFastSnapshotRestoresResponse Core.Int
dfsrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfsrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

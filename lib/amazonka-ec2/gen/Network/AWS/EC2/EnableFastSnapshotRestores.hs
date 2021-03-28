{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables fast snapshot restores for the specified snapshots in the specified Availability Zones.
--
-- You get the full benefit of fast snapshot restores after they enter the @enabled@ state. To get the current state of fast snapshot restores, use 'DescribeFastSnapshotRestores' . To disable fast snapshot restores, use 'DisableFastSnapshotRestores' .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-fast-snapshot-restore.html Amazon EBS fast snapshot restore> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.EnableFastSnapshotRestores
    (
    -- * Creating a request
      EnableFastSnapshotRestores (..)
    , mkEnableFastSnapshotRestores
    -- ** Request lenses
    , efsrAvailabilityZones
    , efsrSourceSnapshotIds
    , efsrDryRun

    -- * Destructuring the response
    , EnableFastSnapshotRestoresResponse (..)
    , mkEnableFastSnapshotRestoresResponse
    -- ** Response lenses
    , efsrrrsSuccessful
    , efsrrrsUnsuccessful
    , efsrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableFastSnapshotRestores' smart constructor.
data EnableFastSnapshotRestores = EnableFastSnapshotRestores'
  { availabilityZones :: [Core.Text]
    -- ^ One or more Availability Zones. For example, @us-east-2a@ .
  , sourceSnapshotIds :: [Types.SnapshotId]
    -- ^ The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableFastSnapshotRestores' value with any optional fields omitted.
mkEnableFastSnapshotRestores
    :: EnableFastSnapshotRestores
mkEnableFastSnapshotRestores
  = EnableFastSnapshotRestores'{availabilityZones = Core.mempty,
                                sourceSnapshotIds = Core.mempty, dryRun = Core.Nothing}

-- | One or more Availability Zones. For example, @us-east-2a@ .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrAvailabilityZones :: Lens.Lens' EnableFastSnapshotRestores [Core.Text]
efsrAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE efsrAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
--
-- /Note:/ Consider using 'sourceSnapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrSourceSnapshotIds :: Lens.Lens' EnableFastSnapshotRestores [Types.SnapshotId]
efsrSourceSnapshotIds = Lens.field @"sourceSnapshotIds"
{-# INLINEABLE efsrSourceSnapshotIds #-}
{-# DEPRECATED sourceSnapshotIds "Use generic-lens or generic-optics with 'sourceSnapshotIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrDryRun :: Lens.Lens' EnableFastSnapshotRestores (Core.Maybe Core.Bool)
efsrDryRun = Lens.field @"dryRun"
{-# INLINEABLE efsrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery EnableFastSnapshotRestores where
        toQuery EnableFastSnapshotRestores{..}
          = Core.toQueryPair "Action"
              ("EnableFastSnapshotRestores" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "AvailabilityZone" availabilityZones
              Core.<> Core.toQueryList "SourceSnapshotId" sourceSnapshotIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders EnableFastSnapshotRestores where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableFastSnapshotRestores where
        type Rs EnableFastSnapshotRestores =
             EnableFastSnapshotRestoresResponse
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
                 EnableFastSnapshotRestoresResponse' Core.<$>
                   (x Core..@? "successful" Core..<@> Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableFastSnapshotRestoresResponse' smart constructor.
data EnableFastSnapshotRestoresResponse = EnableFastSnapshotRestoresResponse'
  { successful :: Core.Maybe [Types.EnableFastSnapshotRestoreSuccessItem]
    -- ^ Information about the snapshots for which fast snapshot restores were successfully enabled.
  , unsuccessful :: Core.Maybe [Types.EnableFastSnapshotRestoreErrorItem]
    -- ^ Information about the snapshots for which fast snapshot restores could not be enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EnableFastSnapshotRestoresResponse' value with any optional fields omitted.
mkEnableFastSnapshotRestoresResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableFastSnapshotRestoresResponse
mkEnableFastSnapshotRestoresResponse responseStatus
  = EnableFastSnapshotRestoresResponse'{successful = Core.Nothing,
                                        unsuccessful = Core.Nothing, responseStatus}

-- | Information about the snapshots for which fast snapshot restores were successfully enabled.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrrsSuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Core.Maybe [Types.EnableFastSnapshotRestoreSuccessItem])
efsrrrsSuccessful = Lens.field @"successful"
{-# INLINEABLE efsrrrsSuccessful #-}
{-# DEPRECATED successful "Use generic-lens or generic-optics with 'successful' instead"  #-}

-- | Information about the snapshots for which fast snapshot restores could not be enabled.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrrsUnsuccessful :: Lens.Lens' EnableFastSnapshotRestoresResponse (Core.Maybe [Types.EnableFastSnapshotRestoreErrorItem])
efsrrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE efsrrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrrrsResponseStatus :: Lens.Lens' EnableFastSnapshotRestoresResponse Core.Int
efsrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE efsrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates crash-consistent snapshots of multiple EBS volumes and stores the data in S3. Volumes are chosen by specifying an instance. Any attached volumes will produce one snapshot each that is crash-consistent across the instance. Boot volumes can be excluded by changing the parameters. 
module Network.AWS.EC2.CreateSnapshots
    (
    -- * Creating a request
      CreateSnapshots (..)
    , mkCreateSnapshots
    -- ** Request lenses
    , csInstanceSpecification
    , csCopyTagsFromSource
    , csDescription
    , csDryRun
    , csTagSpecifications

    -- * Destructuring the response
    , CreateSnapshotsResponse (..)
    , mkCreateSnapshotsResponse
    -- ** Response lenses
    , csrrsSnapshots
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSnapshots' smart constructor.
data CreateSnapshots = CreateSnapshots'
  { instanceSpecification :: Types.InstanceSpecification
    -- ^ The instance to specify which volumes should be included in the snapshots.
  , copyTagsFromSource :: Core.Maybe Types.CopyTagsFromSource
    -- ^ Copies the tags from the specified volume to corresponding snapshot.
  , description :: Core.Maybe Core.Text
    -- ^ A description propagated to every snapshot specified by the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ Tags to apply to every snapshot specified by the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshots' value with any optional fields omitted.
mkCreateSnapshots
    :: Types.InstanceSpecification -- ^ 'instanceSpecification'
    -> CreateSnapshots
mkCreateSnapshots instanceSpecification
  = CreateSnapshots'{instanceSpecification,
                     copyTagsFromSource = Core.Nothing, description = Core.Nothing,
                     dryRun = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The instance to specify which volumes should be included in the snapshots.
--
-- /Note:/ Consider using 'instanceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceSpecification :: Lens.Lens' CreateSnapshots Types.InstanceSpecification
csInstanceSpecification = Lens.field @"instanceSpecification"
{-# INLINEABLE csInstanceSpecification #-}
{-# DEPRECATED instanceSpecification "Use generic-lens or generic-optics with 'instanceSpecification' instead"  #-}

-- | Copies the tags from the specified volume to corresponding snapshot.
--
-- /Note:/ Consider using 'copyTagsFromSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCopyTagsFromSource :: Lens.Lens' CreateSnapshots (Core.Maybe Types.CopyTagsFromSource)
csCopyTagsFromSource = Lens.field @"copyTagsFromSource"
{-# INLINEABLE csCopyTagsFromSource #-}
{-# DEPRECATED copyTagsFromSource "Use generic-lens or generic-optics with 'copyTagsFromSource' instead"  #-}

-- | A description propagated to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSnapshots (Core.Maybe Core.Text)
csDescription = Lens.field @"description"
{-# INLINEABLE csDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDryRun :: Lens.Lens' CreateSnapshots (Core.Maybe Core.Bool)
csDryRun = Lens.field @"dryRun"
{-# INLINEABLE csDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Tags to apply to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTagSpecifications :: Lens.Lens' CreateSnapshots (Core.Maybe [Types.TagSpecification])
csTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE csTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateSnapshots where
        toQuery CreateSnapshots{..}
          = Core.toQueryPair "Action" ("CreateSnapshots" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "InstanceSpecification" instanceSpecification
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsFromSource")
                copyTagsFromSource
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateSnapshots where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateSnapshots where
        type Rs CreateSnapshots = CreateSnapshotsResponse
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
                 CreateSnapshotsResponse' Core.<$>
                   (x Core..@? "snapshotSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSnapshotsResponse' smart constructor.
data CreateSnapshotsResponse = CreateSnapshotsResponse'
  { snapshots :: Core.Maybe [Types.SnapshotInfo]
    -- ^ List of snapshots.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSnapshotsResponse' value with any optional fields omitted.
mkCreateSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSnapshotsResponse
mkCreateSnapshotsResponse responseStatus
  = CreateSnapshotsResponse'{snapshots = Core.Nothing,
                             responseStatus}

-- | List of snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSnapshots :: Lens.Lens' CreateSnapshotsResponse (Core.Maybe [Types.SnapshotInfo])
csrrsSnapshots = Lens.field @"snapshots"
{-# INLINEABLE csrrsSnapshots #-}
{-# DEPRECATED snapshots "Use generic-lens or generic-optics with 'snapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSnapshotsResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateSnapshots (..),
    mkCreateSnapshots,

    -- ** Request lenses
    csInstanceSpecification,
    csCopyTagsFromSource,
    csDescription,
    csDryRun,
    csTagSpecifications,

    -- * Destructuring the response
    CreateSnapshotsResponse (..),
    mkCreateSnapshotsResponse,

    -- ** Response lenses
    csrrsSnapshots,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSnapshots' smart constructor.
data CreateSnapshots = CreateSnapshots'
  { -- | The instance to specify which volumes should be included in the snapshots.
    instanceSpecification :: Types.InstanceSpecification,
    -- | Copies the tags from the specified volume to corresponding snapshot.
    copyTagsFromSource :: Core.Maybe Types.CopyTagsFromSource,
    -- | A description propagated to every snapshot specified by the instance.
    description :: Core.Maybe Types.Description,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Tags to apply to every snapshot specified by the instance.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshots' value with any optional fields omitted.
mkCreateSnapshots ::
  -- | 'instanceSpecification'
  Types.InstanceSpecification ->
  CreateSnapshots
mkCreateSnapshots instanceSpecification =
  CreateSnapshots'
    { instanceSpecification,
      copyTagsFromSource = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The instance to specify which volumes should be included in the snapshots.
--
-- /Note:/ Consider using 'instanceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csInstanceSpecification :: Lens.Lens' CreateSnapshots Types.InstanceSpecification
csInstanceSpecification = Lens.field @"instanceSpecification"
{-# DEPRECATED csInstanceSpecification "Use generic-lens or generic-optics with 'instanceSpecification' instead." #-}

-- | Copies the tags from the specified volume to corresponding snapshot.
--
-- /Note:/ Consider using 'copyTagsFromSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCopyTagsFromSource :: Lens.Lens' CreateSnapshots (Core.Maybe Types.CopyTagsFromSource)
csCopyTagsFromSource = Lens.field @"copyTagsFromSource"
{-# DEPRECATED csCopyTagsFromSource "Use generic-lens or generic-optics with 'copyTagsFromSource' instead." #-}

-- | A description propagated to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateSnapshots (Core.Maybe Types.Description)
csDescription = Lens.field @"description"
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDryRun :: Lens.Lens' CreateSnapshots (Core.Maybe Core.Bool)
csDryRun = Lens.field @"dryRun"
{-# DEPRECATED csDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Tags to apply to every snapshot specified by the instance.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTagSpecifications :: Lens.Lens' CreateSnapshots (Core.Maybe [Types.TagSpecification])
csTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED csTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateSnapshots where
  type Rs CreateSnapshots = CreateSnapshotsResponse
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
            ( Core.pure ("Action", "CreateSnapshots")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceSpecification" instanceSpecification)
                Core.<> ( Core.toQueryValue "CopyTagsFromSource"
                            Core.<$> copyTagsFromSource
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSnapshotsResponse'
            Core.<$> (x Core..@? "snapshotSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSnapshotsResponse' smart constructor.
data CreateSnapshotsResponse = CreateSnapshotsResponse'
  { -- | List of snapshots.
    snapshots :: Core.Maybe [Types.SnapshotInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateSnapshotsResponse' value with any optional fields omitted.
mkCreateSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSnapshotsResponse
mkCreateSnapshotsResponse responseStatus =
  CreateSnapshotsResponse'
    { snapshots = Core.Nothing,
      responseStatus
    }

-- | List of snapshots.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSnapshots :: Lens.Lens' CreateSnapshotsResponse (Core.Maybe [Types.SnapshotInfo])
csrrsSnapshots = Lens.field @"snapshots"
{-# DEPRECATED csrrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSnapshotsResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

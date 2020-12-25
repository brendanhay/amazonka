{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the automatic copy of snapshots from one region to another region for a specified cluster.
module Network.AWS.Redshift.EnableSnapshotCopy
  ( -- * Creating a request
    EnableSnapshotCopy (..),
    mkEnableSnapshotCopy,

    -- ** Request lenses
    escClusterIdentifier,
    escDestinationRegion,
    escManualSnapshotRetentionPeriod,
    escRetentionPeriod,
    escSnapshotCopyGrantName,

    -- * Destructuring the response
    EnableSnapshotCopyResponse (..),
    mkEnableSnapshotCopyResponse,

    -- ** Response lenses
    escrrsCluster,
    escrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkEnableSnapshotCopy' smart constructor.
data EnableSnapshotCopy = EnableSnapshotCopy'
  { -- | The unique identifier of the source cluster to copy snapshots from.
    --
    -- Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
    clusterIdentifier :: Types.String,
    -- | The destination AWS Region that you want to copy snapshots to.
    --
    -- Constraints: Must be the name of a valid AWS Region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
    destinationRegion :: Types.String,
    -- | The number of days to retain newly copied snapshots in the destination AWS Region after they are copied from the source AWS Region. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The number of days to retain automated snapshots in the destination region after they are copied from the source region.
    --
    -- Default: 7.
    -- Constraints: Must be at least 1 and no more than 35.
    retentionPeriod :: Core.Maybe Core.Int,
    -- | The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
    snapshotCopyGrantName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableSnapshotCopy' value with any optional fields omitted.
mkEnableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'destinationRegion'
  Types.String ->
  EnableSnapshotCopy
mkEnableSnapshotCopy clusterIdentifier destinationRegion =
  EnableSnapshotCopy'
    { clusterIdentifier,
      destinationRegion,
      manualSnapshotRetentionPeriod = Core.Nothing,
      retentionPeriod = Core.Nothing,
      snapshotCopyGrantName = Core.Nothing
    }

-- | The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escClusterIdentifier :: Lens.Lens' EnableSnapshotCopy Types.String
escClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED escClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The destination AWS Region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid AWS Region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escDestinationRegion :: Lens.Lens' EnableSnapshotCopy Types.String
escDestinationRegion = Lens.field @"destinationRegion"
{-# DEPRECATED escDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The number of days to retain newly copied snapshots in the destination AWS Region after they are copied from the source AWS Region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escManualSnapshotRetentionPeriod :: Lens.Lens' EnableSnapshotCopy (Core.Maybe Core.Int)
escManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED escManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The number of days to retain automated snapshots in the destination region after they are copied from the source region.
--
-- Default: 7.
-- Constraints: Must be at least 1 and no more than 35.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escRetentionPeriod :: Lens.Lens' EnableSnapshotCopy (Core.Maybe Core.Int)
escRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED escRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSnapshotCopyGrantName :: Lens.Lens' EnableSnapshotCopy (Core.Maybe Types.String)
escSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED escSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Core.AWSRequest EnableSnapshotCopy where
  type Rs EnableSnapshotCopy = EnableSnapshotCopyResponse
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
            ( Core.pure ("Action", "EnableSnapshotCopy")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "DestinationRegion" destinationRegion)
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "RetentionPeriod" Core.<$> retentionPeriod)
                Core.<> ( Core.toQueryValue "SnapshotCopyGrantName"
                            Core.<$> snapshotCopyGrantName
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "EnableSnapshotCopyResult"
      ( \s h x ->
          EnableSnapshotCopyResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableSnapshotCopyResponse' smart constructor.
data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnableSnapshotCopyResponse' value with any optional fields omitted.
mkEnableSnapshotCopyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableSnapshotCopyResponse
mkEnableSnapshotCopyResponse responseStatus =
  EnableSnapshotCopyResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escrrsCluster :: Lens.Lens' EnableSnapshotCopyResponse (Core.Maybe Types.Cluster)
escrrsCluster = Lens.field @"cluster"
{-# DEPRECATED escrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escrrsResponseStatus :: Lens.Lens' EnableSnapshotCopyResponse Core.Int
escrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED escrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

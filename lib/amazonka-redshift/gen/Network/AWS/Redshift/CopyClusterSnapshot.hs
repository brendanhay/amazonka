{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.
--
-- When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CopyClusterSnapshot
  ( -- * Creating a request
    CopyClusterSnapshot (..),
    mkCopyClusterSnapshot,

    -- ** Request lenses
    ccsfSourceSnapshotIdentifier,
    ccsfTargetSnapshotIdentifier,
    ccsfManualSnapshotRetentionPeriod,
    ccsfSourceSnapshotClusterIdentifier,

    -- * Destructuring the response
    CopyClusterSnapshotResponse (..),
    mkCopyClusterSnapshotResponse,

    -- ** Response lenses
    ccsrfrsSnapshot,
    ccsrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCopyClusterSnapshot' smart constructor.
data CopyClusterSnapshot = CopyClusterSnapshot'
  { -- | The identifier for the source snapshot.
    --
    -- Constraints:
    --
    --     * Must be the identifier for a valid automated snapshot whose state is @available@ .
    sourceSnapshotIdentifier :: Types.String,
    -- | The identifier given to the new manual snapshot.
    --
    -- Constraints:
    --
    --     * Cannot be null, empty, or blank.
    --
    --
    --     * Must contain from 1 to 255 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    --
    --
    --     * Must be unique for the AWS account that is making the request.
    targetSnapshotIdentifier :: Types.String,
    -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    --
    -- Constraints:
    --
    --     * Must be the identifier for a valid cluster.
    sourceSnapshotClusterIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyClusterSnapshot' value with any optional fields omitted.
mkCopyClusterSnapshot ::
  -- | 'sourceSnapshotIdentifier'
  Types.String ->
  -- | 'targetSnapshotIdentifier'
  Types.String ->
  CopyClusterSnapshot
mkCopyClusterSnapshot
  sourceSnapshotIdentifier
  targetSnapshotIdentifier =
    CopyClusterSnapshot'
      { sourceSnapshotIdentifier,
        targetSnapshotIdentifier,
        manualSnapshotRetentionPeriod = Core.Nothing,
        sourceSnapshotClusterIdentifier = Core.Nothing
      }

-- | The identifier for the source snapshot.
--
-- Constraints:
--
--     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
--
--
-- /Note:/ Consider using 'sourceSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfSourceSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Types.String
ccsfSourceSnapshotIdentifier = Lens.field @"sourceSnapshotIdentifier"
{-# DEPRECATED ccsfSourceSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotIdentifier' instead." #-}

-- | The identifier given to the new manual snapshot.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank.
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for the AWS account that is making the request.
--
--
--
-- /Note:/ Consider using 'targetSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfTargetSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Types.String
ccsfTargetSnapshotIdentifier = Lens.field @"targetSnapshotIdentifier"
{-# DEPRECATED ccsfTargetSnapshotIdentifier "Use generic-lens or generic-optics with 'targetSnapshotIdentifier' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfManualSnapshotRetentionPeriod :: Lens.Lens' CopyClusterSnapshot (Core.Maybe Core.Int)
ccsfManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED ccsfManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints:
--
--     * Must be the identifier for a valid cluster.
--
--
--
-- /Note:/ Consider using 'sourceSnapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfSourceSnapshotClusterIdentifier :: Lens.Lens' CopyClusterSnapshot (Core.Maybe Types.String)
ccsfSourceSnapshotClusterIdentifier = Lens.field @"sourceSnapshotClusterIdentifier"
{-# DEPRECATED ccsfSourceSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotClusterIdentifier' instead." #-}

instance Core.AWSRequest CopyClusterSnapshot where
  type Rs CopyClusterSnapshot = CopyClusterSnapshotResponse
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
            ( Core.pure ("Action", "CopyClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue
                            "SourceSnapshotIdentifier"
                            sourceSnapshotIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetSnapshotIdentifier"
                            targetSnapshotIdentifier
                        )
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
                Core.<> ( Core.toQueryValue "SourceSnapshotClusterIdentifier"
                            Core.<$> sourceSnapshotClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CopyClusterSnapshotResult"
      ( \s h x ->
          CopyClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyClusterSnapshotResponse' smart constructor.
data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyClusterSnapshotResponse' value with any optional fields omitted.
mkCopyClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyClusterSnapshotResponse
mkCopyClusterSnapshotResponse responseStatus =
  CopyClusterSnapshotResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrfrsSnapshot :: Lens.Lens' CopyClusterSnapshotResponse (Core.Maybe Types.Snapshot)
ccsrfrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED ccsrfrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrfrsResponseStatus :: Lens.Lens' CopyClusterSnapshotResponse Core.Int
ccsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

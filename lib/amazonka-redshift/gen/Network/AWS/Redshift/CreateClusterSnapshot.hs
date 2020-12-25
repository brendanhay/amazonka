{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be in the @available@ state.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSnapshot
  ( -- * Creating a request
    CreateClusterSnapshot (..),
    mkCreateClusterSnapshot,

    -- ** Request lenses
    ccsSnapshotIdentifier,
    ccsClusterIdentifier,
    ccsManualSnapshotRetentionPeriod,
    ccsTags,

    -- * Destructuring the response
    CreateClusterSnapshotResponse (..),
    mkCreateClusterSnapshotResponse,

    -- ** Response lenses
    ccsrrsSnapshot,
    ccsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
  { -- | A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
    --
    -- Constraints:
    --
    --     * Cannot be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 alphanumeric characters or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-snapshot-id@
    snapshotIdentifier :: Types.String,
    -- | The cluster identifier for which you want a snapshot.
    clusterIdentifier :: Types.String,
    -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterSnapshot' value with any optional fields omitted.
mkCreateClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Types.String ->
  -- | 'clusterIdentifier'
  Types.String ->
  CreateClusterSnapshot
mkCreateClusterSnapshot snapshotIdentifier clusterIdentifier =
  CreateClusterSnapshot'
    { snapshotIdentifier,
      clusterIdentifier,
      manualSnapshotRetentionPeriod = Core.Nothing,
      tags = Core.Nothing
    }

-- | A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSnapshotIdentifier :: Lens.Lens' CreateClusterSnapshot Types.String
ccsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED ccsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The cluster identifier for which you want a snapshot.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsClusterIdentifier :: Lens.Lens' CreateClusterSnapshot Types.String
ccsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED ccsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsManualSnapshotRetentionPeriod :: Lens.Lens' CreateClusterSnapshot (Core.Maybe Core.Int)
ccsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED ccsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateClusterSnapshot (Core.Maybe [Types.Tag])
ccsTags = Lens.field @"tags"
{-# DEPRECATED ccsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateClusterSnapshot where
  type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse
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
            ( Core.pure ("Action", "CreateClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateClusterSnapshotResult"
      ( \s h x ->
          CreateClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateClusterSnapshotResponse' value with any optional fields omitted.
mkCreateClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateClusterSnapshotResponse
mkCreateClusterSnapshotResponse responseStatus =
  CreateClusterSnapshotResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsSnapshot :: Lens.Lens' CreateClusterSnapshotResponse (Core.Maybe Types.Snapshot)
ccsrrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED ccsrrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsResponseStatus :: Lens.Lens' CreateClusterSnapshotResponse Core.Int
ccsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

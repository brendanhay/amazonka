{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.BatchModifyClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a set of cluster snapshots.
module Network.AWS.Redshift.BatchModifyClusterSnapshots
  ( -- * Creating a request
    BatchModifyClusterSnapshots (..),
    mkBatchModifyClusterSnapshots,

    -- ** Request lenses
    bmcsSnapshotIdentifierList,
    bmcsForce,
    bmcsManualSnapshotRetentionPeriod,

    -- * Destructuring the response
    BatchModifyClusterSnapshotsResponse (..),
    mkBatchModifyClusterSnapshotsResponse,

    -- ** Response lenses
    bmcsrrsErrors,
    bmcsrrsResources,
    bmcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { -- | A list of snapshot identifiers you want to modify.
    snapshotIdentifierList :: [Types.String],
    -- | A boolean value indicating whether to override an exception if the retention period has passed.
    force :: Core.Maybe Core.Bool,
    -- | The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely.
    --
    -- The number must be either -1 or an integer between 1 and 3,653.
    -- If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchModifyClusterSnapshots' value with any optional fields omitted.
mkBatchModifyClusterSnapshots ::
  BatchModifyClusterSnapshots
mkBatchModifyClusterSnapshots =
  BatchModifyClusterSnapshots'
    { snapshotIdentifierList =
        Core.mempty,
      force = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing
    }

-- | A list of snapshot identifiers you want to modify.
--
-- /Note:/ Consider using 'snapshotIdentifierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsSnapshotIdentifierList :: Lens.Lens' BatchModifyClusterSnapshots [Types.String]
bmcsSnapshotIdentifierList = Lens.field @"snapshotIdentifierList"
{-# DEPRECATED bmcsSnapshotIdentifierList "Use generic-lens or generic-optics with 'snapshotIdentifierList' instead." #-}

-- | A boolean value indicating whether to override an exception if the retention period has passed.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsForce :: Lens.Lens' BatchModifyClusterSnapshots (Core.Maybe Core.Bool)
bmcsForce = Lens.field @"force"
{-# DEPRECATED bmcsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely.
--
-- The number must be either -1 or an integer between 1 and 3,653.
-- If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsManualSnapshotRetentionPeriod :: Lens.Lens' BatchModifyClusterSnapshots (Core.Maybe Core.Int)
bmcsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED bmcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

instance Core.AWSRequest BatchModifyClusterSnapshots where
  type
    Rs BatchModifyClusterSnapshots =
      BatchModifyClusterSnapshotsResponse
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
            ( Core.pure ("Action", "BatchModifyClusterSnapshots")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue
                            "SnapshotIdentifierList"
                            (Core.toQueryList "String" snapshotIdentifierList)
                        )
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BatchModifyClusterSnapshotsResult"
      ( \s h x ->
          BatchModifyClusterSnapshotsResponse'
            Core.<$> ( x Core..@? "Errors"
                         Core..<@> Core.parseXMLList "SnapshotErrorMessage"
                     )
            Core.<*> (x Core..@? "Resources" Core..<@> Core.parseXMLList "String")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { -- | A list of any errors returned.
    errors :: Core.Maybe [Types.SnapshotErrorMessage],
    -- | A list of the snapshots that were modified.
    resources :: Core.Maybe [Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchModifyClusterSnapshotsResponse' value with any optional fields omitted.
mkBatchModifyClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchModifyClusterSnapshotsResponse
mkBatchModifyClusterSnapshotsResponse responseStatus =
  BatchModifyClusterSnapshotsResponse'
    { errors = Core.Nothing,
      resources = Core.Nothing,
      responseStatus
    }

-- | A list of any errors returned.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrrsErrors :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Core.Maybe [Types.SnapshotErrorMessage])
bmcsrrsErrors = Lens.field @"errors"
{-# DEPRECATED bmcsrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | A list of the snapshots that were modified.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrrsResources :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Core.Maybe [Types.String])
bmcsrrsResources = Lens.field @"resources"
{-# DEPRECATED bmcsrrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrrsResponseStatus :: Lens.Lens' BatchModifyClusterSnapshotsResponse Core.Int
bmcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bmcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

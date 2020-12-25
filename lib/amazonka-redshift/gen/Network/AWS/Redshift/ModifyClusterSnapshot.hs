{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a snapshot.
--
-- This exanmple modifies the manual retention period setting for a cluster snapshot.
module Network.AWS.Redshift.ModifyClusterSnapshot
  ( -- * Creating a request
    ModifyClusterSnapshot (..),
    mkModifyClusterSnapshot,

    -- ** Request lenses
    mcsSnapshotIdentifier,
    mcsForce,
    mcsManualSnapshotRetentionPeriod,

    -- * Destructuring the response
    ModifyClusterSnapshotResponse (..),
    mkModifyClusterSnapshotResponse,

    -- ** Response lenses
    mcsrrsSnapshot,
    mcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { -- | The identifier of the snapshot whose setting you want to modify.
    snapshotIdentifier :: Types.String,
    -- | A Boolean option to override an exception if the retention period has already passed.
    force :: Core.Maybe Core.Bool,
    -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshot' value with any optional fields omitted.
mkModifyClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Types.String ->
  ModifyClusterSnapshot
mkModifyClusterSnapshot snapshotIdentifier =
  ModifyClusterSnapshot'
    { snapshotIdentifier,
      force = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing
    }

-- | The identifier of the snapshot whose setting you want to modify.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsSnapshotIdentifier :: Lens.Lens' ModifyClusterSnapshot Types.String
mcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED mcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A Boolean option to override an exception if the retention period has already passed.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsForce :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Bool)
mcsForce = Lens.field @"force"
{-# DEPRECATED mcsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsManualSnapshotRetentionPeriod :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Int)
mcsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED mcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

instance Core.AWSRequest ModifyClusterSnapshot where
  type Rs ModifyClusterSnapshot = ModifyClusterSnapshotResponse
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
            ( Core.pure ("Action", "ModifyClusterSnapshot")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
                Core.<> ( Core.toQueryValue "ManualSnapshotRetentionPeriod"
                            Core.<$> manualSnapshotRetentionPeriod
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSnapshotResult"
      ( \s h x ->
          ModifyClusterSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyClusterSnapshotResponse' value with any optional fields omitted.
mkModifyClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyClusterSnapshotResponse
mkModifyClusterSnapshotResponse responseStatus =
  ModifyClusterSnapshotResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrrsSnapshot :: Lens.Lens' ModifyClusterSnapshotResponse (Core.Maybe Types.Snapshot)
mcsrrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED mcsrrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrrsResponseStatus :: Lens.Lens' ModifyClusterSnapshotResponse Core.Int
mcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

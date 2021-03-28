{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyClusterSnapshot (..)
    , mkModifyClusterSnapshot
    -- ** Request lenses
    , mcsSnapshotIdentifier
    , mcsForce
    , mcsManualSnapshotRetentionPeriod

    -- * Destructuring the response
    , ModifyClusterSnapshotResponse (..)
    , mkModifyClusterSnapshotResponse
    -- ** Response lenses
    , mcsrrsSnapshot
    , mcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { snapshotIdentifier :: Core.Text
    -- ^ The identifier of the snapshot whose setting you want to modify.
  , force :: Core.Maybe Core.Bool
    -- ^ A Boolean option to override an exception if the retention period has already passed.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
-- The value must be either -1 or an integer between 1 and 3,653.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterSnapshot' value with any optional fields omitted.
mkModifyClusterSnapshot
    :: Core.Text -- ^ 'snapshotIdentifier'
    -> ModifyClusterSnapshot
mkModifyClusterSnapshot snapshotIdentifier
  = ModifyClusterSnapshot'{snapshotIdentifier, force = Core.Nothing,
                           manualSnapshotRetentionPeriod = Core.Nothing}

-- | The identifier of the snapshot whose setting you want to modify.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsSnapshotIdentifier :: Lens.Lens' ModifyClusterSnapshot Core.Text
mcsSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE mcsSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | A Boolean option to override an exception if the retention period has already passed.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsForce :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Bool)
mcsForce = Lens.field @"force"
{-# INLINEABLE mcsForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsManualSnapshotRetentionPeriod :: Lens.Lens' ModifyClusterSnapshot (Core.Maybe Core.Int)
mcsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE mcsManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

instance Core.ToQuery ModifyClusterSnapshot where
        toQuery ModifyClusterSnapshot{..}
          = Core.toQueryPair "Action" ("ModifyClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Force") force
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ManualSnapshotRetentionPeriod")
                manualSnapshotRetentionPeriod

instance Core.ToHeaders ModifyClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyClusterSnapshot where
        type Rs ModifyClusterSnapshot = ModifyClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "ModifyClusterSnapshotResult"
              (\ s h x ->
                 ModifyClusterSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyClusterSnapshotResponse' value with any optional fields omitted.
mkModifyClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyClusterSnapshotResponse
mkModifyClusterSnapshotResponse responseStatus
  = ModifyClusterSnapshotResponse'{snapshot = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrrsSnapshot :: Lens.Lens' ModifyClusterSnapshotResponse (Core.Maybe Types.Snapshot)
mcsrrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE mcsrrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrrsResponseStatus :: Lens.Lens' ModifyClusterSnapshotResponse Core.Int
mcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

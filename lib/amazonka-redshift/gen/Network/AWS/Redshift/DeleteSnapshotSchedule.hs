{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot schedule.
module Network.AWS.Redshift.DeleteSnapshotSchedule
    (
    -- * Creating a request
      DeleteSnapshotSchedule (..)
    , mkDeleteSnapshotSchedule
    -- ** Request lenses
    , dScheduleIdentifier

    -- * Destructuring the response
    , DeleteSnapshotScheduleResponse (..)
    , mkDeleteSnapshotScheduleResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { scheduleIdentifier :: Core.Text
    -- ^ A unique identifier of the snapshot schedule to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotSchedule' value with any optional fields omitted.
mkDeleteSnapshotSchedule
    :: Core.Text -- ^ 'scheduleIdentifier'
    -> DeleteSnapshotSchedule
mkDeleteSnapshotSchedule scheduleIdentifier
  = DeleteSnapshotSchedule'{scheduleIdentifier}

-- | A unique identifier of the snapshot schedule to delete.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleIdentifier :: Lens.Lens' DeleteSnapshotSchedule Core.Text
dScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# INLINEABLE dScheduleIdentifier #-}
{-# DEPRECATED scheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead"  #-}

instance Core.ToQuery DeleteSnapshotSchedule where
        toQuery DeleteSnapshotSchedule{..}
          = Core.toQueryPair "Action" ("DeleteSnapshotSchedule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ScheduleIdentifier" scheduleIdentifier

instance Core.ToHeaders DeleteSnapshotSchedule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSnapshotSchedule where
        type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse
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
          = Response.receiveNull DeleteSnapshotScheduleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotScheduleResponse' value with any optional fields omitted.
mkDeleteSnapshotScheduleResponse
    :: DeleteSnapshotScheduleResponse
mkDeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'

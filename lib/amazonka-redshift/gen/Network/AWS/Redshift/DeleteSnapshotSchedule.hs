{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteSnapshotSchedule (..),
    mkDeleteSnapshotSchedule,

    -- ** Request lenses
    dScheduleIdentifier,

    -- * Destructuring the response
    DeleteSnapshotScheduleResponse (..),
    mkDeleteSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { -- | A unique identifier of the snapshot schedule to delete.
    scheduleIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotSchedule' value with any optional fields omitted.
mkDeleteSnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Types.String ->
  DeleteSnapshotSchedule
mkDeleteSnapshotSchedule scheduleIdentifier =
  DeleteSnapshotSchedule' {scheduleIdentifier}

-- | A unique identifier of the snapshot schedule to delete.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduleIdentifier :: Lens.Lens' DeleteSnapshotSchedule Types.String
dScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# DEPRECATED dScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

instance Core.AWSRequest DeleteSnapshotSchedule where
  type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse
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
            ( Core.pure ("Action", "DeleteSnapshotSchedule")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ScheduleIdentifier" scheduleIdentifier)
            )
      }
  response = Response.receiveNull DeleteSnapshotScheduleResponse'

-- | /See:/ 'mkDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotScheduleResponse' value with any optional fields omitted.
mkDeleteSnapshotScheduleResponse ::
  DeleteSnapshotScheduleResponse
mkDeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'

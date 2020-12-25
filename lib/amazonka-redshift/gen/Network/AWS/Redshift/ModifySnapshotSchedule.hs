{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule. Any schedule associated with a cluster is modified asynchronously.
module Network.AWS.Redshift.ModifySnapshotSchedule
  ( -- * Creating a request
    ModifySnapshotSchedule (..),
    mkModifySnapshotSchedule,

    -- ** Request lenses
    mssScheduleIdentifier,
    mssScheduleDefinitions,

    -- * Destructuring the response
    Types.SnapshotSchedule (..),
    Types.mkSnapshotSchedule,

    -- ** Response lenses
    Types.ssAssociatedClusterCount,
    Types.ssAssociatedClusters,
    Types.ssNextInvocations,
    Types.ssScheduleDefinitions,
    Types.ssScheduleDescription,
    Types.ssScheduleIdentifier,
    Types.ssTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { -- | A unique alphanumeric identifier of the schedule to modify.
    scheduleIdentifier :: Types.String,
    -- | An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
    scheduleDefinitions :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySnapshotSchedule' value with any optional fields omitted.
mkModifySnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Types.String ->
  ModifySnapshotSchedule
mkModifySnapshotSchedule scheduleIdentifier =
  ModifySnapshotSchedule'
    { scheduleIdentifier,
      scheduleDefinitions = Core.mempty
    }

-- | A unique alphanumeric identifier of the schedule to modify.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleIdentifier :: Lens.Lens' ModifySnapshotSchedule Types.String
mssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# DEPRECATED mssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- | An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleDefinitions :: Lens.Lens' ModifySnapshotSchedule [Types.String]
mssScheduleDefinitions = Lens.field @"scheduleDefinitions"
{-# DEPRECATED mssScheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead." #-}

instance Core.AWSRequest ModifySnapshotSchedule where
  type Rs ModifySnapshotSchedule = Types.SnapshotSchedule
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
            ( Core.pure ("Action", "ModifySnapshotSchedule")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ScheduleIdentifier" scheduleIdentifier)
                Core.<> ( Core.toQueryValue
                            "ScheduleDefinitions"
                            (Core.toQueryList "ScheduleDefinition" scheduleDefinitions)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifySnapshotScheduleResult"
      (\s h x -> Core.parseXML x)

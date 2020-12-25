{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a snapshot schedule that can be associated to a cluster and which overrides the default system backup schedule.
module Network.AWS.Redshift.CreateSnapshotSchedule
  ( -- * Creating a request
    CreateSnapshotSchedule (..),
    mkCreateSnapshotSchedule,

    -- ** Request lenses
    cssDryRun,
    cssNextInvocations,
    cssScheduleDefinitions,
    cssScheduleDescription,
    cssScheduleIdentifier,
    cssTags,

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

-- | /See:/ 'mkCreateSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { -- |
    dryRun :: Core.Maybe Core.Bool,
    -- |
    nextInvocations :: Core.Maybe Core.Int,
    -- | The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
    scheduleDefinitions :: Core.Maybe [Types.String],
    -- | The description of the snapshot schedule.
    scheduleDescription :: Core.Maybe Types.String,
    -- | A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
    scheduleIdentifier :: Core.Maybe Types.String,
    -- | An optional set of tags you can use to search for the schedule.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotSchedule' value with any optional fields omitted.
mkCreateSnapshotSchedule ::
  CreateSnapshotSchedule
mkCreateSnapshotSchedule =
  CreateSnapshotSchedule'
    { dryRun = Core.Nothing,
      nextInvocations = Core.Nothing,
      scheduleDefinitions = Core.Nothing,
      scheduleDescription = Core.Nothing,
      scheduleIdentifier = Core.Nothing,
      tags = Core.Nothing
    }

-- |
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDryRun :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Bool)
cssDryRun = Lens.field @"dryRun"
{-# DEPRECATED cssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- |
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssNextInvocations :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Int)
cssNextInvocations = Lens.field @"nextInvocations"
{-# DEPRECATED cssNextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead." #-}

-- | The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDefinitions :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Types.String])
cssScheduleDefinitions = Lens.field @"scheduleDefinitions"
{-# DEPRECATED cssScheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead." #-}

-- | The description of the snapshot schedule.
--
-- /Note:/ Consider using 'scheduleDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDescription :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Types.String)
cssScheduleDescription = Lens.field @"scheduleDescription"
{-# DEPRECATED cssScheduleDescription "Use generic-lens or generic-optics with 'scheduleDescription' instead." #-}

-- | A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleIdentifier :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Types.String)
cssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# DEPRECATED cssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- | An optional set of tags you can use to search for the schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTags :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Types.Tag])
cssTags = Lens.field @"tags"
{-# DEPRECATED cssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateSnapshotSchedule where
  type Rs CreateSnapshotSchedule = Types.SnapshotSchedule
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
            ( Core.pure ("Action", "CreateSnapshotSchedule")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "NextInvocations" Core.<$> nextInvocations)
                Core.<> ( Core.toQueryValue
                            "ScheduleDefinitions"
                            ( Core.toQueryList "ScheduleDefinition"
                                Core.<$> scheduleDefinitions
                            )
                        )
                Core.<> ( Core.toQueryValue "ScheduleDescription"
                            Core.<$> scheduleDescription
                        )
                Core.<> ( Core.toQueryValue "ScheduleIdentifier"
                            Core.<$> scheduleIdentifier
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotScheduleResult"
      (\s h x -> Core.parseXML x)

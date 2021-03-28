{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSnapshotSchedule (..)
    , mkCreateSnapshotSchedule
    -- ** Request lenses
    , cssDryRun
    , cssNextInvocations
    , cssScheduleDefinitions
    , cssScheduleDescription
    , cssScheduleIdentifier
    , cssTags

     -- * Destructuring the response
    , Types.SnapshotSchedule (..)
    , Types.mkSnapshotSchedule
    -- ** Response lenses
    , Types.ssAssociatedClusterCount
    , Types.ssAssociatedClusters
    , Types.ssNextInvocations
    , Types.ssScheduleDefinitions
    , Types.ssScheduleDescription
    , Types.ssScheduleIdentifier
    , Types.ssTags
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ 
  , nextInvocations :: Core.Maybe Core.Int
    -- ^ 
  , scheduleDefinitions :: Core.Maybe [Core.Text]
    -- ^ The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)". 
  , scheduleDescription :: Core.Maybe Core.Text
    -- ^ The description of the snapshot schedule.
  , scheduleIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An optional set of tags you can use to search for the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotSchedule' value with any optional fields omitted.
mkCreateSnapshotSchedule
    :: CreateSnapshotSchedule
mkCreateSnapshotSchedule
  = CreateSnapshotSchedule'{dryRun = Core.Nothing,
                            nextInvocations = Core.Nothing, scheduleDefinitions = Core.Nothing,
                            scheduleDescription = Core.Nothing,
                            scheduleIdentifier = Core.Nothing, tags = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDryRun :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Bool)
cssDryRun = Lens.field @"dryRun"
{-# INLINEABLE cssDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssNextInvocations :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Int)
cssNextInvocations = Lens.field @"nextInvocations"
{-# INLINEABLE cssNextInvocations #-}
{-# DEPRECATED nextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead"  #-}

-- | The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)". 
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDefinitions :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Core.Text])
cssScheduleDefinitions = Lens.field @"scheduleDefinitions"
{-# INLINEABLE cssScheduleDefinitions #-}
{-# DEPRECATED scheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead"  #-}

-- | The description of the snapshot schedule.
--
-- /Note:/ Consider using 'scheduleDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleDescription :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Text)
cssScheduleDescription = Lens.field @"scheduleDescription"
{-# INLINEABLE cssScheduleDescription #-}
{-# DEPRECATED scheduleDescription "Use generic-lens or generic-optics with 'scheduleDescription' instead"  #-}

-- | A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScheduleIdentifier :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe Core.Text)
cssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# INLINEABLE cssScheduleIdentifier #-}
{-# DEPRECATED scheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead"  #-}

-- | An optional set of tags you can use to search for the schedule.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTags :: Lens.Lens' CreateSnapshotSchedule (Core.Maybe [Types.Tag])
cssTags = Lens.field @"tags"
{-# INLINEABLE cssTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateSnapshotSchedule where
        toQuery CreateSnapshotSchedule{..}
          = Core.toQueryPair "Action" ("CreateSnapshotSchedule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextInvocations")
                nextInvocations
              Core.<>
              Core.toQueryPair "ScheduleDefinitions"
                (Core.maybe Core.mempty (Core.toQueryList "ScheduleDefinition")
                   scheduleDefinitions)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScheduleDescription")
                scheduleDescription
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ScheduleIdentifier")
                scheduleIdentifier
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateSnapshotSchedule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateSnapshotSchedule where
        type Rs CreateSnapshotSchedule = Types.SnapshotSchedule
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
          = Response.receiveXMLWrapper "CreateSnapshotScheduleResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

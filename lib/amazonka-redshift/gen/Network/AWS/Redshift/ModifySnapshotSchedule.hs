{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifySnapshotSchedule (..)
    , mkModifySnapshotSchedule
    -- ** Request lenses
    , mssScheduleIdentifier
    , mssScheduleDefinitions

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

-- | /See:/ 'mkModifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { scheduleIdentifier :: Core.Text
    -- ^ A unique alphanumeric identifier of the schedule to modify.
  , scheduleDefinitions :: [Core.Text]
    -- ^ An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySnapshotSchedule' value with any optional fields omitted.
mkModifySnapshotSchedule
    :: Core.Text -- ^ 'scheduleIdentifier'
    -> ModifySnapshotSchedule
mkModifySnapshotSchedule scheduleIdentifier
  = ModifySnapshotSchedule'{scheduleIdentifier,
                            scheduleDefinitions = Core.mempty}

-- | A unique alphanumeric identifier of the schedule to modify.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleIdentifier :: Lens.Lens' ModifySnapshotSchedule Core.Text
mssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# INLINEABLE mssScheduleIdentifier #-}
{-# DEPRECATED scheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead"  #-}

-- | An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
--
-- /Note:/ Consider using 'scheduleDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssScheduleDefinitions :: Lens.Lens' ModifySnapshotSchedule [Core.Text]
mssScheduleDefinitions = Lens.field @"scheduleDefinitions"
{-# INLINEABLE mssScheduleDefinitions #-}
{-# DEPRECATED scheduleDefinitions "Use generic-lens or generic-optics with 'scheduleDefinitions' instead"  #-}

instance Core.ToQuery ModifySnapshotSchedule where
        toQuery ModifySnapshotSchedule{..}
          = Core.toQueryPair "Action" ("ModifySnapshotSchedule" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ScheduleIdentifier" scheduleIdentifier
              Core.<>
              Core.toQueryPair "ScheduleDefinitions"
                (Core.toQueryList "ScheduleDefinition" scheduleDefinitions)

instance Core.ToHeaders ModifySnapshotSchedule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifySnapshotSchedule where
        type Rs ModifySnapshotSchedule = Types.SnapshotSchedule
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
          = Response.receiveXMLWrapper "ModifySnapshotScheduleResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

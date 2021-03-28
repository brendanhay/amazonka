{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.BundleTask
  ( BundleTask (..)
  -- * Smart constructor
  , mkBundleTask
  -- * Lenses
  , btBundleId
  , btBundleTaskError
  , btInstanceId
  , btProgress
  , btStartTime
  , btState
  , btStorage
  , btUpdateTime
  ) where

import qualified Network.AWS.EC2.Types.BundleTaskError as Types
import qualified Network.AWS.EC2.Types.BundleTaskState as Types
import qualified Network.AWS.EC2.Types.Storage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a bundle task.
--
-- /See:/ 'mkBundleTask' smart constructor.
data BundleTask = BundleTask'
  { bundleId :: Core.Text
    -- ^ The ID of the bundle task.
  , bundleTaskError :: Core.Maybe Types.BundleTaskError
    -- ^ If the task fails, a description of the error.
  , instanceId :: Core.Text
    -- ^ The ID of the instance associated with this bundle task.
  , progress :: Core.Text
    -- ^ The level of task completion, as a percent (for example, 20%).
  , startTime :: Core.UTCTime
    -- ^ The time this task started.
  , state :: Types.BundleTaskState
    -- ^ The state of the task.
  , storage :: Types.Storage
    -- ^ The Amazon S3 storage locations.
  , updateTime :: Core.UTCTime
    -- ^ The time of the most recent update for the task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BundleTask' value with any optional fields omitted.
mkBundleTask
    :: Core.Text -- ^ 'bundleId'
    -> Core.Text -- ^ 'instanceId'
    -> Core.Text -- ^ 'progress'
    -> Core.UTCTime -- ^ 'startTime'
    -> Types.BundleTaskState -- ^ 'state'
    -> Types.Storage -- ^ 'storage'
    -> Core.UTCTime -- ^ 'updateTime'
    -> BundleTask
mkBundleTask bundleId instanceId progress startTime state storage
  updateTime
  = BundleTask'{bundleId, bundleTaskError = Core.Nothing, instanceId,
                progress, startTime, state, storage, updateTime}

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleId :: Lens.Lens' BundleTask Core.Text
btBundleId = Lens.field @"bundleId"
{-# INLINEABLE btBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | If the task fails, a description of the error.
--
-- /Note:/ Consider using 'bundleTaskError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleTaskError :: Lens.Lens' BundleTask (Core.Maybe Types.BundleTaskError)
btBundleTaskError = Lens.field @"bundleTaskError"
{-# INLINEABLE btBundleTaskError #-}
{-# DEPRECATED bundleTaskError "Use generic-lens or generic-optics with 'bundleTaskError' instead"  #-}

-- | The ID of the instance associated with this bundle task.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btInstanceId :: Lens.Lens' BundleTask Core.Text
btInstanceId = Lens.field @"instanceId"
{-# INLINEABLE btInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The level of task completion, as a percent (for example, 20%).
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btProgress :: Lens.Lens' BundleTask Core.Text
btProgress = Lens.field @"progress"
{-# INLINEABLE btProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The time this task started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStartTime :: Lens.Lens' BundleTask Core.UTCTime
btStartTime = Lens.field @"startTime"
{-# INLINEABLE btStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The state of the task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btState :: Lens.Lens' BundleTask Types.BundleTaskState
btState = Lens.field @"state"
{-# INLINEABLE btState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The Amazon S3 storage locations.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStorage :: Lens.Lens' BundleTask Types.Storage
btStorage = Lens.field @"storage"
{-# INLINEABLE btStorage #-}
{-# DEPRECATED storage "Use generic-lens or generic-optics with 'storage' instead"  #-}

-- | The time of the most recent update for the task.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btUpdateTime :: Lens.Lens' BundleTask Core.UTCTime
btUpdateTime = Lens.field @"updateTime"
{-# INLINEABLE btUpdateTime #-}
{-# DEPRECATED updateTime "Use generic-lens or generic-optics with 'updateTime' instead"  #-}

instance Core.FromXML BundleTask where
        parseXML x
          = BundleTask' Core.<$>
              (x Core..@ "bundleId") Core.<*> x Core..@? "error" Core.<*>
                x Core..@ "instanceId"
                Core.<*> x Core..@ "progress"
                Core.<*> x Core..@ "startTime"
                Core.<*> x Core..@ "state"
                Core.<*> x Core..@ "storage"
                Core.<*> x Core..@ "updateTime"

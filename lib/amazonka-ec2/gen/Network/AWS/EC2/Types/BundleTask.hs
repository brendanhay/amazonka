{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTask
  ( BundleTask (..),

    -- * Smart constructor
    mkBundleTask,

    -- * Lenses
    btBundleId,
    btBundleTaskError,
    btInstanceId,
    btProgress,
    btStartTime,
    btState,
    btStorage,
    btUpdateTime,
  )
where

import qualified Network.AWS.EC2.Types.BundleTaskError as Types
import qualified Network.AWS.EC2.Types.BundleTaskState as Types
import qualified Network.AWS.EC2.Types.Storage as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a bundle task.
--
-- /See:/ 'mkBundleTask' smart constructor.
data BundleTask = BundleTask'
  { -- | The ID of the bundle task.
    bundleId :: Types.String,
    -- | If the task fails, a description of the error.
    bundleTaskError :: Core.Maybe Types.BundleTaskError,
    -- | The ID of the instance associated with this bundle task.
    instanceId :: Types.String,
    -- | The level of task completion, as a percent (for example, 20%).
    progress :: Types.String,
    -- | The time this task started.
    startTime :: Core.UTCTime,
    -- | The state of the task.
    state :: Types.BundleTaskState,
    -- | The Amazon S3 storage locations.
    storage :: Types.Storage,
    -- | The time of the most recent update for the task.
    updateTime :: Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BundleTask' value with any optional fields omitted.
mkBundleTask ::
  -- | 'bundleId'
  Types.String ->
  -- | 'instanceId'
  Types.String ->
  -- | 'progress'
  Types.String ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'state'
  Types.BundleTaskState ->
  -- | 'storage'
  Types.Storage ->
  -- | 'updateTime'
  Core.UTCTime ->
  BundleTask
mkBundleTask
  bundleId
  instanceId
  progress
  startTime
  state
  storage
  updateTime =
    BundleTask'
      { bundleId,
        bundleTaskError = Core.Nothing,
        instanceId,
        progress,
        startTime,
        state,
        storage,
        updateTime
      }

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleId :: Lens.Lens' BundleTask Types.String
btBundleId = Lens.field @"bundleId"
{-# DEPRECATED btBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | If the task fails, a description of the error.
--
-- /Note:/ Consider using 'bundleTaskError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleTaskError :: Lens.Lens' BundleTask (Core.Maybe Types.BundleTaskError)
btBundleTaskError = Lens.field @"bundleTaskError"
{-# DEPRECATED btBundleTaskError "Use generic-lens or generic-optics with 'bundleTaskError' instead." #-}

-- | The ID of the instance associated with this bundle task.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btInstanceId :: Lens.Lens' BundleTask Types.String
btInstanceId = Lens.field @"instanceId"
{-# DEPRECATED btInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The level of task completion, as a percent (for example, 20%).
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btProgress :: Lens.Lens' BundleTask Types.String
btProgress = Lens.field @"progress"
{-# DEPRECATED btProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The time this task started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStartTime :: Lens.Lens' BundleTask Core.UTCTime
btStartTime = Lens.field @"startTime"
{-# DEPRECATED btStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btState :: Lens.Lens' BundleTask Types.BundleTaskState
btState = Lens.field @"state"
{-# DEPRECATED btState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon S3 storage locations.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStorage :: Lens.Lens' BundleTask Types.Storage
btStorage = Lens.field @"storage"
{-# DEPRECATED btStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | The time of the most recent update for the task.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btUpdateTime :: Lens.Lens' BundleTask Core.UTCTime
btUpdateTime = Lens.field @"updateTime"
{-# DEPRECATED btUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

instance Core.FromXML BundleTask where
  parseXML x =
    BundleTask'
      Core.<$> (x Core..@ "bundleId")
      Core.<*> (x Core..@? "error")
      Core.<*> (x Core..@ "instanceId")
      Core.<*> (x Core..@ "progress")
      Core.<*> (x Core..@ "startTime")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "storage")
      Core.<*> (x Core..@ "updateTime")

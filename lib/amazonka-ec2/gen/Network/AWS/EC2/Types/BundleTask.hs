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
    btInstanceId,
    btState,
    btProgress,
    btStartTime,
    btBundleId,
    btStorage,
    btUpdateTime,
    btBundleTaskError,
  )
where

import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.BundleTaskState
import Network.AWS.EC2.Types.Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a bundle task.
--
-- /See:/ 'mkBundleTask' smart constructor.
data BundleTask = BundleTask'
  { -- | The ID of the instance associated with this bundle task.
    instanceId :: Lude.Text,
    -- | The state of the task.
    state :: BundleTaskState,
    -- | The level of task completion, as a percent (for example, 20%).
    progress :: Lude.Text,
    -- | The time this task started.
    startTime :: Lude.DateTime,
    -- | The ID of the bundle task.
    bundleId :: Lude.Text,
    -- | The Amazon S3 storage locations.
    storage :: Storage,
    -- | The time of the most recent update for the task.
    updateTime :: Lude.DateTime,
    -- | If the task fails, a description of the error.
    bundleTaskError :: Lude.Maybe BundleTaskError
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BundleTask' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance associated with this bundle task.
-- * 'state' - The state of the task.
-- * 'progress' - The level of task completion, as a percent (for example, 20%).
-- * 'startTime' - The time this task started.
-- * 'bundleId' - The ID of the bundle task.
-- * 'storage' - The Amazon S3 storage locations.
-- * 'updateTime' - The time of the most recent update for the task.
-- * 'bundleTaskError' - If the task fails, a description of the error.
mkBundleTask ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'state'
  BundleTaskState ->
  -- | 'progress'
  Lude.Text ->
  -- | 'startTime'
  Lude.DateTime ->
  -- | 'bundleId'
  Lude.Text ->
  -- | 'storage'
  Storage ->
  -- | 'updateTime'
  Lude.DateTime ->
  BundleTask
mkBundleTask
  pInstanceId_
  pState_
  pProgress_
  pStartTime_
  pBundleId_
  pStorage_
  pUpdateTime_ =
    BundleTask'
      { instanceId = pInstanceId_,
        state = pState_,
        progress = pProgress_,
        startTime = pStartTime_,
        bundleId = pBundleId_,
        storage = pStorage_,
        updateTime = pUpdateTime_,
        bundleTaskError = Lude.Nothing
      }

-- | The ID of the instance associated with this bundle task.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btInstanceId :: Lens.Lens' BundleTask Lude.Text
btInstanceId = Lens.lens (instanceId :: BundleTask -> Lude.Text) (\s a -> s {instanceId = a} :: BundleTask)
{-# DEPRECATED btInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The state of the task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btState :: Lens.Lens' BundleTask BundleTaskState
btState = Lens.lens (state :: BundleTask -> BundleTaskState) (\s a -> s {state = a} :: BundleTask)
{-# DEPRECATED btState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The level of task completion, as a percent (for example, 20%).
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btProgress :: Lens.Lens' BundleTask Lude.Text
btProgress = Lens.lens (progress :: BundleTask -> Lude.Text) (\s a -> s {progress = a} :: BundleTask)
{-# DEPRECATED btProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The time this task started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStartTime :: Lens.Lens' BundleTask Lude.DateTime
btStartTime = Lens.lens (startTime :: BundleTask -> Lude.DateTime) (\s a -> s {startTime = a} :: BundleTask)
{-# DEPRECATED btStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleId :: Lens.Lens' BundleTask Lude.Text
btBundleId = Lens.lens (bundleId :: BundleTask -> Lude.Text) (\s a -> s {bundleId = a} :: BundleTask)
{-# DEPRECATED btBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The Amazon S3 storage locations.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btStorage :: Lens.Lens' BundleTask Storage
btStorage = Lens.lens (storage :: BundleTask -> Storage) (\s a -> s {storage = a} :: BundleTask)
{-# DEPRECATED btStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | The time of the most recent update for the task.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btUpdateTime :: Lens.Lens' BundleTask Lude.DateTime
btUpdateTime = Lens.lens (updateTime :: BundleTask -> Lude.DateTime) (\s a -> s {updateTime = a} :: BundleTask)
{-# DEPRECATED btUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | If the task fails, a description of the error.
--
-- /Note:/ Consider using 'bundleTaskError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
btBundleTaskError :: Lens.Lens' BundleTask (Lude.Maybe BundleTaskError)
btBundleTaskError = Lens.lens (bundleTaskError :: BundleTask -> Lude.Maybe BundleTaskError) (\s a -> s {bundleTaskError = a} :: BundleTask)
{-# DEPRECATED btBundleTaskError "Use generic-lens or generic-optics with 'bundleTaskError' instead." #-}

instance Lude.FromXML BundleTask where
  parseXML x =
    BundleTask'
      Lude.<$> (x Lude..@ "instanceId")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "progress")
      Lude.<*> (x Lude..@ "startTime")
      Lude.<*> (x Lude..@ "bundleId")
      Lude.<*> (x Lude..@ "storage")
      Lude.<*> (x Lude..@ "updateTime")
      Lude.<*> (x Lude..@? "error")

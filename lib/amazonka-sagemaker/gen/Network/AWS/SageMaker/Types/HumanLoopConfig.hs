{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopConfig
  ( HumanLoopConfig (..),

    -- * Smart constructor
    mkHumanLoopConfig,

    -- * Lenses
    hlcTaskKeywords,
    hlcPublicWorkforceTaskPrice,
    hlcTaskDescription,
    hlcWorkteamARN,
    hlcTaskTimeLimitInSeconds,
    hlcTaskTitle,
    hlcTaskAvailabilityLifetimeInSeconds,
    hlcHumanTaskUiARN,
    hlcTaskCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice

-- | Describes the work to be performed by human workers.
--
-- /See:/ 'mkHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | Keywords used to describe the task so that workers can discover the task.
    taskKeywords :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    publicWorkforceTaskPrice :: Lude.Maybe PublicWorkforceTaskPrice,
    -- | A description for the human worker task.
    taskDescription :: Lude.Text,
    -- | Amazon Resource Name (ARN) of a team of workers.
    workteamARN :: Lude.Text,
    -- | The amount of time that a worker has to complete a task. The default value is 3,600 seconds (1 hour)
    taskTimeLimitInSeconds :: Lude.Maybe Lude.Natural,
    -- | A title for the human worker task.
    taskTitle :: Lude.Text,
    -- | The length of time that a task remains available for review by human workers.
    taskAvailabilityLifetimeInSeconds :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    humanTaskUiARN :: Lude.Text,
    -- | The number of distinct workers who will perform the same task on each object. For example, if @TaskCount@ is set to @3@ for an image classification labeling job, three workers will classify each input image. Increasing @TaskCount@ can improve label accuracy.
    taskCount :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopConfig' with the minimum fields required to make a request.
--
-- * 'taskKeywords' - Keywords used to describe the task so that workers can discover the task.
-- * 'publicWorkforceTaskPrice' -
-- * 'taskDescription' - A description for the human worker task.
-- * 'workteamARN' - Amazon Resource Name (ARN) of a team of workers.
-- * 'taskTimeLimitInSeconds' - The amount of time that a worker has to complete a task. The default value is 3,600 seconds (1 hour)
-- * 'taskTitle' - A title for the human worker task.
-- * 'taskAvailabilityLifetimeInSeconds' - The length of time that a task remains available for review by human workers.
-- * 'humanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface.
-- * 'taskCount' - The number of distinct workers who will perform the same task on each object. For example, if @TaskCount@ is set to @3@ for an image classification labeling job, three workers will classify each input image. Increasing @TaskCount@ can improve label accuracy.
mkHumanLoopConfig ::
  -- | 'taskDescription'
  Lude.Text ->
  -- | 'workteamARN'
  Lude.Text ->
  -- | 'taskTitle'
  Lude.Text ->
  -- | 'humanTaskUiARN'
  Lude.Text ->
  -- | 'taskCount'
  Lude.Natural ->
  HumanLoopConfig
mkHumanLoopConfig
  pTaskDescription_
  pWorkteamARN_
  pTaskTitle_
  pHumanTaskUiARN_
  pTaskCount_ =
    HumanLoopConfig'
      { taskKeywords = Lude.Nothing,
        publicWorkforceTaskPrice = Lude.Nothing,
        taskDescription = pTaskDescription_,
        workteamARN = pWorkteamARN_,
        taskTimeLimitInSeconds = Lude.Nothing,
        taskTitle = pTaskTitle_,
        taskAvailabilityLifetimeInSeconds = Lude.Nothing,
        humanTaskUiARN = pHumanTaskUiARN_,
        taskCount = pTaskCount_
      }

-- | Keywords used to describe the task so that workers can discover the task.
--
-- /Note:/ Consider using 'taskKeywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskKeywords :: Lens.Lens' HumanLoopConfig (Lude.Maybe (Lude.NonEmpty Lude.Text))
hlcTaskKeywords = Lens.lens (taskKeywords :: HumanLoopConfig -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {taskKeywords = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskKeywords "Use generic-lens or generic-optics with 'taskKeywords' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'publicWorkforceTaskPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcPublicWorkforceTaskPrice :: Lens.Lens' HumanLoopConfig (Lude.Maybe PublicWorkforceTaskPrice)
hlcPublicWorkforceTaskPrice = Lens.lens (publicWorkforceTaskPrice :: HumanLoopConfig -> Lude.Maybe PublicWorkforceTaskPrice) (\s a -> s {publicWorkforceTaskPrice = a} :: HumanLoopConfig)
{-# DEPRECATED hlcPublicWorkforceTaskPrice "Use generic-lens or generic-optics with 'publicWorkforceTaskPrice' instead." #-}

-- | A description for the human worker task.
--
-- /Note:/ Consider using 'taskDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskDescription :: Lens.Lens' HumanLoopConfig Lude.Text
hlcTaskDescription = Lens.lens (taskDescription :: HumanLoopConfig -> Lude.Text) (\s a -> s {taskDescription = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskDescription "Use generic-lens or generic-optics with 'taskDescription' instead." #-}

-- | Amazon Resource Name (ARN) of a team of workers.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcWorkteamARN :: Lens.Lens' HumanLoopConfig Lude.Text
hlcWorkteamARN = Lens.lens (workteamARN :: HumanLoopConfig -> Lude.Text) (\s a -> s {workteamARN = a} :: HumanLoopConfig)
{-# DEPRECATED hlcWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | The amount of time that a worker has to complete a task. The default value is 3,600 seconds (1 hour)
--
-- /Note:/ Consider using 'taskTimeLimitInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskTimeLimitInSeconds :: Lens.Lens' HumanLoopConfig (Lude.Maybe Lude.Natural)
hlcTaskTimeLimitInSeconds = Lens.lens (taskTimeLimitInSeconds :: HumanLoopConfig -> Lude.Maybe Lude.Natural) (\s a -> s {taskTimeLimitInSeconds = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskTimeLimitInSeconds "Use generic-lens or generic-optics with 'taskTimeLimitInSeconds' instead." #-}

-- | A title for the human worker task.
--
-- /Note:/ Consider using 'taskTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskTitle :: Lens.Lens' HumanLoopConfig Lude.Text
hlcTaskTitle = Lens.lens (taskTitle :: HumanLoopConfig -> Lude.Text) (\s a -> s {taskTitle = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskTitle "Use generic-lens or generic-optics with 'taskTitle' instead." #-}

-- | The length of time that a task remains available for review by human workers.
--
-- /Note:/ Consider using 'taskAvailabilityLifetimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskAvailabilityLifetimeInSeconds :: Lens.Lens' HumanLoopConfig (Lude.Maybe Lude.Natural)
hlcTaskAvailabilityLifetimeInSeconds = Lens.lens (taskAvailabilityLifetimeInSeconds :: HumanLoopConfig -> Lude.Maybe Lude.Natural) (\s a -> s {taskAvailabilityLifetimeInSeconds = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskAvailabilityLifetimeInSeconds "Use generic-lens or generic-optics with 'taskAvailabilityLifetimeInSeconds' instead." #-}

-- | The Amazon Resource Name (ARN) of the human task user interface.
--
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcHumanTaskUiARN :: Lens.Lens' HumanLoopConfig Lude.Text
hlcHumanTaskUiARN = Lens.lens (humanTaskUiARN :: HumanLoopConfig -> Lude.Text) (\s a -> s {humanTaskUiARN = a} :: HumanLoopConfig)
{-# DEPRECATED hlcHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

-- | The number of distinct workers who will perform the same task on each object. For example, if @TaskCount@ is set to @3@ for an image classification labeling job, three workers will classify each input image. Increasing @TaskCount@ can improve label accuracy.
--
-- /Note:/ Consider using 'taskCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlcTaskCount :: Lens.Lens' HumanLoopConfig Lude.Natural
hlcTaskCount = Lens.lens (taskCount :: HumanLoopConfig -> Lude.Natural) (\s a -> s {taskCount = a} :: HumanLoopConfig)
{-# DEPRECATED hlcTaskCount "Use generic-lens or generic-optics with 'taskCount' instead." #-}

instance Lude.FromJSON HumanLoopConfig where
  parseJSON =
    Lude.withObject
      "HumanLoopConfig"
      ( \x ->
          HumanLoopConfig'
            Lude.<$> (x Lude..:? "TaskKeywords")
            Lude.<*> (x Lude..:? "PublicWorkforceTaskPrice")
            Lude.<*> (x Lude..: "TaskDescription")
            Lude.<*> (x Lude..: "WorkteamArn")
            Lude.<*> (x Lude..:? "TaskTimeLimitInSeconds")
            Lude.<*> (x Lude..: "TaskTitle")
            Lude.<*> (x Lude..:? "TaskAvailabilityLifetimeInSeconds")
            Lude.<*> (x Lude..: "HumanTaskUiArn")
            Lude.<*> (x Lude..: "TaskCount")
      )

instance Lude.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TaskKeywords" Lude..=) Lude.<$> taskKeywords,
            ("PublicWorkforceTaskPrice" Lude..=)
              Lude.<$> publicWorkforceTaskPrice,
            Lude.Just ("TaskDescription" Lude..= taskDescription),
            Lude.Just ("WorkteamArn" Lude..= workteamARN),
            ("TaskTimeLimitInSeconds" Lude..=) Lude.<$> taskTimeLimitInSeconds,
            Lude.Just ("TaskTitle" Lude..= taskTitle),
            ("TaskAvailabilityLifetimeInSeconds" Lude..=)
              Lude.<$> taskAvailabilityLifetimeInSeconds,
            Lude.Just ("HumanTaskUiArn" Lude..= humanTaskUiARN),
            Lude.Just ("TaskCount" Lude..= taskCount)
          ]
      )

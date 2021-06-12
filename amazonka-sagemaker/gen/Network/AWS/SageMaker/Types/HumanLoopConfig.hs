{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice

-- | Describes the work to be performed by human workers.
--
-- /See:/ 'newHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { -- | Keywords used to describe the task so that workers can discover the
    -- task.
    taskKeywords :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The amount of time that a worker has to complete a task. The default
    -- value is 3,600 seconds (1 hour).
    taskTimeLimitInSeconds :: Core.Maybe Core.Natural,
    -- | The length of time that a task remains available for review by human
    -- workers.
    taskAvailabilityLifetimeInSeconds :: Core.Maybe Core.Natural,
    publicWorkforceTaskPrice :: Core.Maybe PublicWorkforceTaskPrice,
    -- | Amazon Resource Name (ARN) of a team of workers. To learn more about the
    -- types of workforces and work teams you can create and use with Amazon
    -- A2I, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-management.html Create and Manage Workforces>.
    workteamArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    --
    -- You can use standard HTML and Crowd HTML Elements to create a custom
    -- worker task template. You use this template to create a human task UI.
    --
    -- To learn how to create a custom HTML template, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-custom-templates.html Create Custom Worker Task Template>.
    --
    -- To learn how to create a human task UI, which is a worker task template
    -- that can be used in a flow definition, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-worker-template-console.html Create and Delete a Worker Task Templates>.
    humanTaskUiArn :: Core.Text,
    -- | A title for the human worker task.
    taskTitle :: Core.Text,
    -- | A description for the human worker task.
    taskDescription :: Core.Text,
    -- | The number of distinct workers who will perform the same task on each
    -- object. For example, if @TaskCount@ is set to @3@ for an image
    -- classification labeling job, three workers will classify each input
    -- image. Increasing @TaskCount@ can improve label accuracy.
    taskCount :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HumanLoopConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskKeywords', 'humanLoopConfig_taskKeywords' - Keywords used to describe the task so that workers can discover the
-- task.
--
-- 'taskTimeLimitInSeconds', 'humanLoopConfig_taskTimeLimitInSeconds' - The amount of time that a worker has to complete a task. The default
-- value is 3,600 seconds (1 hour).
--
-- 'taskAvailabilityLifetimeInSeconds', 'humanLoopConfig_taskAvailabilityLifetimeInSeconds' - The length of time that a task remains available for review by human
-- workers.
--
-- 'publicWorkforceTaskPrice', 'humanLoopConfig_publicWorkforceTaskPrice' - Undocumented member.
--
-- 'workteamArn', 'humanLoopConfig_workteamArn' - Amazon Resource Name (ARN) of a team of workers. To learn more about the
-- types of workforces and work teams you can create and use with Amazon
-- A2I, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-management.html Create and Manage Workforces>.
--
-- 'humanTaskUiArn', 'humanLoopConfig_humanTaskUiArn' - The Amazon Resource Name (ARN) of the human task user interface.
--
-- You can use standard HTML and Crowd HTML Elements to create a custom
-- worker task template. You use this template to create a human task UI.
--
-- To learn how to create a custom HTML template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-custom-templates.html Create Custom Worker Task Template>.
--
-- To learn how to create a human task UI, which is a worker task template
-- that can be used in a flow definition, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-worker-template-console.html Create and Delete a Worker Task Templates>.
--
-- 'taskTitle', 'humanLoopConfig_taskTitle' - A title for the human worker task.
--
-- 'taskDescription', 'humanLoopConfig_taskDescription' - A description for the human worker task.
--
-- 'taskCount', 'humanLoopConfig_taskCount' - The number of distinct workers who will perform the same task on each
-- object. For example, if @TaskCount@ is set to @3@ for an image
-- classification labeling job, three workers will classify each input
-- image. Increasing @TaskCount@ can improve label accuracy.
newHumanLoopConfig ::
  -- | 'workteamArn'
  Core.Text ->
  -- | 'humanTaskUiArn'
  Core.Text ->
  -- | 'taskTitle'
  Core.Text ->
  -- | 'taskDescription'
  Core.Text ->
  -- | 'taskCount'
  Core.Natural ->
  HumanLoopConfig
newHumanLoopConfig
  pWorkteamArn_
  pHumanTaskUiArn_
  pTaskTitle_
  pTaskDescription_
  pTaskCount_ =
    HumanLoopConfig'
      { taskKeywords = Core.Nothing,
        taskTimeLimitInSeconds = Core.Nothing,
        taskAvailabilityLifetimeInSeconds = Core.Nothing,
        publicWorkforceTaskPrice = Core.Nothing,
        workteamArn = pWorkteamArn_,
        humanTaskUiArn = pHumanTaskUiArn_,
        taskTitle = pTaskTitle_,
        taskDescription = pTaskDescription_,
        taskCount = pTaskCount_
      }

-- | Keywords used to describe the task so that workers can discover the
-- task.
humanLoopConfig_taskKeywords :: Lens.Lens' HumanLoopConfig (Core.Maybe (Core.NonEmpty Core.Text))
humanLoopConfig_taskKeywords = Lens.lens (\HumanLoopConfig' {taskKeywords} -> taskKeywords) (\s@HumanLoopConfig' {} a -> s {taskKeywords = a} :: HumanLoopConfig) Core.. Lens.mapping Lens._Coerce

-- | The amount of time that a worker has to complete a task. The default
-- value is 3,600 seconds (1 hour).
humanLoopConfig_taskTimeLimitInSeconds :: Lens.Lens' HumanLoopConfig (Core.Maybe Core.Natural)
humanLoopConfig_taskTimeLimitInSeconds = Lens.lens (\HumanLoopConfig' {taskTimeLimitInSeconds} -> taskTimeLimitInSeconds) (\s@HumanLoopConfig' {} a -> s {taskTimeLimitInSeconds = a} :: HumanLoopConfig)

-- | The length of time that a task remains available for review by human
-- workers.
humanLoopConfig_taskAvailabilityLifetimeInSeconds :: Lens.Lens' HumanLoopConfig (Core.Maybe Core.Natural)
humanLoopConfig_taskAvailabilityLifetimeInSeconds = Lens.lens (\HumanLoopConfig' {taskAvailabilityLifetimeInSeconds} -> taskAvailabilityLifetimeInSeconds) (\s@HumanLoopConfig' {} a -> s {taskAvailabilityLifetimeInSeconds = a} :: HumanLoopConfig)

-- | Undocumented member.
humanLoopConfig_publicWorkforceTaskPrice :: Lens.Lens' HumanLoopConfig (Core.Maybe PublicWorkforceTaskPrice)
humanLoopConfig_publicWorkforceTaskPrice = Lens.lens (\HumanLoopConfig' {publicWorkforceTaskPrice} -> publicWorkforceTaskPrice) (\s@HumanLoopConfig' {} a -> s {publicWorkforceTaskPrice = a} :: HumanLoopConfig)

-- | Amazon Resource Name (ARN) of a team of workers. To learn more about the
-- types of workforces and work teams you can create and use with Amazon
-- A2I, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-management.html Create and Manage Workforces>.
humanLoopConfig_workteamArn :: Lens.Lens' HumanLoopConfig Core.Text
humanLoopConfig_workteamArn = Lens.lens (\HumanLoopConfig' {workteamArn} -> workteamArn) (\s@HumanLoopConfig' {} a -> s {workteamArn = a} :: HumanLoopConfig)

-- | The Amazon Resource Name (ARN) of the human task user interface.
--
-- You can use standard HTML and Crowd HTML Elements to create a custom
-- worker task template. You use this template to create a human task UI.
--
-- To learn how to create a custom HTML template, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-custom-templates.html Create Custom Worker Task Template>.
--
-- To learn how to create a human task UI, which is a worker task template
-- that can be used in a flow definition, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-worker-template-console.html Create and Delete a Worker Task Templates>.
humanLoopConfig_humanTaskUiArn :: Lens.Lens' HumanLoopConfig Core.Text
humanLoopConfig_humanTaskUiArn = Lens.lens (\HumanLoopConfig' {humanTaskUiArn} -> humanTaskUiArn) (\s@HumanLoopConfig' {} a -> s {humanTaskUiArn = a} :: HumanLoopConfig)

-- | A title for the human worker task.
humanLoopConfig_taskTitle :: Lens.Lens' HumanLoopConfig Core.Text
humanLoopConfig_taskTitle = Lens.lens (\HumanLoopConfig' {taskTitle} -> taskTitle) (\s@HumanLoopConfig' {} a -> s {taskTitle = a} :: HumanLoopConfig)

-- | A description for the human worker task.
humanLoopConfig_taskDescription :: Lens.Lens' HumanLoopConfig Core.Text
humanLoopConfig_taskDescription = Lens.lens (\HumanLoopConfig' {taskDescription} -> taskDescription) (\s@HumanLoopConfig' {} a -> s {taskDescription = a} :: HumanLoopConfig)

-- | The number of distinct workers who will perform the same task on each
-- object. For example, if @TaskCount@ is set to @3@ for an image
-- classification labeling job, three workers will classify each input
-- image. Increasing @TaskCount@ can improve label accuracy.
humanLoopConfig_taskCount :: Lens.Lens' HumanLoopConfig Core.Natural
humanLoopConfig_taskCount = Lens.lens (\HumanLoopConfig' {taskCount} -> taskCount) (\s@HumanLoopConfig' {} a -> s {taskCount = a} :: HumanLoopConfig)

instance Core.FromJSON HumanLoopConfig where
  parseJSON =
    Core.withObject
      "HumanLoopConfig"
      ( \x ->
          HumanLoopConfig'
            Core.<$> (x Core..:? "TaskKeywords")
            Core.<*> (x Core..:? "TaskTimeLimitInSeconds")
            Core.<*> (x Core..:? "TaskAvailabilityLifetimeInSeconds")
            Core.<*> (x Core..:? "PublicWorkforceTaskPrice")
            Core.<*> (x Core..: "WorkteamArn")
            Core.<*> (x Core..: "HumanTaskUiArn")
            Core.<*> (x Core..: "TaskTitle")
            Core.<*> (x Core..: "TaskDescription")
            Core.<*> (x Core..: "TaskCount")
      )

instance Core.Hashable HumanLoopConfig

instance Core.NFData HumanLoopConfig

instance Core.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TaskKeywords" Core..=) Core.<$> taskKeywords,
            ("TaskTimeLimitInSeconds" Core..=)
              Core.<$> taskTimeLimitInSeconds,
            ("TaskAvailabilityLifetimeInSeconds" Core..=)
              Core.<$> taskAvailabilityLifetimeInSeconds,
            ("PublicWorkforceTaskPrice" Core..=)
              Core.<$> publicWorkforceTaskPrice,
            Core.Just ("WorkteamArn" Core..= workteamArn),
            Core.Just ("HumanTaskUiArn" Core..= humanTaskUiArn),
            Core.Just ("TaskTitle" Core..= taskTitle),
            Core.Just
              ("TaskDescription" Core..= taskDescription),
            Core.Just ("TaskCount" Core..= taskCount)
          ]
      )

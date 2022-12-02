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
-- Module      : Amazonka.SageMaker.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HumanLoopConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.PublicWorkforceTaskPrice

-- | Describes the work to be performed by human workers.
--
-- /See:/ 'newHumanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { publicWorkforceTaskPrice :: Prelude.Maybe PublicWorkforceTaskPrice,
    -- | Keywords used to describe the task so that workers can discover the
    -- task.
    taskKeywords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The amount of time that a worker has to complete a task. The default
    -- value is 3,600 seconds (1 hour).
    taskTimeLimitInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The length of time that a task remains available for review by human
    -- workers.
    taskAvailabilityLifetimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Amazon Resource Name (ARN) of a team of workers. To learn more about the
    -- types of workforces and work teams you can create and use with Amazon
    -- A2I, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-management.html Create and Manage Workforces>.
    workteamArn :: Prelude.Text,
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
    humanTaskUiArn :: Prelude.Text,
    -- | A title for the human worker task.
    taskTitle :: Prelude.Text,
    -- | A description for the human worker task.
    taskDescription :: Prelude.Text,
    -- | The number of distinct workers who will perform the same task on each
    -- object. For example, if @TaskCount@ is set to @3@ for an image
    -- classification labeling job, three workers will classify each input
    -- image. Increasing @TaskCount@ can improve label accuracy.
    taskCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicWorkforceTaskPrice', 'humanLoopConfig_publicWorkforceTaskPrice' - Undocumented member.
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
  Prelude.Text ->
  -- | 'humanTaskUiArn'
  Prelude.Text ->
  -- | 'taskTitle'
  Prelude.Text ->
  -- | 'taskDescription'
  Prelude.Text ->
  -- | 'taskCount'
  Prelude.Natural ->
  HumanLoopConfig
newHumanLoopConfig
  pWorkteamArn_
  pHumanTaskUiArn_
  pTaskTitle_
  pTaskDescription_
  pTaskCount_ =
    HumanLoopConfig'
      { publicWorkforceTaskPrice =
          Prelude.Nothing,
        taskKeywords = Prelude.Nothing,
        taskTimeLimitInSeconds = Prelude.Nothing,
        taskAvailabilityLifetimeInSeconds = Prelude.Nothing,
        workteamArn = pWorkteamArn_,
        humanTaskUiArn = pHumanTaskUiArn_,
        taskTitle = pTaskTitle_,
        taskDescription = pTaskDescription_,
        taskCount = pTaskCount_
      }

-- | Undocumented member.
humanLoopConfig_publicWorkforceTaskPrice :: Lens.Lens' HumanLoopConfig (Prelude.Maybe PublicWorkforceTaskPrice)
humanLoopConfig_publicWorkforceTaskPrice = Lens.lens (\HumanLoopConfig' {publicWorkforceTaskPrice} -> publicWorkforceTaskPrice) (\s@HumanLoopConfig' {} a -> s {publicWorkforceTaskPrice = a} :: HumanLoopConfig)

-- | Keywords used to describe the task so that workers can discover the
-- task.
humanLoopConfig_taskKeywords :: Lens.Lens' HumanLoopConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
humanLoopConfig_taskKeywords = Lens.lens (\HumanLoopConfig' {taskKeywords} -> taskKeywords) (\s@HumanLoopConfig' {} a -> s {taskKeywords = a} :: HumanLoopConfig) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time that a worker has to complete a task. The default
-- value is 3,600 seconds (1 hour).
humanLoopConfig_taskTimeLimitInSeconds :: Lens.Lens' HumanLoopConfig (Prelude.Maybe Prelude.Natural)
humanLoopConfig_taskTimeLimitInSeconds = Lens.lens (\HumanLoopConfig' {taskTimeLimitInSeconds} -> taskTimeLimitInSeconds) (\s@HumanLoopConfig' {} a -> s {taskTimeLimitInSeconds = a} :: HumanLoopConfig)

-- | The length of time that a task remains available for review by human
-- workers.
humanLoopConfig_taskAvailabilityLifetimeInSeconds :: Lens.Lens' HumanLoopConfig (Prelude.Maybe Prelude.Natural)
humanLoopConfig_taskAvailabilityLifetimeInSeconds = Lens.lens (\HumanLoopConfig' {taskAvailabilityLifetimeInSeconds} -> taskAvailabilityLifetimeInSeconds) (\s@HumanLoopConfig' {} a -> s {taskAvailabilityLifetimeInSeconds = a} :: HumanLoopConfig)

-- | Amazon Resource Name (ARN) of a team of workers. To learn more about the
-- types of workforces and work teams you can create and use with Amazon
-- A2I, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-management.html Create and Manage Workforces>.
humanLoopConfig_workteamArn :: Lens.Lens' HumanLoopConfig Prelude.Text
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
humanLoopConfig_humanTaskUiArn :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_humanTaskUiArn = Lens.lens (\HumanLoopConfig' {humanTaskUiArn} -> humanTaskUiArn) (\s@HumanLoopConfig' {} a -> s {humanTaskUiArn = a} :: HumanLoopConfig)

-- | A title for the human worker task.
humanLoopConfig_taskTitle :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_taskTitle = Lens.lens (\HumanLoopConfig' {taskTitle} -> taskTitle) (\s@HumanLoopConfig' {} a -> s {taskTitle = a} :: HumanLoopConfig)

-- | A description for the human worker task.
humanLoopConfig_taskDescription :: Lens.Lens' HumanLoopConfig Prelude.Text
humanLoopConfig_taskDescription = Lens.lens (\HumanLoopConfig' {taskDescription} -> taskDescription) (\s@HumanLoopConfig' {} a -> s {taskDescription = a} :: HumanLoopConfig)

-- | The number of distinct workers who will perform the same task on each
-- object. For example, if @TaskCount@ is set to @3@ for an image
-- classification labeling job, three workers will classify each input
-- image. Increasing @TaskCount@ can improve label accuracy.
humanLoopConfig_taskCount :: Lens.Lens' HumanLoopConfig Prelude.Natural
humanLoopConfig_taskCount = Lens.lens (\HumanLoopConfig' {taskCount} -> taskCount) (\s@HumanLoopConfig' {} a -> s {taskCount = a} :: HumanLoopConfig)

instance Data.FromJSON HumanLoopConfig where
  parseJSON =
    Data.withObject
      "HumanLoopConfig"
      ( \x ->
          HumanLoopConfig'
            Prelude.<$> (x Data..:? "PublicWorkforceTaskPrice")
            Prelude.<*> (x Data..:? "TaskKeywords")
            Prelude.<*> (x Data..:? "TaskTimeLimitInSeconds")
            Prelude.<*> (x Data..:? "TaskAvailabilityLifetimeInSeconds")
            Prelude.<*> (x Data..: "WorkteamArn")
            Prelude.<*> (x Data..: "HumanTaskUiArn")
            Prelude.<*> (x Data..: "TaskTitle")
            Prelude.<*> (x Data..: "TaskDescription")
            Prelude.<*> (x Data..: "TaskCount")
      )

instance Prelude.Hashable HumanLoopConfig where
  hashWithSalt _salt HumanLoopConfig' {..} =
    _salt
      `Prelude.hashWithSalt` publicWorkforceTaskPrice
      `Prelude.hashWithSalt` taskKeywords
      `Prelude.hashWithSalt` taskTimeLimitInSeconds
      `Prelude.hashWithSalt` taskAvailabilityLifetimeInSeconds
      `Prelude.hashWithSalt` workteamArn
      `Prelude.hashWithSalt` humanTaskUiArn
      `Prelude.hashWithSalt` taskTitle
      `Prelude.hashWithSalt` taskDescription
      `Prelude.hashWithSalt` taskCount

instance Prelude.NFData HumanLoopConfig where
  rnf HumanLoopConfig' {..} =
    Prelude.rnf publicWorkforceTaskPrice
      `Prelude.seq` Prelude.rnf taskKeywords
      `Prelude.seq` Prelude.rnf taskTimeLimitInSeconds
      `Prelude.seq` Prelude.rnf taskAvailabilityLifetimeInSeconds
      `Prelude.seq` Prelude.rnf workteamArn
      `Prelude.seq` Prelude.rnf humanTaskUiArn
      `Prelude.seq` Prelude.rnf taskTitle
      `Prelude.seq` Prelude.rnf taskDescription
      `Prelude.seq` Prelude.rnf taskCount

instance Data.ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PublicWorkforceTaskPrice" Data..=)
              Prelude.<$> publicWorkforceTaskPrice,
            ("TaskKeywords" Data..=) Prelude.<$> taskKeywords,
            ("TaskTimeLimitInSeconds" Data..=)
              Prelude.<$> taskTimeLimitInSeconds,
            ("TaskAvailabilityLifetimeInSeconds" Data..=)
              Prelude.<$> taskAvailabilityLifetimeInSeconds,
            Prelude.Just ("WorkteamArn" Data..= workteamArn),
            Prelude.Just
              ("HumanTaskUiArn" Data..= humanTaskUiArn),
            Prelude.Just ("TaskTitle" Data..= taskTitle),
            Prelude.Just
              ("TaskDescription" Data..= taskDescription),
            Prelude.Just ("TaskCount" Data..= taskCount)
          ]
      )

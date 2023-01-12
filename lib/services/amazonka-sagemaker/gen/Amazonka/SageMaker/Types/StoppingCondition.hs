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
-- Module      : Amazonka.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.StoppingCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a limit to how long a model training job or model compilation
-- job can run. It also specifies how long a managed spot training job has
-- to complete. When the job reaches the time limit, SageMaker ends the
-- training or compilation job. Use this API to cap model training costs.
--
-- To stop a training job, SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- The training algorithms provided by SageMaker automatically save the
-- intermediate results of a model training job when possible. This attempt
-- to save artifacts is only a best effort case as model might not be in a
-- state from which it can be saved. For example, if training has just
-- started, the model might not be ready to save. When saved, this
-- intermediate data is a valid model artifact. You can use it to create a
-- model with @CreateModel@.
--
-- The Neural Topic Model (NTM) currently does not support saving
-- intermediate model artifacts. When training NTMs, make sure that the
-- maximum runtime is sufficient for the training job to complete.
--
-- /See:/ 'newStoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { -- | The maximum length of time, in seconds, that a training or compilation
    -- job can run before it is stopped.
    --
    -- For compilation jobs, if the job does not complete during this time, a
    -- @TimeOut@ error is generated. We recommend starting with 900 seconds and
    -- increasing as necessary based on your model.
    --
    -- For all other jobs, if the job does not complete during this time,
    -- SageMaker ends the job. When @RetryStrategy@ is specified in the job
    -- request, @MaxRuntimeInSeconds@ specifies the maximum time for all of the
    -- attempts in total, not each individual attempt. The default value is 1
    -- day. The maximum value is 28 days.
    --
    -- The maximum time that a @TrainingJob@ can run in total, including any
    -- time spent publishing metrics or archiving and uploading models after it
    -- has been stopped, is 30 days.
    maxRuntimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of time, in seconds, that a managed Spot training job
    -- has to complete. It is the amount of time spent waiting for Spot
    -- capacity plus the amount of time the job can run. It must be equal to or
    -- greater than @MaxRuntimeInSeconds@. If the job does not complete during
    -- this time, SageMaker ends the job.
    --
    -- When @RetryStrategy@ is specified in the job request,
    -- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
    -- attempts in total, not each individual attempt.
    maxWaitTimeInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'stoppingCondition_maxRuntimeInSeconds' - The maximum length of time, in seconds, that a training or compilation
-- job can run before it is stopped.
--
-- For compilation jobs, if the job does not complete during this time, a
-- @TimeOut@ error is generated. We recommend starting with 900 seconds and
-- increasing as necessary based on your model.
--
-- For all other jobs, if the job does not complete during this time,
-- SageMaker ends the job. When @RetryStrategy@ is specified in the job
-- request, @MaxRuntimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt. The default value is 1
-- day. The maximum value is 28 days.
--
-- The maximum time that a @TrainingJob@ can run in total, including any
-- time spent publishing metrics or archiving and uploading models after it
-- has been stopped, is 30 days.
--
-- 'maxWaitTimeInSeconds', 'stoppingCondition_maxWaitTimeInSeconds' - The maximum length of time, in seconds, that a managed Spot training job
-- has to complete. It is the amount of time spent waiting for Spot
-- capacity plus the amount of time the job can run. It must be equal to or
-- greater than @MaxRuntimeInSeconds@. If the job does not complete during
-- this time, SageMaker ends the job.
--
-- When @RetryStrategy@ is specified in the job request,
-- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt.
newStoppingCondition ::
  StoppingCondition
newStoppingCondition =
  StoppingCondition'
    { maxRuntimeInSeconds =
        Prelude.Nothing,
      maxWaitTimeInSeconds = Prelude.Nothing
    }

-- | The maximum length of time, in seconds, that a training or compilation
-- job can run before it is stopped.
--
-- For compilation jobs, if the job does not complete during this time, a
-- @TimeOut@ error is generated. We recommend starting with 900 seconds and
-- increasing as necessary based on your model.
--
-- For all other jobs, if the job does not complete during this time,
-- SageMaker ends the job. When @RetryStrategy@ is specified in the job
-- request, @MaxRuntimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt. The default value is 1
-- day. The maximum value is 28 days.
--
-- The maximum time that a @TrainingJob@ can run in total, including any
-- time spent publishing metrics or archiving and uploading models after it
-- has been stopped, is 30 days.
stoppingCondition_maxRuntimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxRuntimeInSeconds = Lens.lens (\StoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@StoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: StoppingCondition)

-- | The maximum length of time, in seconds, that a managed Spot training job
-- has to complete. It is the amount of time spent waiting for Spot
-- capacity plus the amount of time the job can run. It must be equal to or
-- greater than @MaxRuntimeInSeconds@. If the job does not complete during
-- this time, SageMaker ends the job.
--
-- When @RetryStrategy@ is specified in the job request,
-- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt.
stoppingCondition_maxWaitTimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxWaitTimeInSeconds = Lens.lens (\StoppingCondition' {maxWaitTimeInSeconds} -> maxWaitTimeInSeconds) (\s@StoppingCondition' {} a -> s {maxWaitTimeInSeconds = a} :: StoppingCondition)

instance Data.FromJSON StoppingCondition where
  parseJSON =
    Data.withObject
      "StoppingCondition"
      ( \x ->
          StoppingCondition'
            Prelude.<$> (x Data..:? "MaxRuntimeInSeconds")
            Prelude.<*> (x Data..:? "MaxWaitTimeInSeconds")
      )

instance Prelude.Hashable StoppingCondition where
  hashWithSalt _salt StoppingCondition' {..} =
    _salt `Prelude.hashWithSalt` maxRuntimeInSeconds
      `Prelude.hashWithSalt` maxWaitTimeInSeconds

instance Prelude.NFData StoppingCondition where
  rnf StoppingCondition' {..} =
    Prelude.rnf maxRuntimeInSeconds
      `Prelude.seq` Prelude.rnf maxWaitTimeInSeconds

instance Data.ToJSON StoppingCondition where
  toJSON StoppingCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxRuntimeInSeconds" Data..=)
              Prelude.<$> maxRuntimeInSeconds,
            ("MaxWaitTimeInSeconds" Data..=)
              Prelude.<$> maxWaitTimeInSeconds
          ]
      )

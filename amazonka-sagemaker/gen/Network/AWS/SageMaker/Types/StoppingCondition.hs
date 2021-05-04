{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.StoppingCondition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a limit to how long a model training or compilation job can
-- run. It also specifies how long you are willing to wait for a managed
-- spot training job to complete. When the job reaches the time limit,
-- Amazon SageMaker ends the training or compilation job. Use this API to
-- cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
--
-- The training algorithms provided by Amazon SageMaker automatically save
-- the intermediate results of a model training job when possible. This
-- attempt to save artifacts is only a best effort case as model might not
-- be in a state from which it can be saved. For example, if training has
-- just started, the model might not be ready to save. When saved, this
-- intermediate data is a valid model artifact. You can use it to create a
-- model with @CreateModel@.
--
-- The Neural Topic Model (NTM) currently does not support saving
-- intermediate model artifacts. When training NTMs, make sure that the
-- maximum runtime is sufficient for the training job to complete.
--
-- /See:/ 'newStoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { -- | The maximum length of time, in seconds, that the training or compilation
    -- job can run. If job does not complete during this time, Amazon SageMaker
    -- ends the job. If value is not specified, default value is 1 day. The
    -- maximum value is 28 days.
    maxRuntimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of time, in seconds, how long you are willing to wait
    -- for a managed spot training job to complete. It is the amount of time
    -- spent waiting for Spot capacity plus the amount of time the training job
    -- runs. It must be equal to or greater than @MaxRuntimeInSeconds@.
    maxWaitTimeInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'stoppingCondition_maxRuntimeInSeconds' - The maximum length of time, in seconds, that the training or compilation
-- job can run. If job does not complete during this time, Amazon SageMaker
-- ends the job. If value is not specified, default value is 1 day. The
-- maximum value is 28 days.
--
-- 'maxWaitTimeInSeconds', 'stoppingCondition_maxWaitTimeInSeconds' - The maximum length of time, in seconds, how long you are willing to wait
-- for a managed spot training job to complete. It is the amount of time
-- spent waiting for Spot capacity plus the amount of time the training job
-- runs. It must be equal to or greater than @MaxRuntimeInSeconds@.
newStoppingCondition ::
  StoppingCondition
newStoppingCondition =
  StoppingCondition'
    { maxRuntimeInSeconds =
        Prelude.Nothing,
      maxWaitTimeInSeconds = Prelude.Nothing
    }

-- | The maximum length of time, in seconds, that the training or compilation
-- job can run. If job does not complete during this time, Amazon SageMaker
-- ends the job. If value is not specified, default value is 1 day. The
-- maximum value is 28 days.
stoppingCondition_maxRuntimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxRuntimeInSeconds = Lens.lens (\StoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@StoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: StoppingCondition)

-- | The maximum length of time, in seconds, how long you are willing to wait
-- for a managed spot training job to complete. It is the amount of time
-- spent waiting for Spot capacity plus the amount of time the training job
-- runs. It must be equal to or greater than @MaxRuntimeInSeconds@.
stoppingCondition_maxWaitTimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxWaitTimeInSeconds = Lens.lens (\StoppingCondition' {maxWaitTimeInSeconds} -> maxWaitTimeInSeconds) (\s@StoppingCondition' {} a -> s {maxWaitTimeInSeconds = a} :: StoppingCondition)

instance Prelude.FromJSON StoppingCondition where
  parseJSON =
    Prelude.withObject
      "StoppingCondition"
      ( \x ->
          StoppingCondition'
            Prelude.<$> (x Prelude..:? "MaxRuntimeInSeconds")
            Prelude.<*> (x Prelude..:? "MaxWaitTimeInSeconds")
      )

instance Prelude.Hashable StoppingCondition

instance Prelude.NFData StoppingCondition

instance Prelude.ToJSON StoppingCondition where
  toJSON StoppingCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxRuntimeInSeconds" Prelude..=)
              Prelude.<$> maxRuntimeInSeconds,
            ("MaxWaitTimeInSeconds" Prelude..=)
              Prelude.<$> maxWaitTimeInSeconds
          ]
      )

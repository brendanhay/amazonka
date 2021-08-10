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
-- Module      : Network.AWS.SageMaker.Types.ProcessingStoppingCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingStoppingCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configures conditions under which the processing job should be stopped,
-- such as how long the processing job has been running. After the
-- condition is met, the processing job is stopped.
--
-- /See:/ 'newProcessingStoppingCondition' smart constructor.
data ProcessingStoppingCondition = ProcessingStoppingCondition'
  { -- | Specifies the maximum runtime in seconds.
    maxRuntimeInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingStoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'processingStoppingCondition_maxRuntimeInSeconds' - Specifies the maximum runtime in seconds.
newProcessingStoppingCondition ::
  -- | 'maxRuntimeInSeconds'
  Prelude.Natural ->
  ProcessingStoppingCondition
newProcessingStoppingCondition pMaxRuntimeInSeconds_ =
  ProcessingStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | Specifies the maximum runtime in seconds.
processingStoppingCondition_maxRuntimeInSeconds :: Lens.Lens' ProcessingStoppingCondition Prelude.Natural
processingStoppingCondition_maxRuntimeInSeconds = Lens.lens (\ProcessingStoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@ProcessingStoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: ProcessingStoppingCondition)

instance Core.FromJSON ProcessingStoppingCondition where
  parseJSON =
    Core.withObject
      "ProcessingStoppingCondition"
      ( \x ->
          ProcessingStoppingCondition'
            Prelude.<$> (x Core..: "MaxRuntimeInSeconds")
      )

instance Prelude.Hashable ProcessingStoppingCondition

instance Prelude.NFData ProcessingStoppingCondition

instance Core.ToJSON ProcessingStoppingCondition where
  toJSON ProcessingStoppingCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaxRuntimeInSeconds" Core..= maxRuntimeInSeconds)
          ]
      )

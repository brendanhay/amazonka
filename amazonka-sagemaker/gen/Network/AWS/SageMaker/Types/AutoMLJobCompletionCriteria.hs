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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
--
-- /See:/ 'newAutoMLJobCompletionCriteria' smart constructor.
data AutoMLJobCompletionCriteria = AutoMLJobCompletionCriteria'
  { -- | The maximum time, in seconds, a job is allowed to run.
    maxRuntimePerTrainingJobInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum time, in seconds, an AutoML job is allowed to wait for a
    -- trial to complete. It must be equal to or greater than
    -- MaxRuntimePerTrainingJobInSeconds.
    maxAutoMLJobRuntimeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of times a training job is allowed to run.
    maxCandidates :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLJobCompletionCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimePerTrainingJobInSeconds', 'autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds' - The maximum time, in seconds, a job is allowed to run.
--
-- 'maxAutoMLJobRuntimeInSeconds', 'autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds' - The maximum time, in seconds, an AutoML job is allowed to wait for a
-- trial to complete. It must be equal to or greater than
-- MaxRuntimePerTrainingJobInSeconds.
--
-- 'maxCandidates', 'autoMLJobCompletionCriteria_maxCandidates' - The maximum number of times a training job is allowed to run.
newAutoMLJobCompletionCriteria ::
  AutoMLJobCompletionCriteria
newAutoMLJobCompletionCriteria =
  AutoMLJobCompletionCriteria'
    { maxRuntimePerTrainingJobInSeconds =
        Core.Nothing,
      maxAutoMLJobRuntimeInSeconds = Core.Nothing,
      maxCandidates = Core.Nothing
    }

-- | The maximum time, in seconds, a job is allowed to run.
autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds = Lens.lens (\AutoMLJobCompletionCriteria' {maxRuntimePerTrainingJobInSeconds} -> maxRuntimePerTrainingJobInSeconds) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxRuntimePerTrainingJobInSeconds = a} :: AutoMLJobCompletionCriteria)

-- | The maximum time, in seconds, an AutoML job is allowed to wait for a
-- trial to complete. It must be equal to or greater than
-- MaxRuntimePerTrainingJobInSeconds.
autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds = Lens.lens (\AutoMLJobCompletionCriteria' {maxAutoMLJobRuntimeInSeconds} -> maxAutoMLJobRuntimeInSeconds) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxAutoMLJobRuntimeInSeconds = a} :: AutoMLJobCompletionCriteria)

-- | The maximum number of times a training job is allowed to run.
autoMLJobCompletionCriteria_maxCandidates :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
autoMLJobCompletionCriteria_maxCandidates = Lens.lens (\AutoMLJobCompletionCriteria' {maxCandidates} -> maxCandidates) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxCandidates = a} :: AutoMLJobCompletionCriteria)

instance Core.FromJSON AutoMLJobCompletionCriteria where
  parseJSON =
    Core.withObject
      "AutoMLJobCompletionCriteria"
      ( \x ->
          AutoMLJobCompletionCriteria'
            Core.<$> (x Core..:? "MaxRuntimePerTrainingJobInSeconds")
            Core.<*> (x Core..:? "MaxAutoMLJobRuntimeInSeconds")
            Core.<*> (x Core..:? "MaxCandidates")
      )

instance Core.Hashable AutoMLJobCompletionCriteria

instance Core.NFData AutoMLJobCompletionCriteria

instance Core.ToJSON AutoMLJobCompletionCriteria where
  toJSON AutoMLJobCompletionCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxRuntimePerTrainingJobInSeconds" Core..=)
              Core.<$> maxRuntimePerTrainingJobInSeconds,
            ("MaxAutoMLJobRuntimeInSeconds" Core..=)
              Core.<$> maxAutoMLJobRuntimeInSeconds,
            ("MaxCandidates" Core..=) Core.<$> maxCandidates
          ]
      )

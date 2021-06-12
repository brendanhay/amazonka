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
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatusCounters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ObjectiveStatusCounters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the number of training jobs that this hyperparameter tuning
-- job launched, categorized by the status of their objective metric. The
-- objective metric status shows whether the final objective metric for the
-- training job has been evaluated by the tuning job and used in the
-- hyperparameter tuning process.
--
-- /See:/ 'newObjectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { -- | The number of training jobs whose final objective metric was evaluated
    -- by the hyperparameter tuning job and used in the hyperparameter tuning
    -- process.
    succeeded :: Core.Maybe Core.Natural,
    -- | The number of training jobs that are in progress and pending evaluation
    -- of their final objective metric.
    pending :: Core.Maybe Core.Natural,
    -- | The number of training jobs whose final objective metric was not
    -- evaluated and used in the hyperparameter tuning process. This typically
    -- occurs when the training job failed or did not emit an objective metric.
    failed :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ObjectiveStatusCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeeded', 'objectiveStatusCounters_succeeded' - The number of training jobs whose final objective metric was evaluated
-- by the hyperparameter tuning job and used in the hyperparameter tuning
-- process.
--
-- 'pending', 'objectiveStatusCounters_pending' - The number of training jobs that are in progress and pending evaluation
-- of their final objective metric.
--
-- 'failed', 'objectiveStatusCounters_failed' - The number of training jobs whose final objective metric was not
-- evaluated and used in the hyperparameter tuning process. This typically
-- occurs when the training job failed or did not emit an objective metric.
newObjectiveStatusCounters ::
  ObjectiveStatusCounters
newObjectiveStatusCounters =
  ObjectiveStatusCounters'
    { succeeded = Core.Nothing,
      pending = Core.Nothing,
      failed = Core.Nothing
    }

-- | The number of training jobs whose final objective metric was evaluated
-- by the hyperparameter tuning job and used in the hyperparameter tuning
-- process.
objectiveStatusCounters_succeeded :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
objectiveStatusCounters_succeeded = Lens.lens (\ObjectiveStatusCounters' {succeeded} -> succeeded) (\s@ObjectiveStatusCounters' {} a -> s {succeeded = a} :: ObjectiveStatusCounters)

-- | The number of training jobs that are in progress and pending evaluation
-- of their final objective metric.
objectiveStatusCounters_pending :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
objectiveStatusCounters_pending = Lens.lens (\ObjectiveStatusCounters' {pending} -> pending) (\s@ObjectiveStatusCounters' {} a -> s {pending = a} :: ObjectiveStatusCounters)

-- | The number of training jobs whose final objective metric was not
-- evaluated and used in the hyperparameter tuning process. This typically
-- occurs when the training job failed or did not emit an objective metric.
objectiveStatusCounters_failed :: Lens.Lens' ObjectiveStatusCounters (Core.Maybe Core.Natural)
objectiveStatusCounters_failed = Lens.lens (\ObjectiveStatusCounters' {failed} -> failed) (\s@ObjectiveStatusCounters' {} a -> s {failed = a} :: ObjectiveStatusCounters)

instance Core.FromJSON ObjectiveStatusCounters where
  parseJSON =
    Core.withObject
      "ObjectiveStatusCounters"
      ( \x ->
          ObjectiveStatusCounters'
            Core.<$> (x Core..:? "Succeeded")
            Core.<*> (x Core..:? "Pending")
            Core.<*> (x Core..:? "Failed")
      )

instance Core.Hashable ObjectiveStatusCounters

instance Core.NFData ObjectiveStatusCounters

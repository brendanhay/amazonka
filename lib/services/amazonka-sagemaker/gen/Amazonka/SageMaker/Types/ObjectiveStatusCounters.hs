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
-- Module      : Amazonka.SageMaker.Types.ObjectiveStatusCounters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ObjectiveStatusCounters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the number of training jobs that this hyperparameter tuning
-- job launched, categorized by the status of their objective metric. The
-- objective metric status shows whether the final objective metric for the
-- training job has been evaluated by the tuning job and used in the
-- hyperparameter tuning process.
--
-- /See:/ 'newObjectiveStatusCounters' smart constructor.
data ObjectiveStatusCounters = ObjectiveStatusCounters'
  { -- | The number of training jobs whose final objective metric was not
    -- evaluated and used in the hyperparameter tuning process. This typically
    -- occurs when the training job failed or did not emit an objective metric.
    failed :: Prelude.Maybe Prelude.Natural,
    -- | The number of training jobs that are in progress and pending evaluation
    -- of their final objective metric.
    pending :: Prelude.Maybe Prelude.Natural,
    -- | The number of training jobs whose final objective metric was evaluated
    -- by the hyperparameter tuning job and used in the hyperparameter tuning
    -- process.
    succeeded :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectiveStatusCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'objectiveStatusCounters_failed' - The number of training jobs whose final objective metric was not
-- evaluated and used in the hyperparameter tuning process. This typically
-- occurs when the training job failed or did not emit an objective metric.
--
-- 'pending', 'objectiveStatusCounters_pending' - The number of training jobs that are in progress and pending evaluation
-- of their final objective metric.
--
-- 'succeeded', 'objectiveStatusCounters_succeeded' - The number of training jobs whose final objective metric was evaluated
-- by the hyperparameter tuning job and used in the hyperparameter tuning
-- process.
newObjectiveStatusCounters ::
  ObjectiveStatusCounters
newObjectiveStatusCounters =
  ObjectiveStatusCounters'
    { failed = Prelude.Nothing,
      pending = Prelude.Nothing,
      succeeded = Prelude.Nothing
    }

-- | The number of training jobs whose final objective metric was not
-- evaluated and used in the hyperparameter tuning process. This typically
-- occurs when the training job failed or did not emit an objective metric.
objectiveStatusCounters_failed :: Lens.Lens' ObjectiveStatusCounters (Prelude.Maybe Prelude.Natural)
objectiveStatusCounters_failed = Lens.lens (\ObjectiveStatusCounters' {failed} -> failed) (\s@ObjectiveStatusCounters' {} a -> s {failed = a} :: ObjectiveStatusCounters)

-- | The number of training jobs that are in progress and pending evaluation
-- of their final objective metric.
objectiveStatusCounters_pending :: Lens.Lens' ObjectiveStatusCounters (Prelude.Maybe Prelude.Natural)
objectiveStatusCounters_pending = Lens.lens (\ObjectiveStatusCounters' {pending} -> pending) (\s@ObjectiveStatusCounters' {} a -> s {pending = a} :: ObjectiveStatusCounters)

-- | The number of training jobs whose final objective metric was evaluated
-- by the hyperparameter tuning job and used in the hyperparameter tuning
-- process.
objectiveStatusCounters_succeeded :: Lens.Lens' ObjectiveStatusCounters (Prelude.Maybe Prelude.Natural)
objectiveStatusCounters_succeeded = Lens.lens (\ObjectiveStatusCounters' {succeeded} -> succeeded) (\s@ObjectiveStatusCounters' {} a -> s {succeeded = a} :: ObjectiveStatusCounters)

instance Data.FromJSON ObjectiveStatusCounters where
  parseJSON =
    Data.withObject
      "ObjectiveStatusCounters"
      ( \x ->
          ObjectiveStatusCounters'
            Prelude.<$> (x Data..:? "Failed")
            Prelude.<*> (x Data..:? "Pending")
            Prelude.<*> (x Data..:? "Succeeded")
      )

instance Prelude.Hashable ObjectiveStatusCounters where
  hashWithSalt _salt ObjectiveStatusCounters' {..} =
    _salt `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` succeeded

instance Prelude.NFData ObjectiveStatusCounters where
  rnf ObjectiveStatusCounters' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf succeeded

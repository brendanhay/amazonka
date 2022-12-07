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
-- Module      : Amazonka.Braket.Types.JobStoppingCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobStoppingCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies limits for how long an Amazon Braket job can run.
--
-- /See:/ 'newJobStoppingCondition' smart constructor.
data JobStoppingCondition = JobStoppingCondition'
  { -- | The maximum length of time, in seconds, that an Amazon Braket job can
    -- run.
    maxRuntimeInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobStoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRuntimeInSeconds', 'jobStoppingCondition_maxRuntimeInSeconds' - The maximum length of time, in seconds, that an Amazon Braket job can
-- run.
newJobStoppingCondition ::
  JobStoppingCondition
newJobStoppingCondition =
  JobStoppingCondition'
    { maxRuntimeInSeconds =
        Prelude.Nothing
    }

-- | The maximum length of time, in seconds, that an Amazon Braket job can
-- run.
jobStoppingCondition_maxRuntimeInSeconds :: Lens.Lens' JobStoppingCondition (Prelude.Maybe Prelude.Natural)
jobStoppingCondition_maxRuntimeInSeconds = Lens.lens (\JobStoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@JobStoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: JobStoppingCondition)

instance Data.FromJSON JobStoppingCondition where
  parseJSON =
    Data.withObject
      "JobStoppingCondition"
      ( \x ->
          JobStoppingCondition'
            Prelude.<$> (x Data..:? "maxRuntimeInSeconds")
      )

instance Prelude.Hashable JobStoppingCondition where
  hashWithSalt _salt JobStoppingCondition' {..} =
    _salt `Prelude.hashWithSalt` maxRuntimeInSeconds

instance Prelude.NFData JobStoppingCondition where
  rnf JobStoppingCondition' {..} =
    Prelude.rnf maxRuntimeInSeconds

instance Data.ToJSON JobStoppingCondition where
  toJSON JobStoppingCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxRuntimeInSeconds" Data..=)
              Prelude.<$> maxRuntimeInSeconds
          ]
      )

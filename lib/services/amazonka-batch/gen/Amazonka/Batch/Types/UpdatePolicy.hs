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
-- Module      : Amazonka.Batch.Types.UpdatePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.UpdatePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the infrastructure update policy for the compute environment.
-- For more information about infrastructure updates, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- /See:/ 'newUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { -- | Specifies whether jobs are automatically terminated when the computer
    -- environment infrastructure is updated. The default value is @false@.
    terminateJobsOnUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the job timeout (in minutes) when the compute environment
    -- infrastructure is updated. The default value is 30.
    jobExecutionTimeoutMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminateJobsOnUpdate', 'updatePolicy_terminateJobsOnUpdate' - Specifies whether jobs are automatically terminated when the computer
-- environment infrastructure is updated. The default value is @false@.
--
-- 'jobExecutionTimeoutMinutes', 'updatePolicy_jobExecutionTimeoutMinutes' - Specifies the job timeout (in minutes) when the compute environment
-- infrastructure is updated. The default value is 30.
newUpdatePolicy ::
  UpdatePolicy
newUpdatePolicy =
  UpdatePolicy'
    { terminateJobsOnUpdate =
        Prelude.Nothing,
      jobExecutionTimeoutMinutes = Prelude.Nothing
    }

-- | Specifies whether jobs are automatically terminated when the computer
-- environment infrastructure is updated. The default value is @false@.
updatePolicy_terminateJobsOnUpdate :: Lens.Lens' UpdatePolicy (Prelude.Maybe Prelude.Bool)
updatePolicy_terminateJobsOnUpdate = Lens.lens (\UpdatePolicy' {terminateJobsOnUpdate} -> terminateJobsOnUpdate) (\s@UpdatePolicy' {} a -> s {terminateJobsOnUpdate = a} :: UpdatePolicy)

-- | Specifies the job timeout (in minutes) when the compute environment
-- infrastructure is updated. The default value is 30.
updatePolicy_jobExecutionTimeoutMinutes :: Lens.Lens' UpdatePolicy (Prelude.Maybe Prelude.Natural)
updatePolicy_jobExecutionTimeoutMinutes = Lens.lens (\UpdatePolicy' {jobExecutionTimeoutMinutes} -> jobExecutionTimeoutMinutes) (\s@UpdatePolicy' {} a -> s {jobExecutionTimeoutMinutes = a} :: UpdatePolicy)

instance Data.FromJSON UpdatePolicy where
  parseJSON =
    Data.withObject
      "UpdatePolicy"
      ( \x ->
          UpdatePolicy'
            Prelude.<$> (x Data..:? "terminateJobsOnUpdate")
            Prelude.<*> (x Data..:? "jobExecutionTimeoutMinutes")
      )

instance Prelude.Hashable UpdatePolicy where
  hashWithSalt _salt UpdatePolicy' {..} =
    _salt `Prelude.hashWithSalt` terminateJobsOnUpdate
      `Prelude.hashWithSalt` jobExecutionTimeoutMinutes

instance Prelude.NFData UpdatePolicy where
  rnf UpdatePolicy' {..} =
    Prelude.rnf terminateJobsOnUpdate
      `Prelude.seq` Prelude.rnf jobExecutionTimeoutMinutes

instance Data.ToJSON UpdatePolicy where
  toJSON UpdatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("terminateJobsOnUpdate" Data..=)
              Prelude.<$> terminateJobsOnUpdate,
            ("jobExecutionTimeoutMinutes" Data..=)
              Prelude.<$> jobExecutionTimeoutMinutes
          ]
      )

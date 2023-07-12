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
-- Module      : Amazonka.Batch.Types.JobDependency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobDependency where

import Amazonka.Batch.Types.ArrayJobDependency
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch job dependency.
--
-- /See:/ 'newJobDependency' smart constructor.
data JobDependency = JobDependency'
  { -- | The job ID of the Batch job that\'s associated with this dependency.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The type of the job dependency.
    type' :: Prelude.Maybe ArrayJobDependency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDependency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'jobDependency_jobId' - The job ID of the Batch job that\'s associated with this dependency.
--
-- 'type'', 'jobDependency_type' - The type of the job dependency.
newJobDependency ::
  JobDependency
newJobDependency =
  JobDependency'
    { jobId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The job ID of the Batch job that\'s associated with this dependency.
jobDependency_jobId :: Lens.Lens' JobDependency (Prelude.Maybe Prelude.Text)
jobDependency_jobId = Lens.lens (\JobDependency' {jobId} -> jobId) (\s@JobDependency' {} a -> s {jobId = a} :: JobDependency)

-- | The type of the job dependency.
jobDependency_type :: Lens.Lens' JobDependency (Prelude.Maybe ArrayJobDependency)
jobDependency_type = Lens.lens (\JobDependency' {type'} -> type') (\s@JobDependency' {} a -> s {type' = a} :: JobDependency)

instance Data.FromJSON JobDependency where
  parseJSON =
    Data.withObject
      "JobDependency"
      ( \x ->
          JobDependency'
            Prelude.<$> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable JobDependency where
  hashWithSalt _salt JobDependency' {..} =
    _salt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData JobDependency where
  rnf JobDependency' {..} =
    Prelude.rnf jobId `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON JobDependency where
  toJSON JobDependency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jobId" Data..=) Prelude.<$> jobId,
            ("type" Data..=) Prelude.<$> type'
          ]
      )

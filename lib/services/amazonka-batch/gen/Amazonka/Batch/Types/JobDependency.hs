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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The type of the job dependency.
    type' :: Prelude.Maybe ArrayJobDependency,
    -- | The job ID of the Batch job that\'s associated with this dependency.
    jobId :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'jobDependency_type' - The type of the job dependency.
--
-- 'jobId', 'jobDependency_jobId' - The job ID of the Batch job that\'s associated with this dependency.
newJobDependency ::
  JobDependency
newJobDependency =
  JobDependency'
    { type' = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The type of the job dependency.
jobDependency_type :: Lens.Lens' JobDependency (Prelude.Maybe ArrayJobDependency)
jobDependency_type = Lens.lens (\JobDependency' {type'} -> type') (\s@JobDependency' {} a -> s {type' = a} :: JobDependency)

-- | The job ID of the Batch job that\'s associated with this dependency.
jobDependency_jobId :: Lens.Lens' JobDependency (Prelude.Maybe Prelude.Text)
jobDependency_jobId = Lens.lens (\JobDependency' {jobId} -> jobId) (\s@JobDependency' {} a -> s {jobId = a} :: JobDependency)

instance Data.FromJSON JobDependency where
  parseJSON =
    Data.withObject
      "JobDependency"
      ( \x ->
          JobDependency'
            Prelude.<$> (x Data..:? "type") Prelude.<*> (x Data..:? "jobId")
      )

instance Prelude.Hashable JobDependency where
  hashWithSalt _salt JobDependency' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData JobDependency where
  rnf JobDependency' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf jobId

instance Data.ToJSON JobDependency where
  toJSON JobDependency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("type" Data..=) Prelude.<$> type',
            ("jobId" Data..=) Prelude.<$> jobId
          ]
      )

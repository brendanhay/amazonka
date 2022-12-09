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
-- Module      : Amazonka.Pipes.Types.BatchJobDependency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.BatchJobDependency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.BatchJobDependencyType
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Batch job dependency.
--
-- /See:/ 'newBatchJobDependency' smart constructor.
data BatchJobDependency = BatchJobDependency'
  { -- | The job ID of the Batch job that\'s associated with this dependency.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The type of the job dependency.
    type' :: Prelude.Maybe BatchJobDependencyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchJobDependency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'batchJobDependency_jobId' - The job ID of the Batch job that\'s associated with this dependency.
--
-- 'type'', 'batchJobDependency_type' - The type of the job dependency.
newBatchJobDependency ::
  BatchJobDependency
newBatchJobDependency =
  BatchJobDependency'
    { jobId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The job ID of the Batch job that\'s associated with this dependency.
batchJobDependency_jobId :: Lens.Lens' BatchJobDependency (Prelude.Maybe Prelude.Text)
batchJobDependency_jobId = Lens.lens (\BatchJobDependency' {jobId} -> jobId) (\s@BatchJobDependency' {} a -> s {jobId = a} :: BatchJobDependency)

-- | The type of the job dependency.
batchJobDependency_type :: Lens.Lens' BatchJobDependency (Prelude.Maybe BatchJobDependencyType)
batchJobDependency_type = Lens.lens (\BatchJobDependency' {type'} -> type') (\s@BatchJobDependency' {} a -> s {type' = a} :: BatchJobDependency)

instance Data.FromJSON BatchJobDependency where
  parseJSON =
    Data.withObject
      "BatchJobDependency"
      ( \x ->
          BatchJobDependency'
            Prelude.<$> (x Data..:? "JobId") Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable BatchJobDependency where
  hashWithSalt _salt BatchJobDependency' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData BatchJobDependency where
  rnf BatchJobDependency' {..} =
    Prelude.rnf jobId `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON BatchJobDependency where
  toJSON BatchJobDependency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobId" Data..=) Prelude.<$> jobId,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

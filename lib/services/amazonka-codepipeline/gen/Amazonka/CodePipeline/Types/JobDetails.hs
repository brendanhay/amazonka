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
-- Module      : Amazonka.CodePipeline.Types.JobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.JobDetails where

import Amazonka.CodePipeline.Types.JobData
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the details of a job.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | Represents other information about a job required for a job worker to
    -- complete the job.
    data' :: Prelude.Maybe JobData,
    -- | The AWS account ID associated with the job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The unique system-generated ID of the job.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'jobDetails_data' - Represents other information about a job required for a job worker to
-- complete the job.
--
-- 'accountId', 'jobDetails_accountId' - The AWS account ID associated with the job.
--
-- 'id', 'jobDetails_id' - The unique system-generated ID of the job.
newJobDetails ::
  JobDetails
newJobDetails =
  JobDetails'
    { data' = Prelude.Nothing,
      accountId = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Represents other information about a job required for a job worker to
-- complete the job.
jobDetails_data :: Lens.Lens' JobDetails (Prelude.Maybe JobData)
jobDetails_data = Lens.lens (\JobDetails' {data'} -> data') (\s@JobDetails' {} a -> s {data' = a} :: JobDetails)

-- | The AWS account ID associated with the job.
jobDetails_accountId :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Text)
jobDetails_accountId = Lens.lens (\JobDetails' {accountId} -> accountId) (\s@JobDetails' {} a -> s {accountId = a} :: JobDetails)

-- | The unique system-generated ID of the job.
jobDetails_id :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Text)
jobDetails_id = Lens.lens (\JobDetails' {id} -> id) (\s@JobDetails' {} a -> s {id = a} :: JobDetails)

instance Core.FromJSON JobDetails where
  parseJSON =
    Core.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Prelude.<$> (x Core..:? "data")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable JobDetails where
  hashWithSalt salt' JobDetails' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` data'

instance Prelude.NFData JobDetails where
  rnf JobDetails' {..} =
    Prelude.rnf data' `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf accountId

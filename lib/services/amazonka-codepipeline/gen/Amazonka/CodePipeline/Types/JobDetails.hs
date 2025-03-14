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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.JobDetails where

import Amazonka.CodePipeline.Types.JobData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the details of a job.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | The AWS account ID associated with the job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Represents other information about a job required for a job worker to
    -- complete the job.
    data' :: Prelude.Maybe JobData,
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
-- 'accountId', 'jobDetails_accountId' - The AWS account ID associated with the job.
--
-- 'data'', 'jobDetails_data' - Represents other information about a job required for a job worker to
-- complete the job.
--
-- 'id', 'jobDetails_id' - The unique system-generated ID of the job.
newJobDetails ::
  JobDetails
newJobDetails =
  JobDetails'
    { accountId = Prelude.Nothing,
      data' = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The AWS account ID associated with the job.
jobDetails_accountId :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Text)
jobDetails_accountId = Lens.lens (\JobDetails' {accountId} -> accountId) (\s@JobDetails' {} a -> s {accountId = a} :: JobDetails)

-- | Represents other information about a job required for a job worker to
-- complete the job.
jobDetails_data :: Lens.Lens' JobDetails (Prelude.Maybe JobData)
jobDetails_data = Lens.lens (\JobDetails' {data'} -> data') (\s@JobDetails' {} a -> s {data' = a} :: JobDetails)

-- | The unique system-generated ID of the job.
jobDetails_id :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.Text)
jobDetails_id = Lens.lens (\JobDetails' {id} -> id) (\s@JobDetails' {} a -> s {id = a} :: JobDetails)

instance Data.FromJSON JobDetails where
  parseJSON =
    Data.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "data")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable JobDetails where
  hashWithSalt _salt JobDetails' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` id

instance Prelude.NFData JobDetails where
  rnf JobDetails' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf data' `Prelude.seq`
        Prelude.rnf id

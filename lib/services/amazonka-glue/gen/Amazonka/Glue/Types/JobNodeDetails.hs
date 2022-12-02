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
-- Module      : Amazonka.Glue.Types.JobNodeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobNodeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.JobRun
import qualified Amazonka.Prelude as Prelude

-- | The details of a Job node present in the workflow.
--
-- /See:/ 'newJobNodeDetails' smart constructor.
data JobNodeDetails = JobNodeDetails'
  { -- | The information for the job runs represented by the job node.
    jobRuns :: Prelude.Maybe [JobRun]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobNodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobRuns', 'jobNodeDetails_jobRuns' - The information for the job runs represented by the job node.
newJobNodeDetails ::
  JobNodeDetails
newJobNodeDetails =
  JobNodeDetails' {jobRuns = Prelude.Nothing}

-- | The information for the job runs represented by the job node.
jobNodeDetails_jobRuns :: Lens.Lens' JobNodeDetails (Prelude.Maybe [JobRun])
jobNodeDetails_jobRuns = Lens.lens (\JobNodeDetails' {jobRuns} -> jobRuns) (\s@JobNodeDetails' {} a -> s {jobRuns = a} :: JobNodeDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobNodeDetails where
  parseJSON =
    Data.withObject
      "JobNodeDetails"
      ( \x ->
          JobNodeDetails'
            Prelude.<$> (x Data..:? "JobRuns" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobNodeDetails where
  hashWithSalt _salt JobNodeDetails' {..} =
    _salt `Prelude.hashWithSalt` jobRuns

instance Prelude.NFData JobNodeDetails where
  rnf JobNodeDetails' {..} = Prelude.rnf jobRuns

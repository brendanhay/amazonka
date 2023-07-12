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
-- Module      : Amazonka.IoT.Types.JobExecutionsRetryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionsRetryConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.RetryCriteria
import qualified Amazonka.Prelude as Prelude

-- | The configuration that determines how many retries are allowed for each
-- failure type for a job.
--
-- /See:/ 'newJobExecutionsRetryConfig' smart constructor.
data JobExecutionsRetryConfig = JobExecutionsRetryConfig'
  { -- | The list of criteria that determines how many retries are allowed for
    -- each failure type for a job.
    criteriaList :: Prelude.NonEmpty RetryCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionsRetryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criteriaList', 'jobExecutionsRetryConfig_criteriaList' - The list of criteria that determines how many retries are allowed for
-- each failure type for a job.
newJobExecutionsRetryConfig ::
  -- | 'criteriaList'
  Prelude.NonEmpty RetryCriteria ->
  JobExecutionsRetryConfig
newJobExecutionsRetryConfig pCriteriaList_ =
  JobExecutionsRetryConfig'
    { criteriaList =
        Lens.coerced Lens.# pCriteriaList_
    }

-- | The list of criteria that determines how many retries are allowed for
-- each failure type for a job.
jobExecutionsRetryConfig_criteriaList :: Lens.Lens' JobExecutionsRetryConfig (Prelude.NonEmpty RetryCriteria)
jobExecutionsRetryConfig_criteriaList = Lens.lens (\JobExecutionsRetryConfig' {criteriaList} -> criteriaList) (\s@JobExecutionsRetryConfig' {} a -> s {criteriaList = a} :: JobExecutionsRetryConfig) Prelude.. Lens.coerced

instance Data.FromJSON JobExecutionsRetryConfig where
  parseJSON =
    Data.withObject
      "JobExecutionsRetryConfig"
      ( \x ->
          JobExecutionsRetryConfig'
            Prelude.<$> (x Data..: "criteriaList")
      )

instance Prelude.Hashable JobExecutionsRetryConfig where
  hashWithSalt _salt JobExecutionsRetryConfig' {..} =
    _salt `Prelude.hashWithSalt` criteriaList

instance Prelude.NFData JobExecutionsRetryConfig where
  rnf JobExecutionsRetryConfig' {..} =
    Prelude.rnf criteriaList

instance Data.ToJSON JobExecutionsRetryConfig where
  toJSON JobExecutionsRetryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("criteriaList" Data..= criteriaList)]
      )

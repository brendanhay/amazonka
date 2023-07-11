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
-- Module      : Amazonka.IoT.Types.JobExecutionStatusDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionStatusDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the job execution status.
--
-- /See:/ 'newJobExecutionStatusDetails' smart constructor.
data JobExecutionStatusDetails = JobExecutionStatusDetails'
  { -- | The job execution status.
    detailsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailsMap', 'jobExecutionStatusDetails_detailsMap' - The job execution status.
newJobExecutionStatusDetails ::
  JobExecutionStatusDetails
newJobExecutionStatusDetails =
  JobExecutionStatusDetails'
    { detailsMap =
        Prelude.Nothing
    }

-- | The job execution status.
jobExecutionStatusDetails_detailsMap :: Lens.Lens' JobExecutionStatusDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobExecutionStatusDetails_detailsMap = Lens.lens (\JobExecutionStatusDetails' {detailsMap} -> detailsMap) (\s@JobExecutionStatusDetails' {} a -> s {detailsMap = a} :: JobExecutionStatusDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobExecutionStatusDetails where
  parseJSON =
    Data.withObject
      "JobExecutionStatusDetails"
      ( \x ->
          JobExecutionStatusDetails'
            Prelude.<$> (x Data..:? "detailsMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobExecutionStatusDetails where
  hashWithSalt _salt JobExecutionStatusDetails' {..} =
    _salt `Prelude.hashWithSalt` detailsMap

instance Prelude.NFData JobExecutionStatusDetails where
  rnf JobExecutionStatusDetails' {..} =
    Prelude.rnf detailsMap

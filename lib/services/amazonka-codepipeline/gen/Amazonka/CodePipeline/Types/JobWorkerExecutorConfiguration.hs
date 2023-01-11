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
-- Module      : Amazonka.CodePipeline.Types.JobWorkerExecutorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.JobWorkerExecutorConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the polling configuration for the @JobWorker@ action
-- engine, or executor.
--
-- /See:/ 'newJobWorkerExecutorConfiguration' smart constructor.
data JobWorkerExecutorConfiguration = JobWorkerExecutorConfiguration'
  { -- | The accounts in which the job worker is configured and might poll for
    -- jobs as part of the action execution.
    pollingAccounts :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The service Principals in which the job worker is configured and might
    -- poll for jobs as part of the action execution.
    pollingServicePrincipals :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobWorkerExecutorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pollingAccounts', 'jobWorkerExecutorConfiguration_pollingAccounts' - The accounts in which the job worker is configured and might poll for
-- jobs as part of the action execution.
--
-- 'pollingServicePrincipals', 'jobWorkerExecutorConfiguration_pollingServicePrincipals' - The service Principals in which the job worker is configured and might
-- poll for jobs as part of the action execution.
newJobWorkerExecutorConfiguration ::
  JobWorkerExecutorConfiguration
newJobWorkerExecutorConfiguration =
  JobWorkerExecutorConfiguration'
    { pollingAccounts =
        Prelude.Nothing,
      pollingServicePrincipals = Prelude.Nothing
    }

-- | The accounts in which the job worker is configured and might poll for
-- jobs as part of the action execution.
jobWorkerExecutorConfiguration_pollingAccounts :: Lens.Lens' JobWorkerExecutorConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
jobWorkerExecutorConfiguration_pollingAccounts = Lens.lens (\JobWorkerExecutorConfiguration' {pollingAccounts} -> pollingAccounts) (\s@JobWorkerExecutorConfiguration' {} a -> s {pollingAccounts = a} :: JobWorkerExecutorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The service Principals in which the job worker is configured and might
-- poll for jobs as part of the action execution.
jobWorkerExecutorConfiguration_pollingServicePrincipals :: Lens.Lens' JobWorkerExecutorConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
jobWorkerExecutorConfiguration_pollingServicePrincipals = Lens.lens (\JobWorkerExecutorConfiguration' {pollingServicePrincipals} -> pollingServicePrincipals) (\s@JobWorkerExecutorConfiguration' {} a -> s {pollingServicePrincipals = a} :: JobWorkerExecutorConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobWorkerExecutorConfiguration where
  parseJSON =
    Data.withObject
      "JobWorkerExecutorConfiguration"
      ( \x ->
          JobWorkerExecutorConfiguration'
            Prelude.<$> (x Data..:? "pollingAccounts")
            Prelude.<*> (x Data..:? "pollingServicePrincipals")
      )

instance
  Prelude.Hashable
    JobWorkerExecutorConfiguration
  where
  hashWithSalt
    _salt
    JobWorkerExecutorConfiguration' {..} =
      _salt `Prelude.hashWithSalt` pollingAccounts
        `Prelude.hashWithSalt` pollingServicePrincipals

instance
  Prelude.NFData
    JobWorkerExecutorConfiguration
  where
  rnf JobWorkerExecutorConfiguration' {..} =
    Prelude.rnf pollingAccounts
      `Prelude.seq` Prelude.rnf pollingServicePrincipals

instance Data.ToJSON JobWorkerExecutorConfiguration where
  toJSON JobWorkerExecutorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pollingAccounts" Data..=)
              Prelude.<$> pollingAccounts,
            ("pollingServicePrincipals" Data..=)
              Prelude.<$> pollingServicePrincipals
          ]
      )

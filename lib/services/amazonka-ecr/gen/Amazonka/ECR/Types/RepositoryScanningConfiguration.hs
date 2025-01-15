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
-- Module      : Amazonka.ECR.Types.RepositoryScanningConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RepositoryScanningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ScanFrequency
import Amazonka.ECR.Types.ScanningRepositoryFilter
import qualified Amazonka.Prelude as Prelude

-- | The details of the scanning configuration for a repository.
--
-- /See:/ 'newRepositoryScanningConfiguration' smart constructor.
data RepositoryScanningConfiguration = RepositoryScanningConfiguration'
  { -- | The scan filters applied to the repository.
    appliedScanFilters :: Prelude.Maybe [ScanningRepositoryFilter],
    -- | The ARN of the repository.
    repositoryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The scan frequency for the repository.
    scanFrequency :: Prelude.Maybe ScanFrequency,
    -- | Whether or not scan on push is configured for the repository.
    scanOnPush :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedScanFilters', 'repositoryScanningConfiguration_appliedScanFilters' - The scan filters applied to the repository.
--
-- 'repositoryArn', 'repositoryScanningConfiguration_repositoryArn' - The ARN of the repository.
--
-- 'repositoryName', 'repositoryScanningConfiguration_repositoryName' - The name of the repository.
--
-- 'scanFrequency', 'repositoryScanningConfiguration_scanFrequency' - The scan frequency for the repository.
--
-- 'scanOnPush', 'repositoryScanningConfiguration_scanOnPush' - Whether or not scan on push is configured for the repository.
newRepositoryScanningConfiguration ::
  RepositoryScanningConfiguration
newRepositoryScanningConfiguration =
  RepositoryScanningConfiguration'
    { appliedScanFilters =
        Prelude.Nothing,
      repositoryArn = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      scanFrequency = Prelude.Nothing,
      scanOnPush = Prelude.Nothing
    }

-- | The scan filters applied to the repository.
repositoryScanningConfiguration_appliedScanFilters :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe [ScanningRepositoryFilter])
repositoryScanningConfiguration_appliedScanFilters = Lens.lens (\RepositoryScanningConfiguration' {appliedScanFilters} -> appliedScanFilters) (\s@RepositoryScanningConfiguration' {} a -> s {appliedScanFilters = a} :: RepositoryScanningConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the repository.
repositoryScanningConfiguration_repositoryArn :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Text)
repositoryScanningConfiguration_repositoryArn = Lens.lens (\RepositoryScanningConfiguration' {repositoryArn} -> repositoryArn) (\s@RepositoryScanningConfiguration' {} a -> s {repositoryArn = a} :: RepositoryScanningConfiguration)

-- | The name of the repository.
repositoryScanningConfiguration_repositoryName :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Text)
repositoryScanningConfiguration_repositoryName = Lens.lens (\RepositoryScanningConfiguration' {repositoryName} -> repositoryName) (\s@RepositoryScanningConfiguration' {} a -> s {repositoryName = a} :: RepositoryScanningConfiguration)

-- | The scan frequency for the repository.
repositoryScanningConfiguration_scanFrequency :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe ScanFrequency)
repositoryScanningConfiguration_scanFrequency = Lens.lens (\RepositoryScanningConfiguration' {scanFrequency} -> scanFrequency) (\s@RepositoryScanningConfiguration' {} a -> s {scanFrequency = a} :: RepositoryScanningConfiguration)

-- | Whether or not scan on push is configured for the repository.
repositoryScanningConfiguration_scanOnPush :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Bool)
repositoryScanningConfiguration_scanOnPush = Lens.lens (\RepositoryScanningConfiguration' {scanOnPush} -> scanOnPush) (\s@RepositoryScanningConfiguration' {} a -> s {scanOnPush = a} :: RepositoryScanningConfiguration)

instance
  Data.FromJSON
    RepositoryScanningConfiguration
  where
  parseJSON =
    Data.withObject
      "RepositoryScanningConfiguration"
      ( \x ->
          RepositoryScanningConfiguration'
            Prelude.<$> ( x
                            Data..:? "appliedScanFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "repositoryArn")
            Prelude.<*> (x Data..:? "repositoryName")
            Prelude.<*> (x Data..:? "scanFrequency")
            Prelude.<*> (x Data..:? "scanOnPush")
      )

instance
  Prelude.Hashable
    RepositoryScanningConfiguration
  where
  hashWithSalt
    _salt
    RepositoryScanningConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` appliedScanFilters
        `Prelude.hashWithSalt` repositoryArn
        `Prelude.hashWithSalt` repositoryName
        `Prelude.hashWithSalt` scanFrequency
        `Prelude.hashWithSalt` scanOnPush

instance
  Prelude.NFData
    RepositoryScanningConfiguration
  where
  rnf RepositoryScanningConfiguration' {..} =
    Prelude.rnf appliedScanFilters `Prelude.seq`
      Prelude.rnf repositoryArn `Prelude.seq`
        Prelude.rnf repositoryName `Prelude.seq`
          Prelude.rnf scanFrequency `Prelude.seq`
            Prelude.rnf scanOnPush

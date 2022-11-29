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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RepositoryScanningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.ScanFrequency
import Amazonka.ECR.Types.ScanningRepositoryFilter
import qualified Amazonka.Prelude as Prelude

-- | The details of the scanning configuration for a repository.
--
-- /See:/ 'newRepositoryScanningConfiguration' smart constructor.
data RepositoryScanningConfiguration = RepositoryScanningConfiguration'
  { -- | The ARN of the repository.
    repositoryArn :: Prelude.Maybe Prelude.Text,
    -- | Whether or not scan on push is configured for the repository.
    scanOnPush :: Prelude.Maybe Prelude.Bool,
    -- | The scan frequency for the repository.
    scanFrequency :: Prelude.Maybe ScanFrequency,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The scan filters applied to the repository.
    appliedScanFilters :: Prelude.Maybe [ScanningRepositoryFilter]
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
-- 'repositoryArn', 'repositoryScanningConfiguration_repositoryArn' - The ARN of the repository.
--
-- 'scanOnPush', 'repositoryScanningConfiguration_scanOnPush' - Whether or not scan on push is configured for the repository.
--
-- 'scanFrequency', 'repositoryScanningConfiguration_scanFrequency' - The scan frequency for the repository.
--
-- 'repositoryName', 'repositoryScanningConfiguration_repositoryName' - The name of the repository.
--
-- 'appliedScanFilters', 'repositoryScanningConfiguration_appliedScanFilters' - The scan filters applied to the repository.
newRepositoryScanningConfiguration ::
  RepositoryScanningConfiguration
newRepositoryScanningConfiguration =
  RepositoryScanningConfiguration'
    { repositoryArn =
        Prelude.Nothing,
      scanOnPush = Prelude.Nothing,
      scanFrequency = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      appliedScanFilters = Prelude.Nothing
    }

-- | The ARN of the repository.
repositoryScanningConfiguration_repositoryArn :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Text)
repositoryScanningConfiguration_repositoryArn = Lens.lens (\RepositoryScanningConfiguration' {repositoryArn} -> repositoryArn) (\s@RepositoryScanningConfiguration' {} a -> s {repositoryArn = a} :: RepositoryScanningConfiguration)

-- | Whether or not scan on push is configured for the repository.
repositoryScanningConfiguration_scanOnPush :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Bool)
repositoryScanningConfiguration_scanOnPush = Lens.lens (\RepositoryScanningConfiguration' {scanOnPush} -> scanOnPush) (\s@RepositoryScanningConfiguration' {} a -> s {scanOnPush = a} :: RepositoryScanningConfiguration)

-- | The scan frequency for the repository.
repositoryScanningConfiguration_scanFrequency :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe ScanFrequency)
repositoryScanningConfiguration_scanFrequency = Lens.lens (\RepositoryScanningConfiguration' {scanFrequency} -> scanFrequency) (\s@RepositoryScanningConfiguration' {} a -> s {scanFrequency = a} :: RepositoryScanningConfiguration)

-- | The name of the repository.
repositoryScanningConfiguration_repositoryName :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe Prelude.Text)
repositoryScanningConfiguration_repositoryName = Lens.lens (\RepositoryScanningConfiguration' {repositoryName} -> repositoryName) (\s@RepositoryScanningConfiguration' {} a -> s {repositoryName = a} :: RepositoryScanningConfiguration)

-- | The scan filters applied to the repository.
repositoryScanningConfiguration_appliedScanFilters :: Lens.Lens' RepositoryScanningConfiguration (Prelude.Maybe [ScanningRepositoryFilter])
repositoryScanningConfiguration_appliedScanFilters = Lens.lens (\RepositoryScanningConfiguration' {appliedScanFilters} -> appliedScanFilters) (\s@RepositoryScanningConfiguration' {} a -> s {appliedScanFilters = a} :: RepositoryScanningConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    RepositoryScanningConfiguration
  where
  parseJSON =
    Core.withObject
      "RepositoryScanningConfiguration"
      ( \x ->
          RepositoryScanningConfiguration'
            Prelude.<$> (x Core..:? "repositoryArn")
            Prelude.<*> (x Core..:? "scanOnPush")
            Prelude.<*> (x Core..:? "scanFrequency")
            Prelude.<*> (x Core..:? "repositoryName")
            Prelude.<*> ( x Core..:? "appliedScanFilters"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    RepositoryScanningConfiguration
  where
  hashWithSalt
    _salt
    RepositoryScanningConfiguration' {..} =
      _salt `Prelude.hashWithSalt` repositoryArn
        `Prelude.hashWithSalt` scanOnPush
        `Prelude.hashWithSalt` scanFrequency
        `Prelude.hashWithSalt` repositoryName
        `Prelude.hashWithSalt` appliedScanFilters

instance
  Prelude.NFData
    RepositoryScanningConfiguration
  where
  rnf RepositoryScanningConfiguration' {..} =
    Prelude.rnf repositoryArn
      `Prelude.seq` Prelude.rnf scanOnPush
      `Prelude.seq` Prelude.rnf scanFrequency
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf appliedScanFilters

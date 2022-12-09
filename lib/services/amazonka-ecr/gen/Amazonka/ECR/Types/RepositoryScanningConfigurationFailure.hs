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
-- Module      : Amazonka.ECR.Types.RepositoryScanningConfigurationFailure
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RepositoryScanningConfigurationFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ScanningConfigurationFailureCode
import qualified Amazonka.Prelude as Prelude

-- | The details about any failures associated with the scanning
-- configuration of a repository.
--
-- /See:/ 'newRepositoryScanningConfigurationFailure' smart constructor.
data RepositoryScanningConfigurationFailure = RepositoryScanningConfigurationFailure'
  { -- | The failure code.
    failureCode :: Prelude.Maybe ScanningConfigurationFailureCode,
    -- | The reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryScanningConfigurationFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'repositoryScanningConfigurationFailure_failureCode' - The failure code.
--
-- 'failureReason', 'repositoryScanningConfigurationFailure_failureReason' - The reason for the failure.
--
-- 'repositoryName', 'repositoryScanningConfigurationFailure_repositoryName' - The name of the repository.
newRepositoryScanningConfigurationFailure ::
  RepositoryScanningConfigurationFailure
newRepositoryScanningConfigurationFailure =
  RepositoryScanningConfigurationFailure'
    { failureCode =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The failure code.
repositoryScanningConfigurationFailure_failureCode :: Lens.Lens' RepositoryScanningConfigurationFailure (Prelude.Maybe ScanningConfigurationFailureCode)
repositoryScanningConfigurationFailure_failureCode = Lens.lens (\RepositoryScanningConfigurationFailure' {failureCode} -> failureCode) (\s@RepositoryScanningConfigurationFailure' {} a -> s {failureCode = a} :: RepositoryScanningConfigurationFailure)

-- | The reason for the failure.
repositoryScanningConfigurationFailure_failureReason :: Lens.Lens' RepositoryScanningConfigurationFailure (Prelude.Maybe Prelude.Text)
repositoryScanningConfigurationFailure_failureReason = Lens.lens (\RepositoryScanningConfigurationFailure' {failureReason} -> failureReason) (\s@RepositoryScanningConfigurationFailure' {} a -> s {failureReason = a} :: RepositoryScanningConfigurationFailure)

-- | The name of the repository.
repositoryScanningConfigurationFailure_repositoryName :: Lens.Lens' RepositoryScanningConfigurationFailure (Prelude.Maybe Prelude.Text)
repositoryScanningConfigurationFailure_repositoryName = Lens.lens (\RepositoryScanningConfigurationFailure' {repositoryName} -> repositoryName) (\s@RepositoryScanningConfigurationFailure' {} a -> s {repositoryName = a} :: RepositoryScanningConfigurationFailure)

instance
  Data.FromJSON
    RepositoryScanningConfigurationFailure
  where
  parseJSON =
    Data.withObject
      "RepositoryScanningConfigurationFailure"
      ( \x ->
          RepositoryScanningConfigurationFailure'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance
  Prelude.Hashable
    RepositoryScanningConfigurationFailure
  where
  hashWithSalt
    _salt
    RepositoryScanningConfigurationFailure' {..} =
      _salt `Prelude.hashWithSalt` failureCode
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    RepositoryScanningConfigurationFailure
  where
  rnf RepositoryScanningConfigurationFailure' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf repositoryName

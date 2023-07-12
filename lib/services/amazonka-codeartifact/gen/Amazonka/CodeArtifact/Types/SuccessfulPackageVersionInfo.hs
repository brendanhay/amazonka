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
-- Module      : Amazonka.CodeArtifact.Types.SuccessfulPackageVersionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.SuccessfulPackageVersionInfo where

import Amazonka.CodeArtifact.Types.PackageVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the revision and status of a package version.
--
-- /See:/ 'newSuccessfulPackageVersionInfo' smart constructor.
data SuccessfulPackageVersionInfo = SuccessfulPackageVersionInfo'
  { -- | The revision of a package version.
    revision :: Prelude.Maybe Prelude.Text,
    -- | The status of a package version.
    status :: Prelude.Maybe PackageVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessfulPackageVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'successfulPackageVersionInfo_revision' - The revision of a package version.
--
-- 'status', 'successfulPackageVersionInfo_status' - The status of a package version.
newSuccessfulPackageVersionInfo ::
  SuccessfulPackageVersionInfo
newSuccessfulPackageVersionInfo =
  SuccessfulPackageVersionInfo'
    { revision =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The revision of a package version.
successfulPackageVersionInfo_revision :: Lens.Lens' SuccessfulPackageVersionInfo (Prelude.Maybe Prelude.Text)
successfulPackageVersionInfo_revision = Lens.lens (\SuccessfulPackageVersionInfo' {revision} -> revision) (\s@SuccessfulPackageVersionInfo' {} a -> s {revision = a} :: SuccessfulPackageVersionInfo)

-- | The status of a package version.
successfulPackageVersionInfo_status :: Lens.Lens' SuccessfulPackageVersionInfo (Prelude.Maybe PackageVersionStatus)
successfulPackageVersionInfo_status = Lens.lens (\SuccessfulPackageVersionInfo' {status} -> status) (\s@SuccessfulPackageVersionInfo' {} a -> s {status = a} :: SuccessfulPackageVersionInfo)

instance Data.FromJSON SuccessfulPackageVersionInfo where
  parseJSON =
    Data.withObject
      "SuccessfulPackageVersionInfo"
      ( \x ->
          SuccessfulPackageVersionInfo'
            Prelude.<$> (x Data..:? "revision")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    SuccessfulPackageVersionInfo
  where
  hashWithSalt _salt SuccessfulPackageVersionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` status

instance Prelude.NFData SuccessfulPackageVersionInfo where
  rnf SuccessfulPackageVersionInfo' {..} =
    Prelude.rnf revision
      `Prelude.seq` Prelude.rnf status

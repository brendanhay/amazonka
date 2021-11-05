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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.SuccessfulPackageVersionInfo where

import Amazonka.CodeArtifact.Types.PackageVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the revision and status of a package version.
--
-- /See:/ 'newSuccessfulPackageVersionInfo' smart constructor.
data SuccessfulPackageVersionInfo = SuccessfulPackageVersionInfo'
  { -- | The status of a package version. Valid statuses are:
    --
    -- -   @Published@
    --
    -- -   @Unfinished@
    --
    -- -   @Unlisted@
    --
    -- -   @Archived@
    --
    -- -   @Disposed@
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The revision of a package version.
    revision :: Prelude.Maybe Prelude.Text
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
-- 'status', 'successfulPackageVersionInfo_status' - The status of a package version. Valid statuses are:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
--
-- 'revision', 'successfulPackageVersionInfo_revision' - The revision of a package version.
newSuccessfulPackageVersionInfo ::
  SuccessfulPackageVersionInfo
newSuccessfulPackageVersionInfo =
  SuccessfulPackageVersionInfo'
    { status =
        Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The status of a package version. Valid statuses are:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
successfulPackageVersionInfo_status :: Lens.Lens' SuccessfulPackageVersionInfo (Prelude.Maybe PackageVersionStatus)
successfulPackageVersionInfo_status = Lens.lens (\SuccessfulPackageVersionInfo' {status} -> status) (\s@SuccessfulPackageVersionInfo' {} a -> s {status = a} :: SuccessfulPackageVersionInfo)

-- | The revision of a package version.
successfulPackageVersionInfo_revision :: Lens.Lens' SuccessfulPackageVersionInfo (Prelude.Maybe Prelude.Text)
successfulPackageVersionInfo_revision = Lens.lens (\SuccessfulPackageVersionInfo' {revision} -> revision) (\s@SuccessfulPackageVersionInfo' {} a -> s {revision = a} :: SuccessfulPackageVersionInfo)

instance Core.FromJSON SuccessfulPackageVersionInfo where
  parseJSON =
    Core.withObject
      "SuccessfulPackageVersionInfo"
      ( \x ->
          SuccessfulPackageVersionInfo'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "revision")
      )

instance
  Prelude.Hashable
    SuccessfulPackageVersionInfo

instance Prelude.NFData SuccessfulPackageVersionInfo

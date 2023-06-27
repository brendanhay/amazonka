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
-- Module      : Amazonka.WellArchitected.Types.WorkloadProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The profile associated with a workload.
--
-- /See:/ 'newWorkloadProfile' smart constructor.
data WorkloadProfile = WorkloadProfile'
  { -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The profile version.
    profileVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkloadProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'workloadProfile_profileArn' - The profile ARN.
--
-- 'profileVersion', 'workloadProfile_profileVersion' - The profile version.
newWorkloadProfile ::
  WorkloadProfile
newWorkloadProfile =
  WorkloadProfile'
    { profileArn = Prelude.Nothing,
      profileVersion = Prelude.Nothing
    }

-- | The profile ARN.
workloadProfile_profileArn :: Lens.Lens' WorkloadProfile (Prelude.Maybe Prelude.Text)
workloadProfile_profileArn = Lens.lens (\WorkloadProfile' {profileArn} -> profileArn) (\s@WorkloadProfile' {} a -> s {profileArn = a} :: WorkloadProfile)

-- | The profile version.
workloadProfile_profileVersion :: Lens.Lens' WorkloadProfile (Prelude.Maybe Prelude.Text)
workloadProfile_profileVersion = Lens.lens (\WorkloadProfile' {profileVersion} -> profileVersion) (\s@WorkloadProfile' {} a -> s {profileVersion = a} :: WorkloadProfile)

instance Data.FromJSON WorkloadProfile where
  parseJSON =
    Data.withObject
      "WorkloadProfile"
      ( \x ->
          WorkloadProfile'
            Prelude.<$> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProfileVersion")
      )

instance Prelude.Hashable WorkloadProfile where
  hashWithSalt _salt WorkloadProfile' {..} =
    _salt
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileVersion

instance Prelude.NFData WorkloadProfile where
  rnf WorkloadProfile' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileVersion

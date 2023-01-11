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
-- Module      : Amazonka.MigrationHubStrategy.Types.VersionControlInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.VersionControlInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.VersionControlType
import qualified Amazonka.Prelude as Prelude

-- | Details about the version control configuration.
--
-- /See:/ 'newVersionControlInfo' smart constructor.
data VersionControlInfo = VersionControlInfo'
  { -- | The time when the version control system was last configured.
    versionControlConfigurationTimeStamp :: Prelude.Maybe Prelude.Text,
    -- | The type of version control.
    versionControlType :: Prelude.Maybe VersionControlType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionControlInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionControlConfigurationTimeStamp', 'versionControlInfo_versionControlConfigurationTimeStamp' - The time when the version control system was last configured.
--
-- 'versionControlType', 'versionControlInfo_versionControlType' - The type of version control.
newVersionControlInfo ::
  VersionControlInfo
newVersionControlInfo =
  VersionControlInfo'
    { versionControlConfigurationTimeStamp =
        Prelude.Nothing,
      versionControlType = Prelude.Nothing
    }

-- | The time when the version control system was last configured.
versionControlInfo_versionControlConfigurationTimeStamp :: Lens.Lens' VersionControlInfo (Prelude.Maybe Prelude.Text)
versionControlInfo_versionControlConfigurationTimeStamp = Lens.lens (\VersionControlInfo' {versionControlConfigurationTimeStamp} -> versionControlConfigurationTimeStamp) (\s@VersionControlInfo' {} a -> s {versionControlConfigurationTimeStamp = a} :: VersionControlInfo)

-- | The type of version control.
versionControlInfo_versionControlType :: Lens.Lens' VersionControlInfo (Prelude.Maybe VersionControlType)
versionControlInfo_versionControlType = Lens.lens (\VersionControlInfo' {versionControlType} -> versionControlType) (\s@VersionControlInfo' {} a -> s {versionControlType = a} :: VersionControlInfo)

instance Data.FromJSON VersionControlInfo where
  parseJSON =
    Data.withObject
      "VersionControlInfo"
      ( \x ->
          VersionControlInfo'
            Prelude.<$> (x Data..:? "versionControlConfigurationTimeStamp")
            Prelude.<*> (x Data..:? "versionControlType")
      )

instance Prelude.Hashable VersionControlInfo where
  hashWithSalt _salt VersionControlInfo' {..} =
    _salt
      `Prelude.hashWithSalt` versionControlConfigurationTimeStamp
      `Prelude.hashWithSalt` versionControlType

instance Prelude.NFData VersionControlInfo where
  rnf VersionControlInfo' {..} =
    Prelude.rnf versionControlConfigurationTimeStamp
      `Prelude.seq` Prelude.rnf versionControlType

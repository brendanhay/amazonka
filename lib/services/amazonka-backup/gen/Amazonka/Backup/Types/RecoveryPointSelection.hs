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
-- Module      : Amazonka.Backup.Types.RecoveryPointSelection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointSelection where

import Amazonka.Backup.Types.DateRange
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
--
-- /See:/ 'newRecoveryPointSelection' smart constructor.
data RecoveryPointSelection = RecoveryPointSelection'
  { dateRange :: Prelude.Maybe DateRange,
    -- | These are the resources included in the resource selection (including
    -- type of resources and vaults).
    resourceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | These are the names of the vaults in which the selected recovery points
    -- are contained.
    vaultNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateRange', 'recoveryPointSelection_dateRange' - Undocumented member.
--
-- 'resourceIdentifiers', 'recoveryPointSelection_resourceIdentifiers' - These are the resources included in the resource selection (including
-- type of resources and vaults).
--
-- 'vaultNames', 'recoveryPointSelection_vaultNames' - These are the names of the vaults in which the selected recovery points
-- are contained.
newRecoveryPointSelection ::
  RecoveryPointSelection
newRecoveryPointSelection =
  RecoveryPointSelection'
    { dateRange =
        Prelude.Nothing,
      resourceIdentifiers = Prelude.Nothing,
      vaultNames = Prelude.Nothing
    }

-- | Undocumented member.
recoveryPointSelection_dateRange :: Lens.Lens' RecoveryPointSelection (Prelude.Maybe DateRange)
recoveryPointSelection_dateRange = Lens.lens (\RecoveryPointSelection' {dateRange} -> dateRange) (\s@RecoveryPointSelection' {} a -> s {dateRange = a} :: RecoveryPointSelection)

-- | These are the resources included in the resource selection (including
-- type of resources and vaults).
recoveryPointSelection_resourceIdentifiers :: Lens.Lens' RecoveryPointSelection (Prelude.Maybe [Prelude.Text])
recoveryPointSelection_resourceIdentifiers = Lens.lens (\RecoveryPointSelection' {resourceIdentifiers} -> resourceIdentifiers) (\s@RecoveryPointSelection' {} a -> s {resourceIdentifiers = a} :: RecoveryPointSelection) Prelude.. Lens.mapping Lens.coerced

-- | These are the names of the vaults in which the selected recovery points
-- are contained.
recoveryPointSelection_vaultNames :: Lens.Lens' RecoveryPointSelection (Prelude.Maybe [Prelude.Text])
recoveryPointSelection_vaultNames = Lens.lens (\RecoveryPointSelection' {vaultNames} -> vaultNames) (\s@RecoveryPointSelection' {} a -> s {vaultNames = a} :: RecoveryPointSelection) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecoveryPointSelection where
  parseJSON =
    Data.withObject
      "RecoveryPointSelection"
      ( \x ->
          RecoveryPointSelection'
            Prelude.<$> (x Data..:? "DateRange")
            Prelude.<*> ( x
                            Data..:? "ResourceIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VaultNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecoveryPointSelection where
  hashWithSalt _salt RecoveryPointSelection' {..} =
    _salt
      `Prelude.hashWithSalt` dateRange
      `Prelude.hashWithSalt` resourceIdentifiers
      `Prelude.hashWithSalt` vaultNames

instance Prelude.NFData RecoveryPointSelection where
  rnf RecoveryPointSelection' {..} =
    Prelude.rnf dateRange `Prelude.seq`
      Prelude.rnf resourceIdentifiers `Prelude.seq`
        Prelude.rnf vaultNames

instance Data.ToJSON RecoveryPointSelection where
  toJSON RecoveryPointSelection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateRange" Data..=) Prelude.<$> dateRange,
            ("ResourceIdentifiers" Data..=)
              Prelude.<$> resourceIdentifiers,
            ("VaultNames" Data..=) Prelude.<$> vaultNames
          ]
      )

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
-- Module      : Amazonka.CloudHSMV2.Types.BackupRetentionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types.BackupRetentionPolicy where

import Amazonka.CloudHSMV2.Types.BackupRetentionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A policy that defines the number of days to retain backups.
--
-- /See:/ 'newBackupRetentionPolicy' smart constructor.
data BackupRetentionPolicy = BackupRetentionPolicy'
  { -- | The type of backup retention policy. For the @DAYS@ type, the value is
    -- the number of days to retain backups.
    type' :: Prelude.Maybe BackupRetentionType,
    -- | Use a value between 7 - 379.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'backupRetentionPolicy_type' - The type of backup retention policy. For the @DAYS@ type, the value is
-- the number of days to retain backups.
--
-- 'value', 'backupRetentionPolicy_value' - Use a value between 7 - 379.
newBackupRetentionPolicy ::
  BackupRetentionPolicy
newBackupRetentionPolicy =
  BackupRetentionPolicy'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of backup retention policy. For the @DAYS@ type, the value is
-- the number of days to retain backups.
backupRetentionPolicy_type :: Lens.Lens' BackupRetentionPolicy (Prelude.Maybe BackupRetentionType)
backupRetentionPolicy_type = Lens.lens (\BackupRetentionPolicy' {type'} -> type') (\s@BackupRetentionPolicy' {} a -> s {type' = a} :: BackupRetentionPolicy)

-- | Use a value between 7 - 379.
backupRetentionPolicy_value :: Lens.Lens' BackupRetentionPolicy (Prelude.Maybe Prelude.Text)
backupRetentionPolicy_value = Lens.lens (\BackupRetentionPolicy' {value} -> value) (\s@BackupRetentionPolicy' {} a -> s {value = a} :: BackupRetentionPolicy)

instance Data.FromJSON BackupRetentionPolicy where
  parseJSON =
    Data.withObject
      "BackupRetentionPolicy"
      ( \x ->
          BackupRetentionPolicy'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable BackupRetentionPolicy where
  hashWithSalt _salt BackupRetentionPolicy' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData BackupRetentionPolicy where
  rnf BackupRetentionPolicy' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON BackupRetentionPolicy where
  toJSON BackupRetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )

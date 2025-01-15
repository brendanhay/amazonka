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
-- Module      : Amazonka.Backup.Types.CopyAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.CopyAction where

import Amazonka.Backup.Types.Lifecycle
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the copy operation.
--
-- /See:/ 'newCopyAction' smart constructor.
data CopyAction = CopyAction'
  { lifecycle :: Prelude.Maybe Lifecycle,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the destination
    -- backup vault for the copied backup. For example,
    -- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    destinationBackupVaultArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'copyAction_lifecycle' - Undocumented member.
--
-- 'destinationBackupVaultArn', 'copyAction_destinationBackupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup. For example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
newCopyAction ::
  -- | 'destinationBackupVaultArn'
  Prelude.Text ->
  CopyAction
newCopyAction pDestinationBackupVaultArn_ =
  CopyAction'
    { lifecycle = Prelude.Nothing,
      destinationBackupVaultArn =
        pDestinationBackupVaultArn_
    }

-- | Undocumented member.
copyAction_lifecycle :: Lens.Lens' CopyAction (Prelude.Maybe Lifecycle)
copyAction_lifecycle = Lens.lens (\CopyAction' {lifecycle} -> lifecycle) (\s@CopyAction' {} a -> s {lifecycle = a} :: CopyAction)

-- | An Amazon Resource Name (ARN) that uniquely identifies the destination
-- backup vault for the copied backup. For example,
-- @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
copyAction_destinationBackupVaultArn :: Lens.Lens' CopyAction Prelude.Text
copyAction_destinationBackupVaultArn = Lens.lens (\CopyAction' {destinationBackupVaultArn} -> destinationBackupVaultArn) (\s@CopyAction' {} a -> s {destinationBackupVaultArn = a} :: CopyAction)

instance Data.FromJSON CopyAction where
  parseJSON =
    Data.withObject
      "CopyAction"
      ( \x ->
          CopyAction'
            Prelude.<$> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..: "DestinationBackupVaultArn")
      )

instance Prelude.Hashable CopyAction where
  hashWithSalt _salt CopyAction' {..} =
    _salt
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` destinationBackupVaultArn

instance Prelude.NFData CopyAction where
  rnf CopyAction' {..} =
    Prelude.rnf lifecycle `Prelude.seq`
      Prelude.rnf destinationBackupVaultArn

instance Data.ToJSON CopyAction where
  toJSON CopyAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Lifecycle" Data..=) Prelude.<$> lifecycle,
            Prelude.Just
              ( "DestinationBackupVaultArn"
                  Data..= destinationBackupVaultArn
              )
          ]
      )

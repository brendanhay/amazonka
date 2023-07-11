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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies how long in days before a recovery point transitions to cold
-- storage or is deleted.
--
-- /See:/ 'newAwsBackupRecoveryPointCalculatedLifecycleDetails' smart constructor.
data AwsBackupRecoveryPointCalculatedLifecycleDetails = AwsBackupRecoveryPointCalculatedLifecycleDetails'
  { -- | Specifies the number of days after creation that a recovery point is
    -- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
    deleteAt :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days after creation that a recovery point is
    -- moved to cold storage.
    moveToColdStorageAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupRecoveryPointCalculatedLifecycleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAt', 'awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt' - Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
--
-- 'moveToColdStorageAt', 'awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt' - Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
newAwsBackupRecoveryPointCalculatedLifecycleDetails ::
  AwsBackupRecoveryPointCalculatedLifecycleDetails
newAwsBackupRecoveryPointCalculatedLifecycleDetails =
  AwsBackupRecoveryPointCalculatedLifecycleDetails'
    { deleteAt =
        Prelude.Nothing,
      moveToColdStorageAt =
        Prelude.Nothing
    }

-- | Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt :: Lens.Lens' AwsBackupRecoveryPointCalculatedLifecycleDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt = Lens.lens (\AwsBackupRecoveryPointCalculatedLifecycleDetails' {deleteAt} -> deleteAt) (\s@AwsBackupRecoveryPointCalculatedLifecycleDetails' {} a -> s {deleteAt = a} :: AwsBackupRecoveryPointCalculatedLifecycleDetails)

-- | Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt :: Lens.Lens' AwsBackupRecoveryPointCalculatedLifecycleDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt = Lens.lens (\AwsBackupRecoveryPointCalculatedLifecycleDetails' {moveToColdStorageAt} -> moveToColdStorageAt) (\s@AwsBackupRecoveryPointCalculatedLifecycleDetails' {} a -> s {moveToColdStorageAt = a} :: AwsBackupRecoveryPointCalculatedLifecycleDetails)

instance
  Data.FromJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupRecoveryPointCalculatedLifecycleDetails"
      ( \x ->
          AwsBackupRecoveryPointCalculatedLifecycleDetails'
            Prelude.<$> (x Data..:? "DeleteAt")
            Prelude.<*> (x Data..:? "MoveToColdStorageAt")
      )

instance
  Prelude.Hashable
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  hashWithSalt
    _salt
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      _salt
        `Prelude.hashWithSalt` deleteAt
        `Prelude.hashWithSalt` moveToColdStorageAt

instance
  Prelude.NFData
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  rnf
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      Prelude.rnf deleteAt
        `Prelude.seq` Prelude.rnf moveToColdStorageAt

instance
  Data.ToJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  toJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeleteAt" Data..=) Prelude.<$> deleteAt,
              ("MoveToColdStorageAt" Data..=)
                Prelude.<$> moveToColdStorageAt
            ]
        )

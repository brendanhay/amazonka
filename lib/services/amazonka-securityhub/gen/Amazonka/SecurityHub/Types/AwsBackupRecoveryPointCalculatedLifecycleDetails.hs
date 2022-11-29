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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupRecoveryPointCalculatedLifecycleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies how long in days before a recovery point transitions to cold
-- storage or is deleted.
--
-- /See:/ 'newAwsBackupRecoveryPointCalculatedLifecycleDetails' smart constructor.
data AwsBackupRecoveryPointCalculatedLifecycleDetails = AwsBackupRecoveryPointCalculatedLifecycleDetails'
  { -- | Specifies the number of days after creation that a recovery point is
    -- moved to cold storage.
    moveToColdStorageAt :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days after creation that a recovery point is
    -- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
    deleteAt :: Prelude.Maybe Prelude.Text
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
-- 'moveToColdStorageAt', 'awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt' - Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
--
-- 'deleteAt', 'awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt' - Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
newAwsBackupRecoveryPointCalculatedLifecycleDetails ::
  AwsBackupRecoveryPointCalculatedLifecycleDetails
newAwsBackupRecoveryPointCalculatedLifecycleDetails =
  AwsBackupRecoveryPointCalculatedLifecycleDetails'
    { moveToColdStorageAt =
        Prelude.Nothing,
      deleteAt =
        Prelude.Nothing
    }

-- | Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt :: Lens.Lens' AwsBackupRecoveryPointCalculatedLifecycleDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCalculatedLifecycleDetails_moveToColdStorageAt = Lens.lens (\AwsBackupRecoveryPointCalculatedLifecycleDetails' {moveToColdStorageAt} -> moveToColdStorageAt) (\s@AwsBackupRecoveryPointCalculatedLifecycleDetails' {} a -> s {moveToColdStorageAt = a} :: AwsBackupRecoveryPointCalculatedLifecycleDetails)

-- | Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt :: Lens.Lens' AwsBackupRecoveryPointCalculatedLifecycleDetails (Prelude.Maybe Prelude.Text)
awsBackupRecoveryPointCalculatedLifecycleDetails_deleteAt = Lens.lens (\AwsBackupRecoveryPointCalculatedLifecycleDetails' {deleteAt} -> deleteAt) (\s@AwsBackupRecoveryPointCalculatedLifecycleDetails' {} a -> s {deleteAt = a} :: AwsBackupRecoveryPointCalculatedLifecycleDetails)

instance
  Core.FromJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  parseJSON =
    Core.withObject
      "AwsBackupRecoveryPointCalculatedLifecycleDetails"
      ( \x ->
          AwsBackupRecoveryPointCalculatedLifecycleDetails'
            Prelude.<$> (x Core..:? "MoveToColdStorageAt")
              Prelude.<*> (x Core..:? "DeleteAt")
      )

instance
  Prelude.Hashable
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  hashWithSalt
    _salt
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      _salt `Prelude.hashWithSalt` moveToColdStorageAt
        `Prelude.hashWithSalt` deleteAt

instance
  Prelude.NFData
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  rnf
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      Prelude.rnf moveToColdStorageAt
        `Prelude.seq` Prelude.rnf deleteAt

instance
  Core.ToJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails
  where
  toJSON
    AwsBackupRecoveryPointCalculatedLifecycleDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("MoveToColdStorageAt" Core..=)
                Prelude.<$> moveToColdStorageAt,
              ("DeleteAt" Core..=) Prelude.<$> deleteAt
            ]
        )

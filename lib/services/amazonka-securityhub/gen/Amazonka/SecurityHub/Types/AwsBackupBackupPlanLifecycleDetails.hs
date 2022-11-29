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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanLifecycleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides lifecycle details for the backup plan. A lifecycle defines when
-- a backup is transitioned to cold storage and when it expires.
--
-- /See:/ 'newAwsBackupBackupPlanLifecycleDetails' smart constructor.
data AwsBackupBackupPlanLifecycleDetails = AwsBackupBackupPlanLifecycleDetails'
  { -- | Specifies the number of days after creation that a recovery point is
    -- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
    deleteAfterDays :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the number of days after creation that a recovery point is
    -- moved to cold storage.
    moveToColdStorageAfterDays :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanLifecycleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAfterDays', 'awsBackupBackupPlanLifecycleDetails_deleteAfterDays' - Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
--
-- 'moveToColdStorageAfterDays', 'awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays' - Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
newAwsBackupBackupPlanLifecycleDetails ::
  AwsBackupBackupPlanLifecycleDetails
newAwsBackupBackupPlanLifecycleDetails =
  AwsBackupBackupPlanLifecycleDetails'
    { deleteAfterDays =
        Prelude.Nothing,
      moveToColdStorageAfterDays =
        Prelude.Nothing
    }

-- | Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
awsBackupBackupPlanLifecycleDetails_deleteAfterDays :: Lens.Lens' AwsBackupBackupPlanLifecycleDetails (Prelude.Maybe Prelude.Integer)
awsBackupBackupPlanLifecycleDetails_deleteAfterDays = Lens.lens (\AwsBackupBackupPlanLifecycleDetails' {deleteAfterDays} -> deleteAfterDays) (\s@AwsBackupBackupPlanLifecycleDetails' {} a -> s {deleteAfterDays = a} :: AwsBackupBackupPlanLifecycleDetails)

-- | Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays :: Lens.Lens' AwsBackupBackupPlanLifecycleDetails (Prelude.Maybe Prelude.Integer)
awsBackupBackupPlanLifecycleDetails_moveToColdStorageAfterDays = Lens.lens (\AwsBackupBackupPlanLifecycleDetails' {moveToColdStorageAfterDays} -> moveToColdStorageAfterDays) (\s@AwsBackupBackupPlanLifecycleDetails' {} a -> s {moveToColdStorageAfterDays = a} :: AwsBackupBackupPlanLifecycleDetails)

instance
  Core.FromJSON
    AwsBackupBackupPlanLifecycleDetails
  where
  parseJSON =
    Core.withObject
      "AwsBackupBackupPlanLifecycleDetails"
      ( \x ->
          AwsBackupBackupPlanLifecycleDetails'
            Prelude.<$> (x Core..:? "DeleteAfterDays")
            Prelude.<*> (x Core..:? "MoveToColdStorageAfterDays")
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanLifecycleDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanLifecycleDetails' {..} =
      _salt `Prelude.hashWithSalt` deleteAfterDays
        `Prelude.hashWithSalt` moveToColdStorageAfterDays

instance
  Prelude.NFData
    AwsBackupBackupPlanLifecycleDetails
  where
  rnf AwsBackupBackupPlanLifecycleDetails' {..} =
    Prelude.rnf deleteAfterDays
      `Prelude.seq` Prelude.rnf moveToColdStorageAfterDays

instance
  Core.ToJSON
    AwsBackupBackupPlanLifecycleDetails
  where
  toJSON AwsBackupBackupPlanLifecycleDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeleteAfterDays" Core..=)
              Prelude.<$> deleteAfterDays,
            ("MoveToColdStorageAfterDays" Core..=)
              Prelude.<$> moveToColdStorageAfterDays
          ]
      )

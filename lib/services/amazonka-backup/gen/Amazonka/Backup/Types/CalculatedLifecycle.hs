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
-- Module      : Amazonka.Backup.Types.CalculatedLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.CalculatedLifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains @DeleteAt@ and @MoveToColdStorageAt@ timestamps, which are used
-- to specify a lifecycle for a recovery point.
--
-- The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “retention” setting must be 90 days
-- greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- /See:/ 'newCalculatedLifecycle' smart constructor.
data CalculatedLifecycle = CalculatedLifecycle'
  { -- | A timestamp that specifies when to transition a recovery point to cold
    -- storage.
    moveToColdStorageAt :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that specifies when to delete a recovery point.
    deleteAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'moveToColdStorageAt', 'calculatedLifecycle_moveToColdStorageAt' - A timestamp that specifies when to transition a recovery point to cold
-- storage.
--
-- 'deleteAt', 'calculatedLifecycle_deleteAt' - A timestamp that specifies when to delete a recovery point.
newCalculatedLifecycle ::
  CalculatedLifecycle
newCalculatedLifecycle =
  CalculatedLifecycle'
    { moveToColdStorageAt =
        Prelude.Nothing,
      deleteAt = Prelude.Nothing
    }

-- | A timestamp that specifies when to transition a recovery point to cold
-- storage.
calculatedLifecycle_moveToColdStorageAt :: Lens.Lens' CalculatedLifecycle (Prelude.Maybe Prelude.UTCTime)
calculatedLifecycle_moveToColdStorageAt = Lens.lens (\CalculatedLifecycle' {moveToColdStorageAt} -> moveToColdStorageAt) (\s@CalculatedLifecycle' {} a -> s {moveToColdStorageAt = a} :: CalculatedLifecycle) Prelude.. Lens.mapping Data._Time

-- | A timestamp that specifies when to delete a recovery point.
calculatedLifecycle_deleteAt :: Lens.Lens' CalculatedLifecycle (Prelude.Maybe Prelude.UTCTime)
calculatedLifecycle_deleteAt = Lens.lens (\CalculatedLifecycle' {deleteAt} -> deleteAt) (\s@CalculatedLifecycle' {} a -> s {deleteAt = a} :: CalculatedLifecycle) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CalculatedLifecycle where
  parseJSON =
    Data.withObject
      "CalculatedLifecycle"
      ( \x ->
          CalculatedLifecycle'
            Prelude.<$> (x Data..:? "MoveToColdStorageAt")
            Prelude.<*> (x Data..:? "DeleteAt")
      )

instance Prelude.Hashable CalculatedLifecycle where
  hashWithSalt _salt CalculatedLifecycle' {..} =
    _salt `Prelude.hashWithSalt` moveToColdStorageAt
      `Prelude.hashWithSalt` deleteAt

instance Prelude.NFData CalculatedLifecycle where
  rnf CalculatedLifecycle' {..} =
    Prelude.rnf moveToColdStorageAt
      `Prelude.seq` Prelude.rnf deleteAt

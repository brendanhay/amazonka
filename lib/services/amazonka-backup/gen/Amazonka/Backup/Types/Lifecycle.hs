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
-- Module      : Amazonka.Backup.Types.Lifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Lifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an array of @Transition@ objects specifying how long in days
-- before a recovery point transitions to cold storage or is deleted.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, on the console, the “retention” setting
-- must be 90 days greater than the “transition to cold after days”
-- setting. The “transition to cold after days” setting cannot be changed
-- after a backup has been transitioned to cold.
--
-- Resource types that are able to be transitioned to cold storage are
-- listed in the \"Lifecycle to cold storage\" section of the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html#features-by-resource Feature availability by resource>
-- table. Backup ignores this expression for other resource types.
--
-- /See:/ 'newLifecycle' smart constructor.
data Lifecycle = Lifecycle'
  { -- | Specifies the number of days after creation that a recovery point is
    -- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
    deleteAfterDays :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the number of days after creation that a recovery point is
    -- moved to cold storage.
    moveToColdStorageAfterDays :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Lifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAfterDays', 'lifecycle_deleteAfterDays' - Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
--
-- 'moveToColdStorageAfterDays', 'lifecycle_moveToColdStorageAfterDays' - Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
newLifecycle ::
  Lifecycle
newLifecycle =
  Lifecycle'
    { deleteAfterDays = Prelude.Nothing,
      moveToColdStorageAfterDays = Prelude.Nothing
    }

-- | Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
lifecycle_deleteAfterDays :: Lens.Lens' Lifecycle (Prelude.Maybe Prelude.Integer)
lifecycle_deleteAfterDays = Lens.lens (\Lifecycle' {deleteAfterDays} -> deleteAfterDays) (\s@Lifecycle' {} a -> s {deleteAfterDays = a} :: Lifecycle)

-- | Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
lifecycle_moveToColdStorageAfterDays :: Lens.Lens' Lifecycle (Prelude.Maybe Prelude.Integer)
lifecycle_moveToColdStorageAfterDays = Lens.lens (\Lifecycle' {moveToColdStorageAfterDays} -> moveToColdStorageAfterDays) (\s@Lifecycle' {} a -> s {moveToColdStorageAfterDays = a} :: Lifecycle)

instance Data.FromJSON Lifecycle where
  parseJSON =
    Data.withObject
      "Lifecycle"
      ( \x ->
          Lifecycle'
            Prelude.<$> (x Data..:? "DeleteAfterDays")
            Prelude.<*> (x Data..:? "MoveToColdStorageAfterDays")
      )

instance Prelude.Hashable Lifecycle where
  hashWithSalt _salt Lifecycle' {..} =
    _salt
      `Prelude.hashWithSalt` deleteAfterDays
      `Prelude.hashWithSalt` moveToColdStorageAfterDays

instance Prelude.NFData Lifecycle where
  rnf Lifecycle' {..} =
    Prelude.rnf deleteAfterDays `Prelude.seq`
      Prelude.rnf moveToColdStorageAfterDays

instance Data.ToJSON Lifecycle where
  toJSON Lifecycle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteAfterDays" Data..=)
              Prelude.<$> deleteAfterDays,
            ("MoveToColdStorageAfterDays" Data..=)
              Prelude.<$> moveToColdStorageAfterDays
          ]
      )

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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Lifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an array of @Transition@ objects specifying how long in days
-- before a recovery point transitions to cold storage or is deleted.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, on the console, the “expire after days”
-- setting must be 90 days greater than the “transition to cold after days”
-- setting. The “transition to cold after days” setting cannot be changed
-- after a backup has been transitioned to cold.
--
-- Only Amazon EFS file system backups can be transitioned to cold storage.
--
-- /See:/ 'newLifecycle' smart constructor.
data Lifecycle = Lifecycle'
  { -- | Specifies the number of days after creation that a recovery point is
    -- moved to cold storage.
    moveToColdStorageAfterDays :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the number of days after creation that a recovery point is
    -- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
    deleteAfterDays :: Prelude.Maybe Prelude.Integer
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
-- 'moveToColdStorageAfterDays', 'lifecycle_moveToColdStorageAfterDays' - Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
--
-- 'deleteAfterDays', 'lifecycle_deleteAfterDays' - Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
newLifecycle ::
  Lifecycle
newLifecycle =
  Lifecycle'
    { moveToColdStorageAfterDays =
        Prelude.Nothing,
      deleteAfterDays = Prelude.Nothing
    }

-- | Specifies the number of days after creation that a recovery point is
-- moved to cold storage.
lifecycle_moveToColdStorageAfterDays :: Lens.Lens' Lifecycle (Prelude.Maybe Prelude.Integer)
lifecycle_moveToColdStorageAfterDays = Lens.lens (\Lifecycle' {moveToColdStorageAfterDays} -> moveToColdStorageAfterDays) (\s@Lifecycle' {} a -> s {moveToColdStorageAfterDays = a} :: Lifecycle)

-- | Specifies the number of days after creation that a recovery point is
-- deleted. Must be greater than 90 days plus @MoveToColdStorageAfterDays@.
lifecycle_deleteAfterDays :: Lens.Lens' Lifecycle (Prelude.Maybe Prelude.Integer)
lifecycle_deleteAfterDays = Lens.lens (\Lifecycle' {deleteAfterDays} -> deleteAfterDays) (\s@Lifecycle' {} a -> s {deleteAfterDays = a} :: Lifecycle)

instance Core.FromJSON Lifecycle where
  parseJSON =
    Core.withObject
      "Lifecycle"
      ( \x ->
          Lifecycle'
            Prelude.<$> (x Core..:? "MoveToColdStorageAfterDays")
            Prelude.<*> (x Core..:? "DeleteAfterDays")
      )

instance Prelude.Hashable Lifecycle

instance Prelude.NFData Lifecycle

instance Core.ToJSON Lifecycle where
  toJSON Lifecycle' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MoveToColdStorageAfterDays" Core..=)
              Prelude.<$> moveToColdStorageAfterDays,
            ("DeleteAfterDays" Core..=)
              Prelude.<$> deleteAfterDays
          ]
      )

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
-- Module      : Amazonka.Lightsail.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PendingMaintenanceAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a pending database maintenance action.
--
-- /See:/ 'newPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The type of pending database maintenance action.
    action :: Prelude.Maybe Prelude.Text,
    -- | The effective date of the pending database maintenance action.
    currentApplyDate :: Prelude.Maybe Data.POSIX,
    -- | Additional detail about the pending database maintenance action.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingMaintenanceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'pendingMaintenanceAction_action' - The type of pending database maintenance action.
--
-- 'currentApplyDate', 'pendingMaintenanceAction_currentApplyDate' - The effective date of the pending database maintenance action.
--
-- 'description', 'pendingMaintenanceAction_description' - Additional detail about the pending database maintenance action.
newPendingMaintenanceAction ::
  PendingMaintenanceAction
newPendingMaintenanceAction =
  PendingMaintenanceAction'
    { action = Prelude.Nothing,
      currentApplyDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The type of pending database maintenance action.
pendingMaintenanceAction_action :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_action = Lens.lens (\PendingMaintenanceAction' {action} -> action) (\s@PendingMaintenanceAction' {} a -> s {action = a} :: PendingMaintenanceAction)

-- | The effective date of the pending database maintenance action.
pendingMaintenanceAction_currentApplyDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_currentApplyDate = Lens.lens (\PendingMaintenanceAction' {currentApplyDate} -> currentApplyDate) (\s@PendingMaintenanceAction' {} a -> s {currentApplyDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Data._Time

-- | Additional detail about the pending database maintenance action.
pendingMaintenanceAction_description :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_description = Lens.lens (\PendingMaintenanceAction' {description} -> description) (\s@PendingMaintenanceAction' {} a -> s {description = a} :: PendingMaintenanceAction)

instance Data.FromJSON PendingMaintenanceAction where
  parseJSON =
    Data.withObject
      "PendingMaintenanceAction"
      ( \x ->
          PendingMaintenanceAction'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "currentApplyDate")
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable PendingMaintenanceAction where
  hashWithSalt _salt PendingMaintenanceAction' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` currentApplyDate
      `Prelude.hashWithSalt` description

instance Prelude.NFData PendingMaintenanceAction where
  rnf PendingMaintenanceAction' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf currentApplyDate
      `Prelude.seq` Prelude.rnf description

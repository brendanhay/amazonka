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
-- Module      : Network.AWS.Lightsail.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingMaintenanceAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a pending database maintenance action.
--
-- /See:/ 'newPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The effective date of the pending database maintenance action.
    currentApplyDate :: Core.Maybe Core.POSIX,
    -- | The type of pending database maintenance action.
    action :: Core.Maybe Core.Text,
    -- | Additional detail about the pending database maintenance action.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PendingMaintenanceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentApplyDate', 'pendingMaintenanceAction_currentApplyDate' - The effective date of the pending database maintenance action.
--
-- 'action', 'pendingMaintenanceAction_action' - The type of pending database maintenance action.
--
-- 'description', 'pendingMaintenanceAction_description' - Additional detail about the pending database maintenance action.
newPendingMaintenanceAction ::
  PendingMaintenanceAction
newPendingMaintenanceAction =
  PendingMaintenanceAction'
    { currentApplyDate =
        Core.Nothing,
      action = Core.Nothing,
      description = Core.Nothing
    }

-- | The effective date of the pending database maintenance action.
pendingMaintenanceAction_currentApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pendingMaintenanceAction_currentApplyDate = Lens.lens (\PendingMaintenanceAction' {currentApplyDate} -> currentApplyDate) (\s@PendingMaintenanceAction' {} a -> s {currentApplyDate = a} :: PendingMaintenanceAction) Core.. Lens.mapping Core._Time

-- | The type of pending database maintenance action.
pendingMaintenanceAction_action :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pendingMaintenanceAction_action = Lens.lens (\PendingMaintenanceAction' {action} -> action) (\s@PendingMaintenanceAction' {} a -> s {action = a} :: PendingMaintenanceAction)

-- | Additional detail about the pending database maintenance action.
pendingMaintenanceAction_description :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pendingMaintenanceAction_description = Lens.lens (\PendingMaintenanceAction' {description} -> description) (\s@PendingMaintenanceAction' {} a -> s {description = a} :: PendingMaintenanceAction)

instance Core.FromJSON PendingMaintenanceAction where
  parseJSON =
    Core.withObject
      "PendingMaintenanceAction"
      ( \x ->
          PendingMaintenanceAction'
            Core.<$> (x Core..:? "currentApplyDate")
            Core.<*> (x Core..:? "action")
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable PendingMaintenanceAction

instance Core.NFData PendingMaintenanceAction

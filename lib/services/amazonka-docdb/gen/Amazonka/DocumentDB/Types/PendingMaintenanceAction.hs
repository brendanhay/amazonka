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
-- Module      : Amazonka.DocumentDB.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.PendingMaintenanceAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'newPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The date of the maintenance window when the action is applied. The
    -- maintenance action is applied to the resource during its first
    -- maintenance window after this date. If this date is specified, any
    -- @next-maintenance@ opt-in requests are ignored.
    autoAppliedAfterDate :: Prelude.Maybe Core.ISO8601,
    -- | The type of pending maintenance action that is available for the
    -- resource.
    action :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of opt-in request that has been received for the
    -- resource.
    optInStatus :: Prelude.Maybe Prelude.Text,
    -- | A description providing more detail about the maintenance action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date when the maintenance action is automatically applied. The
    -- maintenance action is applied to the resource on this date regardless of
    -- the maintenance window for the resource. If this date is specified, any
    -- @immediate@ opt-in requests are ignored.
    forcedApplyDate :: Prelude.Maybe Core.ISO8601,
    -- | The effective date when the pending maintenance action is applied to the
    -- resource.
    currentApplyDate :: Prelude.Maybe Core.ISO8601
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
-- 'autoAppliedAfterDate', 'pendingMaintenanceAction_autoAppliedAfterDate' - The date of the maintenance window when the action is applied. The
-- maintenance action is applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- @next-maintenance@ opt-in requests are ignored.
--
-- 'action', 'pendingMaintenanceAction_action' - The type of pending maintenance action that is available for the
-- resource.
--
-- 'optInStatus', 'pendingMaintenanceAction_optInStatus' - Indicates the type of opt-in request that has been received for the
-- resource.
--
-- 'description', 'pendingMaintenanceAction_description' - A description providing more detail about the maintenance action.
--
-- 'forcedApplyDate', 'pendingMaintenanceAction_forcedApplyDate' - The date when the maintenance action is automatically applied. The
-- maintenance action is applied to the resource on this date regardless of
-- the maintenance window for the resource. If this date is specified, any
-- @immediate@ opt-in requests are ignored.
--
-- 'currentApplyDate', 'pendingMaintenanceAction_currentApplyDate' - The effective date when the pending maintenance action is applied to the
-- resource.
newPendingMaintenanceAction ::
  PendingMaintenanceAction
newPendingMaintenanceAction =
  PendingMaintenanceAction'
    { autoAppliedAfterDate =
        Prelude.Nothing,
      action = Prelude.Nothing,
      optInStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      forcedApplyDate = Prelude.Nothing,
      currentApplyDate = Prelude.Nothing
    }

-- | The date of the maintenance window when the action is applied. The
-- maintenance action is applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- @next-maintenance@ opt-in requests are ignored.
pendingMaintenanceAction_autoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_autoAppliedAfterDate = Lens.lens (\PendingMaintenanceAction' {autoAppliedAfterDate} -> autoAppliedAfterDate) (\s@PendingMaintenanceAction' {} a -> s {autoAppliedAfterDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Core._Time

-- | The type of pending maintenance action that is available for the
-- resource.
pendingMaintenanceAction_action :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_action = Lens.lens (\PendingMaintenanceAction' {action} -> action) (\s@PendingMaintenanceAction' {} a -> s {action = a} :: PendingMaintenanceAction)

-- | Indicates the type of opt-in request that has been received for the
-- resource.
pendingMaintenanceAction_optInStatus :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_optInStatus = Lens.lens (\PendingMaintenanceAction' {optInStatus} -> optInStatus) (\s@PendingMaintenanceAction' {} a -> s {optInStatus = a} :: PendingMaintenanceAction)

-- | A description providing more detail about the maintenance action.
pendingMaintenanceAction_description :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_description = Lens.lens (\PendingMaintenanceAction' {description} -> description) (\s@PendingMaintenanceAction' {} a -> s {description = a} :: PendingMaintenanceAction)

-- | The date when the maintenance action is automatically applied. The
-- maintenance action is applied to the resource on this date regardless of
-- the maintenance window for the resource. If this date is specified, any
-- @immediate@ opt-in requests are ignored.
pendingMaintenanceAction_forcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_forcedApplyDate = Lens.lens (\PendingMaintenanceAction' {forcedApplyDate} -> forcedApplyDate) (\s@PendingMaintenanceAction' {} a -> s {forcedApplyDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Core._Time

-- | The effective date when the pending maintenance action is applied to the
-- resource.
pendingMaintenanceAction_currentApplyDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_currentApplyDate = Lens.lens (\PendingMaintenanceAction' {currentApplyDate} -> currentApplyDate) (\s@PendingMaintenanceAction' {} a -> s {currentApplyDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Core._Time

instance Core.FromXML PendingMaintenanceAction where
  parseXML x =
    PendingMaintenanceAction'
      Prelude.<$> (x Core..@? "AutoAppliedAfterDate")
      Prelude.<*> (x Core..@? "Action")
      Prelude.<*> (x Core..@? "OptInStatus")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "ForcedApplyDate")
      Prelude.<*> (x Core..@? "CurrentApplyDate")

instance Prelude.Hashable PendingMaintenanceAction

instance Prelude.NFData PendingMaintenanceAction

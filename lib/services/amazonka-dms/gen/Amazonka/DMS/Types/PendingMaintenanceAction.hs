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
-- Module      : Amazonka.DMS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.PendingMaintenanceAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a maintenance action pending for an DMS resource, including
-- when and how it will be applied. This data type is a response element to
-- the @DescribePendingMaintenanceActions@ operation.
--
-- /See:/ 'newPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The type of pending maintenance action that is available for the
    -- resource.
    action :: Prelude.Maybe Prelude.Text,
    -- | The date of the maintenance window when the action is to be applied. The
    -- maintenance action is applied to the resource during its first
    -- maintenance window after this date. If this date is specified, any
    -- @next-maintenance@ opt-in requests are ignored.
    autoAppliedAfterDate :: Prelude.Maybe Data.POSIX,
    -- | The effective date when the pending maintenance action will be applied
    -- to the resource. This date takes into account opt-in requests received
    -- from the @ApplyPendingMaintenanceAction@ API operation, and also the
    -- @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This
    -- value is blank if an opt-in request has not been received and nothing
    -- has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@.
    currentApplyDate :: Prelude.Maybe Data.POSIX,
    -- | A description providing more detail about the maintenance action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date when the maintenance action will be automatically applied. The
    -- maintenance action is applied to the resource on this date regardless of
    -- the maintenance window for the resource. If this date is specified, any
    -- @immediate@ opt-in requests are ignored.
    forcedApplyDate :: Prelude.Maybe Data.POSIX,
    -- | The type of opt-in request that has been received for the resource.
    optInStatus :: Prelude.Maybe Prelude.Text
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
-- 'action', 'pendingMaintenanceAction_action' - The type of pending maintenance action that is available for the
-- resource.
--
-- 'autoAppliedAfterDate', 'pendingMaintenanceAction_autoAppliedAfterDate' - The date of the maintenance window when the action is to be applied. The
-- maintenance action is applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- @next-maintenance@ opt-in requests are ignored.
--
-- 'currentApplyDate', 'pendingMaintenanceAction_currentApplyDate' - The effective date when the pending maintenance action will be applied
-- to the resource. This date takes into account opt-in requests received
-- from the @ApplyPendingMaintenanceAction@ API operation, and also the
-- @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This
-- value is blank if an opt-in request has not been received and nothing
-- has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@.
--
-- 'description', 'pendingMaintenanceAction_description' - A description providing more detail about the maintenance action.
--
-- 'forcedApplyDate', 'pendingMaintenanceAction_forcedApplyDate' - The date when the maintenance action will be automatically applied. The
-- maintenance action is applied to the resource on this date regardless of
-- the maintenance window for the resource. If this date is specified, any
-- @immediate@ opt-in requests are ignored.
--
-- 'optInStatus', 'pendingMaintenanceAction_optInStatus' - The type of opt-in request that has been received for the resource.
newPendingMaintenanceAction ::
  PendingMaintenanceAction
newPendingMaintenanceAction =
  PendingMaintenanceAction'
    { action = Prelude.Nothing,
      autoAppliedAfterDate = Prelude.Nothing,
      currentApplyDate = Prelude.Nothing,
      description = Prelude.Nothing,
      forcedApplyDate = Prelude.Nothing,
      optInStatus = Prelude.Nothing
    }

-- | The type of pending maintenance action that is available for the
-- resource.
pendingMaintenanceAction_action :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_action = Lens.lens (\PendingMaintenanceAction' {action} -> action) (\s@PendingMaintenanceAction' {} a -> s {action = a} :: PendingMaintenanceAction)

-- | The date of the maintenance window when the action is to be applied. The
-- maintenance action is applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- @next-maintenance@ opt-in requests are ignored.
pendingMaintenanceAction_autoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_autoAppliedAfterDate = Lens.lens (\PendingMaintenanceAction' {autoAppliedAfterDate} -> autoAppliedAfterDate) (\s@PendingMaintenanceAction' {} a -> s {autoAppliedAfterDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Data._Time

-- | The effective date when the pending maintenance action will be applied
-- to the resource. This date takes into account opt-in requests received
-- from the @ApplyPendingMaintenanceAction@ API operation, and also the
-- @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This
-- value is blank if an opt-in request has not been received and nothing
-- has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@.
pendingMaintenanceAction_currentApplyDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_currentApplyDate = Lens.lens (\PendingMaintenanceAction' {currentApplyDate} -> currentApplyDate) (\s@PendingMaintenanceAction' {} a -> s {currentApplyDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Data._Time

-- | A description providing more detail about the maintenance action.
pendingMaintenanceAction_description :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_description = Lens.lens (\PendingMaintenanceAction' {description} -> description) (\s@PendingMaintenanceAction' {} a -> s {description = a} :: PendingMaintenanceAction)

-- | The date when the maintenance action will be automatically applied. The
-- maintenance action is applied to the resource on this date regardless of
-- the maintenance window for the resource. If this date is specified, any
-- @immediate@ opt-in requests are ignored.
pendingMaintenanceAction_forcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.UTCTime)
pendingMaintenanceAction_forcedApplyDate = Lens.lens (\PendingMaintenanceAction' {forcedApplyDate} -> forcedApplyDate) (\s@PendingMaintenanceAction' {} a -> s {forcedApplyDate = a} :: PendingMaintenanceAction) Prelude.. Lens.mapping Data._Time

-- | The type of opt-in request that has been received for the resource.
pendingMaintenanceAction_optInStatus :: Lens.Lens' PendingMaintenanceAction (Prelude.Maybe Prelude.Text)
pendingMaintenanceAction_optInStatus = Lens.lens (\PendingMaintenanceAction' {optInStatus} -> optInStatus) (\s@PendingMaintenanceAction' {} a -> s {optInStatus = a} :: PendingMaintenanceAction)

instance Data.FromJSON PendingMaintenanceAction where
  parseJSON =
    Data.withObject
      "PendingMaintenanceAction"
      ( \x ->
          PendingMaintenanceAction'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "AutoAppliedAfterDate")
            Prelude.<*> (x Data..:? "CurrentApplyDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ForcedApplyDate")
            Prelude.<*> (x Data..:? "OptInStatus")
      )

instance Prelude.Hashable PendingMaintenanceAction where
  hashWithSalt _salt PendingMaintenanceAction' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` autoAppliedAfterDate
      `Prelude.hashWithSalt` currentApplyDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` forcedApplyDate
      `Prelude.hashWithSalt` optInStatus

instance Prelude.NFData PendingMaintenanceAction where
  rnf PendingMaintenanceAction' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf autoAppliedAfterDate `Prelude.seq`
        Prelude.rnf currentApplyDate `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf forcedApplyDate `Prelude.seq`
              Prelude.rnf optInStatus

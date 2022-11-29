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
-- Module      : Amazonka.FSx.Types.AdministrativeAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AdministrativeAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.AdministrativeActionFailureDetails
import Amazonka.FSx.Types.AdministrativeActionType
import {-# SOURCE #-} Amazonka.FSx.Types.FileSystem
import {-# SOURCE #-} Amazonka.FSx.Types.Snapshot
import Amazonka.FSx.Types.Status
import {-# SOURCE #-} Amazonka.FSx.Types.Volume
import qualified Amazonka.Prelude as Prelude

-- | Describes a specific Amazon FSx administrative action for the current
-- Windows, Lustre, or OpenZFS file system.
--
-- /See:/ 'newAdministrativeAction' smart constructor.
data AdministrativeAction = AdministrativeAction'
  { -- | Describes the target value for the administration action, provided in
    -- the @UpdateFileSystem@ operation. Returned for @FILE_SYSTEM_UPDATE@
    -- administrative actions.
    targetFileSystemValues :: Prelude.Maybe FileSystem,
    -- | The time that the administrative action request was received.
    requestTime :: Prelude.Maybe Core.POSIX,
    -- | Describes the status of the administrative action, as follows:
    --
    -- -   @FAILED@ - Amazon FSx failed to process the administrative action
    --     successfully.
    --
    -- -   @IN_PROGRESS@ - Amazon FSx is processing the administrative action.
    --
    -- -   @PENDING@ - Amazon FSx is waiting to process the administrative
    --     action.
    --
    -- -   @COMPLETED@ - Amazon FSx has finished processing the administrative
    --     task.
    --
    -- -   @UPDATED_OPTIMIZING@ - For a storage-capacity increase update,
    --     Amazon FSx has updated the file system with the new storage
    --     capacity, and is now performing the storage-optimization process.
    status :: Prelude.Maybe Status,
    targetVolumeValues :: Prelude.Maybe Volume,
    failureDetails :: Prelude.Maybe AdministrativeActionFailureDetails,
    targetSnapshotValues :: Prelude.Maybe Snapshot,
    -- | The percentage-complete status of a @STORAGE_OPTIMIZATION@
    -- administrative action. Does not apply to any other administrative action
    -- type.
    progressPercent :: Prelude.Maybe Prelude.Natural,
    administrativeActionType :: Prelude.Maybe AdministrativeActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdministrativeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetFileSystemValues', 'administrativeAction_targetFileSystemValues' - Describes the target value for the administration action, provided in
-- the @UpdateFileSystem@ operation. Returned for @FILE_SYSTEM_UPDATE@
-- administrative actions.
--
-- 'requestTime', 'administrativeAction_requestTime' - The time that the administrative action request was received.
--
-- 'status', 'administrativeAction_status' - Describes the status of the administrative action, as follows:
--
-- -   @FAILED@ - Amazon FSx failed to process the administrative action
--     successfully.
--
-- -   @IN_PROGRESS@ - Amazon FSx is processing the administrative action.
--
-- -   @PENDING@ - Amazon FSx is waiting to process the administrative
--     action.
--
-- -   @COMPLETED@ - Amazon FSx has finished processing the administrative
--     task.
--
-- -   @UPDATED_OPTIMIZING@ - For a storage-capacity increase update,
--     Amazon FSx has updated the file system with the new storage
--     capacity, and is now performing the storage-optimization process.
--
-- 'targetVolumeValues', 'administrativeAction_targetVolumeValues' - Undocumented member.
--
-- 'failureDetails', 'administrativeAction_failureDetails' - Undocumented member.
--
-- 'targetSnapshotValues', 'administrativeAction_targetSnapshotValues' - Undocumented member.
--
-- 'progressPercent', 'administrativeAction_progressPercent' - The percentage-complete status of a @STORAGE_OPTIMIZATION@
-- administrative action. Does not apply to any other administrative action
-- type.
--
-- 'administrativeActionType', 'administrativeAction_administrativeActionType' - Undocumented member.
newAdministrativeAction ::
  AdministrativeAction
newAdministrativeAction =
  AdministrativeAction'
    { targetFileSystemValues =
        Prelude.Nothing,
      requestTime = Prelude.Nothing,
      status = Prelude.Nothing,
      targetVolumeValues = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      targetSnapshotValues = Prelude.Nothing,
      progressPercent = Prelude.Nothing,
      administrativeActionType = Prelude.Nothing
    }

-- | Describes the target value for the administration action, provided in
-- the @UpdateFileSystem@ operation. Returned for @FILE_SYSTEM_UPDATE@
-- administrative actions.
administrativeAction_targetFileSystemValues :: Lens.Lens' AdministrativeAction (Prelude.Maybe FileSystem)
administrativeAction_targetFileSystemValues = Lens.lens (\AdministrativeAction' {targetFileSystemValues} -> targetFileSystemValues) (\s@AdministrativeAction' {} a -> s {targetFileSystemValues = a} :: AdministrativeAction)

-- | The time that the administrative action request was received.
administrativeAction_requestTime :: Lens.Lens' AdministrativeAction (Prelude.Maybe Prelude.UTCTime)
administrativeAction_requestTime = Lens.lens (\AdministrativeAction' {requestTime} -> requestTime) (\s@AdministrativeAction' {} a -> s {requestTime = a} :: AdministrativeAction) Prelude.. Lens.mapping Core._Time

-- | Describes the status of the administrative action, as follows:
--
-- -   @FAILED@ - Amazon FSx failed to process the administrative action
--     successfully.
--
-- -   @IN_PROGRESS@ - Amazon FSx is processing the administrative action.
--
-- -   @PENDING@ - Amazon FSx is waiting to process the administrative
--     action.
--
-- -   @COMPLETED@ - Amazon FSx has finished processing the administrative
--     task.
--
-- -   @UPDATED_OPTIMIZING@ - For a storage-capacity increase update,
--     Amazon FSx has updated the file system with the new storage
--     capacity, and is now performing the storage-optimization process.
administrativeAction_status :: Lens.Lens' AdministrativeAction (Prelude.Maybe Status)
administrativeAction_status = Lens.lens (\AdministrativeAction' {status} -> status) (\s@AdministrativeAction' {} a -> s {status = a} :: AdministrativeAction)

-- | Undocumented member.
administrativeAction_targetVolumeValues :: Lens.Lens' AdministrativeAction (Prelude.Maybe Volume)
administrativeAction_targetVolumeValues = Lens.lens (\AdministrativeAction' {targetVolumeValues} -> targetVolumeValues) (\s@AdministrativeAction' {} a -> s {targetVolumeValues = a} :: AdministrativeAction)

-- | Undocumented member.
administrativeAction_failureDetails :: Lens.Lens' AdministrativeAction (Prelude.Maybe AdministrativeActionFailureDetails)
administrativeAction_failureDetails = Lens.lens (\AdministrativeAction' {failureDetails} -> failureDetails) (\s@AdministrativeAction' {} a -> s {failureDetails = a} :: AdministrativeAction)

-- | Undocumented member.
administrativeAction_targetSnapshotValues :: Lens.Lens' AdministrativeAction (Prelude.Maybe Snapshot)
administrativeAction_targetSnapshotValues = Lens.lens (\AdministrativeAction' {targetSnapshotValues} -> targetSnapshotValues) (\s@AdministrativeAction' {} a -> s {targetSnapshotValues = a} :: AdministrativeAction)

-- | The percentage-complete status of a @STORAGE_OPTIMIZATION@
-- administrative action. Does not apply to any other administrative action
-- type.
administrativeAction_progressPercent :: Lens.Lens' AdministrativeAction (Prelude.Maybe Prelude.Natural)
administrativeAction_progressPercent = Lens.lens (\AdministrativeAction' {progressPercent} -> progressPercent) (\s@AdministrativeAction' {} a -> s {progressPercent = a} :: AdministrativeAction)

-- | Undocumented member.
administrativeAction_administrativeActionType :: Lens.Lens' AdministrativeAction (Prelude.Maybe AdministrativeActionType)
administrativeAction_administrativeActionType = Lens.lens (\AdministrativeAction' {administrativeActionType} -> administrativeActionType) (\s@AdministrativeAction' {} a -> s {administrativeActionType = a} :: AdministrativeAction)

instance Core.FromJSON AdministrativeAction where
  parseJSON =
    Core.withObject
      "AdministrativeAction"
      ( \x ->
          AdministrativeAction'
            Prelude.<$> (x Core..:? "TargetFileSystemValues")
            Prelude.<*> (x Core..:? "RequestTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "TargetVolumeValues")
            Prelude.<*> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "TargetSnapshotValues")
            Prelude.<*> (x Core..:? "ProgressPercent")
            Prelude.<*> (x Core..:? "AdministrativeActionType")
      )

instance Prelude.Hashable AdministrativeAction where
  hashWithSalt _salt AdministrativeAction' {..} =
    _salt `Prelude.hashWithSalt` targetFileSystemValues
      `Prelude.hashWithSalt` requestTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetVolumeValues
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` targetSnapshotValues
      `Prelude.hashWithSalt` progressPercent
      `Prelude.hashWithSalt` administrativeActionType

instance Prelude.NFData AdministrativeAction where
  rnf AdministrativeAction' {..} =
    Prelude.rnf targetFileSystemValues
      `Prelude.seq` Prelude.rnf requestTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetVolumeValues
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf targetSnapshotValues
      `Prelude.seq` Prelude.rnf progressPercent
      `Prelude.seq` Prelude.rnf administrativeActionType

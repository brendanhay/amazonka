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
-- Module      : Amazonka.SecurityHub.Types.PatchSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PatchSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides an overview of the patch compliance status for an instance
-- against a selected compliance standard.
--
-- /See:/ 'newPatchSummary' smart constructor.
data PatchSummary = PatchSummary'
  { -- | The number of installed patches that are not part of the compliance
    -- standard.
    installedOtherCount :: Prelude.Maybe Prelude.Int,
    -- | The reboot option specified for the instance.
    rebootOption :: Prelude.Maybe Prelude.Text,
    -- | The number of patches from the compliance standard that failed to
    -- install.
    failedCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the operation completed.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    operationEndTime :: Prelude.Maybe Prelude.Text,
    -- | The number of patches from the compliance standard that were installed
    -- successfully.
    installedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches that are installed but are also on a list of
    -- patches that the customer rejected.
    installedRejectedCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates when the operation started.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    operationStartTime :: Prelude.Maybe Prelude.Text,
    -- | The number of patches that are part of the compliance standard but are
    -- not installed. The count includes patches that failed to install.
    missingCount :: Prelude.Maybe Prelude.Int,
    -- | The type of patch operation performed. For Patch Manager, the values are
    -- @SCAN@ and @INSTALL@.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The number of patches that were applied, but that require the instance
    -- to be rebooted in order to be marked as installed.
    installedPendingReboot :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the compliance standard that was used to determine the
    -- patch compliance status.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installedOtherCount', 'patchSummary_installedOtherCount' - The number of installed patches that are not part of the compliance
-- standard.
--
-- 'rebootOption', 'patchSummary_rebootOption' - The reboot option specified for the instance.
--
-- 'failedCount', 'patchSummary_failedCount' - The number of patches from the compliance standard that failed to
-- install.
--
-- 'operationEndTime', 'patchSummary_operationEndTime' - Indicates when the operation completed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'installedCount', 'patchSummary_installedCount' - The number of patches from the compliance standard that were installed
-- successfully.
--
-- 'installedRejectedCount', 'patchSummary_installedRejectedCount' - The number of patches that are installed but are also on a list of
-- patches that the customer rejected.
--
-- 'operationStartTime', 'patchSummary_operationStartTime' - Indicates when the operation started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'missingCount', 'patchSummary_missingCount' - The number of patches that are part of the compliance standard but are
-- not installed. The count includes patches that failed to install.
--
-- 'operation', 'patchSummary_operation' - The type of patch operation performed. For Patch Manager, the values are
-- @SCAN@ and @INSTALL@.
--
-- 'installedPendingReboot', 'patchSummary_installedPendingReboot' - The number of patches that were applied, but that require the instance
-- to be rebooted in order to be marked as installed.
--
-- 'id', 'patchSummary_id' - The identifier of the compliance standard that was used to determine the
-- patch compliance status.
newPatchSummary ::
  -- | 'id'
  Prelude.Text ->
  PatchSummary
newPatchSummary pId_ =
  PatchSummary'
    { installedOtherCount =
        Prelude.Nothing,
      rebootOption = Prelude.Nothing,
      failedCount = Prelude.Nothing,
      operationEndTime = Prelude.Nothing,
      installedCount = Prelude.Nothing,
      installedRejectedCount = Prelude.Nothing,
      operationStartTime = Prelude.Nothing,
      missingCount = Prelude.Nothing,
      operation = Prelude.Nothing,
      installedPendingReboot = Prelude.Nothing,
      id = pId_
    }

-- | The number of installed patches that are not part of the compliance
-- standard.
patchSummary_installedOtherCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedOtherCount = Lens.lens (\PatchSummary' {installedOtherCount} -> installedOtherCount) (\s@PatchSummary' {} a -> s {installedOtherCount = a} :: PatchSummary)

-- | The reboot option specified for the instance.
patchSummary_rebootOption :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_rebootOption = Lens.lens (\PatchSummary' {rebootOption} -> rebootOption) (\s@PatchSummary' {} a -> s {rebootOption = a} :: PatchSummary)

-- | The number of patches from the compliance standard that failed to
-- install.
patchSummary_failedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_failedCount = Lens.lens (\PatchSummary' {failedCount} -> failedCount) (\s@PatchSummary' {} a -> s {failedCount = a} :: PatchSummary)

-- | Indicates when the operation completed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
patchSummary_operationEndTime :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operationEndTime = Lens.lens (\PatchSummary' {operationEndTime} -> operationEndTime) (\s@PatchSummary' {} a -> s {operationEndTime = a} :: PatchSummary)

-- | The number of patches from the compliance standard that were installed
-- successfully.
patchSummary_installedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedCount = Lens.lens (\PatchSummary' {installedCount} -> installedCount) (\s@PatchSummary' {} a -> s {installedCount = a} :: PatchSummary)

-- | The number of patches that are installed but are also on a list of
-- patches that the customer rejected.
patchSummary_installedRejectedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedRejectedCount = Lens.lens (\PatchSummary' {installedRejectedCount} -> installedRejectedCount) (\s@PatchSummary' {} a -> s {installedRejectedCount = a} :: PatchSummary)

-- | Indicates when the operation started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
patchSummary_operationStartTime :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operationStartTime = Lens.lens (\PatchSummary' {operationStartTime} -> operationStartTime) (\s@PatchSummary' {} a -> s {operationStartTime = a} :: PatchSummary)

-- | The number of patches that are part of the compliance standard but are
-- not installed. The count includes patches that failed to install.
patchSummary_missingCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_missingCount = Lens.lens (\PatchSummary' {missingCount} -> missingCount) (\s@PatchSummary' {} a -> s {missingCount = a} :: PatchSummary)

-- | The type of patch operation performed. For Patch Manager, the values are
-- @SCAN@ and @INSTALL@.
patchSummary_operation :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operation = Lens.lens (\PatchSummary' {operation} -> operation) (\s@PatchSummary' {} a -> s {operation = a} :: PatchSummary)

-- | The number of patches that were applied, but that require the instance
-- to be rebooted in order to be marked as installed.
patchSummary_installedPendingReboot :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedPendingReboot = Lens.lens (\PatchSummary' {installedPendingReboot} -> installedPendingReboot) (\s@PatchSummary' {} a -> s {installedPendingReboot = a} :: PatchSummary)

-- | The identifier of the compliance standard that was used to determine the
-- patch compliance status.
patchSummary_id :: Lens.Lens' PatchSummary Prelude.Text
patchSummary_id = Lens.lens (\PatchSummary' {id} -> id) (\s@PatchSummary' {} a -> s {id = a} :: PatchSummary)

instance Core.FromJSON PatchSummary where
  parseJSON =
    Core.withObject
      "PatchSummary"
      ( \x ->
          PatchSummary'
            Prelude.<$> (x Core..:? "InstalledOtherCount")
            Prelude.<*> (x Core..:? "RebootOption")
            Prelude.<*> (x Core..:? "FailedCount")
            Prelude.<*> (x Core..:? "OperationEndTime")
            Prelude.<*> (x Core..:? "InstalledCount")
            Prelude.<*> (x Core..:? "InstalledRejectedCount")
            Prelude.<*> (x Core..:? "OperationStartTime")
            Prelude.<*> (x Core..:? "MissingCount")
            Prelude.<*> (x Core..:? "Operation")
            Prelude.<*> (x Core..:? "InstalledPendingReboot")
            Prelude.<*> (x Core..: "Id")
      )

instance Prelude.Hashable PatchSummary where
  hashWithSalt _salt PatchSummary' {..} =
    _salt `Prelude.hashWithSalt` installedOtherCount
      `Prelude.hashWithSalt` rebootOption
      `Prelude.hashWithSalt` failedCount
      `Prelude.hashWithSalt` operationEndTime
      `Prelude.hashWithSalt` installedCount
      `Prelude.hashWithSalt` installedRejectedCount
      `Prelude.hashWithSalt` operationStartTime
      `Prelude.hashWithSalt` missingCount
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` installedPendingReboot
      `Prelude.hashWithSalt` id

instance Prelude.NFData PatchSummary where
  rnf PatchSummary' {..} =
    Prelude.rnf installedOtherCount
      `Prelude.seq` Prelude.rnf rebootOption
      `Prelude.seq` Prelude.rnf failedCount
      `Prelude.seq` Prelude.rnf operationEndTime
      `Prelude.seq` Prelude.rnf installedCount
      `Prelude.seq` Prelude.rnf installedRejectedCount
      `Prelude.seq` Prelude.rnf operationStartTime
      `Prelude.seq` Prelude.rnf missingCount
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf installedPendingReboot
      `Prelude.seq` Prelude.rnf id

instance Core.ToJSON PatchSummary where
  toJSON PatchSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstalledOtherCount" Core..=)
              Prelude.<$> installedOtherCount,
            ("RebootOption" Core..=) Prelude.<$> rebootOption,
            ("FailedCount" Core..=) Prelude.<$> failedCount,
            ("OperationEndTime" Core..=)
              Prelude.<$> operationEndTime,
            ("InstalledCount" Core..=)
              Prelude.<$> installedCount,
            ("InstalledRejectedCount" Core..=)
              Prelude.<$> installedRejectedCount,
            ("OperationStartTime" Core..=)
              Prelude.<$> operationStartTime,
            ("MissingCount" Core..=) Prelude.<$> missingCount,
            ("Operation" Core..=) Prelude.<$> operation,
            ("InstalledPendingReboot" Core..=)
              Prelude.<$> installedPendingReboot,
            Prelude.Just ("Id" Core..= id)
          ]
      )

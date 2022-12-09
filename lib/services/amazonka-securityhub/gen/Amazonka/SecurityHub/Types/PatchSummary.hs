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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides an overview of the patch compliance status for an instance
-- against a selected compliance standard.
--
-- /See:/ 'newPatchSummary' smart constructor.
data PatchSummary = PatchSummary'
  { -- | The number of patches from the compliance standard that failed to
    -- install.
    failedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches from the compliance standard that were installed
    -- successfully.
    installedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of installed patches that are not part of the compliance
    -- standard.
    installedOtherCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches that were applied, but that require the instance
    -- to be rebooted in order to be marked as installed.
    installedPendingReboot :: Prelude.Maybe Prelude.Int,
    -- | The number of patches that are installed but are also on a list of
    -- patches that the customer rejected.
    installedRejectedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of patches that are part of the compliance standard but are
    -- not installed. The count includes patches that failed to install.
    missingCount :: Prelude.Maybe Prelude.Int,
    -- | The type of patch operation performed. For Patch Manager, the values are
    -- @SCAN@ and @INSTALL@.
    operation :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the operation completed.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    operationEndTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the operation started.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    operationStartTime :: Prelude.Maybe Prelude.Text,
    -- | The reboot option specified for the instance.
    rebootOption :: Prelude.Maybe Prelude.Text,
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
-- 'failedCount', 'patchSummary_failedCount' - The number of patches from the compliance standard that failed to
-- install.
--
-- 'installedCount', 'patchSummary_installedCount' - The number of patches from the compliance standard that were installed
-- successfully.
--
-- 'installedOtherCount', 'patchSummary_installedOtherCount' - The number of installed patches that are not part of the compliance
-- standard.
--
-- 'installedPendingReboot', 'patchSummary_installedPendingReboot' - The number of patches that were applied, but that require the instance
-- to be rebooted in order to be marked as installed.
--
-- 'installedRejectedCount', 'patchSummary_installedRejectedCount' - The number of patches that are installed but are also on a list of
-- patches that the customer rejected.
--
-- 'missingCount', 'patchSummary_missingCount' - The number of patches that are part of the compliance standard but are
-- not installed. The count includes patches that failed to install.
--
-- 'operation', 'patchSummary_operation' - The type of patch operation performed. For Patch Manager, the values are
-- @SCAN@ and @INSTALL@.
--
-- 'operationEndTime', 'patchSummary_operationEndTime' - Indicates when the operation completed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'operationStartTime', 'patchSummary_operationStartTime' - Indicates when the operation started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'rebootOption', 'patchSummary_rebootOption' - The reboot option specified for the instance.
--
-- 'id', 'patchSummary_id' - The identifier of the compliance standard that was used to determine the
-- patch compliance status.
newPatchSummary ::
  -- | 'id'
  Prelude.Text ->
  PatchSummary
newPatchSummary pId_ =
  PatchSummary'
    { failedCount = Prelude.Nothing,
      installedCount = Prelude.Nothing,
      installedOtherCount = Prelude.Nothing,
      installedPendingReboot = Prelude.Nothing,
      installedRejectedCount = Prelude.Nothing,
      missingCount = Prelude.Nothing,
      operation = Prelude.Nothing,
      operationEndTime = Prelude.Nothing,
      operationStartTime = Prelude.Nothing,
      rebootOption = Prelude.Nothing,
      id = pId_
    }

-- | The number of patches from the compliance standard that failed to
-- install.
patchSummary_failedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_failedCount = Lens.lens (\PatchSummary' {failedCount} -> failedCount) (\s@PatchSummary' {} a -> s {failedCount = a} :: PatchSummary)

-- | The number of patches from the compliance standard that were installed
-- successfully.
patchSummary_installedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedCount = Lens.lens (\PatchSummary' {installedCount} -> installedCount) (\s@PatchSummary' {} a -> s {installedCount = a} :: PatchSummary)

-- | The number of installed patches that are not part of the compliance
-- standard.
patchSummary_installedOtherCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedOtherCount = Lens.lens (\PatchSummary' {installedOtherCount} -> installedOtherCount) (\s@PatchSummary' {} a -> s {installedOtherCount = a} :: PatchSummary)

-- | The number of patches that were applied, but that require the instance
-- to be rebooted in order to be marked as installed.
patchSummary_installedPendingReboot :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedPendingReboot = Lens.lens (\PatchSummary' {installedPendingReboot} -> installedPendingReboot) (\s@PatchSummary' {} a -> s {installedPendingReboot = a} :: PatchSummary)

-- | The number of patches that are installed but are also on a list of
-- patches that the customer rejected.
patchSummary_installedRejectedCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_installedRejectedCount = Lens.lens (\PatchSummary' {installedRejectedCount} -> installedRejectedCount) (\s@PatchSummary' {} a -> s {installedRejectedCount = a} :: PatchSummary)

-- | The number of patches that are part of the compliance standard but are
-- not installed. The count includes patches that failed to install.
patchSummary_missingCount :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Int)
patchSummary_missingCount = Lens.lens (\PatchSummary' {missingCount} -> missingCount) (\s@PatchSummary' {} a -> s {missingCount = a} :: PatchSummary)

-- | The type of patch operation performed. For Patch Manager, the values are
-- @SCAN@ and @INSTALL@.
patchSummary_operation :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operation = Lens.lens (\PatchSummary' {operation} -> operation) (\s@PatchSummary' {} a -> s {operation = a} :: PatchSummary)

-- | Indicates when the operation completed.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
patchSummary_operationEndTime :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operationEndTime = Lens.lens (\PatchSummary' {operationEndTime} -> operationEndTime) (\s@PatchSummary' {} a -> s {operationEndTime = a} :: PatchSummary)

-- | Indicates when the operation started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
patchSummary_operationStartTime :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_operationStartTime = Lens.lens (\PatchSummary' {operationStartTime} -> operationStartTime) (\s@PatchSummary' {} a -> s {operationStartTime = a} :: PatchSummary)

-- | The reboot option specified for the instance.
patchSummary_rebootOption :: Lens.Lens' PatchSummary (Prelude.Maybe Prelude.Text)
patchSummary_rebootOption = Lens.lens (\PatchSummary' {rebootOption} -> rebootOption) (\s@PatchSummary' {} a -> s {rebootOption = a} :: PatchSummary)

-- | The identifier of the compliance standard that was used to determine the
-- patch compliance status.
patchSummary_id :: Lens.Lens' PatchSummary Prelude.Text
patchSummary_id = Lens.lens (\PatchSummary' {id} -> id) (\s@PatchSummary' {} a -> s {id = a} :: PatchSummary)

instance Data.FromJSON PatchSummary where
  parseJSON =
    Data.withObject
      "PatchSummary"
      ( \x ->
          PatchSummary'
            Prelude.<$> (x Data..:? "FailedCount")
            Prelude.<*> (x Data..:? "InstalledCount")
            Prelude.<*> (x Data..:? "InstalledOtherCount")
            Prelude.<*> (x Data..:? "InstalledPendingReboot")
            Prelude.<*> (x Data..:? "InstalledRejectedCount")
            Prelude.<*> (x Data..:? "MissingCount")
            Prelude.<*> (x Data..:? "Operation")
            Prelude.<*> (x Data..:? "OperationEndTime")
            Prelude.<*> (x Data..:? "OperationStartTime")
            Prelude.<*> (x Data..:? "RebootOption")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable PatchSummary where
  hashWithSalt _salt PatchSummary' {..} =
    _salt `Prelude.hashWithSalt` failedCount
      `Prelude.hashWithSalt` installedCount
      `Prelude.hashWithSalt` installedOtherCount
      `Prelude.hashWithSalt` installedPendingReboot
      `Prelude.hashWithSalt` installedRejectedCount
      `Prelude.hashWithSalt` missingCount
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` operationEndTime
      `Prelude.hashWithSalt` operationStartTime
      `Prelude.hashWithSalt` rebootOption
      `Prelude.hashWithSalt` id

instance Prelude.NFData PatchSummary where
  rnf PatchSummary' {..} =
    Prelude.rnf failedCount
      `Prelude.seq` Prelude.rnf installedCount
      `Prelude.seq` Prelude.rnf installedOtherCount
      `Prelude.seq` Prelude.rnf installedPendingReboot
      `Prelude.seq` Prelude.rnf installedRejectedCount
      `Prelude.seq` Prelude.rnf missingCount
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf operationEndTime
      `Prelude.seq` Prelude.rnf operationStartTime
      `Prelude.seq` Prelude.rnf rebootOption
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON PatchSummary where
  toJSON PatchSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FailedCount" Data..=) Prelude.<$> failedCount,
            ("InstalledCount" Data..=)
              Prelude.<$> installedCount,
            ("InstalledOtherCount" Data..=)
              Prelude.<$> installedOtherCount,
            ("InstalledPendingReboot" Data..=)
              Prelude.<$> installedPendingReboot,
            ("InstalledRejectedCount" Data..=)
              Prelude.<$> installedRejectedCount,
            ("MissingCount" Data..=) Prelude.<$> missingCount,
            ("Operation" Data..=) Prelude.<$> operation,
            ("OperationEndTime" Data..=)
              Prelude.<$> operationEndTime,
            ("OperationStartTime" Data..=)
              Prelude.<$> operationStartTime,
            ("RebootOption" Data..=) Prelude.<$> rebootOption,
            Prelude.Just ("Id" Data..= id)
          ]
      )

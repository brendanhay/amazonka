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
-- Module      : Network.AWS.SSM.Types.PatchComplianceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.PatchComplianceDataState

-- | Information about the state of a patch on a particular instance as it
-- relates to the patch baseline used to patch the instance.
--
-- /See:/ 'newPatchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { -- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues
    -- that are resolved by the patch.
    cVEIds :: Core.Maybe Core.Text,
    -- | The title of the patch.
    title :: Core.Text,
    -- | The operating system-specific ID of the patch.
    kBId :: Core.Text,
    -- | The classification of the patch (for example, SecurityUpdates, Updates,
    -- CriticalUpdates).
    classification :: Core.Text,
    -- | The severity of the patch (for example, Critical, Important, Moderate).
    severity :: Core.Text,
    -- | The state of the patch on the instance, such as INSTALLED or FAILED.
    --
    -- For descriptions of each patch state, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance>
    -- in the /AWS Systems Manager User Guide/.
    state :: PatchComplianceDataState,
    -- | The date\/time the patch was installed on the instance. Note that not
    -- all operating systems provide this level of information.
    installedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PatchComplianceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cVEIds', 'patchComplianceData_cVEIds' - The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues
-- that are resolved by the patch.
--
-- 'title', 'patchComplianceData_title' - The title of the patch.
--
-- 'kBId', 'patchComplianceData_kBId' - The operating system-specific ID of the patch.
--
-- 'classification', 'patchComplianceData_classification' - The classification of the patch (for example, SecurityUpdates, Updates,
-- CriticalUpdates).
--
-- 'severity', 'patchComplianceData_severity' - The severity of the patch (for example, Critical, Important, Moderate).
--
-- 'state', 'patchComplianceData_state' - The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance>
-- in the /AWS Systems Manager User Guide/.
--
-- 'installedTime', 'patchComplianceData_installedTime' - The date\/time the patch was installed on the instance. Note that not
-- all operating systems provide this level of information.
newPatchComplianceData ::
  -- | 'title'
  Core.Text ->
  -- | 'kBId'
  Core.Text ->
  -- | 'classification'
  Core.Text ->
  -- | 'severity'
  Core.Text ->
  -- | 'state'
  PatchComplianceDataState ->
  -- | 'installedTime'
  Core.UTCTime ->
  PatchComplianceData
newPatchComplianceData
  pTitle_
  pKBId_
  pClassification_
  pSeverity_
  pState_
  pInstalledTime_ =
    PatchComplianceData'
      { cVEIds = Core.Nothing,
        title = pTitle_,
        kBId = pKBId_,
        classification = pClassification_,
        severity = pSeverity_,
        state = pState_,
        installedTime = Core._Time Lens.# pInstalledTime_
      }

-- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues
-- that are resolved by the patch.
patchComplianceData_cVEIds :: Lens.Lens' PatchComplianceData (Core.Maybe Core.Text)
patchComplianceData_cVEIds = Lens.lens (\PatchComplianceData' {cVEIds} -> cVEIds) (\s@PatchComplianceData' {} a -> s {cVEIds = a} :: PatchComplianceData)

-- | The title of the patch.
patchComplianceData_title :: Lens.Lens' PatchComplianceData Core.Text
patchComplianceData_title = Lens.lens (\PatchComplianceData' {title} -> title) (\s@PatchComplianceData' {} a -> s {title = a} :: PatchComplianceData)

-- | The operating system-specific ID of the patch.
patchComplianceData_kBId :: Lens.Lens' PatchComplianceData Core.Text
patchComplianceData_kBId = Lens.lens (\PatchComplianceData' {kBId} -> kBId) (\s@PatchComplianceData' {} a -> s {kBId = a} :: PatchComplianceData)

-- | The classification of the patch (for example, SecurityUpdates, Updates,
-- CriticalUpdates).
patchComplianceData_classification :: Lens.Lens' PatchComplianceData Core.Text
patchComplianceData_classification = Lens.lens (\PatchComplianceData' {classification} -> classification) (\s@PatchComplianceData' {} a -> s {classification = a} :: PatchComplianceData)

-- | The severity of the patch (for example, Critical, Important, Moderate).
patchComplianceData_severity :: Lens.Lens' PatchComplianceData Core.Text
patchComplianceData_severity = Lens.lens (\PatchComplianceData' {severity} -> severity) (\s@PatchComplianceData' {} a -> s {severity = a} :: PatchComplianceData)

-- | The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance>
-- in the /AWS Systems Manager User Guide/.
patchComplianceData_state :: Lens.Lens' PatchComplianceData PatchComplianceDataState
patchComplianceData_state = Lens.lens (\PatchComplianceData' {state} -> state) (\s@PatchComplianceData' {} a -> s {state = a} :: PatchComplianceData)

-- | The date\/time the patch was installed on the instance. Note that not
-- all operating systems provide this level of information.
patchComplianceData_installedTime :: Lens.Lens' PatchComplianceData Core.UTCTime
patchComplianceData_installedTime = Lens.lens (\PatchComplianceData' {installedTime} -> installedTime) (\s@PatchComplianceData' {} a -> s {installedTime = a} :: PatchComplianceData) Core.. Core._Time

instance Core.FromJSON PatchComplianceData where
  parseJSON =
    Core.withObject
      "PatchComplianceData"
      ( \x ->
          PatchComplianceData'
            Core.<$> (x Core..:? "CVEIds")
            Core.<*> (x Core..: "Title")
            Core.<*> (x Core..: "KBId")
            Core.<*> (x Core..: "Classification")
            Core.<*> (x Core..: "Severity")
            Core.<*> (x Core..: "State")
            Core.<*> (x Core..: "InstalledTime")
      )

instance Core.Hashable PatchComplianceData

instance Core.NFData PatchComplianceData

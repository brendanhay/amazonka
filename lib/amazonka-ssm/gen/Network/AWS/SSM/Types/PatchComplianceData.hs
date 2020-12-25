{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceData
  ( PatchComplianceData (..),

    -- * Smart constructor
    mkPatchComplianceData,

    -- * Lenses
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,
    pcdCVEIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchCVEIds as Types
import qualified Network.AWS.SSM.Types.PatchClassification as Types
import qualified Network.AWS.SSM.Types.PatchComplianceDataState as Types
import qualified Network.AWS.SSM.Types.PatchKbNumber as Types
import qualified Network.AWS.SSM.Types.PatchSeverity as Types
import qualified Network.AWS.SSM.Types.Title as Types

-- | Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.
--
-- /See:/ 'mkPatchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { -- | The title of the patch.
    title :: Types.Title,
    -- | The operating system-specific ID of the patch.
    kBId :: Types.PatchKbNumber,
    -- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
    classification :: Types.PatchClassification,
    -- | The severity of the patch (for example, Critical, Important, Moderate).
    severity :: Types.PatchSeverity,
    -- | The state of the patch on the instance, such as INSTALLED or FAILED.
    --
    -- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
    state :: Types.PatchComplianceDataState,
    -- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
    installedTime :: Core.NominalDiffTime,
    -- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
    cVEIds :: Core.Maybe Types.PatchCVEIds
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PatchComplianceData' value with any optional fields omitted.
mkPatchComplianceData ::
  -- | 'title'
  Types.Title ->
  -- | 'kBId'
  Types.PatchKbNumber ->
  -- | 'classification'
  Types.PatchClassification ->
  -- | 'severity'
  Types.PatchSeverity ->
  -- | 'state'
  Types.PatchComplianceDataState ->
  -- | 'installedTime'
  Core.NominalDiffTime ->
  PatchComplianceData
mkPatchComplianceData
  title
  kBId
  classification
  severity
  state
  installedTime =
    PatchComplianceData'
      { title,
        kBId,
        classification,
        severity,
        state,
        installedTime,
        cVEIds = Core.Nothing
      }

-- | The title of the patch.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdTitle :: Lens.Lens' PatchComplianceData Types.Title
pcdTitle = Lens.field @"title"
{-# DEPRECATED pcdTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The operating system-specific ID of the patch.
--
-- /Note:/ Consider using 'kBId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdKBId :: Lens.Lens' PatchComplianceData Types.PatchKbNumber
pcdKBId = Lens.field @"kBId"
{-# DEPRECATED pcdKBId "Use generic-lens or generic-optics with 'kBId' instead." #-}

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdClassification :: Lens.Lens' PatchComplianceData Types.PatchClassification
pcdClassification = Lens.field @"classification"
{-# DEPRECATED pcdClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The severity of the patch (for example, Critical, Important, Moderate).
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdSeverity :: Lens.Lens' PatchComplianceData Types.PatchSeverity
pcdSeverity = Lens.field @"severity"
{-# DEPRECATED pcdSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdState :: Lens.Lens' PatchComplianceData Types.PatchComplianceDataState
pcdState = Lens.field @"state"
{-# DEPRECATED pcdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
--
-- /Note:/ Consider using 'installedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdInstalledTime :: Lens.Lens' PatchComplianceData Core.NominalDiffTime
pcdInstalledTime = Lens.field @"installedTime"
{-# DEPRECATED pcdInstalledTime "Use generic-lens or generic-optics with 'installedTime' instead." #-}

-- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
--
-- /Note:/ Consider using 'cVEIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCVEIds :: Lens.Lens' PatchComplianceData (Core.Maybe Types.PatchCVEIds)
pcdCVEIds = Lens.field @"cVEIds"
{-# DEPRECATED pcdCVEIds "Use generic-lens or generic-optics with 'cVEIds' instead." #-}

instance Core.FromJSON PatchComplianceData where
  parseJSON =
    Core.withObject "PatchComplianceData" Core.$
      \x ->
        PatchComplianceData'
          Core.<$> (x Core..: "Title")
          Core.<*> (x Core..: "KBId")
          Core.<*> (x Core..: "Classification")
          Core.<*> (x Core..: "Severity")
          Core.<*> (x Core..: "State")
          Core.<*> (x Core..: "InstalledTime")
          Core.<*> (x Core..:? "CVEIds")

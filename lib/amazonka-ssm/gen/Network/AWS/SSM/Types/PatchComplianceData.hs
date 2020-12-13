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
    pcdState,
    pcdSeverity,
    pcdCVEIds,
    pcdClassification,
    pcdKBId,
    pcdInstalledTime,
    pcdTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchComplianceDataState

-- | Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.
--
-- /See:/ 'mkPatchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { -- | The state of the patch on the instance, such as INSTALLED or FAILED.
    --
    -- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
    state :: PatchComplianceDataState,
    -- | The severity of the patch (for example, Critical, Important, Moderate).
    severity :: Lude.Text,
    -- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
    cVEIds :: Lude.Maybe Lude.Text,
    -- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
    classification :: Lude.Text,
    -- | The operating system-specific ID of the patch.
    kBId :: Lude.Text,
    -- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
    installedTime :: Lude.Timestamp,
    -- | The title of the patch.
    title :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchComplianceData' with the minimum fields required to make a request.
--
-- * 'state' - The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
-- * 'severity' - The severity of the patch (for example, Critical, Important, Moderate).
-- * 'cVEIds' - The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
-- * 'classification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
-- * 'kBId' - The operating system-specific ID of the patch.
-- * 'installedTime' - The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
-- * 'title' - The title of the patch.
mkPatchComplianceData ::
  -- | 'state'
  PatchComplianceDataState ->
  -- | 'severity'
  Lude.Text ->
  -- | 'classification'
  Lude.Text ->
  -- | 'kBId'
  Lude.Text ->
  -- | 'installedTime'
  Lude.Timestamp ->
  -- | 'title'
  Lude.Text ->
  PatchComplianceData
mkPatchComplianceData
  pState_
  pSeverity_
  pClassification_
  pKBId_
  pInstalledTime_
  pTitle_ =
    PatchComplianceData'
      { state = pState_,
        severity = pSeverity_,
        cVEIds = Lude.Nothing,
        classification = pClassification_,
        kBId = pKBId_,
        installedTime = pInstalledTime_,
        title = pTitle_
      }

-- | The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdState :: Lens.Lens' PatchComplianceData PatchComplianceDataState
pcdState = Lens.lens (state :: PatchComplianceData -> PatchComplianceDataState) (\s a -> s {state = a} :: PatchComplianceData)
{-# DEPRECATED pcdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The severity of the patch (for example, Critical, Important, Moderate).
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdSeverity :: Lens.Lens' PatchComplianceData Lude.Text
pcdSeverity = Lens.lens (severity :: PatchComplianceData -> Lude.Text) (\s a -> s {severity = a} :: PatchComplianceData)
{-# DEPRECATED pcdSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
--
-- /Note:/ Consider using 'cVEIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCVEIds :: Lens.Lens' PatchComplianceData (Lude.Maybe Lude.Text)
pcdCVEIds = Lens.lens (cVEIds :: PatchComplianceData -> Lude.Maybe Lude.Text) (\s a -> s {cVEIds = a} :: PatchComplianceData)
{-# DEPRECATED pcdCVEIds "Use generic-lens or generic-optics with 'cVEIds' instead." #-}

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdClassification :: Lens.Lens' PatchComplianceData Lude.Text
pcdClassification = Lens.lens (classification :: PatchComplianceData -> Lude.Text) (\s a -> s {classification = a} :: PatchComplianceData)
{-# DEPRECATED pcdClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The operating system-specific ID of the patch.
--
-- /Note:/ Consider using 'kBId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdKBId :: Lens.Lens' PatchComplianceData Lude.Text
pcdKBId = Lens.lens (kBId :: PatchComplianceData -> Lude.Text) (\s a -> s {kBId = a} :: PatchComplianceData)
{-# DEPRECATED pcdKBId "Use generic-lens or generic-optics with 'kBId' instead." #-}

-- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
--
-- /Note:/ Consider using 'installedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdInstalledTime :: Lens.Lens' PatchComplianceData Lude.Timestamp
pcdInstalledTime = Lens.lens (installedTime :: PatchComplianceData -> Lude.Timestamp) (\s a -> s {installedTime = a} :: PatchComplianceData)
{-# DEPRECATED pcdInstalledTime "Use generic-lens or generic-optics with 'installedTime' instead." #-}

-- | The title of the patch.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdTitle :: Lens.Lens' PatchComplianceData Lude.Text
pcdTitle = Lens.lens (title :: PatchComplianceData -> Lude.Text) (\s a -> s {title = a} :: PatchComplianceData)
{-# DEPRECATED pcdTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON PatchComplianceData where
  parseJSON =
    Lude.withObject
      "PatchComplianceData"
      ( \x ->
          PatchComplianceData'
            Lude.<$> (x Lude..: "State")
            Lude.<*> (x Lude..: "Severity")
            Lude.<*> (x Lude..:? "CVEIds")
            Lude.<*> (x Lude..: "Classification")
            Lude.<*> (x Lude..: "KBId")
            Lude.<*> (x Lude..: "InstalledTime")
            Lude.<*> (x Lude..: "Title")
      )

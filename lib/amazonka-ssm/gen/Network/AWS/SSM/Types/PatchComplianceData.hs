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
    pcdCVEIds,
    pcdTitle,
    pcdKBId,
    pcdClassification,
    pcdSeverity,
    pcdState,
    pcdInstalledTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchComplianceDataState

-- | Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.
--
-- /See:/ 'mkPatchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { cVEIds ::
      Lude.Maybe Lude.Text,
    title :: Lude.Text,
    kBId :: Lude.Text,
    classification :: Lude.Text,
    severity :: Lude.Text,
    state :: PatchComplianceDataState,
    installedTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchComplianceData' with the minimum fields required to make a request.
--
-- * 'cVEIds' - The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
-- * 'classification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
-- * 'installedTime' - The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
-- * 'kBId' - The operating system-specific ID of the patch.
-- * 'severity' - The severity of the patch (for example, Critical, Important, Moderate).
-- * 'state' - The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
-- * 'title' - The title of the patch.
mkPatchComplianceData ::
  -- | 'title'
  Lude.Text ->
  -- | 'kBId'
  Lude.Text ->
  -- | 'classification'
  Lude.Text ->
  -- | 'severity'
  Lude.Text ->
  -- | 'state'
  PatchComplianceDataState ->
  -- | 'installedTime'
  Lude.Timestamp ->
  PatchComplianceData
mkPatchComplianceData
  pTitle_
  pKBId_
  pClassification_
  pSeverity_
  pState_
  pInstalledTime_ =
    PatchComplianceData'
      { cVEIds = Lude.Nothing,
        title = pTitle_,
        kBId = pKBId_,
        classification = pClassification_,
        severity = pSeverity_,
        state = pState_,
        installedTime = pInstalledTime_
      }

-- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
--
-- /Note:/ Consider using 'cVEIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCVEIds :: Lens.Lens' PatchComplianceData (Lude.Maybe Lude.Text)
pcdCVEIds = Lens.lens (cVEIds :: PatchComplianceData -> Lude.Maybe Lude.Text) (\s a -> s {cVEIds = a} :: PatchComplianceData)
{-# DEPRECATED pcdCVEIds "Use generic-lens or generic-optics with 'cVEIds' instead." #-}

-- | The title of the patch.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdTitle :: Lens.Lens' PatchComplianceData Lude.Text
pcdTitle = Lens.lens (title :: PatchComplianceData -> Lude.Text) (\s a -> s {title = a} :: PatchComplianceData)
{-# DEPRECATED pcdTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The operating system-specific ID of the patch.
--
-- /Note:/ Consider using 'kBId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdKBId :: Lens.Lens' PatchComplianceData Lude.Text
pcdKBId = Lens.lens (kBId :: PatchComplianceData -> Lude.Text) (\s a -> s {kBId = a} :: PatchComplianceData)
{-# DEPRECATED pcdKBId "Use generic-lens or generic-optics with 'kBId' instead." #-}

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdClassification :: Lens.Lens' PatchComplianceData Lude.Text
pcdClassification = Lens.lens (classification :: PatchComplianceData -> Lude.Text) (\s a -> s {classification = a} :: PatchComplianceData)
{-# DEPRECATED pcdClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The severity of the patch (for example, Critical, Important, Moderate).
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdSeverity :: Lens.Lens' PatchComplianceData Lude.Text
pcdSeverity = Lens.lens (severity :: PatchComplianceData -> Lude.Text) (\s a -> s {severity = a} :: PatchComplianceData)
{-# DEPRECATED pcdSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The state of the patch on the instance, such as INSTALLED or FAILED.
--
-- For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdState :: Lens.Lens' PatchComplianceData PatchComplianceDataState
pcdState = Lens.lens (state :: PatchComplianceData -> PatchComplianceDataState) (\s a -> s {state = a} :: PatchComplianceData)
{-# DEPRECATED pcdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
--
-- /Note:/ Consider using 'installedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdInstalledTime :: Lens.Lens' PatchComplianceData Lude.Timestamp
pcdInstalledTime = Lens.lens (installedTime :: PatchComplianceData -> Lude.Timestamp) (\s a -> s {installedTime = a} :: PatchComplianceData)
{-# DEPRECATED pcdInstalledTime "Use generic-lens or generic-optics with 'installedTime' instead." #-}

instance Lude.FromJSON PatchComplianceData where
  parseJSON =
    Lude.withObject
      "PatchComplianceData"
      ( \x ->
          PatchComplianceData'
            Lude.<$> (x Lude..:? "CVEIds")
            Lude.<*> (x Lude..: "Title")
            Lude.<*> (x Lude..: "KBId")
            Lude.<*> (x Lude..: "Classification")
            Lude.<*> (x Lude..: "Severity")
            Lude.<*> (x Lude..: "State")
            Lude.<*> (x Lude..: "InstalledTime")
      )

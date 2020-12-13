{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceDataState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceDataState
  ( PatchComplianceDataState
      ( PatchComplianceDataState',
        PCDSInstalled,
        PCDSInstalledOther,
        PCDSInstalledPendingReboot,
        PCDSInstalledRejected,
        PCDSMissing,
        PCDSNotApplicable,
        PCDSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PatchComplianceDataState = PatchComplianceDataState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PCDSInstalled :: PatchComplianceDataState
pattern PCDSInstalled = PatchComplianceDataState' "INSTALLED"

pattern PCDSInstalledOther :: PatchComplianceDataState
pattern PCDSInstalledOther = PatchComplianceDataState' "INSTALLED_OTHER"

pattern PCDSInstalledPendingReboot :: PatchComplianceDataState
pattern PCDSInstalledPendingReboot = PatchComplianceDataState' "INSTALLED_PENDING_REBOOT"

pattern PCDSInstalledRejected :: PatchComplianceDataState
pattern PCDSInstalledRejected = PatchComplianceDataState' "INSTALLED_REJECTED"

pattern PCDSMissing :: PatchComplianceDataState
pattern PCDSMissing = PatchComplianceDataState' "MISSING"

pattern PCDSNotApplicable :: PatchComplianceDataState
pattern PCDSNotApplicable = PatchComplianceDataState' "NOT_APPLICABLE"

pattern PCDSFailed :: PatchComplianceDataState
pattern PCDSFailed = PatchComplianceDataState' "FAILED"

{-# COMPLETE
  PCDSInstalled,
  PCDSInstalledOther,
  PCDSInstalledPendingReboot,
  PCDSInstalledRejected,
  PCDSMissing,
  PCDSNotApplicable,
  PCDSFailed,
  PatchComplianceDataState'
  #-}

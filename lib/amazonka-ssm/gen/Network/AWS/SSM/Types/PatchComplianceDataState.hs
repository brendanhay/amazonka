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
        Failed,
        Installed,
        InstalledOther,
        InstalledPendingReboot,
        InstalledRejected,
        Missing,
        NotApplicable
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

pattern Failed :: PatchComplianceDataState
pattern Failed = PatchComplianceDataState' "FAILED"

pattern Installed :: PatchComplianceDataState
pattern Installed = PatchComplianceDataState' "INSTALLED"

pattern InstalledOther :: PatchComplianceDataState
pattern InstalledOther = PatchComplianceDataState' "INSTALLED_OTHER"

pattern InstalledPendingReboot :: PatchComplianceDataState
pattern InstalledPendingReboot = PatchComplianceDataState' "INSTALLED_PENDING_REBOOT"

pattern InstalledRejected :: PatchComplianceDataState
pattern InstalledRejected = PatchComplianceDataState' "INSTALLED_REJECTED"

pattern Missing :: PatchComplianceDataState
pattern Missing = PatchComplianceDataState' "MISSING"

pattern NotApplicable :: PatchComplianceDataState
pattern NotApplicable = PatchComplianceDataState' "NOT_APPLICABLE"

{-# COMPLETE
  Failed,
  Installed,
  InstalledOther,
  InstalledPendingReboot,
  InstalledRejected,
  Missing,
  NotApplicable,
  PatchComplianceDataState'
  #-}

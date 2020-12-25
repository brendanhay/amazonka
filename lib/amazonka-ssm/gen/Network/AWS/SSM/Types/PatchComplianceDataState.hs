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
        PatchComplianceDataStateInstalled,
        PatchComplianceDataStateInstalledOther,
        PatchComplianceDataStateInstalledPendingReboot,
        PatchComplianceDataStateInstalledRejected,
        PatchComplianceDataStateMissing,
        PatchComplianceDataStateNotApplicable,
        PatchComplianceDataStateFailed,
        fromPatchComplianceDataState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PatchComplianceDataState = PatchComplianceDataState'
  { fromPatchComplianceDataState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PatchComplianceDataStateInstalled :: PatchComplianceDataState
pattern PatchComplianceDataStateInstalled = PatchComplianceDataState' "INSTALLED"

pattern PatchComplianceDataStateInstalledOther :: PatchComplianceDataState
pattern PatchComplianceDataStateInstalledOther = PatchComplianceDataState' "INSTALLED_OTHER"

pattern PatchComplianceDataStateInstalledPendingReboot :: PatchComplianceDataState
pattern PatchComplianceDataStateInstalledPendingReboot = PatchComplianceDataState' "INSTALLED_PENDING_REBOOT"

pattern PatchComplianceDataStateInstalledRejected :: PatchComplianceDataState
pattern PatchComplianceDataStateInstalledRejected = PatchComplianceDataState' "INSTALLED_REJECTED"

pattern PatchComplianceDataStateMissing :: PatchComplianceDataState
pattern PatchComplianceDataStateMissing = PatchComplianceDataState' "MISSING"

pattern PatchComplianceDataStateNotApplicable :: PatchComplianceDataState
pattern PatchComplianceDataStateNotApplicable = PatchComplianceDataState' "NOT_APPLICABLE"

pattern PatchComplianceDataStateFailed :: PatchComplianceDataState
pattern PatchComplianceDataStateFailed = PatchComplianceDataState' "FAILED"

{-# COMPLETE
  PatchComplianceDataStateInstalled,
  PatchComplianceDataStateInstalledOther,
  PatchComplianceDataStateInstalledPendingReboot,
  PatchComplianceDataStateInstalledRejected,
  PatchComplianceDataStateMissing,
  PatchComplianceDataStateNotApplicable,
  PatchComplianceDataStateFailed,
  PatchComplianceDataState'
  #-}

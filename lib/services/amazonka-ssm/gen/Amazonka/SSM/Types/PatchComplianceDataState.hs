{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types.PatchComplianceDataState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchComplianceDataState
  ( PatchComplianceDataState
      ( ..,
        PatchComplianceDataState_FAILED,
        PatchComplianceDataState_INSTALLED,
        PatchComplianceDataState_INSTALLED_OTHER,
        PatchComplianceDataState_INSTALLED_PENDING_REBOOT,
        PatchComplianceDataState_INSTALLED_REJECTED,
        PatchComplianceDataState_MISSING,
        PatchComplianceDataState_NOT_APPLICABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PatchComplianceDataState = PatchComplianceDataState'
  { fromPatchComplianceDataState ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern PatchComplianceDataState_FAILED :: PatchComplianceDataState
pattern PatchComplianceDataState_FAILED = PatchComplianceDataState' "FAILED"

pattern PatchComplianceDataState_INSTALLED :: PatchComplianceDataState
pattern PatchComplianceDataState_INSTALLED = PatchComplianceDataState' "INSTALLED"

pattern PatchComplianceDataState_INSTALLED_OTHER :: PatchComplianceDataState
pattern PatchComplianceDataState_INSTALLED_OTHER = PatchComplianceDataState' "INSTALLED_OTHER"

pattern PatchComplianceDataState_INSTALLED_PENDING_REBOOT :: PatchComplianceDataState
pattern PatchComplianceDataState_INSTALLED_PENDING_REBOOT = PatchComplianceDataState' "INSTALLED_PENDING_REBOOT"

pattern PatchComplianceDataState_INSTALLED_REJECTED :: PatchComplianceDataState
pattern PatchComplianceDataState_INSTALLED_REJECTED = PatchComplianceDataState' "INSTALLED_REJECTED"

pattern PatchComplianceDataState_MISSING :: PatchComplianceDataState
pattern PatchComplianceDataState_MISSING = PatchComplianceDataState' "MISSING"

pattern PatchComplianceDataState_NOT_APPLICABLE :: PatchComplianceDataState
pattern PatchComplianceDataState_NOT_APPLICABLE = PatchComplianceDataState' "NOT_APPLICABLE"

{-# COMPLETE
  PatchComplianceDataState_FAILED,
  PatchComplianceDataState_INSTALLED,
  PatchComplianceDataState_INSTALLED_OTHER,
  PatchComplianceDataState_INSTALLED_PENDING_REBOOT,
  PatchComplianceDataState_INSTALLED_REJECTED,
  PatchComplianceDataState_MISSING,
  PatchComplianceDataState_NOT_APPLICABLE,
  PatchComplianceDataState'
  #-}

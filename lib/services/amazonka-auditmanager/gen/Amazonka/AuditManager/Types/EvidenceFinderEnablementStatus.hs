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
-- Module      : Amazonka.AuditManager.Types.EvidenceFinderEnablementStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.EvidenceFinderEnablementStatus
  ( EvidenceFinderEnablementStatus
      ( ..,
        EvidenceFinderEnablementStatus_DISABLED,
        EvidenceFinderEnablementStatus_DISABLE_IN_PROGRESS,
        EvidenceFinderEnablementStatus_ENABLED,
        EvidenceFinderEnablementStatus_ENABLE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EvidenceFinderEnablementStatus = EvidenceFinderEnablementStatus'
  { fromEvidenceFinderEnablementStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern EvidenceFinderEnablementStatus_DISABLED :: EvidenceFinderEnablementStatus
pattern EvidenceFinderEnablementStatus_DISABLED = EvidenceFinderEnablementStatus' "DISABLED"

pattern EvidenceFinderEnablementStatus_DISABLE_IN_PROGRESS :: EvidenceFinderEnablementStatus
pattern EvidenceFinderEnablementStatus_DISABLE_IN_PROGRESS = EvidenceFinderEnablementStatus' "DISABLE_IN_PROGRESS"

pattern EvidenceFinderEnablementStatus_ENABLED :: EvidenceFinderEnablementStatus
pattern EvidenceFinderEnablementStatus_ENABLED = EvidenceFinderEnablementStatus' "ENABLED"

pattern EvidenceFinderEnablementStatus_ENABLE_IN_PROGRESS :: EvidenceFinderEnablementStatus
pattern EvidenceFinderEnablementStatus_ENABLE_IN_PROGRESS = EvidenceFinderEnablementStatus' "ENABLE_IN_PROGRESS"

{-# COMPLETE
  EvidenceFinderEnablementStatus_DISABLED,
  EvidenceFinderEnablementStatus_DISABLE_IN_PROGRESS,
  EvidenceFinderEnablementStatus_ENABLED,
  EvidenceFinderEnablementStatus_ENABLE_IN_PROGRESS,
  EvidenceFinderEnablementStatus'
  #-}

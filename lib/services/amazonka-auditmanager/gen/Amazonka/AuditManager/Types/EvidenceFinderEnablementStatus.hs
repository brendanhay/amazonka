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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EvidenceFinderEnablementStatus = EvidenceFinderEnablementStatus'
  { fromEvidenceFinderEnablementStatus ::
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

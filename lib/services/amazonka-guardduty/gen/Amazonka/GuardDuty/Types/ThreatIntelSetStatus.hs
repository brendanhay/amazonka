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
-- Module      : Amazonka.GuardDuty.Types.ThreatIntelSetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ThreatIntelSetStatus
  ( ThreatIntelSetStatus
      ( ..,
        ThreatIntelSetStatus_ACTIVATING,
        ThreatIntelSetStatus_ACTIVE,
        ThreatIntelSetStatus_DEACTIVATING,
        ThreatIntelSetStatus_DELETED,
        ThreatIntelSetStatus_DELETE_PENDING,
        ThreatIntelSetStatus_ERROR,
        ThreatIntelSetStatus_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ThreatIntelSetStatus = ThreatIntelSetStatus'
  { fromThreatIntelSetStatus ::
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

pattern ThreatIntelSetStatus_ACTIVATING :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_ACTIVATING = ThreatIntelSetStatus' "ACTIVATING"

pattern ThreatIntelSetStatus_ACTIVE :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_ACTIVE = ThreatIntelSetStatus' "ACTIVE"

pattern ThreatIntelSetStatus_DEACTIVATING :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_DEACTIVATING = ThreatIntelSetStatus' "DEACTIVATING"

pattern ThreatIntelSetStatus_DELETED :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_DELETED = ThreatIntelSetStatus' "DELETED"

pattern ThreatIntelSetStatus_DELETE_PENDING :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_DELETE_PENDING = ThreatIntelSetStatus' "DELETE_PENDING"

pattern ThreatIntelSetStatus_ERROR :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_ERROR = ThreatIntelSetStatus' "ERROR"

pattern ThreatIntelSetStatus_INACTIVE :: ThreatIntelSetStatus
pattern ThreatIntelSetStatus_INACTIVE = ThreatIntelSetStatus' "INACTIVE"

{-# COMPLETE
  ThreatIntelSetStatus_ACTIVATING,
  ThreatIntelSetStatus_ACTIVE,
  ThreatIntelSetStatus_DEACTIVATING,
  ThreatIntelSetStatus_DELETED,
  ThreatIntelSetStatus_DELETE_PENDING,
  ThreatIntelSetStatus_ERROR,
  ThreatIntelSetStatus_INACTIVE,
  ThreatIntelSetStatus'
  #-}

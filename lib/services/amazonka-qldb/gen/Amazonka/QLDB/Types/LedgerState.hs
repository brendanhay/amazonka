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
-- Module      : Amazonka.QLDB.Types.LedgerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.LedgerState
  ( LedgerState
      ( ..,
        LedgerState_ACTIVE,
        LedgerState_CREATING,
        LedgerState_DELETED,
        LedgerState_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LedgerState = LedgerState'
  { fromLedgerState ::
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

pattern LedgerState_ACTIVE :: LedgerState
pattern LedgerState_ACTIVE = LedgerState' "ACTIVE"

pattern LedgerState_CREATING :: LedgerState
pattern LedgerState_CREATING = LedgerState' "CREATING"

pattern LedgerState_DELETED :: LedgerState
pattern LedgerState_DELETED = LedgerState' "DELETED"

pattern LedgerState_DELETING :: LedgerState
pattern LedgerState_DELETING = LedgerState' "DELETING"

{-# COMPLETE
  LedgerState_ACTIVE,
  LedgerState_CREATING,
  LedgerState_DELETED,
  LedgerState_DELETING,
  LedgerState'
  #-}

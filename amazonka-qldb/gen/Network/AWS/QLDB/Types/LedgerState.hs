{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types.LedgerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.LedgerState
  ( LedgerState
      ( ..,
        LedgerState_ACTIVE,
        LedgerState_CREATING,
        LedgerState_DELETED,
        LedgerState_DELETING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LedgerState = LedgerState'
  { fromLedgerState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LexStatus
  ( LexStatus
      ( LexStatus',
        LSBuilding,
        LSFailed,
        LSNotBuilt,
        LSReady,
        LSReadyBasicTesting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LexStatus = LexStatus' Lude.Text
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

pattern LSBuilding :: LexStatus
pattern LSBuilding = LexStatus' "BUILDING"

pattern LSFailed :: LexStatus
pattern LSFailed = LexStatus' "FAILED"

pattern LSNotBuilt :: LexStatus
pattern LSNotBuilt = LexStatus' "NOT_BUILT"

pattern LSReady :: LexStatus
pattern LSReady = LexStatus' "READY"

pattern LSReadyBasicTesting :: LexStatus
pattern LSReadyBasicTesting = LexStatus' "READY_BASIC_TESTING"

{-# COMPLETE
  LSBuilding,
  LSFailed,
  LSNotBuilt,
  LSReady,
  LSReadyBasicTesting,
  LexStatus'
  #-}

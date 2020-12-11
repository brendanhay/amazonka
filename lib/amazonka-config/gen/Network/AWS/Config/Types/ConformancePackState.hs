-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackState
  ( ConformancePackState
      ( ConformancePackState',
        CPSCreateComplete,
        CPSCreateFailed,
        CPSCreateInProgress,
        CPSDeleteFailed,
        CPSDeleteInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConformancePackState = ConformancePackState' Lude.Text
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

pattern CPSCreateComplete :: ConformancePackState
pattern CPSCreateComplete = ConformancePackState' "CREATE_COMPLETE"

pattern CPSCreateFailed :: ConformancePackState
pattern CPSCreateFailed = ConformancePackState' "CREATE_FAILED"

pattern CPSCreateInProgress :: ConformancePackState
pattern CPSCreateInProgress = ConformancePackState' "CREATE_IN_PROGRESS"

pattern CPSDeleteFailed :: ConformancePackState
pattern CPSDeleteFailed = ConformancePackState' "DELETE_FAILED"

pattern CPSDeleteInProgress :: ConformancePackState
pattern CPSDeleteInProgress = ConformancePackState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  CPSCreateComplete,
  CPSCreateFailed,
  CPSCreateInProgress,
  CPSDeleteFailed,
  CPSDeleteInProgress,
  ConformancePackState'
  #-}

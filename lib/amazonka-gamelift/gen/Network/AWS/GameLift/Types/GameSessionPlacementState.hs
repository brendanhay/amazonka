-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacementState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacementState
  ( GameSessionPlacementState
      ( GameSessionPlacementState',
        GSPSCancelled,
        GSPSFailed,
        GSPSFulfilled,
        GSPSPending,
        GSPSTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GameSessionPlacementState = GameSessionPlacementState' Lude.Text
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

pattern GSPSCancelled :: GameSessionPlacementState
pattern GSPSCancelled = GameSessionPlacementState' "CANCELLED"

pattern GSPSFailed :: GameSessionPlacementState
pattern GSPSFailed = GameSessionPlacementState' "FAILED"

pattern GSPSFulfilled :: GameSessionPlacementState
pattern GSPSFulfilled = GameSessionPlacementState' "FULFILLED"

pattern GSPSPending :: GameSessionPlacementState
pattern GSPSPending = GameSessionPlacementState' "PENDING"

pattern GSPSTimedOut :: GameSessionPlacementState
pattern GSPSTimedOut = GameSessionPlacementState' "TIMED_OUT"

{-# COMPLETE
  GSPSCancelled,
  GSPSFailed,
  GSPSFulfilled,
  GSPSPending,
  GSPSTimedOut,
  GameSessionPlacementState'
  #-}

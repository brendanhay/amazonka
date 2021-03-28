{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacementState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionPlacementState
  ( GameSessionPlacementState
    ( GameSessionPlacementState'
    , GameSessionPlacementStatePending
    , GameSessionPlacementStateFulfilled
    , GameSessionPlacementStateCancelled
    , GameSessionPlacementStateTimedOut
    , GameSessionPlacementStateFailed
    , fromGameSessionPlacementState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype GameSessionPlacementState = GameSessionPlacementState'{fromGameSessionPlacementState
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern GameSessionPlacementStatePending :: GameSessionPlacementState
pattern GameSessionPlacementStatePending = GameSessionPlacementState' "PENDING"

pattern GameSessionPlacementStateFulfilled :: GameSessionPlacementState
pattern GameSessionPlacementStateFulfilled = GameSessionPlacementState' "FULFILLED"

pattern GameSessionPlacementStateCancelled :: GameSessionPlacementState
pattern GameSessionPlacementStateCancelled = GameSessionPlacementState' "CANCELLED"

pattern GameSessionPlacementStateTimedOut :: GameSessionPlacementState
pattern GameSessionPlacementStateTimedOut = GameSessionPlacementState' "TIMED_OUT"

pattern GameSessionPlacementStateFailed :: GameSessionPlacementState
pattern GameSessionPlacementStateFailed = GameSessionPlacementState' "FAILED"

{-# COMPLETE 
  GameSessionPlacementStatePending,

  GameSessionPlacementStateFulfilled,

  GameSessionPlacementStateCancelled,

  GameSessionPlacementStateTimedOut,

  GameSessionPlacementStateFailed,
  GameSessionPlacementState'
  #-}

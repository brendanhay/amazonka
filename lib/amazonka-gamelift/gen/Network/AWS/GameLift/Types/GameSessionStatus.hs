{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionStatus
  ( GameSessionStatus
    ( GameSessionStatus'
    , GameSessionStatusActive
    , GameSessionStatusActivating
    , GameSessionStatusTerminated
    , GameSessionStatusTerminating
    , GameSessionStatusError
    , fromGameSessionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype GameSessionStatus = GameSessionStatus'{fromGameSessionStatus
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern GameSessionStatusActive :: GameSessionStatus
pattern GameSessionStatusActive = GameSessionStatus' "ACTIVE"

pattern GameSessionStatusActivating :: GameSessionStatus
pattern GameSessionStatusActivating = GameSessionStatus' "ACTIVATING"

pattern GameSessionStatusTerminated :: GameSessionStatus
pattern GameSessionStatusTerminated = GameSessionStatus' "TERMINATED"

pattern GameSessionStatusTerminating :: GameSessionStatus
pattern GameSessionStatusTerminating = GameSessionStatus' "TERMINATING"

pattern GameSessionStatusError :: GameSessionStatus
pattern GameSessionStatusError = GameSessionStatus' "ERROR"

{-# COMPLETE 
  GameSessionStatusActive,

  GameSessionStatusActivating,

  GameSessionStatusTerminated,

  GameSessionStatusTerminating,

  GameSessionStatusError,
  GameSessionStatus'
  #-}

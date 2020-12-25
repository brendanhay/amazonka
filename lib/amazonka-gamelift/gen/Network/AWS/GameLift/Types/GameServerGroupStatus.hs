{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupStatus
  ( GameServerGroupStatus
      ( GameServerGroupStatus',
        GameServerGroupStatusNew,
        GameServerGroupStatusActivating,
        GameServerGroupStatusActive,
        GameServerGroupStatusDeleteScheduled,
        GameServerGroupStatusDeleting,
        GameServerGroupStatusDeleted,
        GameServerGroupStatusError,
        fromGameServerGroupStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype GameServerGroupStatus = GameServerGroupStatus'
  { fromGameServerGroupStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern GameServerGroupStatusNew :: GameServerGroupStatus
pattern GameServerGroupStatusNew = GameServerGroupStatus' "NEW"

pattern GameServerGroupStatusActivating :: GameServerGroupStatus
pattern GameServerGroupStatusActivating = GameServerGroupStatus' "ACTIVATING"

pattern GameServerGroupStatusActive :: GameServerGroupStatus
pattern GameServerGroupStatusActive = GameServerGroupStatus' "ACTIVE"

pattern GameServerGroupStatusDeleteScheduled :: GameServerGroupStatus
pattern GameServerGroupStatusDeleteScheduled = GameServerGroupStatus' "DELETE_SCHEDULED"

pattern GameServerGroupStatusDeleting :: GameServerGroupStatus
pattern GameServerGroupStatusDeleting = GameServerGroupStatus' "DELETING"

pattern GameServerGroupStatusDeleted :: GameServerGroupStatus
pattern GameServerGroupStatusDeleted = GameServerGroupStatus' "DELETED"

pattern GameServerGroupStatusError :: GameServerGroupStatus
pattern GameServerGroupStatusError = GameServerGroupStatus' "ERROR"

{-# COMPLETE
  GameServerGroupStatusNew,
  GameServerGroupStatusActivating,
  GameServerGroupStatusActive,
  GameServerGroupStatusDeleteScheduled,
  GameServerGroupStatusDeleting,
  GameServerGroupStatusDeleted,
  GameServerGroupStatusError,
  GameServerGroupStatus'
  #-}

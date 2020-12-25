{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupDeleteOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupDeleteOption
  ( GameServerGroupDeleteOption
      ( GameServerGroupDeleteOption',
        GameServerGroupDeleteOptionSafeDelete,
        GameServerGroupDeleteOptionForceDelete,
        GameServerGroupDeleteOptionRetain,
        fromGameServerGroupDeleteOption
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype GameServerGroupDeleteOption = GameServerGroupDeleteOption'
  { fromGameServerGroupDeleteOption ::
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

pattern GameServerGroupDeleteOptionSafeDelete :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOptionSafeDelete = GameServerGroupDeleteOption' "SAFE_DELETE"

pattern GameServerGroupDeleteOptionForceDelete :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOptionForceDelete = GameServerGroupDeleteOption' "FORCE_DELETE"

pattern GameServerGroupDeleteOptionRetain :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOptionRetain = GameServerGroupDeleteOption' "RETAIN"

{-# COMPLETE
  GameServerGroupDeleteOptionSafeDelete,
  GameServerGroupDeleteOptionForceDelete,
  GameServerGroupDeleteOptionRetain,
  GameServerGroupDeleteOption'
  #-}

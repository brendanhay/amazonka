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
        ForceDelete,
        Retain,
        SafeDelete
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GameServerGroupDeleteOption = GameServerGroupDeleteOption' Lude.Text
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

pattern ForceDelete :: GameServerGroupDeleteOption
pattern ForceDelete = GameServerGroupDeleteOption' "FORCE_DELETE"

pattern Retain :: GameServerGroupDeleteOption
pattern Retain = GameServerGroupDeleteOption' "RETAIN"

pattern SafeDelete :: GameServerGroupDeleteOption
pattern SafeDelete = GameServerGroupDeleteOption' "SAFE_DELETE"

{-# COMPLETE
  ForceDelete,
  Retain,
  SafeDelete,
  GameServerGroupDeleteOption'
  #-}

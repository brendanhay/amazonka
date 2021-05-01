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
-- Module      : Network.AWS.GameLift.Types.GameServerGroupDeleteOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupDeleteOption
  ( GameServerGroupDeleteOption
      ( ..,
        GameServerGroupDeleteOption_FORCE_DELETE,
        GameServerGroupDeleteOption_RETAIN,
        GameServerGroupDeleteOption_SAFE_DELETE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype GameServerGroupDeleteOption = GameServerGroupDeleteOption'
  { fromGameServerGroupDeleteOption ::
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

pattern GameServerGroupDeleteOption_FORCE_DELETE :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_FORCE_DELETE = GameServerGroupDeleteOption' "FORCE_DELETE"

pattern GameServerGroupDeleteOption_RETAIN :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_RETAIN = GameServerGroupDeleteOption' "RETAIN"

pattern GameServerGroupDeleteOption_SAFE_DELETE :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_SAFE_DELETE = GameServerGroupDeleteOption' "SAFE_DELETE"

{-# COMPLETE
  GameServerGroupDeleteOption_FORCE_DELETE,
  GameServerGroupDeleteOption_RETAIN,
  GameServerGroupDeleteOption_SAFE_DELETE,
  GameServerGroupDeleteOption'
  #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupDeleteOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupDeleteOption where

import Network.AWS.Prelude

data GameServerGroupDeleteOption
  = ForceDelete
  | Retain
  | SafeDelete
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText GameServerGroupDeleteOption where
  parser =
    takeLowerText >>= \case
      "force_delete" -> pure ForceDelete
      "retain" -> pure Retain
      "safe_delete" -> pure SafeDelete
      e ->
        fromTextError $
          "Failure parsing GameServerGroupDeleteOption from value: '" <> e
            <> "'. Accepted values: force_delete, retain, safe_delete"

instance ToText GameServerGroupDeleteOption where
  toText = \case
    ForceDelete -> "FORCE_DELETE"
    Retain -> "RETAIN"
    SafeDelete -> "SAFE_DELETE"

instance Hashable GameServerGroupDeleteOption

instance NFData GameServerGroupDeleteOption

instance ToByteString GameServerGroupDeleteOption

instance ToQuery GameServerGroupDeleteOption

instance ToHeader GameServerGroupDeleteOption

instance ToJSON GameServerGroupDeleteOption where
  toJSON = toJSONText

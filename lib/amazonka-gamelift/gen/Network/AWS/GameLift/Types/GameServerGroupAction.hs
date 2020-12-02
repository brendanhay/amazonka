{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupAction where

import Network.AWS.Prelude

data GameServerGroupAction = ReplaceInstanceTypes
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

instance FromText GameServerGroupAction where
  parser =
    takeLowerText >>= \case
      "replace_instance_types" -> pure ReplaceInstanceTypes
      e ->
        fromTextError $
          "Failure parsing GameServerGroupAction from value: '" <> e
            <> "'. Accepted values: replace_instance_types"

instance ToText GameServerGroupAction where
  toText = \case
    ReplaceInstanceTypes -> "REPLACE_INSTANCE_TYPES"

instance Hashable GameServerGroupAction

instance NFData GameServerGroupAction

instance ToByteString GameServerGroupAction

instance ToQuery GameServerGroupAction

instance ToHeader GameServerGroupAction

instance ToJSON GameServerGroupAction where
  toJSON = toJSONText

instance FromJSON GameServerGroupAction where
  parseJSON = parseJSONText "GameServerGroupAction"

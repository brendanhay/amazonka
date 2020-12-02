{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Op
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Op where

import Network.AWS.Prelude

data Op
  = Add
  | Copy
  | Move
  | Remove
  | Replace
  | Test
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

instance FromText Op where
  parser =
    takeLowerText >>= \case
      "add" -> pure Add
      "copy" -> pure Copy
      "move" -> pure Move
      "remove" -> pure Remove
      "replace" -> pure Replace
      "test" -> pure Test
      e ->
        fromTextError $
          "Failure parsing Op from value: '" <> e
            <> "'. Accepted values: add, copy, move, remove, replace, test"

instance ToText Op where
  toText = \case
    Add -> "add"
    Copy -> "copy"
    Move -> "move"
    Remove -> "remove"
    Replace -> "replace"
    Test -> "test"

instance Hashable Op

instance NFData Op

instance ToByteString Op

instance ToQuery Op

instance ToHeader Op

instance ToJSON Op where
  toJSON = toJSONText

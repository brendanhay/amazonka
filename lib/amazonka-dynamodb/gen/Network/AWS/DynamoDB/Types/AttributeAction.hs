{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AttributeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AttributeAction where

import Network.AWS.Prelude

data AttributeAction
  = Add
  | Delete
  | Put
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

instance FromText AttributeAction where
  parser =
    takeLowerText >>= \case
      "add" -> pure Add
      "delete" -> pure Delete
      "put" -> pure Put
      e ->
        fromTextError $
          "Failure parsing AttributeAction from value: '" <> e
            <> "'. Accepted values: add, delete, put"

instance ToText AttributeAction where
  toText = \case
    Add -> "ADD"
    Delete -> "DELETE"
    Put -> "PUT"

instance Hashable AttributeAction

instance NFData AttributeAction

instance ToByteString AttributeAction

instance ToQuery AttributeAction

instance ToHeader AttributeAction

instance ToJSON AttributeAction where
  toJSON = toJSONText

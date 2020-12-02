{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectType where

import Network.AWS.Prelude

data ObjectType
  = Index
  | LeafNode
  | Node
  | Policy
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

instance FromText ObjectType where
  parser =
    takeLowerText >>= \case
      "index" -> pure Index
      "leaf_node" -> pure LeafNode
      "node" -> pure Node
      "policy" -> pure Policy
      e ->
        fromTextError $
          "Failure parsing ObjectType from value: '" <> e
            <> "'. Accepted values: index, leaf_node, node, policy"

instance ToText ObjectType where
  toText = \case
    Index -> "INDEX"
    LeafNode -> "LEAF_NODE"
    Node -> "NODE"
    Policy -> "POLICY"

instance Hashable ObjectType

instance NFData ObjectType

instance ToByteString ObjectType

instance ToQuery ObjectType

instance ToHeader ObjectType

instance ToJSON ObjectType where
  toJSON = toJSONText

instance FromJSON ObjectType where
  parseJSON = parseJSONText "ObjectType"

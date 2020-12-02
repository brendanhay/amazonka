{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ConsistencyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ConsistencyLevel where

import Network.AWS.Prelude

data ConsistencyLevel
  = Eventual
  | Serializable
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

instance FromText ConsistencyLevel where
  parser =
    takeLowerText >>= \case
      "eventual" -> pure Eventual
      "serializable" -> pure Serializable
      e ->
        fromTextError $
          "Failure parsing ConsistencyLevel from value: '" <> e
            <> "'. Accepted values: eventual, serializable"

instance ToText ConsistencyLevel where
  toText = \case
    Eventual -> "EVENTUAL"
    Serializable -> "SERIALIZABLE"

instance Hashable ConsistencyLevel

instance NFData ConsistencyLevel

instance ToByteString ConsistencyLevel

instance ToQuery ConsistencyLevel

instance ToHeader ConsistencyLevel

instance ToJSON ConsistencyLevel where
  toJSON = toJSONText

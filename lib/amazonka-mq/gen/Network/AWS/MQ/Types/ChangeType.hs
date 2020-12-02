{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ChangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ChangeType where

import Network.AWS.Prelude

-- | The type of change pending for the ActiveMQ user.
data ChangeType
  = Create
  | Delete
  | Update
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

instance FromText ChangeType where
  parser =
    takeLowerText >>= \case
      "create" -> pure Create
      "delete" -> pure Delete
      "update" -> pure Update
      e ->
        fromTextError $
          "Failure parsing ChangeType from value: '" <> e
            <> "'. Accepted values: create, delete, update"

instance ToText ChangeType where
  toText = \case
    Create -> "CREATE"
    Delete -> "DELETE"
    Update -> "UPDATE"

instance Hashable ChangeType

instance NFData ChangeType

instance ToByteString ChangeType

instance ToQuery ChangeType

instance ToHeader ChangeType

instance FromJSON ChangeType where
  parseJSON = parseJSONText "ChangeType"

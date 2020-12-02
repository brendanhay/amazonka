{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ChangeAction where

import Network.AWS.Prelude

data ChangeAction
  = Add
  | Modify
  | Remove
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

instance FromText ChangeAction where
  parser =
    takeLowerText >>= \case
      "add" -> pure Add
      "modify" -> pure Modify
      "remove" -> pure Remove
      e ->
        fromTextError $
          "Failure parsing ChangeAction from value: '" <> e
            <> "'. Accepted values: add, modify, remove"

instance ToText ChangeAction where
  toText = \case
    Add -> "ADD"
    Modify -> "MODIFY"
    Remove -> "REMOVE"

instance Hashable ChangeAction

instance NFData ChangeAction

instance ToByteString ChangeAction

instance ToQuery ChangeAction

instance ToHeader ChangeAction

instance FromJSON ChangeAction where
  parseJSON = parseJSONText "ChangeAction"

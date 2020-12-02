{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeAction where

import Network.AWS.Prelude

data ChangeAction
  = Add
  | Dynamic
  | Import
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
      "dynamic" -> pure Dynamic
      "import" -> pure Import
      "modify" -> pure Modify
      "remove" -> pure Remove
      e ->
        fromTextError $
          "Failure parsing ChangeAction from value: '" <> e
            <> "'. Accepted values: add, dynamic, import, modify, remove"

instance ToText ChangeAction where
  toText = \case
    Add -> "Add"
    Dynamic -> "Dynamic"
    Import -> "Import"
    Modify -> "Modify"
    Remove -> "Remove"

instance Hashable ChangeAction

instance NFData ChangeAction

instance ToByteString ChangeAction

instance ToQuery ChangeAction

instance ToHeader ChangeAction

instance FromXML ChangeAction where
  parseXML = parseXMLText "ChangeAction"

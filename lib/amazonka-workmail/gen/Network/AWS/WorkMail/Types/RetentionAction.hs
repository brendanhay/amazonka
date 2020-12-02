{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.RetentionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.RetentionAction where

import Network.AWS.Prelude

data RetentionAction
  = Delete
  | None
  | PermanentlyDelete
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

instance FromText RetentionAction where
  parser =
    takeLowerText >>= \case
      "delete" -> pure Delete
      "none" -> pure None
      "permanently_delete" -> pure PermanentlyDelete
      e ->
        fromTextError $
          "Failure parsing RetentionAction from value: '" <> e
            <> "'. Accepted values: delete, none, permanently_delete"

instance ToText RetentionAction where
  toText = \case
    Delete -> "DELETE"
    None -> "NONE"
    PermanentlyDelete -> "PERMANENTLY_DELETE"

instance Hashable RetentionAction

instance NFData RetentionAction

instance ToByteString RetentionAction

instance ToQuery RetentionAction

instance ToHeader RetentionAction

instance ToJSON RetentionAction where
  toJSON = toJSONText

instance FromJSON RetentionAction where
  parseJSON = parseJSONText "RetentionAction"

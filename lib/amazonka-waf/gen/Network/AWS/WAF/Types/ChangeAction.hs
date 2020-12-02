{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ChangeAction where

import Network.AWS.Prelude

data ChangeAction
  = Delete
  | Insert
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
      "delete" -> pure Delete
      "insert" -> pure Insert
      e ->
        fromTextError $
          "Failure parsing ChangeAction from value: '" <> e
            <> "'. Accepted values: delete, insert"

instance ToText ChangeAction where
  toText = \case
    Delete -> "DELETE"
    Insert -> "INSERT"

instance Hashable ChangeAction

instance NFData ChangeAction

instance ToByteString ChangeAction

instance ToQuery ChangeAction

instance ToHeader ChangeAction

instance ToJSON ChangeAction where
  toJSON = toJSONText

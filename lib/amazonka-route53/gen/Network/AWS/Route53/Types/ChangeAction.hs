{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeAction where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ChangeAction
  = Create
  | Delete
  | Upsert
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
      "create" -> pure Create
      "delete" -> pure Delete
      "upsert" -> pure Upsert
      e ->
        fromTextError $
          "Failure parsing ChangeAction from value: '" <> e
            <> "'. Accepted values: create, delete, upsert"

instance ToText ChangeAction where
  toText = \case
    Create -> "CREATE"
    Delete -> "DELETE"
    Upsert -> "UPSERT"

instance Hashable ChangeAction

instance NFData ChangeAction

instance ToByteString ChangeAction

instance ToQuery ChangeAction

instance ToHeader ChangeAction

instance ToXML ChangeAction where
  toXML = toXMLText

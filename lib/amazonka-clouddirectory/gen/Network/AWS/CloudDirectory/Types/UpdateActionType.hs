{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.UpdateActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.UpdateActionType where

import Network.AWS.Prelude

data UpdateActionType
  = CreateOrUpdate
  | Delete
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

instance FromText UpdateActionType where
  parser =
    takeLowerText >>= \case
      "create_or_update" -> pure CreateOrUpdate
      "delete" -> pure Delete
      e ->
        fromTextError $
          "Failure parsing UpdateActionType from value: '" <> e
            <> "'. Accepted values: create_or_update, delete"

instance ToText UpdateActionType where
  toText = \case
    CreateOrUpdate -> "CREATE_OR_UPDATE"
    Delete -> "DELETE"

instance Hashable UpdateActionType

instance NFData UpdateActionType

instance ToByteString UpdateActionType

instance ToQuery UpdateActionType

instance ToHeader UpdateActionType

instance ToJSON UpdateActionType where
  toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey where

import Network.AWS.Prelude

data AccessLevelFilterKey
  = Account
  | Role
  | User
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

instance FromText AccessLevelFilterKey where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      "role" -> pure Role
      "user" -> pure User
      e ->
        fromTextError $
          "Failure parsing AccessLevelFilterKey from value: '" <> e
            <> "'. Accepted values: account, role, user"

instance ToText AccessLevelFilterKey where
  toText = \case
    Account -> "Account"
    Role -> "Role"
    User -> "User"

instance Hashable AccessLevelFilterKey

instance NFData AccessLevelFilterKey

instance ToByteString AccessLevelFilterKey

instance ToQuery AccessLevelFilterKey

instance ToHeader AccessLevelFilterKey

instance ToJSON AccessLevelFilterKey where
  toJSON = toJSONText

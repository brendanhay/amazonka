{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserSortType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserSortType where

import Network.AWS.Prelude

data UserSortType
  = FullName
  | StorageLimit
  | StorageUsed
  | UserName
  | UserStatus
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

instance FromText UserSortType where
  parser =
    takeLowerText >>= \case
      "full_name" -> pure FullName
      "storage_limit" -> pure StorageLimit
      "storage_used" -> pure StorageUsed
      "user_name" -> pure UserName
      "user_status" -> pure UserStatus
      e ->
        fromTextError $
          "Failure parsing UserSortType from value: '" <> e
            <> "'. Accepted values: full_name, storage_limit, storage_used, user_name, user_status"

instance ToText UserSortType where
  toText = \case
    FullName -> "FULL_NAME"
    StorageLimit -> "STORAGE_LIMIT"
    StorageUsed -> "STORAGE_USED"
    UserName -> "USER_NAME"
    UserStatus -> "USER_STATUS"

instance Hashable UserSortType

instance NFData UserSortType

instance ToByteString UserSortType

instance ToQuery UserSortType

instance ToHeader UserSortType

instance ToJSON UserSortType where
  toJSON = toJSONText

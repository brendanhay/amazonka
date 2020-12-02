{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserStatusType where

import Network.AWS.Prelude

data UserStatusType
  = Active
  | Inactive
  | Pending
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

instance FromText UserStatusType where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing UserStatusType from value: '" <> e
            <> "'. Accepted values: active, inactive, pending"

instance ToText UserStatusType where
  toText = \case
    Active -> "ACTIVE"
    Inactive -> "INACTIVE"
    Pending -> "PENDING"

instance Hashable UserStatusType

instance NFData UserStatusType

instance ToByteString UserStatusType

instance ToQuery UserStatusType

instance ToHeader UserStatusType

instance FromJSON UserStatusType where
  parseJSON = parseJSONText "UserStatusType"

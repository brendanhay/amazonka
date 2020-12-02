{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.IndexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.IndexStatus where

import Network.AWS.Prelude

data IndexStatus
  = ISActive
  | ISCreating
  | ISDeleting
  | ISUpdating
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

instance FromText IndexStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure ISActive
      "creating" -> pure ISCreating
      "deleting" -> pure ISDeleting
      "updating" -> pure ISUpdating
      e ->
        fromTextError $
          "Failure parsing IndexStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, updating"

instance ToText IndexStatus where
  toText = \case
    ISActive -> "ACTIVE"
    ISCreating -> "CREATING"
    ISDeleting -> "DELETING"
    ISUpdating -> "UPDATING"

instance Hashable IndexStatus

instance NFData IndexStatus

instance ToByteString IndexStatus

instance ToQuery IndexStatus

instance ToHeader IndexStatus

instance FromJSON IndexStatus where
  parseJSON = parseJSONText "IndexStatus"

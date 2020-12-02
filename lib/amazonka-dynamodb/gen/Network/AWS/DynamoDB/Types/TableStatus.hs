{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableStatus where

import Network.AWS.Prelude

data TableStatus
  = TSActive
  | TSArchived
  | TSArchiving
  | TSCreating
  | TSDeleting
  | TSInaccessibleEncryptionCredentials
  | TSUpdating
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

instance FromText TableStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure TSActive
      "archived" -> pure TSArchived
      "archiving" -> pure TSArchiving
      "creating" -> pure TSCreating
      "deleting" -> pure TSDeleting
      "inaccessible_encryption_credentials" -> pure TSInaccessibleEncryptionCredentials
      "updating" -> pure TSUpdating
      e ->
        fromTextError $
          "Failure parsing TableStatus from value: '" <> e
            <> "'. Accepted values: active, archived, archiving, creating, deleting, inaccessible_encryption_credentials, updating"

instance ToText TableStatus where
  toText = \case
    TSActive -> "ACTIVE"
    TSArchived -> "ARCHIVED"
    TSArchiving -> "ARCHIVING"
    TSCreating -> "CREATING"
    TSDeleting -> "DELETING"
    TSInaccessibleEncryptionCredentials -> "INACCESSIBLE_ENCRYPTION_CREDENTIALS"
    TSUpdating -> "UPDATING"

instance Hashable TableStatus

instance NFData TableStatus

instance ToByteString TableStatus

instance ToQuery TableStatus

instance ToHeader TableStatus

instance FromJSON TableStatus where
  parseJSON = parseJSONText "TableStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JQStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JQStatus where

import Network.AWS.Prelude

data JQStatus
  = Creating
  | Deleted
  | Deleting
  | Invalid
  | Updating
  | Valid
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

instance FromText JQStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure Creating
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "invalid" -> pure Invalid
      "updating" -> pure Updating
      "valid" -> pure Valid
      e ->
        fromTextError $
          "Failure parsing JQStatus from value: '" <> e
            <> "'. Accepted values: creating, deleted, deleting, invalid, updating, valid"

instance ToText JQStatus where
  toText = \case
    Creating -> "CREATING"
    Deleted -> "DELETED"
    Deleting -> "DELETING"
    Invalid -> "INVALID"
    Updating -> "UPDATING"
    Valid -> "VALID"

instance Hashable JQStatus

instance NFData JQStatus

instance ToByteString JQStatus

instance ToQuery JQStatus

instance ToHeader JQStatus

instance FromJSON JQStatus where
  parseJSON = parseJSONText "JQStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CEStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CEStatus where

import Network.AWS.Prelude

data CEStatus
  = CESCreating
  | CESDeleted
  | CESDeleting
  | CESInvalid
  | CESUpdating
  | CESValid
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

instance FromText CEStatus where
  parser =
    takeLowerText >>= \case
      "creating" -> pure CESCreating
      "deleted" -> pure CESDeleted
      "deleting" -> pure CESDeleting
      "invalid" -> pure CESInvalid
      "updating" -> pure CESUpdating
      "valid" -> pure CESValid
      e ->
        fromTextError $
          "Failure parsing CEStatus from value: '" <> e
            <> "'. Accepted values: creating, deleted, deleting, invalid, updating, valid"

instance ToText CEStatus where
  toText = \case
    CESCreating -> "CREATING"
    CESDeleted -> "DELETED"
    CESDeleting -> "DELETING"
    CESInvalid -> "INVALID"
    CESUpdating -> "UPDATING"
    CESValid -> "VALID"

instance Hashable CEStatus

instance NFData CEStatus

instance ToByteString CEStatus

instance ToQuery CEStatus

instance ToHeader CEStatus

instance FromJSON CEStatus where
  parseJSON = parseJSONText "CEStatus"

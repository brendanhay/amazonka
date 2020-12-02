{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableStatus where

import Network.AWS.Prelude

data GlobalTableStatus
  = GTSActive
  | GTSCreating
  | GTSDeleting
  | GTSUpdating
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

instance FromText GlobalTableStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure GTSActive
      "creating" -> pure GTSCreating
      "deleting" -> pure GTSDeleting
      "updating" -> pure GTSUpdating
      e ->
        fromTextError $
          "Failure parsing GlobalTableStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, updating"

instance ToText GlobalTableStatus where
  toText = \case
    GTSActive -> "ACTIVE"
    GTSCreating -> "CREATING"
    GTSDeleting -> "DELETING"
    GTSUpdating -> "UPDATING"

instance Hashable GlobalTableStatus

instance NFData GlobalTableStatus

instance ToByteString GlobalTableStatus

instance ToQuery GlobalTableStatus

instance ToHeader GlobalTableStatus

instance FromJSON GlobalTableStatus where
  parseJSON = parseJSONText "GlobalTableStatus"

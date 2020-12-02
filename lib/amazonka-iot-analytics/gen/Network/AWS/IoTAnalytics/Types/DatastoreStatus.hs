{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStatus where

import Network.AWS.Prelude

data DatastoreStatus
  = DSActive
  | DSCreating
  | DSDeleting
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

instance FromText DatastoreStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSActive
      "creating" -> pure DSCreating
      "deleting" -> pure DSDeleting
      e ->
        fromTextError $
          "Failure parsing DatastoreStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting"

instance ToText DatastoreStatus where
  toText = \case
    DSActive -> "ACTIVE"
    DSCreating -> "CREATING"
    DSDeleting -> "DELETING"

instance Hashable DatastoreStatus

instance NFData DatastoreStatus

instance ToByteString DatastoreStatus

instance ToQuery DatastoreStatus

instance ToHeader DatastoreStatus

instance FromJSON DatastoreStatus where
  parseJSON = parseJSONText "DatastoreStatus"

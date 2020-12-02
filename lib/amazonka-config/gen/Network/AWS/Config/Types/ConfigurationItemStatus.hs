{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItemStatus where

import Network.AWS.Prelude

data ConfigurationItemStatus
  = OK
  | ResourceDeleted
  | ResourceDeletedNotRecorded
  | ResourceDiscovered
  | ResourceNotRecorded
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

instance FromText ConfigurationItemStatus where
  parser =
    takeLowerText >>= \case
      "ok" -> pure OK
      "resourcedeleted" -> pure ResourceDeleted
      "resourcedeletednotrecorded" -> pure ResourceDeletedNotRecorded
      "resourcediscovered" -> pure ResourceDiscovered
      "resourcenotrecorded" -> pure ResourceNotRecorded
      e ->
        fromTextError $
          "Failure parsing ConfigurationItemStatus from value: '" <> e
            <> "'. Accepted values: ok, resourcedeleted, resourcedeletednotrecorded, resourcediscovered, resourcenotrecorded"

instance ToText ConfigurationItemStatus where
  toText = \case
    OK -> "OK"
    ResourceDeleted -> "ResourceDeleted"
    ResourceDeletedNotRecorded -> "ResourceDeletedNotRecorded"
    ResourceDiscovered -> "ResourceDiscovered"
    ResourceNotRecorded -> "ResourceNotRecorded"

instance Hashable ConfigurationItemStatus

instance NFData ConfigurationItemStatus

instance ToByteString ConfigurationItemStatus

instance ToQuery ConfigurationItemStatus

instance ToHeader ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
  parseJSON = parseJSONText "ConfigurationItemStatus"

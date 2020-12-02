{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerCatalogStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerCatalogStatus where

import Network.AWS.Prelude

data ServerCatalogStatus
  = SCSAvailable
  | SCSDeleted
  | SCSExpired
  | SCSImporting
  | SCSNotImported
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

instance FromText ServerCatalogStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure SCSAvailable
      "deleted" -> pure SCSDeleted
      "expired" -> pure SCSExpired
      "importing" -> pure SCSImporting
      "not_imported" -> pure SCSNotImported
      e ->
        fromTextError $
          "Failure parsing ServerCatalogStatus from value: '" <> e
            <> "'. Accepted values: available, deleted, expired, importing, not_imported"

instance ToText ServerCatalogStatus where
  toText = \case
    SCSAvailable -> "AVAILABLE"
    SCSDeleted -> "DELETED"
    SCSExpired -> "EXPIRED"
    SCSImporting -> "IMPORTING"
    SCSNotImported -> "NOT_IMPORTED"

instance Hashable ServerCatalogStatus

instance NFData ServerCatalogStatus

instance ToByteString ServerCatalogStatus

instance ToQuery ServerCatalogStatus

instance ToHeader ServerCatalogStatus

instance FromJSON ServerCatalogStatus where
  parseJSON = parseJSONText "ServerCatalogStatus"

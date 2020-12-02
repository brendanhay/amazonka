{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StorageConnectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StorageConnectorType where

import Network.AWS.Prelude

-- | The type of storage connector.
data StorageConnectorType
  = GoogleDrive
  | Homefolders
  | OneDrive
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

instance FromText StorageConnectorType where
  parser =
    takeLowerText >>= \case
      "google_drive" -> pure GoogleDrive
      "homefolders" -> pure Homefolders
      "one_drive" -> pure OneDrive
      e ->
        fromTextError $
          "Failure parsing StorageConnectorType from value: '" <> e
            <> "'. Accepted values: google_drive, homefolders, one_drive"

instance ToText StorageConnectorType where
  toText = \case
    GoogleDrive -> "GOOGLE_DRIVE"
    Homefolders -> "HOMEFOLDERS"
    OneDrive -> "ONE_DRIVE"

instance Hashable StorageConnectorType

instance NFData StorageConnectorType

instance ToByteString StorageConnectorType

instance ToQuery StorageConnectorType

instance ToHeader StorageConnectorType

instance ToJSON StorageConnectorType where
  toJSON = toJSONText

instance FromJSON StorageConnectorType where
  parseJSON = parseJSONText "StorageConnectorType"

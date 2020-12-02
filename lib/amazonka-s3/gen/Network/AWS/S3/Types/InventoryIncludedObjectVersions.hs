{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryIncludedObjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryIncludedObjectVersions where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryIncludedObjectVersions
  = All
  | Current
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

instance FromText InventoryIncludedObjectVersions where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "current" -> pure Current
      e ->
        fromTextError $
          "Failure parsing InventoryIncludedObjectVersions from value: '" <> e
            <> "'. Accepted values: all, current"

instance ToText InventoryIncludedObjectVersions where
  toText = \case
    All -> "All"
    Current -> "Current"

instance Hashable InventoryIncludedObjectVersions

instance NFData InventoryIncludedObjectVersions

instance ToByteString InventoryIncludedObjectVersions

instance ToQuery InventoryIncludedObjectVersions

instance ToHeader InventoryIncludedObjectVersions

instance FromXML InventoryIncludedObjectVersions where
  parseXML = parseXMLText "InventoryIncludedObjectVersions"

instance ToXML InventoryIncludedObjectVersions where
  toXML = toXMLText

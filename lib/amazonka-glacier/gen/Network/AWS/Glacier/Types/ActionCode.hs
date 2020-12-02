{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ActionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ActionCode where

import Network.AWS.Prelude

data ActionCode
  = ArchiveRetrieval
  | InventoryRetrieval
  | Select
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

instance FromText ActionCode where
  parser =
    takeLowerText >>= \case
      "archiveretrieval" -> pure ArchiveRetrieval
      "inventoryretrieval" -> pure InventoryRetrieval
      "select" -> pure Select
      e ->
        fromTextError $
          "Failure parsing ActionCode from value: '" <> e
            <> "'. Accepted values: archiveretrieval, inventoryretrieval, select"

instance ToText ActionCode where
  toText = \case
    ArchiveRetrieval -> "ArchiveRetrieval"
    InventoryRetrieval -> "InventoryRetrieval"
    Select -> "Select"

instance Hashable ActionCode

instance NFData ActionCode

instance ToByteString ActionCode

instance ToQuery ActionCode

instance ToHeader ActionCode

instance FromJSON ActionCode where
  parseJSON = parseJSONText "ActionCode"

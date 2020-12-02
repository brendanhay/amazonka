{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryFrequency where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryFrequency
  = Daily
  | Weekly
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

instance FromText InventoryFrequency where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      "weekly" -> pure Weekly
      e ->
        fromTextError $
          "Failure parsing InventoryFrequency from value: '" <> e
            <> "'. Accepted values: daily, weekly"

instance ToText InventoryFrequency where
  toText = \case
    Daily -> "Daily"
    Weekly -> "Weekly"

instance Hashable InventoryFrequency

instance NFData InventoryFrequency

instance ToByteString InventoryFrequency

instance ToQuery InventoryFrequency

instance ToHeader InventoryFrequency

instance FromXML InventoryFrequency where
  parseXML = parseXMLText "InventoryFrequency"

instance ToXML InventoryFrequency where
  toXML = toXMLText

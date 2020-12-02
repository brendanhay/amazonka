{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.SchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.SchemaElement where

import Network.AWS.Prelude

-- | Whether or not AWS includes resource IDs in the report.
data SchemaElement = Resources
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

instance FromText SchemaElement where
  parser =
    takeLowerText >>= \case
      "resources" -> pure Resources
      e ->
        fromTextError $
          "Failure parsing SchemaElement from value: '" <> e
            <> "'. Accepted values: resources"

instance ToText SchemaElement where
  toText = \case
    Resources -> "RESOURCES"

instance Hashable SchemaElement

instance NFData SchemaElement

instance ToByteString SchemaElement

instance ToQuery SchemaElement

instance ToHeader SchemaElement

instance ToJSON SchemaElement where
  toJSON = toJSONText

instance FromJSON SchemaElement where
  parseJSON = parseJSONText "SchemaElement"

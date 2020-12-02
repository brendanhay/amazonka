{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics where

import Network.AWS.Prelude

data ReturnItemCollectionMetrics
  = RICMNone
  | RICMSize
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

instance FromText ReturnItemCollectionMetrics where
  parser =
    takeLowerText >>= \case
      "none" -> pure RICMNone
      "size" -> pure RICMSize
      e ->
        fromTextError $
          "Failure parsing ReturnItemCollectionMetrics from value: '" <> e
            <> "'. Accepted values: none, size"

instance ToText ReturnItemCollectionMetrics where
  toText = \case
    RICMNone -> "NONE"
    RICMSize -> "SIZE"

instance Hashable ReturnItemCollectionMetrics

instance NFData ReturnItemCollectionMetrics

instance ToByteString ReturnItemCollectionMetrics

instance ToQuery ReturnItemCollectionMetrics

instance ToHeader ReturnItemCollectionMetrics

instance ToJSON ReturnItemCollectionMetrics where
  toJSON = toJSONText

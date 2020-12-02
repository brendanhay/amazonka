{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption where

import Network.AWS.Prelude

data ReservedElasticsearchInstancePaymentOption
  = AllUpfront
  | NoUpfront
  | PartialUpfront
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

instance FromText ReservedElasticsearchInstancePaymentOption where
  parser =
    takeLowerText >>= \case
      "all_upfront" -> pure AllUpfront
      "no_upfront" -> pure NoUpfront
      "partial_upfront" -> pure PartialUpfront
      e ->
        fromTextError $
          "Failure parsing ReservedElasticsearchInstancePaymentOption from value: '" <> e
            <> "'. Accepted values: all_upfront, no_upfront, partial_upfront"

instance ToText ReservedElasticsearchInstancePaymentOption where
  toText = \case
    AllUpfront -> "ALL_UPFRONT"
    NoUpfront -> "NO_UPFRONT"
    PartialUpfront -> "PARTIAL_UPFRONT"

instance Hashable ReservedElasticsearchInstancePaymentOption

instance NFData ReservedElasticsearchInstancePaymentOption

instance ToByteString ReservedElasticsearchInstancePaymentOption

instance ToQuery ReservedElasticsearchInstancePaymentOption

instance ToHeader ReservedElasticsearchInstancePaymentOption

instance FromJSON ReservedElasticsearchInstancePaymentOption where
  parseJSON = parseJSONText "ReservedElasticsearchInstancePaymentOption"

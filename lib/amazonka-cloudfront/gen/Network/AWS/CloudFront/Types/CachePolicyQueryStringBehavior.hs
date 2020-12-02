{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior where

import Network.AWS.Prelude

data CachePolicyQueryStringBehavior
  = CPQSBAll
  | CPQSBAllExcept
  | CPQSBNone
  | CPQSBWhitelist
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

instance FromText CachePolicyQueryStringBehavior where
  parser =
    takeLowerText >>= \case
      "all" -> pure CPQSBAll
      "allexcept" -> pure CPQSBAllExcept
      "none" -> pure CPQSBNone
      "whitelist" -> pure CPQSBWhitelist
      e ->
        fromTextError $
          "Failure parsing CachePolicyQueryStringBehavior from value: '" <> e
            <> "'. Accepted values: all, allexcept, none, whitelist"

instance ToText CachePolicyQueryStringBehavior where
  toText = \case
    CPQSBAll -> "all"
    CPQSBAllExcept -> "allExcept"
    CPQSBNone -> "none"
    CPQSBWhitelist -> "whitelist"

instance Hashable CachePolicyQueryStringBehavior

instance NFData CachePolicyQueryStringBehavior

instance ToByteString CachePolicyQueryStringBehavior

instance ToQuery CachePolicyQueryStringBehavior

instance ToHeader CachePolicyQueryStringBehavior

instance FromXML CachePolicyQueryStringBehavior where
  parseXML = parseXMLText "CachePolicyQueryStringBehavior"

instance ToXML CachePolicyQueryStringBehavior where
  toXML = toXMLText

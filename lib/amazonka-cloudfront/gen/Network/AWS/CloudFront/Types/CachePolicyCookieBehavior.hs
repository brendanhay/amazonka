{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyCookieBehavior where

import Network.AWS.Prelude

data CachePolicyCookieBehavior
  = CPCBAll
  | CPCBAllExcept
  | CPCBNone
  | CPCBWhitelist
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

instance FromText CachePolicyCookieBehavior where
  parser =
    takeLowerText >>= \case
      "all" -> pure CPCBAll
      "allexcept" -> pure CPCBAllExcept
      "none" -> pure CPCBNone
      "whitelist" -> pure CPCBWhitelist
      e ->
        fromTextError $
          "Failure parsing CachePolicyCookieBehavior from value: '" <> e
            <> "'. Accepted values: all, allexcept, none, whitelist"

instance ToText CachePolicyCookieBehavior where
  toText = \case
    CPCBAll -> "all"
    CPCBAllExcept -> "allExcept"
    CPCBNone -> "none"
    CPCBWhitelist -> "whitelist"

instance Hashable CachePolicyCookieBehavior

instance NFData CachePolicyCookieBehavior

instance ToByteString CachePolicyCookieBehavior

instance ToQuery CachePolicyCookieBehavior

instance ToHeader CachePolicyCookieBehavior

instance FromXML CachePolicyCookieBehavior where
  parseXML = parseXMLText "CachePolicyCookieBehavior"

instance ToXML CachePolicyCookieBehavior where
  toXML = toXMLText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior where

import Network.AWS.Prelude

data OriginRequestPolicyCookieBehavior
  = ORPCBAll
  | ORPCBNone
  | ORPCBWhitelist
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

instance FromText OriginRequestPolicyCookieBehavior where
  parser =
    takeLowerText >>= \case
      "all" -> pure ORPCBAll
      "none" -> pure ORPCBNone
      "whitelist" -> pure ORPCBWhitelist
      e ->
        fromTextError $
          "Failure parsing OriginRequestPolicyCookieBehavior from value: '" <> e
            <> "'. Accepted values: all, none, whitelist"

instance ToText OriginRequestPolicyCookieBehavior where
  toText = \case
    ORPCBAll -> "all"
    ORPCBNone -> "none"
    ORPCBWhitelist -> "whitelist"

instance Hashable OriginRequestPolicyCookieBehavior

instance NFData OriginRequestPolicyCookieBehavior

instance ToByteString OriginRequestPolicyCookieBehavior

instance ToQuery OriginRequestPolicyCookieBehavior

instance ToHeader OriginRequestPolicyCookieBehavior

instance FromXML OriginRequestPolicyCookieBehavior where
  parseXML = parseXMLText "OriginRequestPolicyCookieBehavior"

instance ToXML OriginRequestPolicyCookieBehavior where
  toXML = toXMLText

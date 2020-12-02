{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior where

import Network.AWS.Prelude

data OriginRequestPolicyQueryStringBehavior
  = All
  | None
  | Whitelist
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

instance FromText OriginRequestPolicyQueryStringBehavior where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "none" -> pure None
      "whitelist" -> pure Whitelist
      e ->
        fromTextError $
          "Failure parsing OriginRequestPolicyQueryStringBehavior from value: '" <> e
            <> "'. Accepted values: all, none, whitelist"

instance ToText OriginRequestPolicyQueryStringBehavior where
  toText = \case
    All -> "all"
    None -> "none"
    Whitelist -> "whitelist"

instance Hashable OriginRequestPolicyQueryStringBehavior

instance NFData OriginRequestPolicyQueryStringBehavior

instance ToByteString OriginRequestPolicyQueryStringBehavior

instance ToQuery OriginRequestPolicyQueryStringBehavior

instance ToHeader OriginRequestPolicyQueryStringBehavior

instance FromXML OriginRequestPolicyQueryStringBehavior where
  parseXML = parseXMLText "OriginRequestPolicyQueryStringBehavior"

instance ToXML OriginRequestPolicyQueryStringBehavior where
  toXML = toXMLText

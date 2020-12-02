{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackPropertyIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackPropertyIdentifier where

import Network.AWS.Prelude

data AttackPropertyIdentifier
  = DestinationURL
  | Referrer
  | SourceASN
  | SourceCountry
  | SourceIPAddress
  | SourceUserAgent
  | WordpressPingbackReflector
  | WordpressPingbackSource
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

instance FromText AttackPropertyIdentifier where
  parser =
    takeLowerText >>= \case
      "destination_url" -> pure DestinationURL
      "referrer" -> pure Referrer
      "source_asn" -> pure SourceASN
      "source_country" -> pure SourceCountry
      "source_ip_address" -> pure SourceIPAddress
      "source_user_agent" -> pure SourceUserAgent
      "wordpress_pingback_reflector" -> pure WordpressPingbackReflector
      "wordpress_pingback_source" -> pure WordpressPingbackSource
      e ->
        fromTextError $
          "Failure parsing AttackPropertyIdentifier from value: '" <> e
            <> "'. Accepted values: destination_url, referrer, source_asn, source_country, source_ip_address, source_user_agent, wordpress_pingback_reflector, wordpress_pingback_source"

instance ToText AttackPropertyIdentifier where
  toText = \case
    DestinationURL -> "DESTINATION_URL"
    Referrer -> "REFERRER"
    SourceASN -> "SOURCE_ASN"
    SourceCountry -> "SOURCE_COUNTRY"
    SourceIPAddress -> "SOURCE_IP_ADDRESS"
    SourceUserAgent -> "SOURCE_USER_AGENT"
    WordpressPingbackReflector -> "WORDPRESS_PINGBACK_REFLECTOR"
    WordpressPingbackSource -> "WORDPRESS_PINGBACK_SOURCE"

instance Hashable AttackPropertyIdentifier

instance NFData AttackPropertyIdentifier

instance ToByteString AttackPropertyIdentifier

instance ToQuery AttackPropertyIdentifier

instance ToHeader AttackPropertyIdentifier

instance FromJSON AttackPropertyIdentifier where
  parseJSON = parseJSONText "AttackPropertyIdentifier"

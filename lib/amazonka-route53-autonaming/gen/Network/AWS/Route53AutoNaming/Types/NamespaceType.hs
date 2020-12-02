{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceType where

import Network.AWS.Prelude

data NamespaceType
  = NTDNSPrivate
  | NTDNSPublic
  | NTHTTP
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

instance FromText NamespaceType where
  parser =
    takeLowerText >>= \case
      "dns_private" -> pure NTDNSPrivate
      "dns_public" -> pure NTDNSPublic
      "http" -> pure NTHTTP
      e ->
        fromTextError $
          "Failure parsing NamespaceType from value: '" <> e
            <> "'. Accepted values: dns_private, dns_public, http"

instance ToText NamespaceType where
  toText = \case
    NTDNSPrivate -> "DNS_PRIVATE"
    NTDNSPublic -> "DNS_PUBLIC"
    NTHTTP -> "HTTP"

instance Hashable NamespaceType

instance NFData NamespaceType

instance ToByteString NamespaceType

instance ToQuery NamespaceType

instance ToHeader NamespaceType

instance FromJSON NamespaceType where
  parseJSON = parseJSONText "NamespaceType"

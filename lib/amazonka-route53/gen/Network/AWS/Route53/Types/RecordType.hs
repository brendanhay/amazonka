{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.RecordType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.RecordType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data RecordType
  = A
  | Aaaa
  | Caa
  | Cname
  | MX
  | NS
  | Naptr
  | Ptr
  | Soa
  | Spf
  | Srv
  | Txt
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

instance FromText RecordType where
  parser =
    takeLowerText >>= \case
      "a" -> pure A
      "aaaa" -> pure Aaaa
      "caa" -> pure Caa
      "cname" -> pure Cname
      "mx" -> pure MX
      "ns" -> pure NS
      "naptr" -> pure Naptr
      "ptr" -> pure Ptr
      "soa" -> pure Soa
      "spf" -> pure Spf
      "srv" -> pure Srv
      "txt" -> pure Txt
      e ->
        fromTextError $
          "Failure parsing RecordType from value: '" <> e
            <> "'. Accepted values: a, aaaa, caa, cname, mx, ns, naptr, ptr, soa, spf, srv, txt"

instance ToText RecordType where
  toText = \case
    A -> "A"
    Aaaa -> "AAAA"
    Caa -> "CAA"
    Cname -> "CNAME"
    MX -> "MX"
    NS -> "NS"
    Naptr -> "NAPTR"
    Ptr -> "PTR"
    Soa -> "SOA"
    Spf -> "SPF"
    Srv -> "SRV"
    Txt -> "TXT"

instance Hashable RecordType

instance NFData RecordType

instance ToByteString RecordType

instance ToQuery RecordType

instance ToHeader RecordType

instance FromXML RecordType where
  parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
  toXML = toXMLText

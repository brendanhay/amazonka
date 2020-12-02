{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SSLSupportMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SSLSupportMethod where

import Network.AWS.Prelude

data SSLSupportMethod
  = SNIOnly
  | StaticIP
  | VIP
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

instance FromText SSLSupportMethod where
  parser =
    takeLowerText >>= \case
      "sni-only" -> pure SNIOnly
      "static-ip" -> pure StaticIP
      "vip" -> pure VIP
      e ->
        fromTextError $
          "Failure parsing SSLSupportMethod from value: '" <> e
            <> "'. Accepted values: sni-only, static-ip, vip"

instance ToText SSLSupportMethod where
  toText = \case
    SNIOnly -> "sni-only"
    StaticIP -> "static-ip"
    VIP -> "vip"

instance Hashable SSLSupportMethod

instance NFData SSLSupportMethod

instance ToByteString SSLSupportMethod

instance ToQuery SSLSupportMethod

instance ToHeader SSLSupportMethod

instance FromXML SSLSupportMethod where
  parseXML = parseXMLText "SSLSupportMethod"

instance ToXML SSLSupportMethod where
  toXML = toXMLText

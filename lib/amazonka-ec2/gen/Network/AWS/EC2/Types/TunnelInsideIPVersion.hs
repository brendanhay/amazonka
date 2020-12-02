{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TunnelInsideIPVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TunnelInsideIPVersion where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TunnelInsideIPVersion
  = IPV4
  | IPV6
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

instance FromText TunnelInsideIPVersion where
  parser =
    takeLowerText >>= \case
      "ipv4" -> pure IPV4
      "ipv6" -> pure IPV6
      e ->
        fromTextError $
          "Failure parsing TunnelInsideIPVersion from value: '" <> e
            <> "'. Accepted values: ipv4, ipv6"

instance ToText TunnelInsideIPVersion where
  toText = \case
    IPV4 -> "ipv4"
    IPV6 -> "ipv6"

instance Hashable TunnelInsideIPVersion

instance NFData TunnelInsideIPVersion

instance ToByteString TunnelInsideIPVersion

instance ToQuery TunnelInsideIPVersion

instance ToHeader TunnelInsideIPVersion

instance FromXML TunnelInsideIPVersion where
  parseXML = parseXMLText "TunnelInsideIPVersion"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.IPAddressType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.IPAddressType where

import Network.AWS.Prelude

data IPAddressType
  = Dualstack
  | IPV4
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

instance FromText IPAddressType where
  parser =
    takeLowerText >>= \case
      "dualstack" -> pure Dualstack
      "ipv4" -> pure IPV4
      e ->
        fromTextError $
          "Failure parsing IPAddressType from value: '" <> e
            <> "'. Accepted values: dualstack, ipv4"

instance ToText IPAddressType where
  toText = \case
    Dualstack -> "dualstack"
    IPV4 -> "ipv4"

instance Hashable IPAddressType

instance NFData IPAddressType

instance ToByteString IPAddressType

instance ToQuery IPAddressType

instance ToHeader IPAddressType

instance FromXML IPAddressType where
  parseXML = parseXMLText "IPAddressType"

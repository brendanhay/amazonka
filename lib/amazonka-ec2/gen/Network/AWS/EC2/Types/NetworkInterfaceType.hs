{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceType
  = NITEfa
  | NITInterface
  | NITNatGateway
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

instance FromText NetworkInterfaceType where
  parser =
    takeLowerText >>= \case
      "efa" -> pure NITEfa
      "interface" -> pure NITInterface
      "natgateway" -> pure NITNatGateway
      e ->
        fromTextError $
          "Failure parsing NetworkInterfaceType from value: '" <> e
            <> "'. Accepted values: efa, interface, natgateway"

instance ToText NetworkInterfaceType where
  toText = \case
    NITEfa -> "efa"
    NITInterface -> "interface"
    NITNatGateway -> "natGateway"

instance Hashable NetworkInterfaceType

instance NFData NetworkInterfaceType

instance ToByteString NetworkInterfaceType

instance ToQuery NetworkInterfaceType

instance ToHeader NetworkInterfaceType

instance FromXML NetworkInterfaceType where
  parseXML = parseXMLText "NetworkInterfaceType"

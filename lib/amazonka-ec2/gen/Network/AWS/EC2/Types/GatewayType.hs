{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GatewayType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GatewayType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data GatewayType = IPsec_1
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

instance FromText GatewayType where
  parser =
    takeLowerText >>= \case
      "ipsec.1" -> pure IPsec_1
      e ->
        fromTextError $
          "Failure parsing GatewayType from value: '" <> e
            <> "'. Accepted values: ipsec.1"

instance ToText GatewayType where
  toText = \case
    IPsec_1 -> "ipsec.1"

instance Hashable GatewayType

instance NFData GatewayType

instance ToByteString GatewayType

instance ToQuery GatewayType

instance ToHeader GatewayType

instance FromXML GatewayType where
  parseXML = parseXMLText "GatewayType"

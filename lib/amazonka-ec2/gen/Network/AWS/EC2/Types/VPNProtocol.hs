{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNProtocol where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPNProtocol = Openvpn
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

instance FromText VPNProtocol where
  parser =
    takeLowerText >>= \case
      "openvpn" -> pure Openvpn
      e ->
        fromTextError $
          "Failure parsing VPNProtocol from value: '" <> e
            <> "'. Accepted values: openvpn"

instance ToText VPNProtocol where
  toText = \case
    Openvpn -> "openvpn"

instance Hashable VPNProtocol

instance NFData VPNProtocol

instance ToByteString VPNProtocol

instance ToQuery VPNProtocol

instance ToHeader VPNProtocol

instance FromXML VPNProtocol where
  parseXML = parseXMLText "VPNProtocol"

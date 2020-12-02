{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransportProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransportProtocol where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransportProtocol
  = TCP
  | Udp
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

instance FromText TransportProtocol where
  parser =
    takeLowerText >>= \case
      "tcp" -> pure TCP
      "udp" -> pure Udp
      e ->
        fromTextError $
          "Failure parsing TransportProtocol from value: '" <> e
            <> "'. Accepted values: tcp, udp"

instance ToText TransportProtocol where
  toText = \case
    TCP -> "tcp"
    Udp -> "udp"

instance Hashable TransportProtocol

instance NFData TransportProtocol

instance ToByteString TransportProtocol

instance ToQuery TransportProtocol

instance ToHeader TransportProtocol

instance FromXML TransportProtocol where
  parseXML = parseXMLText "TransportProtocol"

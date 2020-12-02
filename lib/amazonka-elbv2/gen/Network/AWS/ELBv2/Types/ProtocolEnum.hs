{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ProtocolEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ProtocolEnum where

import Network.AWS.Prelude

data ProtocolEnum
  = Geneve
  | HTTP
  | HTTPS
  | TCP
  | TCPUdp
  | TLS
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

instance FromText ProtocolEnum where
  parser =
    takeLowerText >>= \case
      "geneve" -> pure Geneve
      "http" -> pure HTTP
      "https" -> pure HTTPS
      "tcp" -> pure TCP
      "tcp_udp" -> pure TCPUdp
      "tls" -> pure TLS
      "udp" -> pure Udp
      e ->
        fromTextError $
          "Failure parsing ProtocolEnum from value: '" <> e
            <> "'. Accepted values: geneve, http, https, tcp, tcp_udp, tls, udp"

instance ToText ProtocolEnum where
  toText = \case
    Geneve -> "GENEVE"
    HTTP -> "HTTP"
    HTTPS -> "HTTPS"
    TCP -> "TCP"
    TCPUdp -> "TCP_UDP"
    TLS -> "TLS"
    Udp -> "UDP"

instance Hashable ProtocolEnum

instance NFData ProtocolEnum

instance ToByteString ProtocolEnum

instance ToQuery ProtocolEnum

instance ToHeader ProtocolEnum

instance FromXML ProtocolEnum where
  parseXML = parseXMLText "ProtocolEnum"

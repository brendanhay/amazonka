{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceProtocol where

import Network.AWS.Prelude

data ContainerServiceProtocol
  = CSPHTTP
  | CSPHTTPS
  | CSPTCP
  | CSPUdp
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

instance FromText ContainerServiceProtocol where
  parser =
    takeLowerText >>= \case
      "http" -> pure CSPHTTP
      "https" -> pure CSPHTTPS
      "tcp" -> pure CSPTCP
      "udp" -> pure CSPUdp
      e ->
        fromTextError $
          "Failure parsing ContainerServiceProtocol from value: '" <> e
            <> "'. Accepted values: http, https, tcp, udp"

instance ToText ContainerServiceProtocol where
  toText = \case
    CSPHTTP -> "HTTP"
    CSPHTTPS -> "HTTPS"
    CSPTCP -> "TCP"
    CSPUdp -> "UDP"

instance Hashable ContainerServiceProtocol

instance NFData ContainerServiceProtocol

instance ToByteString ContainerServiceProtocol

instance ToQuery ContainerServiceProtocol

instance ToHeader ContainerServiceProtocol

instance ToJSON ContainerServiceProtocol where
  toJSON = toJSONText

instance FromJSON ContainerServiceProtocol where
  parseJSON = parseJSONText "ContainerServiceProtocol"

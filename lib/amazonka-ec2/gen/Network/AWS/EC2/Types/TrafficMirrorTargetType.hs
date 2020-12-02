{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorTargetType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorTargetType
  = NetworkInterface
  | NetworkLoadBalancer
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

instance FromText TrafficMirrorTargetType where
  parser =
    takeLowerText >>= \case
      "network-interface" -> pure NetworkInterface
      "network-load-balancer" -> pure NetworkLoadBalancer
      e ->
        fromTextError $
          "Failure parsing TrafficMirrorTargetType from value: '" <> e
            <> "'. Accepted values: network-interface, network-load-balancer"

instance ToText TrafficMirrorTargetType where
  toText = \case
    NetworkInterface -> "network-interface"
    NetworkLoadBalancer -> "network-load-balancer"

instance Hashable TrafficMirrorTargetType

instance NFData TrafficMirrorTargetType

instance ToByteString TrafficMirrorTargetType

instance ToQuery TrafficMirrorTargetType

instance ToHeader TrafficMirrorTargetType

instance FromXML TrafficMirrorTargetType where
  parseXML = parseXMLText "TrafficMirrorTargetType"

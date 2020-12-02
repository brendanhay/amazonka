{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSessionField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorSessionField where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorSessionField
  = TMSFDescription
  | TMSFPacketLength
  | TMSFVirtualNetworkId
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

instance FromText TrafficMirrorSessionField where
  parser =
    takeLowerText >>= \case
      "description" -> pure TMSFDescription
      "packet-length" -> pure TMSFPacketLength
      "virtual-network-id" -> pure TMSFVirtualNetworkId
      e ->
        fromTextError $
          "Failure parsing TrafficMirrorSessionField from value: '" <> e
            <> "'. Accepted values: description, packet-length, virtual-network-id"

instance ToText TrafficMirrorSessionField where
  toText = \case
    TMSFDescription -> "description"
    TMSFPacketLength -> "packet-length"
    TMSFVirtualNetworkId -> "virtual-network-id"

instance Hashable TrafficMirrorSessionField

instance NFData TrafficMirrorSessionField

instance ToByteString TrafficMirrorSessionField

instance ToQuery TrafficMirrorSessionField

instance ToHeader TrafficMirrorSessionField

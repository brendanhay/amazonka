{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorNetworkService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorNetworkService where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorNetworkService = AmazonDNS
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

instance FromText TrafficMirrorNetworkService where
  parser =
    takeLowerText >>= \case
      "amazon-dns" -> pure AmazonDNS
      e ->
        fromTextError $
          "Failure parsing TrafficMirrorNetworkService from value: '" <> e
            <> "'. Accepted values: amazon-dns"

instance ToText TrafficMirrorNetworkService where
  toText = \case
    AmazonDNS -> "amazon-dns"

instance Hashable TrafficMirrorNetworkService

instance NFData TrafficMirrorNetworkService

instance ToByteString TrafficMirrorNetworkService

instance ToQuery TrafficMirrorNetworkService

instance ToHeader TrafficMirrorNetworkService

instance FromXML TrafficMirrorNetworkService where
  parseXML = parseXMLText "TrafficMirrorNetworkService"

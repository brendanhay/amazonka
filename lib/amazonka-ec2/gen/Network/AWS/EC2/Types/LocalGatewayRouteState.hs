{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LocalGatewayRouteState
  = LGRSActive
  | LGRSBlackhole
  | LGRSDeleted
  | LGRSDeleting
  | LGRSPending
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

instance FromText LocalGatewayRouteState where
  parser =
    takeLowerText >>= \case
      "active" -> pure LGRSActive
      "blackhole" -> pure LGRSBlackhole
      "deleted" -> pure LGRSDeleted
      "deleting" -> pure LGRSDeleting
      "pending" -> pure LGRSPending
      e ->
        fromTextError $
          "Failure parsing LocalGatewayRouteState from value: '" <> e
            <> "'. Accepted values: active, blackhole, deleted, deleting, pending"

instance ToText LocalGatewayRouteState where
  toText = \case
    LGRSActive -> "active"
    LGRSBlackhole -> "blackhole"
    LGRSDeleted -> "deleted"
    LGRSDeleting -> "deleting"
    LGRSPending -> "pending"

instance Hashable LocalGatewayRouteState

instance NFData LocalGatewayRouteState

instance ToByteString LocalGatewayRouteState

instance ToQuery LocalGatewayRouteState

instance ToHeader LocalGatewayRouteState

instance FromXML LocalGatewayRouteState where
  parseXML = parseXMLText "LocalGatewayRouteState"

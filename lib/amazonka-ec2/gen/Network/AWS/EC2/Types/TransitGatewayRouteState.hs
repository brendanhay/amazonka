{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteState
  ( TransitGatewayRouteState
    ( TransitGatewayRouteState'
    , TransitGatewayRouteStatePending
    , TransitGatewayRouteStateActive
    , TransitGatewayRouteStateBlackhole
    , TransitGatewayRouteStateDeleting
    , TransitGatewayRouteStateDeleted
    , fromTransitGatewayRouteState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TransitGatewayRouteState = TransitGatewayRouteState'{fromTransitGatewayRouteState
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern TransitGatewayRouteStatePending :: TransitGatewayRouteState
pattern TransitGatewayRouteStatePending = TransitGatewayRouteState' "pending"

pattern TransitGatewayRouteStateActive :: TransitGatewayRouteState
pattern TransitGatewayRouteStateActive = TransitGatewayRouteState' "active"

pattern TransitGatewayRouteStateBlackhole :: TransitGatewayRouteState
pattern TransitGatewayRouteStateBlackhole = TransitGatewayRouteState' "blackhole"

pattern TransitGatewayRouteStateDeleting :: TransitGatewayRouteState
pattern TransitGatewayRouteStateDeleting = TransitGatewayRouteState' "deleting"

pattern TransitGatewayRouteStateDeleted :: TransitGatewayRouteState
pattern TransitGatewayRouteStateDeleted = TransitGatewayRouteState' "deleted"

{-# COMPLETE 
  TransitGatewayRouteStatePending,

  TransitGatewayRouteStateActive,

  TransitGatewayRouteStateBlackhole,

  TransitGatewayRouteStateDeleting,

  TransitGatewayRouteStateDeleted,
  TransitGatewayRouteState'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteState
  ( TransitGatewayRouteState
      ( TransitGatewayRouteState',
        TGRSActive,
        TGRSBlackhole,
        TGRSDeleted,
        TGRSDeleting,
        TGRSPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayRouteState = TransitGatewayRouteState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TGRSActive :: TransitGatewayRouteState
pattern TGRSActive = TransitGatewayRouteState' "active"

pattern TGRSBlackhole :: TransitGatewayRouteState
pattern TGRSBlackhole = TransitGatewayRouteState' "blackhole"

pattern TGRSDeleted :: TransitGatewayRouteState
pattern TGRSDeleted = TransitGatewayRouteState' "deleted"

pattern TGRSDeleting :: TransitGatewayRouteState
pattern TGRSDeleting = TransitGatewayRouteState' "deleting"

pattern TGRSPending :: TransitGatewayRouteState
pattern TGRSPending = TransitGatewayRouteState' "pending"

{-# COMPLETE
  TGRSActive,
  TGRSBlackhole,
  TGRSDeleted,
  TGRSDeleting,
  TGRSPending,
  TransitGatewayRouteState'
  #-}

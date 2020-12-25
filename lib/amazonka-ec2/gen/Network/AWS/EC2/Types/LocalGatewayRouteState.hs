{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteState
  ( LocalGatewayRouteState
      ( LocalGatewayRouteState',
        LocalGatewayRouteStatePending,
        LocalGatewayRouteStateActive,
        LocalGatewayRouteStateBlackhole,
        LocalGatewayRouteStateDeleting,
        LocalGatewayRouteStateDeleted,
        fromLocalGatewayRouteState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LocalGatewayRouteState = LocalGatewayRouteState'
  { fromLocalGatewayRouteState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern LocalGatewayRouteStatePending :: LocalGatewayRouteState
pattern LocalGatewayRouteStatePending = LocalGatewayRouteState' "pending"

pattern LocalGatewayRouteStateActive :: LocalGatewayRouteState
pattern LocalGatewayRouteStateActive = LocalGatewayRouteState' "active"

pattern LocalGatewayRouteStateBlackhole :: LocalGatewayRouteState
pattern LocalGatewayRouteStateBlackhole = LocalGatewayRouteState' "blackhole"

pattern LocalGatewayRouteStateDeleting :: LocalGatewayRouteState
pattern LocalGatewayRouteStateDeleting = LocalGatewayRouteState' "deleting"

pattern LocalGatewayRouteStateDeleted :: LocalGatewayRouteState
pattern LocalGatewayRouteStateDeleted = LocalGatewayRouteState' "deleted"

{-# COMPLETE
  LocalGatewayRouteStatePending,
  LocalGatewayRouteStateActive,
  LocalGatewayRouteStateBlackhole,
  LocalGatewayRouteStateDeleting,
  LocalGatewayRouteStateDeleted,
  LocalGatewayRouteState'
  #-}

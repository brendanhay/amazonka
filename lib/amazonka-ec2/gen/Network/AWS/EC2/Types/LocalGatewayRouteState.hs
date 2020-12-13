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
        LGRSPending,
        LGRSActive,
        LGRSBlackhole,
        LGRSDeleting,
        LGRSDeleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LocalGatewayRouteState = LocalGatewayRouteState' Lude.Text
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

pattern LGRSPending :: LocalGatewayRouteState
pattern LGRSPending = LocalGatewayRouteState' "pending"

pattern LGRSActive :: LocalGatewayRouteState
pattern LGRSActive = LocalGatewayRouteState' "active"

pattern LGRSBlackhole :: LocalGatewayRouteState
pattern LGRSBlackhole = LocalGatewayRouteState' "blackhole"

pattern LGRSDeleting :: LocalGatewayRouteState
pattern LGRSDeleting = LocalGatewayRouteState' "deleting"

pattern LGRSDeleted :: LocalGatewayRouteState
pattern LGRSDeleted = LocalGatewayRouteState' "deleted"

{-# COMPLETE
  LGRSPending,
  LGRSActive,
  LGRSBlackhole,
  LGRSDeleting,
  LGRSDeleted,
  LocalGatewayRouteState'
  #-}

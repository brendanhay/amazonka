{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayState
  ( TransitGatewayState
      ( TransitGatewayState',
        TransitGatewayStatePending,
        TransitGatewayStateAvailable,
        TransitGatewayStateModifying,
        TransitGatewayStateDeleting,
        TransitGatewayStateDeleted,
        fromTransitGatewayState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TransitGatewayState = TransitGatewayState'
  { fromTransitGatewayState ::
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

pattern TransitGatewayStatePending :: TransitGatewayState
pattern TransitGatewayStatePending = TransitGatewayState' "pending"

pattern TransitGatewayStateAvailable :: TransitGatewayState
pattern TransitGatewayStateAvailable = TransitGatewayState' "available"

pattern TransitGatewayStateModifying :: TransitGatewayState
pattern TransitGatewayStateModifying = TransitGatewayState' "modifying"

pattern TransitGatewayStateDeleting :: TransitGatewayState
pattern TransitGatewayStateDeleting = TransitGatewayState' "deleting"

pattern TransitGatewayStateDeleted :: TransitGatewayState
pattern TransitGatewayStateDeleted = TransitGatewayState' "deleted"

{-# COMPLETE
  TransitGatewayStatePending,
  TransitGatewayStateAvailable,
  TransitGatewayStateModifying,
  TransitGatewayStateDeleting,
  TransitGatewayStateDeleted,
  TransitGatewayState'
  #-}

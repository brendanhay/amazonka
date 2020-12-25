{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGatewayState
  ( CarrierGatewayState
      ( CarrierGatewayState',
        CarrierGatewayStatePending,
        CarrierGatewayStateAvailable,
        CarrierGatewayStateDeleting,
        CarrierGatewayStateDeleted,
        fromCarrierGatewayState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CarrierGatewayState = CarrierGatewayState'
  { fromCarrierGatewayState ::
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

pattern CarrierGatewayStatePending :: CarrierGatewayState
pattern CarrierGatewayStatePending = CarrierGatewayState' "pending"

pattern CarrierGatewayStateAvailable :: CarrierGatewayState
pattern CarrierGatewayStateAvailable = CarrierGatewayState' "available"

pattern CarrierGatewayStateDeleting :: CarrierGatewayState
pattern CarrierGatewayStateDeleting = CarrierGatewayState' "deleting"

pattern CarrierGatewayStateDeleted :: CarrierGatewayState
pattern CarrierGatewayStateDeleted = CarrierGatewayState' "deleted"

{-# COMPLETE
  CarrierGatewayStatePending,
  CarrierGatewayStateAvailable,
  CarrierGatewayStateDeleting,
  CarrierGatewayStateDeleted,
  CarrierGatewayState'
  #-}

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
        Available,
        Deleted,
        Deleting,
        Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CarrierGatewayState = CarrierGatewayState' Lude.Text
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

pattern Available :: CarrierGatewayState
pattern Available = CarrierGatewayState' "available"

pattern Deleted :: CarrierGatewayState
pattern Deleted = CarrierGatewayState' "deleted"

pattern Deleting :: CarrierGatewayState
pattern Deleting = CarrierGatewayState' "deleting"

pattern Pending :: CarrierGatewayState
pattern Pending = CarrierGatewayState' "pending"

{-# COMPLETE
  Available,
  Deleted,
  Deleting,
  Pending,
  CarrierGatewayState'
  #-}

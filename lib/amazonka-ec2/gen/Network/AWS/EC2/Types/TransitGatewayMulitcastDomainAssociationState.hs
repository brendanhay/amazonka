{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulitcastDomainAssociationState
  ( TransitGatewayMulitcastDomainAssociationState
      ( TransitGatewayMulitcastDomainAssociationState',
        Associated,
        Associating,
        Disassociated,
        Disassociating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayMulitcastDomainAssociationState = TransitGatewayMulitcastDomainAssociationState' Lude.Text
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

pattern Associated :: TransitGatewayMulitcastDomainAssociationState
pattern Associated = TransitGatewayMulitcastDomainAssociationState' "associated"

pattern Associating :: TransitGatewayMulitcastDomainAssociationState
pattern Associating = TransitGatewayMulitcastDomainAssociationState' "associating"

pattern Disassociated :: TransitGatewayMulitcastDomainAssociationState
pattern Disassociated = TransitGatewayMulitcastDomainAssociationState' "disassociated"

pattern Disassociating :: TransitGatewayMulitcastDomainAssociationState
pattern Disassociating = TransitGatewayMulitcastDomainAssociationState' "disassociating"

{-# COMPLETE
  Associated,
  Associating,
  Disassociated,
  Disassociating,
  TransitGatewayMulitcastDomainAssociationState'
  #-}

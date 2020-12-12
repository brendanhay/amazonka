{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAssociationState
  ( TransitGatewayAssociationState
      ( TransitGatewayAssociationState',
        TGASAssociated,
        TGASAssociating,
        TGASDisassociated,
        TGASDisassociating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayAssociationState = TransitGatewayAssociationState' Lude.Text
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

pattern TGASAssociated :: TransitGatewayAssociationState
pattern TGASAssociated = TransitGatewayAssociationState' "associated"

pattern TGASAssociating :: TransitGatewayAssociationState
pattern TGASAssociating = TransitGatewayAssociationState' "associating"

pattern TGASDisassociated :: TransitGatewayAssociationState
pattern TGASDisassociated = TransitGatewayAssociationState' "disassociated"

pattern TGASDisassociating :: TransitGatewayAssociationState
pattern TGASDisassociating = TransitGatewayAssociationState' "disassociating"

{-# COMPLETE
  TGASAssociated,
  TGASAssociating,
  TGASDisassociated,
  TGASDisassociating,
  TransitGatewayAssociationState'
  #-}

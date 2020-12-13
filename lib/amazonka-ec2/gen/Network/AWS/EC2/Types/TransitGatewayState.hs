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
        TGSPending,
        TGSAvailable,
        TGSModifying,
        TGSDeleting,
        TGSDeleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitGatewayState = TransitGatewayState' Lude.Text
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

pattern TGSPending :: TransitGatewayState
pattern TGSPending = TransitGatewayState' "pending"

pattern TGSAvailable :: TransitGatewayState
pattern TGSAvailable = TransitGatewayState' "available"

pattern TGSModifying :: TransitGatewayState
pattern TGSModifying = TransitGatewayState' "modifying"

pattern TGSDeleting :: TransitGatewayState
pattern TGSDeleting = TransitGatewayState' "deleting"

pattern TGSDeleted :: TransitGatewayState
pattern TGSDeleted = TransitGatewayState' "deleted"

{-# COMPLETE
  TGSPending,
  TGSAvailable,
  TGSModifying,
  TGSDeleting,
  TGSDeleted,
  TransitGatewayState'
  #-}

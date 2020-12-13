{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayState
  ( DirectConnectGatewayState
      ( DirectConnectGatewayState',
        DCGSPending,
        DCGSAvailable,
        DCGSDeleting,
        DCGSDeleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DirectConnectGatewayState = DirectConnectGatewayState' Lude.Text
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

pattern DCGSPending :: DirectConnectGatewayState
pattern DCGSPending = DirectConnectGatewayState' "pending"

pattern DCGSAvailable :: DirectConnectGatewayState
pattern DCGSAvailable = DirectConnectGatewayState' "available"

pattern DCGSDeleting :: DirectConnectGatewayState
pattern DCGSDeleting = DirectConnectGatewayState' "deleting"

pattern DCGSDeleted :: DirectConnectGatewayState
pattern DCGSDeleted = DirectConnectGatewayState' "deleted"

{-# COMPLETE
  DCGSPending,
  DCGSAvailable,
  DCGSDeleting,
  DCGSDeleted,
  DirectConnectGatewayState'
  #-}

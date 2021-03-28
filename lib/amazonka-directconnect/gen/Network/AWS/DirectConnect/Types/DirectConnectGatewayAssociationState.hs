{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
  ( DirectConnectGatewayAssociationState
    ( DirectConnectGatewayAssociationState'
    , DirectConnectGatewayAssociationStateAssociating
    , DirectConnectGatewayAssociationStateAssociated
    , DirectConnectGatewayAssociationStateDisassociating
    , DirectConnectGatewayAssociationStateDisassociated
    , DirectConnectGatewayAssociationStateUpdating
    , fromDirectConnectGatewayAssociationState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState'{fromDirectConnectGatewayAssociationState
                                                                                     :: Core.Text}
                                                 deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                 Core.Show, Core.Generic)
                                                 deriving newtype (Core.IsString, Core.Hashable,
                                                                   Core.NFData, Core.ToJSONKey,
                                                                   Core.FromJSONKey, Core.ToJSON,
                                                                   Core.FromJSON, Core.ToXML,
                                                                   Core.FromXML, Core.ToText,
                                                                   Core.FromText, Core.ToByteString,
                                                                   Core.ToQuery, Core.ToHeader)

pattern DirectConnectGatewayAssociationStateAssociating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationStateAssociating = DirectConnectGatewayAssociationState' "associating"

pattern DirectConnectGatewayAssociationStateAssociated :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationStateAssociated = DirectConnectGatewayAssociationState' "associated"

pattern DirectConnectGatewayAssociationStateDisassociating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationStateDisassociating = DirectConnectGatewayAssociationState' "disassociating"

pattern DirectConnectGatewayAssociationStateDisassociated :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationStateDisassociated = DirectConnectGatewayAssociationState' "disassociated"

pattern DirectConnectGatewayAssociationStateUpdating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationStateUpdating = DirectConnectGatewayAssociationState' "updating"

{-# COMPLETE 
  DirectConnectGatewayAssociationStateAssociating,

  DirectConnectGatewayAssociationStateAssociated,

  DirectConnectGatewayAssociationStateDisassociating,

  DirectConnectGatewayAssociationStateDisassociated,

  DirectConnectGatewayAssociationStateUpdating,
  DirectConnectGatewayAssociationState'
  #-}

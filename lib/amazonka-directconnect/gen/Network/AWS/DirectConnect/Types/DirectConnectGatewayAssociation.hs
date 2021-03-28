{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
  ( DirectConnectGatewayAssociation (..)
  -- * Smart constructor
  , mkDirectConnectGatewayAssociation
  -- * Lenses
  , dcgaAllowedPrefixesToDirectConnectGateway
  , dcgaAssociatedGateway
  , dcgaAssociationId
  , dcgaAssociationState
  , dcgaDirectConnectGatewayId
  , dcgaDirectConnectGatewayOwnerAccount
  , dcgaStateChangeError
  , dcgaVirtualGatewayId
  , dcgaVirtualGatewayOwnerAccount
  , dcgaVirtualGatewayRegion
  ) where

import qualified Network.AWS.DirectConnect.Types.AssociatedGateway as Types
import qualified Network.AWS.DirectConnect.Types.AssociationId as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayOwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.RouteFilterPrefix as Types
import qualified Network.AWS.DirectConnect.Types.StateChangeError as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayOwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an association between a Direct Connect gateway and a virtual private gateway or transit gateway.
--
-- /See:/ 'mkDirectConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { allowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The Amazon VPC prefixes to advertise to the Direct Connect gateway.
  , associatedGateway :: Core.Maybe Types.AssociatedGateway
    -- ^ Information about the associated gateway.
  , associationId :: Core.Maybe Types.AssociationId
    -- ^ The ID of the Direct Connect gateway association.
  , associationState :: Core.Maybe Types.DirectConnectGatewayAssociationState
    -- ^ The state of the association. The following are the possible values:
--
--
--     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
--
--
--     * @associated@ : The Direct Connect gateway and virtual private gateway or transit gateway are successfully associated and ready to pass traffic.
--
--
--     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
--
--
--     * @disassociated@ : The virtual private gateway or transit gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway or transit gateway is stopped.
--
--
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , directConnectGatewayOwnerAccount :: Core.Maybe Types.DirectConnectGatewayOwnerAccount
    -- ^ The ID of the AWS account that owns the associated gateway.
  , stateChangeError :: Core.Maybe Types.StateChangeError
    -- ^ The error message if the state of an object failed to advance.
  , virtualGatewayId :: Core.Maybe Types.VirtualGatewayId
    -- ^ The ID of the virtual private gateway. Applies only to private virtual interfaces.
  , virtualGatewayOwnerAccount :: Core.Maybe Types.VirtualGatewayOwnerAccount
    -- ^ The ID of the AWS account that owns the virtual private gateway.
  , virtualGatewayRegion :: Core.Maybe Types.VirtualGatewayRegion
    -- ^ The AWS Region where the virtual private gateway is located.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectConnectGatewayAssociation' value with any optional fields omitted.
mkDirectConnectGatewayAssociation
    :: DirectConnectGatewayAssociation
mkDirectConnectGatewayAssociation
  = DirectConnectGatewayAssociation'{allowedPrefixesToDirectConnectGateway
                                       = Core.Nothing,
                                     associatedGateway = Core.Nothing, associationId = Core.Nothing,
                                     associationState = Core.Nothing,
                                     directConnectGatewayId = Core.Nothing,
                                     directConnectGatewayOwnerAccount = Core.Nothing,
                                     stateChangeError = Core.Nothing,
                                     virtualGatewayId = Core.Nothing,
                                     virtualGatewayOwnerAccount = Core.Nothing,
                                     virtualGatewayRegion = Core.Nothing}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'allowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
dcgaAllowedPrefixesToDirectConnectGateway = Lens.field @"allowedPrefixesToDirectConnectGateway"
{-# INLINEABLE dcgaAllowedPrefixesToDirectConnectGateway #-}
{-# DEPRECATED allowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'allowedPrefixesToDirectConnectGateway' instead"  #-}

-- | Information about the associated gateway.
--
-- /Note:/ Consider using 'associatedGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociatedGateway :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.AssociatedGateway)
dcgaAssociatedGateway = Lens.field @"associatedGateway"
{-# INLINEABLE dcgaAssociatedGateway #-}
{-# DEPRECATED associatedGateway "Use generic-lens or generic-optics with 'associatedGateway' instead"  #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociationId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.AssociationId)
dcgaAssociationId = Lens.field @"associationId"
{-# INLINEABLE dcgaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The state of the association. The following are the possible values:
--
--
--     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
--
--
--     * @associated@ : The Direct Connect gateway and virtual private gateway or transit gateway are successfully associated and ready to pass traffic.
--
--
--     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
--
--
--     * @disassociated@ : The virtual private gateway or transit gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway or transit gateway is stopped.
--
--
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociationState :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.DirectConnectGatewayAssociationState)
dcgaAssociationState = Lens.field @"associationState"
{-# INLINEABLE dcgaAssociationState #-}
{-# DEPRECATED associationState "Use generic-lens or generic-optics with 'associationState' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaDirectConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.DirectConnectGatewayId)
dcgaDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE dcgaDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The ID of the AWS account that owns the associated gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaDirectConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.DirectConnectGatewayOwnerAccount)
dcgaDirectConnectGatewayOwnerAccount = Lens.field @"directConnectGatewayOwnerAccount"
{-# INLINEABLE dcgaDirectConnectGatewayOwnerAccount #-}
{-# DEPRECATED directConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead"  #-}

-- | The error message if the state of an object failed to advance.
--
-- /Note:/ Consider using 'stateChangeError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaStateChangeError :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.StateChangeError)
dcgaStateChangeError = Lens.field @"stateChangeError"
{-# INLINEABLE dcgaStateChangeError #-}
{-# DEPRECATED stateChangeError "Use generic-lens or generic-optics with 'stateChangeError' instead"  #-}

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.VirtualGatewayId)
dcgaVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# INLINEABLE dcgaVirtualGatewayId #-}
{-# DEPRECATED virtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead"  #-}

-- | The ID of the AWS account that owns the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.VirtualGatewayOwnerAccount)
dcgaVirtualGatewayOwnerAccount = Lens.field @"virtualGatewayOwnerAccount"
{-# INLINEABLE dcgaVirtualGatewayOwnerAccount #-}
{-# DEPRECATED virtualGatewayOwnerAccount "Use generic-lens or generic-optics with 'virtualGatewayOwnerAccount' instead"  #-}

-- | The AWS Region where the virtual private gateway is located.
--
-- /Note:/ Consider using 'virtualGatewayRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayRegion :: Lens.Lens' DirectConnectGatewayAssociation (Core.Maybe Types.VirtualGatewayRegion)
dcgaVirtualGatewayRegion = Lens.field @"virtualGatewayRegion"
{-# INLINEABLE dcgaVirtualGatewayRegion #-}
{-# DEPRECATED virtualGatewayRegion "Use generic-lens or generic-optics with 'virtualGatewayRegion' instead"  #-}

instance Core.FromJSON DirectConnectGatewayAssociation where
        parseJSON
          = Core.withObject "DirectConnectGatewayAssociation" Core.$
              \ x ->
                DirectConnectGatewayAssociation' Core.<$>
                  (x Core..:? "allowedPrefixesToDirectConnectGateway") Core.<*>
                    x Core..:? "associatedGateway"
                    Core.<*> x Core..:? "associationId"
                    Core.<*> x Core..:? "associationState"
                    Core.<*> x Core..:? "directConnectGatewayId"
                    Core.<*> x Core..:? "directConnectGatewayOwnerAccount"
                    Core.<*> x Core..:? "stateChangeError"
                    Core.<*> x Core..:? "virtualGatewayId"
                    Core.<*> x Core..:? "virtualGatewayOwnerAccount"
                    Core.<*> x Core..:? "virtualGatewayRegion"

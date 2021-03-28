{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a virtual interface with a specified link aggregation group (LAG) or connection. Connectivity to AWS is temporarily interrupted as the virtual interface is being migrated. If the target connection or LAG has an associated virtual interface with a conflicting VLAN number or a conflicting IP address, the operation fails.
--
-- Virtual interfaces associated with a hosted connection cannot be associated with a LAG; hosted connections must be migrated along with their virtual interfaces using 'AssociateHostedConnection' .
-- To reassociate a virtual interface to a new connection or LAG, the requester must own either the virtual interface itself or the connection to which the virtual interface is currently associated. Additionally, the requester must own the connection or LAG for the association.
module Network.AWS.DirectConnect.AssociateVirtualInterface
    (
    -- * Creating a request
      AssociateVirtualInterface (..)
    , mkAssociateVirtualInterface
    -- ** Request lenses
    , aviVirtualInterfaceId
    , aviConnectionId

     -- * Destructuring the response
    , Types.VirtualInterface (..)
    , Types.mkVirtualInterface
    -- ** Response lenses
    , Types.viAddressFamily
    , Types.viAmazonAddress
    , Types.viAmazonSideAsn
    , Types.viAsn
    , Types.viAuthKey
    , Types.viAwsDeviceV2
    , Types.viBgpPeers
    , Types.viConnectionId
    , Types.viCustomerAddress
    , Types.viCustomerRouterConfig
    , Types.viDirectConnectGatewayId
    , Types.viJumboFrameCapable
    , Types.viLocation
    , Types.viMtu
    , Types.viOwnerAccount
    , Types.viRegion
    , Types.viRouteFilterPrefixes
    , Types.viTags
    , Types.viVirtualGatewayId
    , Types.viVirtualInterfaceId
    , Types.viVirtualInterfaceName
    , Types.viVirtualInterfaceState
    , Types.viVirtualInterfaceType
    , Types.viVlan
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateVirtualInterface' smart constructor.
data AssociateVirtualInterface = AssociateVirtualInterface'
  { virtualInterfaceId :: Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface.
  , connectionId :: Types.ConnectionId
    -- ^ The ID of the LAG or connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateVirtualInterface' value with any optional fields omitted.
mkAssociateVirtualInterface
    :: Types.VirtualInterfaceId -- ^ 'virtualInterfaceId'
    -> Types.ConnectionId -- ^ 'connectionId'
    -> AssociateVirtualInterface
mkAssociateVirtualInterface virtualInterfaceId connectionId
  = AssociateVirtualInterface'{virtualInterfaceId, connectionId}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviVirtualInterfaceId :: Lens.Lens' AssociateVirtualInterface Types.VirtualInterfaceId
aviVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE aviVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

-- | The ID of the LAG or connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviConnectionId :: Lens.Lens' AssociateVirtualInterface Types.ConnectionId
aviConnectionId = Lens.field @"connectionId"
{-# INLINEABLE aviConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery AssociateVirtualInterface where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateVirtualInterface where
        toHeaders AssociateVirtualInterface{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.AssociateVirtualInterface")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateVirtualInterface where
        toJSON AssociateVirtualInterface{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("virtualInterfaceId" Core..= virtualInterfaceId),
                  Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest AssociateVirtualInterface where
        type Rs AssociateVirtualInterface = Types.VirtualInterface
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

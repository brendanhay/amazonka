{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateHostedConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.
module Network.AWS.DirectConnect.AssociateHostedConnection
    (
    -- * Creating a request
      AssociateHostedConnection (..)
    , mkAssociateHostedConnection
    -- ** Request lenses
    , ahcfConnectionId
    , ahcfParentConnectionId

     -- * Destructuring the response
    , Types.Connection (..)
    , Types.mkConnection
    -- ** Response lenses
    , Types.cAwsDevice
    , Types.cAwsDeviceV2
    , Types.cBandwidth
    , Types.cConnectionId
    , Types.cConnectionName
    , Types.cConnectionState
    , Types.cHasLogicalRedundancy
    , Types.cJumboFrameCapable
    , Types.cLagId
    , Types.cLoaIssueTime
    , Types.cLocation
    , Types.cOwnerAccount
    , Types.cPartnerName
    , Types.cProviderName
    , Types.cRegion
    , Types.cTags
    , Types.cVlan
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateHostedConnection' smart constructor.
data AssociateHostedConnection = AssociateHostedConnection'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the hosted connection.
  , parentConnectionId :: Types.ConnectionId
    -- ^ The ID of the interconnect or the LAG.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateHostedConnection' value with any optional fields omitted.
mkAssociateHostedConnection
    :: Types.ConnectionId -- ^ 'connectionId'
    -> Types.ConnectionId -- ^ 'parentConnectionId'
    -> AssociateHostedConnection
mkAssociateHostedConnection connectionId parentConnectionId
  = AssociateHostedConnection'{connectionId, parentConnectionId}

-- | The ID of the hosted connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcfConnectionId :: Lens.Lens' AssociateHostedConnection Types.ConnectionId
ahcfConnectionId = Lens.field @"connectionId"
{-# INLINEABLE ahcfConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The ID of the interconnect or the LAG.
--
-- /Note:/ Consider using 'parentConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcfParentConnectionId :: Lens.Lens' AssociateHostedConnection Types.ConnectionId
ahcfParentConnectionId = Lens.field @"parentConnectionId"
{-# INLINEABLE ahcfParentConnectionId #-}
{-# DEPRECATED parentConnectionId "Use generic-lens or generic-optics with 'parentConnectionId' instead"  #-}

instance Core.ToQuery AssociateHostedConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateHostedConnection where
        toHeaders AssociateHostedConnection{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.AssociateHostedConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateHostedConnection where
        toJSON AssociateHostedConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("connectionId" Core..= connectionId),
                  Core.Just ("parentConnectionId" Core..= parentConnectionId)])

instance Core.AWSRequest AssociateHostedConnection where
        type Rs AssociateHostedConnection = Types.Connection
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

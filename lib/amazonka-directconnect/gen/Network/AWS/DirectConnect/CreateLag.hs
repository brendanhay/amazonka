{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a link aggregation group (LAG) with the specified number of bundled physical dedicated connections between the customer network and a specific AWS Direct Connect location. A LAG is a logical interface that uses the Link Aggregation Control Protocol (LACP) to aggregate multiple interfaces, enabling you to treat them as a single interface.
--
-- All connections in a LAG must use the same bandwidth (either 1Gbps or 10Gbps) and must terminate at the same AWS Direct Connect endpoint.
-- You can have up to 10 dedicated connections per LAG. Regardless of this limit, if you request more connections for the LAG than AWS Direct Connect can allocate on a single endpoint, no LAG is created.
-- You can specify an existing physical dedicated connection or interconnect to include in the LAG (which counts towards the total number of connections). Doing so interrupts the current physical dedicated connection, and re-establishes them as a member of the LAG. The LAG will be created on the same AWS Direct Connect endpoint to which the dedicated connection terminates. Any virtual interfaces associated with the dedicated connection are automatically disassociated and re-associated with the LAG. The connection ID does not change.
-- If the AWS account used to create a LAG is a registered AWS Direct Connect Partner, the LAG is automatically enabled to host sub-connections. For a LAG owned by a partner, any associated virtual interfaces cannot be directly configured.
module Network.AWS.DirectConnect.CreateLag
    (
    -- * Creating a request
      CreateLag (..)
    , mkCreateLag
    -- ** Request lenses
    , clNumberOfConnections
    , clLocation
    , clConnectionsBandwidth
    , clLagName
    , clChildConnectionTags
    , clConnectionId
    , clProviderName
    , clTags

     -- * Destructuring the response
    , Types.Lag (..)
    , Types.mkLag
    -- ** Response lenses
    , Types.lfAllowsHostedConnections
    , Types.lfAwsDevice
    , Types.lfAwsDeviceV2
    , Types.lfConnections
    , Types.lfConnectionsBandwidth
    , Types.lfHasLogicalRedundancy
    , Types.lfJumboFrameCapable
    , Types.lfLagId
    , Types.lfLagName
    , Types.lfLagState
    , Types.lfLocation
    , Types.lfMinimumLinks
    , Types.lfNumberOfConnections
    , Types.lfOwnerAccount
    , Types.lfProviderName
    , Types.lfRegion
    , Types.lfTags
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLag' smart constructor.
data CreateLag = CreateLag'
  { numberOfConnections :: Core.Int
    -- ^ The number of physical dedicated connections initially provisioned and bundled by the LAG.
  , location :: Types.LocationCode
    -- ^ The location for the LAG.
  , connectionsBandwidth :: Types.Bandwidth
    -- ^ The bandwidth of the individual physical dedicated connections bundled by the LAG. The possible values are 1Gbps and 10Gbps. 
  , lagName :: Types.LagName
    -- ^ The name of the LAG.
  , childConnectionTags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags to associate with the automtically created LAGs.
  , connectionId :: Core.Maybe Types.ConnectionId
    -- ^ The ID of an existing dedicated connection to migrate to the LAG.
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The name of the service provider associated with the LAG.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags to associate with the LAG.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLag' value with any optional fields omitted.
mkCreateLag
    :: Core.Int -- ^ 'numberOfConnections'
    -> Types.LocationCode -- ^ 'location'
    -> Types.Bandwidth -- ^ 'connectionsBandwidth'
    -> Types.LagName -- ^ 'lagName'
    -> CreateLag
mkCreateLag numberOfConnections location connectionsBandwidth
  lagName
  = CreateLag'{numberOfConnections, location, connectionsBandwidth,
               lagName, childConnectionTags = Core.Nothing,
               connectionId = Core.Nothing, providerName = Core.Nothing,
               tags = Core.Nothing}

-- | The number of physical dedicated connections initially provisioned and bundled by the LAG.
--
-- /Note:/ Consider using 'numberOfConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clNumberOfConnections :: Lens.Lens' CreateLag Core.Int
clNumberOfConnections = Lens.field @"numberOfConnections"
{-# INLINEABLE clNumberOfConnections #-}
{-# DEPRECATED numberOfConnections "Use generic-lens or generic-optics with 'numberOfConnections' instead"  #-}

-- | The location for the LAG.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLocation :: Lens.Lens' CreateLag Types.LocationCode
clLocation = Lens.field @"location"
{-# INLINEABLE clLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The bandwidth of the individual physical dedicated connections bundled by the LAG. The possible values are 1Gbps and 10Gbps. 
--
-- /Note:/ Consider using 'connectionsBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnectionsBandwidth :: Lens.Lens' CreateLag Types.Bandwidth
clConnectionsBandwidth = Lens.field @"connectionsBandwidth"
{-# INLINEABLE clConnectionsBandwidth #-}
{-# DEPRECATED connectionsBandwidth "Use generic-lens or generic-optics with 'connectionsBandwidth' instead"  #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLagName :: Lens.Lens' CreateLag Types.LagName
clLagName = Lens.field @"lagName"
{-# INLINEABLE clLagName #-}
{-# DEPRECATED lagName "Use generic-lens or generic-optics with 'lagName' instead"  #-}

-- | The tags to associate with the automtically created LAGs.
--
-- /Note:/ Consider using 'childConnectionTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clChildConnectionTags :: Lens.Lens' CreateLag (Core.Maybe (Core.NonEmpty Types.Tag))
clChildConnectionTags = Lens.field @"childConnectionTags"
{-# INLINEABLE clChildConnectionTags #-}
{-# DEPRECATED childConnectionTags "Use generic-lens or generic-optics with 'childConnectionTags' instead"  #-}

-- | The ID of an existing dedicated connection to migrate to the LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clConnectionId :: Lens.Lens' CreateLag (Core.Maybe Types.ConnectionId)
clConnectionId = Lens.field @"connectionId"
{-# INLINEABLE clConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The name of the service provider associated with the LAG.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clProviderName :: Lens.Lens' CreateLag (Core.Maybe Types.ProviderName)
clProviderName = Lens.field @"providerName"
{-# INLINEABLE clProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | The tags to associate with the LAG.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clTags :: Lens.Lens' CreateLag (Core.Maybe (Core.NonEmpty Types.Tag))
clTags = Lens.field @"tags"
{-# INLINEABLE clTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateLag where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLag where
        toHeaders CreateLag{..}
          = Core.pure ("X-Amz-Target", "OvertureService.CreateLag") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLag where
        toJSON CreateLag{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("numberOfConnections" Core..= numberOfConnections),
                  Core.Just ("location" Core..= location),
                  Core.Just ("connectionsBandwidth" Core..= connectionsBandwidth),
                  Core.Just ("lagName" Core..= lagName),
                  ("childConnectionTags" Core..=) Core.<$> childConnectionTags,
                  ("connectionId" Core..=) Core.<$> connectionId,
                  ("providerName" Core..=) Core.<$> providerName,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateLag where
        type Rs CreateLag = Types.Lag
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

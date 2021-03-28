{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosted connections that have been provisioned on the specified interconnect or link aggregation group (LAG).
module Network.AWS.DirectConnect.DescribeHostedConnections
    (
    -- * Creating a request
      DescribeHostedConnections (..)
    , mkDescribeHostedConnections
    -- ** Request lenses
    , dhcConnectionId

     -- * Destructuring the response
    , Types.Connections (..)
    , Types.mkConnections
    -- ** Response lenses
    , Types.cConnections
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHostedConnections' smart constructor.
newtype DescribeHostedConnections = DescribeHostedConnections'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the interconnect or LAG.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHostedConnections' value with any optional fields omitted.
mkDescribeHostedConnections
    :: Types.ConnectionId -- ^ 'connectionId'
    -> DescribeHostedConnections
mkDescribeHostedConnections connectionId
  = DescribeHostedConnections'{connectionId}

-- | The ID of the interconnect or LAG.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcConnectionId :: Lens.Lens' DescribeHostedConnections Types.ConnectionId
dhcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE dhcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery DescribeHostedConnections where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHostedConnections where
        toHeaders DescribeHostedConnections{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.DescribeHostedConnections")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHostedConnections where
        toJSON DescribeHostedConnections{..}
          = Core.object
              (Core.catMaybes [Core.Just ("connectionId" Core..= connectionId)])

instance Core.AWSRequest DescribeHostedConnections where
        type Rs DescribeHostedConnections = Types.Connections
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

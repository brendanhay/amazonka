{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteLag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified link aggregation group (LAG). You cannot delete a LAG if it has active virtual interfaces or hosted connections.
module Network.AWS.DirectConnect.DeleteLag
    (
    -- * Creating a request
      DeleteLag (..)
    , mkDeleteLag
    -- ** Request lenses
    , dLagId

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

-- | /See:/ 'mkDeleteLag' smart constructor.
newtype DeleteLag = DeleteLag'
  { lagId :: Types.LagId
    -- ^ The ID of the LAG.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLag' value with any optional fields omitted.
mkDeleteLag
    :: Types.LagId -- ^ 'lagId'
    -> DeleteLag
mkDeleteLag lagId = DeleteLag'{lagId}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLagId :: Lens.Lens' DeleteLag Types.LagId
dLagId = Lens.field @"lagId"
{-# INLINEABLE dLagId #-}
{-# DEPRECATED lagId "Use generic-lens or generic-optics with 'lagId' instead"  #-}

instance Core.ToQuery DeleteLag where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLag where
        toHeaders DeleteLag{..}
          = Core.pure ("X-Amz-Target", "OvertureService.DeleteLag") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLag where
        toJSON DeleteLag{..}
          = Core.object (Core.catMaybes [Core.Just ("lagId" Core..= lagId)])

instance Core.AWSRequest DeleteLag where
        type Rs DeleteLag = Types.Lag
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}

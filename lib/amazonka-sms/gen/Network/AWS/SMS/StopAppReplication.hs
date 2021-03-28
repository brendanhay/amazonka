{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StopAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replicating the specified application by deleting the replication job for each server in the application.
module Network.AWS.SMS.StopAppReplication
    (
    -- * Creating a request
      StopAppReplication (..)
    , mkStopAppReplication
    -- ** Request lenses
    , sAppId

    -- * Destructuring the response
    , StopAppReplicationResponse (..)
    , mkStopAppReplicationResponse
    -- ** Response lenses
    , sarrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStopAppReplication' smart constructor.
newtype StopAppReplication = StopAppReplication'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAppReplication' value with any optional fields omitted.
mkStopAppReplication
    :: StopAppReplication
mkStopAppReplication = StopAppReplication'{appId = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAppId :: Lens.Lens' StopAppReplication (Core.Maybe Types.AppId)
sAppId = Lens.field @"appId"
{-# INLINEABLE sAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery StopAppReplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopAppReplication where
        toHeaders StopAppReplication{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.StopAppReplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopAppReplication where
        toJSON StopAppReplication{..}
          = Core.object (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.AWSRequest StopAppReplication where
        type Rs StopAppReplication = StopAppReplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopAppReplicationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopAppReplicationResponse' smart constructor.
newtype StopAppReplicationResponse = StopAppReplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAppReplicationResponse' value with any optional fields omitted.
mkStopAppReplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopAppReplicationResponse
mkStopAppReplicationResponse responseStatus
  = StopAppReplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsResponseStatus :: Lens.Lens' StopAppReplicationResponse Core.Int
sarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

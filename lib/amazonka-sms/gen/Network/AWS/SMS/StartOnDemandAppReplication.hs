{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Network.AWS.SMS.StartOnDemandAppReplication
    (
    -- * Creating a request
      StartOnDemandAppReplication (..)
    , mkStartOnDemandAppReplication
    -- ** Request lenses
    , sodarAppId
    , sodarDescription

    -- * Destructuring the response
    , StartOnDemandAppReplicationResponse (..)
    , mkStartOnDemandAppReplicationResponse
    -- ** Response lenses
    , sodarrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkStartOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { appId :: Types.AppId
    -- ^ The ID of the application.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the replication run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAppReplication' value with any optional fields omitted.
mkStartOnDemandAppReplication
    :: Types.AppId -- ^ 'appId'
    -> StartOnDemandAppReplication
mkStartOnDemandAppReplication appId
  = StartOnDemandAppReplication'{appId, description = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarAppId :: Lens.Lens' StartOnDemandAppReplication Types.AppId
sodarAppId = Lens.field @"appId"
{-# INLINEABLE sodarAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarDescription :: Lens.Lens' StartOnDemandAppReplication (Core.Maybe Types.Description)
sodarDescription = Lens.field @"description"
{-# INLINEABLE sodarDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery StartOnDemandAppReplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartOnDemandAppReplication where
        toHeaders StartOnDemandAppReplication{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartOnDemandAppReplication where
        toJSON StartOnDemandAppReplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("appId" Core..= appId),
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest StartOnDemandAppReplication where
        type Rs StartOnDemandAppReplication =
             StartOnDemandAppReplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartOnDemandAppReplicationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartOnDemandAppReplicationResponse' smart constructor.
newtype StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAppReplicationResponse' value with any optional fields omitted.
mkStartOnDemandAppReplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartOnDemandAppReplicationResponse
mkStartOnDemandAppReplicationResponse responseStatus
  = StartOnDemandAppReplicationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodarrrsResponseStatus :: Lens.Lens' StartOnDemandAppReplicationResponse Core.Int
sodarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sodarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

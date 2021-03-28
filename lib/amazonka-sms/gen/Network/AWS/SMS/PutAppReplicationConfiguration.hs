{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the replication configuration for the specified application.
module Network.AWS.SMS.PutAppReplicationConfiguration
    (
    -- * Creating a request
      PutAppReplicationConfiguration (..)
    , mkPutAppReplicationConfiguration
    -- ** Request lenses
    , parcAppId
    , parcServerGroupReplicationConfigurations

    -- * Destructuring the response
    , PutAppReplicationConfigurationResponse (..)
    , mkPutAppReplicationConfigurationResponse
    -- ** Response lenses
    , parcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkPutAppReplicationConfiguration' smart constructor.
data PutAppReplicationConfiguration = PutAppReplicationConfiguration'
  { appId :: Core.Maybe Types.AppId
    -- ^ The ID of the application.
  , serverGroupReplicationConfigurations :: Core.Maybe [Types.ServerGroupReplicationConfiguration]
    -- ^ Information about the replication configurations for server groups in the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutAppReplicationConfiguration' value with any optional fields omitted.
mkPutAppReplicationConfiguration
    :: PutAppReplicationConfiguration
mkPutAppReplicationConfiguration
  = PutAppReplicationConfiguration'{appId = Core.Nothing,
                                    serverGroupReplicationConfigurations = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcAppId :: Lens.Lens' PutAppReplicationConfiguration (Core.Maybe Types.AppId)
parcAppId = Lens.field @"appId"
{-# INLINEABLE parcAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | Information about the replication configurations for server groups in the application.
--
-- /Note:/ Consider using 'serverGroupReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcServerGroupReplicationConfigurations :: Lens.Lens' PutAppReplicationConfiguration (Core.Maybe [Types.ServerGroupReplicationConfiguration])
parcServerGroupReplicationConfigurations = Lens.field @"serverGroupReplicationConfigurations"
{-# INLINEABLE parcServerGroupReplicationConfigurations #-}
{-# DEPRECATED serverGroupReplicationConfigurations "Use generic-lens or generic-optics with 'serverGroupReplicationConfigurations' instead"  #-}

instance Core.ToQuery PutAppReplicationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutAppReplicationConfiguration where
        toHeaders PutAppReplicationConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.PutAppReplicationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutAppReplicationConfiguration where
        toJSON PutAppReplicationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("appId" Core..=) Core.<$> appId,
                  ("serverGroupReplicationConfigurations" Core..=) Core.<$>
                    serverGroupReplicationConfigurations])

instance Core.AWSRequest PutAppReplicationConfiguration where
        type Rs PutAppReplicationConfiguration =
             PutAppReplicationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutAppReplicationConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAppReplicationConfigurationResponse' smart constructor.
newtype PutAppReplicationConfigurationResponse = PutAppReplicationConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAppReplicationConfigurationResponse' value with any optional fields omitted.
mkPutAppReplicationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAppReplicationConfigurationResponse
mkPutAppReplicationConfigurationResponse responseStatus
  = PutAppReplicationConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parcrrsResponseStatus :: Lens.Lens' PutAppReplicationConfigurationResponse Core.Int
parcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE parcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

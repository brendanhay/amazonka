{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies lifecycle settings for an application.
module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
    (
    -- * Creating a request
      UpdateApplicationResourceLifecycle (..)
    , mkUpdateApplicationResourceLifecycle
    -- ** Request lenses
    , uarlApplicationName
    , uarlResourceLifecycleConfig

    -- * Destructuring the response
    , UpdateApplicationResourceLifecycleResponse (..)
    , mkUpdateApplicationResourceLifecycleResponse
    -- ** Response lenses
    , uarlrrsApplicationName
    , uarlrrsResourceLifecycleConfig
    , uarlrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application.
  , resourceLifecycleConfig :: Types.ApplicationResourceLifecycleConfig
    -- ^ The lifecycle configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResourceLifecycle' value with any optional fields omitted.
mkUpdateApplicationResourceLifecycle
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.ApplicationResourceLifecycleConfig -- ^ 'resourceLifecycleConfig'
    -> UpdateApplicationResourceLifecycle
mkUpdateApplicationResourceLifecycle applicationName
  resourceLifecycleConfig
  = UpdateApplicationResourceLifecycle'{applicationName,
                                        resourceLifecycleConfig}

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycle Types.ApplicationName
uarlApplicationName = Lens.field @"applicationName"
{-# INLINEABLE uarlApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycle Types.ApplicationResourceLifecycleConfig
uarlResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# INLINEABLE uarlResourceLifecycleConfig #-}
{-# DEPRECATED resourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead"  #-}

instance Core.ToQuery UpdateApplicationResourceLifecycle where
        toQuery UpdateApplicationResourceLifecycle{..}
          = Core.toQueryPair "Action"
              ("UpdateApplicationResourceLifecycle" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.toQueryPair "ResourceLifecycleConfig" resourceLifecycleConfig

instance Core.ToHeaders UpdateApplicationResourceLifecycle where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateApplicationResourceLifecycle where
        type Rs UpdateApplicationResourceLifecycle =
             UpdateApplicationResourceLifecycleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "UpdateApplicationResourceLifecycleResult"
              (\ s h x ->
                 UpdateApplicationResourceLifecycleResponse' Core.<$>
                   (x Core..@? "ApplicationName") Core.<*>
                     x Core..@? "ResourceLifecycleConfig"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of the application.
  , resourceLifecycleConfig :: Core.Maybe Types.ApplicationResourceLifecycleConfig
    -- ^ The lifecycle configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResourceLifecycleResponse' value with any optional fields omitted.
mkUpdateApplicationResourceLifecycleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateApplicationResourceLifecycleResponse
mkUpdateApplicationResourceLifecycleResponse responseStatus
  = UpdateApplicationResourceLifecycleResponse'{applicationName =
                                                  Core.Nothing,
                                                resourceLifecycleConfig = Core.Nothing,
                                                responseStatus}

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe Types.ApplicationName)
uarlrrsApplicationName = Lens.field @"applicationName"
{-# INLINEABLE uarlrrsApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe Types.ApplicationResourceLifecycleConfig)
uarlrrsResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# INLINEABLE uarlrrsResourceLifecycleConfig #-}
{-# DEPRECATED resourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsResponseStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Core.Int
uarlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uarlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

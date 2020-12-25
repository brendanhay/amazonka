{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateApplicationResourceLifecycle (..),
    mkUpdateApplicationResourceLifecycle,

    -- ** Request lenses
    uarlApplicationName,
    uarlResourceLifecycleConfig,

    -- * Destructuring the response
    UpdateApplicationResourceLifecycleResponse (..),
    mkUpdateApplicationResourceLifecycleResponse,

    -- ** Response lenses
    uarlrrsApplicationName,
    uarlrrsResourceLifecycleConfig,
    uarlrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { -- | The name of the application.
    applicationName :: Types.ApplicationName,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: Types.ApplicationResourceLifecycleConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResourceLifecycle' value with any optional fields omitted.
mkUpdateApplicationResourceLifecycle ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'resourceLifecycleConfig'
  Types.ApplicationResourceLifecycleConfig ->
  UpdateApplicationResourceLifecycle
mkUpdateApplicationResourceLifecycle
  applicationName
  resourceLifecycleConfig =
    UpdateApplicationResourceLifecycle'
      { applicationName,
        resourceLifecycleConfig
      }

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycle Types.ApplicationName
uarlApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uarlApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycle Types.ApplicationResourceLifecycleConfig
uarlResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# DEPRECATED uarlResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

instance Core.AWSRequest UpdateApplicationResourceLifecycle where
  type
    Rs UpdateApplicationResourceLifecycle =
      UpdateApplicationResourceLifecycleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateApplicationResourceLifecycle")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> ( Core.toQueryValue
                            "ResourceLifecycleConfig"
                            resourceLifecycleConfig
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateApplicationResourceLifecycleResult"
      ( \s h x ->
          UpdateApplicationResourceLifecycleResponse'
            Core.<$> (x Core..@? "ApplicationName")
            Core.<*> (x Core..@? "ResourceLifecycleConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { -- | The name of the application.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The lifecycle configuration.
    resourceLifecycleConfig :: Core.Maybe Types.ApplicationResourceLifecycleConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResourceLifecycleResponse' value with any optional fields omitted.
mkUpdateApplicationResourceLifecycleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateApplicationResourceLifecycleResponse
mkUpdateApplicationResourceLifecycleResponse responseStatus =
  UpdateApplicationResourceLifecycleResponse'
    { applicationName =
        Core.Nothing,
      resourceLifecycleConfig = Core.Nothing,
      responseStatus
    }

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsApplicationName :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe Types.ApplicationName)
uarlrrsApplicationName = Lens.field @"applicationName"
{-# DEPRECATED uarlrrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The lifecycle configuration.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsResourceLifecycleConfig :: Lens.Lens' UpdateApplicationResourceLifecycleResponse (Core.Maybe Types.ApplicationResourceLifecycleConfig)
uarlrrsResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# DEPRECATED uarlrrsResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarlrrsResponseStatus :: Lens.Lens' UpdateApplicationResourceLifecycleResponse Core.Int
uarlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uarlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

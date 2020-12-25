{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a running App for the specified UserProfile. Supported Apps are JupyterServer and KernelGateway. This operation is automatically invoked by Amazon SageMaker Studio upon access to the associated Domain, and when new kernel configurations are selected by the user. A user may have multiple Apps active simultaneously.
module Network.AWS.SageMaker.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caDomainId,
    caUserProfileName,
    caAppType,
    caAppName,
    caResourceSpec,
    caTags,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carrsAppArn,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The domain ID.
    domainId :: Types.DomainId,
    -- | The user profile name.
    userProfileName :: Types.UserProfileName,
    -- | The type of app.
    appType :: Types.AppType,
    -- | The name of the app.
    appName :: Types.AppName,
    -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
    resourceSpec :: Core.Maybe Types.ResourceSpec,
    -- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp ::
  -- | 'domainId'
  Types.DomainId ->
  -- | 'userProfileName'
  Types.UserProfileName ->
  -- | 'appType'
  Types.AppType ->
  -- | 'appName'
  Types.AppName ->
  CreateApp
mkCreateApp domainId userProfileName appType appName =
  CreateApp'
    { domainId,
      userProfileName,
      appType,
      appName,
      resourceSpec = Core.Nothing,
      tags = Core.Nothing
    }

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomainId :: Lens.Lens' CreateApp Types.DomainId
caDomainId = Lens.field @"domainId"
{-# DEPRECATED caDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caUserProfileName :: Lens.Lens' CreateApp Types.UserProfileName
caUserProfileName = Lens.field @"userProfileName"
{-# DEPRECATED caUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppType :: Lens.Lens' CreateApp Types.AppType
caAppType = Lens.field @"appType"
{-# DEPRECATED caAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppName :: Lens.Lens' CreateApp Types.AppName
caAppName = Lens.field @"appName"
{-# DEPRECATED caAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceSpec :: Lens.Lens' CreateApp (Core.Maybe Types.ResourceSpec)
caResourceSpec = Lens.field @"resourceSpec"
{-# DEPRECATED caResourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead." #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateApp where
  toJSON CreateApp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            Core.Just ("UserProfileName" Core..= userProfileName),
            Core.Just ("AppType" Core..= appType),
            Core.Just ("AppName" Core..= appName),
            ("ResourceSpec" Core..=) Core.<$> resourceSpec,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Core.<$> (x Core..:? "AppArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Core.Maybe Types.AppArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAppResponse
mkCreateAppResponse responseStatus =
  CreateAppResponse' {appArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAppArn :: Lens.Lens' CreateAppResponse (Core.Maybe Types.AppArn)
carrsAppArn = Lens.field @"appArn"
{-# DEPRECATED carrsAppArn "Use generic-lens or generic-optics with 'appArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

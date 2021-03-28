{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateApp (..)
    , mkCreateApp
    -- ** Request lenses
    , caDomainId
    , caUserProfileName
    , caAppType
    , caAppName
    , caResourceSpec
    , caTags

    -- * Destructuring the response
    , CreateAppResponse (..)
    , mkCreateAppResponse
    -- ** Response lenses
    , carrsAppArn
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The user profile name.
  , appType :: Types.AppType
    -- ^ The type of app.
  , appName :: Types.AppName
    -- ^ The name of the app.
  , resourceSpec :: Core.Maybe Types.ResourceSpec
    -- ^ The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Each tag consists of a key and an optional value. Tag keys must be unique per resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> Types.AppType -- ^ 'appType'
    -> Types.AppName -- ^ 'appName'
    -> CreateApp
mkCreateApp domainId userProfileName appType appName
  = CreateApp'{domainId, userProfileName, appType, appName,
               resourceSpec = Core.Nothing, tags = Core.Nothing}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomainId :: Lens.Lens' CreateApp Types.DomainId
caDomainId = Lens.field @"domainId"
{-# INLINEABLE caDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caUserProfileName :: Lens.Lens' CreateApp Types.UserProfileName
caUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE caUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppType :: Lens.Lens' CreateApp Types.AppType
caAppType = Lens.field @"appType"
{-# INLINEABLE caAppType #-}
{-# DEPRECATED appType "Use generic-lens or generic-optics with 'appType' instead"  #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppName :: Lens.Lens' CreateApp Types.AppName
caAppName = Lens.field @"appName"
{-# INLINEABLE caAppName #-}
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'appName' instead"  #-}

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceSpec :: Lens.Lens' CreateApp (Core.Maybe Types.ResourceSpec)
caResourceSpec = Lens.field @"resourceSpec"
{-# INLINEABLE caResourceSpec #-}
{-# DEPRECATED resourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead"  #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApp where
        toHeaders CreateApp{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateApp") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApp where
        toJSON CreateApp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  Core.Just ("AppType" Core..= appType),
                  Core.Just ("AppName" Core..= appName),
                  ("ResourceSpec" Core..=) Core.<$> resourceSpec,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateApp where
        type Rs CreateApp = CreateAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAppResponse' Core.<$>
                   (x Core..:? "AppArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { appArn :: Core.Maybe Types.AppArn
    -- ^ The Amazon Resource Name (ARN) of the app.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAppResponse
mkCreateAppResponse responseStatus
  = CreateAppResponse'{appArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAppArn :: Lens.Lens' CreateAppResponse (Core.Maybe Types.AppArn)
carrsAppArn = Lens.field @"appArn"
{-# INLINEABLE carrsAppArn #-}
{-# DEPRECATED appArn "Use generic-lens or generic-optics with 'appArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

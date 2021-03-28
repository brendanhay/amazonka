{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to stop and delete an app.
module Network.AWS.SageMaker.DeleteApp
    (
    -- * Creating a request
      DeleteApp (..)
    , mkDeleteApp
    -- ** Request lenses
    , dDomainId
    , dUserProfileName
    , dAppType
    , dAppName

    -- * Destructuring the response
    , DeleteAppResponse (..)
    , mkDeleteAppResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The user profile name.
  , appType :: Types.AppType
    -- ^ The type of app.
  , appName :: Types.AppName
    -- ^ The name of the app.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApp' value with any optional fields omitted.
mkDeleteApp
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> Types.AppType -- ^ 'appType'
    -> Types.AppName -- ^ 'appName'
    -> DeleteApp
mkDeleteApp domainId userProfileName appType appName
  = DeleteApp'{domainId, userProfileName, appType, appName}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainId :: Lens.Lens' DeleteApp Types.DomainId
dDomainId = Lens.field @"domainId"
{-# INLINEABLE dDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserProfileName :: Lens.Lens' DeleteApp Types.UserProfileName
dUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE dUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppType :: Lens.Lens' DeleteApp Types.AppType
dAppType = Lens.field @"appType"
{-# INLINEABLE dAppType #-}
{-# DEPRECATED appType "Use generic-lens or generic-optics with 'appType' instead"  #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppName :: Lens.Lens' DeleteApp Types.AppName
dAppName = Lens.field @"appName"
{-# INLINEABLE dAppName #-}
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'appName' instead"  #-}

instance Core.ToQuery DeleteApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApp where
        toHeaders DeleteApp{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteApp") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApp where
        toJSON DeleteApp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  Core.Just ("AppType" Core..= appType),
                  Core.Just ("AppName" Core..= appName)])

instance Core.AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAppResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppResponse' value with any optional fields omitted.
mkDeleteAppResponse
    :: DeleteAppResponse
mkDeleteAppResponse = DeleteAppResponse'

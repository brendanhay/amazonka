{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetDefaultAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.
module Network.AWS.IoT.SetDefaultAuthorizer
    (
    -- * Creating a request
      SetDefaultAuthorizer (..)
    , mkSetDefaultAuthorizer
    -- ** Request lenses
    , sdaAuthorizerName

    -- * Destructuring the response
    , SetDefaultAuthorizerResponse (..)
    , mkSetDefaultAuthorizerResponse
    -- ** Response lenses
    , sdarrsAuthorizerArn
    , sdarrsAuthorizerName
    , sdarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetDefaultAuthorizer' smart constructor.
newtype SetDefaultAuthorizer = SetDefaultAuthorizer'
  { authorizerName :: Types.AuthorizerName
    -- ^ The authorizer name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultAuthorizer' value with any optional fields omitted.
mkSetDefaultAuthorizer
    :: Types.AuthorizerName -- ^ 'authorizerName'
    -> SetDefaultAuthorizer
mkSetDefaultAuthorizer authorizerName
  = SetDefaultAuthorizer'{authorizerName}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdaAuthorizerName :: Lens.Lens' SetDefaultAuthorizer Types.AuthorizerName
sdaAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE sdaAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

instance Core.ToQuery SetDefaultAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetDefaultAuthorizer where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetDefaultAuthorizer where
        toJSON SetDefaultAuthorizer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("authorizerName" Core..= authorizerName)])

instance Core.AWSRequest SetDefaultAuthorizer where
        type Rs SetDefaultAuthorizer = SetDefaultAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/default-authorizer",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetDefaultAuthorizerResponse' Core.<$>
                   (x Core..:? "authorizerArn") Core.<*> x Core..:? "authorizerName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetDefaultAuthorizerResponse' smart constructor.
data SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse'
  { authorizerArn :: Core.Maybe Types.AuthorizerArn
    -- ^ The authorizer ARN.
  , authorizerName :: Core.Maybe Types.AuthorizerName
    -- ^ The authorizer name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultAuthorizerResponse' value with any optional fields omitted.
mkSetDefaultAuthorizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetDefaultAuthorizerResponse
mkSetDefaultAuthorizerResponse responseStatus
  = SetDefaultAuthorizerResponse'{authorizerArn = Core.Nothing,
                                  authorizerName = Core.Nothing, responseStatus}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarrsAuthorizerArn :: Lens.Lens' SetDefaultAuthorizerResponse (Core.Maybe Types.AuthorizerArn)
sdarrsAuthorizerArn = Lens.field @"authorizerArn"
{-# INLINEABLE sdarrsAuthorizerArn #-}
{-# DEPRECATED authorizerArn "Use generic-lens or generic-optics with 'authorizerArn' instead"  #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarrsAuthorizerName :: Lens.Lens' SetDefaultAuthorizerResponse (Core.Maybe Types.AuthorizerName)
sdarrsAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE sdarrsAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdarrsResponseStatus :: Lens.Lens' SetDefaultAuthorizerResponse Core.Int
sdarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

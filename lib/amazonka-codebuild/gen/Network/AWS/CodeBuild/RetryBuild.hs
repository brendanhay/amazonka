{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.RetryBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a build.
module Network.AWS.CodeBuild.RetryBuild
    (
    -- * Creating a request
      RetryBuild (..)
    , mkRetryBuild
    -- ** Request lenses
    , rbId
    , rbIdempotencyToken

    -- * Destructuring the response
    , RetryBuildResponse (..)
    , mkRetryBuildResponse
    -- ** Response lenses
    , rbrrsBuild
    , rbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRetryBuild' smart constructor.
data RetryBuild = RetryBuild'
  { id :: Core.Maybe Types.Id
    -- ^ Specifies the identifier of the build to restart.
  , idempotencyToken :: Core.Maybe Core.Text
    -- ^ A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryBuild' value with any optional fields omitted.
mkRetryBuild
    :: RetryBuild
mkRetryBuild
  = RetryBuild'{id = Core.Nothing, idempotencyToken = Core.Nothing}

-- | Specifies the identifier of the build to restart.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbId :: Lens.Lens' RetryBuild (Core.Maybe Types.Id)
rbId = Lens.field @"id"
{-# INLINEABLE rbId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @RetryBuild@ request. The token is included in the @RetryBuild@ request and is valid for five minutes. If you repeat the @RetryBuild@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbIdempotencyToken :: Lens.Lens' RetryBuild (Core.Maybe Core.Text)
rbIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE rbIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

instance Core.ToQuery RetryBuild where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RetryBuild where
        toHeaders RetryBuild{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.RetryBuild")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RetryBuild where
        toJSON RetryBuild{..}
          = Core.object
              (Core.catMaybes
                 [("id" Core..=) Core.<$> id,
                  ("idempotencyToken" Core..=) Core.<$> idempotencyToken])

instance Core.AWSRequest RetryBuild where
        type Rs RetryBuild = RetryBuildResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RetryBuildResponse' Core.<$>
                   (x Core..:? "build") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRetryBuildResponse' smart constructor.
data RetryBuildResponse = RetryBuildResponse'
  { build :: Core.Maybe Types.Build
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RetryBuildResponse' value with any optional fields omitted.
mkRetryBuildResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RetryBuildResponse
mkRetryBuildResponse responseStatus
  = RetryBuildResponse'{build = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsBuild :: Lens.Lens' RetryBuildResponse (Core.Maybe Types.Build)
rbrrsBuild = Lens.field @"build"
{-# INLINEABLE rbrrsBuild #-}
{-# DEPRECATED build "Use generic-lens or generic-optics with 'build' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsResponseStatus :: Lens.Lens' RetryBuildResponse Core.Int
rbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

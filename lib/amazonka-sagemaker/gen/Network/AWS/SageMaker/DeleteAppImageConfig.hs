{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppImageConfig.
module Network.AWS.SageMaker.DeleteAppImageConfig
    (
    -- * Creating a request
      DeleteAppImageConfig (..)
    , mkDeleteAppImageConfig
    -- ** Request lenses
    , daicAppImageConfigName

    -- * Destructuring the response
    , DeleteAppImageConfigResponse (..)
    , mkDeleteAppImageConfigResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteAppImageConfig' smart constructor.
newtype DeleteAppImageConfig = DeleteAppImageConfig'
  { appImageConfigName :: Types.AppImageConfigName
    -- ^ The name of the AppImageConfig to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppImageConfig' value with any optional fields omitted.
mkDeleteAppImageConfig
    :: Types.AppImageConfigName -- ^ 'appImageConfigName'
    -> DeleteAppImageConfig
mkDeleteAppImageConfig appImageConfigName
  = DeleteAppImageConfig'{appImageConfigName}

-- | The name of the AppImageConfig to delete.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicAppImageConfigName :: Lens.Lens' DeleteAppImageConfig Types.AppImageConfigName
daicAppImageConfigName = Lens.field @"appImageConfigName"
{-# INLINEABLE daicAppImageConfigName #-}
{-# DEPRECATED appImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead"  #-}

instance Core.ToQuery DeleteAppImageConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAppImageConfig where
        toHeaders DeleteAppImageConfig{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteAppImageConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAppImageConfig where
        toJSON DeleteAppImageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AppImageConfigName" Core..= appImageConfigName)])

instance Core.AWSRequest DeleteAppImageConfig where
        type Rs DeleteAppImageConfig = DeleteAppImageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAppImageConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppImageConfigResponse' smart constructor.
data DeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppImageConfigResponse' value with any optional fields omitted.
mkDeleteAppImageConfigResponse
    :: DeleteAppImageConfigResponse
mkDeleteAppImageConfigResponse = DeleteAppImageConfigResponse'

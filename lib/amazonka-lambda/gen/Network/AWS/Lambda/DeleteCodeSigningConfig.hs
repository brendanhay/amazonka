{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the code signing configuration. You can delete the code signing configuration only if no function is using it. 
module Network.AWS.Lambda.DeleteCodeSigningConfig
    (
    -- * Creating a request
      DeleteCodeSigningConfig (..)
    , mkDeleteCodeSigningConfig
    -- ** Request lenses
    , dcscCodeSigningConfigArn

    -- * Destructuring the response
    , DeleteCodeSigningConfigResponse (..)
    , mkDeleteCodeSigningConfigResponse
    -- ** Response lenses
    , dcscrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCodeSigningConfig' smart constructor.
newtype DeleteCodeSigningConfig = DeleteCodeSigningConfig'
  { codeSigningConfigArn :: Types.CodeSigningConfigArn
    -- ^ The The Amazon Resource Name (ARN) of the code signing configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCodeSigningConfig' value with any optional fields omitted.
mkDeleteCodeSigningConfig
    :: Types.CodeSigningConfigArn -- ^ 'codeSigningConfigArn'
    -> DeleteCodeSigningConfig
mkDeleteCodeSigningConfig codeSigningConfigArn
  = DeleteCodeSigningConfig'{codeSigningConfigArn}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscCodeSigningConfigArn :: Lens.Lens' DeleteCodeSigningConfig Types.CodeSigningConfigArn
dcscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# INLINEABLE dcscCodeSigningConfigArn #-}
{-# DEPRECATED codeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead"  #-}

instance Core.ToQuery DeleteCodeSigningConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCodeSigningConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCodeSigningConfig where
        type Rs DeleteCodeSigningConfig = DeleteCodeSigningConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2020-04-22/code-signing-configs/" Core.<>
                             Core.toText codeSigningConfigArn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCodeSigningConfigResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCodeSigningConfigResponse' smart constructor.
newtype DeleteCodeSigningConfigResponse = DeleteCodeSigningConfigResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCodeSigningConfigResponse' value with any optional fields omitted.
mkDeleteCodeSigningConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCodeSigningConfigResponse
mkDeleteCodeSigningConfigResponse responseStatus
  = DeleteCodeSigningConfigResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscrrsResponseStatus :: Lens.Lens' DeleteCodeSigningConfigResponse Core.Int
dcscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

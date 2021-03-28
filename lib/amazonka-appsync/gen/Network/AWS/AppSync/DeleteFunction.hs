{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Function@ .
module Network.AWS.AppSync.DeleteFunction
    (
    -- * Creating a request
      DeleteFunction (..)
    , mkDeleteFunction
    -- ** Request lenses
    , dfApiId
    , dfFunctionId

    -- * Destructuring the response
    , DeleteFunctionResponse (..)
    , mkDeleteFunctionResponse
    -- ** Response lenses
    , dfrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { apiId :: Core.Text
    -- ^ The GraphQL API ID.
  , functionId :: Types.ResourceName
    -- ^ The @Function@ ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunction' value with any optional fields omitted.
mkDeleteFunction
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'functionId'
    -> DeleteFunction
mkDeleteFunction apiId functionId
  = DeleteFunction'{apiId, functionId}

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfApiId :: Lens.Lens' DeleteFunction Core.Text
dfApiId = Lens.field @"apiId"
{-# INLINEABLE dfApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The @Function@ ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFunctionId :: Lens.Lens' DeleteFunction Types.ResourceName
dfFunctionId = Lens.field @"functionId"
{-# INLINEABLE dfFunctionId #-}
{-# DEPRECATED functionId "Use generic-lens or generic-optics with 'functionId' instead"  #-}

instance Core.ToQuery DeleteFunction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFunction where
        toHeaders DeleteFunction{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteFunction where
        type Rs DeleteFunction = DeleteFunctionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/functions/" Core.<>
                             Core.toText functionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteFunctionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFunctionResponse' smart constructor.
newtype DeleteFunctionResponse = DeleteFunctionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionResponse' value with any optional fields omitted.
mkDeleteFunctionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFunctionResponse
mkDeleteFunctionResponse responseStatus
  = DeleteFunctionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFunctionResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

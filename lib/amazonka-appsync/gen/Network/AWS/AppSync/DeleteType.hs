{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Type@ object.
module Network.AWS.AppSync.DeleteType
    (
    -- * Creating a request
      DeleteType (..)
    , mkDeleteType
    -- ** Request lenses
    , dtApiId
    , dtTypeName

    -- * Destructuring the response
    , DeleteTypeResponse (..)
    , mkDeleteTypeResponse
    -- ** Response lenses
    , dtrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteType' smart constructor.
data DeleteType = DeleteType'
  { apiId :: Core.Text
    -- ^ The API ID.
  , typeName :: Types.ResourceName
    -- ^ The type name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteType' value with any optional fields omitted.
mkDeleteType
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'typeName'
    -> DeleteType
mkDeleteType apiId typeName = DeleteType'{apiId, typeName}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtApiId :: Lens.Lens' DeleteType Core.Text
dtApiId = Lens.field @"apiId"
{-# INLINEABLE dtApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTypeName :: Lens.Lens' DeleteType Types.ResourceName
dtTypeName = Lens.field @"typeName"
{-# INLINEABLE dtTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.ToQuery DeleteType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteType where
        toHeaders DeleteType{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteType where
        type Rs DeleteType = DeleteTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types/" Core.<>
                             Core.toText typeName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTypeResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTypeResponse' smart constructor.
newtype DeleteTypeResponse = DeleteTypeResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTypeResponse' value with any optional fields omitted.
mkDeleteTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTypeResponse
mkDeleteTypeResponse responseStatus
  = DeleteTypeResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DeleteTypeResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

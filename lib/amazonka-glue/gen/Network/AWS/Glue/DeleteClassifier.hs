{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a classifier from the Data Catalog.
module Network.AWS.Glue.DeleteClassifier
    (
    -- * Creating a request
      DeleteClassifier (..)
    , mkDeleteClassifier
    -- ** Request lenses
    , dcfName

    -- * Destructuring the response
    , DeleteClassifierResponse (..)
    , mkDeleteClassifierResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteClassifier' smart constructor.
newtype DeleteClassifier = DeleteClassifier'
  { name :: Types.Name
    -- ^ Name of the classifier to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClassifier' value with any optional fields omitted.
mkDeleteClassifier
    :: Types.Name -- ^ 'name'
    -> DeleteClassifier
mkDeleteClassifier name = DeleteClassifier'{name}

-- | Name of the classifier to remove.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfName :: Lens.Lens' DeleteClassifier Types.Name
dcfName = Lens.field @"name"
{-# INLINEABLE dcfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteClassifier where
        toHeaders DeleteClassifier{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteClassifier") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteClassifier where
        toJSON DeleteClassifier{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteClassifier where
        type Rs DeleteClassifier = DeleteClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteClassifierResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClassifierResponse' smart constructor.
newtype DeleteClassifierResponse = DeleteClassifierResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClassifierResponse' value with any optional fields omitted.
mkDeleteClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClassifierResponse
mkDeleteClassifierResponse responseStatus
  = DeleteClassifierResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteClassifierResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

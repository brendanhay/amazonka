{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a classifier by name.
module Network.AWS.Glue.GetClassifier
    (
    -- * Creating a request
      GetClassifier (..)
    , mkGetClassifier
    -- ** Request lenses
    , gcfName

    -- * Destructuring the response
    , GetClassifierResponse (..)
    , mkGetClassifierResponse
    -- ** Response lenses
    , gcrrsClassifier
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetClassifier' smart constructor.
newtype GetClassifier = GetClassifier'
  { name :: Types.Name
    -- ^ Name of the classifier to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetClassifier' value with any optional fields omitted.
mkGetClassifier
    :: Types.Name -- ^ 'name'
    -> GetClassifier
mkGetClassifier name = GetClassifier'{name}

-- | Name of the classifier to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfName :: Lens.Lens' GetClassifier Types.Name
gcfName = Lens.field @"name"
{-# INLINEABLE gcfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetClassifier where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetClassifier where
        toHeaders GetClassifier{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetClassifier") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetClassifier where
        toJSON GetClassifier{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetClassifier where
        type Rs GetClassifier = GetClassifierResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetClassifierResponse' Core.<$>
                   (x Core..:? "Classifier") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetClassifierResponse' smart constructor.
data GetClassifierResponse = GetClassifierResponse'
  { classifier :: Core.Maybe Types.Classifier
    -- ^ The requested classifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetClassifierResponse' value with any optional fields omitted.
mkGetClassifierResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetClassifierResponse
mkGetClassifierResponse responseStatus
  = GetClassifierResponse'{classifier = Core.Nothing, responseStatus}

-- | The requested classifier.
--
-- /Note:/ Consider using 'classifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsClassifier :: Lens.Lens' GetClassifierResponse (Core.Maybe Types.Classifier)
gcrrsClassifier = Lens.field @"classifier"
{-# INLINEABLE gcrrsClassifier #-}
{-# DEPRECATED classifier "Use generic-lens or generic-optics with 'classifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetClassifierResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

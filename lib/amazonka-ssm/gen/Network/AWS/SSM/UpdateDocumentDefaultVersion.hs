{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateDocumentDefaultVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the default version of a document. 
module Network.AWS.SSM.UpdateDocumentDefaultVersion
    (
    -- * Creating a request
      UpdateDocumentDefaultVersion (..)
    , mkUpdateDocumentDefaultVersion
    -- ** Request lenses
    , uddvName
    , uddvDocumentVersion

    -- * Destructuring the response
    , UpdateDocumentDefaultVersionResponse (..)
    , mkUpdateDocumentDefaultVersionResponse
    -- ** Response lenses
    , uddvrrsDescription
    , uddvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateDocumentDefaultVersion' smart constructor.
data UpdateDocumentDefaultVersion = UpdateDocumentDefaultVersion'
  { name :: Types.DocumentName
    -- ^ The name of a custom document that you want to set as the default version.
  , documentVersion :: Types.DocumentVersionNumber
    -- ^ The version of a custom document that you want to set as the default version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentDefaultVersion' value with any optional fields omitted.
mkUpdateDocumentDefaultVersion
    :: Types.DocumentName -- ^ 'name'
    -> Types.DocumentVersionNumber -- ^ 'documentVersion'
    -> UpdateDocumentDefaultVersion
mkUpdateDocumentDefaultVersion name documentVersion
  = UpdateDocumentDefaultVersion'{name, documentVersion}

-- | The name of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvName :: Lens.Lens' UpdateDocumentDefaultVersion Types.DocumentName
uddvName = Lens.field @"name"
{-# INLINEABLE uddvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvDocumentVersion :: Lens.Lens' UpdateDocumentDefaultVersion Types.DocumentVersionNumber
uddvDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE uddvDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

instance Core.ToQuery UpdateDocumentDefaultVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDocumentDefaultVersion where
        toHeaders UpdateDocumentDefaultVersion{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.UpdateDocumentDefaultVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDocumentDefaultVersion where
        toJSON UpdateDocumentDefaultVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("DocumentVersion" Core..= documentVersion)])

instance Core.AWSRequest UpdateDocumentDefaultVersion where
        type Rs UpdateDocumentDefaultVersion =
             UpdateDocumentDefaultVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDocumentDefaultVersionResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDocumentDefaultVersionResponse' smart constructor.
data UpdateDocumentDefaultVersionResponse = UpdateDocumentDefaultVersionResponse'
  { description :: Core.Maybe Types.DocumentDefaultVersionDescription
    -- ^ The description of a custom document that you want to set as the default version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentDefaultVersionResponse' value with any optional fields omitted.
mkUpdateDocumentDefaultVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDocumentDefaultVersionResponse
mkUpdateDocumentDefaultVersionResponse responseStatus
  = UpdateDocumentDefaultVersionResponse'{description = Core.Nothing,
                                          responseStatus}

-- | The description of a custom document that you want to set as the default version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvrrsDescription :: Lens.Lens' UpdateDocumentDefaultVersionResponse (Core.Maybe Types.DocumentDefaultVersionDescription)
uddvrrsDescription = Lens.field @"description"
{-# INLINEABLE uddvrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddvrrsResponseStatus :: Lens.Lens' UpdateDocumentDefaultVersionResponse Core.Int
uddvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uddvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

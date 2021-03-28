{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
module Network.AWS.SSM.DeleteDocument
    (
    -- * Creating a request
      DeleteDocument (..)
    , mkDeleteDocument
    -- ** Request lenses
    , ddfName
    , ddfDocumentVersion
    , ddfForce
    , ddfVersionName

    -- * Destructuring the response
    , DeleteDocumentResponse (..)
    , mkDeleteDocumentResponse
    -- ** Response lenses
    , ddrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { name :: Types.DocumentName
    -- ^ The name of the document.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The version of the document that you want to delete. If not provided, all versions of the document are deleted.
  , force :: Core.Maybe Core.Bool
    -- ^ Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
  , versionName :: Core.Maybe Types.DocumentVersionName
    -- ^ The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocument' value with any optional fields omitted.
mkDeleteDocument
    :: Types.DocumentName -- ^ 'name'
    -> DeleteDocument
mkDeleteDocument name
  = DeleteDocument'{name, documentVersion = Core.Nothing,
                    force = Core.Nothing, versionName = Core.Nothing}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfName :: Lens.Lens' DeleteDocument Types.DocumentName
ddfName = Lens.field @"name"
{-# INLINEABLE ddfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDocumentVersion :: Lens.Lens' DeleteDocument (Core.Maybe Types.DocumentVersion)
ddfDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE ddfDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfForce :: Lens.Lens' DeleteDocument (Core.Maybe Core.Bool)
ddfForce = Lens.field @"force"
{-# INLINEABLE ddfForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

-- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfVersionName :: Lens.Lens' DeleteDocument (Core.Maybe Types.DocumentVersionName)
ddfVersionName = Lens.field @"versionName"
{-# INLINEABLE ddfVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

instance Core.ToQuery DeleteDocument where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDocument where
        toHeaders DeleteDocument{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteDocument") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDocument where
        toJSON DeleteDocument{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("DocumentVersion" Core..=) Core.<$> documentVersion,
                  ("Force" Core..=) Core.<$> force,
                  ("VersionName" Core..=) Core.<$> versionName])

instance Core.AWSRequest DeleteDocument where
        type Rs DeleteDocument = DeleteDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDocumentResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentResponse' value with any optional fields omitted.
mkDeleteDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDocumentResponse
mkDeleteDocumentResponse responseStatus
  = DeleteDocumentResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDocumentResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

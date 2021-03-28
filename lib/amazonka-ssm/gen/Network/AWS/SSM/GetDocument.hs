{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
module Network.AWS.SSM.GetDocument
    (
    -- * Creating a request
      GetDocument (..)
    , mkGetDocument
    -- ** Request lenses
    , gdName
    , gdDocumentFormat
    , gdDocumentVersion
    , gdVersionName

    -- * Destructuring the response
    , GetDocumentResponse (..)
    , mkGetDocumentResponse
    -- ** Response lenses
    , gdrrsAttachmentsContent
    , gdrrsContent
    , gdrrsDocumentFormat
    , gdrrsDocumentType
    , gdrrsDocumentVersion
    , gdrrsName
    , gdrrsRequires
    , gdrrsStatus
    , gdrrsStatusInformation
    , gdrrsVersionName
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { name :: Types.DocumentARN
    -- ^ The name of the Systems Manager document.
  , documentFormat :: Core.Maybe Types.DocumentFormat
    -- ^ Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The document version for which you want information.
  , versionName :: Core.Maybe Types.DocumentVersionName
    -- ^ An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocument' value with any optional fields omitted.
mkGetDocument
    :: Types.DocumentARN -- ^ 'name'
    -> GetDocument
mkGetDocument name
  = GetDocument'{name, documentFormat = Core.Nothing,
                 documentVersion = Core.Nothing, versionName = Core.Nothing}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdName :: Lens.Lens' GetDocument Types.DocumentARN
gdName = Lens.field @"name"
{-# INLINEABLE gdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentFormat :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentFormat)
gdDocumentFormat = Lens.field @"documentFormat"
{-# INLINEABLE gdDocumentFormat #-}
{-# DEPRECATED documentFormat "Use generic-lens or generic-optics with 'documentFormat' instead"  #-}

-- | The document version for which you want information.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentVersion :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentVersion)
gdDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE gdDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdVersionName :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentVersionName)
gdVersionName = Lens.field @"versionName"
{-# INLINEABLE gdVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

instance Core.ToQuery GetDocument where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDocument where
        toHeaders GetDocument{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetDocument") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDocument where
        toJSON GetDocument{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("DocumentFormat" Core..=) Core.<$> documentFormat,
                  ("DocumentVersion" Core..=) Core.<$> documentVersion,
                  ("VersionName" Core..=) Core.<$> versionName])

instance Core.AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDocumentResponse' Core.<$>
                   (x Core..:? "AttachmentsContent") Core.<*> x Core..:? "Content"
                     Core.<*> x Core..:? "DocumentFormat"
                     Core.<*> x Core..:? "DocumentType"
                     Core.<*> x Core..:? "DocumentVersion"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "Requires"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusInformation"
                     Core.<*> x Core..:? "VersionName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { attachmentsContent :: Core.Maybe [Types.AttachmentContent]
    -- ^ A description of the document attachments, including names, locations, sizes, and so on.
  , content :: Core.Maybe Types.Content
    -- ^ The contents of the Systems Manager document.
  , documentFormat :: Core.Maybe Types.DocumentFormat
    -- ^ The document format, either JSON or YAML.
  , documentType :: Core.Maybe Types.DocumentType
    -- ^ The document type.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The document version.
  , name :: Core.Maybe Types.DocumentARN
    -- ^ The name of the Systems Manager document.
  , requires :: Core.Maybe (Core.NonEmpty Types.DocumentRequires)
    -- ^ A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
  , status :: Core.Maybe Types.DocumentStatus
    -- ^ The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
  , statusInformation :: Core.Maybe Types.StatusInformation
    -- ^ A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
  , versionName :: Core.Maybe Types.DocumentVersionName
    -- ^ The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentResponse' value with any optional fields omitted.
mkGetDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDocumentResponse
mkGetDocumentResponse responseStatus
  = GetDocumentResponse'{attachmentsContent = Core.Nothing,
                         content = Core.Nothing, documentFormat = Core.Nothing,
                         documentType = Core.Nothing, documentVersion = Core.Nothing,
                         name = Core.Nothing, requires = Core.Nothing,
                         status = Core.Nothing, statusInformation = Core.Nothing,
                         versionName = Core.Nothing, responseStatus}

-- | A description of the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsAttachmentsContent :: Lens.Lens' GetDocumentResponse (Core.Maybe [Types.AttachmentContent])
gdrrsAttachmentsContent = Lens.field @"attachmentsContent"
{-# INLINEABLE gdrrsAttachmentsContent #-}
{-# DEPRECATED attachmentsContent "Use generic-lens or generic-optics with 'attachmentsContent' instead"  #-}

-- | The contents of the Systems Manager document.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsContent :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.Content)
gdrrsContent = Lens.field @"content"
{-# INLINEABLE gdrrsContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentFormat :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentFormat)
gdrrsDocumentFormat = Lens.field @"documentFormat"
{-# INLINEABLE gdrrsDocumentFormat #-}
{-# DEPRECATED documentFormat "Use generic-lens or generic-optics with 'documentFormat' instead"  #-}

-- | The document type.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentType :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentType)
gdrrsDocumentType = Lens.field @"documentType"
{-# INLINEABLE gdrrsDocumentType #-}
{-# DEPRECATED documentType "Use generic-lens or generic-optics with 'documentType' instead"  #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentVersion :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentVersion)
gdrrsDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE gdrrsDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsName :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentARN)
gdrrsName = Lens.field @"name"
{-# INLINEABLE gdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsRequires :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.NonEmpty Types.DocumentRequires))
gdrrsRequires = Lens.field @"requires"
{-# INLINEABLE gdrrsRequires #-}
{-# DEPRECATED requires "Use generic-lens or generic-optics with 'requires' instead"  #-}

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsStatus :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentStatus)
gdrrsStatus = Lens.field @"status"
{-# INLINEABLE gdrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsStatusInformation :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.StatusInformation)
gdrrsStatusInformation = Lens.field @"statusInformation"
{-# INLINEABLE gdrrsStatusInformation #-}
{-# DEPRECATED statusInformation "Use generic-lens or generic-optics with 'statusInformation' instead"  #-}

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsVersionName :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentVersionName)
gdrrsVersionName = Lens.field @"versionName"
{-# INLINEABLE gdrrsVersionName #-}
{-# DEPRECATED versionName "Use generic-lens or generic-optics with 'versionName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDocumentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

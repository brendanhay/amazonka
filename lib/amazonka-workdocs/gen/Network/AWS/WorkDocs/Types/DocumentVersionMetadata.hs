{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.DocumentVersionMetadata
  ( DocumentVersionMetadata (..)
  -- * Smart constructor
  , mkDocumentVersionMetadata
  -- * Lenses
  , dvmContentCreatedTimestamp
  , dvmContentModifiedTimestamp
  , dvmContentType
  , dvmCreatedTimestamp
  , dvmCreatorId
  , dvmId
  , dvmModifiedTimestamp
  , dvmName
  , dvmSignature
  , dvmSize
  , dvmSource
  , dvmStatus
  , dvmThumbnail
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.ContentType as Types
import qualified Network.AWS.WorkDocs.Types.DocumentSourceType as Types
import qualified Network.AWS.WorkDocs.Types.DocumentStatusType as Types
import qualified Network.AWS.WorkDocs.Types.DocumentThumbnailType as Types
import qualified Network.AWS.WorkDocs.Types.Id as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.Name as Types
import qualified Network.AWS.WorkDocs.Types.Signature as Types
import qualified Network.AWS.WorkDocs.Types.UrlType as Types

-- | Describes a version of a document.
--
-- /See:/ 'mkDocumentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { contentCreatedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the content of the document was originally created.
  , contentModifiedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the content of the document was modified.
  , contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the document.
  , createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the document was first uploaded.
  , creatorId :: Core.Maybe Types.IdType
    -- ^ The ID of the creator.
  , id :: Core.Maybe Types.Id
    -- ^ The ID of the version.
  , modifiedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the document was last uploaded.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the version.
  , signature :: Core.Maybe Types.Signature
    -- ^ The signature of the document.
  , size :: Core.Maybe Core.Integer
    -- ^ The size of the document, in bytes.
  , source :: Core.Maybe (Core.HashMap Types.DocumentSourceType Types.UrlType)
    -- ^ The source of the document.
  , status :: Core.Maybe Types.DocumentStatusType
    -- ^ The status of the document.
  , thumbnail :: Core.Maybe (Core.HashMap Types.DocumentThumbnailType Types.UrlType)
    -- ^ The thumbnail of the document.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DocumentVersionMetadata' value with any optional fields omitted.
mkDocumentVersionMetadata
    :: DocumentVersionMetadata
mkDocumentVersionMetadata
  = DocumentVersionMetadata'{contentCreatedTimestamp = Core.Nothing,
                             contentModifiedTimestamp = Core.Nothing,
                             contentType = Core.Nothing, createdTimestamp = Core.Nothing,
                             creatorId = Core.Nothing, id = Core.Nothing,
                             modifiedTimestamp = Core.Nothing, name = Core.Nothing,
                             signature = Core.Nothing, size = Core.Nothing,
                             source = Core.Nothing, status = Core.Nothing,
                             thumbnail = Core.Nothing}

-- | The timestamp when the content of the document was originally created.
--
-- /Note:/ Consider using 'contentCreatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.NominalDiffTime)
dvmContentCreatedTimestamp = Lens.field @"contentCreatedTimestamp"
{-# INLINEABLE dvmContentCreatedTimestamp #-}
{-# DEPRECATED contentCreatedTimestamp "Use generic-lens or generic-optics with 'contentCreatedTimestamp' instead"  #-}

-- | The timestamp when the content of the document was modified.
--
-- /Note:/ Consider using 'contentModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.NominalDiffTime)
dvmContentModifiedTimestamp = Lens.field @"contentModifiedTimestamp"
{-# INLINEABLE dvmContentModifiedTimestamp #-}
{-# DEPRECATED contentModifiedTimestamp "Use generic-lens or generic-optics with 'contentModifiedTimestamp' instead"  #-}

-- | The content type of the document.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentType :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.ContentType)
dvmContentType = Lens.field @"contentType"
{-# INLINEABLE dvmContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The timestamp when the document was first uploaded.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.NominalDiffTime)
dvmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE dvmCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The ID of the creator.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmCreatorId :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.IdType)
dvmCreatorId = Lens.field @"creatorId"
{-# INLINEABLE dvmCreatorId #-}
{-# DEPRECATED creatorId "Use generic-lens or generic-optics with 'creatorId' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmId :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.Id)
dvmId = Lens.field @"id"
{-# INLINEABLE dvmId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The timestamp when the document was last uploaded.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.NominalDiffTime)
dvmModifiedTimestamp = Lens.field @"modifiedTimestamp"
{-# INLINEABLE dvmModifiedTimestamp #-}
{-# DEPRECATED modifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead"  #-}

-- | The name of the version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmName :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.Name)
dvmName = Lens.field @"name"
{-# INLINEABLE dvmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The signature of the document.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSignature :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.Signature)
dvmSignature = Lens.field @"signature"
{-# INLINEABLE dvmSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | The size of the document, in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSize :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Core.Integer)
dvmSize = Lens.field @"size"
{-# INLINEABLE dvmSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | The source of the document.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSource :: Lens.Lens' DocumentVersionMetadata (Core.Maybe (Core.HashMap Types.DocumentSourceType Types.UrlType))
dvmSource = Lens.field @"source"
{-# INLINEABLE dvmSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The status of the document.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmStatus :: Lens.Lens' DocumentVersionMetadata (Core.Maybe Types.DocumentStatusType)
dvmStatus = Lens.field @"status"
{-# INLINEABLE dvmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The thumbnail of the document.
--
-- /Note:/ Consider using 'thumbnail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmThumbnail :: Lens.Lens' DocumentVersionMetadata (Core.Maybe (Core.HashMap Types.DocumentThumbnailType Types.UrlType))
dvmThumbnail = Lens.field @"thumbnail"
{-# INLINEABLE dvmThumbnail #-}
{-# DEPRECATED thumbnail "Use generic-lens or generic-optics with 'thumbnail' instead"  #-}

instance Core.FromJSON DocumentVersionMetadata where
        parseJSON
          = Core.withObject "DocumentVersionMetadata" Core.$
              \ x ->
                DocumentVersionMetadata' Core.<$>
                  (x Core..:? "ContentCreatedTimestamp") Core.<*>
                    x Core..:? "ContentModifiedTimestamp"
                    Core.<*> x Core..:? "ContentType"
                    Core.<*> x Core..:? "CreatedTimestamp"
                    Core.<*> x Core..:? "CreatorId"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "ModifiedTimestamp"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Signature"
                    Core.<*> x Core..:? "Size"
                    Core.<*> x Core..:? "Source"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "Thumbnail"

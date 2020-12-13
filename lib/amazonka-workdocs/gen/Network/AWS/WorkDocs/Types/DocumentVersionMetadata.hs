{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentVersionMetadata
  ( DocumentVersionMetadata (..),

    -- * Smart constructor
    mkDocumentVersionMetadata,

    -- * Lenses
    dvmThumbnail,
    dvmStatus,
    dvmSignature,
    dvmContentCreatedTimestamp,
    dvmSize,
    dvmName,
    dvmModifiedTimestamp,
    dvmSource,
    dvmId,
    dvmCreatedTimestamp,
    dvmContentModifiedTimestamp,
    dvmCreatorId,
    dvmContentType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.DocumentSourceType
import Network.AWS.WorkDocs.Types.DocumentStatusType
import Network.AWS.WorkDocs.Types.DocumentThumbnailType

-- | Describes a version of a document.
--
-- /See:/ 'mkDocumentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { -- | The thumbnail of the document.
    thumbnail :: Lude.Maybe (Lude.HashMap DocumentThumbnailType (Lude.Sensitive Lude.Text)),
    -- | The status of the document.
    status :: Lude.Maybe DocumentStatusType,
    -- | The signature of the document.
    signature :: Lude.Maybe Lude.Text,
    -- | The timestamp when the content of the document was originally created.
    contentCreatedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The size of the document, in bytes.
    size :: Lude.Maybe Lude.Integer,
    -- | The name of the version.
    name :: Lude.Maybe Lude.Text,
    -- | The timestamp when the document was last uploaded.
    modifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The source of the document.
    source :: Lude.Maybe (Lude.HashMap DocumentSourceType (Lude.Sensitive Lude.Text)),
    -- | The ID of the version.
    id :: Lude.Maybe Lude.Text,
    -- | The timestamp when the document was first uploaded.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The timestamp when the content of the document was modified.
    contentModifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the creator.
    creatorId :: Lude.Maybe Lude.Text,
    -- | The content type of the document.
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentVersionMetadata' with the minimum fields required to make a request.
--
-- * 'thumbnail' - The thumbnail of the document.
-- * 'status' - The status of the document.
-- * 'signature' - The signature of the document.
-- * 'contentCreatedTimestamp' - The timestamp when the content of the document was originally created.
-- * 'size' - The size of the document, in bytes.
-- * 'name' - The name of the version.
-- * 'modifiedTimestamp' - The timestamp when the document was last uploaded.
-- * 'source' - The source of the document.
-- * 'id' - The ID of the version.
-- * 'createdTimestamp' - The timestamp when the document was first uploaded.
-- * 'contentModifiedTimestamp' - The timestamp when the content of the document was modified.
-- * 'creatorId' - The ID of the creator.
-- * 'contentType' - The content type of the document.
mkDocumentVersionMetadata ::
  DocumentVersionMetadata
mkDocumentVersionMetadata =
  DocumentVersionMetadata'
    { thumbnail = Lude.Nothing,
      status = Lude.Nothing,
      signature = Lude.Nothing,
      contentCreatedTimestamp = Lude.Nothing,
      size = Lude.Nothing,
      name = Lude.Nothing,
      modifiedTimestamp = Lude.Nothing,
      source = Lude.Nothing,
      id = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      contentModifiedTimestamp = Lude.Nothing,
      creatorId = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | The thumbnail of the document.
--
-- /Note:/ Consider using 'thumbnail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmThumbnail :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe (Lude.HashMap DocumentThumbnailType (Lude.Sensitive Lude.Text)))
dvmThumbnail = Lens.lens (thumbnail :: DocumentVersionMetadata -> Lude.Maybe (Lude.HashMap DocumentThumbnailType (Lude.Sensitive Lude.Text))) (\s a -> s {thumbnail = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmThumbnail "Use generic-lens or generic-optics with 'thumbnail' instead." #-}

-- | The status of the document.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmStatus :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe DocumentStatusType)
dvmStatus = Lens.lens (status :: DocumentVersionMetadata -> Lude.Maybe DocumentStatusType) (\s a -> s {status = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The signature of the document.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSignature :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Text)
dvmSignature = Lens.lens (signature :: DocumentVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The timestamp when the content of the document was originally created.
--
-- /Note:/ Consider using 'contentCreatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Timestamp)
dvmContentCreatedTimestamp = Lens.lens (contentCreatedTimestamp :: DocumentVersionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {contentCreatedTimestamp = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmContentCreatedTimestamp "Use generic-lens or generic-optics with 'contentCreatedTimestamp' instead." #-}

-- | The size of the document, in bytes.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSize :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Integer)
dvmSize = Lens.lens (size :: DocumentVersionMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The name of the version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmName :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Text)
dvmName = Lens.lens (name :: DocumentVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The timestamp when the document was last uploaded.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Timestamp)
dvmModifiedTimestamp = Lens.lens (modifiedTimestamp :: DocumentVersionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimestamp = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The source of the document.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmSource :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe (Lude.HashMap DocumentSourceType (Lude.Sensitive Lude.Text)))
dvmSource = Lens.lens (source :: DocumentVersionMetadata -> Lude.Maybe (Lude.HashMap DocumentSourceType (Lude.Sensitive Lude.Text))) (\s a -> s {source = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmId :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Text)
dvmId = Lens.lens (id :: DocumentVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The timestamp when the document was first uploaded.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmCreatedTimestamp :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Timestamp)
dvmCreatedTimestamp = Lens.lens (createdTimestamp :: DocumentVersionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The timestamp when the content of the document was modified.
--
-- /Note:/ Consider using 'contentModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentModifiedTimestamp :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Timestamp)
dvmContentModifiedTimestamp = Lens.lens (contentModifiedTimestamp :: DocumentVersionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {contentModifiedTimestamp = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmContentModifiedTimestamp "Use generic-lens or generic-optics with 'contentModifiedTimestamp' instead." #-}

-- | The ID of the creator.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmCreatorId :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Text)
dvmCreatorId = Lens.lens (creatorId :: DocumentVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {creatorId = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

-- | The content type of the document.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmContentType :: Lens.Lens' DocumentVersionMetadata (Lude.Maybe Lude.Text)
dvmContentType = Lens.lens (contentType :: DocumentVersionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: DocumentVersionMetadata)
{-# DEPRECATED dvmContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON DocumentVersionMetadata where
  parseJSON =
    Lude.withObject
      "DocumentVersionMetadata"
      ( \x ->
          DocumentVersionMetadata'
            Lude.<$> (x Lude..:? "Thumbnail" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Signature")
            Lude.<*> (x Lude..:? "ContentCreatedTimestamp")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ModifiedTimestamp")
            Lude.<*> (x Lude..:? "Source" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "ContentModifiedTimestamp")
            Lude.<*> (x Lude..:? "CreatorId")
            Lude.<*> (x Lude..:? "ContentType")
      )

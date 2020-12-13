{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentMetadata
  ( DocumentMetadata (..),

    -- * Smart constructor
    mkDocumentMetadata,

    -- * Lenses
    dmLatestVersionMetadata,
    dmParentFolderId,
    dmModifiedTimestamp,
    dmId,
    dmLabels,
    dmResourceState,
    dmCreatedTimestamp,
    dmCreatorId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.DocumentVersionMetadata
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes the document.
--
-- /See:/ 'mkDocumentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { -- | The latest version of the document.
    latestVersionMetadata :: Lude.Maybe DocumentVersionMetadata,
    -- | The ID of the parent folder.
    parentFolderId :: Lude.Maybe Lude.Text,
    -- | The time when the document was updated.
    modifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the document.
    id :: Lude.Maybe Lude.Text,
    -- | List of labels on the document.
    labels :: Lude.Maybe [Lude.Text],
    -- | The resource state.
    resourceState :: Lude.Maybe ResourceStateType,
    -- | The time when the document was created.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the creator.
    creatorId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentMetadata' with the minimum fields required to make a request.
--
-- * 'latestVersionMetadata' - The latest version of the document.
-- * 'parentFolderId' - The ID of the parent folder.
-- * 'modifiedTimestamp' - The time when the document was updated.
-- * 'id' - The ID of the document.
-- * 'labels' - List of labels on the document.
-- * 'resourceState' - The resource state.
-- * 'createdTimestamp' - The time when the document was created.
-- * 'creatorId' - The ID of the creator.
mkDocumentMetadata ::
  DocumentMetadata
mkDocumentMetadata =
  DocumentMetadata'
    { latestVersionMetadata = Lude.Nothing,
      parentFolderId = Lude.Nothing,
      modifiedTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      labels = Lude.Nothing,
      resourceState = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      creatorId = Lude.Nothing
    }

-- | The latest version of the document.
--
-- /Note:/ Consider using 'latestVersionMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmLatestVersionMetadata :: Lens.Lens' DocumentMetadata (Lude.Maybe DocumentVersionMetadata)
dmLatestVersionMetadata = Lens.lens (latestVersionMetadata :: DocumentMetadata -> Lude.Maybe DocumentVersionMetadata) (\s a -> s {latestVersionMetadata = a} :: DocumentMetadata)
{-# DEPRECATED dmLatestVersionMetadata "Use generic-lens or generic-optics with 'latestVersionMetadata' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmParentFolderId :: Lens.Lens' DocumentMetadata (Lude.Maybe Lude.Text)
dmParentFolderId = Lens.lens (parentFolderId :: DocumentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {parentFolderId = a} :: DocumentMetadata)
{-# DEPRECATED dmParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | The time when the document was updated.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmModifiedTimestamp :: Lens.Lens' DocumentMetadata (Lude.Maybe Lude.Timestamp)
dmModifiedTimestamp = Lens.lens (modifiedTimestamp :: DocumentMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimestamp = a} :: DocumentMetadata)
{-# DEPRECATED dmModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmId :: Lens.Lens' DocumentMetadata (Lude.Maybe Lude.Text)
dmId = Lens.lens (id :: DocumentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DocumentMetadata)
{-# DEPRECATED dmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of labels on the document.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmLabels :: Lens.Lens' DocumentMetadata (Lude.Maybe [Lude.Text])
dmLabels = Lens.lens (labels :: DocumentMetadata -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: DocumentMetadata)
{-# DEPRECATED dmLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The resource state.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmResourceState :: Lens.Lens' DocumentMetadata (Lude.Maybe ResourceStateType)
dmResourceState = Lens.lens (resourceState :: DocumentMetadata -> Lude.Maybe ResourceStateType) (\s a -> s {resourceState = a} :: DocumentMetadata)
{-# DEPRECATED dmResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

-- | The time when the document was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmCreatedTimestamp :: Lens.Lens' DocumentMetadata (Lude.Maybe Lude.Timestamp)
dmCreatedTimestamp = Lens.lens (createdTimestamp :: DocumentMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: DocumentMetadata)
{-# DEPRECATED dmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the creator.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmCreatorId :: Lens.Lens' DocumentMetadata (Lude.Maybe Lude.Text)
dmCreatorId = Lens.lens (creatorId :: DocumentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {creatorId = a} :: DocumentMetadata)
{-# DEPRECATED dmCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

instance Lude.FromJSON DocumentMetadata where
  parseJSON =
    Lude.withObject
      "DocumentMetadata"
      ( \x ->
          DocumentMetadata'
            Lude.<$> (x Lude..:? "LatestVersionMetadata")
            Lude.<*> (x Lude..:? "ParentFolderId")
            Lude.<*> (x Lude..:? "ModifiedTimestamp")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Labels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceState")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "CreatorId")
      )

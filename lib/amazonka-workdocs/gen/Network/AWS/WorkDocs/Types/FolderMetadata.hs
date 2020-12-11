-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.FolderMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.FolderMetadata
  ( FolderMetadata (..),

    -- * Smart constructor
    mkFolderMetadata,

    -- * Lenses
    fmSignature,
    fmParentFolderId,
    fmSize,
    fmLatestVersionSize,
    fmName,
    fmModifiedTimestamp,
    fmId,
    fmLabels,
    fmResourceState,
    fmCreatedTimestamp,
    fmCreatorId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.ResourceStateType

-- | Describes a folder.
--
-- /See:/ 'mkFolderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { signature ::
      Lude.Maybe Lude.Text,
    parentFolderId :: Lude.Maybe Lude.Text,
    size :: Lude.Maybe Lude.Integer,
    latestVersionSize :: Lude.Maybe Lude.Integer,
    name :: Lude.Maybe Lude.Text,
    modifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    id :: Lude.Maybe Lude.Text,
    labels :: Lude.Maybe [Lude.Text],
    resourceState :: Lude.Maybe ResourceStateType,
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    creatorId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FolderMetadata' with the minimum fields required to make a request.
--
-- * 'createdTimestamp' - The time when the folder was created.
-- * 'creatorId' - The ID of the creator.
-- * 'id' - The ID of the folder.
-- * 'labels' - List of labels on the folder.
-- * 'latestVersionSize' - The size of the latest version of the folder metadata.
-- * 'modifiedTimestamp' - The time when the folder was updated.
-- * 'name' - The name of the folder.
-- * 'parentFolderId' - The ID of the parent folder.
-- * 'resourceState' - The resource state of the folder.
-- * 'signature' - The unique identifier created from the subfolders and documents of the folder.
-- * 'size' - The size of the folder metadata.
mkFolderMetadata ::
  FolderMetadata
mkFolderMetadata =
  FolderMetadata'
    { signature = Lude.Nothing,
      parentFolderId = Lude.Nothing,
      size = Lude.Nothing,
      latestVersionSize = Lude.Nothing,
      name = Lude.Nothing,
      modifiedTimestamp = Lude.Nothing,
      id = Lude.Nothing,
      labels = Lude.Nothing,
      resourceState = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      creatorId = Lude.Nothing
    }

-- | The unique identifier created from the subfolders and documents of the folder.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSignature :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Text)
fmSignature = Lens.lens (signature :: FolderMetadata -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: FolderMetadata)
{-# DEPRECATED fmSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmParentFolderId :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Text)
fmParentFolderId = Lens.lens (parentFolderId :: FolderMetadata -> Lude.Maybe Lude.Text) (\s a -> s {parentFolderId = a} :: FolderMetadata)
{-# DEPRECATED fmParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | The size of the folder metadata.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSize :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Integer)
fmSize = Lens.lens (size :: FolderMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: FolderMetadata)
{-# DEPRECATED fmSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The size of the latest version of the folder metadata.
--
-- /Note:/ Consider using 'latestVersionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmLatestVersionSize :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Integer)
fmLatestVersionSize = Lens.lens (latestVersionSize :: FolderMetadata -> Lude.Maybe Lude.Integer) (\s a -> s {latestVersionSize = a} :: FolderMetadata)
{-# DEPRECATED fmLatestVersionSize "Use generic-lens or generic-optics with 'latestVersionSize' instead." #-}

-- | The name of the folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmName :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Text)
fmName = Lens.lens (name :: FolderMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FolderMetadata)
{-# DEPRECATED fmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time when the folder was updated.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmModifiedTimestamp :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Timestamp)
fmModifiedTimestamp = Lens.lens (modifiedTimestamp :: FolderMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedTimestamp = a} :: FolderMetadata)
{-# DEPRECATED fmModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmId :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Text)
fmId = Lens.lens (id :: FolderMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: FolderMetadata)
{-# DEPRECATED fmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of labels on the folder.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmLabels :: Lens.Lens' FolderMetadata (Lude.Maybe [Lude.Text])
fmLabels = Lens.lens (labels :: FolderMetadata -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: FolderMetadata)
{-# DEPRECATED fmLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The resource state of the folder.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmResourceState :: Lens.Lens' FolderMetadata (Lude.Maybe ResourceStateType)
fmResourceState = Lens.lens (resourceState :: FolderMetadata -> Lude.Maybe ResourceStateType) (\s a -> s {resourceState = a} :: FolderMetadata)
{-# DEPRECATED fmResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

-- | The time when the folder was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmCreatedTimestamp :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Timestamp)
fmCreatedTimestamp = Lens.lens (createdTimestamp :: FolderMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: FolderMetadata)
{-# DEPRECATED fmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the creator.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmCreatorId :: Lens.Lens' FolderMetadata (Lude.Maybe Lude.Text)
fmCreatorId = Lens.lens (creatorId :: FolderMetadata -> Lude.Maybe Lude.Text) (\s a -> s {creatorId = a} :: FolderMetadata)
{-# DEPRECATED fmCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

instance Lude.FromJSON FolderMetadata where
  parseJSON =
    Lude.withObject
      "FolderMetadata"
      ( \x ->
          FolderMetadata'
            Lude.<$> (x Lude..:? "Signature")
            Lude.<*> (x Lude..:? "ParentFolderId")
            Lude.<*> (x Lude..:? "Size")
            Lude.<*> (x Lude..:? "LatestVersionSize")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ModifiedTimestamp")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Labels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceState")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "CreatorId")
      )

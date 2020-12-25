{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    fmCreatedTimestamp,
    fmCreatorId,
    fmId,
    fmLabels,
    fmLatestVersionSize,
    fmModifiedTimestamp,
    fmName,
    fmParentFolderId,
    fmResourceState,
    fmSignature,
    fmSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.Id as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.Name as Types
import qualified Network.AWS.WorkDocs.Types.ParentFolderId as Types
import qualified Network.AWS.WorkDocs.Types.ResourceStateType as Types
import qualified Network.AWS.WorkDocs.Types.SharedLabel as Types
import qualified Network.AWS.WorkDocs.Types.Signature as Types

-- | Describes a folder.
--
-- /See:/ 'mkFolderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { -- | The time when the folder was created.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the creator.
    creatorId :: Core.Maybe Types.IdType,
    -- | The ID of the folder.
    id :: Core.Maybe Types.Id,
    -- | List of labels on the folder.
    labels :: Core.Maybe [Types.SharedLabel],
    -- | The size of the latest version of the folder metadata.
    latestVersionSize :: Core.Maybe Core.Integer,
    -- | The time when the folder was updated.
    modifiedTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the folder.
    name :: Core.Maybe Types.Name,
    -- | The ID of the parent folder.
    parentFolderId :: Core.Maybe Types.ParentFolderId,
    -- | The resource state of the folder.
    resourceState :: Core.Maybe Types.ResourceStateType,
    -- | The unique identifier created from the subfolders and documents of the folder.
    signature :: Core.Maybe Types.Signature,
    -- | The size of the folder metadata.
    size :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FolderMetadata' value with any optional fields omitted.
mkFolderMetadata ::
  FolderMetadata
mkFolderMetadata =
  FolderMetadata'
    { createdTimestamp = Core.Nothing,
      creatorId = Core.Nothing,
      id = Core.Nothing,
      labels = Core.Nothing,
      latestVersionSize = Core.Nothing,
      modifiedTimestamp = Core.Nothing,
      name = Core.Nothing,
      parentFolderId = Core.Nothing,
      resourceState = Core.Nothing,
      signature = Core.Nothing,
      size = Core.Nothing
    }

-- | The time when the folder was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmCreatedTimestamp :: Lens.Lens' FolderMetadata (Core.Maybe Core.NominalDiffTime)
fmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED fmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the creator.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmCreatorId :: Lens.Lens' FolderMetadata (Core.Maybe Types.IdType)
fmCreatorId = Lens.field @"creatorId"
{-# DEPRECATED fmCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmId :: Lens.Lens' FolderMetadata (Core.Maybe Types.Id)
fmId = Lens.field @"id"
{-# DEPRECATED fmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | List of labels on the folder.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmLabels :: Lens.Lens' FolderMetadata (Core.Maybe [Types.SharedLabel])
fmLabels = Lens.field @"labels"
{-# DEPRECATED fmLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The size of the latest version of the folder metadata.
--
-- /Note:/ Consider using 'latestVersionSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmLatestVersionSize :: Lens.Lens' FolderMetadata (Core.Maybe Core.Integer)
fmLatestVersionSize = Lens.field @"latestVersionSize"
{-# DEPRECATED fmLatestVersionSize "Use generic-lens or generic-optics with 'latestVersionSize' instead." #-}

-- | The time when the folder was updated.
--
-- /Note:/ Consider using 'modifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmModifiedTimestamp :: Lens.Lens' FolderMetadata (Core.Maybe Core.NominalDiffTime)
fmModifiedTimestamp = Lens.field @"modifiedTimestamp"
{-# DEPRECATED fmModifiedTimestamp "Use generic-lens or generic-optics with 'modifiedTimestamp' instead." #-}

-- | The name of the folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmName :: Lens.Lens' FolderMetadata (Core.Maybe Types.Name)
fmName = Lens.field @"name"
{-# DEPRECATED fmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmParentFolderId :: Lens.Lens' FolderMetadata (Core.Maybe Types.ParentFolderId)
fmParentFolderId = Lens.field @"parentFolderId"
{-# DEPRECATED fmParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | The resource state of the folder.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmResourceState :: Lens.Lens' FolderMetadata (Core.Maybe Types.ResourceStateType)
fmResourceState = Lens.field @"resourceState"
{-# DEPRECATED fmResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

-- | The unique identifier created from the subfolders and documents of the folder.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSignature :: Lens.Lens' FolderMetadata (Core.Maybe Types.Signature)
fmSignature = Lens.field @"signature"
{-# DEPRECATED fmSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The size of the folder metadata.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSize :: Lens.Lens' FolderMetadata (Core.Maybe Core.Integer)
fmSize = Lens.field @"size"
{-# DEPRECATED fmSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromJSON FolderMetadata where
  parseJSON =
    Core.withObject "FolderMetadata" Core.$
      \x ->
        FolderMetadata'
          Core.<$> (x Core..:? "CreatedTimestamp")
          Core.<*> (x Core..:? "CreatorId")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Labels")
          Core.<*> (x Core..:? "LatestVersionSize")
          Core.<*> (x Core..:? "ModifiedTimestamp")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "ParentFolderId")
          Core.<*> (x Core..:? "ResourceState")
          Core.<*> (x Core..:? "Signature")
          Core.<*> (x Core..:? "Size")

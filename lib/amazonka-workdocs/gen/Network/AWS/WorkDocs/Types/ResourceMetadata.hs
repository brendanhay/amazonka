{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceMetadata
  ( ResourceMetadata (..),

    -- * Smart constructor
    mkResourceMetadata,

    -- * Lenses
    rmVersionId,
    rmOwner,
    rmName,
    rmId,
    rmType,
    rmOriginalName,
    rmParentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.ResourceType
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the metadata of a resource.
--
-- /See:/ 'mkResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { versionId ::
      Lude.Maybe Lude.Text,
    owner :: Lude.Maybe UserMetadata,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ResourceType,
    originalName :: Lude.Maybe Lude.Text,
    parentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceMetadata' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the resource.
-- * 'name' - The name of the resource.
-- * 'originalName' - The original name of the resource before a rename operation.
-- * 'owner' - The owner of the resource.
-- * 'parentId' - The parent ID of the resource before a rename operation.
-- * 'type'' - The type of resource.
-- * 'versionId' - The version ID of the resource. This is an optional field and is filled for action on document version.
mkResourceMetadata ::
  ResourceMetadata
mkResourceMetadata =
  ResourceMetadata'
    { versionId = Lude.Nothing,
      owner = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      originalName = Lude.Nothing,
      parentId = Lude.Nothing
    }

-- | The version ID of the resource. This is an optional field and is filled for action on document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmVersionId :: Lens.Lens' ResourceMetadata (Lude.Maybe Lude.Text)
rmVersionId = Lens.lens (versionId :: ResourceMetadata -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: ResourceMetadata)
{-# DEPRECATED rmVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The owner of the resource.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmOwner :: Lens.Lens' ResourceMetadata (Lude.Maybe UserMetadata)
rmOwner = Lens.lens (owner :: ResourceMetadata -> Lude.Maybe UserMetadata) (\s a -> s {owner = a} :: ResourceMetadata)
{-# DEPRECATED rmOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmName :: Lens.Lens' ResourceMetadata (Lude.Maybe Lude.Text)
rmName = Lens.lens (name :: ResourceMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceMetadata)
{-# DEPRECATED rmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmId :: Lens.Lens' ResourceMetadata (Lude.Maybe Lude.Text)
rmId = Lens.lens (id :: ResourceMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ResourceMetadata)
{-# DEPRECATED rmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmType :: Lens.Lens' ResourceMetadata (Lude.Maybe ResourceType)
rmType = Lens.lens (type' :: ResourceMetadata -> Lude.Maybe ResourceType) (\s a -> s {type' = a} :: ResourceMetadata)
{-# DEPRECATED rmType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The original name of the resource before a rename operation.
--
-- /Note:/ Consider using 'originalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmOriginalName :: Lens.Lens' ResourceMetadata (Lude.Maybe Lude.Text)
rmOriginalName = Lens.lens (originalName :: ResourceMetadata -> Lude.Maybe Lude.Text) (\s a -> s {originalName = a} :: ResourceMetadata)
{-# DEPRECATED rmOriginalName "Use generic-lens or generic-optics with 'originalName' instead." #-}

-- | The parent ID of the resource before a rename operation.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmParentId :: Lens.Lens' ResourceMetadata (Lude.Maybe Lude.Text)
rmParentId = Lens.lens (parentId :: ResourceMetadata -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: ResourceMetadata)
{-# DEPRECATED rmParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Lude.FromJSON ResourceMetadata where
  parseJSON =
    Lude.withObject
      "ResourceMetadata"
      ( \x ->
          ResourceMetadata'
            Lude.<$> (x Lude..:? "VersionId")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "OriginalName")
            Lude.<*> (x Lude..:? "ParentId")
      )

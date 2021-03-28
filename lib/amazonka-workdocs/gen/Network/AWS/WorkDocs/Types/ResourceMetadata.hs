{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.ResourceMetadata
  ( ResourceMetadata (..)
  -- * Smart constructor
  , mkResourceMetadata
  -- * Lenses
  , rmId
  , rmName
  , rmOriginalName
  , rmOwner
  , rmParentId
  , rmType
  , rmVersionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.Id as Types
import qualified Network.AWS.WorkDocs.Types.Name as Types
import qualified Network.AWS.WorkDocs.Types.OriginalName as Types
import qualified Network.AWS.WorkDocs.Types.ParentId as Types
import qualified Network.AWS.WorkDocs.Types.ResourceType as Types
import qualified Network.AWS.WorkDocs.Types.UserMetadata as Types
import qualified Network.AWS.WorkDocs.Types.VersionId as Types

-- | Describes the metadata of a resource.
--
-- /See:/ 'mkResourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { id :: Core.Maybe Types.Id
    -- ^ The ID of the resource.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the resource.
  , originalName :: Core.Maybe Types.OriginalName
    -- ^ The original name of the resource before a rename operation.
  , owner :: Core.Maybe Types.UserMetadata
    -- ^ The owner of the resource.
  , parentId :: Core.Maybe Types.ParentId
    -- ^ The parent ID of the resource before a rename operation.
  , type' :: Core.Maybe Types.ResourceType
    -- ^ The type of resource.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ The version ID of the resource. This is an optional field and is filled for action on document version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceMetadata' value with any optional fields omitted.
mkResourceMetadata
    :: ResourceMetadata
mkResourceMetadata
  = ResourceMetadata'{id = Core.Nothing, name = Core.Nothing,
                      originalName = Core.Nothing, owner = Core.Nothing,
                      parentId = Core.Nothing, type' = Core.Nothing,
                      versionId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmId :: Lens.Lens' ResourceMetadata (Core.Maybe Types.Id)
rmId = Lens.field @"id"
{-# INLINEABLE rmId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmName :: Lens.Lens' ResourceMetadata (Core.Maybe Types.Name)
rmName = Lens.field @"name"
{-# INLINEABLE rmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The original name of the resource before a rename operation.
--
-- /Note:/ Consider using 'originalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmOriginalName :: Lens.Lens' ResourceMetadata (Core.Maybe Types.OriginalName)
rmOriginalName = Lens.field @"originalName"
{-# INLINEABLE rmOriginalName #-}
{-# DEPRECATED originalName "Use generic-lens or generic-optics with 'originalName' instead"  #-}

-- | The owner of the resource.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmOwner :: Lens.Lens' ResourceMetadata (Core.Maybe Types.UserMetadata)
rmOwner = Lens.field @"owner"
{-# INLINEABLE rmOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The parent ID of the resource before a rename operation.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmParentId :: Lens.Lens' ResourceMetadata (Core.Maybe Types.ParentId)
rmParentId = Lens.field @"parentId"
{-# INLINEABLE rmParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmType :: Lens.Lens' ResourceMetadata (Core.Maybe Types.ResourceType)
rmType = Lens.field @"type'"
{-# INLINEABLE rmType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The version ID of the resource. This is an optional field and is filled for action on document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmVersionId :: Lens.Lens' ResourceMetadata (Core.Maybe Types.VersionId)
rmVersionId = Lens.field @"versionId"
{-# INLINEABLE rmVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromJSON ResourceMetadata where
        parseJSON
          = Core.withObject "ResourceMetadata" Core.$
              \ x ->
                ResourceMetadata' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "OriginalName"
                    Core.<*> x Core..:? "Owner"
                    Core.<*> x Core..:? "ParentId"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "VersionId"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Relationship
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Relationship
  ( Relationship (..),

    -- * Smart constructor
    mkRelationship,

    -- * Lenses
    rRelationshipName,
    rResourceId,
    rResourceName,
    rResourceType,
  )
where

import qualified Network.AWS.Config.Types.RelationshipName as Types
import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'mkRelationship' smart constructor.
data Relationship = Relationship'
  { -- | The type of relationship with the related resource.
    relationshipName :: Core.Maybe Types.RelationshipName,
    -- | The ID of the related resource (for example, @sg-xxxxxx@ ).
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The custom name of the related resource, if available.
    resourceName :: Core.Maybe Types.ResourceName,
    -- | The resource type of the related resource.
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Relationship' value with any optional fields omitted.
mkRelationship ::
  Relationship
mkRelationship =
  Relationship'
    { relationshipName = Core.Nothing,
      resourceId = Core.Nothing,
      resourceName = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The type of relationship with the related resource.
--
-- /Note:/ Consider using 'relationshipName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRelationshipName :: Lens.Lens' Relationship (Core.Maybe Types.RelationshipName)
rRelationshipName = Lens.field @"relationshipName"
{-# DEPRECATED rRelationshipName "Use generic-lens or generic-optics with 'relationshipName' instead." #-}

-- | The ID of the related resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceId :: Lens.Lens' Relationship (Core.Maybe Types.ResourceId)
rResourceId = Lens.field @"resourceId"
{-# DEPRECATED rResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The custom name of the related resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceName :: Lens.Lens' Relationship (Core.Maybe Types.ResourceName)
rResourceName = Lens.field @"resourceName"
{-# DEPRECATED rResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The resource type of the related resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' Relationship (Core.Maybe Types.ResourceType)
rResourceType = Lens.field @"resourceType"
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON Relationship where
  parseJSON =
    Core.withObject "Relationship" Core.$
      \x ->
        Relationship'
          Core.<$> (x Core..:? "relationshipName")
          Core.<*> (x Core..:? "resourceId")
          Core.<*> (x Core..:? "resourceName")
          Core.<*> (x Core..:? "resourceType")

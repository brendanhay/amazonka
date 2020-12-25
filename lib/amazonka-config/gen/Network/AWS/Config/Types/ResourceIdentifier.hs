{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceIdentifier
  ( ResourceIdentifier (..),

    -- * Smart constructor
    mkResourceIdentifier,

    -- * Lenses
    riResourceDeletionTime,
    riResourceId,
    riResourceName,
    riResourceType,
  )
where

import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
-- /See:/ 'mkResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The time that the resource was deleted.
    resourceDeletionTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the resource (for example, @sg-xxxxxx@ ).
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The custom name of the resource (if available).
    resourceName :: Core.Maybe Types.ResourceName,
    -- | The type of resource.
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResourceIdentifier' value with any optional fields omitted.
mkResourceIdentifier ::
  ResourceIdentifier
mkResourceIdentifier =
  ResourceIdentifier'
    { resourceDeletionTime = Core.Nothing,
      resourceId = Core.Nothing,
      resourceName = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The time that the resource was deleted.
--
-- /Note:/ Consider using 'resourceDeletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceDeletionTime :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.NominalDiffTime)
riResourceDeletionTime = Lens.field @"resourceDeletionTime"
{-# DEPRECATED riResourceDeletionTime "Use generic-lens or generic-optics with 'resourceDeletionTime' instead." #-}

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceId :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ResourceId)
riResourceId = Lens.field @"resourceId"
{-# DEPRECATED riResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The custom name of the resource (if available).
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceName :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ResourceName)
riResourceName = Lens.field @"resourceName"
{-# DEPRECATED riResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riResourceType :: Lens.Lens' ResourceIdentifier (Core.Maybe Types.ResourceType)
riResourceType = Lens.field @"resourceType"
{-# DEPRECATED riResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON ResourceIdentifier where
  parseJSON =
    Core.withObject "ResourceIdentifier" Core.$
      \x ->
        ResourceIdentifier'
          Core.<$> (x Core..:? "resourceDeletionTime")
          Core.<*> (x Core..:? "resourceId")
          Core.<*> (x Core..:? "resourceName")
          Core.<*> (x Core..:? "resourceType")

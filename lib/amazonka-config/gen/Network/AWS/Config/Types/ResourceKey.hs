{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceKey
  ( ResourceKey (..),

    -- * Smart constructor
    mkResourceKey,

    -- * Lenses
    rkResourceType,
    rkResourceId,
  )
where

import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
-- /See:/ 'mkResourceKey' smart constructor.
data ResourceKey = ResourceKey'
  { -- | The resource type.
    resourceType :: Types.ResourceType,
    -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceKey' value with any optional fields omitted.
mkResourceKey ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'resourceId'
  Types.ResourceId ->
  ResourceKey
mkResourceKey resourceType resourceId =
  ResourceKey' {resourceType, resourceId}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rkResourceType :: Lens.Lens' ResourceKey Types.ResourceType
rkResourceType = Lens.field @"resourceType"
{-# DEPRECATED rkResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rkResourceId :: Lens.Lens' ResourceKey Types.ResourceId
rkResourceId = Lens.field @"resourceId"
{-# DEPRECATED rkResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Core.FromJSON ResourceKey where
  toJSON ResourceKey {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("resourceId" Core..= resourceId)
          ]
      )

instance Core.FromJSON ResourceKey where
  parseJSON =
    Core.withObject "ResourceKey" Core.$
      \x ->
        ResourceKey'
          Core.<$> (x Core..: "resourceType") Core.<*> (x Core..: "resourceId")

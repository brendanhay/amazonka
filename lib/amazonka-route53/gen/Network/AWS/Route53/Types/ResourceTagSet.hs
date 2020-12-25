{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceTagSet
  ( ResourceTagSet (..),

    -- * Smart constructor
    mkResourceTagSet,

    -- * Lenses
    rtsResourceId,
    rtsResourceType,
    rtsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Tag as Types
import qualified Network.AWS.Route53.Types.TagResourceId as Types
import qualified Network.AWS.Route53.Types.TagResourceType as Types

-- | A complex type containing a resource and its associated tags.
--
-- /See:/ 'mkResourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
  { -- | The ID for the specified resource.
    resourceId :: Core.Maybe Types.TagResourceId,
    -- | The type of the resource.
    --
    --
    --     * The resource type for health checks is @healthcheck@ .
    --
    --
    --     * The resource type for hosted zones is @hostedzone@ .
    resourceType :: Core.Maybe Types.TagResourceType,
    -- | The tags associated with the specified resource.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTagSet' value with any optional fields omitted.
mkResourceTagSet ::
  ResourceTagSet
mkResourceTagSet =
  ResourceTagSet'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID for the specified resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsResourceId :: Lens.Lens' ResourceTagSet (Core.Maybe Types.TagResourceId)
rtsResourceId = Lens.field @"resourceId"
{-# DEPRECATED rtsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of the resource.
--
--
--     * The resource type for health checks is @healthcheck@ .
--
--
--     * The resource type for hosted zones is @hostedzone@ .
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsResourceType :: Lens.Lens' ResourceTagSet (Core.Maybe Types.TagResourceType)
rtsResourceType = Lens.field @"resourceType"
{-# DEPRECATED rtsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags associated with the specified resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtsTags :: Lens.Lens' ResourceTagSet (Core.Maybe (Core.NonEmpty Types.Tag))
rtsTags = Lens.field @"tags"
{-# DEPRECATED rtsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ResourceTagSet where
  parseXML x =
    ResourceTagSet'
      Core.<$> (x Core..@? "ResourceId")
      Core.<*> (x Core..@? "ResourceType")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLNonEmpty "Tag")

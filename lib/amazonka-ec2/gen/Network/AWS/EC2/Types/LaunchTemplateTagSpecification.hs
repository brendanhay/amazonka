{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecification
  ( LaunchTemplateTagSpecification (..),

    -- * Smart constructor
    mkLaunchTemplateTagSpecification,

    -- * Lenses
    lttsResourceType,
    lttsTags,
  )
where

import qualified Network.AWS.EC2.Types.ResourceType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tag specification for the launch template.
--
-- /See:/ 'mkLaunchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { -- | The type of resource.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The tags for the resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateTagSpecification' value with any optional fields omitted.
mkLaunchTemplateTagSpecification ::
  LaunchTemplateTagSpecification
mkLaunchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    { resourceType = Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsResourceType :: Lens.Lens' LaunchTemplateTagSpecification (Core.Maybe Types.ResourceType)
lttsResourceType = Lens.field @"resourceType"
{-# DEPRECATED lttsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags for the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsTags :: Lens.Lens' LaunchTemplateTagSpecification (Core.Maybe [Types.Tag])
lttsTags = Lens.field @"tags"
{-# DEPRECATED lttsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML LaunchTemplateTagSpecification where
  parseXML x =
    LaunchTemplateTagSpecification'
      Core.<$> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")

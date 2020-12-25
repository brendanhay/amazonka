{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
  ( LaunchTemplateTagSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateTagSpecificationRequest,

    -- * Lenses
    lttsrResourceType,
    lttsrTags,
  )
where

import qualified Network.AWS.EC2.Types.ResourceType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tags specification for the launch template.
--
-- /See:/ 'mkLaunchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { -- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The tags to apply to the resource.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateTagSpecificationRequest' value with any optional fields omitted.
mkLaunchTemplateTagSpecificationRequest ::
  LaunchTemplateTagSpecificationRequest
mkLaunchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    { resourceType =
        Core.Nothing,
      tags = Core.Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsrResourceType :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Core.Maybe Types.ResourceType)
lttsrResourceType = Lens.field @"resourceType"
{-# DEPRECATED lttsrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tags to apply to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttsrTags :: Lens.Lens' LaunchTemplateTagSpecificationRequest (Core.Maybe [Types.Tag])
lttsrTags = Lens.field @"tags"
{-# DEPRECATED lttsrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

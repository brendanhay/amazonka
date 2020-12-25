{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
  ( ResourceTagMapping (..),

    -- * Smart constructor
    mkResourceTagMapping,

    -- * Lenses
    rtmComplianceDetails,
    rtmResourceARN,
    rtmTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.ResourceARN as Types
import qualified Network.AWS.ResourceGroupsTagging.Types.Tag as Types

-- | A list of resource ARNs and the tags (keys and values) that are associated with each.
--
-- /See:/ 'mkResourceTagMapping' smart constructor.
data ResourceTagMapping = ResourceTagMapping'
  { -- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
    complianceDetails :: Core.Maybe Types.ComplianceDetails,
    -- | The ARN of the resource.
    resourceARN :: Core.Maybe Types.ResourceARN,
    -- | The tags that have been applied to one or more AWS resources.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTagMapping' value with any optional fields omitted.
mkResourceTagMapping ::
  ResourceTagMapping
mkResourceTagMapping =
  ResourceTagMapping'
    { complianceDetails = Core.Nothing,
      resourceARN = Core.Nothing,
      tags = Core.Nothing
    }

-- | Information that shows whether a resource is compliant with the effective tag policy, including details on any noncompliant tag keys.
--
-- /Note:/ Consider using 'complianceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmComplianceDetails :: Lens.Lens' ResourceTagMapping (Core.Maybe Types.ComplianceDetails)
rtmComplianceDetails = Lens.field @"complianceDetails"
{-# DEPRECATED rtmComplianceDetails "Use generic-lens or generic-optics with 'complianceDetails' instead." #-}

-- | The ARN of the resource.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmResourceARN :: Lens.Lens' ResourceTagMapping (Core.Maybe Types.ResourceARN)
rtmResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED rtmResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The tags that have been applied to one or more AWS resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtmTags :: Lens.Lens' ResourceTagMapping (Core.Maybe [Types.Tag])
rtmTags = Lens.field @"tags"
{-# DEPRECATED rtmTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ResourceTagMapping where
  parseJSON =
    Core.withObject "ResourceTagMapping" Core.$
      \x ->
        ResourceTagMapping'
          Core.<$> (x Core..:? "ComplianceDetails")
          Core.<*> (x Core..:? "ResourceARN")
          Core.<*> (x Core..:? "Tags")

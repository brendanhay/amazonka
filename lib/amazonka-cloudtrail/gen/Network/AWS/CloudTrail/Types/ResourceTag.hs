{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtResourceId,
    rtTagsList,
  )
where

import qualified Network.AWS.CloudTrail.Types.String as Types
import qualified Network.AWS.CloudTrail.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A resource tag.
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { -- | Specifies the ARN of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | A list of tags.
    tagsList :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTag' value with any optional fields omitted.
mkResourceTag ::
  ResourceTag
mkResourceTag =
  ResourceTag' {resourceId = Core.Nothing, tagsList = Core.Nothing}

-- | Specifies the ARN of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtResourceId :: Lens.Lens' ResourceTag (Core.Maybe Types.String)
rtResourceId = Lens.field @"resourceId"
{-# DEPRECATED rtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | A list of tags.
--
-- /Note:/ Consider using 'tagsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTagsList :: Lens.Lens' ResourceTag (Core.Maybe [Types.Tag])
rtTagsList = Lens.field @"tagsList"
{-# DEPRECATED rtTagsList "Use generic-lens or generic-optics with 'tagsList' instead." #-}

instance Core.FromJSON ResourceTag where
  parseJSON =
    Core.withObject "ResourceTag" Core.$
      \x ->
        ResourceTag'
          Core.<$> (x Core..:? "ResourceId") Core.<*> (x Core..:? "TagsList")

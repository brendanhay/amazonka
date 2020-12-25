{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.DescribeImagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.DescribeImagesFilter
  ( DescribeImagesFilter (..),

    -- * Smart constructor
    mkDescribeImagesFilter,

    -- * Lenses
    difTagStatus,
  )
where

import qualified Network.AWS.ECR.Types.TagStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a filter on a 'DescribeImages' operation.
--
-- /See:/ 'mkDescribeImagesFilter' smart constructor.
newtype DescribeImagesFilter = DescribeImagesFilter'
  { -- | The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
    tagStatus :: Core.Maybe Types.TagStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagesFilter' value with any optional fields omitted.
mkDescribeImagesFilter ::
  DescribeImagesFilter
mkDescribeImagesFilter =
  DescribeImagesFilter' {tagStatus = Core.Nothing}

-- | The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difTagStatus :: Lens.Lens' DescribeImagesFilter (Core.Maybe Types.TagStatus)
difTagStatus = Lens.field @"tagStatus"
{-# DEPRECATED difTagStatus "Use generic-lens or generic-optics with 'tagStatus' instead." #-}

instance Core.FromJSON DescribeImagesFilter where
  toJSON DescribeImagesFilter {..} =
    Core.object
      (Core.catMaybes [("tagStatus" Core..=) Core.<$> tagStatus])

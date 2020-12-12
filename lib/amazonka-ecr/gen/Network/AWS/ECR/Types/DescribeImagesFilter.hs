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

import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a filter on a 'DescribeImages' operation.
--
-- /See:/ 'mkDescribeImagesFilter' smart constructor.
newtype DescribeImagesFilter = DescribeImagesFilter'
  { tagStatus ::
      Lude.Maybe TagStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImagesFilter' with the minimum fields required to make a request.
--
-- * 'tagStatus' - The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
mkDescribeImagesFilter ::
  DescribeImagesFilter
mkDescribeImagesFilter =
  DescribeImagesFilter' {tagStatus = Lude.Nothing}

-- | The tag status with which to filter your 'DescribeImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difTagStatus :: Lens.Lens' DescribeImagesFilter (Lude.Maybe TagStatus)
difTagStatus = Lens.lens (tagStatus :: DescribeImagesFilter -> Lude.Maybe TagStatus) (\s a -> s {tagStatus = a} :: DescribeImagesFilter)
{-# DEPRECATED difTagStatus "Use generic-lens or generic-optics with 'tagStatus' instead." #-}

instance Lude.ToJSON DescribeImagesFilter where
  toJSON DescribeImagesFilter' {..} =
    Lude.object
      (Lude.catMaybes [("tagStatus" Lude..=) Lude.<$> tagStatus])

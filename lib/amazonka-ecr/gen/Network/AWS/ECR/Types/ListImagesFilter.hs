{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ListImagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ListImagesFilter
  ( ListImagesFilter (..),

    -- * Smart constructor
    mkListImagesFilter,

    -- * Lenses
    lifTagStatus,
  )
where

import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a filter on a 'ListImages' operation.
--
-- /See:/ 'mkListImagesFilter' smart constructor.
newtype ListImagesFilter = ListImagesFilter'
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

-- | Creates a value of 'ListImagesFilter' with the minimum fields required to make a request.
--
-- * 'tagStatus' - The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
mkListImagesFilter ::
  ListImagesFilter
mkListImagesFilter = ListImagesFilter' {tagStatus = Lude.Nothing}

-- | The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifTagStatus :: Lens.Lens' ListImagesFilter (Lude.Maybe TagStatus)
lifTagStatus = Lens.lens (tagStatus :: ListImagesFilter -> Lude.Maybe TagStatus) (\s a -> s {tagStatus = a} :: ListImagesFilter)
{-# DEPRECATED lifTagStatus "Use generic-lens or generic-optics with 'tagStatus' instead." #-}

instance Lude.ToJSON ListImagesFilter where
  toJSON ListImagesFilter' {..} =
    Lude.object
      (Lude.catMaybes [("tagStatus" Lude..=) Lude.<$> tagStatus])

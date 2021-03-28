{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ListImagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ListImagesFilter
  ( ListImagesFilter (..)
  -- * Smart constructor
  , mkListImagesFilter
  -- * Lenses
  , lifTagStatus
  ) where

import qualified Network.AWS.ECR.Types.TagStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a filter on a 'ListImages' operation.
--
-- /See:/ 'mkListImagesFilter' smart constructor.
newtype ListImagesFilter = ListImagesFilter'
  { tagStatus :: Core.Maybe Types.TagStatus
    -- ^ The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListImagesFilter' value with any optional fields omitted.
mkListImagesFilter
    :: ListImagesFilter
mkListImagesFilter = ListImagesFilter'{tagStatus = Core.Nothing}

-- | The tag status with which to filter your 'ListImages' results. You can filter results based on whether they are @TAGGED@ or @UNTAGGED@ .
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifTagStatus :: Lens.Lens' ListImagesFilter (Core.Maybe Types.TagStatus)
lifTagStatus = Lens.field @"tagStatus"
{-# INLINEABLE lifTagStatus #-}
{-# DEPRECATED tagStatus "Use generic-lens or generic-optics with 'tagStatus' instead"  #-}

instance Core.FromJSON ListImagesFilter where
        toJSON ListImagesFilter{..}
          = Core.object
              (Core.catMaybes [("tagStatus" Core..=) Core.<$> tagStatus])

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
  ( LifecyclePolicyPreviewFilter (..)
  -- * Smart constructor
  , mkLifecyclePolicyPreviewFilter
  -- * Lenses
  , lppfTagStatus
  ) where

import qualified Network.AWS.ECR.Types.TagStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The filter for the lifecycle policy preview.
--
-- /See:/ 'mkLifecyclePolicyPreviewFilter' smart constructor.
newtype LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
  { tagStatus :: Core.Maybe Types.TagStatus
    -- ^ The tag status of the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecyclePolicyPreviewFilter' value with any optional fields omitted.
mkLifecyclePolicyPreviewFilter
    :: LifecyclePolicyPreviewFilter
mkLifecyclePolicyPreviewFilter
  = LifecyclePolicyPreviewFilter'{tagStatus = Core.Nothing}

-- | The tag status of the image.
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppfTagStatus :: Lens.Lens' LifecyclePolicyPreviewFilter (Core.Maybe Types.TagStatus)
lppfTagStatus = Lens.field @"tagStatus"
{-# INLINEABLE lppfTagStatus #-}
{-# DEPRECATED tagStatus "Use generic-lens or generic-optics with 'tagStatus' instead"  #-}

instance Core.FromJSON LifecyclePolicyPreviewFilter where
        toJSON LifecyclePolicyPreviewFilter{..}
          = Core.object
              (Core.catMaybes [("tagStatus" Core..=) Core.<$> tagStatus])

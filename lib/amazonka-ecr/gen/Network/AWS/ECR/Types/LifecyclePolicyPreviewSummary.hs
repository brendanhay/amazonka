{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
  ( LifecyclePolicyPreviewSummary (..)
  -- * Smart constructor
  , mkLifecyclePolicyPreviewSummary
  -- * Lenses
  , lppsExpiringImageTotalCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of the lifecycle policy preview request.
--
-- /See:/ 'mkLifecyclePolicyPreviewSummary' smart constructor.
newtype LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary'
  { expiringImageTotalCount :: Core.Maybe Core.Natural
    -- ^ The number of expiring images.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecyclePolicyPreviewSummary' value with any optional fields omitted.
mkLifecyclePolicyPreviewSummary
    :: LifecyclePolicyPreviewSummary
mkLifecyclePolicyPreviewSummary
  = LifecyclePolicyPreviewSummary'{expiringImageTotalCount =
                                     Core.Nothing}

-- | The number of expiring images.
--
-- /Note:/ Consider using 'expiringImageTotalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppsExpiringImageTotalCount :: Lens.Lens' LifecyclePolicyPreviewSummary (Core.Maybe Core.Natural)
lppsExpiringImageTotalCount = Lens.field @"expiringImageTotalCount"
{-# INLINEABLE lppsExpiringImageTotalCount #-}
{-# DEPRECATED expiringImageTotalCount "Use generic-lens or generic-optics with 'expiringImageTotalCount' instead"  #-}

instance Core.FromJSON LifecyclePolicyPreviewSummary where
        parseJSON
          = Core.withObject "LifecyclePolicyPreviewSummary" Core.$
              \ x ->
                LifecyclePolicyPreviewSummary' Core.<$>
                  (x Core..:? "expiringImageTotalCount")

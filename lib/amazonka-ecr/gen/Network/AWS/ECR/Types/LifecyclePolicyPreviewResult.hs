{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
  ( LifecyclePolicyPreviewResult (..)
  -- * Smart constructor
  , mkLifecyclePolicyPreviewResult
  -- * Lenses
  , lpprAction
  , lpprAppliedRulePriority
  , lpprImageDigest
  , lpprImagePushedAt
  , lpprImageTags
  ) where

import qualified Network.AWS.ECR.Types.ImageDigest as Types
import qualified Network.AWS.ECR.Types.ImageTag as Types
import qualified Network.AWS.ECR.Types.LifecyclePolicyRuleAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of the lifecycle policy preview.
--
-- /See:/ 'mkLifecyclePolicyPreviewResult' smart constructor.
data LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult'
  { action :: Core.Maybe Types.LifecyclePolicyRuleAction
    -- ^ The type of action to be taken.
  , appliedRulePriority :: Core.Maybe Core.Natural
    -- ^ The priority of the applied rule.
  , imageDigest :: Core.Maybe Types.ImageDigest
    -- ^ The @sha256@ digest of the image manifest.
  , imagePushedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
  , imageTags :: Core.Maybe [Types.ImageTag]
    -- ^ The list of tags associated with this image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LifecyclePolicyPreviewResult' value with any optional fields omitted.
mkLifecyclePolicyPreviewResult
    :: LifecyclePolicyPreviewResult
mkLifecyclePolicyPreviewResult
  = LifecyclePolicyPreviewResult'{action = Core.Nothing,
                                  appliedRulePriority = Core.Nothing, imageDigest = Core.Nothing,
                                  imagePushedAt = Core.Nothing, imageTags = Core.Nothing}

-- | The type of action to be taken.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprAction :: Lens.Lens' LifecyclePolicyPreviewResult (Core.Maybe Types.LifecyclePolicyRuleAction)
lpprAction = Lens.field @"action"
{-# INLINEABLE lpprAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The priority of the applied rule.
--
-- /Note:/ Consider using 'appliedRulePriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprAppliedRulePriority :: Lens.Lens' LifecyclePolicyPreviewResult (Core.Maybe Core.Natural)
lpprAppliedRulePriority = Lens.field @"appliedRulePriority"
{-# INLINEABLE lpprAppliedRulePriority #-}
{-# DEPRECATED appliedRulePriority "Use generic-lens or generic-optics with 'appliedRulePriority' instead"  #-}

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImageDigest :: Lens.Lens' LifecyclePolicyPreviewResult (Core.Maybe Types.ImageDigest)
lpprImageDigest = Lens.field @"imageDigest"
{-# INLINEABLE lpprImageDigest #-}
{-# DEPRECATED imageDigest "Use generic-lens or generic-optics with 'imageDigest' instead"  #-}

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- /Note:/ Consider using 'imagePushedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImagePushedAt :: Lens.Lens' LifecyclePolicyPreviewResult (Core.Maybe Core.NominalDiffTime)
lpprImagePushedAt = Lens.field @"imagePushedAt"
{-# INLINEABLE lpprImagePushedAt #-}
{-# DEPRECATED imagePushedAt "Use generic-lens or generic-optics with 'imagePushedAt' instead"  #-}

-- | The list of tags associated with this image.
--
-- /Note:/ Consider using 'imageTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImageTags :: Lens.Lens' LifecyclePolicyPreviewResult (Core.Maybe [Types.ImageTag])
lpprImageTags = Lens.field @"imageTags"
{-# INLINEABLE lpprImageTags #-}
{-# DEPRECATED imageTags "Use generic-lens or generic-optics with 'imageTags' instead"  #-}

instance Core.FromJSON LifecyclePolicyPreviewResult where
        parseJSON
          = Core.withObject "LifecyclePolicyPreviewResult" Core.$
              \ x ->
                LifecyclePolicyPreviewResult' Core.<$>
                  (x Core..:? "action") Core.<*> x Core..:? "appliedRulePriority"
                    Core.<*> x Core..:? "imageDigest"
                    Core.<*> x Core..:? "imagePushedAt"
                    Core.<*> x Core..:? "imageTags"

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
  ( LifecyclePolicyPreviewResult (..),

    -- * Smart constructor
    mkLifecyclePolicyPreviewResult,

    -- * Lenses
    lpprImageTags,
    lpprAction,
    lpprImageDigest,
    lpprImagePushedAt,
    lpprAppliedRulePriority,
  )
where

import Network.AWS.ECR.Types.LifecyclePolicyRuleAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of the lifecycle policy preview.
--
-- /See:/ 'mkLifecyclePolicyPreviewResult' smart constructor.
data LifecyclePolicyPreviewResult = LifecyclePolicyPreviewResult'
  { imageTags ::
      Lude.Maybe [Lude.Text],
    action ::
      Lude.Maybe
        LifecyclePolicyRuleAction,
    imageDigest ::
      Lude.Maybe Lude.Text,
    imagePushedAt ::
      Lude.Maybe Lude.Timestamp,
    appliedRulePriority ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecyclePolicyPreviewResult' with the minimum fields required to make a request.
--
-- * 'action' - The type of action to be taken.
-- * 'appliedRulePriority' - The priority of the applied rule.
-- * 'imageDigest' - The @sha256@ digest of the image manifest.
-- * 'imagePushedAt' - The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
-- * 'imageTags' - The list of tags associated with this image.
mkLifecyclePolicyPreviewResult ::
  LifecyclePolicyPreviewResult
mkLifecyclePolicyPreviewResult =
  LifecyclePolicyPreviewResult'
    { imageTags = Lude.Nothing,
      action = Lude.Nothing,
      imageDigest = Lude.Nothing,
      imagePushedAt = Lude.Nothing,
      appliedRulePriority = Lude.Nothing
    }

-- | The list of tags associated with this image.
--
-- /Note:/ Consider using 'imageTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImageTags :: Lens.Lens' LifecyclePolicyPreviewResult (Lude.Maybe [Lude.Text])
lpprImageTags = Lens.lens (imageTags :: LifecyclePolicyPreviewResult -> Lude.Maybe [Lude.Text]) (\s a -> s {imageTags = a} :: LifecyclePolicyPreviewResult)
{-# DEPRECATED lpprImageTags "Use generic-lens or generic-optics with 'imageTags' instead." #-}

-- | The type of action to be taken.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprAction :: Lens.Lens' LifecyclePolicyPreviewResult (Lude.Maybe LifecyclePolicyRuleAction)
lpprAction = Lens.lens (action :: LifecyclePolicyPreviewResult -> Lude.Maybe LifecyclePolicyRuleAction) (\s a -> s {action = a} :: LifecyclePolicyPreviewResult)
{-# DEPRECATED lpprAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImageDigest :: Lens.Lens' LifecyclePolicyPreviewResult (Lude.Maybe Lude.Text)
lpprImageDigest = Lens.lens (imageDigest :: LifecyclePolicyPreviewResult -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: LifecyclePolicyPreviewResult)
{-# DEPRECATED lpprImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- /Note:/ Consider using 'imagePushedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprImagePushedAt :: Lens.Lens' LifecyclePolicyPreviewResult (Lude.Maybe Lude.Timestamp)
lpprImagePushedAt = Lens.lens (imagePushedAt :: LifecyclePolicyPreviewResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {imagePushedAt = a} :: LifecyclePolicyPreviewResult)
{-# DEPRECATED lpprImagePushedAt "Use generic-lens or generic-optics with 'imagePushedAt' instead." #-}

-- | The priority of the applied rule.
--
-- /Note:/ Consider using 'appliedRulePriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpprAppliedRulePriority :: Lens.Lens' LifecyclePolicyPreviewResult (Lude.Maybe Lude.Natural)
lpprAppliedRulePriority = Lens.lens (appliedRulePriority :: LifecyclePolicyPreviewResult -> Lude.Maybe Lude.Natural) (\s a -> s {appliedRulePriority = a} :: LifecyclePolicyPreviewResult)
{-# DEPRECATED lpprAppliedRulePriority "Use generic-lens or generic-optics with 'appliedRulePriority' instead." #-}

instance Lude.FromJSON LifecyclePolicyPreviewResult where
  parseJSON =
    Lude.withObject
      "LifecyclePolicyPreviewResult"
      ( \x ->
          LifecyclePolicyPreviewResult'
            Lude.<$> (x Lude..:? "imageTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "imageDigest")
            Lude.<*> (x Lude..:? "imagePushedAt")
            Lude.<*> (x Lude..:? "appliedRulePriority")
      )

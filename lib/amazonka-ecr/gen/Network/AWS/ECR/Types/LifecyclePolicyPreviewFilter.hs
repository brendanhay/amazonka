{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
  ( LifecyclePolicyPreviewFilter (..),

    -- * Smart constructor
    mkLifecyclePolicyPreviewFilter,

    -- * Lenses
    lppfTagStatus,
  )
where

import Network.AWS.ECR.Types.TagStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The filter for the lifecycle policy preview.
--
-- /See:/ 'mkLifecyclePolicyPreviewFilter' smart constructor.
newtype LifecyclePolicyPreviewFilter = LifecyclePolicyPreviewFilter'
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

-- | Creates a value of 'LifecyclePolicyPreviewFilter' with the minimum fields required to make a request.
--
-- * 'tagStatus' - The tag status of the image.
mkLifecyclePolicyPreviewFilter ::
  LifecyclePolicyPreviewFilter
mkLifecyclePolicyPreviewFilter =
  LifecyclePolicyPreviewFilter' {tagStatus = Lude.Nothing}

-- | The tag status of the image.
--
-- /Note:/ Consider using 'tagStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppfTagStatus :: Lens.Lens' LifecyclePolicyPreviewFilter (Lude.Maybe TagStatus)
lppfTagStatus = Lens.lens (tagStatus :: LifecyclePolicyPreviewFilter -> Lude.Maybe TagStatus) (\s a -> s {tagStatus = a} :: LifecyclePolicyPreviewFilter)
{-# DEPRECATED lppfTagStatus "Use generic-lens or generic-optics with 'tagStatus' instead." #-}

instance Lude.ToJSON LifecyclePolicyPreviewFilter where
  toJSON LifecyclePolicyPreviewFilter' {..} =
    Lude.object
      (Lude.catMaybes [("tagStatus" Lude..=) Lude.<$> tagStatus])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
  ( LifecyclePolicyPreviewSummary (..),

    -- * Smart constructor
    mkLifecyclePolicyPreviewSummary,

    -- * Lenses
    lppsExpiringImageTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of the lifecycle policy preview request.
--
-- /See:/ 'mkLifecyclePolicyPreviewSummary' smart constructor.
newtype LifecyclePolicyPreviewSummary = LifecyclePolicyPreviewSummary'
  { -- | The number of expiring images.
    expiringImageTotalCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecyclePolicyPreviewSummary' with the minimum fields required to make a request.
--
-- * 'expiringImageTotalCount' - The number of expiring images.
mkLifecyclePolicyPreviewSummary ::
  LifecyclePolicyPreviewSummary
mkLifecyclePolicyPreviewSummary =
  LifecyclePolicyPreviewSummary'
    { expiringImageTotalCount =
        Lude.Nothing
    }

-- | The number of expiring images.
--
-- /Note:/ Consider using 'expiringImageTotalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lppsExpiringImageTotalCount :: Lens.Lens' LifecyclePolicyPreviewSummary (Lude.Maybe Lude.Natural)
lppsExpiringImageTotalCount = Lens.lens (expiringImageTotalCount :: LifecyclePolicyPreviewSummary -> Lude.Maybe Lude.Natural) (\s a -> s {expiringImageTotalCount = a} :: LifecyclePolicyPreviewSummary)
{-# DEPRECATED lppsExpiringImageTotalCount "Use generic-lens or generic-optics with 'expiringImageTotalCount' instead." #-}

instance Lude.FromJSON LifecyclePolicyPreviewSummary where
  parseJSON =
    Lude.withObject
      "LifecyclePolicyPreviewSummary"
      ( \x ->
          LifecyclePolicyPreviewSummary'
            Lude.<$> (x Lude..:? "expiringImageTotalCount")
      )

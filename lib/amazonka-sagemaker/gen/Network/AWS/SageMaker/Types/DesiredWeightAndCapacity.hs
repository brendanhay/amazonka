-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
  ( DesiredWeightAndCapacity (..),

    -- * Smart constructor
    mkDesiredWeightAndCapacity,

    -- * Lenses
    dwacDesiredInstanceCount,
    dwacDesiredWeight,
    dwacVariantName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies weight and capacity values for a production variant.
--
-- /See:/ 'mkDesiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { desiredInstanceCount ::
      Lude.Maybe Lude.Natural,
    desiredWeight :: Lude.Maybe Lude.Double,
    variantName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DesiredWeightAndCapacity' with the minimum fields required to make a request.
--
-- * 'desiredInstanceCount' - The variant's capacity.
-- * 'desiredWeight' - The variant's weight.
-- * 'variantName' - The name of the variant to update.
mkDesiredWeightAndCapacity ::
  -- | 'variantName'
  Lude.Text ->
  DesiredWeightAndCapacity
mkDesiredWeightAndCapacity pVariantName_ =
  DesiredWeightAndCapacity'
    { desiredInstanceCount = Lude.Nothing,
      desiredWeight = Lude.Nothing,
      variantName = pVariantName_
    }

-- | The variant's capacity.
--
-- /Note:/ Consider using 'desiredInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacDesiredInstanceCount :: Lens.Lens' DesiredWeightAndCapacity (Lude.Maybe Lude.Natural)
dwacDesiredInstanceCount = Lens.lens (desiredInstanceCount :: DesiredWeightAndCapacity -> Lude.Maybe Lude.Natural) (\s a -> s {desiredInstanceCount = a} :: DesiredWeightAndCapacity)
{-# DEPRECATED dwacDesiredInstanceCount "Use generic-lens or generic-optics with 'desiredInstanceCount' instead." #-}

-- | The variant's weight.
--
-- /Note:/ Consider using 'desiredWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacDesiredWeight :: Lens.Lens' DesiredWeightAndCapacity (Lude.Maybe Lude.Double)
dwacDesiredWeight = Lens.lens (desiredWeight :: DesiredWeightAndCapacity -> Lude.Maybe Lude.Double) (\s a -> s {desiredWeight = a} :: DesiredWeightAndCapacity)
{-# DEPRECATED dwacDesiredWeight "Use generic-lens or generic-optics with 'desiredWeight' instead." #-}

-- | The name of the variant to update.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacVariantName :: Lens.Lens' DesiredWeightAndCapacity Lude.Text
dwacVariantName = Lens.lens (variantName :: DesiredWeightAndCapacity -> Lude.Text) (\s a -> s {variantName = a} :: DesiredWeightAndCapacity)
{-# DEPRECATED dwacVariantName "Use generic-lens or generic-optics with 'variantName' instead." #-}

instance Lude.ToJSON DesiredWeightAndCapacity where
  toJSON DesiredWeightAndCapacity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DesiredInstanceCount" Lude..=) Lude.<$> desiredInstanceCount,
            ("DesiredWeight" Lude..=) Lude.<$> desiredWeight,
            Lude.Just ("VariantName" Lude..= variantName)
          ]
      )

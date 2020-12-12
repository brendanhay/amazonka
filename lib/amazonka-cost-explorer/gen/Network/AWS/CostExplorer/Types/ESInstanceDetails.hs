{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ESInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ESInstanceDetails
  ( ESInstanceDetails (..),

    -- * Smart constructor
    mkESInstanceDetails,

    -- * Lenses
    esidCurrentGeneration,
    esidInstanceClass,
    esidInstanceSize,
    esidSizeFlexEligible,
    esidRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Amazon ES instances that AWS recommends that you purchase.
--
-- /See:/ 'mkESInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { currentGeneration ::
      Lude.Maybe Lude.Bool,
    instanceClass :: Lude.Maybe Lude.Text,
    instanceSize :: Lude.Maybe Lude.Text,
    sizeFlexEligible :: Lude.Maybe Lude.Bool,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ESInstanceDetails' with the minimum fields required to make a request.
--
-- * 'currentGeneration' - Whether the recommendation is for a current-generation instance.
-- * 'instanceClass' - The class of instance that AWS recommends.
-- * 'instanceSize' - The size of instance that AWS recommends.
-- * 'region' - The AWS Region of the recommended reservation.
-- * 'sizeFlexEligible' - Whether the recommended reservation is size flexible.
mkESInstanceDetails ::
  ESInstanceDetails
mkESInstanceDetails =
  ESInstanceDetails'
    { currentGeneration = Lude.Nothing,
      instanceClass = Lude.Nothing,
      instanceSize = Lude.Nothing,
      sizeFlexEligible = Lude.Nothing,
      region = Lude.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esidCurrentGeneration :: Lens.Lens' ESInstanceDetails (Lude.Maybe Lude.Bool)
esidCurrentGeneration = Lens.lens (currentGeneration :: ESInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: ESInstanceDetails)
{-# DEPRECATED esidCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The class of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esidInstanceClass :: Lens.Lens' ESInstanceDetails (Lude.Maybe Lude.Text)
esidInstanceClass = Lens.lens (instanceClass :: ESInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceClass = a} :: ESInstanceDetails)
{-# DEPRECATED esidInstanceClass "Use generic-lens or generic-optics with 'instanceClass' instead." #-}

-- | The size of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esidInstanceSize :: Lens.Lens' ESInstanceDetails (Lude.Maybe Lude.Text)
esidInstanceSize = Lens.lens (instanceSize :: ESInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceSize = a} :: ESInstanceDetails)
{-# DEPRECATED esidInstanceSize "Use generic-lens or generic-optics with 'instanceSize' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esidSizeFlexEligible :: Lens.Lens' ESInstanceDetails (Lude.Maybe Lude.Bool)
esidSizeFlexEligible = Lens.lens (sizeFlexEligible :: ESInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {sizeFlexEligible = a} :: ESInstanceDetails)
{-# DEPRECATED esidSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esidRegion :: Lens.Lens' ESInstanceDetails (Lude.Maybe Lude.Text)
esidRegion = Lens.lens (region :: ESInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ESInstanceDetails)
{-# DEPRECATED esidRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON ESInstanceDetails where
  parseJSON =
    Lude.withObject
      "ESInstanceDetails"
      ( \x ->
          ESInstanceDetails'
            Lude.<$> (x Lude..:? "CurrentGeneration")
            Lude.<*> (x Lude..:? "InstanceClass")
            Lude.<*> (x Lude..:? "InstanceSize")
            Lude.<*> (x Lude..:? "SizeFlexEligible")
            Lude.<*> (x Lude..:? "Region")
      )

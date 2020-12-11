-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2Specification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2Specification
  ( EC2Specification (..),

    -- * Smart constructor
    mkEC2Specification,

    -- * Lenses
    esOfferingClass,
  )
where

import Network.AWS.CostExplorer.Types.OfferingClass
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
-- /See:/ 'mkEC2Specification' smart constructor.
newtype EC2Specification = EC2Specification'
  { offeringClass ::
      Lude.Maybe OfferingClass
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2Specification' with the minimum fields required to make a request.
--
-- * 'offeringClass' - Whether you want a recommendation for standard or convertible reservations.
mkEC2Specification ::
  EC2Specification
mkEC2Specification =
  EC2Specification' {offeringClass = Lude.Nothing}

-- | Whether you want a recommendation for standard or convertible reservations.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esOfferingClass :: Lens.Lens' EC2Specification (Lude.Maybe OfferingClass)
esOfferingClass = Lens.lens (offeringClass :: EC2Specification -> Lude.Maybe OfferingClass) (\s a -> s {offeringClass = a} :: EC2Specification)
{-# DEPRECATED esOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

instance Lude.FromJSON EC2Specification where
  parseJSON =
    Lude.withObject
      "EC2Specification"
      (\x -> EC2Specification' Lude.<$> (x Lude..:? "OfferingClass"))

instance Lude.ToJSON EC2Specification where
  toJSON EC2Specification' {..} =
    Lude.object
      (Lude.catMaybes [("OfferingClass" Lude..=) Lude.<$> offeringClass])

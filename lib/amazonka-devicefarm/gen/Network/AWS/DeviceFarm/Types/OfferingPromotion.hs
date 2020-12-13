{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingPromotion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingPromotion
  ( OfferingPromotion (..),

    -- * Smart constructor
    mkOfferingPromotion,

    -- * Lenses
    opId,
    opDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an offering promotion.
--
-- /See:/ 'mkOfferingPromotion' smart constructor.
data OfferingPromotion = OfferingPromotion'
  { -- | The ID of the offering promotion.
    id :: Lude.Maybe Lude.Text,
    -- | A string that describes the offering promotion.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OfferingPromotion' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the offering promotion.
-- * 'description' - A string that describes the offering promotion.
mkOfferingPromotion ::
  OfferingPromotion
mkOfferingPromotion =
  OfferingPromotion' {id = Lude.Nothing, description = Lude.Nothing}

-- | The ID of the offering promotion.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opId :: Lens.Lens' OfferingPromotion (Lude.Maybe Lude.Text)
opId = Lens.lens (id :: OfferingPromotion -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OfferingPromotion)
{-# DEPRECATED opId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A string that describes the offering promotion.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opDescription :: Lens.Lens' OfferingPromotion (Lude.Maybe Lude.Text)
opDescription = Lens.lens (description :: OfferingPromotion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OfferingPromotion)
{-# DEPRECATED opDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON OfferingPromotion where
  parseJSON =
    Lude.withObject
      "OfferingPromotion"
      ( \x ->
          OfferingPromotion'
            Lude.<$> (x Lude..:? "id") Lude.<*> (x Lude..:? "description")
      )

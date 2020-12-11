-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingStatus
  ( OfferingStatus (..),

    -- * Smart constructor
    mkOfferingStatus,

    -- * Lenses
    osEffectiveOn,
    osOffering,
    osQuantity,
    osType,
  )
where

import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the offering.
--
-- /See:/ 'mkOfferingStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { effectiveOn ::
      Lude.Maybe Lude.Timestamp,
    offering :: Lude.Maybe Offering,
    quantity :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe OfferingTransactionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OfferingStatus' with the minimum fields required to make a request.
--
-- * 'effectiveOn' - The date on which the offering is effective.
-- * 'offering' - Represents the metadata of an offering status.
-- * 'quantity' - The number of available devices in the offering.
-- * 'type'' - The type specified for the offering status.
mkOfferingStatus ::
  OfferingStatus
mkOfferingStatus =
  OfferingStatus'
    { effectiveOn = Lude.Nothing,
      offering = Lude.Nothing,
      quantity = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The date on which the offering is effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEffectiveOn :: Lens.Lens' OfferingStatus (Lude.Maybe Lude.Timestamp)
osEffectiveOn = Lens.lens (effectiveOn :: OfferingStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {effectiveOn = a} :: OfferingStatus)
{-# DEPRECATED osEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

-- | Represents the metadata of an offering status.
--
-- /Note:/ Consider using 'offering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOffering :: Lens.Lens' OfferingStatus (Lude.Maybe Offering)
osOffering = Lens.lens (offering :: OfferingStatus -> Lude.Maybe Offering) (\s a -> s {offering = a} :: OfferingStatus)
{-# DEPRECATED osOffering "Use generic-lens or generic-optics with 'offering' instead." #-}

-- | The number of available devices in the offering.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osQuantity :: Lens.Lens' OfferingStatus (Lude.Maybe Lude.Int)
osQuantity = Lens.lens (quantity :: OfferingStatus -> Lude.Maybe Lude.Int) (\s a -> s {quantity = a} :: OfferingStatus)
{-# DEPRECATED osQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The type specified for the offering status.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OfferingStatus (Lude.Maybe OfferingTransactionType)
osType = Lens.lens (type' :: OfferingStatus -> Lude.Maybe OfferingTransactionType) (\s a -> s {type' = a} :: OfferingStatus)
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON OfferingStatus where
  parseJSON =
    Lude.withObject
      "OfferingStatus"
      ( \x ->
          OfferingStatus'
            Lude.<$> (x Lude..:? "effectiveOn")
            Lude.<*> (x Lude..:? "offering")
            Lude.<*> (x Lude..:? "quantity")
            Lude.<*> (x Lude..:? "type")
      )

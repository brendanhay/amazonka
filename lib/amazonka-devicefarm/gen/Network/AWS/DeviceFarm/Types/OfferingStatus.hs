{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.DeviceFarm.Types.Offering as Types
import qualified Network.AWS.DeviceFarm.Types.OfferingTransactionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the offering.
--
-- /See:/ 'mkOfferingStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { -- | The date on which the offering is effective.
    effectiveOn :: Core.Maybe Core.NominalDiffTime,
    -- | Represents the metadata of an offering status.
    offering :: Core.Maybe Types.Offering,
    -- | The number of available devices in the offering.
    quantity :: Core.Maybe Core.Int,
    -- | The type specified for the offering status.
    type' :: Core.Maybe Types.OfferingTransactionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OfferingStatus' value with any optional fields omitted.
mkOfferingStatus ::
  OfferingStatus
mkOfferingStatus =
  OfferingStatus'
    { effectiveOn = Core.Nothing,
      offering = Core.Nothing,
      quantity = Core.Nothing,
      type' = Core.Nothing
    }

-- | The date on which the offering is effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEffectiveOn :: Lens.Lens' OfferingStatus (Core.Maybe Core.NominalDiffTime)
osEffectiveOn = Lens.field @"effectiveOn"
{-# DEPRECATED osEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

-- | Represents the metadata of an offering status.
--
-- /Note:/ Consider using 'offering' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOffering :: Lens.Lens' OfferingStatus (Core.Maybe Types.Offering)
osOffering = Lens.field @"offering"
{-# DEPRECATED osOffering "Use generic-lens or generic-optics with 'offering' instead." #-}

-- | The number of available devices in the offering.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osQuantity :: Lens.Lens' OfferingStatus (Core.Maybe Core.Int)
osQuantity = Lens.field @"quantity"
{-# DEPRECATED osQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The type specified for the offering status.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OfferingStatus (Core.Maybe Types.OfferingTransactionType)
osType = Lens.field @"type'"
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON OfferingStatus where
  parseJSON =
    Core.withObject "OfferingStatus" Core.$
      \x ->
        OfferingStatus'
          Core.<$> (x Core..:? "effectiveOn")
          Core.<*> (x Core..:? "offering")
          Core.<*> (x Core..:? "quantity")
          Core.<*> (x Core..:? "type")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Offering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Offering
  ( Offering (..),

    -- * Smart constructor
    mkOffering,

    -- * Lenses
    oDescription,
    oId,
    oPlatform,
    oRecurringCharges,
    oType,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Description as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePlatform as Types
import qualified Network.AWS.DeviceFarm.Types.Id as Types
import qualified Network.AWS.DeviceFarm.Types.OfferingType as Types
import qualified Network.AWS.DeviceFarm.Types.RecurringCharge as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the metadata of a device offering.
--
-- /See:/ 'mkOffering' smart constructor.
data Offering = Offering'
  { -- | A string that describes the offering.
    description :: Core.Maybe Types.Description,
    -- | The ID that corresponds to a device offering.
    id :: Core.Maybe Types.Id,
    -- | The platform of the device (for example, @ANDROID@ or @IOS@ ).
    platform :: Core.Maybe Types.DevicePlatform,
    -- | Specifies whether there are recurring charges for the offering.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The type of offering (for example, @RECURRING@ ) for a device.
    type' :: Core.Maybe Types.OfferingType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Offering' value with any optional fields omitted.
mkOffering ::
  Offering
mkOffering =
  Offering'
    { description = Core.Nothing,
      id = Core.Nothing,
      platform = Core.Nothing,
      recurringCharges = Core.Nothing,
      type' = Core.Nothing
    }

-- | A string that describes the offering.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDescription :: Lens.Lens' Offering (Core.Maybe Types.Description)
oDescription = Lens.field @"description"
{-# DEPRECATED oDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID that corresponds to a device offering.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Offering (Core.Maybe Types.Id)
oId = Lens.field @"id"
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The platform of the device (for example, @ANDROID@ or @IOS@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPlatform :: Lens.Lens' Offering (Core.Maybe Types.DevicePlatform)
oPlatform = Lens.field @"platform"
{-# DEPRECATED oPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Specifies whether there are recurring charges for the offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRecurringCharges :: Lens.Lens' Offering (Core.Maybe [Types.RecurringCharge])
oRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED oRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The type of offering (for example, @RECURRING@ ) for a device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oType :: Lens.Lens' Offering (Core.Maybe Types.OfferingType)
oType = Lens.field @"type'"
{-# DEPRECATED oType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Offering where
  parseJSON =
    Core.withObject "Offering" Core.$
      \x ->
        Offering'
          Core.<$> (x Core..:? "description")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "platform")
          Core.<*> (x Core..:? "recurringCharges")
          Core.<*> (x Core..:? "type")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
  ( SmartHomeAppliance (..),

    -- * Smart constructor
    mkSmartHomeAppliance,

    -- * Lenses
    shaDescription,
    shaFriendlyName,
    shaManufacturerName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.ApplianceFriendlyName as Types
import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.ManufacturerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A smart home appliance that can connect to a central system. Any domestic device can be a smart appliance.
--
-- /See:/ 'mkSmartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { -- | The description of the smart home appliance.
    description :: Core.Maybe Types.Description,
    -- | The friendly name of the smart home appliance.
    friendlyName :: Core.Maybe Types.ApplianceFriendlyName,
    -- | The name of the manufacturer of the smart home appliance.
    manufacturerName :: Core.Maybe Types.ManufacturerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SmartHomeAppliance' value with any optional fields omitted.
mkSmartHomeAppliance ::
  SmartHomeAppliance
mkSmartHomeAppliance =
  SmartHomeAppliance'
    { description = Core.Nothing,
      friendlyName = Core.Nothing,
      manufacturerName = Core.Nothing
    }

-- | The description of the smart home appliance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaDescription :: Lens.Lens' SmartHomeAppliance (Core.Maybe Types.Description)
shaDescription = Lens.field @"description"
{-# DEPRECATED shaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The friendly name of the smart home appliance.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaFriendlyName :: Lens.Lens' SmartHomeAppliance (Core.Maybe Types.ApplianceFriendlyName)
shaFriendlyName = Lens.field @"friendlyName"
{-# DEPRECATED shaFriendlyName "Use generic-lens or generic-optics with 'friendlyName' instead." #-}

-- | The name of the manufacturer of the smart home appliance.
--
-- /Note:/ Consider using 'manufacturerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaManufacturerName :: Lens.Lens' SmartHomeAppliance (Core.Maybe Types.ManufacturerName)
shaManufacturerName = Lens.field @"manufacturerName"
{-# DEPRECATED shaManufacturerName "Use generic-lens or generic-optics with 'manufacturerName' instead." #-}

instance Core.FromJSON SmartHomeAppliance where
  parseJSON =
    Core.withObject "SmartHomeAppliance" Core.$
      \x ->
        SmartHomeAppliance'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "FriendlyName")
          Core.<*> (x Core..:? "ManufacturerName")

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
    shaFriendlyName,
    shaManufacturerName,
    shaDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A smart home appliance that can connect to a central system. Any domestic device can be a smart appliance.
--
-- /See:/ 'mkSmartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { friendlyName ::
      Lude.Maybe Lude.Text,
    manufacturerName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SmartHomeAppliance' with the minimum fields required to make a request.
--
-- * 'description' - The description of the smart home appliance.
-- * 'friendlyName' - The friendly name of the smart home appliance.
-- * 'manufacturerName' - The name of the manufacturer of the smart home appliance.
mkSmartHomeAppliance ::
  SmartHomeAppliance
mkSmartHomeAppliance =
  SmartHomeAppliance'
    { friendlyName = Lude.Nothing,
      manufacturerName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The friendly name of the smart home appliance.
--
-- /Note:/ Consider using 'friendlyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaFriendlyName :: Lens.Lens' SmartHomeAppliance (Lude.Maybe Lude.Text)
shaFriendlyName = Lens.lens (friendlyName :: SmartHomeAppliance -> Lude.Maybe Lude.Text) (\s a -> s {friendlyName = a} :: SmartHomeAppliance)
{-# DEPRECATED shaFriendlyName "Use generic-lens or generic-optics with 'friendlyName' instead." #-}

-- | The name of the manufacturer of the smart home appliance.
--
-- /Note:/ Consider using 'manufacturerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaManufacturerName :: Lens.Lens' SmartHomeAppliance (Lude.Maybe Lude.Text)
shaManufacturerName = Lens.lens (manufacturerName :: SmartHomeAppliance -> Lude.Maybe Lude.Text) (\s a -> s {manufacturerName = a} :: SmartHomeAppliance)
{-# DEPRECATED shaManufacturerName "Use generic-lens or generic-optics with 'manufacturerName' instead." #-}

-- | The description of the smart home appliance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shaDescription :: Lens.Lens' SmartHomeAppliance (Lude.Maybe Lude.Text)
shaDescription = Lens.lens (description :: SmartHomeAppliance -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SmartHomeAppliance)
{-# DEPRECATED shaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON SmartHomeAppliance where
  parseJSON =
    Lude.withObject
      "SmartHomeAppliance"
      ( \x ->
          SmartHomeAppliance'
            Lude.<$> (x Lude..:? "FriendlyName")
            Lude.<*> (x Lude..:? "ManufacturerName")
            Lude.<*> (x Lude..:? "Description")
      )

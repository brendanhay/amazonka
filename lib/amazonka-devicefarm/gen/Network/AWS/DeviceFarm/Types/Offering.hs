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
    oPlatform,
    oId,
    oRecurringCharges,
    oType,
    oDescription,
  )
where

import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.OfferingType
import Network.AWS.DeviceFarm.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the metadata of a device offering.
--
-- /See:/ 'mkOffering' smart constructor.
data Offering = Offering'
  { -- | The platform of the device (for example, @ANDROID@ or @IOS@ ).
    platform :: Lude.Maybe DevicePlatform,
    -- | The ID that corresponds to a device offering.
    id :: Lude.Maybe Lude.Text,
    -- | Specifies whether there are recurring charges for the offering.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The type of offering (for example, @RECURRING@ ) for a device.
    type' :: Lude.Maybe OfferingType,
    -- | A string that describes the offering.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- * 'platform' - The platform of the device (for example, @ANDROID@ or @IOS@ ).
-- * 'id' - The ID that corresponds to a device offering.
-- * 'recurringCharges' - Specifies whether there are recurring charges for the offering.
-- * 'type'' - The type of offering (for example, @RECURRING@ ) for a device.
-- * 'description' - A string that describes the offering.
mkOffering ::
  Offering
mkOffering =
  Offering'
    { platform = Lude.Nothing,
      id = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The platform of the device (for example, @ANDROID@ or @IOS@ ).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oPlatform :: Lens.Lens' Offering (Lude.Maybe DevicePlatform)
oPlatform = Lens.lens (platform :: Offering -> Lude.Maybe DevicePlatform) (\s a -> s {platform = a} :: Offering)
{-# DEPRECATED oPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The ID that corresponds to a device offering.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Offering)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies whether there are recurring charges for the offering.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRecurringCharges :: Lens.Lens' Offering (Lude.Maybe [RecurringCharge])
oRecurringCharges = Lens.lens (recurringCharges :: Offering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: Offering)
{-# DEPRECATED oRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The type of offering (for example, @RECURRING@ ) for a device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oType :: Lens.Lens' Offering (Lude.Maybe OfferingType)
oType = Lens.lens (type' :: Offering -> Lude.Maybe OfferingType) (\s a -> s {type' = a} :: Offering)
{-# DEPRECATED oType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A string that describes the offering.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDescription :: Lens.Lens' Offering (Lude.Maybe Lude.Text)
oDescription = Lens.lens (description :: Offering -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Offering)
{-# DEPRECATED oDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Offering where
  parseJSON =
    Lude.withObject
      "Offering"
      ( \x ->
          Offering'
            Lude.<$> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "recurringCharges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )

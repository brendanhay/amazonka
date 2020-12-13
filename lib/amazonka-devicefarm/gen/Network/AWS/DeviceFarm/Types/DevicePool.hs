{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePool
  ( DevicePool (..),

    -- * Smart constructor
    mkDevicePool,

    -- * Lenses
    dpArn,
    dpRules,
    dpName,
    dpMaxDevices,
    dpType,
    dpDescription,
  )
where

import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.Rule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a collection of device types.
--
-- /See:/ 'mkDevicePool' smart constructor.
data DevicePool = DevicePool'
  { -- | The device pool's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | Information about the device pool's rules.
    rules :: Lude.Maybe [Rule],
    -- | The device pool's name.
    name :: Lude.Maybe Lude.Text,
    -- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
    maxDevices :: Lude.Maybe Lude.Int,
    -- | The device pool's type.
    --
    -- Allowed values include:
    --
    --     * CURATED: A device pool that is created and managed by AWS Device Farm.
    --
    --
    --     * PRIVATE: A device pool that is created and managed by the device pool developer.
    type' :: Lude.Maybe DevicePoolType,
    -- | The device pool's description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- * 'arn' - The device pool's ARN.
-- * 'rules' - Information about the device pool's rules.
-- * 'name' - The device pool's name.
-- * 'maxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- * 'type'' - The device pool's type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
--
-- * 'description' - The device pool's description.
mkDevicePool ::
  DevicePool
mkDevicePool =
  DevicePool'
    { arn = Lude.Nothing,
      rules = Lude.Nothing,
      name = Lude.Nothing,
      maxDevices = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpArn :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
dpArn = Lens.lens (arn :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DevicePool)
{-# DEPRECATED dpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the device pool's rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpRules :: Lens.Lens' DevicePool (Lude.Maybe [Rule])
dpRules = Lens.lens (rules :: DevicePool -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: DevicePool)
{-# DEPRECATED dpRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The device pool's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
dpName = Lens.lens (name :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DevicePool)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxDevices :: Lens.Lens' DevicePool (Lude.Maybe Lude.Int)
dpMaxDevices = Lens.lens (maxDevices :: DevicePool -> Lude.Maybe Lude.Int) (\s a -> s {maxDevices = a} :: DevicePool)
{-# DEPRECATED dpMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

-- | The device pool's type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpType :: Lens.Lens' DevicePool (Lude.Maybe DevicePoolType)
dpType = Lens.lens (type' :: DevicePool -> Lude.Maybe DevicePoolType) (\s a -> s {type' = a} :: DevicePool)
{-# DEPRECATED dpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The device pool's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDescription :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
dpDescription = Lens.lens (description :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DevicePool)
{-# DEPRECATED dpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON DevicePool where
  parseJSON =
    Lude.withObject
      "DevicePool"
      ( \x ->
          DevicePool'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "rules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "maxDevices")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )

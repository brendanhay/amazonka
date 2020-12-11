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
    devArn,
    devRules,
    devName,
    devMaxDevices,
    devType,
    devDescription,
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
  { arn :: Lude.Maybe Lude.Text,
    rules :: Lude.Maybe [Rule],
    name :: Lude.Maybe Lude.Text,
    maxDevices :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe DevicePoolType,
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

-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- * 'arn' - The device pool's ARN.
-- * 'description' - The device pool's description.
-- * 'maxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
-- * 'name' - The device pool's name.
-- * 'rules' - Information about the device pool's rules.
-- * 'type'' - The device pool's type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
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
devArn :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
devArn = Lens.lens (arn :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DevicePool)
{-# DEPRECATED devArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the device pool's rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devRules :: Lens.Lens' DevicePool (Lude.Maybe [Rule])
devRules = Lens.lens (rules :: DevicePool -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: DevicePool)
{-# DEPRECATED devRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The device pool's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devName :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
devName = Lens.lens (name :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DevicePool)
{-# DEPRECATED devName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devMaxDevices :: Lens.Lens' DevicePool (Lude.Maybe Lude.Int)
devMaxDevices = Lens.lens (maxDevices :: DevicePool -> Lude.Maybe Lude.Int) (\s a -> s {maxDevices = a} :: DevicePool)
{-# DEPRECATED devMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

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
devType :: Lens.Lens' DevicePool (Lude.Maybe DevicePoolType)
devType = Lens.lens (type' :: DevicePool -> Lude.Maybe DevicePoolType) (\s a -> s {type' = a} :: DevicePool)
{-# DEPRECATED devType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The device pool's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devDescription :: Lens.Lens' DevicePool (Lude.Maybe Lude.Text)
devDescription = Lens.lens (description :: DevicePool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DevicePool)
{-# DEPRECATED devDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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

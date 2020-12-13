{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DeviceConfiguration
  ( DeviceConfiguration (..),

    -- * Smart constructor
    mkDeviceConfiguration,

    -- * Lenses
    dcSnowconeDeviceConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration

-- | The container for @SnowconeDeviceConfiguration@ .
--
-- /See:/ 'mkDeviceConfiguration' smart constructor.
newtype DeviceConfiguration = DeviceConfiguration'
  { -- | Returns information about the device configuration for an AWS Snowcone job.
    snowconeDeviceConfiguration :: Lude.Maybe SnowconeDeviceConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceConfiguration' with the minimum fields required to make a request.
--
-- * 'snowconeDeviceConfiguration' - Returns information about the device configuration for an AWS Snowcone job.
mkDeviceConfiguration ::
  DeviceConfiguration
mkDeviceConfiguration =
  DeviceConfiguration' {snowconeDeviceConfiguration = Lude.Nothing}

-- | Returns information about the device configuration for an AWS Snowcone job.
--
-- /Note:/ Consider using 'snowconeDeviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSnowconeDeviceConfiguration :: Lens.Lens' DeviceConfiguration (Lude.Maybe SnowconeDeviceConfiguration)
dcSnowconeDeviceConfiguration = Lens.lens (snowconeDeviceConfiguration :: DeviceConfiguration -> Lude.Maybe SnowconeDeviceConfiguration) (\s a -> s {snowconeDeviceConfiguration = a} :: DeviceConfiguration)
{-# DEPRECATED dcSnowconeDeviceConfiguration "Use generic-lens or generic-optics with 'snowconeDeviceConfiguration' instead." #-}

instance Lude.FromJSON DeviceConfiguration where
  parseJSON =
    Lude.withObject
      "DeviceConfiguration"
      ( \x ->
          DeviceConfiguration'
            Lude.<$> (x Lude..:? "SnowconeDeviceConfiguration")
      )

instance Lude.ToJSON DeviceConfiguration where
  toJSON DeviceConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SnowconeDeviceConfiguration" Lude..=)
              Lude.<$> snowconeDeviceConfiguration
          ]
      )

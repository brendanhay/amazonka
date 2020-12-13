{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
  ( SnowconeDeviceConfiguration (..),

    -- * Smart constructor
    mkSnowconeDeviceConfiguration,

    -- * Lenses
    sdcWirelessConnection,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.WirelessConnection

-- | Specifies the device configuration for an AWS Snowcone job.
--
-- /See:/ 'mkSnowconeDeviceConfiguration' smart constructor.
newtype SnowconeDeviceConfiguration = SnowconeDeviceConfiguration'
  { -- | Configures the wireless connection for the AWS Snowcone device.
    wirelessConnection :: Lude.Maybe WirelessConnection
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnowconeDeviceConfiguration' with the minimum fields required to make a request.
--
-- * 'wirelessConnection' - Configures the wireless connection for the AWS Snowcone device.
mkSnowconeDeviceConfiguration ::
  SnowconeDeviceConfiguration
mkSnowconeDeviceConfiguration =
  SnowconeDeviceConfiguration' {wirelessConnection = Lude.Nothing}

-- | Configures the wireless connection for the AWS Snowcone device.
--
-- /Note:/ Consider using 'wirelessConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcWirelessConnection :: Lens.Lens' SnowconeDeviceConfiguration (Lude.Maybe WirelessConnection)
sdcWirelessConnection = Lens.lens (wirelessConnection :: SnowconeDeviceConfiguration -> Lude.Maybe WirelessConnection) (\s a -> s {wirelessConnection = a} :: SnowconeDeviceConfiguration)
{-# DEPRECATED sdcWirelessConnection "Use generic-lens or generic-optics with 'wirelessConnection' instead." #-}

instance Lude.FromJSON SnowconeDeviceConfiguration where
  parseJSON =
    Lude.withObject
      "SnowconeDeviceConfiguration"
      ( \x ->
          SnowconeDeviceConfiguration'
            Lude.<$> (x Lude..:? "WirelessConnection")
      )

instance Lude.ToJSON SnowconeDeviceConfiguration where
  toJSON SnowconeDeviceConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("WirelessConnection" Lude..=) Lude.<$> wirelessConnection]
      )

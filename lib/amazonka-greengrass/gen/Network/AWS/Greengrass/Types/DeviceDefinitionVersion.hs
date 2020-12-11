-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeviceDefinitionVersion
  ( DeviceDefinitionVersion (..),

    -- * Smart constructor
    mkDeviceDefinitionVersion,

    -- * Lenses
    ddvDevices,
  )
where

import Network.AWS.Greengrass.Types.Device
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a device definition version.
--
-- /See:/ 'mkDeviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
  { devices ::
      Lude.Maybe [Device]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'devices' - A list of devices in the definition version.
mkDeviceDefinitionVersion ::
  DeviceDefinitionVersion
mkDeviceDefinitionVersion =
  DeviceDefinitionVersion' {devices = Lude.Nothing}

-- | A list of devices in the definition version.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDevices :: Lens.Lens' DeviceDefinitionVersion (Lude.Maybe [Device])
ddvDevices = Lens.lens (devices :: DeviceDefinitionVersion -> Lude.Maybe [Device]) (\s a -> s {devices = a} :: DeviceDefinitionVersion)
{-# DEPRECATED ddvDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

instance Lude.FromJSON DeviceDefinitionVersion where
  parseJSON =
    Lude.withObject
      "DeviceDefinitionVersion"
      ( \x ->
          DeviceDefinitionVersion'
            Lude.<$> (x Lude..:? "Devices" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DeviceDefinitionVersion where
  toJSON DeviceDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Devices" Lude..=) Lude.<$> devices])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSettings
  ( InputDeviceSettings (..),

    -- * Smart constructor
    mkInputDeviceSettings,

    -- * Lenses
    idssId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for an input device.
--
-- /See:/ 'mkInputDeviceSettings' smart constructor.
newtype InputDeviceSettings = InputDeviceSettings'
  { -- | The unique ID for the device.
    id :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputDeviceSettings' value with any optional fields omitted.
mkInputDeviceSettings ::
  InputDeviceSettings
mkInputDeviceSettings = InputDeviceSettings' {id = Core.Nothing}

-- | The unique ID for the device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idssId :: Lens.Lens' InputDeviceSettings (Core.Maybe Core.Text)
idssId = Lens.field @"id"
{-# DEPRECATED idssId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON InputDeviceSettings where
  toJSON InputDeviceSettings {..} =
    Core.object (Core.catMaybes [("id" Core..=) Core.<$> id])

instance Core.FromJSON InputDeviceSettings where
  parseJSON =
    Core.withObject "InputDeviceSettings" Core.$
      \x -> InputDeviceSettings' Core.<$> (x Core..:? "id")

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
import qualified Network.AWS.Prelude as Lude

-- | Settings for an input device.
--
-- /See:/ 'mkInputDeviceSettings' smart constructor.
newtype InputDeviceSettings = InputDeviceSettings'
  { id ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDeviceSettings' with the minimum fields required to make a request.
--
-- * 'id' - The unique ID for the device.
mkInputDeviceSettings ::
  InputDeviceSettings
mkInputDeviceSettings = InputDeviceSettings' {id = Lude.Nothing}

-- | The unique ID for the device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idssId :: Lens.Lens' InputDeviceSettings (Lude.Maybe Lude.Text)
idssId = Lens.lens (id :: InputDeviceSettings -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InputDeviceSettings)
{-# DEPRECATED idssId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON InputDeviceSettings where
  parseJSON =
    Lude.withObject
      "InputDeviceSettings"
      (\x -> InputDeviceSettings' Lude.<$> (x Lude..:? "id"))

instance Lude.ToJSON InputDeviceSettings where
  toJSON InputDeviceSettings' {..} =
    Lude.object (Lude.catMaybes [("id" Lude..=) Lude.<$> id])

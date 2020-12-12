{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceRequest
  ( InputDeviceRequest (..),

    -- * Smart constructor
    mkInputDeviceRequest,

    -- * Lenses
    idrId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for an input device.
--
-- /See:/ 'mkInputDeviceRequest' smart constructor.
newtype InputDeviceRequest = InputDeviceRequest'
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

-- | Creates a value of 'InputDeviceRequest' with the minimum fields required to make a request.
--
-- * 'id' - The unique ID for the device.
mkInputDeviceRequest ::
  InputDeviceRequest
mkInputDeviceRequest = InputDeviceRequest' {id = Lude.Nothing}

-- | The unique ID for the device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrId :: Lens.Lens' InputDeviceRequest (Lude.Maybe Lude.Text)
idrId = Lens.lens (id :: InputDeviceRequest -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InputDeviceRequest)
{-# DEPRECATED idrId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.ToJSON InputDeviceRequest where
  toJSON InputDeviceRequest' {..} =
    Lude.object (Lude.catMaybes [("id" Lude..=) Lude.<$> id])

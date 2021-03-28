{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDeviceRequest
  ( InputDeviceRequest (..)
  -- * Smart constructor
  , mkInputDeviceRequest
  -- * Lenses
  , idrId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for an input device.
--
-- /See:/ 'mkInputDeviceRequest' smart constructor.
newtype InputDeviceRequest = InputDeviceRequest'
  { id :: Core.Maybe Core.Text
    -- ^ The unique ID for the device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputDeviceRequest' value with any optional fields omitted.
mkInputDeviceRequest
    :: InputDeviceRequest
mkInputDeviceRequest = InputDeviceRequest'{id = Core.Nothing}

-- | The unique ID for the device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrId :: Lens.Lens' InputDeviceRequest (Core.Maybe Core.Text)
idrId = Lens.field @"id"
{-# INLINEABLE idrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON InputDeviceRequest where
        toJSON InputDeviceRequest{..}
          = Core.object (Core.catMaybes [("id" Core..=) Core.<$> id])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationSettings
  ( AccelerationSettings (..),

    -- * Smart constructor
    mkAccelerationSettings,

    -- * Lenses
    asMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AccelerationMode as Types
import qualified Network.AWS.Prelude as Core

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /See:/ 'mkAccelerationSettings' smart constructor.
newtype AccelerationSettings = AccelerationSettings'
  { -- | Specify the conditions when the service will run your job with accelerated transcoding.
    mode :: Types.AccelerationMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccelerationSettings' value with any optional fields omitted.
mkAccelerationSettings ::
  -- | 'mode'
  Types.AccelerationMode ->
  AccelerationSettings
mkAccelerationSettings mode = AccelerationSettings' {mode}

-- | Specify the conditions when the service will run your job with accelerated transcoding.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMode :: Lens.Lens' AccelerationSettings Types.AccelerationMode
asMode = Lens.field @"mode"
{-# DEPRECATED asMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Core.FromJSON AccelerationSettings where
  toJSON AccelerationSettings {..} =
    Core.object (Core.catMaybes [Core.Just ("mode" Core..= mode)])

instance Core.FromJSON AccelerationSettings where
  parseJSON =
    Core.withObject "AccelerationSettings" Core.$
      \x -> AccelerationSettings' Core.<$> (x Core..: "mode")

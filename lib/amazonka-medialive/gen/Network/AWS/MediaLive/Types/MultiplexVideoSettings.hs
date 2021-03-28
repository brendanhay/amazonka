{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexVideoSettings
  ( MultiplexVideoSettings (..)
  -- * Smart constructor
  , mkMultiplexVideoSettings
  -- * Lenses
  , mvsConstantBitrate
  , mvsStatmuxSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings as Types
import qualified Network.AWS.Prelude as Core

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'mkMultiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { constantBitrate :: Core.Maybe Core.Natural
    -- ^ The constant bitrate configuration for the video encode.
--
-- When this field is defined, StatmuxSettings must be undefined.
  , statmuxSettings :: Core.Maybe Types.MultiplexStatmuxVideoSettings
    -- ^ Statmux rate control settings.
--
-- When this field is defined, ConstantBitrate must be undefined.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexVideoSettings' value with any optional fields omitted.
mkMultiplexVideoSettings
    :: MultiplexVideoSettings
mkMultiplexVideoSettings
  = MultiplexVideoSettings'{constantBitrate = Core.Nothing,
                            statmuxSettings = Core.Nothing}

-- | The constant bitrate configuration for the video encode.
--
-- When this field is defined, StatmuxSettings must be undefined.
--
-- /Note:/ Consider using 'constantBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvsConstantBitrate :: Lens.Lens' MultiplexVideoSettings (Core.Maybe Core.Natural)
mvsConstantBitrate = Lens.field @"constantBitrate"
{-# INLINEABLE mvsConstantBitrate #-}
{-# DEPRECATED constantBitrate "Use generic-lens or generic-optics with 'constantBitrate' instead"  #-}

-- | Statmux rate control settings.
--
-- When this field is defined, ConstantBitrate must be undefined.
--
-- /Note:/ Consider using 'statmuxSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvsStatmuxSettings :: Lens.Lens' MultiplexVideoSettings (Core.Maybe Types.MultiplexStatmuxVideoSettings)
mvsStatmuxSettings = Lens.field @"statmuxSettings"
{-# INLINEABLE mvsStatmuxSettings #-}
{-# DEPRECATED statmuxSettings "Use generic-lens or generic-optics with 'statmuxSettings' instead"  #-}

instance Core.FromJSON MultiplexVideoSettings where
        toJSON MultiplexVideoSettings{..}
          = Core.object
              (Core.catMaybes
                 [("constantBitrate" Core..=) Core.<$> constantBitrate,
                  ("statmuxSettings" Core..=) Core.<$> statmuxSettings])

instance Core.FromJSON MultiplexVideoSettings where
        parseJSON
          = Core.withObject "MultiplexVideoSettings" Core.$
              \ x ->
                MultiplexVideoSettings' Core.<$>
                  (x Core..:? "constantBitrate") Core.<*>
                    x Core..:? "statmuxSettings"

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexVideoSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexVideoSettings
  ( MultiplexVideoSettings (..),

    -- * Smart constructor
    mkMultiplexVideoSettings,

    -- * Lenses
    mvsStatmuxSettings,
    mvsConstantBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexStatmuxVideoSettings
import qualified Network.AWS.Prelude as Lude

-- | The video configuration for each program in a multiplex.
--
-- /See:/ 'mkMultiplexVideoSettings' smart constructor.
data MultiplexVideoSettings = MultiplexVideoSettings'
  { statmuxSettings ::
      Lude.Maybe MultiplexStatmuxVideoSettings,
    constantBitrate :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexVideoSettings' with the minimum fields required to make a request.
--
-- * 'constantBitrate' - The constant bitrate configuration for the video encode.
--
-- When this field is defined, StatmuxSettings must be undefined.
-- * 'statmuxSettings' - Statmux rate control settings.
--
-- When this field is defined, ConstantBitrate must be undefined.
mkMultiplexVideoSettings ::
  MultiplexVideoSettings
mkMultiplexVideoSettings =
  MultiplexVideoSettings'
    { statmuxSettings = Lude.Nothing,
      constantBitrate = Lude.Nothing
    }

-- | Statmux rate control settings.
--
-- When this field is defined, ConstantBitrate must be undefined.
--
-- /Note:/ Consider using 'statmuxSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvsStatmuxSettings :: Lens.Lens' MultiplexVideoSettings (Lude.Maybe MultiplexStatmuxVideoSettings)
mvsStatmuxSettings = Lens.lens (statmuxSettings :: MultiplexVideoSettings -> Lude.Maybe MultiplexStatmuxVideoSettings) (\s a -> s {statmuxSettings = a} :: MultiplexVideoSettings)
{-# DEPRECATED mvsStatmuxSettings "Use generic-lens or generic-optics with 'statmuxSettings' instead." #-}

-- | The constant bitrate configuration for the video encode.
--
-- When this field is defined, StatmuxSettings must be undefined.
--
-- /Note:/ Consider using 'constantBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvsConstantBitrate :: Lens.Lens' MultiplexVideoSettings (Lude.Maybe Lude.Natural)
mvsConstantBitrate = Lens.lens (constantBitrate :: MultiplexVideoSettings -> Lude.Maybe Lude.Natural) (\s a -> s {constantBitrate = a} :: MultiplexVideoSettings)
{-# DEPRECATED mvsConstantBitrate "Use generic-lens or generic-optics with 'constantBitrate' instead." #-}

instance Lude.FromJSON MultiplexVideoSettings where
  parseJSON =
    Lude.withObject
      "MultiplexVideoSettings"
      ( \x ->
          MultiplexVideoSettings'
            Lude.<$> (x Lude..:? "statmuxSettings")
            Lude.<*> (x Lude..:? "constantBitrate")
      )

instance Lude.ToJSON MultiplexVideoSettings where
  toJSON MultiplexVideoSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("statmuxSettings" Lude..=) Lude.<$> statmuxSettings,
            ("constantBitrate" Lude..=) Lude.<$> constantBitrate
          ]
      )

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImscDestinationSettings
  ( ImscDestinationSettings (..),

    -- * Smart constructor
    mkImscDestinationSettings,

    -- * Lenses
    idsStylePassthrough,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ImscStylePassthrough
import qualified Network.AWS.Prelude as Lude

-- | Settings specific to IMSC caption outputs.
--
-- /See:/ 'mkImscDestinationSettings' smart constructor.
newtype ImscDestinationSettings = ImscDestinationSettings'
  { stylePassthrough ::
      Lude.Maybe ImscStylePassthrough
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImscDestinationSettings' with the minimum fields required to make a request.
--
-- * 'stylePassthrough' - Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
mkImscDestinationSettings ::
  ImscDestinationSettings
mkImscDestinationSettings =
  ImscDestinationSettings' {stylePassthrough = Lude.Nothing}

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
--
-- /Note:/ Consider using 'stylePassthrough' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsStylePassthrough :: Lens.Lens' ImscDestinationSettings (Lude.Maybe ImscStylePassthrough)
idsStylePassthrough = Lens.lens (stylePassthrough :: ImscDestinationSettings -> Lude.Maybe ImscStylePassthrough) (\s a -> s {stylePassthrough = a} :: ImscDestinationSettings)
{-# DEPRECATED idsStylePassthrough "Use generic-lens or generic-optics with 'stylePassthrough' instead." #-}

instance Lude.FromJSON ImscDestinationSettings where
  parseJSON =
    Lude.withObject
      "ImscDestinationSettings"
      ( \x ->
          ImscDestinationSettings' Lude.<$> (x Lude..:? "stylePassthrough")
      )

instance Lude.ToJSON ImscDestinationSettings where
  toJSON ImscDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("stylePassthrough" Lude..=) Lude.<$> stylePassthrough]
      )

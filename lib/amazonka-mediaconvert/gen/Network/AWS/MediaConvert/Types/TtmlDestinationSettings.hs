-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlDestinationSettings
  ( TtmlDestinationSettings (..),

    -- * Smart constructor
    mkTtmlDestinationSettings,

    -- * Lenses
    tdsStylePassthrough,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TtmlStylePassthrough
import qualified Network.AWS.Prelude as Lude

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /See:/ 'mkTtmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { stylePassthrough ::
      Lude.Maybe TtmlStylePassthrough
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- * 'stylePassthrough' - Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
mkTtmlDestinationSettings ::
  TtmlDestinationSettings
mkTtmlDestinationSettings =
  TtmlDestinationSettings' {stylePassthrough = Lude.Nothing}

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
--
-- /Note:/ Consider using 'stylePassthrough' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsStylePassthrough :: Lens.Lens' TtmlDestinationSettings (Lude.Maybe TtmlStylePassthrough)
tdsStylePassthrough = Lens.lens (stylePassthrough :: TtmlDestinationSettings -> Lude.Maybe TtmlStylePassthrough) (\s a -> s {stylePassthrough = a} :: TtmlDestinationSettings)
{-# DEPRECATED tdsStylePassthrough "Use generic-lens or generic-optics with 'stylePassthrough' instead." #-}

instance Lude.FromJSON TtmlDestinationSettings where
  parseJSON =
    Lude.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings' Lude.<$> (x Lude..:? "stylePassthrough")
      )

instance Lude.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("stylePassthrough" Lude..=) Lude.<$> stylePassthrough]
      )

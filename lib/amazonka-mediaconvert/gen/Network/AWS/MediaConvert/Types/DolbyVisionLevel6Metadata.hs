{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
  ( DolbyVisionLevel6Metadata (..),

    -- * Smart constructor
    mkDolbyVisionLevel6Metadata,

    -- * Lenses
    dvlmMaxFall,
    dvlmMaxCll,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
--
-- /See:/ 'mkDolbyVisionLevel6Metadata' smart constructor.
data DolbyVisionLevel6Metadata = DolbyVisionLevel6Metadata'
  { maxFall ::
      Lude.Maybe Lude.Natural,
    maxCll :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DolbyVisionLevel6Metadata' with the minimum fields required to make a request.
--
-- * 'maxCll' - Maximum Content Light Level. Static HDR metadata that corresponds to the brightest pixel in the entire stream. Measured in nits.
-- * 'maxFall' - Maximum Frame-Average Light Level. Static HDR metadata that corresponds to the highest frame-average brightness in the entire stream. Measured in nits.
mkDolbyVisionLevel6Metadata ::
  DolbyVisionLevel6Metadata
mkDolbyVisionLevel6Metadata =
  DolbyVisionLevel6Metadata'
    { maxFall = Lude.Nothing,
      maxCll = Lude.Nothing
    }

-- | Maximum Frame-Average Light Level. Static HDR metadata that corresponds to the highest frame-average brightness in the entire stream. Measured in nits.
--
-- /Note:/ Consider using 'maxFall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlmMaxFall :: Lens.Lens' DolbyVisionLevel6Metadata (Lude.Maybe Lude.Natural)
dvlmMaxFall = Lens.lens (maxFall :: DolbyVisionLevel6Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxFall = a} :: DolbyVisionLevel6Metadata)
{-# DEPRECATED dvlmMaxFall "Use generic-lens or generic-optics with 'maxFall' instead." #-}

-- | Maximum Content Light Level. Static HDR metadata that corresponds to the brightest pixel in the entire stream. Measured in nits.
--
-- /Note:/ Consider using 'maxCll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlmMaxCll :: Lens.Lens' DolbyVisionLevel6Metadata (Lude.Maybe Lude.Natural)
dvlmMaxCll = Lens.lens (maxCll :: DolbyVisionLevel6Metadata -> Lude.Maybe Lude.Natural) (\s a -> s {maxCll = a} :: DolbyVisionLevel6Metadata)
{-# DEPRECATED dvlmMaxCll "Use generic-lens or generic-optics with 'maxCll' instead." #-}

instance Lude.FromJSON DolbyVisionLevel6Metadata where
  parseJSON =
    Lude.withObject
      "DolbyVisionLevel6Metadata"
      ( \x ->
          DolbyVisionLevel6Metadata'
            Lude.<$> (x Lude..:? "maxFall") Lude.<*> (x Lude..:? "maxCll")
      )

instance Lude.ToJSON DolbyVisionLevel6Metadata where
  toJSON DolbyVisionLevel6Metadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxFall" Lude..=) Lude.<$> maxFall,
            ("maxCll" Lude..=) Lude.<$> maxCll
          ]
      )

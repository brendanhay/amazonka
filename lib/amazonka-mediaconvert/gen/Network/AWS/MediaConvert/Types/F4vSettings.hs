{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.F4vSettings
  ( F4vSettings (..),

    -- * Smart constructor
    mkF4vSettings,

    -- * Lenses
    fsMoovPlacement,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.F4vMoovPlacement
import qualified Network.AWS.Prelude as Lude

-- | Settings for F4v container
--
-- /See:/ 'mkF4vSettings' smart constructor.
newtype F4vSettings = F4vSettings'
  { -- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
    moovPlacement :: Lude.Maybe F4vMoovPlacement
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'F4vSettings' with the minimum fields required to make a request.
--
-- * 'moovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
mkF4vSettings ::
  F4vSettings
mkF4vSettings = F4vSettings' {moovPlacement = Lude.Nothing}

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
--
-- /Note:/ Consider using 'moovPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsMoovPlacement :: Lens.Lens' F4vSettings (Lude.Maybe F4vMoovPlacement)
fsMoovPlacement = Lens.lens (moovPlacement :: F4vSettings -> Lude.Maybe F4vMoovPlacement) (\s a -> s {moovPlacement = a} :: F4vSettings)
{-# DEPRECATED fsMoovPlacement "Use generic-lens or generic-optics with 'moovPlacement' instead." #-}

instance Lude.FromJSON F4vSettings where
  parseJSON =
    Lude.withObject
      "F4vSettings"
      (\x -> F4vSettings' Lude.<$> (x Lude..:? "moovPlacement"))

instance Lude.ToJSON F4vSettings where
  toJSON F4vSettings' {..} =
    Lude.object
      (Lude.catMaybes [("moovPlacement" Lude..=) Lude.<$> moovPlacement])

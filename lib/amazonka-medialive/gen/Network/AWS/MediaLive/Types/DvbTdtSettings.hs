{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbTdtSettings
  ( DvbTdtSettings (..),

    -- * Smart constructor
    mkDvbTdtSettings,

    -- * Lenses
    dtsRepInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'mkDvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { repInterval ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DvbTdtSettings' with the minimum fields required to make a request.
--
-- * 'repInterval' - The number of milliseconds between instances of this table in the output transport stream.
mkDvbTdtSettings ::
  DvbTdtSettings
mkDvbTdtSettings = DvbTdtSettings' {repInterval = Lude.Nothing}

-- | The number of milliseconds between instances of this table in the output transport stream.
--
-- /Note:/ Consider using 'repInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsRepInterval :: Lens.Lens' DvbTdtSettings (Lude.Maybe Lude.Natural)
dtsRepInterval = Lens.lens (repInterval :: DvbTdtSettings -> Lude.Maybe Lude.Natural) (\s a -> s {repInterval = a} :: DvbTdtSettings)
{-# DEPRECATED dtsRepInterval "Use generic-lens or generic-optics with 'repInterval' instead." #-}

instance Lude.FromJSON DvbTdtSettings where
  parseJSON =
    Lude.withObject
      "DvbTdtSettings"
      (\x -> DvbTdtSettings' Lude.<$> (x Lude..:? "repInterval"))

instance Lude.ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    Lude.object
      (Lude.catMaybes [("repInterval" Lude..=) Lude.<$> repInterval])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubSourceSettings
  ( DvbSubSourceSettings (..),

    -- * Smart constructor
    mkDvbSubSourceSettings,

    -- * Lenses
    dsssPid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | DVB Sub Source Settings
--
-- /See:/ 'mkDvbSubSourceSettings' smart constructor.
newtype DvbSubSourceSettings = DvbSubSourceSettings'
  { pid ::
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

-- | Creates a value of 'DvbSubSourceSettings' with the minimum fields required to make a request.
--
-- * 'pid' - When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
mkDvbSubSourceSettings ::
  DvbSubSourceSettings
mkDvbSubSourceSettings = DvbSubSourceSettings' {pid = Lude.Nothing}

-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
--
-- /Note:/ Consider using 'pid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsssPid :: Lens.Lens' DvbSubSourceSettings (Lude.Maybe Lude.Natural)
dsssPid = Lens.lens (pid :: DvbSubSourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {pid = a} :: DvbSubSourceSettings)
{-# DEPRECATED dsssPid "Use generic-lens or generic-optics with 'pid' instead." #-}

instance Lude.FromJSON DvbSubSourceSettings where
  parseJSON =
    Lude.withObject
      "DvbSubSourceSettings"
      (\x -> DvbSubSourceSettings' Lude.<$> (x Lude..:? "pid"))

instance Lude.ToJSON DvbSubSourceSettings where
  toJSON DvbSubSourceSettings' {..} =
    Lude.object (Lude.catMaybes [("pid" Lude..=) Lude.<$> pid])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailSettings
  ( AvailSettings (..),

    -- * Smart constructor
    mkAvailSettings,

    -- * Lenses
    asScte35SpliceInsert,
    asScte35TimeSignalApos,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsert
import Network.AWS.MediaLive.Types.Scte35TimeSignalApos
import qualified Network.AWS.Prelude as Lude

-- | Avail Settings
--
-- /See:/ 'mkAvailSettings' smart constructor.
data AvailSettings = AvailSettings'
  { scte35SpliceInsert ::
      Lude.Maybe Scte35SpliceInsert,
    scte35TimeSignalApos :: Lude.Maybe Scte35TimeSignalApos
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailSettings' with the minimum fields required to make a request.
--
-- * 'scte35SpliceInsert' - Undocumented field.
-- * 'scte35TimeSignalApos' - Undocumented field.
mkAvailSettings ::
  AvailSettings
mkAvailSettings =
  AvailSettings'
    { scte35SpliceInsert = Lude.Nothing,
      scte35TimeSignalApos = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35SpliceInsert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asScte35SpliceInsert :: Lens.Lens' AvailSettings (Lude.Maybe Scte35SpliceInsert)
asScte35SpliceInsert = Lens.lens (scte35SpliceInsert :: AvailSettings -> Lude.Maybe Scte35SpliceInsert) (\s a -> s {scte35SpliceInsert = a} :: AvailSettings)
{-# DEPRECATED asScte35SpliceInsert "Use generic-lens or generic-optics with 'scte35SpliceInsert' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35TimeSignalApos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asScte35TimeSignalApos :: Lens.Lens' AvailSettings (Lude.Maybe Scte35TimeSignalApos)
asScte35TimeSignalApos = Lens.lens (scte35TimeSignalApos :: AvailSettings -> Lude.Maybe Scte35TimeSignalApos) (\s a -> s {scte35TimeSignalApos = a} :: AvailSettings)
{-# DEPRECATED asScte35TimeSignalApos "Use generic-lens or generic-optics with 'scte35TimeSignalApos' instead." #-}

instance Lude.FromJSON AvailSettings where
  parseJSON =
    Lude.withObject
      "AvailSettings"
      ( \x ->
          AvailSettings'
            Lude.<$> (x Lude..:? "scte35SpliceInsert")
            Lude.<*> (x Lude..:? "scte35TimeSignalApos")
      )

instance Lude.ToJSON AvailSettings where
  toJSON AvailSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scte35SpliceInsert" Lude..=) Lude.<$> scte35SpliceInsert,
            ("scte35TimeSignalApos" Lude..=) Lude.<$> scte35TimeSignalApos
          ]
      )

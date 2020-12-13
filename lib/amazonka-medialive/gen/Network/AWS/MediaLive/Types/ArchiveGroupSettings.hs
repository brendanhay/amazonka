{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveGroupSettings
  ( ArchiveGroupSettings (..),

    -- * Smart constructor
    mkArchiveGroupSettings,

    -- * Lenses
    agsRolloverInterval,
    agsDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Lude

-- | Archive Group Settings
--
-- /See:/ 'mkArchiveGroupSettings' smart constructor.
data ArchiveGroupSettings = ArchiveGroupSettings'
  { -- | Number of seconds to write to archive file before closing and starting a new one.
    rolloverInterval :: Lude.Maybe Lude.Natural,
    -- | A directory and base filename where archive files should be written.
    destination :: OutputLocationRef
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchiveGroupSettings' with the minimum fields required to make a request.
--
-- * 'rolloverInterval' - Number of seconds to write to archive file before closing and starting a new one.
-- * 'destination' - A directory and base filename where archive files should be written.
mkArchiveGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  ArchiveGroupSettings
mkArchiveGroupSettings pDestination_ =
  ArchiveGroupSettings'
    { rolloverInterval = Lude.Nothing,
      destination = pDestination_
    }

-- | Number of seconds to write to archive file before closing and starting a new one.
--
-- /Note:/ Consider using 'rolloverInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agsRolloverInterval :: Lens.Lens' ArchiveGroupSettings (Lude.Maybe Lude.Natural)
agsRolloverInterval = Lens.lens (rolloverInterval :: ArchiveGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {rolloverInterval = a} :: ArchiveGroupSettings)
{-# DEPRECATED agsRolloverInterval "Use generic-lens or generic-optics with 'rolloverInterval' instead." #-}

-- | A directory and base filename where archive files should be written.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agsDestination :: Lens.Lens' ArchiveGroupSettings OutputLocationRef
agsDestination = Lens.lens (destination :: ArchiveGroupSettings -> OutputLocationRef) (\s a -> s {destination = a} :: ArchiveGroupSettings)
{-# DEPRECATED agsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON ArchiveGroupSettings where
  parseJSON =
    Lude.withObject
      "ArchiveGroupSettings"
      ( \x ->
          ArchiveGroupSettings'
            Lude.<$> (x Lude..:? "rolloverInterval") Lude.<*> (x Lude..: "destination")
      )

instance Lude.ToJSON ArchiveGroupSettings where
  toJSON ArchiveGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rolloverInterval" Lude..=) Lude.<$> rolloverInterval,
            Lude.Just ("destination" Lude..= destination)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
  ( Scte35TimeSignalScheduleActionSettings (..),

    -- * Smart constructor
    mkScte35TimeSignalScheduleActionSettings,

    -- * Lenses
    stssasScte35Descriptors,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35Descriptor
import qualified Network.AWS.Prelude as Lude

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'mkScte35TimeSignalScheduleActionSettings' smart constructor.
newtype Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'
  { -- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
    scte35Descriptors :: [Scte35Descriptor]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35TimeSignalScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'scte35Descriptors' - The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
mkScte35TimeSignalScheduleActionSettings ::
  Scte35TimeSignalScheduleActionSettings
mkScte35TimeSignalScheduleActionSettings =
  Scte35TimeSignalScheduleActionSettings'
    { scte35Descriptors =
        Lude.mempty
    }

-- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
--
-- /Note:/ Consider using 'scte35Descriptors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stssasScte35Descriptors :: Lens.Lens' Scte35TimeSignalScheduleActionSettings [Scte35Descriptor]
stssasScte35Descriptors = Lens.lens (scte35Descriptors :: Scte35TimeSignalScheduleActionSettings -> [Scte35Descriptor]) (\s a -> s {scte35Descriptors = a} :: Scte35TimeSignalScheduleActionSettings)
{-# DEPRECATED stssasScte35Descriptors "Use generic-lens or generic-optics with 'scte35Descriptors' instead." #-}

instance Lude.FromJSON Scte35TimeSignalScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "Scte35TimeSignalScheduleActionSettings"
      ( \x ->
          Scte35TimeSignalScheduleActionSettings'
            Lude.<$> (x Lude..:? "scte35Descriptors" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Scte35TimeSignalScheduleActionSettings where
  toJSON Scte35TimeSignalScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("scte35Descriptors" Lude..= scte35Descriptors)]
      )

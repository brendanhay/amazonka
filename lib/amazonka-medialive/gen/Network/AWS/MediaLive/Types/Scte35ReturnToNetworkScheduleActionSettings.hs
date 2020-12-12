{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
  ( Scte35ReturnToNetworkScheduleActionSettings (..),

    -- * Smart constructor
    mkScte35ReturnToNetworkScheduleActionSettings,

    -- * Lenses
    srtnsasSpliceEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'mkScte35ReturnToNetworkScheduleActionSettings' smart constructor.
newtype Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'
  { spliceEventId ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'Scte35ReturnToNetworkScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'spliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
mkScte35ReturnToNetworkScheduleActionSettings ::
  -- | 'spliceEventId'
  Lude.Natural ->
  Scte35ReturnToNetworkScheduleActionSettings
mkScte35ReturnToNetworkScheduleActionSettings pSpliceEventId_ =
  Scte35ReturnToNetworkScheduleActionSettings'
    { spliceEventId =
        pSpliceEventId_
    }

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
--
-- /Note:/ Consider using 'spliceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtnsasSpliceEventId :: Lens.Lens' Scte35ReturnToNetworkScheduleActionSettings Lude.Natural
srtnsasSpliceEventId = Lens.lens (spliceEventId :: Scte35ReturnToNetworkScheduleActionSettings -> Lude.Natural) (\s a -> s {spliceEventId = a} :: Scte35ReturnToNetworkScheduleActionSettings)
{-# DEPRECATED srtnsasSpliceEventId "Use generic-lens or generic-optics with 'spliceEventId' instead." #-}

instance Lude.FromJSON Scte35ReturnToNetworkScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "Scte35ReturnToNetworkScheduleActionSettings"
      ( \x ->
          Scte35ReturnToNetworkScheduleActionSettings'
            Lude.<$> (x Lude..: "spliceEventId")
      )

instance Lude.ToJSON Scte35ReturnToNetworkScheduleActionSettings where
  toJSON Scte35ReturnToNetworkScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("spliceEventId" Lude..= spliceEventId)]
      )

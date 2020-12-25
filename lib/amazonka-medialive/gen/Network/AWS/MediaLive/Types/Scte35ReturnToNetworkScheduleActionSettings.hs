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
import qualified Network.AWS.Prelude as Core

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'mkScte35ReturnToNetworkScheduleActionSettings' smart constructor.
newtype Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'
  { -- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
    spliceEventId :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35ReturnToNetworkScheduleActionSettings' value with any optional fields omitted.
mkScte35ReturnToNetworkScheduleActionSettings ::
  -- | 'spliceEventId'
  Core.Natural ->
  Scte35ReturnToNetworkScheduleActionSettings
mkScte35ReturnToNetworkScheduleActionSettings spliceEventId =
  Scte35ReturnToNetworkScheduleActionSettings' {spliceEventId}

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
--
-- /Note:/ Consider using 'spliceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtnsasSpliceEventId :: Lens.Lens' Scte35ReturnToNetworkScheduleActionSettings Core.Natural
srtnsasSpliceEventId = Lens.field @"spliceEventId"
{-# DEPRECATED srtnsasSpliceEventId "Use generic-lens or generic-optics with 'spliceEventId' instead." #-}

instance Core.FromJSON Scte35ReturnToNetworkScheduleActionSettings where
  toJSON Scte35ReturnToNetworkScheduleActionSettings {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("spliceEventId" Core..= spliceEventId)]
      )

instance Core.FromJSON Scte35ReturnToNetworkScheduleActionSettings where
  parseJSON =
    Core.withObject "Scte35ReturnToNetworkScheduleActionSettings" Core.$
      \x ->
        Scte35ReturnToNetworkScheduleActionSettings'
          Core.<$> (x Core..: "spliceEventId")

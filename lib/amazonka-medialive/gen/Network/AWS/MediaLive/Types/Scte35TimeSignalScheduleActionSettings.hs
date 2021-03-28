{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
  ( Scte35TimeSignalScheduleActionSettings (..)
  -- * Smart constructor
  , mkScte35TimeSignalScheduleActionSettings
  -- * Lenses
  , stssasScte35Descriptors
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35Descriptor as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'mkScte35TimeSignalScheduleActionSettings' smart constructor.
newtype Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'
  { scte35Descriptors :: [Types.Scte35Descriptor]
    -- ^ The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35TimeSignalScheduleActionSettings' value with any optional fields omitted.
mkScte35TimeSignalScheduleActionSettings
    :: Scte35TimeSignalScheduleActionSettings
mkScte35TimeSignalScheduleActionSettings
  = Scte35TimeSignalScheduleActionSettings'{scte35Descriptors =
                                              Core.mempty}

-- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
--
-- /Note:/ Consider using 'scte35Descriptors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stssasScte35Descriptors :: Lens.Lens' Scte35TimeSignalScheduleActionSettings [Types.Scte35Descriptor]
stssasScte35Descriptors = Lens.field @"scte35Descriptors"
{-# INLINEABLE stssasScte35Descriptors #-}
{-# DEPRECATED scte35Descriptors "Use generic-lens or generic-optics with 'scte35Descriptors' instead"  #-}

instance Core.FromJSON Scte35TimeSignalScheduleActionSettings where
        toJSON Scte35TimeSignalScheduleActionSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("scte35Descriptors" Core..= scte35Descriptors)])

instance Core.FromJSON Scte35TimeSignalScheduleActionSettings where
        parseJSON
          = Core.withObject "Scte35TimeSignalScheduleActionSettings" Core.$
              \ x ->
                Scte35TimeSignalScheduleActionSettings' Core.<$>
                  (x Core..:? "scte35Descriptors" Core..!= Core.mempty)

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
  ( PauseStateScheduleActionSettings (..)
  -- * Smart constructor
  , mkPauseStateScheduleActionSettings
  -- * Lenses
  , pssasPipelines
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.PipelinePauseStateSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'mkPauseStateScheduleActionSettings' smart constructor.
newtype PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'
  { pipelines :: Core.Maybe [Types.PipelinePauseStateSettings]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PauseStateScheduleActionSettings' value with any optional fields omitted.
mkPauseStateScheduleActionSettings
    :: PauseStateScheduleActionSettings
mkPauseStateScheduleActionSettings
  = PauseStateScheduleActionSettings'{pipelines = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pssasPipelines :: Lens.Lens' PauseStateScheduleActionSettings (Core.Maybe [Types.PipelinePauseStateSettings])
pssasPipelines = Lens.field @"pipelines"
{-# INLINEABLE pssasPipelines #-}
{-# DEPRECATED pipelines "Use generic-lens or generic-optics with 'pipelines' instead"  #-}

instance Core.FromJSON PauseStateScheduleActionSettings where
        toJSON PauseStateScheduleActionSettings{..}
          = Core.object
              (Core.catMaybes [("pipelines" Core..=) Core.<$> pipelines])

instance Core.FromJSON PauseStateScheduleActionSettings where
        parseJSON
          = Core.withObject "PauseStateScheduleActionSettings" Core.$
              \ x ->
                PauseStateScheduleActionSettings' Core.<$> (x Core..:? "pipelines")

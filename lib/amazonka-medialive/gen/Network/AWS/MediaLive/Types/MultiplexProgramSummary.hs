{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramSummary
  ( MultiplexProgramSummary (..),

    -- * Smart constructor
    mkMultiplexProgramSummary,

    -- * Lenses
    mpsChannelId,
    mpsProgramName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for MultiplexProgramSummary
--
-- /See:/ 'mkMultiplexProgramSummary' smart constructor.
data MultiplexProgramSummary = MultiplexProgramSummary'
  { -- | The MediaLive Channel associated with the program.
    channelId :: Core.Maybe Core.Text,
    -- | The name of the multiplex program.
    programName :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexProgramSummary' value with any optional fields omitted.
mkMultiplexProgramSummary ::
  MultiplexProgramSummary
mkMultiplexProgramSummary =
  MultiplexProgramSummary'
    { channelId = Core.Nothing,
      programName = Core.Nothing
    }

-- | The MediaLive Channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsChannelId :: Lens.Lens' MultiplexProgramSummary (Core.Maybe Core.Text)
mpsChannelId = Lens.field @"channelId"
{-# DEPRECATED mpsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsProgramName :: Lens.Lens' MultiplexProgramSummary (Core.Maybe Core.Text)
mpsProgramName = Lens.field @"programName"
{-# DEPRECATED mpsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Core.FromJSON MultiplexProgramSummary where
  parseJSON =
    Core.withObject "MultiplexProgramSummary" Core.$
      \x ->
        MultiplexProgramSummary'
          Core.<$> (x Core..:? "channelId") Core.<*> (x Core..:? "programName")

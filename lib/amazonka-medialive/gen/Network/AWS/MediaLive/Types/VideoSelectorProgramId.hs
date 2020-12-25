{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorProgramId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorProgramId
  ( VideoSelectorProgramId (..),

    -- * Smart constructor
    mkVideoSelectorProgramId,

    -- * Lenses
    vspiProgramId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Video Selector Program Id
--
-- /See:/ 'mkVideoSelectorProgramId' smart constructor.
newtype VideoSelectorProgramId = VideoSelectorProgramId'
  { -- | Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
    programId :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VideoSelectorProgramId' value with any optional fields omitted.
mkVideoSelectorProgramId ::
  VideoSelectorProgramId
mkVideoSelectorProgramId =
  VideoSelectorProgramId' {programId = Core.Nothing}

-- | Selects a specific program from within a multi-program transport stream. If the program doesn't exist, the first program within the transport stream will be selected by default.
--
-- /Note:/ Consider using 'programId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspiProgramId :: Lens.Lens' VideoSelectorProgramId (Core.Maybe Core.Natural)
vspiProgramId = Lens.field @"programId"
{-# DEPRECATED vspiProgramId "Use generic-lens or generic-optics with 'programId' instead." #-}

instance Core.FromJSON VideoSelectorProgramId where
  toJSON VideoSelectorProgramId {..} =
    Core.object
      (Core.catMaybes [("programId" Core..=) Core.<$> programId])

instance Core.FromJSON VideoSelectorProgramId where
  parseJSON =
    Core.withObject "VideoSelectorProgramId" Core.$
      \x -> VideoSelectorProgramId' Core.<$> (x Core..:? "programId")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputDetail
  ( OutputDetail (..),

    -- * Smart constructor
    mkOutputDetail,

    -- * Lenses
    odDurationInMs,
    odVideoDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.VideoDetail as Types
import qualified Network.AWS.Prelude as Core

-- | Details regarding output
--
-- /See:/ 'mkOutputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { -- | Duration in milliseconds
    durationInMs :: Core.Maybe Core.Int,
    -- | Contains details about the output's video stream
    videoDetails :: Core.Maybe Types.VideoDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDetail' value with any optional fields omitted.
mkOutputDetail ::
  OutputDetail
mkOutputDetail =
  OutputDetail'
    { durationInMs = Core.Nothing,
      videoDetails = Core.Nothing
    }

-- | Duration in milliseconds
--
-- /Note:/ Consider using 'durationInMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odDurationInMs :: Lens.Lens' OutputDetail (Core.Maybe Core.Int)
odDurationInMs = Lens.field @"durationInMs"
{-# DEPRECATED odDurationInMs "Use generic-lens or generic-optics with 'durationInMs' instead." #-}

-- | Contains details about the output's video stream
--
-- /Note:/ Consider using 'videoDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odVideoDetails :: Lens.Lens' OutputDetail (Core.Maybe Types.VideoDetail)
odVideoDetails = Lens.field @"videoDetails"
{-# DEPRECATED odVideoDetails "Use generic-lens or generic-optics with 'videoDetails' instead." #-}

instance Core.FromJSON OutputDetail where
  parseJSON =
    Core.withObject "OutputDetail" Core.$
      \x ->
        OutputDetail'
          Core.<$> (x Core..:? "durationInMs") Core.<*> (x Core..:? "videoDetails")

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
    odVideoDetails,
    odDurationInMs,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.VideoDetail
import qualified Network.AWS.Prelude as Lude

-- | Details regarding output
--
-- /See:/ 'mkOutputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { -- | Contains details about the output's video stream
    videoDetails :: Lude.Maybe VideoDetail,
    -- | Duration in milliseconds
    durationInMs :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDetail' with the minimum fields required to make a request.
--
-- * 'videoDetails' - Contains details about the output's video stream
-- * 'durationInMs' - Duration in milliseconds
mkOutputDetail ::
  OutputDetail
mkOutputDetail =
  OutputDetail'
    { videoDetails = Lude.Nothing,
      durationInMs = Lude.Nothing
    }

-- | Contains details about the output's video stream
--
-- /Note:/ Consider using 'videoDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odVideoDetails :: Lens.Lens' OutputDetail (Lude.Maybe VideoDetail)
odVideoDetails = Lens.lens (videoDetails :: OutputDetail -> Lude.Maybe VideoDetail) (\s a -> s {videoDetails = a} :: OutputDetail)
{-# DEPRECATED odVideoDetails "Use generic-lens or generic-optics with 'videoDetails' instead." #-}

-- | Duration in milliseconds
--
-- /Note:/ Consider using 'durationInMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odDurationInMs :: Lens.Lens' OutputDetail (Lude.Maybe Lude.Int)
odDurationInMs = Lens.lens (durationInMs :: OutputDetail -> Lude.Maybe Lude.Int) (\s a -> s {durationInMs = a} :: OutputDetail)
{-# DEPRECATED odDurationInMs "Use generic-lens or generic-optics with 'durationInMs' instead." #-}

instance Lude.FromJSON OutputDetail where
  parseJSON =
    Lude.withObject
      "OutputDetail"
      ( \x ->
          OutputDetail'
            Lude.<$> (x Lude..:? "videoDetails") Lude.<*> (x Lude..:? "durationInMs")
      )

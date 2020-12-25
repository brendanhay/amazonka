{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorSettings
  ( StreamProcessorSettings (..),

    -- * Smart constructor
    mkStreamProcessorSettings,

    -- * Lenses
    spsFaceSearch,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.FaceSearchSettings as Types

-- | Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.
--
-- /See:/ 'mkStreamProcessorSettings' smart constructor.
newtype StreamProcessorSettings = StreamProcessorSettings'
  { -- | Face search settings to use on a streaming video.
    faceSearch :: Core.Maybe Types.FaceSearchSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StreamProcessorSettings' value with any optional fields omitted.
mkStreamProcessorSettings ::
  StreamProcessorSettings
mkStreamProcessorSettings =
  StreamProcessorSettings' {faceSearch = Core.Nothing}

-- | Face search settings to use on a streaming video.
--
-- /Note:/ Consider using 'faceSearch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsFaceSearch :: Lens.Lens' StreamProcessorSettings (Core.Maybe Types.FaceSearchSettings)
spsFaceSearch = Lens.field @"faceSearch"
{-# DEPRECATED spsFaceSearch "Use generic-lens or generic-optics with 'faceSearch' instead." #-}

instance Core.FromJSON StreamProcessorSettings where
  toJSON StreamProcessorSettings {..} =
    Core.object
      (Core.catMaybes [("FaceSearch" Core..=) Core.<$> faceSearch])

instance Core.FromJSON StreamProcessorSettings where
  parseJSON =
    Core.withObject "StreamProcessorSettings" Core.$
      \x -> StreamProcessorSettings' Core.<$> (x Core..:? "FaceSearch")

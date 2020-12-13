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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.FaceSearchSettings

-- | Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.
--
-- /See:/ 'mkStreamProcessorSettings' smart constructor.
newtype StreamProcessorSettings = StreamProcessorSettings'
  { -- | Face search settings to use on a streaming video.
    faceSearch :: Lude.Maybe FaceSearchSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamProcessorSettings' with the minimum fields required to make a request.
--
-- * 'faceSearch' - Face search settings to use on a streaming video.
mkStreamProcessorSettings ::
  StreamProcessorSettings
mkStreamProcessorSettings =
  StreamProcessorSettings' {faceSearch = Lude.Nothing}

-- | Face search settings to use on a streaming video.
--
-- /Note:/ Consider using 'faceSearch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsFaceSearch :: Lens.Lens' StreamProcessorSettings (Lude.Maybe FaceSearchSettings)
spsFaceSearch = Lens.lens (faceSearch :: StreamProcessorSettings -> Lude.Maybe FaceSearchSettings) (\s a -> s {faceSearch = a} :: StreamProcessorSettings)
{-# DEPRECATED spsFaceSearch "Use generic-lens or generic-optics with 'faceSearch' instead." #-}

instance Lude.FromJSON StreamProcessorSettings where
  parseJSON =
    Lude.withObject
      "StreamProcessorSettings"
      ( \x ->
          StreamProcessorSettings' Lude.<$> (x Lude..:? "FaceSearch")
      )

instance Lude.ToJSON StreamProcessorSettings where
  toJSON StreamProcessorSettings' {..} =
    Lude.object
      (Lude.catMaybes [("FaceSearch" Lude..=) Lude.<$> faceSearch])

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Stream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Stream
  ( Stream (..),

    -- * Smart constructor
    mkStream,

    -- * Lenses
    sFileId,
    sStreamId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a group of files that can be streamed.
--
-- /See:/ 'mkStream' smart constructor.
data Stream = Stream'
  { -- | The ID of a file associated with a stream.
    fileId :: Lude.Maybe Lude.Natural,
    -- | The stream ID.
    streamId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stream' with the minimum fields required to make a request.
--
-- * 'fileId' - The ID of a file associated with a stream.
-- * 'streamId' - The stream ID.
mkStream ::
  Stream
mkStream = Stream' {fileId = Lude.Nothing, streamId = Lude.Nothing}

-- | The ID of a file associated with a stream.
--
-- /Note:/ Consider using 'fileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFileId :: Lens.Lens' Stream (Lude.Maybe Lude.Natural)
sFileId = Lens.lens (fileId :: Stream -> Lude.Maybe Lude.Natural) (\s a -> s {fileId = a} :: Stream)
{-# DEPRECATED sFileId "Use generic-lens or generic-optics with 'fileId' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStreamId :: Lens.Lens' Stream (Lude.Maybe Lude.Text)
sStreamId = Lens.lens (streamId :: Stream -> Lude.Maybe Lude.Text) (\s a -> s {streamId = a} :: Stream)
{-# DEPRECATED sStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

instance Lude.FromJSON Stream where
  parseJSON =
    Lude.withObject
      "Stream"
      ( \x ->
          Stream'
            Lude.<$> (x Lude..:? "fileId") Lude.<*> (x Lude..:? "streamId")
      )

instance Lude.ToJSON Stream where
  toJSON Stream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fileId" Lude..=) Lude.<$> fileId,
            ("streamId" Lude..=) Lude.<$> streamId
          ]
      )

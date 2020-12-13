{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Media
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Media
  ( Media (..),

    -- * Smart constructor
    mkMedia,

    -- * Lenses
    mMediaFileURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the input media file in a transcription request.
--
-- /See:/ 'mkMedia' smart constructor.
newtype Media = Media'
  { -- | The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is:
    --
    -- For example:
    -- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
    mediaFileURI :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Media' with the minimum fields required to make a request.
--
-- * 'mediaFileURI' - The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is:
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
mkMedia ::
  Media
mkMedia = Media' {mediaFileURI = Lude.Nothing}

-- | The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is:
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'mediaFileURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMediaFileURI :: Lens.Lens' Media (Lude.Maybe Lude.Text)
mMediaFileURI = Lens.lens (mediaFileURI :: Media -> Lude.Maybe Lude.Text) (\s a -> s {mediaFileURI = a} :: Media)
{-# DEPRECATED mMediaFileURI "Use generic-lens or generic-optics with 'mediaFileURI' instead." #-}

instance Lude.FromJSON Media where
  parseJSON =
    Lude.withObject
      "Media"
      (\x -> Media' Lude.<$> (x Lude..:? "MediaFileUri"))

instance Lude.ToJSON Media where
  toJSON Media' {..} =
    Lude.object
      (Lude.catMaybes [("MediaFileUri" Lude..=) Lude.<$> mediaFileURI])

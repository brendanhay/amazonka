{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Video
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Video
  ( Video (..),

    -- * Smart constructor
    mkVideo,

    -- * Lenses
    vS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.S3Object

-- | Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as 'StartLabelDetection' use @Video@ to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.
--
-- /See:/ 'mkVideo' smart constructor.
newtype Video = Video' {s3Object :: Lude.Maybe S3Object}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Video' with the minimum fields required to make a request.
--
-- * 's3Object' - The Amazon S3 bucket name and file name for the video.
mkVideo ::
  Video
mkVideo = Video' {s3Object = Lude.Nothing}

-- | The Amazon S3 bucket name and file name for the video.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vS3Object :: Lens.Lens' Video (Lude.Maybe S3Object)
vS3Object = Lens.lens (s3Object :: Video -> Lude.Maybe S3Object) (\s a -> s {s3Object = a} :: Video)
{-# DEPRECATED vS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Lude.ToJSON Video where
  toJSON Video' {..} =
    Lude.object
      (Lude.catMaybes [("S3Object" Lude..=) Lude.<$> s3Object])

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Video
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Video
  ( Video (..)
  -- * Smart constructor
  , mkVideo
  -- * Lenses
  , vS3Object
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.S3Object as Types

-- | Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as 'StartLabelDetection' use @Video@ to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.
--
-- /See:/ 'mkVideo' smart constructor.
newtype Video = Video'
  { s3Object :: Core.Maybe Types.S3Object
    -- ^ The Amazon S3 bucket name and file name for the video.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Video' value with any optional fields omitted.
mkVideo
    :: Video
mkVideo = Video'{s3Object = Core.Nothing}

-- | The Amazon S3 bucket name and file name for the video.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vS3Object :: Lens.Lens' Video (Core.Maybe Types.S3Object)
vS3Object = Lens.field @"s3Object"
{-# INLINEABLE vS3Object #-}
{-# DEPRECATED s3Object "Use generic-lens or generic-optics with 's3Object' instead"  #-}

instance Core.FromJSON Video where
        toJSON Video{..}
          = Core.object
              (Core.catMaybes [("S3Object" Core..=) Core.<$> s3Object])

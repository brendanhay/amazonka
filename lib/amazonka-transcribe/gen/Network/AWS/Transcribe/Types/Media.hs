{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Media
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Media
  ( Media (..)
  -- * Smart constructor
  , mkMedia
  -- * Lenses
  , mMediaFileUri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.Uri as Types

-- | Describes the input media file in a transcription request.
--
-- /See:/ 'mkMedia' smart constructor.
newtype Media = Media'
  { mediaFileUri :: Core.Maybe Types.Uri
    -- ^ The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is:
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Media' value with any optional fields omitted.
mkMedia
    :: Media
mkMedia = Media'{mediaFileUri = Core.Nothing}

-- | The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is:
--
-- For example:
-- For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
--
-- /Note:/ Consider using 'mediaFileUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMediaFileUri :: Lens.Lens' Media (Core.Maybe Types.Uri)
mMediaFileUri = Lens.field @"mediaFileUri"
{-# INLINEABLE mMediaFileUri #-}
{-# DEPRECATED mediaFileUri "Use generic-lens or generic-optics with 'mediaFileUri' instead"  #-}

instance Core.FromJSON Media where
        toJSON Media{..}
          = Core.object
              (Core.catMaybes [("MediaFileUri" Core..=) Core.<$> mediaFileUri])

instance Core.FromJSON Media where
        parseJSON
          = Core.withObject "Media" Core.$
              \ x -> Media' Core.<$> (x Core..:? "MediaFileUri")

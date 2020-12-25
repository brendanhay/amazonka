{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
  ( PipelineOutputConfig (..),

    -- * Smart constructor
    mkPipelineOutputConfig,

    -- * Lenses
    pocBucket,
    pocPermissions,
    pocStorageClass,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.Bucket as Types
import qualified Network.AWS.ElasticTranscoder.Types.Permission as Types
import qualified Network.AWS.ElasticTranscoder.Types.StorageClass as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @PipelineOutputConfig@ structure.
--
-- /See:/ 'mkPipelineOutputConfig' smart constructor.
data PipelineOutputConfig = PipelineOutputConfig'
  { -- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:
    --
    --
    --     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.
    --
    --
    --     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.
    --
    --
    --     * You do not want to specify the permissions that Elastic Transcoder grants to the files.
    --
    --
    --     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class.
    --
    --
    -- If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
    bucket :: Core.Maybe Types.Bucket,
    -- | Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
    --
    -- If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user.
    -- If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
    permissions :: Core.Maybe [Types.Permission],
    -- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
    storageClass :: Core.Maybe Types.StorageClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineOutputConfig' value with any optional fields omitted.
mkPipelineOutputConfig ::
  PipelineOutputConfig
mkPipelineOutputConfig =
  PipelineOutputConfig'
    { bucket = Core.Nothing,
      permissions = Core.Nothing,
      storageClass = Core.Nothing
    }

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:
--
--
--     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.
--
--
--     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.
--
--
--     * You do not want to specify the permissions that Elastic Transcoder grants to the files.
--
--
--     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class.
--
--
-- If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocBucket :: Lens.Lens' PipelineOutputConfig (Core.Maybe Types.Bucket)
pocBucket = Lens.field @"bucket"
{-# DEPRECATED pocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
-- If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user.
-- If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocPermissions :: Lens.Lens' PipelineOutputConfig (Core.Maybe [Types.Permission])
pocPermissions = Lens.field @"permissions"
{-# DEPRECATED pocPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocStorageClass :: Lens.Lens' PipelineOutputConfig (Core.Maybe Types.StorageClass)
pocStorageClass = Lens.field @"storageClass"
{-# DEPRECATED pocStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Core.FromJSON PipelineOutputConfig where
  toJSON PipelineOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("Bucket" Core..=) Core.<$> bucket,
            ("Permissions" Core..=) Core.<$> permissions,
            ("StorageClass" Core..=) Core.<$> storageClass
          ]
      )

instance Core.FromJSON PipelineOutputConfig where
  parseJSON =
    Core.withObject "PipelineOutputConfig" Core.$
      \x ->
        PipelineOutputConfig'
          Core.<$> (x Core..:? "Bucket")
          Core.<*> (x Core..:? "Permissions")
          Core.<*> (x Core..:? "StorageClass")

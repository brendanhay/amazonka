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
    pocStorageClass,
    pocPermissions,
  )
where

import Network.AWS.ElasticTranscoder.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @PipelineOutputConfig@ structure.
--
-- /See:/ 'mkPipelineOutputConfig' smart constructor.
data PipelineOutputConfig = PipelineOutputConfig'
  { bucket ::
      Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe Lude.Text,
    permissions :: Lude.Maybe [Permission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineOutputConfig' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:
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
-- * 'permissions' - Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
-- If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user.
-- If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
-- * 'storageClass' - The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
mkPipelineOutputConfig ::
  PipelineOutputConfig
mkPipelineOutputConfig =
  PipelineOutputConfig'
    { bucket = Lude.Nothing,
      storageClass = Lude.Nothing,
      permissions = Lude.Nothing
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
pocBucket :: Lens.Lens' PipelineOutputConfig (Lude.Maybe Lude.Text)
pocBucket = Lens.lens (bucket :: PipelineOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: PipelineOutputConfig)
{-# DEPRECATED pocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocStorageClass :: Lens.Lens' PipelineOutputConfig (Lude.Maybe Lude.Text)
pocStorageClass = Lens.lens (storageClass :: PipelineOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {storageClass = a} :: PipelineOutputConfig)
{-# DEPRECATED pocStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups.
--
-- If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user.
-- If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocPermissions :: Lens.Lens' PipelineOutputConfig (Lude.Maybe [Permission])
pocPermissions = Lens.lens (permissions :: PipelineOutputConfig -> Lude.Maybe [Permission]) (\s a -> s {permissions = a} :: PipelineOutputConfig)
{-# DEPRECATED pocPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

instance Lude.FromJSON PipelineOutputConfig where
  parseJSON =
    Lude.withObject
      "PipelineOutputConfig"
      ( \x ->
          PipelineOutputConfig'
            Lude.<$> (x Lude..:? "Bucket")
            Lude.<*> (x Lude..:? "StorageClass")
            Lude.<*> (x Lude..:? "Permissions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PipelineOutputConfig where
  toJSON PipelineOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Bucket" Lude..=) Lude.<$> bucket,
            ("StorageClass" Lude..=) Lude.<$> storageClass,
            ("Permissions" Lude..=) Lude.<$> permissions
          ]
      )

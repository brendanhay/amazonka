{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.Permission
import qualified Network.AWS.Lens as Lens

-- | The @PipelineOutputConfig@ structure.
--
-- /See:/ 'newPipelineOutputConfig' smart constructor.
data PipelineOutputConfig = PipelineOutputConfig'
  { -- | Optional. The @Permissions@ object specifies which users and\/or
    -- predefined Amazon S3 groups you want to have access to transcoded files
    -- and playlists, and the type of access you want them to have. You can
    -- grant permissions to a maximum of 30 users and\/or predefined Amazon S3
    -- groups.
    --
    -- If you include @Permissions@, Elastic Transcoder grants only the
    -- permissions that you specify. It does not grant full permissions to the
    -- owner of the role specified by @Role@. If you want that user to have
    -- full control, you must explicitly grant full control to the user.
    --
    -- If you omit @Permissions@, Elastic Transcoder grants full control over
    -- the transcoded files and playlists to the owner of the role specified by
    -- @Role@, and grants no other permissions to any other user or group.
    permissions :: Core.Maybe [Permission],
    -- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@, that you
    -- want Elastic Transcoder to assign to the video files and playlists that
    -- it stores in your Amazon S3 bucket.
    storageClass :: Core.Maybe Core.Text,
    -- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
    -- transcoded files. Specify this value when all of the following are true:
    --
    -- -   You want to save transcoded files, thumbnails (if any), and
    --     playlists (if any) together in one bucket.
    --
    -- -   You do not want to specify the users or groups who have access to
    --     the transcoded files, thumbnails, and playlists.
    --
    -- -   You do not want to specify the permissions that Elastic Transcoder
    --     grants to the files.
    --
    -- -   You want to associate the transcoded files and thumbnails with the
    --     Amazon S3 Standard storage class.
    --
    -- If you want to save transcoded files and playlists in one bucket and
    -- thumbnails in another bucket, specify which users can access the
    -- transcoded files or the permissions the users have, or change the Amazon
    -- S3 storage class, omit OutputBucket and specify values for
    -- @ContentConfig@ and @ThumbnailConfig@ instead.
    bucket :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'pipelineOutputConfig_permissions' - Optional. The @Permissions@ object specifies which users and\/or
-- predefined Amazon S3 groups you want to have access to transcoded files
-- and playlists, and the type of access you want them to have. You can
-- grant permissions to a maximum of 30 users and\/or predefined Amazon S3
-- groups.
--
-- If you include @Permissions@, Elastic Transcoder grants only the
-- permissions that you specify. It does not grant full permissions to the
-- owner of the role specified by @Role@. If you want that user to have
-- full control, you must explicitly grant full control to the user.
--
-- If you omit @Permissions@, Elastic Transcoder grants full control over
-- the transcoded files and playlists to the owner of the role specified by
-- @Role@, and grants no other permissions to any other user or group.
--
-- 'storageClass', 'pipelineOutputConfig_storageClass' - The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@, that you
-- want Elastic Transcoder to assign to the video files and playlists that
-- it stores in your Amazon S3 bucket.
--
-- 'bucket', 'pipelineOutputConfig_bucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. Specify this value when all of the following are true:
--
-- -   You want to save transcoded files, thumbnails (if any), and
--     playlists (if any) together in one bucket.
--
-- -   You do not want to specify the users or groups who have access to
--     the transcoded files, thumbnails, and playlists.
--
-- -   You do not want to specify the permissions that Elastic Transcoder
--     grants to the files.
--
-- -   You want to associate the transcoded files and thumbnails with the
--     Amazon S3 Standard storage class.
--
-- If you want to save transcoded files and playlists in one bucket and
-- thumbnails in another bucket, specify which users can access the
-- transcoded files or the permissions the users have, or change the Amazon
-- S3 storage class, omit OutputBucket and specify values for
-- @ContentConfig@ and @ThumbnailConfig@ instead.
newPipelineOutputConfig ::
  PipelineOutputConfig
newPipelineOutputConfig =
  PipelineOutputConfig'
    { permissions = Core.Nothing,
      storageClass = Core.Nothing,
      bucket = Core.Nothing
    }

-- | Optional. The @Permissions@ object specifies which users and\/or
-- predefined Amazon S3 groups you want to have access to transcoded files
-- and playlists, and the type of access you want them to have. You can
-- grant permissions to a maximum of 30 users and\/or predefined Amazon S3
-- groups.
--
-- If you include @Permissions@, Elastic Transcoder grants only the
-- permissions that you specify. It does not grant full permissions to the
-- owner of the role specified by @Role@. If you want that user to have
-- full control, you must explicitly grant full control to the user.
--
-- If you omit @Permissions@, Elastic Transcoder grants full control over
-- the transcoded files and playlists to the owner of the role specified by
-- @Role@, and grants no other permissions to any other user or group.
pipelineOutputConfig_permissions :: Lens.Lens' PipelineOutputConfig (Core.Maybe [Permission])
pipelineOutputConfig_permissions = Lens.lens (\PipelineOutputConfig' {permissions} -> permissions) (\s@PipelineOutputConfig' {} a -> s {permissions = a} :: PipelineOutputConfig) Core.. Lens.mapping Lens._Coerce

-- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@, that you
-- want Elastic Transcoder to assign to the video files and playlists that
-- it stores in your Amazon S3 bucket.
pipelineOutputConfig_storageClass :: Lens.Lens' PipelineOutputConfig (Core.Maybe Core.Text)
pipelineOutputConfig_storageClass = Lens.lens (\PipelineOutputConfig' {storageClass} -> storageClass) (\s@PipelineOutputConfig' {} a -> s {storageClass = a} :: PipelineOutputConfig)

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the
-- transcoded files. Specify this value when all of the following are true:
--
-- -   You want to save transcoded files, thumbnails (if any), and
--     playlists (if any) together in one bucket.
--
-- -   You do not want to specify the users or groups who have access to
--     the transcoded files, thumbnails, and playlists.
--
-- -   You do not want to specify the permissions that Elastic Transcoder
--     grants to the files.
--
-- -   You want to associate the transcoded files and thumbnails with the
--     Amazon S3 Standard storage class.
--
-- If you want to save transcoded files and playlists in one bucket and
-- thumbnails in another bucket, specify which users can access the
-- transcoded files or the permissions the users have, or change the Amazon
-- S3 storage class, omit OutputBucket and specify values for
-- @ContentConfig@ and @ThumbnailConfig@ instead.
pipelineOutputConfig_bucket :: Lens.Lens' PipelineOutputConfig (Core.Maybe Core.Text)
pipelineOutputConfig_bucket = Lens.lens (\PipelineOutputConfig' {bucket} -> bucket) (\s@PipelineOutputConfig' {} a -> s {bucket = a} :: PipelineOutputConfig)

instance Core.FromJSON PipelineOutputConfig where
  parseJSON =
    Core.withObject
      "PipelineOutputConfig"
      ( \x ->
          PipelineOutputConfig'
            Core.<$> (x Core..:? "Permissions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StorageClass")
            Core.<*> (x Core..:? "Bucket")
      )

instance Core.Hashable PipelineOutputConfig

instance Core.NFData PipelineOutputConfig

instance Core.ToJSON PipelineOutputConfig where
  toJSON PipelineOutputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Permissions" Core..=) Core.<$> permissions,
            ("StorageClass" Core..=) Core.<$> storageClass,
            ("Bucket" Core..=) Core.<$> bucket
          ]
      )

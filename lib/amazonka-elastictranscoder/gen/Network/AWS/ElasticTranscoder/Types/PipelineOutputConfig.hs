{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PipelineOutputConfig where

import Network.AWS.ElasticTranscoder.Types.Permission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @PipelineOutputConfig@ structure.
--
--
--
-- /See:/ 'pipelineOutputConfig' smart constructor.
data PipelineOutputConfig = PipelineOutputConfig'
  { _pocBucket ::
      !(Maybe Text),
    _pocStorageClass :: !(Maybe Text),
    _pocPermissions :: !(Maybe [Permission])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- * 'pocStorageClass' - The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- * 'pocPermissions' - Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups. If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user. If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
pipelineOutputConfig ::
  PipelineOutputConfig
pipelineOutputConfig =
  PipelineOutputConfig'
    { _pocBucket = Nothing,
      _pocStorageClass = Nothing,
      _pocPermissions = Nothing
    }

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
pocBucket :: Lens' PipelineOutputConfig (Maybe Text)
pocBucket = lens _pocBucket (\s a -> s {_pocBucket = a})

-- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
pocStorageClass :: Lens' PipelineOutputConfig (Maybe Text)
pocStorageClass = lens _pocStorageClass (\s a -> s {_pocStorageClass = a})

-- | Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups. If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user. If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
pocPermissions :: Lens' PipelineOutputConfig [Permission]
pocPermissions = lens _pocPermissions (\s a -> s {_pocPermissions = a}) . _Default . _Coerce

instance FromJSON PipelineOutputConfig where
  parseJSON =
    withObject
      "PipelineOutputConfig"
      ( \x ->
          PipelineOutputConfig'
            <$> (x .:? "Bucket")
            <*> (x .:? "StorageClass")
            <*> (x .:? "Permissions" .!= mempty)
      )

instance Hashable PipelineOutputConfig

instance NFData PipelineOutputConfig

instance ToJSON PipelineOutputConfig where
  toJSON PipelineOutputConfig' {..} =
    object
      ( catMaybes
          [ ("Bucket" .=) <$> _pocBucket,
            ("StorageClass" .=) <$> _pocStorageClass,
            ("Permissions" .=) <$> _pocPermissions
          ]
      )

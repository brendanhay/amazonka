{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Video
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Video where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as 'StartLabelDetection' use @Video@ to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.
--
--
--
-- /See:/ 'video' smart constructor.
newtype Video = Video' {_vS3Object :: Maybe S3Object}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Video' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vS3Object' - The Amazon S3 bucket name and file name for the video.
video ::
  Video
video = Video' {_vS3Object = Nothing}

-- | The Amazon S3 bucket name and file name for the video.
vS3Object :: Lens' Video (Maybe S3Object)
vS3Object = lens _vS3Object (\s a -> s {_vS3Object = a})

instance Hashable Video

instance NFData Video

instance ToJSON Video where
  toJSON Video' {..} =
    object (catMaybes [("S3Object" .=) <$> _vS3Object])

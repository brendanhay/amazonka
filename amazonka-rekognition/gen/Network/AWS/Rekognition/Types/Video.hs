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
-- Module      : Network.AWS.Rekognition.Types.Video
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Video where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.S3Object

-- | Video file stored in an Amazon S3 bucket. Amazon Rekognition video start
-- operations such as StartLabelDetection use @Video@ to specify a video
-- for analysis. The supported file formats are .mp4, .mov and .avi.
--
-- /See:/ 'newVideo' smart constructor.
data Video = Video'
  { -- | The Amazon S3 bucket name and file name for the video.
    s3Object :: Core.Maybe S3Object
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Video' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'video_s3Object' - The Amazon S3 bucket name and file name for the video.
newVideo ::
  Video
newVideo = Video' {s3Object = Core.Nothing}

-- | The Amazon S3 bucket name and file name for the video.
video_s3Object :: Lens.Lens' Video (Core.Maybe S3Object)
video_s3Object = Lens.lens (\Video' {s3Object} -> s3Object) (\s@Video' {} a -> s {s3Object = a} :: Video)

instance Core.Hashable Video

instance Core.NFData Video

instance Core.ToJSON Video where
  toJSON Video' {..} =
    Core.object
      ( Core.catMaybes
          [("S3Object" Core..=) Core.<$> s3Object]
      )

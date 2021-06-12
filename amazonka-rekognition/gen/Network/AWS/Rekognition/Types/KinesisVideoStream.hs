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
-- Module      : Network.AWS.Rekognition.Types.KinesisVideoStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.KinesisVideoStream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Kinesis video stream stream that provides the source streaming video for
-- a Amazon Rekognition Video stream processor. For more information, see
-- CreateStreamProcessor in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newKinesisVideoStream' smart constructor.
data KinesisVideoStream = KinesisVideoStream'
  { -- | ARN of the Kinesis video stream stream that streams the source video.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisVideoStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'kinesisVideoStream_arn' - ARN of the Kinesis video stream stream that streams the source video.
newKinesisVideoStream ::
  KinesisVideoStream
newKinesisVideoStream =
  KinesisVideoStream' {arn = Core.Nothing}

-- | ARN of the Kinesis video stream stream that streams the source video.
kinesisVideoStream_arn :: Lens.Lens' KinesisVideoStream (Core.Maybe Core.Text)
kinesisVideoStream_arn = Lens.lens (\KinesisVideoStream' {arn} -> arn) (\s@KinesisVideoStream' {} a -> s {arn = a} :: KinesisVideoStream)

instance Core.FromJSON KinesisVideoStream where
  parseJSON =
    Core.withObject
      "KinesisVideoStream"
      ( \x ->
          KinesisVideoStream' Core.<$> (x Core..:? "Arn")
      )

instance Core.Hashable KinesisVideoStream

instance Core.NFData KinesisVideoStream

instance Core.ToJSON KinesisVideoStream where
  toJSON KinesisVideoStream' {..} =
    Core.object
      (Core.catMaybes [("Arn" Core..=) Core.<$> arn])

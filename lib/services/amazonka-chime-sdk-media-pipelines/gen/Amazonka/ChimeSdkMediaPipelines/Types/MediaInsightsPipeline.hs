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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipeline where

import Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamRecordingSourceRuntimeConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamSourceRuntimeConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
import Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkRuntimeConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A media pipeline that streams call analytics data.
--
-- /See:/ 'newMediaInsightsPipeline' smart constructor.
data MediaInsightsPipeline = MediaInsightsPipeline'
  { -- | The time at which the media insights pipeline was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The runtime configuration settings for a Kinesis recording video stream
    -- in a media insights pipeline.
    kinesisVideoStreamRecordingSourceRuntimeConfiguration :: Prelude.Maybe KinesisVideoStreamRecordingSourceRuntimeConfiguration,
    -- | The configuration settings for a Kinesis runtime video stream in a media
    -- insights pipeline.
    kinesisVideoStreamSourceRuntimeConfiguration :: Prelude.Maybe KinesisVideoStreamSourceRuntimeConfiguration,
    -- | The ARN of a media insight pipeline\'s configuration settings.
    mediaInsightsPipelineConfigurationArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The runtime metadata of a media insights pipeline.
    mediaInsightsRuntimeMetadata :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ARN of a media insights pipeline.
    mediaPipelineArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of a media insights pipeline.
    mediaPipelineId :: Prelude.Maybe Prelude.Text,
    -- | The runtime configuration of the Amazon S3 bucket that stores recordings
    -- in a media insights pipeline.
    s3RecordingSinkRuntimeConfiguration :: Prelude.Maybe S3RecordingSinkRuntimeConfiguration,
    -- | The status of a media insights pipeline.
    status :: Prelude.Maybe MediaPipelineStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaInsightsPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'mediaInsightsPipeline_createdTimestamp' - The time at which the media insights pipeline was created.
--
-- 'kinesisVideoStreamRecordingSourceRuntimeConfiguration', 'mediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration' - The runtime configuration settings for a Kinesis recording video stream
-- in a media insights pipeline.
--
-- 'kinesisVideoStreamSourceRuntimeConfiguration', 'mediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration' - The configuration settings for a Kinesis runtime video stream in a media
-- insights pipeline.
--
-- 'mediaInsightsPipelineConfigurationArn', 'mediaInsightsPipeline_mediaInsightsPipelineConfigurationArn' - The ARN of a media insight pipeline\'s configuration settings.
--
-- 'mediaInsightsRuntimeMetadata', 'mediaInsightsPipeline_mediaInsightsRuntimeMetadata' - The runtime metadata of a media insights pipeline.
--
-- 'mediaPipelineArn', 'mediaInsightsPipeline_mediaPipelineArn' - The ARN of a media insights pipeline.
--
-- 'mediaPipelineId', 'mediaInsightsPipeline_mediaPipelineId' - The ID of a media insights pipeline.
--
-- 's3RecordingSinkRuntimeConfiguration', 'mediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration' - The runtime configuration of the Amazon S3 bucket that stores recordings
-- in a media insights pipeline.
--
-- 'status', 'mediaInsightsPipeline_status' - The status of a media insights pipeline.
newMediaInsightsPipeline ::
  MediaInsightsPipeline
newMediaInsightsPipeline =
  MediaInsightsPipeline'
    { createdTimestamp =
        Prelude.Nothing,
      kinesisVideoStreamRecordingSourceRuntimeConfiguration =
        Prelude.Nothing,
      kinesisVideoStreamSourceRuntimeConfiguration =
        Prelude.Nothing,
      mediaInsightsPipelineConfigurationArn =
        Prelude.Nothing,
      mediaInsightsRuntimeMetadata = Prelude.Nothing,
      mediaPipelineArn = Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing,
      s3RecordingSinkRuntimeConfiguration =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time at which the media insights pipeline was created.
mediaInsightsPipeline_createdTimestamp :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe Prelude.UTCTime)
mediaInsightsPipeline_createdTimestamp = Lens.lens (\MediaInsightsPipeline' {createdTimestamp} -> createdTimestamp) (\s@MediaInsightsPipeline' {} a -> s {createdTimestamp = a} :: MediaInsightsPipeline) Prelude.. Lens.mapping Data._Time

-- | The runtime configuration settings for a Kinesis recording video stream
-- in a media insights pipeline.
mediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe KinesisVideoStreamRecordingSourceRuntimeConfiguration)
mediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration = Lens.lens (\MediaInsightsPipeline' {kinesisVideoStreamRecordingSourceRuntimeConfiguration} -> kinesisVideoStreamRecordingSourceRuntimeConfiguration) (\s@MediaInsightsPipeline' {} a -> s {kinesisVideoStreamRecordingSourceRuntimeConfiguration = a} :: MediaInsightsPipeline)

-- | The configuration settings for a Kinesis runtime video stream in a media
-- insights pipeline.
mediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe KinesisVideoStreamSourceRuntimeConfiguration)
mediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration = Lens.lens (\MediaInsightsPipeline' {kinesisVideoStreamSourceRuntimeConfiguration} -> kinesisVideoStreamSourceRuntimeConfiguration) (\s@MediaInsightsPipeline' {} a -> s {kinesisVideoStreamSourceRuntimeConfiguration = a} :: MediaInsightsPipeline)

-- | The ARN of a media insight pipeline\'s configuration settings.
mediaInsightsPipeline_mediaInsightsPipelineConfigurationArn :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe Prelude.Text)
mediaInsightsPipeline_mediaInsightsPipelineConfigurationArn = Lens.lens (\MediaInsightsPipeline' {mediaInsightsPipelineConfigurationArn} -> mediaInsightsPipelineConfigurationArn) (\s@MediaInsightsPipeline' {} a -> s {mediaInsightsPipelineConfigurationArn = a} :: MediaInsightsPipeline) Prelude.. Lens.mapping Data._Sensitive

-- | The runtime metadata of a media insights pipeline.
mediaInsightsPipeline_mediaInsightsRuntimeMetadata :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
mediaInsightsPipeline_mediaInsightsRuntimeMetadata = Lens.lens (\MediaInsightsPipeline' {mediaInsightsRuntimeMetadata} -> mediaInsightsRuntimeMetadata) (\s@MediaInsightsPipeline' {} a -> s {mediaInsightsRuntimeMetadata = a} :: MediaInsightsPipeline) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ARN of a media insights pipeline.
mediaInsightsPipeline_mediaPipelineArn :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe Prelude.Text)
mediaInsightsPipeline_mediaPipelineArn = Lens.lens (\MediaInsightsPipeline' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaInsightsPipeline' {} a -> s {mediaPipelineArn = a} :: MediaInsightsPipeline) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of a media insights pipeline.
mediaInsightsPipeline_mediaPipelineId :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe Prelude.Text)
mediaInsightsPipeline_mediaPipelineId = Lens.lens (\MediaInsightsPipeline' {mediaPipelineId} -> mediaPipelineId) (\s@MediaInsightsPipeline' {} a -> s {mediaPipelineId = a} :: MediaInsightsPipeline)

-- | The runtime configuration of the Amazon S3 bucket that stores recordings
-- in a media insights pipeline.
mediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe S3RecordingSinkRuntimeConfiguration)
mediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration = Lens.lens (\MediaInsightsPipeline' {s3RecordingSinkRuntimeConfiguration} -> s3RecordingSinkRuntimeConfiguration) (\s@MediaInsightsPipeline' {} a -> s {s3RecordingSinkRuntimeConfiguration = a} :: MediaInsightsPipeline)

-- | The status of a media insights pipeline.
mediaInsightsPipeline_status :: Lens.Lens' MediaInsightsPipeline (Prelude.Maybe MediaPipelineStatus)
mediaInsightsPipeline_status = Lens.lens (\MediaInsightsPipeline' {status} -> status) (\s@MediaInsightsPipeline' {} a -> s {status = a} :: MediaInsightsPipeline)

instance Data.FromJSON MediaInsightsPipeline where
  parseJSON =
    Data.withObject
      "MediaInsightsPipeline"
      ( \x ->
          MediaInsightsPipeline'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> ( x
                            Data..:? "KinesisVideoStreamRecordingSourceRuntimeConfiguration"
                        )
            Prelude.<*> ( x
                            Data..:? "KinesisVideoStreamSourceRuntimeConfiguration"
                        )
            Prelude.<*> (x Data..:? "MediaInsightsPipelineConfigurationArn")
            Prelude.<*> ( x
                            Data..:? "MediaInsightsRuntimeMetadata"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MediaPipelineArn")
            Prelude.<*> (x Data..:? "MediaPipelineId")
            Prelude.<*> (x Data..:? "S3RecordingSinkRuntimeConfiguration")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable MediaInsightsPipeline where
  hashWithSalt _salt MediaInsightsPipeline' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` kinesisVideoStreamRecordingSourceRuntimeConfiguration
      `Prelude.hashWithSalt` kinesisVideoStreamSourceRuntimeConfiguration
      `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationArn
      `Prelude.hashWithSalt` mediaInsightsRuntimeMetadata
      `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` mediaPipelineId
      `Prelude.hashWithSalt` s3RecordingSinkRuntimeConfiguration
      `Prelude.hashWithSalt` status

instance Prelude.NFData MediaInsightsPipeline where
  rnf MediaInsightsPipeline' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf
        kinesisVideoStreamRecordingSourceRuntimeConfiguration
      `Prelude.seq` Prelude.rnf
        kinesisVideoStreamSourceRuntimeConfiguration
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationArn
      `Prelude.seq` Prelude.rnf mediaInsightsRuntimeMetadata
      `Prelude.seq` Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf mediaPipelineId
      `Prelude.seq` Prelude.rnf s3RecordingSinkRuntimeConfiguration
      `Prelude.seq` Prelude.rnf status

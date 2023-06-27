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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElement where

import Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeCallAnalyticsProcessorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.AmazonTranscribeProcessorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.KinesisDataStreamSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LambdaFunctionSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElementType
import Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SnsTopicSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.SqsQueueSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.VoiceAnalyticsProcessorConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An element in a media insights pipeline configuration.
--
-- /See:/ 'newMediaInsightsPipelineConfigurationElement' smart constructor.
data MediaInsightsPipelineConfigurationElement = MediaInsightsPipelineConfigurationElement'
  { -- | The analytics configuration settings for transcribing audio in a media
    -- insights pipeline configuration element.
    amazonTranscribeCallAnalyticsProcessorConfiguration :: Prelude.Maybe AmazonTranscribeCallAnalyticsProcessorConfiguration,
    -- | The transcription processor configuration settings in a media insights
    -- pipeline configuration element.
    amazonTranscribeProcessorConfiguration :: Prelude.Maybe AmazonTranscribeProcessorConfiguration,
    -- | The configuration settings for the Kinesis Data Stream Sink in a media
    -- insights pipeline configuration element.
    kinesisDataStreamSinkConfiguration :: Prelude.Maybe KinesisDataStreamSinkConfiguration,
    -- | The configuration settings for the Amazon Web Services Lambda sink in a
    -- media insights pipeline configuration element.
    lambdaFunctionSinkConfiguration :: Prelude.Maybe LambdaFunctionSinkConfiguration,
    -- | The configuration settings for the Amazon S3 recording bucket in a media
    -- insights pipeline configuration element.
    s3RecordingSinkConfiguration :: Prelude.Maybe S3RecordingSinkConfiguration,
    -- | The configuration settings for an SNS topic sink in a media insights
    -- pipeline configuration element.
    snsTopicSinkConfiguration :: Prelude.Maybe SnsTopicSinkConfiguration,
    -- | The configuration settings for an SQS queue sink in a media insights
    -- pipeline configuration element.
    sqsQueueSinkConfiguration :: Prelude.Maybe SqsQueueSinkConfiguration,
    -- | The voice analytics configuration settings in a media insights pipeline
    -- configuration element.
    voiceAnalyticsProcessorConfiguration :: Prelude.Maybe VoiceAnalyticsProcessorConfiguration,
    -- | The element type.
    type' :: MediaInsightsPipelineConfigurationElementType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaInsightsPipelineConfigurationElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonTranscribeCallAnalyticsProcessorConfiguration', 'mediaInsightsPipelineConfigurationElement_amazonTranscribeCallAnalyticsProcessorConfiguration' - The analytics configuration settings for transcribing audio in a media
-- insights pipeline configuration element.
--
-- 'amazonTranscribeProcessorConfiguration', 'mediaInsightsPipelineConfigurationElement_amazonTranscribeProcessorConfiguration' - The transcription processor configuration settings in a media insights
-- pipeline configuration element.
--
-- 'kinesisDataStreamSinkConfiguration', 'mediaInsightsPipelineConfigurationElement_kinesisDataStreamSinkConfiguration' - The configuration settings for the Kinesis Data Stream Sink in a media
-- insights pipeline configuration element.
--
-- 'lambdaFunctionSinkConfiguration', 'mediaInsightsPipelineConfigurationElement_lambdaFunctionSinkConfiguration' - The configuration settings for the Amazon Web Services Lambda sink in a
-- media insights pipeline configuration element.
--
-- 's3RecordingSinkConfiguration', 'mediaInsightsPipelineConfigurationElement_s3RecordingSinkConfiguration' - The configuration settings for the Amazon S3 recording bucket in a media
-- insights pipeline configuration element.
--
-- 'snsTopicSinkConfiguration', 'mediaInsightsPipelineConfigurationElement_snsTopicSinkConfiguration' - The configuration settings for an SNS topic sink in a media insights
-- pipeline configuration element.
--
-- 'sqsQueueSinkConfiguration', 'mediaInsightsPipelineConfigurationElement_sqsQueueSinkConfiguration' - The configuration settings for an SQS queue sink in a media insights
-- pipeline configuration element.
--
-- 'voiceAnalyticsProcessorConfiguration', 'mediaInsightsPipelineConfigurationElement_voiceAnalyticsProcessorConfiguration' - The voice analytics configuration settings in a media insights pipeline
-- configuration element.
--
-- 'type'', 'mediaInsightsPipelineConfigurationElement_type' - The element type.
newMediaInsightsPipelineConfigurationElement ::
  -- | 'type''
  MediaInsightsPipelineConfigurationElementType ->
  MediaInsightsPipelineConfigurationElement
newMediaInsightsPipelineConfigurationElement pType_ =
  MediaInsightsPipelineConfigurationElement'
    { amazonTranscribeCallAnalyticsProcessorConfiguration =
        Prelude.Nothing,
      amazonTranscribeProcessorConfiguration =
        Prelude.Nothing,
      kinesisDataStreamSinkConfiguration =
        Prelude.Nothing,
      lambdaFunctionSinkConfiguration =
        Prelude.Nothing,
      s3RecordingSinkConfiguration =
        Prelude.Nothing,
      snsTopicSinkConfiguration =
        Prelude.Nothing,
      sqsQueueSinkConfiguration =
        Prelude.Nothing,
      voiceAnalyticsProcessorConfiguration =
        Prelude.Nothing,
      type' = pType_
    }

-- | The analytics configuration settings for transcribing audio in a media
-- insights pipeline configuration element.
mediaInsightsPipelineConfigurationElement_amazonTranscribeCallAnalyticsProcessorConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe AmazonTranscribeCallAnalyticsProcessorConfiguration)
mediaInsightsPipelineConfigurationElement_amazonTranscribeCallAnalyticsProcessorConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {amazonTranscribeCallAnalyticsProcessorConfiguration} -> amazonTranscribeCallAnalyticsProcessorConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {amazonTranscribeCallAnalyticsProcessorConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The transcription processor configuration settings in a media insights
-- pipeline configuration element.
mediaInsightsPipelineConfigurationElement_amazonTranscribeProcessorConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe AmazonTranscribeProcessorConfiguration)
mediaInsightsPipelineConfigurationElement_amazonTranscribeProcessorConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {amazonTranscribeProcessorConfiguration} -> amazonTranscribeProcessorConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {amazonTranscribeProcessorConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The configuration settings for the Kinesis Data Stream Sink in a media
-- insights pipeline configuration element.
mediaInsightsPipelineConfigurationElement_kinesisDataStreamSinkConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe KinesisDataStreamSinkConfiguration)
mediaInsightsPipelineConfigurationElement_kinesisDataStreamSinkConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {kinesisDataStreamSinkConfiguration} -> kinesisDataStreamSinkConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {kinesisDataStreamSinkConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The configuration settings for the Amazon Web Services Lambda sink in a
-- media insights pipeline configuration element.
mediaInsightsPipelineConfigurationElement_lambdaFunctionSinkConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe LambdaFunctionSinkConfiguration)
mediaInsightsPipelineConfigurationElement_lambdaFunctionSinkConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {lambdaFunctionSinkConfiguration} -> lambdaFunctionSinkConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {lambdaFunctionSinkConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The configuration settings for the Amazon S3 recording bucket in a media
-- insights pipeline configuration element.
mediaInsightsPipelineConfigurationElement_s3RecordingSinkConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe S3RecordingSinkConfiguration)
mediaInsightsPipelineConfigurationElement_s3RecordingSinkConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {s3RecordingSinkConfiguration} -> s3RecordingSinkConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {s3RecordingSinkConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The configuration settings for an SNS topic sink in a media insights
-- pipeline configuration element.
mediaInsightsPipelineConfigurationElement_snsTopicSinkConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe SnsTopicSinkConfiguration)
mediaInsightsPipelineConfigurationElement_snsTopicSinkConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {snsTopicSinkConfiguration} -> snsTopicSinkConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {snsTopicSinkConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The configuration settings for an SQS queue sink in a media insights
-- pipeline configuration element.
mediaInsightsPipelineConfigurationElement_sqsQueueSinkConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe SqsQueueSinkConfiguration)
mediaInsightsPipelineConfigurationElement_sqsQueueSinkConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {sqsQueueSinkConfiguration} -> sqsQueueSinkConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {sqsQueueSinkConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The voice analytics configuration settings in a media insights pipeline
-- configuration element.
mediaInsightsPipelineConfigurationElement_voiceAnalyticsProcessorConfiguration :: Lens.Lens' MediaInsightsPipelineConfigurationElement (Prelude.Maybe VoiceAnalyticsProcessorConfiguration)
mediaInsightsPipelineConfigurationElement_voiceAnalyticsProcessorConfiguration = Lens.lens (\MediaInsightsPipelineConfigurationElement' {voiceAnalyticsProcessorConfiguration} -> voiceAnalyticsProcessorConfiguration) (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {voiceAnalyticsProcessorConfiguration = a} :: MediaInsightsPipelineConfigurationElement)

-- | The element type.
mediaInsightsPipelineConfigurationElement_type :: Lens.Lens' MediaInsightsPipelineConfigurationElement MediaInsightsPipelineConfigurationElementType
mediaInsightsPipelineConfigurationElement_type = Lens.lens (\MediaInsightsPipelineConfigurationElement' {type'} -> type') (\s@MediaInsightsPipelineConfigurationElement' {} a -> s {type' = a} :: MediaInsightsPipelineConfigurationElement)

instance
  Data.FromJSON
    MediaInsightsPipelineConfigurationElement
  where
  parseJSON =
    Data.withObject
      "MediaInsightsPipelineConfigurationElement"
      ( \x ->
          MediaInsightsPipelineConfigurationElement'
            Prelude.<$> ( x
                            Data..:? "AmazonTranscribeCallAnalyticsProcessorConfiguration"
                        )
            Prelude.<*> (x Data..:? "AmazonTranscribeProcessorConfiguration")
            Prelude.<*> (x Data..:? "KinesisDataStreamSinkConfiguration")
            Prelude.<*> (x Data..:? "LambdaFunctionSinkConfiguration")
            Prelude.<*> (x Data..:? "S3RecordingSinkConfiguration")
            Prelude.<*> (x Data..:? "SnsTopicSinkConfiguration")
            Prelude.<*> (x Data..:? "SqsQueueSinkConfiguration")
            Prelude.<*> (x Data..:? "VoiceAnalyticsProcessorConfiguration")
            Prelude.<*> (x Data..: "Type")
      )

instance
  Prelude.Hashable
    MediaInsightsPipelineConfigurationElement
  where
  hashWithSalt
    _salt
    MediaInsightsPipelineConfigurationElement' {..} =
      _salt
        `Prelude.hashWithSalt` amazonTranscribeCallAnalyticsProcessorConfiguration
        `Prelude.hashWithSalt` amazonTranscribeProcessorConfiguration
        `Prelude.hashWithSalt` kinesisDataStreamSinkConfiguration
        `Prelude.hashWithSalt` lambdaFunctionSinkConfiguration
        `Prelude.hashWithSalt` s3RecordingSinkConfiguration
        `Prelude.hashWithSalt` snsTopicSinkConfiguration
        `Prelude.hashWithSalt` sqsQueueSinkConfiguration
        `Prelude.hashWithSalt` voiceAnalyticsProcessorConfiguration
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    MediaInsightsPipelineConfigurationElement
  where
  rnf MediaInsightsPipelineConfigurationElement' {..} =
    Prelude.rnf
      amazonTranscribeCallAnalyticsProcessorConfiguration
      `Prelude.seq` Prelude.rnf amazonTranscribeProcessorConfiguration
      `Prelude.seq` Prelude.rnf kinesisDataStreamSinkConfiguration
      `Prelude.seq` Prelude.rnf lambdaFunctionSinkConfiguration
      `Prelude.seq` Prelude.rnf s3RecordingSinkConfiguration
      `Prelude.seq` Prelude.rnf snsTopicSinkConfiguration
      `Prelude.seq` Prelude.rnf sqsQueueSinkConfiguration
      `Prelude.seq` Prelude.rnf voiceAnalyticsProcessorConfiguration
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    MediaInsightsPipelineConfigurationElement
  where
  toJSON MediaInsightsPipelineConfigurationElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "AmazonTranscribeCallAnalyticsProcessorConfiguration"
                Data..=
            )
              Prelude.<$> amazonTranscribeCallAnalyticsProcessorConfiguration,
            ("AmazonTranscribeProcessorConfiguration" Data..=)
              Prelude.<$> amazonTranscribeProcessorConfiguration,
            ("KinesisDataStreamSinkConfiguration" Data..=)
              Prelude.<$> kinesisDataStreamSinkConfiguration,
            ("LambdaFunctionSinkConfiguration" Data..=)
              Prelude.<$> lambdaFunctionSinkConfiguration,
            ("S3RecordingSinkConfiguration" Data..=)
              Prelude.<$> s3RecordingSinkConfiguration,
            ("SnsTopicSinkConfiguration" Data..=)
              Prelude.<$> snsTopicSinkConfiguration,
            ("SqsQueueSinkConfiguration" Data..=)
              Prelude.<$> sqsQueueSinkConfiguration,
            ("VoiceAnalyticsProcessorConfiguration" Data..=)
              Prelude.<$> voiceAnalyticsProcessorConfiguration,
            Prelude.Just ("Type" Data..= type')
          ]
      )

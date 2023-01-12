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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaCapturePipeline where

import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSinkType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineSourceType
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A media pipeline object consisting of an ID, source type, source ARN, a
-- sink type, a sink ARN, and a configuration object.
--
-- /See:/ 'newMediaCapturePipeline' smart constructor.
data MediaCapturePipeline = MediaCapturePipeline'
  { -- | The configuration for a specified media pipeline. @SourceType@ must be
    -- @ChimeSdkMeeting@.
    chimeSdkMeetingConfiguration :: Prelude.Maybe ChimeSdkMeetingConfiguration,
    -- | The time at which the pipeline was created, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the media capture pipeline
    mediaPipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a media pipeline.
    mediaPipelineId :: Prelude.Maybe Prelude.Text,
    -- | ARN of the destination to which the media artifacts are saved.
    sinkArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Destination type to which the media artifacts are saved. You must use an
    -- S3 Bucket.
    sinkType :: Prelude.Maybe MediaPipelineSinkType,
    -- | ARN of the source from which the media artifacts are saved.
    sourceArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Source type from which media artifacts are saved. You must use
    -- @ChimeMeeting@.
    sourceType :: Prelude.Maybe MediaPipelineSourceType,
    -- | The status of the media pipeline.
    status :: Prelude.Maybe MediaPipelineStatus,
    -- | The time at which the pipeline was updated, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaCapturePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeSdkMeetingConfiguration', 'mediaCapturePipeline_chimeSdkMeetingConfiguration' - The configuration for a specified media pipeline. @SourceType@ must be
-- @ChimeSdkMeeting@.
--
-- 'createdTimestamp', 'mediaCapturePipeline_createdTimestamp' - The time at which the pipeline was created, in ISO 8601 format.
--
-- 'mediaPipelineArn', 'mediaCapturePipeline_mediaPipelineArn' - The ARN of the media capture pipeline
--
-- 'mediaPipelineId', 'mediaCapturePipeline_mediaPipelineId' - The ID of a media pipeline.
--
-- 'sinkArn', 'mediaCapturePipeline_sinkArn' - ARN of the destination to which the media artifacts are saved.
--
-- 'sinkType', 'mediaCapturePipeline_sinkType' - Destination type to which the media artifacts are saved. You must use an
-- S3 Bucket.
--
-- 'sourceArn', 'mediaCapturePipeline_sourceArn' - ARN of the source from which the media artifacts are saved.
--
-- 'sourceType', 'mediaCapturePipeline_sourceType' - Source type from which media artifacts are saved. You must use
-- @ChimeMeeting@.
--
-- 'status', 'mediaCapturePipeline_status' - The status of the media pipeline.
--
-- 'updatedTimestamp', 'mediaCapturePipeline_updatedTimestamp' - The time at which the pipeline was updated, in ISO 8601 format.
newMediaCapturePipeline ::
  MediaCapturePipeline
newMediaCapturePipeline =
  MediaCapturePipeline'
    { chimeSdkMeetingConfiguration =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      mediaPipelineArn = Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      sinkType = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The configuration for a specified media pipeline. @SourceType@ must be
-- @ChimeSdkMeeting@.
mediaCapturePipeline_chimeSdkMeetingConfiguration :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe ChimeSdkMeetingConfiguration)
mediaCapturePipeline_chimeSdkMeetingConfiguration = Lens.lens (\MediaCapturePipeline' {chimeSdkMeetingConfiguration} -> chimeSdkMeetingConfiguration) (\s@MediaCapturePipeline' {} a -> s {chimeSdkMeetingConfiguration = a} :: MediaCapturePipeline)

-- | The time at which the pipeline was created, in ISO 8601 format.
mediaCapturePipeline_createdTimestamp :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.UTCTime)
mediaCapturePipeline_createdTimestamp = Lens.lens (\MediaCapturePipeline' {createdTimestamp} -> createdTimestamp) (\s@MediaCapturePipeline' {} a -> s {createdTimestamp = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Data._Time

-- | The ARN of the media capture pipeline
mediaCapturePipeline_mediaPipelineArn :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_mediaPipelineArn = Lens.lens (\MediaCapturePipeline' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaCapturePipeline' {} a -> s {mediaPipelineArn = a} :: MediaCapturePipeline)

-- | The ID of a media pipeline.
mediaCapturePipeline_mediaPipelineId :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_mediaPipelineId = Lens.lens (\MediaCapturePipeline' {mediaPipelineId} -> mediaPipelineId) (\s@MediaCapturePipeline' {} a -> s {mediaPipelineId = a} :: MediaCapturePipeline)

-- | ARN of the destination to which the media artifacts are saved.
mediaCapturePipeline_sinkArn :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_sinkArn = Lens.lens (\MediaCapturePipeline' {sinkArn} -> sinkArn) (\s@MediaCapturePipeline' {} a -> s {sinkArn = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Data._Sensitive

-- | Destination type to which the media artifacts are saved. You must use an
-- S3 Bucket.
mediaCapturePipeline_sinkType :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineSinkType)
mediaCapturePipeline_sinkType = Lens.lens (\MediaCapturePipeline' {sinkType} -> sinkType) (\s@MediaCapturePipeline' {} a -> s {sinkType = a} :: MediaCapturePipeline)

-- | ARN of the source from which the media artifacts are saved.
mediaCapturePipeline_sourceArn :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_sourceArn = Lens.lens (\MediaCapturePipeline' {sourceArn} -> sourceArn) (\s@MediaCapturePipeline' {} a -> s {sourceArn = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Data._Sensitive

-- | Source type from which media artifacts are saved. You must use
-- @ChimeMeeting@.
mediaCapturePipeline_sourceType :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineSourceType)
mediaCapturePipeline_sourceType = Lens.lens (\MediaCapturePipeline' {sourceType} -> sourceType) (\s@MediaCapturePipeline' {} a -> s {sourceType = a} :: MediaCapturePipeline)

-- | The status of the media pipeline.
mediaCapturePipeline_status :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineStatus)
mediaCapturePipeline_status = Lens.lens (\MediaCapturePipeline' {status} -> status) (\s@MediaCapturePipeline' {} a -> s {status = a} :: MediaCapturePipeline)

-- | The time at which the pipeline was updated, in ISO 8601 format.
mediaCapturePipeline_updatedTimestamp :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.UTCTime)
mediaCapturePipeline_updatedTimestamp = Lens.lens (\MediaCapturePipeline' {updatedTimestamp} -> updatedTimestamp) (\s@MediaCapturePipeline' {} a -> s {updatedTimestamp = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON MediaCapturePipeline where
  parseJSON =
    Data.withObject
      "MediaCapturePipeline"
      ( \x ->
          MediaCapturePipeline'
            Prelude.<$> (x Data..:? "ChimeSdkMeetingConfiguration")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "MediaPipelineArn")
            Prelude.<*> (x Data..:? "MediaPipelineId")
            Prelude.<*> (x Data..:? "SinkArn")
            Prelude.<*> (x Data..:? "SinkType")
            Prelude.<*> (x Data..:? "SourceArn")
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable MediaCapturePipeline where
  hashWithSalt _salt MediaCapturePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` chimeSdkMeetingConfiguration
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` mediaPipelineId
      `Prelude.hashWithSalt` sinkArn
      `Prelude.hashWithSalt` sinkType
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData MediaCapturePipeline where
  rnf MediaCapturePipeline' {..} =
    Prelude.rnf chimeSdkMeetingConfiguration
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf mediaPipelineId
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf sinkType
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedTimestamp

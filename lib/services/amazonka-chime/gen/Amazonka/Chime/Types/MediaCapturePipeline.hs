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
-- Module      : Amazonka.Chime.Types.MediaCapturePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.MediaCapturePipeline where

import Amazonka.Chime.Types.ChimeSdkMeetingConfiguration
import Amazonka.Chime.Types.MediaPipelineSinkType
import Amazonka.Chime.Types.MediaPipelineSourceType
import Amazonka.Chime.Types.MediaPipelineStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A media capture pipeline object consisting of an ID, source type, source
-- ARN, a sink type, a sink ARN, and a configuration object.
--
-- /See:/ 'newMediaCapturePipeline' smart constructor.
data MediaCapturePipeline = MediaCapturePipeline'
  { -- | The status of the media capture pipeline.
    status :: Prelude.Maybe MediaPipelineStatus,
    -- | Source type from which media artifacts are saved. You must use
    -- @ChimeMeeting@.
    sourceType :: Prelude.Maybe MediaPipelineSourceType,
    -- | ARN of the source from which the media artifacts will be saved.
    sourceArn :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time at which the capture pipeline was updated, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Destination type to which the media artifacts are saved. You must use an
    -- S3 Bucket.
    sinkType :: Prelude.Maybe MediaPipelineSinkType,
    -- | The configuration for a specified media capture pipeline. @SourceType@
    -- must be @ChimeSdkMeeting@.
    chimeSdkMeetingConfiguration :: Prelude.Maybe ChimeSdkMeetingConfiguration,
    -- | ARN of the destination to which the media artifacts are saved.
    sinkArn :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of a media capture pipeline.
    mediaPipelineId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the capture pipeline was created, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'status', 'mediaCapturePipeline_status' - The status of the media capture pipeline.
--
-- 'sourceType', 'mediaCapturePipeline_sourceType' - Source type from which media artifacts are saved. You must use
-- @ChimeMeeting@.
--
-- 'sourceArn', 'mediaCapturePipeline_sourceArn' - ARN of the source from which the media artifacts will be saved.
--
-- 'updatedTimestamp', 'mediaCapturePipeline_updatedTimestamp' - The time at which the capture pipeline was updated, in ISO 8601 format.
--
-- 'sinkType', 'mediaCapturePipeline_sinkType' - Destination type to which the media artifacts are saved. You must use an
-- S3 Bucket.
--
-- 'chimeSdkMeetingConfiguration', 'mediaCapturePipeline_chimeSdkMeetingConfiguration' - The configuration for a specified media capture pipeline. @SourceType@
-- must be @ChimeSdkMeeting@.
--
-- 'sinkArn', 'mediaCapturePipeline_sinkArn' - ARN of the destination to which the media artifacts are saved.
--
-- 'mediaPipelineId', 'mediaCapturePipeline_mediaPipelineId' - The ID of a media capture pipeline.
--
-- 'createdTimestamp', 'mediaCapturePipeline_createdTimestamp' - The time at which the capture pipeline was created, in ISO 8601 format.
newMediaCapturePipeline ::
  MediaCapturePipeline
newMediaCapturePipeline =
  MediaCapturePipeline'
    { status = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      sinkType = Prelude.Nothing,
      chimeSdkMeetingConfiguration = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The status of the media capture pipeline.
mediaCapturePipeline_status :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineStatus)
mediaCapturePipeline_status = Lens.lens (\MediaCapturePipeline' {status} -> status) (\s@MediaCapturePipeline' {} a -> s {status = a} :: MediaCapturePipeline)

-- | Source type from which media artifacts are saved. You must use
-- @ChimeMeeting@.
mediaCapturePipeline_sourceType :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineSourceType)
mediaCapturePipeline_sourceType = Lens.lens (\MediaCapturePipeline' {sourceType} -> sourceType) (\s@MediaCapturePipeline' {} a -> s {sourceType = a} :: MediaCapturePipeline)

-- | ARN of the source from which the media artifacts will be saved.
mediaCapturePipeline_sourceArn :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_sourceArn = Lens.lens (\MediaCapturePipeline' {sourceArn} -> sourceArn) (\s@MediaCapturePipeline' {} a -> s {sourceArn = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Core._Sensitive

-- | The time at which the capture pipeline was updated, in ISO 8601 format.
mediaCapturePipeline_updatedTimestamp :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.UTCTime)
mediaCapturePipeline_updatedTimestamp = Lens.lens (\MediaCapturePipeline' {updatedTimestamp} -> updatedTimestamp) (\s@MediaCapturePipeline' {} a -> s {updatedTimestamp = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Core._Time

-- | Destination type to which the media artifacts are saved. You must use an
-- S3 Bucket.
mediaCapturePipeline_sinkType :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe MediaPipelineSinkType)
mediaCapturePipeline_sinkType = Lens.lens (\MediaCapturePipeline' {sinkType} -> sinkType) (\s@MediaCapturePipeline' {} a -> s {sinkType = a} :: MediaCapturePipeline)

-- | The configuration for a specified media capture pipeline. @SourceType@
-- must be @ChimeSdkMeeting@.
mediaCapturePipeline_chimeSdkMeetingConfiguration :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe ChimeSdkMeetingConfiguration)
mediaCapturePipeline_chimeSdkMeetingConfiguration = Lens.lens (\MediaCapturePipeline' {chimeSdkMeetingConfiguration} -> chimeSdkMeetingConfiguration) (\s@MediaCapturePipeline' {} a -> s {chimeSdkMeetingConfiguration = a} :: MediaCapturePipeline)

-- | ARN of the destination to which the media artifacts are saved.
mediaCapturePipeline_sinkArn :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_sinkArn = Lens.lens (\MediaCapturePipeline' {sinkArn} -> sinkArn) (\s@MediaCapturePipeline' {} a -> s {sinkArn = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of a media capture pipeline.
mediaCapturePipeline_mediaPipelineId :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.Text)
mediaCapturePipeline_mediaPipelineId = Lens.lens (\MediaCapturePipeline' {mediaPipelineId} -> mediaPipelineId) (\s@MediaCapturePipeline' {} a -> s {mediaPipelineId = a} :: MediaCapturePipeline)

-- | The time at which the capture pipeline was created, in ISO 8601 format.
mediaCapturePipeline_createdTimestamp :: Lens.Lens' MediaCapturePipeline (Prelude.Maybe Prelude.UTCTime)
mediaCapturePipeline_createdTimestamp = Lens.lens (\MediaCapturePipeline' {createdTimestamp} -> createdTimestamp) (\s@MediaCapturePipeline' {} a -> s {createdTimestamp = a} :: MediaCapturePipeline) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON MediaCapturePipeline where
  parseJSON =
    Core.withObject
      "MediaCapturePipeline"
      ( \x ->
          MediaCapturePipeline'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SourceArn")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "SinkType")
            Prelude.<*> (x Core..:? "ChimeSdkMeetingConfiguration")
            Prelude.<*> (x Core..:? "SinkArn")
            Prelude.<*> (x Core..:? "MediaPipelineId")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable MediaCapturePipeline where
  hashWithSalt _salt MediaCapturePipeline' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` sinkType
      `Prelude.hashWithSalt` chimeSdkMeetingConfiguration
      `Prelude.hashWithSalt` sinkArn
      `Prelude.hashWithSalt` mediaPipelineId
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData MediaCapturePipeline where
  rnf MediaCapturePipeline' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf sinkType
      `Prelude.seq` Prelude.rnf chimeSdkMeetingConfiguration
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf mediaPipelineId
      `Prelude.seq` Prelude.rnf createdTimestamp

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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaConcatenationPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaConcatenationPipeline where

import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSink
import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSource
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Concatenates audio and video data from one or more data streams.
--
-- /See:/ 'newMediaConcatenationPipeline' smart constructor.
data MediaConcatenationPipeline = MediaConcatenationPipeline'
  { -- | The data sources being concatnated.
    sources :: Prelude.Maybe (Prelude.NonEmpty ConcatenationSource),
    -- | The time at which the concatenation pipeline was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the media pipeline that you specify in the
    -- @SourceConfiguration@ object.
    mediaPipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the concatenation pipeline was last updated.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of the concatenation pipeline.
    status :: Prelude.Maybe MediaPipelineStatus,
    -- | The data sinks of the concatenation pipeline.
    sinks :: Prelude.Maybe (Prelude.NonEmpty ConcatenationSink),
    -- | The ID of the media pipeline being concatenated.
    mediaPipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaConcatenationPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'mediaConcatenationPipeline_sources' - The data sources being concatnated.
--
-- 'createdTimestamp', 'mediaConcatenationPipeline_createdTimestamp' - The time at which the concatenation pipeline was created.
--
-- 'mediaPipelineArn', 'mediaConcatenationPipeline_mediaPipelineArn' - The ARN of the media pipeline that you specify in the
-- @SourceConfiguration@ object.
--
-- 'updatedTimestamp', 'mediaConcatenationPipeline_updatedTimestamp' - The time at which the concatenation pipeline was last updated.
--
-- 'status', 'mediaConcatenationPipeline_status' - The status of the concatenation pipeline.
--
-- 'sinks', 'mediaConcatenationPipeline_sinks' - The data sinks of the concatenation pipeline.
--
-- 'mediaPipelineId', 'mediaConcatenationPipeline_mediaPipelineId' - The ID of the media pipeline being concatenated.
newMediaConcatenationPipeline ::
  MediaConcatenationPipeline
newMediaConcatenationPipeline =
  MediaConcatenationPipeline'
    { sources =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      mediaPipelineArn = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      sinks = Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing
    }

-- | The data sources being concatnated.
mediaConcatenationPipeline_sources :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe (Prelude.NonEmpty ConcatenationSource))
mediaConcatenationPipeline_sources = Lens.lens (\MediaConcatenationPipeline' {sources} -> sources) (\s@MediaConcatenationPipeline' {} a -> s {sources = a} :: MediaConcatenationPipeline) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the concatenation pipeline was created.
mediaConcatenationPipeline_createdTimestamp :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe Prelude.UTCTime)
mediaConcatenationPipeline_createdTimestamp = Lens.lens (\MediaConcatenationPipeline' {createdTimestamp} -> createdTimestamp) (\s@MediaConcatenationPipeline' {} a -> s {createdTimestamp = a} :: MediaConcatenationPipeline) Prelude.. Lens.mapping Core._Time

-- | The ARN of the media pipeline that you specify in the
-- @SourceConfiguration@ object.
mediaConcatenationPipeline_mediaPipelineArn :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe Prelude.Text)
mediaConcatenationPipeline_mediaPipelineArn = Lens.lens (\MediaConcatenationPipeline' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaConcatenationPipeline' {} a -> s {mediaPipelineArn = a} :: MediaConcatenationPipeline)

-- | The time at which the concatenation pipeline was last updated.
mediaConcatenationPipeline_updatedTimestamp :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe Prelude.UTCTime)
mediaConcatenationPipeline_updatedTimestamp = Lens.lens (\MediaConcatenationPipeline' {updatedTimestamp} -> updatedTimestamp) (\s@MediaConcatenationPipeline' {} a -> s {updatedTimestamp = a} :: MediaConcatenationPipeline) Prelude.. Lens.mapping Core._Time

-- | The status of the concatenation pipeline.
mediaConcatenationPipeline_status :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe MediaPipelineStatus)
mediaConcatenationPipeline_status = Lens.lens (\MediaConcatenationPipeline' {status} -> status) (\s@MediaConcatenationPipeline' {} a -> s {status = a} :: MediaConcatenationPipeline)

-- | The data sinks of the concatenation pipeline.
mediaConcatenationPipeline_sinks :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe (Prelude.NonEmpty ConcatenationSink))
mediaConcatenationPipeline_sinks = Lens.lens (\MediaConcatenationPipeline' {sinks} -> sinks) (\s@MediaConcatenationPipeline' {} a -> s {sinks = a} :: MediaConcatenationPipeline) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the media pipeline being concatenated.
mediaConcatenationPipeline_mediaPipelineId :: Lens.Lens' MediaConcatenationPipeline (Prelude.Maybe Prelude.Text)
mediaConcatenationPipeline_mediaPipelineId = Lens.lens (\MediaConcatenationPipeline' {mediaPipelineId} -> mediaPipelineId) (\s@MediaConcatenationPipeline' {} a -> s {mediaPipelineId = a} :: MediaConcatenationPipeline)

instance Core.FromJSON MediaConcatenationPipeline where
  parseJSON =
    Core.withObject
      "MediaConcatenationPipeline"
      ( \x ->
          MediaConcatenationPipeline'
            Prelude.<$> (x Core..:? "Sources")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "MediaPipelineArn")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Sinks")
            Prelude.<*> (x Core..:? "MediaPipelineId")
      )

instance Prelude.Hashable MediaConcatenationPipeline where
  hashWithSalt _salt MediaConcatenationPipeline' {..} =
    _salt `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sinks
      `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData MediaConcatenationPipeline where
  rnf MediaConcatenationPipeline' {..} =
    Prelude.rnf sources
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sinks
      `Prelude.seq` Prelude.rnf mediaPipelineId

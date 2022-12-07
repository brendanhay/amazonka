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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaLiveConnectorPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaLiveConnectorPipeline where

import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector pipeline.
--
-- /See:/ 'newMediaLiveConnectorPipeline' smart constructor.
data MediaLiveConnectorPipeline = MediaLiveConnectorPipeline'
  { -- | The connector pipeline\'s data sources.
    sources :: Prelude.Maybe (Prelude.NonEmpty LiveConnectorSourceConfiguration),
    -- | Thetime at which the connector pipeline was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The connector pipeline\'s ARN.
    mediaPipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the connector pipeline was last updated.
    updatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The connector pipeline\'s status.
    status :: Prelude.Maybe MediaPipelineStatus,
    -- | The connector pipeline\'s data sinks.
    sinks :: Prelude.Maybe (Prelude.NonEmpty LiveConnectorSinkConfiguration),
    -- | The connector pipeline\'s ID.
    mediaPipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaLiveConnectorPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'mediaLiveConnectorPipeline_sources' - The connector pipeline\'s data sources.
--
-- 'createdTimestamp', 'mediaLiveConnectorPipeline_createdTimestamp' - Thetime at which the connector pipeline was created.
--
-- 'mediaPipelineArn', 'mediaLiveConnectorPipeline_mediaPipelineArn' - The connector pipeline\'s ARN.
--
-- 'updatedTimestamp', 'mediaLiveConnectorPipeline_updatedTimestamp' - The time at which the connector pipeline was last updated.
--
-- 'status', 'mediaLiveConnectorPipeline_status' - The connector pipeline\'s status.
--
-- 'sinks', 'mediaLiveConnectorPipeline_sinks' - The connector pipeline\'s data sinks.
--
-- 'mediaPipelineId', 'mediaLiveConnectorPipeline_mediaPipelineId' - The connector pipeline\'s ID.
newMediaLiveConnectorPipeline ::
  MediaLiveConnectorPipeline
newMediaLiveConnectorPipeline =
  MediaLiveConnectorPipeline'
    { sources =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      mediaPipelineArn = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      sinks = Prelude.Nothing,
      mediaPipelineId = Prelude.Nothing
    }

-- | The connector pipeline\'s data sources.
mediaLiveConnectorPipeline_sources :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe (Prelude.NonEmpty LiveConnectorSourceConfiguration))
mediaLiveConnectorPipeline_sources = Lens.lens (\MediaLiveConnectorPipeline' {sources} -> sources) (\s@MediaLiveConnectorPipeline' {} a -> s {sources = a} :: MediaLiveConnectorPipeline) Prelude.. Lens.mapping Lens.coerced

-- | Thetime at which the connector pipeline was created.
mediaLiveConnectorPipeline_createdTimestamp :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe Prelude.UTCTime)
mediaLiveConnectorPipeline_createdTimestamp = Lens.lens (\MediaLiveConnectorPipeline' {createdTimestamp} -> createdTimestamp) (\s@MediaLiveConnectorPipeline' {} a -> s {createdTimestamp = a} :: MediaLiveConnectorPipeline) Prelude.. Lens.mapping Data._Time

-- | The connector pipeline\'s ARN.
mediaLiveConnectorPipeline_mediaPipelineArn :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe Prelude.Text)
mediaLiveConnectorPipeline_mediaPipelineArn = Lens.lens (\MediaLiveConnectorPipeline' {mediaPipelineArn} -> mediaPipelineArn) (\s@MediaLiveConnectorPipeline' {} a -> s {mediaPipelineArn = a} :: MediaLiveConnectorPipeline)

-- | The time at which the connector pipeline was last updated.
mediaLiveConnectorPipeline_updatedTimestamp :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe Prelude.UTCTime)
mediaLiveConnectorPipeline_updatedTimestamp = Lens.lens (\MediaLiveConnectorPipeline' {updatedTimestamp} -> updatedTimestamp) (\s@MediaLiveConnectorPipeline' {} a -> s {updatedTimestamp = a} :: MediaLiveConnectorPipeline) Prelude.. Lens.mapping Data._Time

-- | The connector pipeline\'s status.
mediaLiveConnectorPipeline_status :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe MediaPipelineStatus)
mediaLiveConnectorPipeline_status = Lens.lens (\MediaLiveConnectorPipeline' {status} -> status) (\s@MediaLiveConnectorPipeline' {} a -> s {status = a} :: MediaLiveConnectorPipeline)

-- | The connector pipeline\'s data sinks.
mediaLiveConnectorPipeline_sinks :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe (Prelude.NonEmpty LiveConnectorSinkConfiguration))
mediaLiveConnectorPipeline_sinks = Lens.lens (\MediaLiveConnectorPipeline' {sinks} -> sinks) (\s@MediaLiveConnectorPipeline' {} a -> s {sinks = a} :: MediaLiveConnectorPipeline) Prelude.. Lens.mapping Lens.coerced

-- | The connector pipeline\'s ID.
mediaLiveConnectorPipeline_mediaPipelineId :: Lens.Lens' MediaLiveConnectorPipeline (Prelude.Maybe Prelude.Text)
mediaLiveConnectorPipeline_mediaPipelineId = Lens.lens (\MediaLiveConnectorPipeline' {mediaPipelineId} -> mediaPipelineId) (\s@MediaLiveConnectorPipeline' {} a -> s {mediaPipelineId = a} :: MediaLiveConnectorPipeline)

instance Data.FromJSON MediaLiveConnectorPipeline where
  parseJSON =
    Data.withObject
      "MediaLiveConnectorPipeline"
      ( \x ->
          MediaLiveConnectorPipeline'
            Prelude.<$> (x Data..:? "Sources")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "MediaPipelineArn")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Sinks")
            Prelude.<*> (x Data..:? "MediaPipelineId")
      )

instance Prelude.Hashable MediaLiveConnectorPipeline where
  hashWithSalt _salt MediaLiveConnectorPipeline' {..} =
    _salt `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` mediaPipelineArn
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sinks
      `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData MediaLiveConnectorPipeline where
  rnf MediaLiveConnectorPipeline' {..} =
    Prelude.rnf sources
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf mediaPipelineArn
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sinks
      `Prelude.seq` Prelude.rnf mediaPipelineId

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
-- Module      : Amazonka.IoTAnalytics.Types.Pipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Pipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.PipelineActivity
import Amazonka.IoTAnalytics.Types.ReprocessingSummary
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a pipeline.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The activities that perform transformations on the messages.
    activities :: Prelude.Maybe (Prelude.NonEmpty PipelineActivity),
    -- | The ARN of the pipeline.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the pipeline was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the pipeline was updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipeline.
    name :: Prelude.Maybe Prelude.Text,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Prelude.Maybe [ReprocessingSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activities', 'pipeline_activities' - The activities that perform transformations on the messages.
--
-- 'arn', 'pipeline_arn' - The ARN of the pipeline.
--
-- 'creationTime', 'pipeline_creationTime' - When the pipeline was created.
--
-- 'lastUpdateTime', 'pipeline_lastUpdateTime' - The last time the pipeline was updated.
--
-- 'name', 'pipeline_name' - The name of the pipeline.
--
-- 'reprocessingSummaries', 'pipeline_reprocessingSummaries' - A summary of information about the pipeline reprocessing.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { activities = Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      reprocessingSummaries = Prelude.Nothing
    }

-- | The activities that perform transformations on the messages.
pipeline_activities :: Lens.Lens' Pipeline (Prelude.Maybe (Prelude.NonEmpty PipelineActivity))
pipeline_activities = Lens.lens (\Pipeline' {activities} -> activities) (\s@Pipeline' {} a -> s {activities = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the pipeline.
pipeline_arn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_arn = Lens.lens (\Pipeline' {arn} -> arn) (\s@Pipeline' {} a -> s {arn = a} :: Pipeline)

-- | When the pipeline was created.
pipeline_creationTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | The last time the pipeline was updated.
pipeline_lastUpdateTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastUpdateTime = Lens.lens (\Pipeline' {lastUpdateTime} -> lastUpdateTime) (\s@Pipeline' {} a -> s {lastUpdateTime = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | The name of the pipeline.
pipeline_name :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_name = Lens.lens (\Pipeline' {name} -> name) (\s@Pipeline' {} a -> s {name = a} :: Pipeline)

-- | A summary of information about the pipeline reprocessing.
pipeline_reprocessingSummaries :: Lens.Lens' Pipeline (Prelude.Maybe [ReprocessingSummary])
pipeline_reprocessingSummaries = Lens.lens (\Pipeline' {reprocessingSummaries} -> reprocessingSummaries) (\s@Pipeline' {} a -> s {reprocessingSummaries = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Pipeline where
  parseJSON =
    Data.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Data..:? "activities")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x
                            Data..:? "reprocessingSummaries"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Pipeline where
  hashWithSalt _salt Pipeline' {..} =
    _salt
      `Prelude.hashWithSalt` activities
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reprocessingSummaries

instance Prelude.NFData Pipeline where
  rnf Pipeline' {..} =
    Prelude.rnf activities `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf lastUpdateTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf reprocessingSummaries

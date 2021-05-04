{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.Pipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Pipeline where

import Network.AWS.IoTAnalytics.Types.PipelineActivity
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a pipeline.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | When the pipeline was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The last time the pipeline was updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The activities that perform transformations on the messages.
    activities :: Prelude.Maybe (Prelude.NonEmpty PipelineActivity),
    -- | The ARN of the pipeline.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    name :: Prelude.Maybe Prelude.Text,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Prelude.Maybe [ReprocessingSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Pipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'pipeline_creationTime' - When the pipeline was created.
--
-- 'lastUpdateTime', 'pipeline_lastUpdateTime' - The last time the pipeline was updated.
--
-- 'activities', 'pipeline_activities' - The activities that perform transformations on the messages.
--
-- 'arn', 'pipeline_arn' - The ARN of the pipeline.
--
-- 'name', 'pipeline_name' - The name of the pipeline.
--
-- 'reprocessingSummaries', 'pipeline_reprocessingSummaries' - A summary of information about the pipeline reprocessing.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      activities = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      reprocessingSummaries = Prelude.Nothing
    }

-- | When the pipeline was created.
pipeline_creationTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Time

-- | The last time the pipeline was updated.
pipeline_lastUpdateTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastUpdateTime = Lens.lens (\Pipeline' {lastUpdateTime} -> lastUpdateTime) (\s@Pipeline' {} a -> s {lastUpdateTime = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Time

-- | The activities that perform transformations on the messages.
pipeline_activities :: Lens.Lens' Pipeline (Prelude.Maybe (Prelude.NonEmpty PipelineActivity))
pipeline_activities = Lens.lens (\Pipeline' {activities} -> activities) (\s@Pipeline' {} a -> s {activities = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the pipeline.
pipeline_arn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_arn = Lens.lens (\Pipeline' {arn} -> arn) (\s@Pipeline' {} a -> s {arn = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_name :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_name = Lens.lens (\Pipeline' {name} -> name) (\s@Pipeline' {} a -> s {name = a} :: Pipeline)

-- | A summary of information about the pipeline reprocessing.
pipeline_reprocessingSummaries :: Lens.Lens' Pipeline (Prelude.Maybe [ReprocessingSummary])
pipeline_reprocessingSummaries = Lens.lens (\Pipeline' {reprocessingSummaries} -> reprocessingSummaries) (\s@Pipeline' {} a -> s {reprocessingSummaries = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Pipeline where
  parseJSON =
    Prelude.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "lastUpdateTime")
            Prelude.<*> (x Prelude..:? "activities")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "reprocessingSummaries"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Pipeline

instance Prelude.NFData Pipeline

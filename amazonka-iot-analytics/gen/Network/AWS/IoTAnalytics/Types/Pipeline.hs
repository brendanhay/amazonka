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

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.PipelineActivity
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens

-- | Contains information about a pipeline.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | When the pipeline was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The last time the pipeline was updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The activities that perform transformations on the messages.
    activities :: Core.Maybe (Core.NonEmpty PipelineActivity),
    -- | The ARN of the pipeline.
    arn :: Core.Maybe Core.Text,
    -- | The name of the pipeline.
    name :: Core.Maybe Core.Text,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Core.Maybe [ReprocessingSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      activities = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      reprocessingSummaries = Core.Nothing
    }

-- | When the pipeline was created.
pipeline_creationTime :: Lens.Lens' Pipeline (Core.Maybe Core.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Core.. Lens.mapping Core._Time

-- | The last time the pipeline was updated.
pipeline_lastUpdateTime :: Lens.Lens' Pipeline (Core.Maybe Core.UTCTime)
pipeline_lastUpdateTime = Lens.lens (\Pipeline' {lastUpdateTime} -> lastUpdateTime) (\s@Pipeline' {} a -> s {lastUpdateTime = a} :: Pipeline) Core.. Lens.mapping Core._Time

-- | The activities that perform transformations on the messages.
pipeline_activities :: Lens.Lens' Pipeline (Core.Maybe (Core.NonEmpty PipelineActivity))
pipeline_activities = Lens.lens (\Pipeline' {activities} -> activities) (\s@Pipeline' {} a -> s {activities = a} :: Pipeline) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the pipeline.
pipeline_arn :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_arn = Lens.lens (\Pipeline' {arn} -> arn) (\s@Pipeline' {} a -> s {arn = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_name :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_name = Lens.lens (\Pipeline' {name} -> name) (\s@Pipeline' {} a -> s {name = a} :: Pipeline)

-- | A summary of information about the pipeline reprocessing.
pipeline_reprocessingSummaries :: Lens.Lens' Pipeline (Core.Maybe [ReprocessingSummary])
pipeline_reprocessingSummaries = Lens.lens (\Pipeline' {reprocessingSummaries} -> reprocessingSummaries) (\s@Pipeline' {} a -> s {reprocessingSummaries = a} :: Pipeline) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Pipeline where
  parseJSON =
    Core.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Core.<$> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "activities")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> ( x Core..:? "reprocessingSummaries"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable Pipeline

instance Core.NFData Pipeline

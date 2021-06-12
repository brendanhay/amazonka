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
-- Module      : Network.AWS.SageMaker.Types.Pipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Pipeline where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.PipelineStatus
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | A SageMaker Model Building Pipeline instance.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Core.Maybe Core.Text,
    -- | The creation time of the pipeline.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the role that created the pipeline.
    roleArn :: Core.Maybe Core.Text,
    -- | The time when the pipeline was last run.
    lastRunTime :: Core.Maybe Core.POSIX,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Core.Maybe Core.Text,
    -- | A list of tags that apply to the pipeline.
    tags :: Core.Maybe [Tag],
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The status of the pipeline.
    pipelineStatus :: Core.Maybe PipelineStatus,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Core.Text
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
-- 'pipelineArn', 'pipeline_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDescription', 'pipeline_pipelineDescription' - The description of the pipeline.
--
-- 'creationTime', 'pipeline_creationTime' - The creation time of the pipeline.
--
-- 'roleArn', 'pipeline_roleArn' - The Amazon Resource Name (ARN) of the role that created the pipeline.
--
-- 'lastRunTime', 'pipeline_lastRunTime' - The time when the pipeline was last run.
--
-- 'pipelineDisplayName', 'pipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'tags', 'pipeline_tags' - A list of tags that apply to the pipeline.
--
-- 'lastModifiedTime', 'pipeline_lastModifiedTime' - The time that the pipeline was last modified.
--
-- 'pipelineStatus', 'pipeline_pipelineStatus' - The status of the pipeline.
--
-- 'createdBy', 'pipeline_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'pipeline_lastModifiedBy' - Undocumented member.
--
-- 'pipelineName', 'pipeline_pipelineName' - The name of the pipeline.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { pipelineArn = Core.Nothing,
      pipelineDescription = Core.Nothing,
      creationTime = Core.Nothing,
      roleArn = Core.Nothing,
      lastRunTime = Core.Nothing,
      pipelineDisplayName = Core.Nothing,
      tags = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      pipelineStatus = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      pipelineName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipeline_pipelineArn :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_pipelineArn = Lens.lens (\Pipeline' {pipelineArn} -> pipelineArn) (\s@Pipeline' {} a -> s {pipelineArn = a} :: Pipeline)

-- | The description of the pipeline.
pipeline_pipelineDescription :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_pipelineDescription = Lens.lens (\Pipeline' {pipelineDescription} -> pipelineDescription) (\s@Pipeline' {} a -> s {pipelineDescription = a} :: Pipeline)

-- | The creation time of the pipeline.
pipeline_creationTime :: Lens.Lens' Pipeline (Core.Maybe Core.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the role that created the pipeline.
pipeline_roleArn :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_roleArn = Lens.lens (\Pipeline' {roleArn} -> roleArn) (\s@Pipeline' {} a -> s {roleArn = a} :: Pipeline)

-- | The time when the pipeline was last run.
pipeline_lastRunTime :: Lens.Lens' Pipeline (Core.Maybe Core.UTCTime)
pipeline_lastRunTime = Lens.lens (\Pipeline' {lastRunTime} -> lastRunTime) (\s@Pipeline' {} a -> s {lastRunTime = a} :: Pipeline) Core.. Lens.mapping Core._Time

-- | The display name of the pipeline.
pipeline_pipelineDisplayName :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_pipelineDisplayName = Lens.lens (\Pipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@Pipeline' {} a -> s {pipelineDisplayName = a} :: Pipeline)

-- | A list of tags that apply to the pipeline.
pipeline_tags :: Lens.Lens' Pipeline (Core.Maybe [Tag])
pipeline_tags = Lens.lens (\Pipeline' {tags} -> tags) (\s@Pipeline' {} a -> s {tags = a} :: Pipeline) Core.. Lens.mapping Lens._Coerce

-- | The time that the pipeline was last modified.
pipeline_lastModifiedTime :: Lens.Lens' Pipeline (Core.Maybe Core.UTCTime)
pipeline_lastModifiedTime = Lens.lens (\Pipeline' {lastModifiedTime} -> lastModifiedTime) (\s@Pipeline' {} a -> s {lastModifiedTime = a} :: Pipeline) Core.. Lens.mapping Core._Time

-- | The status of the pipeline.
pipeline_pipelineStatus :: Lens.Lens' Pipeline (Core.Maybe PipelineStatus)
pipeline_pipelineStatus = Lens.lens (\Pipeline' {pipelineStatus} -> pipelineStatus) (\s@Pipeline' {} a -> s {pipelineStatus = a} :: Pipeline)

-- | Undocumented member.
pipeline_createdBy :: Lens.Lens' Pipeline (Core.Maybe UserContext)
pipeline_createdBy = Lens.lens (\Pipeline' {createdBy} -> createdBy) (\s@Pipeline' {} a -> s {createdBy = a} :: Pipeline)

-- | Undocumented member.
pipeline_lastModifiedBy :: Lens.Lens' Pipeline (Core.Maybe UserContext)
pipeline_lastModifiedBy = Lens.lens (\Pipeline' {lastModifiedBy} -> lastModifiedBy) (\s@Pipeline' {} a -> s {lastModifiedBy = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_pipelineName :: Lens.Lens' Pipeline (Core.Maybe Core.Text)
pipeline_pipelineName = Lens.lens (\Pipeline' {pipelineName} -> pipelineName) (\s@Pipeline' {} a -> s {pipelineName = a} :: Pipeline)

instance Core.FromJSON Pipeline where
  parseJSON =
    Core.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Core.<$> (x Core..:? "PipelineArn")
            Core.<*> (x Core..:? "PipelineDescription")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "LastRunTime")
            Core.<*> (x Core..:? "PipelineDisplayName")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "PipelineStatus")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "PipelineName")
      )

instance Core.Hashable Pipeline

instance Core.NFData Pipeline

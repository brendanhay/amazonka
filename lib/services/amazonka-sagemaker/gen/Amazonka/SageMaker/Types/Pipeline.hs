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
-- Module      : Amazonka.SageMaker.Types.Pipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Pipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ParallelismConfiguration
import Amazonka.SageMaker.Types.PipelineStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | A SageMaker Model Building Pipeline instance.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | A list of tags that apply to the pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the role that created the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The parallelism configuration applied to the pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The time when the pipeline was last run.
    lastRunTime :: Prelude.Maybe Core.POSIX,
    -- | The creation time of the pipeline.
    creationTime :: Prelude.Maybe Core.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    createdBy :: Prelude.Maybe UserContext,
    -- | The status of the pipeline.
    pipelineStatus :: Prelude.Maybe PipelineStatus
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
-- 'tags', 'pipeline_tags' - A list of tags that apply to the pipeline.
--
-- 'roleArn', 'pipeline_roleArn' - The Amazon Resource Name (ARN) of the role that created the pipeline.
--
-- 'pipelineArn', 'pipeline_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDisplayName', 'pipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'pipelineDescription', 'pipeline_pipelineDescription' - The description of the pipeline.
--
-- 'lastModifiedTime', 'pipeline_lastModifiedTime' - The time that the pipeline was last modified.
--
-- 'parallelismConfiguration', 'pipeline_parallelismConfiguration' - The parallelism configuration applied to the pipeline.
--
-- 'pipelineName', 'pipeline_pipelineName' - The name of the pipeline.
--
-- 'lastRunTime', 'pipeline_lastRunTime' - The time when the pipeline was last run.
--
-- 'creationTime', 'pipeline_creationTime' - The creation time of the pipeline.
--
-- 'lastModifiedBy', 'pipeline_lastModifiedBy' - Undocumented member.
--
-- 'createdBy', 'pipeline_createdBy' - Undocumented member.
--
-- 'pipelineStatus', 'pipeline_pipelineStatus' - The status of the pipeline.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { tags = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      lastRunTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      pipelineStatus = Prelude.Nothing
    }

-- | A list of tags that apply to the pipeline.
pipeline_tags :: Lens.Lens' Pipeline (Prelude.Maybe [Tag])
pipeline_tags = Lens.lens (\Pipeline' {tags} -> tags) (\s@Pipeline' {} a -> s {tags = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role that created the pipeline.
pipeline_roleArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_roleArn = Lens.lens (\Pipeline' {roleArn} -> roleArn) (\s@Pipeline' {} a -> s {roleArn = a} :: Pipeline)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipeline_pipelineArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineArn = Lens.lens (\Pipeline' {pipelineArn} -> pipelineArn) (\s@Pipeline' {} a -> s {pipelineArn = a} :: Pipeline)

-- | The display name of the pipeline.
pipeline_pipelineDisplayName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDisplayName = Lens.lens (\Pipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@Pipeline' {} a -> s {pipelineDisplayName = a} :: Pipeline)

-- | The description of the pipeline.
pipeline_pipelineDescription :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDescription = Lens.lens (\Pipeline' {pipelineDescription} -> pipelineDescription) (\s@Pipeline' {} a -> s {pipelineDescription = a} :: Pipeline)

-- | The time that the pipeline was last modified.
pipeline_lastModifiedTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastModifiedTime = Lens.lens (\Pipeline' {lastModifiedTime} -> lastModifiedTime) (\s@Pipeline' {} a -> s {lastModifiedTime = a} :: Pipeline) Prelude.. Lens.mapping Core._Time

-- | The parallelism configuration applied to the pipeline.
pipeline_parallelismConfiguration :: Lens.Lens' Pipeline (Prelude.Maybe ParallelismConfiguration)
pipeline_parallelismConfiguration = Lens.lens (\Pipeline' {parallelismConfiguration} -> parallelismConfiguration) (\s@Pipeline' {} a -> s {parallelismConfiguration = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_pipelineName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineName = Lens.lens (\Pipeline' {pipelineName} -> pipelineName) (\s@Pipeline' {} a -> s {pipelineName = a} :: Pipeline)

-- | The time when the pipeline was last run.
pipeline_lastRunTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastRunTime = Lens.lens (\Pipeline' {lastRunTime} -> lastRunTime) (\s@Pipeline' {} a -> s {lastRunTime = a} :: Pipeline) Prelude.. Lens.mapping Core._Time

-- | The creation time of the pipeline.
pipeline_creationTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
pipeline_lastModifiedBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_lastModifiedBy = Lens.lens (\Pipeline' {lastModifiedBy} -> lastModifiedBy) (\s@Pipeline' {} a -> s {lastModifiedBy = a} :: Pipeline)

-- | Undocumented member.
pipeline_createdBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_createdBy = Lens.lens (\Pipeline' {createdBy} -> createdBy) (\s@Pipeline' {} a -> s {createdBy = a} :: Pipeline)

-- | The status of the pipeline.
pipeline_pipelineStatus :: Lens.Lens' Pipeline (Prelude.Maybe PipelineStatus)
pipeline_pipelineStatus = Lens.lens (\Pipeline' {pipelineStatus} -> pipelineStatus) (\s@Pipeline' {} a -> s {pipelineStatus = a} :: Pipeline)

instance Core.FromJSON Pipeline where
  parseJSON =
    Core.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "PipelineArn")
            Prelude.<*> (x Core..:? "PipelineDisplayName")
            Prelude.<*> (x Core..:? "PipelineDescription")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ParallelismConfiguration")
            Prelude.<*> (x Core..:? "PipelineName")
            Prelude.<*> (x Core..:? "LastRunTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "PipelineStatus")
      )

instance Prelude.Hashable Pipeline where
  hashWithSalt _salt Pipeline' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineDisplayName
      `Prelude.hashWithSalt` pipelineDescription
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` lastRunTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` pipelineStatus

instance Prelude.NFData Pipeline where
  rnf Pipeline' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineDisplayName
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parallelismConfiguration
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf lastRunTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf pipelineStatus

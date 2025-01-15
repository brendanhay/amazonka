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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Pipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ParallelismConfiguration
import Amazonka.SageMaker.Types.PipelineStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | A SageMaker Model Building Pipeline instance.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { createdBy :: Prelude.Maybe UserContext,
    -- | The creation time of the pipeline.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the pipeline was last run.
    lastRunTime :: Prelude.Maybe Data.POSIX,
    -- | The parallelism configuration applied to the pipeline.
    parallelismConfiguration :: Prelude.Maybe ParallelismConfiguration,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline.
    pipelineStatus :: Prelude.Maybe PipelineStatus,
    -- | The Amazon Resource Name (ARN) of the role that created the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that apply to the pipeline.
    tags :: Prelude.Maybe [Tag]
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
-- 'createdBy', 'pipeline_createdBy' - Undocumented member.
--
-- 'creationTime', 'pipeline_creationTime' - The creation time of the pipeline.
--
-- 'lastModifiedBy', 'pipeline_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'pipeline_lastModifiedTime' - The time that the pipeline was last modified.
--
-- 'lastRunTime', 'pipeline_lastRunTime' - The time when the pipeline was last run.
--
-- 'parallelismConfiguration', 'pipeline_parallelismConfiguration' - The parallelism configuration applied to the pipeline.
--
-- 'pipelineArn', 'pipeline_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDescription', 'pipeline_pipelineDescription' - The description of the pipeline.
--
-- 'pipelineDisplayName', 'pipeline_pipelineDisplayName' - The display name of the pipeline.
--
-- 'pipelineName', 'pipeline_pipelineName' - The name of the pipeline.
--
-- 'pipelineStatus', 'pipeline_pipelineStatus' - The status of the pipeline.
--
-- 'roleArn', 'pipeline_roleArn' - The Amazon Resource Name (ARN) of the role that created the pipeline.
--
-- 'tags', 'pipeline_tags' - A list of tags that apply to the pipeline.
newPipeline ::
  Pipeline
newPipeline =
  Pipeline'
    { createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      lastRunTime = Prelude.Nothing,
      parallelismConfiguration = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      pipelineStatus = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
pipeline_createdBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_createdBy = Lens.lens (\Pipeline' {createdBy} -> createdBy) (\s@Pipeline' {} a -> s {createdBy = a} :: Pipeline)

-- | The creation time of the pipeline.
pipeline_creationTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
pipeline_lastModifiedBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_lastModifiedBy = Lens.lens (\Pipeline' {lastModifiedBy} -> lastModifiedBy) (\s@Pipeline' {} a -> s {lastModifiedBy = a} :: Pipeline)

-- | The time that the pipeline was last modified.
pipeline_lastModifiedTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastModifiedTime = Lens.lens (\Pipeline' {lastModifiedTime} -> lastModifiedTime) (\s@Pipeline' {} a -> s {lastModifiedTime = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | The time when the pipeline was last run.
pipeline_lastRunTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastRunTime = Lens.lens (\Pipeline' {lastRunTime} -> lastRunTime) (\s@Pipeline' {} a -> s {lastRunTime = a} :: Pipeline) Prelude.. Lens.mapping Data._Time

-- | The parallelism configuration applied to the pipeline.
pipeline_parallelismConfiguration :: Lens.Lens' Pipeline (Prelude.Maybe ParallelismConfiguration)
pipeline_parallelismConfiguration = Lens.lens (\Pipeline' {parallelismConfiguration} -> parallelismConfiguration) (\s@Pipeline' {} a -> s {parallelismConfiguration = a} :: Pipeline)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipeline_pipelineArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineArn = Lens.lens (\Pipeline' {pipelineArn} -> pipelineArn) (\s@Pipeline' {} a -> s {pipelineArn = a} :: Pipeline)

-- | The description of the pipeline.
pipeline_pipelineDescription :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDescription = Lens.lens (\Pipeline' {pipelineDescription} -> pipelineDescription) (\s@Pipeline' {} a -> s {pipelineDescription = a} :: Pipeline)

-- | The display name of the pipeline.
pipeline_pipelineDisplayName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDisplayName = Lens.lens (\Pipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@Pipeline' {} a -> s {pipelineDisplayName = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_pipelineName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineName = Lens.lens (\Pipeline' {pipelineName} -> pipelineName) (\s@Pipeline' {} a -> s {pipelineName = a} :: Pipeline)

-- | The status of the pipeline.
pipeline_pipelineStatus :: Lens.Lens' Pipeline (Prelude.Maybe PipelineStatus)
pipeline_pipelineStatus = Lens.lens (\Pipeline' {pipelineStatus} -> pipelineStatus) (\s@Pipeline' {} a -> s {pipelineStatus = a} :: Pipeline)

-- | The Amazon Resource Name (ARN) of the role that created the pipeline.
pipeline_roleArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_roleArn = Lens.lens (\Pipeline' {roleArn} -> roleArn) (\s@Pipeline' {} a -> s {roleArn = a} :: Pipeline)

-- | A list of tags that apply to the pipeline.
pipeline_tags :: Lens.Lens' Pipeline (Prelude.Maybe [Tag])
pipeline_tags = Lens.lens (\Pipeline' {tags} -> tags) (\s@Pipeline' {} a -> s {tags = a} :: Pipeline) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Pipeline where
  parseJSON =
    Data.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "LastRunTime")
            Prelude.<*> (x Data..:? "ParallelismConfiguration")
            Prelude.<*> (x Data..:? "PipelineArn")
            Prelude.<*> (x Data..:? "PipelineDescription")
            Prelude.<*> (x Data..:? "PipelineDisplayName")
            Prelude.<*> (x Data..:? "PipelineName")
            Prelude.<*> (x Data..:? "PipelineStatus")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Pipeline where
  hashWithSalt _salt Pipeline' {..} =
    _salt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` lastRunTime
      `Prelude.hashWithSalt` parallelismConfiguration
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineDescription
      `Prelude.hashWithSalt` pipelineDisplayName
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` pipelineStatus
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Pipeline where
  rnf Pipeline' {..} =
    Prelude.rnf createdBy `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf lastModifiedBy `Prelude.seq`
          Prelude.rnf lastModifiedTime `Prelude.seq`
            Prelude.rnf lastRunTime `Prelude.seq`
              Prelude.rnf parallelismConfiguration `Prelude.seq`
                Prelude.rnf pipelineArn `Prelude.seq`
                  Prelude.rnf pipelineDescription `Prelude.seq`
                    Prelude.rnf pipelineDisplayName `Prelude.seq`
                      Prelude.rnf pipelineName `Prelude.seq`
                        Prelude.rnf pipelineStatus `Prelude.seq`
                          Prelude.rnf roleArn `Prelude.seq`
                            Prelude.rnf tags

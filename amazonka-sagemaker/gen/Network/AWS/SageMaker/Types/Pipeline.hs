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
-- Module      : Network.AWS.SageMaker.Types.Pipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Pipeline where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.PipelineStatus
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | A SageMaker Model Building Pipeline instance.
--
-- /See:/ 'newPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the pipeline.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the role that created the pipeline.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the pipeline was last run.
    lastRunTime :: Prelude.Maybe Prelude.POSIX,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that apply to the pipeline.
    tags :: Prelude.Maybe [Tag],
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the pipeline.
    pipelineStatus :: Prelude.Maybe PipelineStatus,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text
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
    { pipelineArn = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      lastRunTime = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      pipelineStatus = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      pipelineName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipeline_pipelineArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineArn = Lens.lens (\Pipeline' {pipelineArn} -> pipelineArn) (\s@Pipeline' {} a -> s {pipelineArn = a} :: Pipeline)

-- | The description of the pipeline.
pipeline_pipelineDescription :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDescription = Lens.lens (\Pipeline' {pipelineDescription} -> pipelineDescription) (\s@Pipeline' {} a -> s {pipelineDescription = a} :: Pipeline)

-- | The creation time of the pipeline.
pipeline_creationTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_creationTime = Lens.lens (\Pipeline' {creationTime} -> creationTime) (\s@Pipeline' {} a -> s {creationTime = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the role that created the pipeline.
pipeline_roleArn :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_roleArn = Lens.lens (\Pipeline' {roleArn} -> roleArn) (\s@Pipeline' {} a -> s {roleArn = a} :: Pipeline)

-- | The time when the pipeline was last run.
pipeline_lastRunTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastRunTime = Lens.lens (\Pipeline' {lastRunTime} -> lastRunTime) (\s@Pipeline' {} a -> s {lastRunTime = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Time

-- | The display name of the pipeline.
pipeline_pipelineDisplayName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineDisplayName = Lens.lens (\Pipeline' {pipelineDisplayName} -> pipelineDisplayName) (\s@Pipeline' {} a -> s {pipelineDisplayName = a} :: Pipeline)

-- | A list of tags that apply to the pipeline.
pipeline_tags :: Lens.Lens' Pipeline (Prelude.Maybe [Tag])
pipeline_tags = Lens.lens (\Pipeline' {tags} -> tags) (\s@Pipeline' {} a -> s {tags = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Coerce

-- | The time that the pipeline was last modified.
pipeline_lastModifiedTime :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.UTCTime)
pipeline_lastModifiedTime = Lens.lens (\Pipeline' {lastModifiedTime} -> lastModifiedTime) (\s@Pipeline' {} a -> s {lastModifiedTime = a} :: Pipeline) Prelude.. Lens.mapping Prelude._Time

-- | The status of the pipeline.
pipeline_pipelineStatus :: Lens.Lens' Pipeline (Prelude.Maybe PipelineStatus)
pipeline_pipelineStatus = Lens.lens (\Pipeline' {pipelineStatus} -> pipelineStatus) (\s@Pipeline' {} a -> s {pipelineStatus = a} :: Pipeline)

-- | Undocumented member.
pipeline_createdBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_createdBy = Lens.lens (\Pipeline' {createdBy} -> createdBy) (\s@Pipeline' {} a -> s {createdBy = a} :: Pipeline)

-- | Undocumented member.
pipeline_lastModifiedBy :: Lens.Lens' Pipeline (Prelude.Maybe UserContext)
pipeline_lastModifiedBy = Lens.lens (\Pipeline' {lastModifiedBy} -> lastModifiedBy) (\s@Pipeline' {} a -> s {lastModifiedBy = a} :: Pipeline)

-- | The name of the pipeline.
pipeline_pipelineName :: Lens.Lens' Pipeline (Prelude.Maybe Prelude.Text)
pipeline_pipelineName = Lens.lens (\Pipeline' {pipelineName} -> pipelineName) (\s@Pipeline' {} a -> s {pipelineName = a} :: Pipeline)

instance Prelude.FromJSON Pipeline where
  parseJSON =
    Prelude.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Prelude.<$> (x Prelude..:? "PipelineArn")
            Prelude.<*> (x Prelude..:? "PipelineDescription")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "LastRunTime")
            Prelude.<*> (x Prelude..:? "PipelineDisplayName")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "PipelineStatus")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..:? "PipelineName")
      )

instance Prelude.Hashable Pipeline

instance Prelude.NFData Pipeline

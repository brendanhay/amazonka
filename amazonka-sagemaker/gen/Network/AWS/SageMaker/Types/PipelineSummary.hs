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
-- Module      : Network.AWS.SageMaker.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Core.Maybe Core.Text,
    -- | The creation time of the pipeline.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that the pipeline used to execute.
    roleArn :: Core.Maybe Core.Text,
    -- | The last time that a pipeline execution began.
    lastExecutionTime :: Core.Maybe Core.POSIX,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Core.Maybe Core.Text,
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the pipeline.
    pipelineName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'pipelineSummary_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineDescription', 'pipelineSummary_pipelineDescription' - The description of the pipeline.
--
-- 'creationTime', 'pipelineSummary_creationTime' - The creation time of the pipeline.
--
-- 'roleArn', 'pipelineSummary_roleArn' - The Amazon Resource Name (ARN) that the pipeline used to execute.
--
-- 'lastExecutionTime', 'pipelineSummary_lastExecutionTime' - The last time that a pipeline execution began.
--
-- 'pipelineDisplayName', 'pipelineSummary_pipelineDisplayName' - The display name of the pipeline.
--
-- 'lastModifiedTime', 'pipelineSummary_lastModifiedTime' - The time that the pipeline was last modified.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { pipelineArn = Core.Nothing,
      pipelineDescription = Core.Nothing,
      creationTime = Core.Nothing,
      roleArn = Core.Nothing,
      lastExecutionTime = Core.Nothing,
      pipelineDisplayName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      pipelineName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineSummary_pipelineArn :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_pipelineArn = Lens.lens (\PipelineSummary' {pipelineArn} -> pipelineArn) (\s@PipelineSummary' {} a -> s {pipelineArn = a} :: PipelineSummary)

-- | The description of the pipeline.
pipelineSummary_pipelineDescription :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_pipelineDescription = Lens.lens (\PipelineSummary' {pipelineDescription} -> pipelineDescription) (\s@PipelineSummary' {} a -> s {pipelineDescription = a} :: PipelineSummary)

-- | The creation time of the pipeline.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that the pipeline used to execute.
pipelineSummary_roleArn :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_roleArn = Lens.lens (\PipelineSummary' {roleArn} -> roleArn) (\s@PipelineSummary' {} a -> s {roleArn = a} :: PipelineSummary)

-- | The last time that a pipeline execution began.
pipelineSummary_lastExecutionTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_lastExecutionTime = Lens.lens (\PipelineSummary' {lastExecutionTime} -> lastExecutionTime) (\s@PipelineSummary' {} a -> s {lastExecutionTime = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | The display name of the pipeline.
pipelineSummary_pipelineDisplayName :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_pipelineDisplayName = Lens.lens (\PipelineSummary' {pipelineDisplayName} -> pipelineDisplayName) (\s@PipelineSummary' {} a -> s {pipelineDisplayName = a} :: PipelineSummary)

-- | The time that the pipeline was last modified.
pipelineSummary_lastModifiedTime :: Lens.Lens' PipelineSummary (Core.Maybe Core.UTCTime)
pipelineSummary_lastModifiedTime = Lens.lens (\PipelineSummary' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineSummary' {} a -> s {lastModifiedTime = a} :: PipelineSummary) Core.. Lens.mapping Core._Time

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Core.Maybe Core.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Core.<$> (x Core..:? "PipelineArn")
            Core.<*> (x Core..:? "PipelineDescription")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "LastExecutionTime")
            Core.<*> (x Core..:? "PipelineDisplayName")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "PipelineName")
      )

instance Core.Hashable PipelineSummary

instance Core.NFData PipelineSummary

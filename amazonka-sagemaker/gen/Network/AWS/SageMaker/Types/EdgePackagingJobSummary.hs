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
-- Module      : Network.AWS.SageMaker.Types.EdgePackagingJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgePackagingJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.EdgePackagingJobStatus

-- | Summary of edge packaging job.
--
-- /See:/ 'newEdgePackagingJobSummary' smart constructor.
data EdgePackagingJobSummary = EdgePackagingJobSummary'
  { -- | The timestamp of when the job was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the SageMaker Neo compilation job.
    compilationJobName :: Core.Maybe Core.Text,
    -- | The version of the model.
    modelVersion :: Core.Maybe Core.Text,
    -- | The timestamp of when the edge packaging job was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The name of the model.
    modelName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the edge packaging job.
    edgePackagingJobArn :: Core.Text,
    -- | The name of the edge packaging job.
    edgePackagingJobName :: Core.Text,
    -- | The status of the edge packaging job.
    edgePackagingJobStatus :: EdgePackagingJobStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EdgePackagingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'edgePackagingJobSummary_creationTime' - The timestamp of when the job was created.
--
-- 'compilationJobName', 'edgePackagingJobSummary_compilationJobName' - The name of the SageMaker Neo compilation job.
--
-- 'modelVersion', 'edgePackagingJobSummary_modelVersion' - The version of the model.
--
-- 'lastModifiedTime', 'edgePackagingJobSummary_lastModifiedTime' - The timestamp of when the edge packaging job was last updated.
--
-- 'modelName', 'edgePackagingJobSummary_modelName' - The name of the model.
--
-- 'edgePackagingJobArn', 'edgePackagingJobSummary_edgePackagingJobArn' - The Amazon Resource Name (ARN) of the edge packaging job.
--
-- 'edgePackagingJobName', 'edgePackagingJobSummary_edgePackagingJobName' - The name of the edge packaging job.
--
-- 'edgePackagingJobStatus', 'edgePackagingJobSummary_edgePackagingJobStatus' - The status of the edge packaging job.
newEdgePackagingJobSummary ::
  -- | 'edgePackagingJobArn'
  Core.Text ->
  -- | 'edgePackagingJobName'
  Core.Text ->
  -- | 'edgePackagingJobStatus'
  EdgePackagingJobStatus ->
  EdgePackagingJobSummary
newEdgePackagingJobSummary
  pEdgePackagingJobArn_
  pEdgePackagingJobName_
  pEdgePackagingJobStatus_ =
    EdgePackagingJobSummary'
      { creationTime =
          Core.Nothing,
        compilationJobName = Core.Nothing,
        modelVersion = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        modelName = Core.Nothing,
        edgePackagingJobArn = pEdgePackagingJobArn_,
        edgePackagingJobName = pEdgePackagingJobName_,
        edgePackagingJobStatus = pEdgePackagingJobStatus_
      }

-- | The timestamp of when the job was created.
edgePackagingJobSummary_creationTime :: Lens.Lens' EdgePackagingJobSummary (Core.Maybe Core.UTCTime)
edgePackagingJobSummary_creationTime = Lens.lens (\EdgePackagingJobSummary' {creationTime} -> creationTime) (\s@EdgePackagingJobSummary' {} a -> s {creationTime = a} :: EdgePackagingJobSummary) Core.. Lens.mapping Core._Time

-- | The name of the SageMaker Neo compilation job.
edgePackagingJobSummary_compilationJobName :: Lens.Lens' EdgePackagingJobSummary (Core.Maybe Core.Text)
edgePackagingJobSummary_compilationJobName = Lens.lens (\EdgePackagingJobSummary' {compilationJobName} -> compilationJobName) (\s@EdgePackagingJobSummary' {} a -> s {compilationJobName = a} :: EdgePackagingJobSummary)

-- | The version of the model.
edgePackagingJobSummary_modelVersion :: Lens.Lens' EdgePackagingJobSummary (Core.Maybe Core.Text)
edgePackagingJobSummary_modelVersion = Lens.lens (\EdgePackagingJobSummary' {modelVersion} -> modelVersion) (\s@EdgePackagingJobSummary' {} a -> s {modelVersion = a} :: EdgePackagingJobSummary)

-- | The timestamp of when the edge packaging job was last updated.
edgePackagingJobSummary_lastModifiedTime :: Lens.Lens' EdgePackagingJobSummary (Core.Maybe Core.UTCTime)
edgePackagingJobSummary_lastModifiedTime = Lens.lens (\EdgePackagingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EdgePackagingJobSummary' {} a -> s {lastModifiedTime = a} :: EdgePackagingJobSummary) Core.. Lens.mapping Core._Time

-- | The name of the model.
edgePackagingJobSummary_modelName :: Lens.Lens' EdgePackagingJobSummary (Core.Maybe Core.Text)
edgePackagingJobSummary_modelName = Lens.lens (\EdgePackagingJobSummary' {modelName} -> modelName) (\s@EdgePackagingJobSummary' {} a -> s {modelName = a} :: EdgePackagingJobSummary)

-- | The Amazon Resource Name (ARN) of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobArn :: Lens.Lens' EdgePackagingJobSummary Core.Text
edgePackagingJobSummary_edgePackagingJobArn = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobArn} -> edgePackagingJobArn) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobArn = a} :: EdgePackagingJobSummary)

-- | The name of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobName :: Lens.Lens' EdgePackagingJobSummary Core.Text
edgePackagingJobSummary_edgePackagingJobName = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobName} -> edgePackagingJobName) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobName = a} :: EdgePackagingJobSummary)

-- | The status of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobStatus :: Lens.Lens' EdgePackagingJobSummary EdgePackagingJobStatus
edgePackagingJobSummary_edgePackagingJobStatus = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobStatus} -> edgePackagingJobStatus) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobStatus = a} :: EdgePackagingJobSummary)

instance Core.FromJSON EdgePackagingJobSummary where
  parseJSON =
    Core.withObject
      "EdgePackagingJobSummary"
      ( \x ->
          EdgePackagingJobSummary'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "CompilationJobName")
            Core.<*> (x Core..:? "ModelVersion")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ModelName")
            Core.<*> (x Core..: "EdgePackagingJobArn")
            Core.<*> (x Core..: "EdgePackagingJobName")
            Core.<*> (x Core..: "EdgePackagingJobStatus")
      )

instance Core.Hashable EdgePackagingJobSummary

instance Core.NFData EdgePackagingJobSummary

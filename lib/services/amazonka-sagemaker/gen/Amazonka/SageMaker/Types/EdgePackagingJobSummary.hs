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
-- Module      : Amazonka.SageMaker.Types.EdgePackagingJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgePackagingJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EdgePackagingJobStatus

-- | Summary of edge packaging job.
--
-- /See:/ 'newEdgePackagingJobSummary' smart constructor.
data EdgePackagingJobSummary = EdgePackagingJobSummary'
  { -- | The name of the SageMaker Neo compilation job.
    compilationJobName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the edge packaging job was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the model.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the edge packaging job.
    edgePackagingJobArn :: Prelude.Text,
    -- | The name of the edge packaging job.
    edgePackagingJobName :: Prelude.Text,
    -- | The status of the edge packaging job.
    edgePackagingJobStatus :: EdgePackagingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgePackagingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationJobName', 'edgePackagingJobSummary_compilationJobName' - The name of the SageMaker Neo compilation job.
--
-- 'creationTime', 'edgePackagingJobSummary_creationTime' - The timestamp of when the job was created.
--
-- 'lastModifiedTime', 'edgePackagingJobSummary_lastModifiedTime' - The timestamp of when the edge packaging job was last updated.
--
-- 'modelName', 'edgePackagingJobSummary_modelName' - The name of the model.
--
-- 'modelVersion', 'edgePackagingJobSummary_modelVersion' - The version of the model.
--
-- 'edgePackagingJobArn', 'edgePackagingJobSummary_edgePackagingJobArn' - The Amazon Resource Name (ARN) of the edge packaging job.
--
-- 'edgePackagingJobName', 'edgePackagingJobSummary_edgePackagingJobName' - The name of the edge packaging job.
--
-- 'edgePackagingJobStatus', 'edgePackagingJobSummary_edgePackagingJobStatus' - The status of the edge packaging job.
newEdgePackagingJobSummary ::
  -- | 'edgePackagingJobArn'
  Prelude.Text ->
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  -- | 'edgePackagingJobStatus'
  EdgePackagingJobStatus ->
  EdgePackagingJobSummary
newEdgePackagingJobSummary
  pEdgePackagingJobArn_
  pEdgePackagingJobName_
  pEdgePackagingJobStatus_ =
    EdgePackagingJobSummary'
      { compilationJobName =
          Prelude.Nothing,
        creationTime = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        modelName = Prelude.Nothing,
        modelVersion = Prelude.Nothing,
        edgePackagingJobArn = pEdgePackagingJobArn_,
        edgePackagingJobName = pEdgePackagingJobName_,
        edgePackagingJobStatus = pEdgePackagingJobStatus_
      }

-- | The name of the SageMaker Neo compilation job.
edgePackagingJobSummary_compilationJobName :: Lens.Lens' EdgePackagingJobSummary (Prelude.Maybe Prelude.Text)
edgePackagingJobSummary_compilationJobName = Lens.lens (\EdgePackagingJobSummary' {compilationJobName} -> compilationJobName) (\s@EdgePackagingJobSummary' {} a -> s {compilationJobName = a} :: EdgePackagingJobSummary)

-- | The timestamp of when the job was created.
edgePackagingJobSummary_creationTime :: Lens.Lens' EdgePackagingJobSummary (Prelude.Maybe Prelude.UTCTime)
edgePackagingJobSummary_creationTime = Lens.lens (\EdgePackagingJobSummary' {creationTime} -> creationTime) (\s@EdgePackagingJobSummary' {} a -> s {creationTime = a} :: EdgePackagingJobSummary) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the edge packaging job was last updated.
edgePackagingJobSummary_lastModifiedTime :: Lens.Lens' EdgePackagingJobSummary (Prelude.Maybe Prelude.UTCTime)
edgePackagingJobSummary_lastModifiedTime = Lens.lens (\EdgePackagingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EdgePackagingJobSummary' {} a -> s {lastModifiedTime = a} :: EdgePackagingJobSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the model.
edgePackagingJobSummary_modelName :: Lens.Lens' EdgePackagingJobSummary (Prelude.Maybe Prelude.Text)
edgePackagingJobSummary_modelName = Lens.lens (\EdgePackagingJobSummary' {modelName} -> modelName) (\s@EdgePackagingJobSummary' {} a -> s {modelName = a} :: EdgePackagingJobSummary)

-- | The version of the model.
edgePackagingJobSummary_modelVersion :: Lens.Lens' EdgePackagingJobSummary (Prelude.Maybe Prelude.Text)
edgePackagingJobSummary_modelVersion = Lens.lens (\EdgePackagingJobSummary' {modelVersion} -> modelVersion) (\s@EdgePackagingJobSummary' {} a -> s {modelVersion = a} :: EdgePackagingJobSummary)

-- | The Amazon Resource Name (ARN) of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobArn :: Lens.Lens' EdgePackagingJobSummary Prelude.Text
edgePackagingJobSummary_edgePackagingJobArn = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobArn} -> edgePackagingJobArn) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobArn = a} :: EdgePackagingJobSummary)

-- | The name of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobName :: Lens.Lens' EdgePackagingJobSummary Prelude.Text
edgePackagingJobSummary_edgePackagingJobName = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobName} -> edgePackagingJobName) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobName = a} :: EdgePackagingJobSummary)

-- | The status of the edge packaging job.
edgePackagingJobSummary_edgePackagingJobStatus :: Lens.Lens' EdgePackagingJobSummary EdgePackagingJobStatus
edgePackagingJobSummary_edgePackagingJobStatus = Lens.lens (\EdgePackagingJobSummary' {edgePackagingJobStatus} -> edgePackagingJobStatus) (\s@EdgePackagingJobSummary' {} a -> s {edgePackagingJobStatus = a} :: EdgePackagingJobSummary)

instance Data.FromJSON EdgePackagingJobSummary where
  parseJSON =
    Data.withObject
      "EdgePackagingJobSummary"
      ( \x ->
          EdgePackagingJobSummary'
            Prelude.<$> (x Data..:? "CompilationJobName")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ModelName")
            Prelude.<*> (x Data..:? "ModelVersion")
            Prelude.<*> (x Data..: "EdgePackagingJobArn")
            Prelude.<*> (x Data..: "EdgePackagingJobName")
            Prelude.<*> (x Data..: "EdgePackagingJobStatus")
      )

instance Prelude.Hashable EdgePackagingJobSummary where
  hashWithSalt _salt EdgePackagingJobSummary' {..} =
    _salt `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` edgePackagingJobArn
      `Prelude.hashWithSalt` edgePackagingJobName
      `Prelude.hashWithSalt` edgePackagingJobStatus

instance Prelude.NFData EdgePackagingJobSummary where
  rnf EdgePackagingJobSummary' {..} =
    Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf edgePackagingJobArn
      `Prelude.seq` Prelude.rnf edgePackagingJobName
      `Prelude.seq` Prelude.rnf edgePackagingJobStatus

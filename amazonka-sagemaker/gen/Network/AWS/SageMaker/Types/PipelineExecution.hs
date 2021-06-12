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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.Parameter
import Network.AWS.SageMaker.Types.PipelineExecutionStatus
import Network.AWS.SageMaker.Types.UserContext

-- | An execution of a pipeline.
--
-- /See:/ 'newPipelineExecution' smart constructor.
data PipelineExecution = PipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the pipeline that was executed.
    pipelineArn :: Core.Maybe Core.Text,
    -- | The creation time of the pipeline execution.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The description of the pipeline execution.
    pipelineExecutionDescription :: Core.Maybe Core.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Core.Maybe [Parameter],
    -- | The display name of the pipeline execution.
    pipelineExecutionDisplayName :: Core.Maybe Core.Text,
    -- | The status of the pipeline status.
    pipelineExecutionStatus :: Core.Maybe PipelineExecutionStatus,
    -- | The time that the pipeline execution was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineArn', 'pipelineExecution_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline that was executed.
--
-- 'creationTime', 'pipelineExecution_creationTime' - The creation time of the pipeline execution.
--
-- 'pipelineExecutionDescription', 'pipelineExecution_pipelineExecutionDescription' - The description of the pipeline execution.
--
-- 'pipelineParameters', 'pipelineExecution_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'pipelineExecutionDisplayName', 'pipelineExecution_pipelineExecutionDisplayName' - The display name of the pipeline execution.
--
-- 'pipelineExecutionStatus', 'pipelineExecution_pipelineExecutionStatus' - The status of the pipeline status.
--
-- 'lastModifiedTime', 'pipelineExecution_lastModifiedTime' - The time that the pipeline execution was last modified.
--
-- 'createdBy', 'pipelineExecution_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'pipelineExecution_lastModifiedBy' - Undocumented member.
--
-- 'pipelineExecutionArn', 'pipelineExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newPipelineExecution ::
  PipelineExecution
newPipelineExecution =
  PipelineExecution'
    { pipelineArn = Core.Nothing,
      creationTime = Core.Nothing,
      pipelineExecutionDescription = Core.Nothing,
      pipelineParameters = Core.Nothing,
      pipelineExecutionDisplayName = Core.Nothing,
      pipelineExecutionStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      pipelineExecutionArn = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline that was executed.
pipelineExecution_pipelineArn :: Lens.Lens' PipelineExecution (Core.Maybe Core.Text)
pipelineExecution_pipelineArn = Lens.lens (\PipelineExecution' {pipelineArn} -> pipelineArn) (\s@PipelineExecution' {} a -> s {pipelineArn = a} :: PipelineExecution)

-- | The creation time of the pipeline execution.
pipelineExecution_creationTime :: Lens.Lens' PipelineExecution (Core.Maybe Core.UTCTime)
pipelineExecution_creationTime = Lens.lens (\PipelineExecution' {creationTime} -> creationTime) (\s@PipelineExecution' {} a -> s {creationTime = a} :: PipelineExecution) Core.. Lens.mapping Core._Time

-- | The description of the pipeline execution.
pipelineExecution_pipelineExecutionDescription :: Lens.Lens' PipelineExecution (Core.Maybe Core.Text)
pipelineExecution_pipelineExecutionDescription = Lens.lens (\PipelineExecution' {pipelineExecutionDescription} -> pipelineExecutionDescription) (\s@PipelineExecution' {} a -> s {pipelineExecutionDescription = a} :: PipelineExecution)

-- | Contains a list of pipeline parameters. This list can be empty.
pipelineExecution_pipelineParameters :: Lens.Lens' PipelineExecution (Core.Maybe [Parameter])
pipelineExecution_pipelineParameters = Lens.lens (\PipelineExecution' {pipelineParameters} -> pipelineParameters) (\s@PipelineExecution' {} a -> s {pipelineParameters = a} :: PipelineExecution) Core.. Lens.mapping Lens._Coerce

-- | The display name of the pipeline execution.
pipelineExecution_pipelineExecutionDisplayName :: Lens.Lens' PipelineExecution (Core.Maybe Core.Text)
pipelineExecution_pipelineExecutionDisplayName = Lens.lens (\PipelineExecution' {pipelineExecutionDisplayName} -> pipelineExecutionDisplayName) (\s@PipelineExecution' {} a -> s {pipelineExecutionDisplayName = a} :: PipelineExecution)

-- | The status of the pipeline status.
pipelineExecution_pipelineExecutionStatus :: Lens.Lens' PipelineExecution (Core.Maybe PipelineExecutionStatus)
pipelineExecution_pipelineExecutionStatus = Lens.lens (\PipelineExecution' {pipelineExecutionStatus} -> pipelineExecutionStatus) (\s@PipelineExecution' {} a -> s {pipelineExecutionStatus = a} :: PipelineExecution)

-- | The time that the pipeline execution was last modified.
pipelineExecution_lastModifiedTime :: Lens.Lens' PipelineExecution (Core.Maybe Core.UTCTime)
pipelineExecution_lastModifiedTime = Lens.lens (\PipelineExecution' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineExecution' {} a -> s {lastModifiedTime = a} :: PipelineExecution) Core.. Lens.mapping Core._Time

-- | Undocumented member.
pipelineExecution_createdBy :: Lens.Lens' PipelineExecution (Core.Maybe UserContext)
pipelineExecution_createdBy = Lens.lens (\PipelineExecution' {createdBy} -> createdBy) (\s@PipelineExecution' {} a -> s {createdBy = a} :: PipelineExecution)

-- | Undocumented member.
pipelineExecution_lastModifiedBy :: Lens.Lens' PipelineExecution (Core.Maybe UserContext)
pipelineExecution_lastModifiedBy = Lens.lens (\PipelineExecution' {lastModifiedBy} -> lastModifiedBy) (\s@PipelineExecution' {} a -> s {lastModifiedBy = a} :: PipelineExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
pipelineExecution_pipelineExecutionArn :: Lens.Lens' PipelineExecution (Core.Maybe Core.Text)
pipelineExecution_pipelineExecutionArn = Lens.lens (\PipelineExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@PipelineExecution' {} a -> s {pipelineExecutionArn = a} :: PipelineExecution)

instance Core.FromJSON PipelineExecution where
  parseJSON =
    Core.withObject
      "PipelineExecution"
      ( \x ->
          PipelineExecution'
            Core.<$> (x Core..:? "PipelineArn")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "PipelineExecutionDescription")
            Core.<*> ( x Core..:? "PipelineParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "PipelineExecutionDisplayName")
            Core.<*> (x Core..:? "PipelineExecutionStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "PipelineExecutionArn")
      )

instance Core.Hashable PipelineExecution

instance Core.NFData PipelineExecution

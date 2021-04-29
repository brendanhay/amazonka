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
-- Module      : Network.AWS.SageMaker.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the pipeline.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) that the pipeline used to execute.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The last time that a pipeline execution began.
    lastExecutionTime :: Prelude.Maybe Prelude.POSIX,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { pipelineArn = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      lastExecutionTime = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      pipelineName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineSummary_pipelineArn :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineArn = Lens.lens (\PipelineSummary' {pipelineArn} -> pipelineArn) (\s@PipelineSummary' {} a -> s {pipelineArn = a} :: PipelineSummary)

-- | The description of the pipeline.
pipelineSummary_pipelineDescription :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineDescription = Lens.lens (\PipelineSummary' {pipelineDescription} -> pipelineDescription) (\s@PipelineSummary' {} a -> s {pipelineDescription = a} :: PipelineSummary)

-- | The creation time of the pipeline.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) that the pipeline used to execute.
pipelineSummary_roleArn :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_roleArn = Lens.lens (\PipelineSummary' {roleArn} -> roleArn) (\s@PipelineSummary' {} a -> s {roleArn = a} :: PipelineSummary)

-- | The last time that a pipeline execution began.
pipelineSummary_lastExecutionTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastExecutionTime = Lens.lens (\PipelineSummary' {lastExecutionTime} -> lastExecutionTime) (\s@PipelineSummary' {} a -> s {lastExecutionTime = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Time

-- | The display name of the pipeline.
pipelineSummary_pipelineDisplayName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineDisplayName = Lens.lens (\PipelineSummary' {pipelineDisplayName} -> pipelineDisplayName) (\s@PipelineSummary' {} a -> s {pipelineDisplayName = a} :: PipelineSummary)

-- | The time that the pipeline was last modified.
pipelineSummary_lastModifiedTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastModifiedTime = Lens.lens (\PipelineSummary' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineSummary' {} a -> s {lastModifiedTime = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

instance Prelude.FromJSON PipelineSummary where
  parseJSON =
    Prelude.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> (x Prelude..:? "PipelineArn")
            Prelude.<*> (x Prelude..:? "PipelineDescription")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "LastExecutionTime")
            Prelude.<*> (x Prelude..:? "PipelineDisplayName")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "PipelineName")
      )

instance Prelude.Hashable PipelineSummary

instance Prelude.NFData PipelineSummary

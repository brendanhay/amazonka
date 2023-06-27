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
-- Module      : Amazonka.OsIs.Types.PipelineSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.PipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types.PipelineStatus
import Amazonka.OsIs.Types.PipelineStatusReason
import qualified Amazonka.Prelude as Prelude

-- | Summary information for an OpenSearch Ingestion pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The date and time when the pipeline was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the pipeline was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
    maxUnits :: Prelude.Maybe Prelude.Natural,
    -- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
    minUnits :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the pipeline.
    status :: Prelude.Maybe PipelineStatus,
    statusReason :: Prelude.Maybe PipelineStatusReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'pipelineSummary_createdAt' - The date and time when the pipeline was created.
--
-- 'lastUpdatedAt', 'pipelineSummary_lastUpdatedAt' - The date and time when the pipeline was last updated.
--
-- 'maxUnits', 'pipelineSummary_maxUnits' - The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'minUnits', 'pipelineSummary_minUnits' - The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
--
-- 'pipelineArn', 'pipelineSummary_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
--
-- 'status', 'pipelineSummary_status' - The current status of the pipeline.
--
-- 'statusReason', 'pipelineSummary_statusReason' - Undocumented member.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { createdAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      maxUnits = Prelude.Nothing,
      minUnits = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The date and time when the pipeline was created.
pipelineSummary_createdAt :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_createdAt = Lens.lens (\PipelineSummary' {createdAt} -> createdAt) (\s@PipelineSummary' {} a -> s {createdAt = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time when the pipeline was last updated.
pipelineSummary_lastUpdatedAt :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastUpdatedAt = Lens.lens (\PipelineSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@PipelineSummary' {} a -> s {lastUpdatedAt = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

-- | The maximum pipeline capacity, in Ingestion Compute Units (ICUs).
pipelineSummary_maxUnits :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Natural)
pipelineSummary_maxUnits = Lens.lens (\PipelineSummary' {maxUnits} -> maxUnits) (\s@PipelineSummary' {} a -> s {maxUnits = a} :: PipelineSummary)

-- | The minimum pipeline capacity, in Ingestion Compute Units (ICUs).
pipelineSummary_minUnits :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Natural)
pipelineSummary_minUnits = Lens.lens (\PipelineSummary' {minUnits} -> minUnits) (\s@PipelineSummary' {} a -> s {minUnits = a} :: PipelineSummary)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineSummary_pipelineArn :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineArn = Lens.lens (\PipelineSummary' {pipelineArn} -> pipelineArn) (\s@PipelineSummary' {} a -> s {pipelineArn = a} :: PipelineSummary)

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

-- | The current status of the pipeline.
pipelineSummary_status :: Lens.Lens' PipelineSummary (Prelude.Maybe PipelineStatus)
pipelineSummary_status = Lens.lens (\PipelineSummary' {status} -> status) (\s@PipelineSummary' {} a -> s {status = a} :: PipelineSummary)

-- | Undocumented member.
pipelineSummary_statusReason :: Lens.Lens' PipelineSummary (Prelude.Maybe PipelineStatusReason)
pipelineSummary_statusReason = Lens.lens (\PipelineSummary' {statusReason} -> statusReason) (\s@PipelineSummary' {} a -> s {statusReason = a} :: PipelineSummary)

instance Data.FromJSON PipelineSummary where
  parseJSON =
    Data.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "MaxUnits")
            Prelude.<*> (x Data..:? "MinUnits")
            Prelude.<*> (x Data..:? "PipelineArn")
            Prelude.<*> (x Data..:? "PipelineName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
      )

instance Prelude.Hashable PipelineSummary where
  hashWithSalt _salt PipelineSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` maxUnits
      `Prelude.hashWithSalt` minUnits
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData PipelineSummary where
  rnf PipelineSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf maxUnits
      `Prelude.seq` Prelude.rnf minUnits
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason

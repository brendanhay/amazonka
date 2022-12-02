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
-- Module      : Amazonka.IoTAnalytics.Types.PipelineSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.PipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.ReprocessingSummary
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Prelude.Maybe [ReprocessingSummary],
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | When the pipeline was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | When the pipeline was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX
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
-- 'reprocessingSummaries', 'pipelineSummary_reprocessingSummaries' - A summary of information about the pipeline reprocessing.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
--
-- 'creationTime', 'pipelineSummary_creationTime' - When the pipeline was created.
--
-- 'lastUpdateTime', 'pipelineSummary_lastUpdateTime' - When the pipeline was last updated.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { reprocessingSummaries =
        Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | A summary of information about the pipeline reprocessing.
pipelineSummary_reprocessingSummaries :: Lens.Lens' PipelineSummary (Prelude.Maybe [ReprocessingSummary])
pipelineSummary_reprocessingSummaries = Lens.lens (\PipelineSummary' {reprocessingSummaries} -> reprocessingSummaries) (\s@PipelineSummary' {} a -> s {reprocessingSummaries = a} :: PipelineSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

-- | When the pipeline was created.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

-- | When the pipeline was last updated.
pipelineSummary_lastUpdateTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastUpdateTime = Lens.lens (\PipelineSummary' {lastUpdateTime} -> lastUpdateTime) (\s@PipelineSummary' {} a -> s {lastUpdateTime = a} :: PipelineSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PipelineSummary where
  parseJSON =
    Data.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> ( x Data..:? "reprocessingSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "pipelineName")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
      )

instance Prelude.Hashable PipelineSummary where
  hashWithSalt _salt PipelineSummary' {..} =
    _salt `Prelude.hashWithSalt` reprocessingSummaries
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData PipelineSummary where
  rnf PipelineSummary' {..} =
    Prelude.rnf reprocessingSummaries
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime

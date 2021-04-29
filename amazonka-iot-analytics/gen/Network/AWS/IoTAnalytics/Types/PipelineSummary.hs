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
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineSummary where

import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of information about a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | When the pipeline was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | When the pipeline was last updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Prelude.Maybe [ReprocessingSummary],
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
-- 'creationTime', 'pipelineSummary_creationTime' - When the pipeline was created.
--
-- 'lastUpdateTime', 'pipelineSummary_lastUpdateTime' - When the pipeline was last updated.
--
-- 'reprocessingSummaries', 'pipelineSummary_reprocessingSummaries' - A summary of information about the pipeline reprocessing.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      reprocessingSummaries = Prelude.Nothing,
      pipelineName = Prelude.Nothing
    }

-- | When the pipeline was created.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Time

-- | When the pipeline was last updated.
pipelineSummary_lastUpdateTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastUpdateTime = Lens.lens (\PipelineSummary' {lastUpdateTime} -> lastUpdateTime) (\s@PipelineSummary' {} a -> s {lastUpdateTime = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Time

-- | A summary of information about the pipeline reprocessing.
pipelineSummary_reprocessingSummaries :: Lens.Lens' PipelineSummary (Prelude.Maybe [ReprocessingSummary])
pipelineSummary_reprocessingSummaries = Lens.lens (\PipelineSummary' {reprocessingSummaries} -> reprocessingSummaries) (\s@PipelineSummary' {} a -> s {reprocessingSummaries = a} :: PipelineSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

instance Prelude.FromJSON PipelineSummary where
  parseJSON =
    Prelude.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "lastUpdateTime")
            Prelude.<*> ( x Prelude..:? "reprocessingSummaries"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "pipelineName")
      )

instance Prelude.Hashable PipelineSummary

instance Prelude.NFData PipelineSummary

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
-- Module      : Amazonka.Pipes.Types.PipeSourceSqsQueueParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceSqsQueueParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Amazon SQS stream as a source.
--
-- /See:/ 'newPipeSourceSqsQueueParameters' smart constructor.
data PipeSourceSqsQueueParameters = PipeSourceSqsQueueParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceSqsQueueParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceSqsQueueParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
newPipeSourceSqsQueueParameters ::
  PipeSourceSqsQueueParameters
newPipeSourceSqsQueueParameters =
  PipeSourceSqsQueueParameters'
    { batchSize =
        Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing
    }

-- | The maximum number of records to include in each batch.
pipeSourceSqsQueueParameters_batchSize :: Lens.Lens' PipeSourceSqsQueueParameters (Prelude.Maybe Prelude.Natural)
pipeSourceSqsQueueParameters_batchSize = Lens.lens (\PipeSourceSqsQueueParameters' {batchSize} -> batchSize) (\s@PipeSourceSqsQueueParameters' {} a -> s {batchSize = a} :: PipeSourceSqsQueueParameters)

-- | The maximum length of a time to wait for events.
pipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceSqsQueueParameters (Prelude.Maybe Prelude.Natural)
pipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceSqsQueueParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceSqsQueueParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceSqsQueueParameters)

instance Data.FromJSON PipeSourceSqsQueueParameters where
  parseJSON =
    Data.withObject
      "PipeSourceSqsQueueParameters"
      ( \x ->
          PipeSourceSqsQueueParameters'
            Prelude.<$> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
      )

instance
  Prelude.Hashable
    PipeSourceSqsQueueParameters
  where
  hashWithSalt _salt PipeSourceSqsQueueParameters' {..} =
    _salt
      `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` maximumBatchingWindowInSeconds

instance Prelude.NFData PipeSourceSqsQueueParameters where
  rnf PipeSourceSqsQueueParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds

instance Data.ToJSON PipeSourceSqsQueueParameters where
  toJSON PipeSourceSqsQueueParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds
          ]
      )

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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceSqsQueueParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceSqsQueueParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Amazon SQS stream as a source.
--
-- /See:/ 'newUpdatePipeSourceSqsQueueParameters' smart constructor.
data UpdatePipeSourceSqsQueueParameters = UpdatePipeSourceSqsQueueParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceSqsQueueParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceSqsQueueParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
newUpdatePipeSourceSqsQueueParameters ::
  UpdatePipeSourceSqsQueueParameters
newUpdatePipeSourceSqsQueueParameters =
  UpdatePipeSourceSqsQueueParameters'
    { batchSize =
        Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing
    }

-- | The maximum number of records to include in each batch.
updatePipeSourceSqsQueueParameters_batchSize :: Lens.Lens' UpdatePipeSourceSqsQueueParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceSqsQueueParameters_batchSize = Lens.lens (\UpdatePipeSourceSqsQueueParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceSqsQueueParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceSqsQueueParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceSqsQueueParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceSqsQueueParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceSqsQueueParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceSqsQueueParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceSqsQueueParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceSqsQueueParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds

instance
  Prelude.NFData
    UpdatePipeSourceSqsQueueParameters
  where
  rnf UpdatePipeSourceSqsQueueParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds

instance
  Data.ToJSON
    UpdatePipeSourceSqsQueueParameters
  where
  toJSON UpdatePipeSourceSqsQueueParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds
          ]
      )

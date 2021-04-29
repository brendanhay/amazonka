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
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch
-- job. If you specify a retry strategy here, it overrides the retry
-- strategy defined in the job definition.
--
-- /See:/ 'newBatchRetryStrategy' smart constructor.
data BatchRetryStrategy = BatchRetryStrategy'
  { -- | The number of times to attempt to retry, if the job fails. Valid values
    -- are 1–10.
    attempts :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchRetryStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attempts', 'batchRetryStrategy_attempts' - The number of times to attempt to retry, if the job fails. Valid values
-- are 1–10.
newBatchRetryStrategy ::
  BatchRetryStrategy
newBatchRetryStrategy =
  BatchRetryStrategy' {attempts = Prelude.Nothing}

-- | The number of times to attempt to retry, if the job fails. Valid values
-- are 1–10.
batchRetryStrategy_attempts :: Lens.Lens' BatchRetryStrategy (Prelude.Maybe Prelude.Int)
batchRetryStrategy_attempts = Lens.lens (\BatchRetryStrategy' {attempts} -> attempts) (\s@BatchRetryStrategy' {} a -> s {attempts = a} :: BatchRetryStrategy)

instance Prelude.FromJSON BatchRetryStrategy where
  parseJSON =
    Prelude.withObject
      "BatchRetryStrategy"
      ( \x ->
          BatchRetryStrategy'
            Prelude.<$> (x Prelude..:? "Attempts")
      )

instance Prelude.Hashable BatchRetryStrategy

instance Prelude.NFData BatchRetryStrategy

instance Prelude.ToJSON BatchRetryStrategy where
  toJSON BatchRetryStrategy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Attempts" Prelude..=) Prelude.<$> attempts]
      )

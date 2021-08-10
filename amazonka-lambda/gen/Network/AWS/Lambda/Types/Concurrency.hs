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
-- Module      : Network.AWS.Lambda.Types.Concurrency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Concurrency where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newConcurrency' smart constructor.
data Concurrency = Concurrency'
  { -- | The number of concurrent executions that are reserved for this function.
    -- For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency>.
    reservedConcurrentExecutions :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Concurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedConcurrentExecutions', 'concurrency_reservedConcurrentExecutions' - The number of concurrent executions that are reserved for this function.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency>.
newConcurrency ::
  Concurrency
newConcurrency =
  Concurrency'
    { reservedConcurrentExecutions =
        Prelude.Nothing
    }

-- | The number of concurrent executions that are reserved for this function.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency>.
concurrency_reservedConcurrentExecutions :: Lens.Lens' Concurrency (Prelude.Maybe Prelude.Natural)
concurrency_reservedConcurrentExecutions = Lens.lens (\Concurrency' {reservedConcurrentExecutions} -> reservedConcurrentExecutions) (\s@Concurrency' {} a -> s {reservedConcurrentExecutions = a} :: Concurrency)

instance Core.FromJSON Concurrency where
  parseJSON =
    Core.withObject
      "Concurrency"
      ( \x ->
          Concurrency'
            Prelude.<$> (x Core..:? "ReservedConcurrentExecutions")
      )

instance Prelude.Hashable Concurrency

instance Prelude.NFData Concurrency

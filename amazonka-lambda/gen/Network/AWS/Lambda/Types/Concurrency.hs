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

-- | /See:/ 'newConcurrency' smart constructor.
data Concurrency = Concurrency'
  { -- | The number of concurrent executions that are reserved for this function.
    -- For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency>.
    reservedConcurrentExecutions :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The number of concurrent executions that are reserved for this function.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency>.
concurrency_reservedConcurrentExecutions :: Lens.Lens' Concurrency (Core.Maybe Core.Natural)
concurrency_reservedConcurrentExecutions = Lens.lens (\Concurrency' {reservedConcurrentExecutions} -> reservedConcurrentExecutions) (\s@Concurrency' {} a -> s {reservedConcurrentExecutions = a} :: Concurrency)

instance Core.FromJSON Concurrency where
  parseJSON =
    Core.withObject
      "Concurrency"
      ( \x ->
          Concurrency'
            Core.<$> (x Core..:? "ReservedConcurrentExecutions")
      )

instance Core.Hashable Concurrency

instance Core.NFData Concurrency

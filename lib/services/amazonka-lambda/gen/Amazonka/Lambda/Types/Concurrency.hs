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
-- Module      : Amazonka.Lambda.Types.Concurrency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.Concurrency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newConcurrency' smart constructor.
data Concurrency = Concurrency'
  { -- | The number of concurrent executions that are reserved for this function.
    -- For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-concurrency.html Managing Lambda reserved concurrency>.
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
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-concurrency.html Managing Lambda reserved concurrency>.
newConcurrency ::
  Concurrency
newConcurrency =
  Concurrency'
    { reservedConcurrentExecutions =
        Prelude.Nothing
    }

-- | The number of concurrent executions that are reserved for this function.
-- For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-concurrency.html Managing Lambda reserved concurrency>.
concurrency_reservedConcurrentExecutions :: Lens.Lens' Concurrency (Prelude.Maybe Prelude.Natural)
concurrency_reservedConcurrentExecutions = Lens.lens (\Concurrency' {reservedConcurrentExecutions} -> reservedConcurrentExecutions) (\s@Concurrency' {} a -> s {reservedConcurrentExecutions = a} :: Concurrency)

instance Data.FromJSON Concurrency where
  parseJSON =
    Data.withObject
      "Concurrency"
      ( \x ->
          Concurrency'
            Prelude.<$> (x Data..:? "ReservedConcurrentExecutions")
      )

instance Prelude.Hashable Concurrency where
  hashWithSalt _salt Concurrency' {..} =
    _salt
      `Prelude.hashWithSalt` reservedConcurrentExecutions

instance Prelude.NFData Concurrency where
  rnf Concurrency' {..} =
    Prelude.rnf reservedConcurrentExecutions

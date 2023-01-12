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
-- Module      : Amazonka.Glue.Types.ExecutionProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ExecutionProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An execution property of a job.
--
-- /See:/ 'newExecutionProperty' smart constructor.
data ExecutionProperty = ExecutionProperty'
  { -- | The maximum number of concurrent runs allowed for the job. The default
    -- is 1. An error is returned when this threshold is reached. The maximum
    -- value you can specify is controlled by a service limit.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentRuns', 'executionProperty_maxConcurrentRuns' - The maximum number of concurrent runs allowed for the job. The default
-- is 1. An error is returned when this threshold is reached. The maximum
-- value you can specify is controlled by a service limit.
newExecutionProperty ::
  ExecutionProperty
newExecutionProperty =
  ExecutionProperty'
    { maxConcurrentRuns =
        Prelude.Nothing
    }

-- | The maximum number of concurrent runs allowed for the job. The default
-- is 1. An error is returned when this threshold is reached. The maximum
-- value you can specify is controlled by a service limit.
executionProperty_maxConcurrentRuns :: Lens.Lens' ExecutionProperty (Prelude.Maybe Prelude.Int)
executionProperty_maxConcurrentRuns = Lens.lens (\ExecutionProperty' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@ExecutionProperty' {} a -> s {maxConcurrentRuns = a} :: ExecutionProperty)

instance Data.FromJSON ExecutionProperty where
  parseJSON =
    Data.withObject
      "ExecutionProperty"
      ( \x ->
          ExecutionProperty'
            Prelude.<$> (x Data..:? "MaxConcurrentRuns")
      )

instance Prelude.Hashable ExecutionProperty where
  hashWithSalt _salt ExecutionProperty' {..} =
    _salt `Prelude.hashWithSalt` maxConcurrentRuns

instance Prelude.NFData ExecutionProperty where
  rnf ExecutionProperty' {..} =
    Prelude.rnf maxConcurrentRuns

instance Data.ToJSON ExecutionProperty where
  toJSON ExecutionProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxConcurrentRuns" Data..=)
              Prelude.<$> maxConcurrentRuns
          ]
      )

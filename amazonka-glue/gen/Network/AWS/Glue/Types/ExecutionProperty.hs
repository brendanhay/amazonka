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
-- Module      : Network.AWS.Glue.Types.ExecutionProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExecutionProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An execution property of a job.
--
-- /See:/ 'newExecutionProperty' smart constructor.
data ExecutionProperty = ExecutionProperty'
  { -- | The maximum number of concurrent runs allowed for the job. The default
    -- is 1. An error is returned when this threshold is reached. The maximum
    -- value you can specify is controlled by a service limit.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ExecutionProperty where
  parseJSON =
    Prelude.withObject
      "ExecutionProperty"
      ( \x ->
          ExecutionProperty'
            Prelude.<$> (x Prelude..:? "MaxConcurrentRuns")
      )

instance Prelude.Hashable ExecutionProperty

instance Prelude.NFData ExecutionProperty

instance Prelude.ToJSON ExecutionProperty where
  toJSON ExecutionProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxConcurrentRuns" Prelude..=)
              Prelude.<$> maxConcurrentRuns
          ]
      )

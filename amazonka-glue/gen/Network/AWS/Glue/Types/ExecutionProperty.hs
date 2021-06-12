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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An execution property of a job.
--
-- /See:/ 'newExecutionProperty' smart constructor.
data ExecutionProperty = ExecutionProperty'
  { -- | The maximum number of concurrent runs allowed for the job. The default
    -- is 1. An error is returned when this threshold is reached. The maximum
    -- value you can specify is controlled by a service limit.
    maxConcurrentRuns :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The maximum number of concurrent runs allowed for the job. The default
-- is 1. An error is returned when this threshold is reached. The maximum
-- value you can specify is controlled by a service limit.
executionProperty_maxConcurrentRuns :: Lens.Lens' ExecutionProperty (Core.Maybe Core.Int)
executionProperty_maxConcurrentRuns = Lens.lens (\ExecutionProperty' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@ExecutionProperty' {} a -> s {maxConcurrentRuns = a} :: ExecutionProperty)

instance Core.FromJSON ExecutionProperty where
  parseJSON =
    Core.withObject
      "ExecutionProperty"
      ( \x ->
          ExecutionProperty'
            Core.<$> (x Core..:? "MaxConcurrentRuns")
      )

instance Core.Hashable ExecutionProperty

instance Core.NFData ExecutionProperty

instance Core.ToJSON ExecutionProperty where
  toJSON ExecutionProperty' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxConcurrentRuns" Core..=)
              Core.<$> maxConcurrentRuns
          ]
      )

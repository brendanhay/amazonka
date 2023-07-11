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
-- Module      : Amazonka.SSM.Types.StepExecutionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.StepExecutionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.StepExecutionFilterKey

-- | A filter to limit the amount of step execution information returned by
-- the call.
--
-- /See:/ 'newStepExecutionFilter' smart constructor.
data StepExecutionFilter = StepExecutionFilter'
  { -- | One or more keys to limit the results. Valid filter keys include the
    -- following: StepName, Action, StepExecutionId, StepExecutionStatus,
    -- StartTimeBefore, StartTimeAfter.
    key :: StepExecutionFilterKey,
    -- | The values of the filter key.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepExecutionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'stepExecutionFilter_key' - One or more keys to limit the results. Valid filter keys include the
-- following: StepName, Action, StepExecutionId, StepExecutionStatus,
-- StartTimeBefore, StartTimeAfter.
--
-- 'values', 'stepExecutionFilter_values' - The values of the filter key.
newStepExecutionFilter ::
  -- | 'key'
  StepExecutionFilterKey ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  StepExecutionFilter
newStepExecutionFilter pKey_ pValues_ =
  StepExecutionFilter'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | One or more keys to limit the results. Valid filter keys include the
-- following: StepName, Action, StepExecutionId, StepExecutionStatus,
-- StartTimeBefore, StartTimeAfter.
stepExecutionFilter_key :: Lens.Lens' StepExecutionFilter StepExecutionFilterKey
stepExecutionFilter_key = Lens.lens (\StepExecutionFilter' {key} -> key) (\s@StepExecutionFilter' {} a -> s {key = a} :: StepExecutionFilter)

-- | The values of the filter key.
stepExecutionFilter_values :: Lens.Lens' StepExecutionFilter (Prelude.NonEmpty Prelude.Text)
stepExecutionFilter_values = Lens.lens (\StepExecutionFilter' {values} -> values) (\s@StepExecutionFilter' {} a -> s {values = a} :: StepExecutionFilter) Prelude.. Lens.coerced

instance Prelude.Hashable StepExecutionFilter where
  hashWithSalt _salt StepExecutionFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData StepExecutionFilter where
  rnf StepExecutionFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON StepExecutionFilter where
  toJSON StepExecutionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )

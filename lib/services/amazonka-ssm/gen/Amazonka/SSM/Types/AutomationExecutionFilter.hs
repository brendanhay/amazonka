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
-- Module      : Amazonka.SSM.Types.AutomationExecutionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AutomationExecutionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AutomationExecutionFilterKey

-- | A filter used to match specific automation executions. This is used to
-- limit the scope of Automation execution information returned.
--
-- /See:/ 'newAutomationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { -- | One or more keys to limit the results.
    key :: AutomationExecutionFilterKey,
    -- | The values used to limit the execution information associated with the
    -- filter\'s key.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationExecutionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'automationExecutionFilter_key' - One or more keys to limit the results.
--
-- 'values', 'automationExecutionFilter_values' - The values used to limit the execution information associated with the
-- filter\'s key.
newAutomationExecutionFilter ::
  -- | 'key'
  AutomationExecutionFilterKey ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  AutomationExecutionFilter
newAutomationExecutionFilter pKey_ pValues_ =
  AutomationExecutionFilter'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | One or more keys to limit the results.
automationExecutionFilter_key :: Lens.Lens' AutomationExecutionFilter AutomationExecutionFilterKey
automationExecutionFilter_key = Lens.lens (\AutomationExecutionFilter' {key} -> key) (\s@AutomationExecutionFilter' {} a -> s {key = a} :: AutomationExecutionFilter)

-- | The values used to limit the execution information associated with the
-- filter\'s key.
automationExecutionFilter_values :: Lens.Lens' AutomationExecutionFilter (Prelude.NonEmpty Prelude.Text)
automationExecutionFilter_values = Lens.lens (\AutomationExecutionFilter' {values} -> values) (\s@AutomationExecutionFilter' {} a -> s {values = a} :: AutomationExecutionFilter) Prelude.. Lens.coerced

instance Prelude.Hashable AutomationExecutionFilter where
  hashWithSalt _salt AutomationExecutionFilter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData AutomationExecutionFilter where
  rnf AutomationExecutionFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Core.ToJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Values" Core..= values)
          ]
      )

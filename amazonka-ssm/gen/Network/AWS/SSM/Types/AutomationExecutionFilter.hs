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
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AutomationExecutionFilterKey

-- | A filter used to match specific automation executions. This is used to
-- limit the scope of Automation execution information returned.
--
-- /See:/ 'newAutomationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { -- | One or more keys to limit the results.
    key :: AutomationExecutionFilterKey,
    -- | The values used to limit the execution information associated with the
    -- filter\'s key.
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  AutomationExecutionFilter
newAutomationExecutionFilter pKey_ pValues_ =
  AutomationExecutionFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | One or more keys to limit the results.
automationExecutionFilter_key :: Lens.Lens' AutomationExecutionFilter AutomationExecutionFilterKey
automationExecutionFilter_key = Lens.lens (\AutomationExecutionFilter' {key} -> key) (\s@AutomationExecutionFilter' {} a -> s {key = a} :: AutomationExecutionFilter)

-- | The values used to limit the execution information associated with the
-- filter\'s key.
automationExecutionFilter_values :: Lens.Lens' AutomationExecutionFilter (Core.NonEmpty Core.Text)
automationExecutionFilter_values = Lens.lens (\AutomationExecutionFilter' {values} -> values) (\s@AutomationExecutionFilter' {} a -> s {values = a} :: AutomationExecutionFilter) Core.. Lens._Coerce

instance Core.Hashable AutomationExecutionFilter

instance Core.NFData AutomationExecutionFilter

instance Core.ToJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )

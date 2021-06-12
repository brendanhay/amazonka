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
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRule where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import qualified Network.AWS.Lens as Lens

-- | A structure that contains the name and configuration information of a
-- late data rule.
--
-- /See:/ 'newLateDataRule' smart constructor.
data LateDataRule = LateDataRule'
  { -- | The name of the late data rule.
    ruleName :: Core.Maybe Core.Text,
    -- | The information needed to configure the late data rule.
    ruleConfiguration :: LateDataRuleConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LateDataRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'lateDataRule_ruleName' - The name of the late data rule.
--
-- 'ruleConfiguration', 'lateDataRule_ruleConfiguration' - The information needed to configure the late data rule.
newLateDataRule ::
  -- | 'ruleConfiguration'
  LateDataRuleConfiguration ->
  LateDataRule
newLateDataRule pRuleConfiguration_ =
  LateDataRule'
    { ruleName = Core.Nothing,
      ruleConfiguration = pRuleConfiguration_
    }

-- | The name of the late data rule.
lateDataRule_ruleName :: Lens.Lens' LateDataRule (Core.Maybe Core.Text)
lateDataRule_ruleName = Lens.lens (\LateDataRule' {ruleName} -> ruleName) (\s@LateDataRule' {} a -> s {ruleName = a} :: LateDataRule)

-- | The information needed to configure the late data rule.
lateDataRule_ruleConfiguration :: Lens.Lens' LateDataRule LateDataRuleConfiguration
lateDataRule_ruleConfiguration = Lens.lens (\LateDataRule' {ruleConfiguration} -> ruleConfiguration) (\s@LateDataRule' {} a -> s {ruleConfiguration = a} :: LateDataRule)

instance Core.FromJSON LateDataRule where
  parseJSON =
    Core.withObject
      "LateDataRule"
      ( \x ->
          LateDataRule'
            Core.<$> (x Core..:? "ruleName")
            Core.<*> (x Core..: "ruleConfiguration")
      )

instance Core.Hashable LateDataRule

instance Core.NFData LateDataRule

instance Core.ToJSON LateDataRule where
  toJSON LateDataRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ruleName" Core..=) Core.<$> ruleName,
            Core.Just
              ("ruleConfiguration" Core..= ruleConfiguration)
          ]
      )

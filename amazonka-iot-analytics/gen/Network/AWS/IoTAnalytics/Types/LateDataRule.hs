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
-- Module      : Network.AWS.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LateDataRule where

import Network.AWS.IoTAnalytics.Types.LateDataRuleConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that contains the name and configuration information of a
-- late data rule.
--
-- /See:/ 'newLateDataRule' smart constructor.
data LateDataRule = LateDataRule'
  { -- | The name of the late data rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The information needed to configure the late data rule.
    ruleConfiguration :: LateDataRuleConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ruleName = Prelude.Nothing,
      ruleConfiguration = pRuleConfiguration_
    }

-- | The name of the late data rule.
lateDataRule_ruleName :: Lens.Lens' LateDataRule (Prelude.Maybe Prelude.Text)
lateDataRule_ruleName = Lens.lens (\LateDataRule' {ruleName} -> ruleName) (\s@LateDataRule' {} a -> s {ruleName = a} :: LateDataRule)

-- | The information needed to configure the late data rule.
lateDataRule_ruleConfiguration :: Lens.Lens' LateDataRule LateDataRuleConfiguration
lateDataRule_ruleConfiguration = Lens.lens (\LateDataRule' {ruleConfiguration} -> ruleConfiguration) (\s@LateDataRule' {} a -> s {ruleConfiguration = a} :: LateDataRule)

instance Prelude.FromJSON LateDataRule where
  parseJSON =
    Prelude.withObject
      "LateDataRule"
      ( \x ->
          LateDataRule'
            Prelude.<$> (x Prelude..:? "ruleName")
            Prelude.<*> (x Prelude..: "ruleConfiguration")
      )

instance Prelude.Hashable LateDataRule

instance Prelude.NFData LateDataRule

instance Prelude.ToJSON LateDataRule where
  toJSON LateDataRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ruleName" Prelude..=) Prelude.<$> ruleName,
            Prelude.Just
              ("ruleConfiguration" Prelude..= ruleConfiguration)
          ]
      )

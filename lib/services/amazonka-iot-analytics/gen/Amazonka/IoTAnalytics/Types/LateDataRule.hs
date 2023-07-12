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
-- Module      : Amazonka.IoTAnalytics.Types.LateDataRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.LateDataRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.LateDataRuleConfiguration
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON LateDataRule where
  parseJSON =
    Data.withObject
      "LateDataRule"
      ( \x ->
          LateDataRule'
            Prelude.<$> (x Data..:? "ruleName")
            Prelude.<*> (x Data..: "ruleConfiguration")
      )

instance Prelude.Hashable LateDataRule where
  hashWithSalt _salt LateDataRule' {..} =
    _salt
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` ruleConfiguration

instance Prelude.NFData LateDataRule where
  rnf LateDataRule' {..} =
    Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf ruleConfiguration

instance Data.ToJSON LateDataRule where
  toJSON LateDataRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ruleName" Data..=) Prelude.<$> ruleName,
            Prelude.Just
              ("ruleConfiguration" Data..= ruleConfiguration)
          ]
      )

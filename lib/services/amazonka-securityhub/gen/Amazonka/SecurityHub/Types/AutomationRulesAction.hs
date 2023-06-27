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
-- Module      : Amazonka.SecurityHub.Types.AutomationRulesAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AutomationRulesAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AutomationRulesActionType
import Amazonka.SecurityHub.Types.AutomationRulesFindingFieldsUpdate

-- | One or more actions to update finding fields if a finding matches the
-- defined criteria of the rule.
--
-- /See:/ 'newAutomationRulesAction' smart constructor.
data AutomationRulesAction = AutomationRulesAction'
  { -- | Specifies that the automation rule action is an update to a finding
    -- field.
    findingFieldsUpdate :: Prelude.Maybe AutomationRulesFindingFieldsUpdate,
    -- | Specifies that the rule action should update the @Types@ finding field.
    -- The @Types@ finding field provides one or more finding types in the
    -- format of namespace\/category\/classifier that classify a finding. For
    -- more information, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
    -- in the /Security Hub User Guide/.
    type' :: Prelude.Maybe AutomationRulesActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationRulesAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingFieldsUpdate', 'automationRulesAction_findingFieldsUpdate' - Specifies that the automation rule action is an update to a finding
-- field.
--
-- 'type'', 'automationRulesAction_type' - Specifies that the rule action should update the @Types@ finding field.
-- The @Types@ finding field provides one or more finding types in the
-- format of namespace\/category\/classifier that classify a finding. For
-- more information, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
-- in the /Security Hub User Guide/.
newAutomationRulesAction ::
  AutomationRulesAction
newAutomationRulesAction =
  AutomationRulesAction'
    { findingFieldsUpdate =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Specifies that the automation rule action is an update to a finding
-- field.
automationRulesAction_findingFieldsUpdate :: Lens.Lens' AutomationRulesAction (Prelude.Maybe AutomationRulesFindingFieldsUpdate)
automationRulesAction_findingFieldsUpdate = Lens.lens (\AutomationRulesAction' {findingFieldsUpdate} -> findingFieldsUpdate) (\s@AutomationRulesAction' {} a -> s {findingFieldsUpdate = a} :: AutomationRulesAction)

-- | Specifies that the rule action should update the @Types@ finding field.
-- The @Types@ finding field provides one or more finding types in the
-- format of namespace\/category\/classifier that classify a finding. For
-- more information, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
-- in the /Security Hub User Guide/.
automationRulesAction_type :: Lens.Lens' AutomationRulesAction (Prelude.Maybe AutomationRulesActionType)
automationRulesAction_type = Lens.lens (\AutomationRulesAction' {type'} -> type') (\s@AutomationRulesAction' {} a -> s {type' = a} :: AutomationRulesAction)

instance Data.FromJSON AutomationRulesAction where
  parseJSON =
    Data.withObject
      "AutomationRulesAction"
      ( \x ->
          AutomationRulesAction'
            Prelude.<$> (x Data..:? "FindingFieldsUpdate")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AutomationRulesAction where
  hashWithSalt _salt AutomationRulesAction' {..} =
    _salt
      `Prelude.hashWithSalt` findingFieldsUpdate
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AutomationRulesAction where
  rnf AutomationRulesAction' {..} =
    Prelude.rnf findingFieldsUpdate
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AutomationRulesAction where
  toJSON AutomationRulesAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FindingFieldsUpdate" Data..=)
              Prelude.<$> findingFieldsUpdate,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

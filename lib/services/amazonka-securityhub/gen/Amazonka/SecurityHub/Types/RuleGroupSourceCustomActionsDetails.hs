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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatelessCustomActionDefinition

-- | A custom action definition. A custom action is an optional, non-standard
-- action to use for stateless packet handling.
--
-- /See:/ 'newRuleGroupSourceCustomActionsDetails' smart constructor.
data RuleGroupSourceCustomActionsDetails = RuleGroupSourceCustomActionsDetails'
  { -- | A descriptive name of the custom action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The definition of a custom action.
    actionDefinition :: Prelude.Maybe StatelessCustomActionDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceCustomActionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'ruleGroupSourceCustomActionsDetails_actionName' - A descriptive name of the custom action.
--
-- 'actionDefinition', 'ruleGroupSourceCustomActionsDetails_actionDefinition' - The definition of a custom action.
newRuleGroupSourceCustomActionsDetails ::
  RuleGroupSourceCustomActionsDetails
newRuleGroupSourceCustomActionsDetails =
  RuleGroupSourceCustomActionsDetails'
    { actionName =
        Prelude.Nothing,
      actionDefinition = Prelude.Nothing
    }

-- | A descriptive name of the custom action.
ruleGroupSourceCustomActionsDetails_actionName :: Lens.Lens' RuleGroupSourceCustomActionsDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceCustomActionsDetails_actionName = Lens.lens (\RuleGroupSourceCustomActionsDetails' {actionName} -> actionName) (\s@RuleGroupSourceCustomActionsDetails' {} a -> s {actionName = a} :: RuleGroupSourceCustomActionsDetails)

-- | The definition of a custom action.
ruleGroupSourceCustomActionsDetails_actionDefinition :: Lens.Lens' RuleGroupSourceCustomActionsDetails (Prelude.Maybe StatelessCustomActionDefinition)
ruleGroupSourceCustomActionsDetails_actionDefinition = Lens.lens (\RuleGroupSourceCustomActionsDetails' {actionDefinition} -> actionDefinition) (\s@RuleGroupSourceCustomActionsDetails' {} a -> s {actionDefinition = a} :: RuleGroupSourceCustomActionsDetails)

instance
  Core.FromJSON
    RuleGroupSourceCustomActionsDetails
  where
  parseJSON =
    Core.withObject
      "RuleGroupSourceCustomActionsDetails"
      ( \x ->
          RuleGroupSourceCustomActionsDetails'
            Prelude.<$> (x Core..:? "ActionName")
            Prelude.<*> (x Core..:? "ActionDefinition")
      )

instance
  Prelude.Hashable
    RuleGroupSourceCustomActionsDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceCustomActionsDetails' {..} =
      _salt `Prelude.hashWithSalt` actionName
        `Prelude.hashWithSalt` actionDefinition

instance
  Prelude.NFData
    RuleGroupSourceCustomActionsDetails
  where
  rnf RuleGroupSourceCustomActionsDetails' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf actionDefinition

instance
  Core.ToJSON
    RuleGroupSourceCustomActionsDetails
  where
  toJSON RuleGroupSourceCustomActionsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ActionName" Core..=) Prelude.<$> actionName,
            ("ActionDefinition" Core..=)
              Prelude.<$> actionDefinition
          ]
      )

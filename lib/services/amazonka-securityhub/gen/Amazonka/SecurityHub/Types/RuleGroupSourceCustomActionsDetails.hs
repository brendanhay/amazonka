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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceCustomActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatelessCustomActionDefinition

-- | A custom action definition. A custom action is an optional, non-standard
-- action to use for stateless packet handling.
--
-- /See:/ 'newRuleGroupSourceCustomActionsDetails' smart constructor.
data RuleGroupSourceCustomActionsDetails = RuleGroupSourceCustomActionsDetails'
  { -- | The definition of a custom action.
    actionDefinition :: Prelude.Maybe StatelessCustomActionDefinition,
    -- | A descriptive name of the custom action.
    actionName :: Prelude.Maybe Prelude.Text
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
-- 'actionDefinition', 'ruleGroupSourceCustomActionsDetails_actionDefinition' - The definition of a custom action.
--
-- 'actionName', 'ruleGroupSourceCustomActionsDetails_actionName' - A descriptive name of the custom action.
newRuleGroupSourceCustomActionsDetails ::
  RuleGroupSourceCustomActionsDetails
newRuleGroupSourceCustomActionsDetails =
  RuleGroupSourceCustomActionsDetails'
    { actionDefinition =
        Prelude.Nothing,
      actionName = Prelude.Nothing
    }

-- | The definition of a custom action.
ruleGroupSourceCustomActionsDetails_actionDefinition :: Lens.Lens' RuleGroupSourceCustomActionsDetails (Prelude.Maybe StatelessCustomActionDefinition)
ruleGroupSourceCustomActionsDetails_actionDefinition = Lens.lens (\RuleGroupSourceCustomActionsDetails' {actionDefinition} -> actionDefinition) (\s@RuleGroupSourceCustomActionsDetails' {} a -> s {actionDefinition = a} :: RuleGroupSourceCustomActionsDetails)

-- | A descriptive name of the custom action.
ruleGroupSourceCustomActionsDetails_actionName :: Lens.Lens' RuleGroupSourceCustomActionsDetails (Prelude.Maybe Prelude.Text)
ruleGroupSourceCustomActionsDetails_actionName = Lens.lens (\RuleGroupSourceCustomActionsDetails' {actionName} -> actionName) (\s@RuleGroupSourceCustomActionsDetails' {} a -> s {actionName = a} :: RuleGroupSourceCustomActionsDetails)

instance
  Data.FromJSON
    RuleGroupSourceCustomActionsDetails
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceCustomActionsDetails"
      ( \x ->
          RuleGroupSourceCustomActionsDetails'
            Prelude.<$> (x Data..:? "ActionDefinition")
            Prelude.<*> (x Data..:? "ActionName")
      )

instance
  Prelude.Hashable
    RuleGroupSourceCustomActionsDetails
  where
  hashWithSalt
    _salt
    RuleGroupSourceCustomActionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` actionDefinition
        `Prelude.hashWithSalt` actionName

instance
  Prelude.NFData
    RuleGroupSourceCustomActionsDetails
  where
  rnf RuleGroupSourceCustomActionsDetails' {..} =
    Prelude.rnf actionDefinition
      `Prelude.seq` Prelude.rnf actionName

instance
  Data.ToJSON
    RuleGroupSourceCustomActionsDetails
  where
  toJSON RuleGroupSourceCustomActionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionDefinition" Data..=)
              Prelude.<$> actionDefinition,
            ("ActionName" Data..=) Prelude.<$> actionName
          ]
      )

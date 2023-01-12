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
-- Module      : Amazonka.WorkMail.Types.ImpersonationRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.ImpersonationRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.AccessEffect

-- | The rules for the given impersonation role.
--
-- /See:/ 'newImpersonationRule' smart constructor.
data ImpersonationRule = ImpersonationRule'
  { -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The rule name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of user IDs that don\'t match the rule.
    notTargetUsers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of user IDs that match the rule.
    targetUsers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The identifier of the rule.
    impersonationRuleId :: Prelude.Text,
    -- | The effect of the rule when it matches the input. Allowed effect values
    -- are @ALLOW@ or @DENY@.
    effect :: AccessEffect
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImpersonationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'impersonationRule_description' - The rule description.
--
-- 'name', 'impersonationRule_name' - The rule name.
--
-- 'notTargetUsers', 'impersonationRule_notTargetUsers' - A list of user IDs that don\'t match the rule.
--
-- 'targetUsers', 'impersonationRule_targetUsers' - A list of user IDs that match the rule.
--
-- 'impersonationRuleId', 'impersonationRule_impersonationRuleId' - The identifier of the rule.
--
-- 'effect', 'impersonationRule_effect' - The effect of the rule when it matches the input. Allowed effect values
-- are @ALLOW@ or @DENY@.
newImpersonationRule ::
  -- | 'impersonationRuleId'
  Prelude.Text ->
  -- | 'effect'
  AccessEffect ->
  ImpersonationRule
newImpersonationRule pImpersonationRuleId_ pEffect_ =
  ImpersonationRule'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      notTargetUsers = Prelude.Nothing,
      targetUsers = Prelude.Nothing,
      impersonationRuleId = pImpersonationRuleId_,
      effect = pEffect_
    }

-- | The rule description.
impersonationRule_description :: Lens.Lens' ImpersonationRule (Prelude.Maybe Prelude.Text)
impersonationRule_description = Lens.lens (\ImpersonationRule' {description} -> description) (\s@ImpersonationRule' {} a -> s {description = a} :: ImpersonationRule)

-- | The rule name.
impersonationRule_name :: Lens.Lens' ImpersonationRule (Prelude.Maybe Prelude.Text)
impersonationRule_name = Lens.lens (\ImpersonationRule' {name} -> name) (\s@ImpersonationRule' {} a -> s {name = a} :: ImpersonationRule)

-- | A list of user IDs that don\'t match the rule.
impersonationRule_notTargetUsers :: Lens.Lens' ImpersonationRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
impersonationRule_notTargetUsers = Lens.lens (\ImpersonationRule' {notTargetUsers} -> notTargetUsers) (\s@ImpersonationRule' {} a -> s {notTargetUsers = a} :: ImpersonationRule) Prelude.. Lens.mapping Lens.coerced

-- | A list of user IDs that match the rule.
impersonationRule_targetUsers :: Lens.Lens' ImpersonationRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
impersonationRule_targetUsers = Lens.lens (\ImpersonationRule' {targetUsers} -> targetUsers) (\s@ImpersonationRule' {} a -> s {targetUsers = a} :: ImpersonationRule) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the rule.
impersonationRule_impersonationRuleId :: Lens.Lens' ImpersonationRule Prelude.Text
impersonationRule_impersonationRuleId = Lens.lens (\ImpersonationRule' {impersonationRuleId} -> impersonationRuleId) (\s@ImpersonationRule' {} a -> s {impersonationRuleId = a} :: ImpersonationRule)

-- | The effect of the rule when it matches the input. Allowed effect values
-- are @ALLOW@ or @DENY@.
impersonationRule_effect :: Lens.Lens' ImpersonationRule AccessEffect
impersonationRule_effect = Lens.lens (\ImpersonationRule' {effect} -> effect) (\s@ImpersonationRule' {} a -> s {effect = a} :: ImpersonationRule)

instance Data.FromJSON ImpersonationRule where
  parseJSON =
    Data.withObject
      "ImpersonationRule"
      ( \x ->
          ImpersonationRule'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotTargetUsers")
            Prelude.<*> (x Data..:? "TargetUsers")
            Prelude.<*> (x Data..: "ImpersonationRuleId")
            Prelude.<*> (x Data..: "Effect")
      )

instance Prelude.Hashable ImpersonationRule where
  hashWithSalt _salt ImpersonationRule' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notTargetUsers
      `Prelude.hashWithSalt` targetUsers
      `Prelude.hashWithSalt` impersonationRuleId
      `Prelude.hashWithSalt` effect

instance Prelude.NFData ImpersonationRule where
  rnf ImpersonationRule' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notTargetUsers
      `Prelude.seq` Prelude.rnf targetUsers
      `Prelude.seq` Prelude.rnf impersonationRuleId
      `Prelude.seq` Prelude.rnf effect

instance Data.ToJSON ImpersonationRule where
  toJSON ImpersonationRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("NotTargetUsers" Data..=)
              Prelude.<$> notTargetUsers,
            ("TargetUsers" Data..=) Prelude.<$> targetUsers,
            Prelude.Just
              ("ImpersonationRuleId" Data..= impersonationRuleId),
            Prelude.Just ("Effect" Data..= effect)
          ]
      )

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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.ImpersonationRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.AccessEffect

-- | The rules for the given impersonation role.
--
-- /See:/ 'newImpersonationRule' smart constructor.
data ImpersonationRule = ImpersonationRule'
  { -- | The rule name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of user IDs that match the rule.
    targetUsers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of user IDs that don\'t match the rule.
    notTargetUsers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'name', 'impersonationRule_name' - The rule name.
--
-- 'targetUsers', 'impersonationRule_targetUsers' - A list of user IDs that match the rule.
--
-- 'description', 'impersonationRule_description' - The rule description.
--
-- 'notTargetUsers', 'impersonationRule_notTargetUsers' - A list of user IDs that don\'t match the rule.
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
    { name = Prelude.Nothing,
      targetUsers = Prelude.Nothing,
      description = Prelude.Nothing,
      notTargetUsers = Prelude.Nothing,
      impersonationRuleId = pImpersonationRuleId_,
      effect = pEffect_
    }

-- | The rule name.
impersonationRule_name :: Lens.Lens' ImpersonationRule (Prelude.Maybe Prelude.Text)
impersonationRule_name = Lens.lens (\ImpersonationRule' {name} -> name) (\s@ImpersonationRule' {} a -> s {name = a} :: ImpersonationRule)

-- | A list of user IDs that match the rule.
impersonationRule_targetUsers :: Lens.Lens' ImpersonationRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
impersonationRule_targetUsers = Lens.lens (\ImpersonationRule' {targetUsers} -> targetUsers) (\s@ImpersonationRule' {} a -> s {targetUsers = a} :: ImpersonationRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule description.
impersonationRule_description :: Lens.Lens' ImpersonationRule (Prelude.Maybe Prelude.Text)
impersonationRule_description = Lens.lens (\ImpersonationRule' {description} -> description) (\s@ImpersonationRule' {} a -> s {description = a} :: ImpersonationRule)

-- | A list of user IDs that don\'t match the rule.
impersonationRule_notTargetUsers :: Lens.Lens' ImpersonationRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
impersonationRule_notTargetUsers = Lens.lens (\ImpersonationRule' {notTargetUsers} -> notTargetUsers) (\s@ImpersonationRule' {} a -> s {notTargetUsers = a} :: ImpersonationRule) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the rule.
impersonationRule_impersonationRuleId :: Lens.Lens' ImpersonationRule Prelude.Text
impersonationRule_impersonationRuleId = Lens.lens (\ImpersonationRule' {impersonationRuleId} -> impersonationRuleId) (\s@ImpersonationRule' {} a -> s {impersonationRuleId = a} :: ImpersonationRule)

-- | The effect of the rule when it matches the input. Allowed effect values
-- are @ALLOW@ or @DENY@.
impersonationRule_effect :: Lens.Lens' ImpersonationRule AccessEffect
impersonationRule_effect = Lens.lens (\ImpersonationRule' {effect} -> effect) (\s@ImpersonationRule' {} a -> s {effect = a} :: ImpersonationRule)

instance Core.FromJSON ImpersonationRule where
  parseJSON =
    Core.withObject
      "ImpersonationRule"
      ( \x ->
          ImpersonationRule'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "TargetUsers")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "NotTargetUsers")
            Prelude.<*> (x Core..: "ImpersonationRuleId")
            Prelude.<*> (x Core..: "Effect")
      )

instance Prelude.Hashable ImpersonationRule where
  hashWithSalt _salt ImpersonationRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetUsers
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notTargetUsers
      `Prelude.hashWithSalt` impersonationRuleId
      `Prelude.hashWithSalt` effect

instance Prelude.NFData ImpersonationRule where
  rnf ImpersonationRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetUsers
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notTargetUsers
      `Prelude.seq` Prelude.rnf impersonationRuleId
      `Prelude.seq` Prelude.rnf effect

instance Core.ToJSON ImpersonationRule where
  toJSON ImpersonationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("TargetUsers" Core..=) Prelude.<$> targetUsers,
            ("Description" Core..=) Prelude.<$> description,
            ("NotTargetUsers" Core..=)
              Prelude.<$> notTargetUsers,
            Prelude.Just
              ("ImpersonationRuleId" Core..= impersonationRuleId),
            Prelude.Just ("Effect" Core..= effect)
          ]
      )

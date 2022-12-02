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
-- Module      : Amazonka.CognitoIdentity.Types.RulesConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.RulesConfigurationType where

import Amazonka.CognitoIdentity.Types.MappingRule
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for rules.
--
-- /See:/ 'newRulesConfigurationType' smart constructor.
data RulesConfigurationType = RulesConfigurationType'
  { -- | An array of rules. You can specify up to 25 rules per identity provider.
    --
    -- Rules are evaluated in order. The first one to match specifies the role.
    rules :: Prelude.NonEmpty MappingRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulesConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'rulesConfigurationType_rules' - An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
newRulesConfigurationType ::
  -- | 'rules'
  Prelude.NonEmpty MappingRule ->
  RulesConfigurationType
newRulesConfigurationType pRules_ =
  RulesConfigurationType'
    { rules =
        Lens.coerced Lens.# pRules_
    }

-- | An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
rulesConfigurationType_rules :: Lens.Lens' RulesConfigurationType (Prelude.NonEmpty MappingRule)
rulesConfigurationType_rules = Lens.lens (\RulesConfigurationType' {rules} -> rules) (\s@RulesConfigurationType' {} a -> s {rules = a} :: RulesConfigurationType) Prelude.. Lens.coerced

instance Data.FromJSON RulesConfigurationType where
  parseJSON =
    Data.withObject
      "RulesConfigurationType"
      ( \x ->
          RulesConfigurationType'
            Prelude.<$> (x Data..: "Rules")
      )

instance Prelude.Hashable RulesConfigurationType where
  hashWithSalt _salt RulesConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` rules

instance Prelude.NFData RulesConfigurationType where
  rnf RulesConfigurationType' {..} = Prelude.rnf rules

instance Data.ToJSON RulesConfigurationType where
  toJSON RulesConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Rules" Data..= rules)]
      )

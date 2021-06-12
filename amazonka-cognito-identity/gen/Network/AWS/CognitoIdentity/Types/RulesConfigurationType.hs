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
-- Module      : Network.AWS.CognitoIdentity.Types.RulesConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RulesConfigurationType where

import Network.AWS.CognitoIdentity.Types.MappingRule
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for rules.
--
-- /See:/ 'newRulesConfigurationType' smart constructor.
data RulesConfigurationType = RulesConfigurationType'
  { -- | An array of rules. You can specify up to 25 rules per identity provider.
    --
    -- Rules are evaluated in order. The first one to match specifies the role.
    rules :: Core.NonEmpty MappingRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty MappingRule ->
  RulesConfigurationType
newRulesConfigurationType pRules_ =
  RulesConfigurationType'
    { rules =
        Lens._Coerce Lens.# pRules_
    }

-- | An array of rules. You can specify up to 25 rules per identity provider.
--
-- Rules are evaluated in order. The first one to match specifies the role.
rulesConfigurationType_rules :: Lens.Lens' RulesConfigurationType (Core.NonEmpty MappingRule)
rulesConfigurationType_rules = Lens.lens (\RulesConfigurationType' {rules} -> rules) (\s@RulesConfigurationType' {} a -> s {rules = a} :: RulesConfigurationType) Core.. Lens._Coerce

instance Core.FromJSON RulesConfigurationType where
  parseJSON =
    Core.withObject
      "RulesConfigurationType"
      ( \x ->
          RulesConfigurationType' Core.<$> (x Core..: "Rules")
      )

instance Core.Hashable RulesConfigurationType

instance Core.NFData RulesConfigurationType

instance Core.ToJSON RulesConfigurationType where
  toJSON RulesConfigurationType' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Rules" Core..= rules)])

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
-- Module      : Amazonka.EC2.Types.RuleGroupRuleOptionsPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RuleGroupRuleOptionsPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RuleOption
import qualified Amazonka.Prelude as Prelude

-- | Describes the rule options for a stateful rule group.
--
-- /See:/ 'newRuleGroupRuleOptionsPair' smart constructor.
data RuleGroupRuleOptionsPair = RuleGroupRuleOptionsPair'
  { -- | The ARN of the rule group.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The rule options.
    ruleOptions :: Prelude.Maybe [RuleOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupRuleOptionsPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupArn', 'ruleGroupRuleOptionsPair_ruleGroupArn' - The ARN of the rule group.
--
-- 'ruleOptions', 'ruleGroupRuleOptionsPair_ruleOptions' - The rule options.
newRuleGroupRuleOptionsPair ::
  RuleGroupRuleOptionsPair
newRuleGroupRuleOptionsPair =
  RuleGroupRuleOptionsPair'
    { ruleGroupArn =
        Prelude.Nothing,
      ruleOptions = Prelude.Nothing
    }

-- | The ARN of the rule group.
ruleGroupRuleOptionsPair_ruleGroupArn :: Lens.Lens' RuleGroupRuleOptionsPair (Prelude.Maybe Prelude.Text)
ruleGroupRuleOptionsPair_ruleGroupArn = Lens.lens (\RuleGroupRuleOptionsPair' {ruleGroupArn} -> ruleGroupArn) (\s@RuleGroupRuleOptionsPair' {} a -> s {ruleGroupArn = a} :: RuleGroupRuleOptionsPair)

-- | The rule options.
ruleGroupRuleOptionsPair_ruleOptions :: Lens.Lens' RuleGroupRuleOptionsPair (Prelude.Maybe [RuleOption])
ruleGroupRuleOptionsPair_ruleOptions = Lens.lens (\RuleGroupRuleOptionsPair' {ruleOptions} -> ruleOptions) (\s@RuleGroupRuleOptionsPair' {} a -> s {ruleOptions = a} :: RuleGroupRuleOptionsPair) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML RuleGroupRuleOptionsPair where
  parseXML x =
    RuleGroupRuleOptionsPair'
      Prelude.<$> (x Data..@? "ruleGroupArn")
      Prelude.<*> ( x
                      Data..@? "ruleOptionSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable RuleGroupRuleOptionsPair where
  hashWithSalt _salt RuleGroupRuleOptionsPair' {..} =
    _salt
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` ruleOptions

instance Prelude.NFData RuleGroupRuleOptionsPair where
  rnf RuleGroupRuleOptionsPair' {..} =
    Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleOptions

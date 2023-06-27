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
-- Module      : Amazonka.EC2.Types.RuleGroupTypePair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RuleGroupTypePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the type of a stateful rule group.
--
-- /See:/ 'newRuleGroupTypePair' smart constructor.
data RuleGroupTypePair = RuleGroupTypePair'
  { -- | The ARN of the rule group.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The rule group type. The possible values are @Domain List@ and
    -- @Suricata@.
    ruleGroupType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupTypePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupArn', 'ruleGroupTypePair_ruleGroupArn' - The ARN of the rule group.
--
-- 'ruleGroupType', 'ruleGroupTypePair_ruleGroupType' - The rule group type. The possible values are @Domain List@ and
-- @Suricata@.
newRuleGroupTypePair ::
  RuleGroupTypePair
newRuleGroupTypePair =
  RuleGroupTypePair'
    { ruleGroupArn = Prelude.Nothing,
      ruleGroupType = Prelude.Nothing
    }

-- | The ARN of the rule group.
ruleGroupTypePair_ruleGroupArn :: Lens.Lens' RuleGroupTypePair (Prelude.Maybe Prelude.Text)
ruleGroupTypePair_ruleGroupArn = Lens.lens (\RuleGroupTypePair' {ruleGroupArn} -> ruleGroupArn) (\s@RuleGroupTypePair' {} a -> s {ruleGroupArn = a} :: RuleGroupTypePair)

-- | The rule group type. The possible values are @Domain List@ and
-- @Suricata@.
ruleGroupTypePair_ruleGroupType :: Lens.Lens' RuleGroupTypePair (Prelude.Maybe Prelude.Text)
ruleGroupTypePair_ruleGroupType = Lens.lens (\RuleGroupTypePair' {ruleGroupType} -> ruleGroupType) (\s@RuleGroupTypePair' {} a -> s {ruleGroupType = a} :: RuleGroupTypePair)

instance Data.FromXML RuleGroupTypePair where
  parseXML x =
    RuleGroupTypePair'
      Prelude.<$> (x Data..@? "ruleGroupArn")
      Prelude.<*> (x Data..@? "ruleGroupType")

instance Prelude.Hashable RuleGroupTypePair where
  hashWithSalt _salt RuleGroupTypePair' {..} =
    _salt
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` ruleGroupType

instance Prelude.NFData RuleGroupTypePair where
  rnf RuleGroupTypePair' {..} =
    Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf ruleGroupType

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
-- Module      : Amazonka.VPCLattice.Types.RuleUpdateSuccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleUpdateSuccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.RuleAction
import Amazonka.VPCLattice.Types.RuleMatch

-- | Describes a successful rule update.
--
-- /See:/ 'newRuleUpdateSuccess' smart constructor.
data RuleUpdateSuccess = RuleUpdateSuccess'
  { -- | The action for the default rule.
    action :: Prelude.Maybe RuleAction,
    -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default rule.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The rule match.
    match :: Prelude.Maybe RuleMatch,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rule priority.
    priority :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleUpdateSuccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'ruleUpdateSuccess_action' - The action for the default rule.
--
-- 'arn', 'ruleUpdateSuccess_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'id', 'ruleUpdateSuccess_id' - The ID of the listener.
--
-- 'isDefault', 'ruleUpdateSuccess_isDefault' - Indicates whether this is the default rule.
--
-- 'match', 'ruleUpdateSuccess_match' - The rule match.
--
-- 'name', 'ruleUpdateSuccess_name' - The name of the listener.
--
-- 'priority', 'ruleUpdateSuccess_priority' - The rule priority.
newRuleUpdateSuccess ::
  RuleUpdateSuccess
newRuleUpdateSuccess =
  RuleUpdateSuccess'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      match = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | The action for the default rule.
ruleUpdateSuccess_action :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe RuleAction)
ruleUpdateSuccess_action = Lens.lens (\RuleUpdateSuccess' {action} -> action) (\s@RuleUpdateSuccess' {} a -> s {action = a} :: RuleUpdateSuccess)

-- | The Amazon Resource Name (ARN) of the listener.
ruleUpdateSuccess_arn :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe Prelude.Text)
ruleUpdateSuccess_arn = Lens.lens (\RuleUpdateSuccess' {arn} -> arn) (\s@RuleUpdateSuccess' {} a -> s {arn = a} :: RuleUpdateSuccess)

-- | The ID of the listener.
ruleUpdateSuccess_id :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe Prelude.Text)
ruleUpdateSuccess_id = Lens.lens (\RuleUpdateSuccess' {id} -> id) (\s@RuleUpdateSuccess' {} a -> s {id = a} :: RuleUpdateSuccess)

-- | Indicates whether this is the default rule.
ruleUpdateSuccess_isDefault :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe Prelude.Bool)
ruleUpdateSuccess_isDefault = Lens.lens (\RuleUpdateSuccess' {isDefault} -> isDefault) (\s@RuleUpdateSuccess' {} a -> s {isDefault = a} :: RuleUpdateSuccess)

-- | The rule match.
ruleUpdateSuccess_match :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe RuleMatch)
ruleUpdateSuccess_match = Lens.lens (\RuleUpdateSuccess' {match} -> match) (\s@RuleUpdateSuccess' {} a -> s {match = a} :: RuleUpdateSuccess)

-- | The name of the listener.
ruleUpdateSuccess_name :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe Prelude.Text)
ruleUpdateSuccess_name = Lens.lens (\RuleUpdateSuccess' {name} -> name) (\s@RuleUpdateSuccess' {} a -> s {name = a} :: RuleUpdateSuccess)

-- | The rule priority.
ruleUpdateSuccess_priority :: Lens.Lens' RuleUpdateSuccess (Prelude.Maybe Prelude.Natural)
ruleUpdateSuccess_priority = Lens.lens (\RuleUpdateSuccess' {priority} -> priority) (\s@RuleUpdateSuccess' {} a -> s {priority = a} :: RuleUpdateSuccess)

instance Data.FromJSON RuleUpdateSuccess where
  parseJSON =
    Data.withObject
      "RuleUpdateSuccess"
      ( \x ->
          RuleUpdateSuccess'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "isDefault")
            Prelude.<*> (x Data..:? "match")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "priority")
      )

instance Prelude.Hashable RuleUpdateSuccess where
  hashWithSalt _salt RuleUpdateSuccess' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority

instance Prelude.NFData RuleUpdateSuccess where
  rnf RuleUpdateSuccess' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority

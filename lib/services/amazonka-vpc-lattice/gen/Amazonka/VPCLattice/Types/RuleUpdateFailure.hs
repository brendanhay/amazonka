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
-- Module      : Amazonka.VPCLattice.Types.RuleUpdateFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleUpdateFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a rule update that failed.
--
-- /See:/ 'newRuleUpdateFailure' smart constructor.
data RuleUpdateFailure = RuleUpdateFailure'
  { -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the rule.
    ruleIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleUpdateFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'ruleUpdateFailure_failureCode' - The failure code.
--
-- 'failureMessage', 'ruleUpdateFailure_failureMessage' - The failure message.
--
-- 'ruleIdentifier', 'ruleUpdateFailure_ruleIdentifier' - The ID or Amazon Resource Name (ARN) of the rule.
newRuleUpdateFailure ::
  RuleUpdateFailure
newRuleUpdateFailure =
  RuleUpdateFailure'
    { failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      ruleIdentifier = Prelude.Nothing
    }

-- | The failure code.
ruleUpdateFailure_failureCode :: Lens.Lens' RuleUpdateFailure (Prelude.Maybe Prelude.Text)
ruleUpdateFailure_failureCode = Lens.lens (\RuleUpdateFailure' {failureCode} -> failureCode) (\s@RuleUpdateFailure' {} a -> s {failureCode = a} :: RuleUpdateFailure)

-- | The failure message.
ruleUpdateFailure_failureMessage :: Lens.Lens' RuleUpdateFailure (Prelude.Maybe Prelude.Text)
ruleUpdateFailure_failureMessage = Lens.lens (\RuleUpdateFailure' {failureMessage} -> failureMessage) (\s@RuleUpdateFailure' {} a -> s {failureMessage = a} :: RuleUpdateFailure)

-- | The ID or Amazon Resource Name (ARN) of the rule.
ruleUpdateFailure_ruleIdentifier :: Lens.Lens' RuleUpdateFailure (Prelude.Maybe Prelude.Text)
ruleUpdateFailure_ruleIdentifier = Lens.lens (\RuleUpdateFailure' {ruleIdentifier} -> ruleIdentifier) (\s@RuleUpdateFailure' {} a -> s {ruleIdentifier = a} :: RuleUpdateFailure)

instance Data.FromJSON RuleUpdateFailure where
  parseJSON =
    Data.withObject
      "RuleUpdateFailure"
      ( \x ->
          RuleUpdateFailure'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "failureMessage")
            Prelude.<*> (x Data..:? "ruleIdentifier")
      )

instance Prelude.Hashable RuleUpdateFailure where
  hashWithSalt _salt RuleUpdateFailure' {..} =
    _salt
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` failureMessage
      `Prelude.hashWithSalt` ruleIdentifier

instance Prelude.NFData RuleUpdateFailure where
  rnf RuleUpdateFailure' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf ruleIdentifier

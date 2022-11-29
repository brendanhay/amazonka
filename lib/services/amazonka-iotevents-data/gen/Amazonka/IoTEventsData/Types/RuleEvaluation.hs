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
-- Module      : Amazonka.IoTEventsData.Types.RuleEvaluation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.RuleEvaluation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types.SimpleRuleEvaluation
import qualified Amazonka.Prelude as Prelude

-- | Information needed to evaluate data.
--
-- /See:/ 'newRuleEvaluation' smart constructor.
data RuleEvaluation = RuleEvaluation'
  { -- | Information needed to compare two values with a comparison operator.
    simpleRuleEvaluation :: Prelude.Maybe SimpleRuleEvaluation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simpleRuleEvaluation', 'ruleEvaluation_simpleRuleEvaluation' - Information needed to compare two values with a comparison operator.
newRuleEvaluation ::
  RuleEvaluation
newRuleEvaluation =
  RuleEvaluation'
    { simpleRuleEvaluation =
        Prelude.Nothing
    }

-- | Information needed to compare two values with a comparison operator.
ruleEvaluation_simpleRuleEvaluation :: Lens.Lens' RuleEvaluation (Prelude.Maybe SimpleRuleEvaluation)
ruleEvaluation_simpleRuleEvaluation = Lens.lens (\RuleEvaluation' {simpleRuleEvaluation} -> simpleRuleEvaluation) (\s@RuleEvaluation' {} a -> s {simpleRuleEvaluation = a} :: RuleEvaluation)

instance Core.FromJSON RuleEvaluation where
  parseJSON =
    Core.withObject
      "RuleEvaluation"
      ( \x ->
          RuleEvaluation'
            Prelude.<$> (x Core..:? "simpleRuleEvaluation")
      )

instance Prelude.Hashable RuleEvaluation where
  hashWithSalt _salt RuleEvaluation' {..} =
    _salt `Prelude.hashWithSalt` simpleRuleEvaluation

instance Prelude.NFData RuleEvaluation where
  rnf RuleEvaluation' {..} =
    Prelude.rnf simpleRuleEvaluation

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
-- Module      : Network.AWS.IoTEventsData.Types.RuleEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEventsData.Types.RuleEvaluation where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEventsData.Types.SimpleRuleEvaluation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable RuleEvaluation

instance Prelude.NFData RuleEvaluation

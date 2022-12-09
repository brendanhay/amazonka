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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSourcePorts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A port range to specify the source ports to inspect for.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts' smart constructor.
data RuleGroupSourceStatelessRuleMatchAttributesSourcePorts = RuleGroupSourceStatelessRuleMatchAttributesSourcePorts'
  { -- | The starting port value for the port range.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The ending port value for the port range.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort' - The starting port value for the port range.
--
-- 'toPort', 'ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort' - The ending port value for the port range.
newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts ::
  RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
newRuleGroupSourceStatelessRuleMatchAttributesSourcePorts =
  RuleGroupSourceStatelessRuleMatchAttributesSourcePorts'
    { fromPort =
        Prelude.Nothing,
      toPort =
        Prelude.Nothing
    }

-- | The starting port value for the port range.
ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesSourcePorts (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_fromPort = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {fromPort} -> fromPort) (\s@RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {} a -> s {fromPort = a} :: RuleGroupSourceStatelessRuleMatchAttributesSourcePorts)

-- | The ending port value for the port range.
ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesSourcePorts (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRuleMatchAttributesSourcePorts_toPort = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {toPort} -> toPort) (\s@RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {} a -> s {toPort = a} :: RuleGroupSourceStatelessRuleMatchAttributesSourcePorts)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRuleMatchAttributesSourcePorts"
      ( \x ->
          RuleGroupSourceStatelessRuleMatchAttributesSourcePorts'
            Prelude.<$> (x Data..:? "FromPort")
              Prelude.<*> (x Data..:? "ToPort")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {..} =
      _salt `Prelude.hashWithSalt` fromPort
        `Prelude.hashWithSalt` toPort

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
  where
  rnf
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {..} =
      Prelude.rnf fromPort
        `Prelude.seq` Prelude.rnf toPort

instance
  Data.ToJSON
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
  where
  toJSON
    RuleGroupSourceStatelessRuleMatchAttributesSourcePorts' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FromPort" Data..=) Prelude.<$> fromPort,
              ("ToPort" Data..=) Prelude.<$> toPort
            ]
        )

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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A port range to specify the destination ports to inspect for.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' smart constructor.
data RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts = RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts'
  { -- | The starting port value for the port range.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The ending port value for the port range.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort' - The starting port value for the port range.
--
-- 'toPort', 'ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort' - The ending port value for the port range.
newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts ::
  RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
newRuleGroupSourceStatelessRuleMatchAttributesDestinationPorts =
  RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts'
    { fromPort =
        Prelude.Nothing,
      toPort =
        Prelude.Nothing
    }

-- | The starting port value for the port range.
ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_fromPort = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {fromPort} -> fromPort) (\s@RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {} a -> s {fromPort = a} :: RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts)

-- | The ending port value for the port range.
ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts (Prelude.Maybe Prelude.Int)
ruleGroupSourceStatelessRuleMatchAttributesDestinationPorts_toPort = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {toPort} -> toPort) (\s@RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {} a -> s {toPort = a} :: RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts)

instance
  Data.FromJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
  where
  parseJSON =
    Data.withObject
      "RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts"
      ( \x ->
          RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts'
            Prelude.<$> (x Data..:? "FromPort")
              Prelude.<*> (x Data..:? "ToPort")
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {..} =
      _salt `Prelude.hashWithSalt` fromPort
        `Prelude.hashWithSalt` toPort

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
  where
  rnf
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {..} =
      Prelude.rnf fromPort
        `Prelude.seq` Prelude.rnf toPort

instance
  Data.ToJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
  where
  toJSON
    RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FromPort" Data..=) Prelude.<$> fromPort,
              ("ToPort" Data..=) Prelude.<$> toPort
            ]
        )

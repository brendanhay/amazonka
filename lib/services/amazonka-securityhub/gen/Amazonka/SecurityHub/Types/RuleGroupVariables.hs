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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupVariables
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupVariables where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupVariablesIpSetsDetails
import Amazonka.SecurityHub.Types.RuleGroupVariablesPortSetsDetails

-- | Additional settings to use in the specified rules.
--
-- /See:/ 'newRuleGroupVariables' smart constructor.
data RuleGroupVariables = RuleGroupVariables'
  { -- | A list of IP addresses and address ranges, in CIDR notation.
    ipSets :: Prelude.Maybe RuleGroupVariablesIpSetsDetails,
    -- | A list of port ranges.
    portSets :: Prelude.Maybe RuleGroupVariablesPortSetsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupVariables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipSets', 'ruleGroupVariables_ipSets' - A list of IP addresses and address ranges, in CIDR notation.
--
-- 'portSets', 'ruleGroupVariables_portSets' - A list of port ranges.
newRuleGroupVariables ::
  RuleGroupVariables
newRuleGroupVariables =
  RuleGroupVariables'
    { ipSets = Prelude.Nothing,
      portSets = Prelude.Nothing
    }

-- | A list of IP addresses and address ranges, in CIDR notation.
ruleGroupVariables_ipSets :: Lens.Lens' RuleGroupVariables (Prelude.Maybe RuleGroupVariablesIpSetsDetails)
ruleGroupVariables_ipSets = Lens.lens (\RuleGroupVariables' {ipSets} -> ipSets) (\s@RuleGroupVariables' {} a -> s {ipSets = a} :: RuleGroupVariables)

-- | A list of port ranges.
ruleGroupVariables_portSets :: Lens.Lens' RuleGroupVariables (Prelude.Maybe RuleGroupVariablesPortSetsDetails)
ruleGroupVariables_portSets = Lens.lens (\RuleGroupVariables' {portSets} -> portSets) (\s@RuleGroupVariables' {} a -> s {portSets = a} :: RuleGroupVariables)

instance Data.FromJSON RuleGroupVariables where
  parseJSON =
    Data.withObject
      "RuleGroupVariables"
      ( \x ->
          RuleGroupVariables'
            Prelude.<$> (x Data..:? "IpSets")
            Prelude.<*> (x Data..:? "PortSets")
      )

instance Prelude.Hashable RuleGroupVariables where
  hashWithSalt _salt RuleGroupVariables' {..} =
    _salt `Prelude.hashWithSalt` ipSets
      `Prelude.hashWithSalt` portSets

instance Prelude.NFData RuleGroupVariables where
  rnf RuleGroupVariables' {..} =
    Prelude.rnf ipSets
      `Prelude.seq` Prelude.rnf portSets

instance Data.ToJSON RuleGroupVariables where
  toJSON RuleGroupVariables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IpSets" Data..=) Prelude.<$> ipSets,
            ("PortSets" Data..=) Prelude.<$> portSets
          ]
      )

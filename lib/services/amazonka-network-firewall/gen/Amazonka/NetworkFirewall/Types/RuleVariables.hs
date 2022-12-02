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
-- Module      : Amazonka.NetworkFirewall.Types.RuleVariables
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleVariables where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.IPSet
import Amazonka.NetworkFirewall.Types.PortSet
import qualified Amazonka.Prelude as Prelude

-- | Settings that are available for use in the rules in the RuleGroup where
-- this is defined.
--
-- /See:/ 'newRuleVariables' smart constructor.
data RuleVariables = RuleVariables'
  { -- | A list of IP addresses and address ranges, in CIDR notation.
    iPSets :: Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet),
    -- | A list of port ranges.
    portSets :: Prelude.Maybe (Prelude.HashMap Prelude.Text PortSet)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleVariables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSets', 'ruleVariables_iPSets' - A list of IP addresses and address ranges, in CIDR notation.
--
-- 'portSets', 'ruleVariables_portSets' - A list of port ranges.
newRuleVariables ::
  RuleVariables
newRuleVariables =
  RuleVariables'
    { iPSets = Prelude.Nothing,
      portSets = Prelude.Nothing
    }

-- | A list of IP addresses and address ranges, in CIDR notation.
ruleVariables_iPSets :: Lens.Lens' RuleVariables (Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet))
ruleVariables_iPSets = Lens.lens (\RuleVariables' {iPSets} -> iPSets) (\s@RuleVariables' {} a -> s {iPSets = a} :: RuleVariables) Prelude.. Lens.mapping Lens.coerced

-- | A list of port ranges.
ruleVariables_portSets :: Lens.Lens' RuleVariables (Prelude.Maybe (Prelude.HashMap Prelude.Text PortSet))
ruleVariables_portSets = Lens.lens (\RuleVariables' {portSets} -> portSets) (\s@RuleVariables' {} a -> s {portSets = a} :: RuleVariables) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RuleVariables where
  parseJSON =
    Data.withObject
      "RuleVariables"
      ( \x ->
          RuleVariables'
            Prelude.<$> (x Data..:? "IPSets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PortSets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RuleVariables where
  hashWithSalt _salt RuleVariables' {..} =
    _salt `Prelude.hashWithSalt` iPSets
      `Prelude.hashWithSalt` portSets

instance Prelude.NFData RuleVariables where
  rnf RuleVariables' {..} =
    Prelude.rnf iPSets
      `Prelude.seq` Prelude.rnf portSets

instance Data.ToJSON RuleVariables where
  toJSON RuleVariables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IPSets" Data..=) Prelude.<$> iPSets,
            ("PortSets" Data..=) Prelude.<$> portSets
          ]
      )

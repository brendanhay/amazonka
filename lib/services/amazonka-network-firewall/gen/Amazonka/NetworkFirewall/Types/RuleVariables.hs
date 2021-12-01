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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleVariables where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.IPSet
import Amazonka.NetworkFirewall.Types.PortSet
import qualified Amazonka.Prelude as Prelude

-- | Settings that are available for use in the rules in the RuleGroup where
-- this is defined.
--
-- /See:/ 'newRuleVariables' smart constructor.
data RuleVariables = RuleVariables'
  { -- | A list of port ranges.
    portSets :: Prelude.Maybe (Prelude.HashMap Prelude.Text PortSet),
    -- | A list of IP addresses and address ranges, in CIDR notation.
    iPSets :: Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet)
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
-- 'portSets', 'ruleVariables_portSets' - A list of port ranges.
--
-- 'iPSets', 'ruleVariables_iPSets' - A list of IP addresses and address ranges, in CIDR notation.
newRuleVariables ::
  RuleVariables
newRuleVariables =
  RuleVariables'
    { portSets = Prelude.Nothing,
      iPSets = Prelude.Nothing
    }

-- | A list of port ranges.
ruleVariables_portSets :: Lens.Lens' RuleVariables (Prelude.Maybe (Prelude.HashMap Prelude.Text PortSet))
ruleVariables_portSets = Lens.lens (\RuleVariables' {portSets} -> portSets) (\s@RuleVariables' {} a -> s {portSets = a} :: RuleVariables) Prelude.. Lens.mapping Lens.coerced

-- | A list of IP addresses and address ranges, in CIDR notation.
ruleVariables_iPSets :: Lens.Lens' RuleVariables (Prelude.Maybe (Prelude.HashMap Prelude.Text IPSet))
ruleVariables_iPSets = Lens.lens (\RuleVariables' {iPSets} -> iPSets) (\s@RuleVariables' {} a -> s {iPSets = a} :: RuleVariables) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON RuleVariables where
  parseJSON =
    Core.withObject
      "RuleVariables"
      ( \x ->
          RuleVariables'
            Prelude.<$> (x Core..:? "PortSets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IPSets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RuleVariables where
  hashWithSalt salt' RuleVariables' {..} =
    salt' `Prelude.hashWithSalt` iPSets
      `Prelude.hashWithSalt` portSets

instance Prelude.NFData RuleVariables where
  rnf RuleVariables' {..} =
    Prelude.rnf portSets
      `Prelude.seq` Prelude.rnf iPSets

instance Core.ToJSON RuleVariables where
  toJSON RuleVariables' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortSets" Core..=) Prelude.<$> portSets,
            ("IPSets" Core..=) Prelude.<$> iPSets
          ]
      )

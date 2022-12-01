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
-- Module      : Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesDestinations
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSourcePorts
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesSources
import Amazonka.SecurityHub.Types.RuleGroupSourceStatelessRuleMatchAttributesTcpFlags

-- | Criteria for the stateless rule.
--
-- /See:/ 'newRuleGroupSourceStatelessRuleMatchAttributes' smart constructor.
data RuleGroupSourceStatelessRuleMatchAttributes = RuleGroupSourceStatelessRuleMatchAttributes'
  { -- | A list of port ranges to specify the destination ports to inspect for.
    destinationPorts :: Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts],
    -- | The source IP addresses and address ranges to inspect for, in CIDR
    -- notation.
    sources :: Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesSources],
    -- | The TCP flags and masks to inspect for.
    tcpFlags :: Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesTcpFlags],
    -- | The protocols to inspect for.
    protocols :: Prelude.Maybe [Prelude.Int],
    -- | A list of port ranges to specify the source ports to inspect for.
    sourcePorts :: Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesSourcePorts],
    -- | The destination IP addresses and address ranges to inspect for, in CIDR
    -- notation.
    destinations :: Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesDestinations]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSourceStatelessRuleMatchAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts' - A list of port ranges to specify the destination ports to inspect for.
--
-- 'sources', 'ruleGroupSourceStatelessRuleMatchAttributes_sources' - The source IP addresses and address ranges to inspect for, in CIDR
-- notation.
--
-- 'tcpFlags', 'ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags' - The TCP flags and masks to inspect for.
--
-- 'protocols', 'ruleGroupSourceStatelessRuleMatchAttributes_protocols' - The protocols to inspect for.
--
-- 'sourcePorts', 'ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts' - A list of port ranges to specify the source ports to inspect for.
--
-- 'destinations', 'ruleGroupSourceStatelessRuleMatchAttributes_destinations' - The destination IP addresses and address ranges to inspect for, in CIDR
-- notation.
newRuleGroupSourceStatelessRuleMatchAttributes ::
  RuleGroupSourceStatelessRuleMatchAttributes
newRuleGroupSourceStatelessRuleMatchAttributes =
  RuleGroupSourceStatelessRuleMatchAttributes'
    { destinationPorts =
        Prelude.Nothing,
      sources = Prelude.Nothing,
      tcpFlags = Prelude.Nothing,
      protocols = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      destinations = Prelude.Nothing
    }

-- | A list of port ranges to specify the destination ports to inspect for.
ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesDestinationPorts])
ruleGroupSourceStatelessRuleMatchAttributes_destinationPorts = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {destinationPorts} -> destinationPorts) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {destinationPorts = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The source IP addresses and address ranges to inspect for, in CIDR
-- notation.
ruleGroupSourceStatelessRuleMatchAttributes_sources :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesSources])
ruleGroupSourceStatelessRuleMatchAttributes_sources = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {sources} -> sources) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {sources = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The TCP flags and masks to inspect for.
ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesTcpFlags])
ruleGroupSourceStatelessRuleMatchAttributes_tcpFlags = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {tcpFlags} -> tcpFlags) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {tcpFlags = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The protocols to inspect for.
ruleGroupSourceStatelessRuleMatchAttributes_protocols :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [Prelude.Int])
ruleGroupSourceStatelessRuleMatchAttributes_protocols = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {protocols} -> protocols) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {protocols = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A list of port ranges to specify the source ports to inspect for.
ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesSourcePorts])
ruleGroupSourceStatelessRuleMatchAttributes_sourcePorts = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {sourcePorts} -> sourcePorts) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {sourcePorts = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The destination IP addresses and address ranges to inspect for, in CIDR
-- notation.
ruleGroupSourceStatelessRuleMatchAttributes_destinations :: Lens.Lens' RuleGroupSourceStatelessRuleMatchAttributes (Prelude.Maybe [RuleGroupSourceStatelessRuleMatchAttributesDestinations])
ruleGroupSourceStatelessRuleMatchAttributes_destinations = Lens.lens (\RuleGroupSourceStatelessRuleMatchAttributes' {destinations} -> destinations) (\s@RuleGroupSourceStatelessRuleMatchAttributes' {} a -> s {destinations = a} :: RuleGroupSourceStatelessRuleMatchAttributes) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    RuleGroupSourceStatelessRuleMatchAttributes
  where
  parseJSON =
    Core.withObject
      "RuleGroupSourceStatelessRuleMatchAttributes"
      ( \x ->
          RuleGroupSourceStatelessRuleMatchAttributes'
            Prelude.<$> ( x Core..:? "DestinationPorts"
                            Core..!= Prelude.mempty
                        )
              Prelude.<*> (x Core..:? "Sources" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "TcpFlags" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Protocols" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "SourcePorts" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Destinations" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    RuleGroupSourceStatelessRuleMatchAttributes
  where
  hashWithSalt
    _salt
    RuleGroupSourceStatelessRuleMatchAttributes' {..} =
      _salt `Prelude.hashWithSalt` destinationPorts
        `Prelude.hashWithSalt` sources
        `Prelude.hashWithSalt` tcpFlags
        `Prelude.hashWithSalt` protocols
        `Prelude.hashWithSalt` sourcePorts
        `Prelude.hashWithSalt` destinations

instance
  Prelude.NFData
    RuleGroupSourceStatelessRuleMatchAttributes
  where
  rnf RuleGroupSourceStatelessRuleMatchAttributes' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tcpFlags
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf destinations

instance
  Core.ToJSON
    RuleGroupSourceStatelessRuleMatchAttributes
  where
  toJSON
    RuleGroupSourceStatelessRuleMatchAttributes' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("DestinationPorts" Core..=)
                Prelude.<$> destinationPorts,
              ("Sources" Core..=) Prelude.<$> sources,
              ("TcpFlags" Core..=) Prelude.<$> tcpFlags,
              ("Protocols" Core..=) Prelude.<$> protocols,
              ("SourcePorts" Core..=) Prelude.<$> sourcePorts,
              ("Destinations" Core..=) Prelude.<$> destinations
            ]
        )

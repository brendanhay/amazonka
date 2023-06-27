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
-- Module      : Amazonka.EC2.Types.FirewallStatefulRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FirewallStatefulRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a stateful rule.
--
-- /See:/ 'newFirewallStatefulRule' smart constructor.
data FirewallStatefulRule = FirewallStatefulRule'
  { -- | The destination ports.
    destinationPorts :: Prelude.Maybe [PortRange],
    -- | The destination IP addresses, in CIDR notation.
    destinations :: Prelude.Maybe [Prelude.Text],
    -- | The direction. The possible values are @FORWARD@ and @ANY@.
    direction :: Prelude.Maybe Prelude.Text,
    -- | The protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The rule action. The possible values are @pass@, @drop@, and @alert@.
    ruleAction :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stateful rule group.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The source ports.
    sourcePorts :: Prelude.Maybe [PortRange],
    -- | The source IP addresses, in CIDR notation.
    sources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallStatefulRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'firewallStatefulRule_destinationPorts' - The destination ports.
--
-- 'destinations', 'firewallStatefulRule_destinations' - The destination IP addresses, in CIDR notation.
--
-- 'direction', 'firewallStatefulRule_direction' - The direction. The possible values are @FORWARD@ and @ANY@.
--
-- 'protocol', 'firewallStatefulRule_protocol' - The protocol.
--
-- 'ruleAction', 'firewallStatefulRule_ruleAction' - The rule action. The possible values are @pass@, @drop@, and @alert@.
--
-- 'ruleGroupArn', 'firewallStatefulRule_ruleGroupArn' - The ARN of the stateful rule group.
--
-- 'sourcePorts', 'firewallStatefulRule_sourcePorts' - The source ports.
--
-- 'sources', 'firewallStatefulRule_sources' - The source IP addresses, in CIDR notation.
newFirewallStatefulRule ::
  FirewallStatefulRule
newFirewallStatefulRule =
  FirewallStatefulRule'
    { destinationPorts =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      direction = Prelude.Nothing,
      protocol = Prelude.Nothing,
      ruleAction = Prelude.Nothing,
      ruleGroupArn = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | The destination ports.
firewallStatefulRule_destinationPorts :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe [PortRange])
firewallStatefulRule_destinationPorts = Lens.lens (\FirewallStatefulRule' {destinationPorts} -> destinationPorts) (\s@FirewallStatefulRule' {} a -> s {destinationPorts = a} :: FirewallStatefulRule) Prelude.. Lens.mapping Lens.coerced

-- | The destination IP addresses, in CIDR notation.
firewallStatefulRule_destinations :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe [Prelude.Text])
firewallStatefulRule_destinations = Lens.lens (\FirewallStatefulRule' {destinations} -> destinations) (\s@FirewallStatefulRule' {} a -> s {destinations = a} :: FirewallStatefulRule) Prelude.. Lens.mapping Lens.coerced

-- | The direction. The possible values are @FORWARD@ and @ANY@.
firewallStatefulRule_direction :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe Prelude.Text)
firewallStatefulRule_direction = Lens.lens (\FirewallStatefulRule' {direction} -> direction) (\s@FirewallStatefulRule' {} a -> s {direction = a} :: FirewallStatefulRule)

-- | The protocol.
firewallStatefulRule_protocol :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe Prelude.Text)
firewallStatefulRule_protocol = Lens.lens (\FirewallStatefulRule' {protocol} -> protocol) (\s@FirewallStatefulRule' {} a -> s {protocol = a} :: FirewallStatefulRule)

-- | The rule action. The possible values are @pass@, @drop@, and @alert@.
firewallStatefulRule_ruleAction :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe Prelude.Text)
firewallStatefulRule_ruleAction = Lens.lens (\FirewallStatefulRule' {ruleAction} -> ruleAction) (\s@FirewallStatefulRule' {} a -> s {ruleAction = a} :: FirewallStatefulRule)

-- | The ARN of the stateful rule group.
firewallStatefulRule_ruleGroupArn :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe Prelude.Text)
firewallStatefulRule_ruleGroupArn = Lens.lens (\FirewallStatefulRule' {ruleGroupArn} -> ruleGroupArn) (\s@FirewallStatefulRule' {} a -> s {ruleGroupArn = a} :: FirewallStatefulRule)

-- | The source ports.
firewallStatefulRule_sourcePorts :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe [PortRange])
firewallStatefulRule_sourcePorts = Lens.lens (\FirewallStatefulRule' {sourcePorts} -> sourcePorts) (\s@FirewallStatefulRule' {} a -> s {sourcePorts = a} :: FirewallStatefulRule) Prelude.. Lens.mapping Lens.coerced

-- | The source IP addresses, in CIDR notation.
firewallStatefulRule_sources :: Lens.Lens' FirewallStatefulRule (Prelude.Maybe [Prelude.Text])
firewallStatefulRule_sources = Lens.lens (\FirewallStatefulRule' {sources} -> sources) (\s@FirewallStatefulRule' {} a -> s {sources = a} :: FirewallStatefulRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML FirewallStatefulRule where
  parseXML x =
    FirewallStatefulRule'
      Prelude.<$> ( x
                      Data..@? "destinationPortSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "destinationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "direction")
      Prelude.<*> (x Data..@? "protocol")
      Prelude.<*> (x Data..@? "ruleAction")
      Prelude.<*> (x Data..@? "ruleGroupArn")
      Prelude.<*> ( x
                      Data..@? "sourcePortSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "sourceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable FirewallStatefulRule where
  hashWithSalt _salt FirewallStatefulRule' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ruleAction
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sources

instance Prelude.NFData FirewallStatefulRule where
  rnf FirewallStatefulRule' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ruleAction
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sources

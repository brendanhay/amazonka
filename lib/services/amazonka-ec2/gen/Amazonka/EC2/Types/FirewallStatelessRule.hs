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
-- Module      : Amazonka.EC2.Types.FirewallStatelessRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FirewallStatelessRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a stateless rule.
--
-- /See:/ 'newFirewallStatelessRule' smart constructor.
data FirewallStatelessRule = FirewallStatelessRule'
  { -- | The destination ports.
    destinationPorts :: Prelude.Maybe [PortRange],
    -- | The destination IP addresses, in CIDR notation.
    destinations :: Prelude.Maybe [Prelude.Text],
    -- | The rule priority.
    priority :: Prelude.Maybe Prelude.Int,
    -- | The protocols.
    protocols :: Prelude.Maybe [Prelude.Natural],
    -- | The rule action. The possible values are @pass@, @drop@, and
    -- @forward_to_site@.
    ruleAction :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stateless rule group.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The source ports.
    sourcePorts :: Prelude.Maybe [PortRange],
    -- | The source IP addresses, in CIDR notation.
    sources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallStatelessRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPorts', 'firewallStatelessRule_destinationPorts' - The destination ports.
--
-- 'destinations', 'firewallStatelessRule_destinations' - The destination IP addresses, in CIDR notation.
--
-- 'priority', 'firewallStatelessRule_priority' - The rule priority.
--
-- 'protocols', 'firewallStatelessRule_protocols' - The protocols.
--
-- 'ruleAction', 'firewallStatelessRule_ruleAction' - The rule action. The possible values are @pass@, @drop@, and
-- @forward_to_site@.
--
-- 'ruleGroupArn', 'firewallStatelessRule_ruleGroupArn' - The ARN of the stateless rule group.
--
-- 'sourcePorts', 'firewallStatelessRule_sourcePorts' - The source ports.
--
-- 'sources', 'firewallStatelessRule_sources' - The source IP addresses, in CIDR notation.
newFirewallStatelessRule ::
  FirewallStatelessRule
newFirewallStatelessRule =
  FirewallStatelessRule'
    { destinationPorts =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      priority = Prelude.Nothing,
      protocols = Prelude.Nothing,
      ruleAction = Prelude.Nothing,
      ruleGroupArn = Prelude.Nothing,
      sourcePorts = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | The destination ports.
firewallStatelessRule_destinationPorts :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe [PortRange])
firewallStatelessRule_destinationPorts = Lens.lens (\FirewallStatelessRule' {destinationPorts} -> destinationPorts) (\s@FirewallStatelessRule' {} a -> s {destinationPorts = a} :: FirewallStatelessRule) Prelude.. Lens.mapping Lens.coerced

-- | The destination IP addresses, in CIDR notation.
firewallStatelessRule_destinations :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe [Prelude.Text])
firewallStatelessRule_destinations = Lens.lens (\FirewallStatelessRule' {destinations} -> destinations) (\s@FirewallStatelessRule' {} a -> s {destinations = a} :: FirewallStatelessRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule priority.
firewallStatelessRule_priority :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe Prelude.Int)
firewallStatelessRule_priority = Lens.lens (\FirewallStatelessRule' {priority} -> priority) (\s@FirewallStatelessRule' {} a -> s {priority = a} :: FirewallStatelessRule)

-- | The protocols.
firewallStatelessRule_protocols :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe [Prelude.Natural])
firewallStatelessRule_protocols = Lens.lens (\FirewallStatelessRule' {protocols} -> protocols) (\s@FirewallStatelessRule' {} a -> s {protocols = a} :: FirewallStatelessRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule action. The possible values are @pass@, @drop@, and
-- @forward_to_site@.
firewallStatelessRule_ruleAction :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe Prelude.Text)
firewallStatelessRule_ruleAction = Lens.lens (\FirewallStatelessRule' {ruleAction} -> ruleAction) (\s@FirewallStatelessRule' {} a -> s {ruleAction = a} :: FirewallStatelessRule)

-- | The ARN of the stateless rule group.
firewallStatelessRule_ruleGroupArn :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe Prelude.Text)
firewallStatelessRule_ruleGroupArn = Lens.lens (\FirewallStatelessRule' {ruleGroupArn} -> ruleGroupArn) (\s@FirewallStatelessRule' {} a -> s {ruleGroupArn = a} :: FirewallStatelessRule)

-- | The source ports.
firewallStatelessRule_sourcePorts :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe [PortRange])
firewallStatelessRule_sourcePorts = Lens.lens (\FirewallStatelessRule' {sourcePorts} -> sourcePorts) (\s@FirewallStatelessRule' {} a -> s {sourcePorts = a} :: FirewallStatelessRule) Prelude.. Lens.mapping Lens.coerced

-- | The source IP addresses, in CIDR notation.
firewallStatelessRule_sources :: Lens.Lens' FirewallStatelessRule (Prelude.Maybe [Prelude.Text])
firewallStatelessRule_sources = Lens.lens (\FirewallStatelessRule' {sources} -> sources) (\s@FirewallStatelessRule' {} a -> s {sources = a} :: FirewallStatelessRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML FirewallStatelessRule where
  parseXML x =
    FirewallStatelessRule'
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
      Prelude.<*> (x Data..@? "priority")
      Prelude.<*> ( x
                      Data..@? "protocolSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
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

instance Prelude.Hashable FirewallStatelessRule where
  hashWithSalt _salt FirewallStatelessRule' {..} =
    _salt
      `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` ruleAction
      `Prelude.hashWithSalt` ruleGroupArn
      `Prelude.hashWithSalt` sourcePorts
      `Prelude.hashWithSalt` sources

instance Prelude.NFData FirewallStatelessRule where
  rnf FirewallStatelessRule' {..} =
    Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf ruleAction
      `Prelude.seq` Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf sourcePorts
      `Prelude.seq` Prelude.rnf sources

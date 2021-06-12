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
-- Module      : Network.AWS.EC2.Types.AnalysisAclRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AnalysisAclRule where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PortRange
import qualified Network.AWS.Lens as Lens

-- | Describes a network access control (ACL) rule.
--
-- /See:/ 'newAnalysisAclRule' smart constructor.
data AnalysisAclRule = AnalysisAclRule'
  { -- | The range of ports.
    portRange :: Core.Maybe PortRange,
    -- | Indicates whether to allow or deny traffic that matches the rule.
    ruleAction :: Core.Maybe Core.Text,
    -- | Indicates whether the rule is an outbound rule.
    egress :: Core.Maybe Core.Bool,
    -- | The IPv4 address range, in CIDR notation.
    cidr :: Core.Maybe Core.Text,
    -- | The protocol.
    protocol :: Core.Maybe Core.Text,
    -- | The rule number.
    ruleNumber :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalysisAclRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portRange', 'analysisAclRule_portRange' - The range of ports.
--
-- 'ruleAction', 'analysisAclRule_ruleAction' - Indicates whether to allow or deny traffic that matches the rule.
--
-- 'egress', 'analysisAclRule_egress' - Indicates whether the rule is an outbound rule.
--
-- 'cidr', 'analysisAclRule_cidr' - The IPv4 address range, in CIDR notation.
--
-- 'protocol', 'analysisAclRule_protocol' - The protocol.
--
-- 'ruleNumber', 'analysisAclRule_ruleNumber' - The rule number.
newAnalysisAclRule ::
  AnalysisAclRule
newAnalysisAclRule =
  AnalysisAclRule'
    { portRange = Core.Nothing,
      ruleAction = Core.Nothing,
      egress = Core.Nothing,
      cidr = Core.Nothing,
      protocol = Core.Nothing,
      ruleNumber = Core.Nothing
    }

-- | The range of ports.
analysisAclRule_portRange :: Lens.Lens' AnalysisAclRule (Core.Maybe PortRange)
analysisAclRule_portRange = Lens.lens (\AnalysisAclRule' {portRange} -> portRange) (\s@AnalysisAclRule' {} a -> s {portRange = a} :: AnalysisAclRule)

-- | Indicates whether to allow or deny traffic that matches the rule.
analysisAclRule_ruleAction :: Lens.Lens' AnalysisAclRule (Core.Maybe Core.Text)
analysisAclRule_ruleAction = Lens.lens (\AnalysisAclRule' {ruleAction} -> ruleAction) (\s@AnalysisAclRule' {} a -> s {ruleAction = a} :: AnalysisAclRule)

-- | Indicates whether the rule is an outbound rule.
analysisAclRule_egress :: Lens.Lens' AnalysisAclRule (Core.Maybe Core.Bool)
analysisAclRule_egress = Lens.lens (\AnalysisAclRule' {egress} -> egress) (\s@AnalysisAclRule' {} a -> s {egress = a} :: AnalysisAclRule)

-- | The IPv4 address range, in CIDR notation.
analysisAclRule_cidr :: Lens.Lens' AnalysisAclRule (Core.Maybe Core.Text)
analysisAclRule_cidr = Lens.lens (\AnalysisAclRule' {cidr} -> cidr) (\s@AnalysisAclRule' {} a -> s {cidr = a} :: AnalysisAclRule)

-- | The protocol.
analysisAclRule_protocol :: Lens.Lens' AnalysisAclRule (Core.Maybe Core.Text)
analysisAclRule_protocol = Lens.lens (\AnalysisAclRule' {protocol} -> protocol) (\s@AnalysisAclRule' {} a -> s {protocol = a} :: AnalysisAclRule)

-- | The rule number.
analysisAclRule_ruleNumber :: Lens.Lens' AnalysisAclRule (Core.Maybe Core.Int)
analysisAclRule_ruleNumber = Lens.lens (\AnalysisAclRule' {ruleNumber} -> ruleNumber) (\s@AnalysisAclRule' {} a -> s {ruleNumber = a} :: AnalysisAclRule)

instance Core.FromXML AnalysisAclRule where
  parseXML x =
    AnalysisAclRule'
      Core.<$> (x Core..@? "portRange")
      Core.<*> (x Core..@? "ruleAction")
      Core.<*> (x Core..@? "egress")
      Core.<*> (x Core..@? "cidr")
      Core.<*> (x Core..@? "protocol")
      Core.<*> (x Core..@? "ruleNumber")

instance Core.Hashable AnalysisAclRule

instance Core.NFData AnalysisAclRule

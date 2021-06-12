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
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilterRule where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TrafficDirection
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorRuleAction
import qualified Network.AWS.Lens as Lens

-- | Describes the Traffic Mirror rule.
--
-- /See:/ 'newTrafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Core.Maybe Core.Text,
    -- | The source port range assigned to the Traffic Mirror rule.
    sourcePortRange :: Core.Maybe TrafficMirrorPortRange,
    -- | The traffic direction assigned to the Traffic Mirror rule.
    trafficDirection :: Core.Maybe TrafficDirection,
    -- | The action assigned to the Traffic Mirror rule.
    ruleAction :: Core.Maybe TrafficMirrorRuleAction,
    -- | The source CIDR block assigned to the Traffic Mirror rule.
    sourceCidrBlock :: Core.Maybe Core.Text,
    -- | The ID of the Traffic Mirror filter that the rule is associated with.
    trafficMirrorFilterId :: Core.Maybe Core.Text,
    -- | The destination CIDR block assigned to the Traffic Mirror rule.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The protocol assigned to the Traffic Mirror rule.
    protocol :: Core.Maybe Core.Int,
    -- | The description of the Traffic Mirror rule.
    description :: Core.Maybe Core.Text,
    -- | The rule number of the Traffic Mirror rule.
    ruleNumber :: Core.Maybe Core.Int,
    -- | The destination port range assigned to the Traffic Mirror rule.
    destinationPortRange :: Core.Maybe TrafficMirrorPortRange
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrafficMirrorFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilterRuleId', 'trafficMirrorFilterRule_trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
--
-- 'sourcePortRange', 'trafficMirrorFilterRule_sourcePortRange' - The source port range assigned to the Traffic Mirror rule.
--
-- 'trafficDirection', 'trafficMirrorFilterRule_trafficDirection' - The traffic direction assigned to the Traffic Mirror rule.
--
-- 'ruleAction', 'trafficMirrorFilterRule_ruleAction' - The action assigned to the Traffic Mirror rule.
--
-- 'sourceCidrBlock', 'trafficMirrorFilterRule_sourceCidrBlock' - The source CIDR block assigned to the Traffic Mirror rule.
--
-- 'trafficMirrorFilterId', 'trafficMirrorFilterRule_trafficMirrorFilterId' - The ID of the Traffic Mirror filter that the rule is associated with.
--
-- 'destinationCidrBlock', 'trafficMirrorFilterRule_destinationCidrBlock' - The destination CIDR block assigned to the Traffic Mirror rule.
--
-- 'protocol', 'trafficMirrorFilterRule_protocol' - The protocol assigned to the Traffic Mirror rule.
--
-- 'description', 'trafficMirrorFilterRule_description' - The description of the Traffic Mirror rule.
--
-- 'ruleNumber', 'trafficMirrorFilterRule_ruleNumber' - The rule number of the Traffic Mirror rule.
--
-- 'destinationPortRange', 'trafficMirrorFilterRule_destinationPortRange' - The destination port range assigned to the Traffic Mirror rule.
newTrafficMirrorFilterRule ::
  TrafficMirrorFilterRule
newTrafficMirrorFilterRule =
  TrafficMirrorFilterRule'
    { trafficMirrorFilterRuleId =
        Core.Nothing,
      sourcePortRange = Core.Nothing,
      trafficDirection = Core.Nothing,
      ruleAction = Core.Nothing,
      sourceCidrBlock = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      protocol = Core.Nothing,
      description = Core.Nothing,
      ruleNumber = Core.Nothing,
      destinationPortRange = Core.Nothing
    }

-- | The ID of the Traffic Mirror rule.
trafficMirrorFilterRule_trafficMirrorFilterRuleId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
trafficMirrorFilterRule_trafficMirrorFilterRuleId = Lens.lens (\TrafficMirrorFilterRule' {trafficMirrorFilterRuleId} -> trafficMirrorFilterRuleId) (\s@TrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterRuleId = a} :: TrafficMirrorFilterRule)

-- | The source port range assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_sourcePortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe TrafficMirrorPortRange)
trafficMirrorFilterRule_sourcePortRange = Lens.lens (\TrafficMirrorFilterRule' {sourcePortRange} -> sourcePortRange) (\s@TrafficMirrorFilterRule' {} a -> s {sourcePortRange = a} :: TrafficMirrorFilterRule)

-- | The traffic direction assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_trafficDirection :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe TrafficDirection)
trafficMirrorFilterRule_trafficDirection = Lens.lens (\TrafficMirrorFilterRule' {trafficDirection} -> trafficDirection) (\s@TrafficMirrorFilterRule' {} a -> s {trafficDirection = a} :: TrafficMirrorFilterRule)

-- | The action assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_ruleAction :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe TrafficMirrorRuleAction)
trafficMirrorFilterRule_ruleAction = Lens.lens (\TrafficMirrorFilterRule' {ruleAction} -> ruleAction) (\s@TrafficMirrorFilterRule' {} a -> s {ruleAction = a} :: TrafficMirrorFilterRule)

-- | The source CIDR block assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_sourceCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
trafficMirrorFilterRule_sourceCidrBlock = Lens.lens (\TrafficMirrorFilterRule' {sourceCidrBlock} -> sourceCidrBlock) (\s@TrafficMirrorFilterRule' {} a -> s {sourceCidrBlock = a} :: TrafficMirrorFilterRule)

-- | The ID of the Traffic Mirror filter that the rule is associated with.
trafficMirrorFilterRule_trafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
trafficMirrorFilterRule_trafficMirrorFilterId = Lens.lens (\TrafficMirrorFilterRule' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@TrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilterRule)

-- | The destination CIDR block assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_destinationCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
trafficMirrorFilterRule_destinationCidrBlock = Lens.lens (\TrafficMirrorFilterRule' {destinationCidrBlock} -> destinationCidrBlock) (\s@TrafficMirrorFilterRule' {} a -> s {destinationCidrBlock = a} :: TrafficMirrorFilterRule)

-- | The protocol assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_protocol :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
trafficMirrorFilterRule_protocol = Lens.lens (\TrafficMirrorFilterRule' {protocol} -> protocol) (\s@TrafficMirrorFilterRule' {} a -> s {protocol = a} :: TrafficMirrorFilterRule)

-- | The description of the Traffic Mirror rule.
trafficMirrorFilterRule_description :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Text)
trafficMirrorFilterRule_description = Lens.lens (\TrafficMirrorFilterRule' {description} -> description) (\s@TrafficMirrorFilterRule' {} a -> s {description = a} :: TrafficMirrorFilterRule)

-- | The rule number of the Traffic Mirror rule.
trafficMirrorFilterRule_ruleNumber :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe Core.Int)
trafficMirrorFilterRule_ruleNumber = Lens.lens (\TrafficMirrorFilterRule' {ruleNumber} -> ruleNumber) (\s@TrafficMirrorFilterRule' {} a -> s {ruleNumber = a} :: TrafficMirrorFilterRule)

-- | The destination port range assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_destinationPortRange :: Lens.Lens' TrafficMirrorFilterRule (Core.Maybe TrafficMirrorPortRange)
trafficMirrorFilterRule_destinationPortRange = Lens.lens (\TrafficMirrorFilterRule' {destinationPortRange} -> destinationPortRange) (\s@TrafficMirrorFilterRule' {} a -> s {destinationPortRange = a} :: TrafficMirrorFilterRule)

instance Core.FromXML TrafficMirrorFilterRule where
  parseXML x =
    TrafficMirrorFilterRule'
      Core.<$> (x Core..@? "trafficMirrorFilterRuleId")
      Core.<*> (x Core..@? "sourcePortRange")
      Core.<*> (x Core..@? "trafficDirection")
      Core.<*> (x Core..@? "ruleAction")
      Core.<*> (x Core..@? "sourceCidrBlock")
      Core.<*> (x Core..@? "trafficMirrorFilterId")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "protocol")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "ruleNumber")
      Core.<*> (x Core..@? "destinationPortRange")

instance Core.Hashable TrafficMirrorFilterRule

instance Core.NFData TrafficMirrorFilterRule

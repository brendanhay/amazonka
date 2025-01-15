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
-- Module      : Amazonka.EC2.Types.TrafficMirrorFilterRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorFilterRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TrafficDirection
import Amazonka.EC2.Types.TrafficMirrorPortRange
import Amazonka.EC2.Types.TrafficMirrorRuleAction
import qualified Amazonka.Prelude as Prelude

-- | Describes the Traffic Mirror rule.
--
-- /See:/ 'newTrafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { -- | The description of the Traffic Mirror rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The destination CIDR block assigned to the Traffic Mirror rule.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The destination port range assigned to the Traffic Mirror rule.
    destinationPortRange :: Prelude.Maybe TrafficMirrorPortRange,
    -- | The protocol assigned to the Traffic Mirror rule.
    protocol :: Prelude.Maybe Prelude.Int,
    -- | The action assigned to the Traffic Mirror rule.
    ruleAction :: Prelude.Maybe TrafficMirrorRuleAction,
    -- | The rule number of the Traffic Mirror rule.
    ruleNumber :: Prelude.Maybe Prelude.Int,
    -- | The source CIDR block assigned to the Traffic Mirror rule.
    sourceCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The source port range assigned to the Traffic Mirror rule.
    sourcePortRange :: Prelude.Maybe TrafficMirrorPortRange,
    -- | The traffic direction assigned to the Traffic Mirror rule.
    trafficDirection :: Prelude.Maybe TrafficDirection,
    -- | The ID of the Traffic Mirror filter that the rule is associated with.
    trafficMirrorFilterId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficMirrorFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'trafficMirrorFilterRule_description' - The description of the Traffic Mirror rule.
--
-- 'destinationCidrBlock', 'trafficMirrorFilterRule_destinationCidrBlock' - The destination CIDR block assigned to the Traffic Mirror rule.
--
-- 'destinationPortRange', 'trafficMirrorFilterRule_destinationPortRange' - The destination port range assigned to the Traffic Mirror rule.
--
-- 'protocol', 'trafficMirrorFilterRule_protocol' - The protocol assigned to the Traffic Mirror rule.
--
-- 'ruleAction', 'trafficMirrorFilterRule_ruleAction' - The action assigned to the Traffic Mirror rule.
--
-- 'ruleNumber', 'trafficMirrorFilterRule_ruleNumber' - The rule number of the Traffic Mirror rule.
--
-- 'sourceCidrBlock', 'trafficMirrorFilterRule_sourceCidrBlock' - The source CIDR block assigned to the Traffic Mirror rule.
--
-- 'sourcePortRange', 'trafficMirrorFilterRule_sourcePortRange' - The source port range assigned to the Traffic Mirror rule.
--
-- 'trafficDirection', 'trafficMirrorFilterRule_trafficDirection' - The traffic direction assigned to the Traffic Mirror rule.
--
-- 'trafficMirrorFilterId', 'trafficMirrorFilterRule_trafficMirrorFilterId' - The ID of the Traffic Mirror filter that the rule is associated with.
--
-- 'trafficMirrorFilterRuleId', 'trafficMirrorFilterRule_trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
newTrafficMirrorFilterRule ::
  TrafficMirrorFilterRule
newTrafficMirrorFilterRule =
  TrafficMirrorFilterRule'
    { description =
        Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      destinationPortRange = Prelude.Nothing,
      protocol = Prelude.Nothing,
      ruleAction = Prelude.Nothing,
      ruleNumber = Prelude.Nothing,
      sourceCidrBlock = Prelude.Nothing,
      sourcePortRange = Prelude.Nothing,
      trafficDirection = Prelude.Nothing,
      trafficMirrorFilterId = Prelude.Nothing,
      trafficMirrorFilterRuleId = Prelude.Nothing
    }

-- | The description of the Traffic Mirror rule.
trafficMirrorFilterRule_description :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Text)
trafficMirrorFilterRule_description = Lens.lens (\TrafficMirrorFilterRule' {description} -> description) (\s@TrafficMirrorFilterRule' {} a -> s {description = a} :: TrafficMirrorFilterRule)

-- | The destination CIDR block assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_destinationCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Text)
trafficMirrorFilterRule_destinationCidrBlock = Lens.lens (\TrafficMirrorFilterRule' {destinationCidrBlock} -> destinationCidrBlock) (\s@TrafficMirrorFilterRule' {} a -> s {destinationCidrBlock = a} :: TrafficMirrorFilterRule)

-- | The destination port range assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_destinationPortRange :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe TrafficMirrorPortRange)
trafficMirrorFilterRule_destinationPortRange = Lens.lens (\TrafficMirrorFilterRule' {destinationPortRange} -> destinationPortRange) (\s@TrafficMirrorFilterRule' {} a -> s {destinationPortRange = a} :: TrafficMirrorFilterRule)

-- | The protocol assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_protocol :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Int)
trafficMirrorFilterRule_protocol = Lens.lens (\TrafficMirrorFilterRule' {protocol} -> protocol) (\s@TrafficMirrorFilterRule' {} a -> s {protocol = a} :: TrafficMirrorFilterRule)

-- | The action assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_ruleAction :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe TrafficMirrorRuleAction)
trafficMirrorFilterRule_ruleAction = Lens.lens (\TrafficMirrorFilterRule' {ruleAction} -> ruleAction) (\s@TrafficMirrorFilterRule' {} a -> s {ruleAction = a} :: TrafficMirrorFilterRule)

-- | The rule number of the Traffic Mirror rule.
trafficMirrorFilterRule_ruleNumber :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Int)
trafficMirrorFilterRule_ruleNumber = Lens.lens (\TrafficMirrorFilterRule' {ruleNumber} -> ruleNumber) (\s@TrafficMirrorFilterRule' {} a -> s {ruleNumber = a} :: TrafficMirrorFilterRule)

-- | The source CIDR block assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_sourceCidrBlock :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Text)
trafficMirrorFilterRule_sourceCidrBlock = Lens.lens (\TrafficMirrorFilterRule' {sourceCidrBlock} -> sourceCidrBlock) (\s@TrafficMirrorFilterRule' {} a -> s {sourceCidrBlock = a} :: TrafficMirrorFilterRule)

-- | The source port range assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_sourcePortRange :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe TrafficMirrorPortRange)
trafficMirrorFilterRule_sourcePortRange = Lens.lens (\TrafficMirrorFilterRule' {sourcePortRange} -> sourcePortRange) (\s@TrafficMirrorFilterRule' {} a -> s {sourcePortRange = a} :: TrafficMirrorFilterRule)

-- | The traffic direction assigned to the Traffic Mirror rule.
trafficMirrorFilterRule_trafficDirection :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe TrafficDirection)
trafficMirrorFilterRule_trafficDirection = Lens.lens (\TrafficMirrorFilterRule' {trafficDirection} -> trafficDirection) (\s@TrafficMirrorFilterRule' {} a -> s {trafficDirection = a} :: TrafficMirrorFilterRule)

-- | The ID of the Traffic Mirror filter that the rule is associated with.
trafficMirrorFilterRule_trafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Text)
trafficMirrorFilterRule_trafficMirrorFilterId = Lens.lens (\TrafficMirrorFilterRule' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@TrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilterRule)

-- | The ID of the Traffic Mirror rule.
trafficMirrorFilterRule_trafficMirrorFilterRuleId :: Lens.Lens' TrafficMirrorFilterRule (Prelude.Maybe Prelude.Text)
trafficMirrorFilterRule_trafficMirrorFilterRuleId = Lens.lens (\TrafficMirrorFilterRule' {trafficMirrorFilterRuleId} -> trafficMirrorFilterRuleId) (\s@TrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterRuleId = a} :: TrafficMirrorFilterRule)

instance Data.FromXML TrafficMirrorFilterRule where
  parseXML x =
    TrafficMirrorFilterRule'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "destinationCidrBlock")
      Prelude.<*> (x Data..@? "destinationPortRange")
      Prelude.<*> (x Data..@? "protocol")
      Prelude.<*> (x Data..@? "ruleAction")
      Prelude.<*> (x Data..@? "ruleNumber")
      Prelude.<*> (x Data..@? "sourceCidrBlock")
      Prelude.<*> (x Data..@? "sourcePortRange")
      Prelude.<*> (x Data..@? "trafficDirection")
      Prelude.<*> (x Data..@? "trafficMirrorFilterId")
      Prelude.<*> (x Data..@? "trafficMirrorFilterRuleId")

instance Prelude.Hashable TrafficMirrorFilterRule where
  hashWithSalt _salt TrafficMirrorFilterRule' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinationPortRange
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ruleAction
      `Prelude.hashWithSalt` ruleNumber
      `Prelude.hashWithSalt` sourceCidrBlock
      `Prelude.hashWithSalt` sourcePortRange
      `Prelude.hashWithSalt` trafficDirection
      `Prelude.hashWithSalt` trafficMirrorFilterId
      `Prelude.hashWithSalt` trafficMirrorFilterRuleId

instance Prelude.NFData TrafficMirrorFilterRule where
  rnf TrafficMirrorFilterRule' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf destinationCidrBlock `Prelude.seq`
        Prelude.rnf destinationPortRange `Prelude.seq`
          Prelude.rnf protocol `Prelude.seq`
            Prelude.rnf ruleAction `Prelude.seq`
              Prelude.rnf ruleNumber `Prelude.seq`
                Prelude.rnf sourceCidrBlock `Prelude.seq`
                  Prelude.rnf sourcePortRange `Prelude.seq`
                    Prelude.rnf trafficDirection `Prelude.seq`
                      Prelude.rnf trafficMirrorFilterId `Prelude.seq`
                        Prelude.rnf trafficMirrorFilterRuleId

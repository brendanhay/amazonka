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
-- Module      : Amazonka.EC2.Types.TransitGatewayPolicyTableEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPolicyTableEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayPolicyRule
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway policy table entry
--
-- /See:/ 'newTransitGatewayPolicyTableEntry' smart constructor.
data TransitGatewayPolicyTableEntry = TransitGatewayPolicyTableEntry'
  { -- | The rule number for the transit gateway policy table entry.
    policyRuleNumber :: Prelude.Maybe Prelude.Text,
    -- | The policy rule associated with the transit gateway policy table.
    policyRule :: Prelude.Maybe TransitGatewayPolicyRule,
    -- | The ID of the target route table.
    targetRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPolicyTableEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRuleNumber', 'transitGatewayPolicyTableEntry_policyRuleNumber' - The rule number for the transit gateway policy table entry.
--
-- 'policyRule', 'transitGatewayPolicyTableEntry_policyRule' - The policy rule associated with the transit gateway policy table.
--
-- 'targetRouteTableId', 'transitGatewayPolicyTableEntry_targetRouteTableId' - The ID of the target route table.
newTransitGatewayPolicyTableEntry ::
  TransitGatewayPolicyTableEntry
newTransitGatewayPolicyTableEntry =
  TransitGatewayPolicyTableEntry'
    { policyRuleNumber =
        Prelude.Nothing,
      policyRule = Prelude.Nothing,
      targetRouteTableId = Prelude.Nothing
    }

-- | The rule number for the transit gateway policy table entry.
transitGatewayPolicyTableEntry_policyRuleNumber :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableEntry_policyRuleNumber = Lens.lens (\TransitGatewayPolicyTableEntry' {policyRuleNumber} -> policyRuleNumber) (\s@TransitGatewayPolicyTableEntry' {} a -> s {policyRuleNumber = a} :: TransitGatewayPolicyTableEntry)

-- | The policy rule associated with the transit gateway policy table.
transitGatewayPolicyTableEntry_policyRule :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe TransitGatewayPolicyRule)
transitGatewayPolicyTableEntry_policyRule = Lens.lens (\TransitGatewayPolicyTableEntry' {policyRule} -> policyRule) (\s@TransitGatewayPolicyTableEntry' {} a -> s {policyRule = a} :: TransitGatewayPolicyTableEntry)

-- | The ID of the target route table.
transitGatewayPolicyTableEntry_targetRouteTableId :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableEntry_targetRouteTableId = Lens.lens (\TransitGatewayPolicyTableEntry' {targetRouteTableId} -> targetRouteTableId) (\s@TransitGatewayPolicyTableEntry' {} a -> s {targetRouteTableId = a} :: TransitGatewayPolicyTableEntry)

instance Core.FromXML TransitGatewayPolicyTableEntry where
  parseXML x =
    TransitGatewayPolicyTableEntry'
      Prelude.<$> (x Core..@? "policyRuleNumber")
      Prelude.<*> (x Core..@? "policyRule")
      Prelude.<*> (x Core..@? "targetRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayPolicyTableEntry
  where
  hashWithSalt
    _salt
    TransitGatewayPolicyTableEntry' {..} =
      _salt `Prelude.hashWithSalt` policyRuleNumber
        `Prelude.hashWithSalt` policyRule
        `Prelude.hashWithSalt` targetRouteTableId

instance
  Prelude.NFData
    TransitGatewayPolicyTableEntry
  where
  rnf TransitGatewayPolicyTableEntry' {..} =
    Prelude.rnf policyRuleNumber
      `Prelude.seq` Prelude.rnf policyRule
      `Prelude.seq` Prelude.rnf targetRouteTableId

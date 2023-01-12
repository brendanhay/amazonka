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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPolicyTableEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayPolicyRule
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway policy table entry
--
-- /See:/ 'newTransitGatewayPolicyTableEntry' smart constructor.
data TransitGatewayPolicyTableEntry = TransitGatewayPolicyTableEntry'
  { -- | The policy rule associated with the transit gateway policy table.
    policyRule :: Prelude.Maybe TransitGatewayPolicyRule,
    -- | The rule number for the transit gateway policy table entry.
    policyRuleNumber :: Prelude.Maybe Prelude.Text,
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
-- 'policyRule', 'transitGatewayPolicyTableEntry_policyRule' - The policy rule associated with the transit gateway policy table.
--
-- 'policyRuleNumber', 'transitGatewayPolicyTableEntry_policyRuleNumber' - The rule number for the transit gateway policy table entry.
--
-- 'targetRouteTableId', 'transitGatewayPolicyTableEntry_targetRouteTableId' - The ID of the target route table.
newTransitGatewayPolicyTableEntry ::
  TransitGatewayPolicyTableEntry
newTransitGatewayPolicyTableEntry =
  TransitGatewayPolicyTableEntry'
    { policyRule =
        Prelude.Nothing,
      policyRuleNumber = Prelude.Nothing,
      targetRouteTableId = Prelude.Nothing
    }

-- | The policy rule associated with the transit gateway policy table.
transitGatewayPolicyTableEntry_policyRule :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe TransitGatewayPolicyRule)
transitGatewayPolicyTableEntry_policyRule = Lens.lens (\TransitGatewayPolicyTableEntry' {policyRule} -> policyRule) (\s@TransitGatewayPolicyTableEntry' {} a -> s {policyRule = a} :: TransitGatewayPolicyTableEntry)

-- | The rule number for the transit gateway policy table entry.
transitGatewayPolicyTableEntry_policyRuleNumber :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableEntry_policyRuleNumber = Lens.lens (\TransitGatewayPolicyTableEntry' {policyRuleNumber} -> policyRuleNumber) (\s@TransitGatewayPolicyTableEntry' {} a -> s {policyRuleNumber = a} :: TransitGatewayPolicyTableEntry)

-- | The ID of the target route table.
transitGatewayPolicyTableEntry_targetRouteTableId :: Lens.Lens' TransitGatewayPolicyTableEntry (Prelude.Maybe Prelude.Text)
transitGatewayPolicyTableEntry_targetRouteTableId = Lens.lens (\TransitGatewayPolicyTableEntry' {targetRouteTableId} -> targetRouteTableId) (\s@TransitGatewayPolicyTableEntry' {} a -> s {targetRouteTableId = a} :: TransitGatewayPolicyTableEntry)

instance Data.FromXML TransitGatewayPolicyTableEntry where
  parseXML x =
    TransitGatewayPolicyTableEntry'
      Prelude.<$> (x Data..@? "policyRule")
      Prelude.<*> (x Data..@? "policyRuleNumber")
      Prelude.<*> (x Data..@? "targetRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayPolicyTableEntry
  where
  hashWithSalt
    _salt
    TransitGatewayPolicyTableEntry' {..} =
      _salt `Prelude.hashWithSalt` policyRule
        `Prelude.hashWithSalt` policyRuleNumber
        `Prelude.hashWithSalt` targetRouteTableId

instance
  Prelude.NFData
    TransitGatewayPolicyTableEntry
  where
  rnf TransitGatewayPolicyTableEntry' {..} =
    Prelude.rnf policyRule
      `Prelude.seq` Prelude.rnf policyRuleNumber
      `Prelude.seq` Prelude.rnf targetRouteTableId

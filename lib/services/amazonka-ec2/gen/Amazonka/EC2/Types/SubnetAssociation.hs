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
-- Module      : Amazonka.EC2.Types.SubnetAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SubnetAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayMulitcastDomainAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes the subnet association with the transit gateway multicast
-- domain.
--
-- /See:/ 'newSubnetAssociation' smart constructor.
data SubnetAssociation = SubnetAssociation'
  { -- | The state of the subnet association.
    state :: Prelude.Maybe TransitGatewayMulitcastDomainAssociationState,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'subnetAssociation_state' - The state of the subnet association.
--
-- 'subnetId', 'subnetAssociation_subnetId' - The ID of the subnet.
newSubnetAssociation ::
  SubnetAssociation
newSubnetAssociation =
  SubnetAssociation'
    { state = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The state of the subnet association.
subnetAssociation_state :: Lens.Lens' SubnetAssociation (Prelude.Maybe TransitGatewayMulitcastDomainAssociationState)
subnetAssociation_state = Lens.lens (\SubnetAssociation' {state} -> state) (\s@SubnetAssociation' {} a -> s {state = a} :: SubnetAssociation)

-- | The ID of the subnet.
subnetAssociation_subnetId :: Lens.Lens' SubnetAssociation (Prelude.Maybe Prelude.Text)
subnetAssociation_subnetId = Lens.lens (\SubnetAssociation' {subnetId} -> subnetId) (\s@SubnetAssociation' {} a -> s {subnetId = a} :: SubnetAssociation)

instance Data.FromXML SubnetAssociation where
  parseXML x =
    SubnetAssociation'
      Prelude.<$> (x Data..@? "state")
      Prelude.<*> (x Data..@? "subnetId")

instance Prelude.Hashable SubnetAssociation where
  hashWithSalt _salt SubnetAssociation' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData SubnetAssociation where
  rnf SubnetAssociation' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetId

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
-- Module      : Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetCidrBlockState
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a subnet and an IPv6 CIDR block.
--
-- /See:/ 'newSubnetIpv6CidrBlockAssociation' smart constructor.
data SubnetIpv6CidrBlockAssociation = SubnetIpv6CidrBlockAssociation'
  { -- | The ID of the association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The state of the CIDR block.
    ipv6CidrBlockState :: Prelude.Maybe SubnetCidrBlockState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetIpv6CidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'subnetIpv6CidrBlockAssociation_associationId' - The ID of the association.
--
-- 'ipv6CidrBlock', 'subnetIpv6CidrBlockAssociation_ipv6CidrBlock' - The IPv6 CIDR block.
--
-- 'ipv6CidrBlockState', 'subnetIpv6CidrBlockAssociation_ipv6CidrBlockState' - The state of the CIDR block.
newSubnetIpv6CidrBlockAssociation ::
  SubnetIpv6CidrBlockAssociation
newSubnetIpv6CidrBlockAssociation =
  SubnetIpv6CidrBlockAssociation'
    { associationId =
        Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      ipv6CidrBlockState = Prelude.Nothing
    }

-- | The ID of the association.
subnetIpv6CidrBlockAssociation_associationId :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
subnetIpv6CidrBlockAssociation_associationId = Lens.lens (\SubnetIpv6CidrBlockAssociation' {associationId} -> associationId) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {associationId = a} :: SubnetIpv6CidrBlockAssociation)

-- | The IPv6 CIDR block.
subnetIpv6CidrBlockAssociation_ipv6CidrBlock :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
subnetIpv6CidrBlockAssociation_ipv6CidrBlock = Lens.lens (\SubnetIpv6CidrBlockAssociation' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlock = a} :: SubnetIpv6CidrBlockAssociation)

-- | The state of the CIDR block.
subnetIpv6CidrBlockAssociation_ipv6CidrBlockState :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe SubnetCidrBlockState)
subnetIpv6CidrBlockAssociation_ipv6CidrBlockState = Lens.lens (\SubnetIpv6CidrBlockAssociation' {ipv6CidrBlockState} -> ipv6CidrBlockState) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlockState = a} :: SubnetIpv6CidrBlockAssociation)

instance Data.FromXML SubnetIpv6CidrBlockAssociation where
  parseXML x =
    SubnetIpv6CidrBlockAssociation'
      Prelude.<$> (x Data..@? "associationId")
      Prelude.<*> (x Data..@? "ipv6CidrBlock")
      Prelude.<*> (x Data..@? "ipv6CidrBlockState")

instance
  Prelude.Hashable
    SubnetIpv6CidrBlockAssociation
  where
  hashWithSalt
    _salt
    SubnetIpv6CidrBlockAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` ipv6CidrBlock
        `Prelude.hashWithSalt` ipv6CidrBlockState

instance
  Prelude.NFData
    SubnetIpv6CidrBlockAssociation
  where
  rnf SubnetIpv6CidrBlockAssociation' {..} =
    Prelude.rnf associationId `Prelude.seq`
      Prelude.rnf ipv6CidrBlock `Prelude.seq`
        Prelude.rnf ipv6CidrBlockState

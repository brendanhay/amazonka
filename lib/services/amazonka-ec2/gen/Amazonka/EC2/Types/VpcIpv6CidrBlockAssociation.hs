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
-- Module      : Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpcCidrBlockState
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv6 CIDR block associated with a VPC.
--
-- /See:/ 'newVpcIpv6CidrBlockAssociation' smart constructor.
data VpcIpv6CidrBlockAssociation = VpcIpv6CidrBlockAssociation'
  { -- | The association ID for the IPv6 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the CIDR block.
    ipv6CidrBlockState :: Prelude.Maybe VpcCidrBlockState,
    -- | The ID of the IPv6 address pool from which the IPv6 CIDR block is
    -- allocated.
    ipv6Pool :: Prelude.Maybe Prelude.Text,
    -- | The name of the unique set of Availability Zones, Local Zones, or
    -- Wavelength Zones from which Amazon Web Services advertises IP addresses,
    -- for example, @us-east-1-wl1-bos-wlz-1@.
    networkBorderGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcIpv6CidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'vpcIpv6CidrBlockAssociation_associationId' - The association ID for the IPv6 CIDR block.
--
-- 'ipv6CidrBlock', 'vpcIpv6CidrBlockAssociation_ipv6CidrBlock' - The IPv6 CIDR block.
--
-- 'ipv6CidrBlockState', 'vpcIpv6CidrBlockAssociation_ipv6CidrBlockState' - Information about the state of the CIDR block.
--
-- 'ipv6Pool', 'vpcIpv6CidrBlockAssociation_ipv6Pool' - The ID of the IPv6 address pool from which the IPv6 CIDR block is
-- allocated.
--
-- 'networkBorderGroup', 'vpcIpv6CidrBlockAssociation_networkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which Amazon Web Services advertises IP addresses,
-- for example, @us-east-1-wl1-bos-wlz-1@.
newVpcIpv6CidrBlockAssociation ::
  VpcIpv6CidrBlockAssociation
newVpcIpv6CidrBlockAssociation =
  VpcIpv6CidrBlockAssociation'
    { associationId =
        Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing,
      ipv6CidrBlockState = Prelude.Nothing,
      ipv6Pool = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing
    }

-- | The association ID for the IPv6 CIDR block.
vpcIpv6CidrBlockAssociation_associationId :: Lens.Lens' VpcIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcIpv6CidrBlockAssociation_associationId = Lens.lens (\VpcIpv6CidrBlockAssociation' {associationId} -> associationId) (\s@VpcIpv6CidrBlockAssociation' {} a -> s {associationId = a} :: VpcIpv6CidrBlockAssociation)

-- | The IPv6 CIDR block.
vpcIpv6CidrBlockAssociation_ipv6CidrBlock :: Lens.Lens' VpcIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcIpv6CidrBlockAssociation_ipv6CidrBlock = Lens.lens (\VpcIpv6CidrBlockAssociation' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@VpcIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlock = a} :: VpcIpv6CidrBlockAssociation)

-- | Information about the state of the CIDR block.
vpcIpv6CidrBlockAssociation_ipv6CidrBlockState :: Lens.Lens' VpcIpv6CidrBlockAssociation (Prelude.Maybe VpcCidrBlockState)
vpcIpv6CidrBlockAssociation_ipv6CidrBlockState = Lens.lens (\VpcIpv6CidrBlockAssociation' {ipv6CidrBlockState} -> ipv6CidrBlockState) (\s@VpcIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlockState = a} :: VpcIpv6CidrBlockAssociation)

-- | The ID of the IPv6 address pool from which the IPv6 CIDR block is
-- allocated.
vpcIpv6CidrBlockAssociation_ipv6Pool :: Lens.Lens' VpcIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcIpv6CidrBlockAssociation_ipv6Pool = Lens.lens (\VpcIpv6CidrBlockAssociation' {ipv6Pool} -> ipv6Pool) (\s@VpcIpv6CidrBlockAssociation' {} a -> s {ipv6Pool = a} :: VpcIpv6CidrBlockAssociation)

-- | The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which Amazon Web Services advertises IP addresses,
-- for example, @us-east-1-wl1-bos-wlz-1@.
vpcIpv6CidrBlockAssociation_networkBorderGroup :: Lens.Lens' VpcIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcIpv6CidrBlockAssociation_networkBorderGroup = Lens.lens (\VpcIpv6CidrBlockAssociation' {networkBorderGroup} -> networkBorderGroup) (\s@VpcIpv6CidrBlockAssociation' {} a -> s {networkBorderGroup = a} :: VpcIpv6CidrBlockAssociation)

instance Data.FromXML VpcIpv6CidrBlockAssociation where
  parseXML x =
    VpcIpv6CidrBlockAssociation'
      Prelude.<$> (x Data..@? "associationId")
      Prelude.<*> (x Data..@? "ipv6CidrBlock")
      Prelude.<*> (x Data..@? "ipv6CidrBlockState")
      Prelude.<*> (x Data..@? "ipv6Pool")
      Prelude.<*> (x Data..@? "networkBorderGroup")

instance Prelude.Hashable VpcIpv6CidrBlockAssociation where
  hashWithSalt _salt VpcIpv6CidrBlockAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` ipv6CidrBlockState
      `Prelude.hashWithSalt` ipv6Pool
      `Prelude.hashWithSalt` networkBorderGroup

instance Prelude.NFData VpcIpv6CidrBlockAssociation where
  rnf VpcIpv6CidrBlockAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf ipv6CidrBlockState
      `Prelude.seq` Prelude.rnf ipv6Pool
      `Prelude.seq` Prelude.rnf networkBorderGroup

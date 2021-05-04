{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetCidrBlockState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv6 CIDR block associated with a subnet.
--
-- /See:/ 'newSubnetIpv6CidrBlockAssociation' smart constructor.
data SubnetIpv6CidrBlockAssociation = SubnetIpv6CidrBlockAssociation'
  { -- | Information about the state of the CIDR block.
    ipv6CidrBlockState :: Prelude.Maybe SubnetCidrBlockState,
    -- | The association ID for the CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR block.
    ipv6CidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubnetIpv6CidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlockState', 'subnetIpv6CidrBlockAssociation_ipv6CidrBlockState' - Information about the state of the CIDR block.
--
-- 'associationId', 'subnetIpv6CidrBlockAssociation_associationId' - The association ID for the CIDR block.
--
-- 'ipv6CidrBlock', 'subnetIpv6CidrBlockAssociation_ipv6CidrBlock' - The IPv6 CIDR block.
newSubnetIpv6CidrBlockAssociation ::
  SubnetIpv6CidrBlockAssociation
newSubnetIpv6CidrBlockAssociation =
  SubnetIpv6CidrBlockAssociation'
    { ipv6CidrBlockState =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      ipv6CidrBlock = Prelude.Nothing
    }

-- | Information about the state of the CIDR block.
subnetIpv6CidrBlockAssociation_ipv6CidrBlockState :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe SubnetCidrBlockState)
subnetIpv6CidrBlockAssociation_ipv6CidrBlockState = Lens.lens (\SubnetIpv6CidrBlockAssociation' {ipv6CidrBlockState} -> ipv6CidrBlockState) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlockState = a} :: SubnetIpv6CidrBlockAssociation)

-- | The association ID for the CIDR block.
subnetIpv6CidrBlockAssociation_associationId :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
subnetIpv6CidrBlockAssociation_associationId = Lens.lens (\SubnetIpv6CidrBlockAssociation' {associationId} -> associationId) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {associationId = a} :: SubnetIpv6CidrBlockAssociation)

-- | The IPv6 CIDR block.
subnetIpv6CidrBlockAssociation_ipv6CidrBlock :: Lens.Lens' SubnetIpv6CidrBlockAssociation (Prelude.Maybe Prelude.Text)
subnetIpv6CidrBlockAssociation_ipv6CidrBlock = Lens.lens (\SubnetIpv6CidrBlockAssociation' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@SubnetIpv6CidrBlockAssociation' {} a -> s {ipv6CidrBlock = a} :: SubnetIpv6CidrBlockAssociation)

instance
  Prelude.FromXML
    SubnetIpv6CidrBlockAssociation
  where
  parseXML x =
    SubnetIpv6CidrBlockAssociation'
      Prelude.<$> (x Prelude..@? "ipv6CidrBlockState")
      Prelude.<*> (x Prelude..@? "associationId")
      Prelude.<*> (x Prelude..@? "ipv6CidrBlock")

instance
  Prelude.Hashable
    SubnetIpv6CidrBlockAssociation

instance
  Prelude.NFData
    SubnetIpv6CidrBlockAssociation

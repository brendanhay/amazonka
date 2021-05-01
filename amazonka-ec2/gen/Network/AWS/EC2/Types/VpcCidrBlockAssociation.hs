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
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcCidrBlockAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpcCidrBlockState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'newVpcCidrBlockAssociation' smart constructor.
data VpcCidrBlockAssociation = VpcCidrBlockAssociation'
  { -- | Information about the state of the CIDR block.
    cidrBlockState :: Prelude.Maybe VpcCidrBlockState,
    -- | The association ID for the IPv4 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcCidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlockState', 'vpcCidrBlockAssociation_cidrBlockState' - Information about the state of the CIDR block.
--
-- 'associationId', 'vpcCidrBlockAssociation_associationId' - The association ID for the IPv4 CIDR block.
--
-- 'cidrBlock', 'vpcCidrBlockAssociation_cidrBlock' - The IPv4 CIDR block.
newVpcCidrBlockAssociation ::
  VpcCidrBlockAssociation
newVpcCidrBlockAssociation =
  VpcCidrBlockAssociation'
    { cidrBlockState =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      cidrBlock = Prelude.Nothing
    }

-- | Information about the state of the CIDR block.
vpcCidrBlockAssociation_cidrBlockState :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe VpcCidrBlockState)
vpcCidrBlockAssociation_cidrBlockState = Lens.lens (\VpcCidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlockState = a} :: VpcCidrBlockAssociation)

-- | The association ID for the IPv4 CIDR block.
vpcCidrBlockAssociation_associationId :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_associationId = Lens.lens (\VpcCidrBlockAssociation' {associationId} -> associationId) (\s@VpcCidrBlockAssociation' {} a -> s {associationId = a} :: VpcCidrBlockAssociation)

-- | The IPv4 CIDR block.
vpcCidrBlockAssociation_cidrBlock :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_cidrBlock = Lens.lens (\VpcCidrBlockAssociation' {cidrBlock} -> cidrBlock) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlock = a} :: VpcCidrBlockAssociation)

instance Prelude.FromXML VpcCidrBlockAssociation where
  parseXML x =
    VpcCidrBlockAssociation'
      Prelude.<$> (x Prelude..@? "cidrBlockState")
      Prelude.<*> (x Prelude..@? "associationId")
      Prelude.<*> (x Prelude..@? "cidrBlock")

instance Prelude.Hashable VpcCidrBlockAssociation

instance Prelude.NFData VpcCidrBlockAssociation

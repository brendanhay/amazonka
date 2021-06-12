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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VpcCidrBlockState
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'newVpcCidrBlockAssociation' smart constructor.
data VpcCidrBlockAssociation = VpcCidrBlockAssociation'
  { -- | Information about the state of the CIDR block.
    cidrBlockState :: Core.Maybe VpcCidrBlockState,
    -- | The association ID for the IPv4 CIDR block.
    associationId :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR block.
    cidrBlock :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      associationId = Core.Nothing,
      cidrBlock = Core.Nothing
    }

-- | Information about the state of the CIDR block.
vpcCidrBlockAssociation_cidrBlockState :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe VpcCidrBlockState)
vpcCidrBlockAssociation_cidrBlockState = Lens.lens (\VpcCidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlockState = a} :: VpcCidrBlockAssociation)

-- | The association ID for the IPv4 CIDR block.
vpcCidrBlockAssociation_associationId :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe Core.Text)
vpcCidrBlockAssociation_associationId = Lens.lens (\VpcCidrBlockAssociation' {associationId} -> associationId) (\s@VpcCidrBlockAssociation' {} a -> s {associationId = a} :: VpcCidrBlockAssociation)

-- | The IPv4 CIDR block.
vpcCidrBlockAssociation_cidrBlock :: Lens.Lens' VpcCidrBlockAssociation (Core.Maybe Core.Text)
vpcCidrBlockAssociation_cidrBlock = Lens.lens (\VpcCidrBlockAssociation' {cidrBlock} -> cidrBlock) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlock = a} :: VpcCidrBlockAssociation)

instance Core.FromXML VpcCidrBlockAssociation where
  parseXML x =
    VpcCidrBlockAssociation'
      Core.<$> (x Core..@? "cidrBlockState")
      Core.<*> (x Core..@? "associationId")
      Core.<*> (x Core..@? "cidrBlock")

instance Core.Hashable VpcCidrBlockAssociation

instance Core.NFData VpcCidrBlockAssociation

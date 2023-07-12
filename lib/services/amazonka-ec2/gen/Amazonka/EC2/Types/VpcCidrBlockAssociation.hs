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
-- Module      : Amazonka.EC2.Types.VpcCidrBlockAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcCidrBlockAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VpcCidrBlockState
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv4 CIDR block associated with a VPC.
--
-- /See:/ 'newVpcCidrBlockAssociation' smart constructor.
data VpcCidrBlockAssociation = VpcCidrBlockAssociation'
  { -- | The association ID for the IPv4 CIDR block.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR block.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the state of the CIDR block.
    cidrBlockState :: Prelude.Maybe VpcCidrBlockState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcCidrBlockAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'vpcCidrBlockAssociation_associationId' - The association ID for the IPv4 CIDR block.
--
-- 'cidrBlock', 'vpcCidrBlockAssociation_cidrBlock' - The IPv4 CIDR block.
--
-- 'cidrBlockState', 'vpcCidrBlockAssociation_cidrBlockState' - Information about the state of the CIDR block.
newVpcCidrBlockAssociation ::
  VpcCidrBlockAssociation
newVpcCidrBlockAssociation =
  VpcCidrBlockAssociation'
    { associationId =
        Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      cidrBlockState = Prelude.Nothing
    }

-- | The association ID for the IPv4 CIDR block.
vpcCidrBlockAssociation_associationId :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_associationId = Lens.lens (\VpcCidrBlockAssociation' {associationId} -> associationId) (\s@VpcCidrBlockAssociation' {} a -> s {associationId = a} :: VpcCidrBlockAssociation)

-- | The IPv4 CIDR block.
vpcCidrBlockAssociation_cidrBlock :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe Prelude.Text)
vpcCidrBlockAssociation_cidrBlock = Lens.lens (\VpcCidrBlockAssociation' {cidrBlock} -> cidrBlock) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlock = a} :: VpcCidrBlockAssociation)

-- | Information about the state of the CIDR block.
vpcCidrBlockAssociation_cidrBlockState :: Lens.Lens' VpcCidrBlockAssociation (Prelude.Maybe VpcCidrBlockState)
vpcCidrBlockAssociation_cidrBlockState = Lens.lens (\VpcCidrBlockAssociation' {cidrBlockState} -> cidrBlockState) (\s@VpcCidrBlockAssociation' {} a -> s {cidrBlockState = a} :: VpcCidrBlockAssociation)

instance Data.FromXML VpcCidrBlockAssociation where
  parseXML x =
    VpcCidrBlockAssociation'
      Prelude.<$> (x Data..@? "associationId")
      Prelude.<*> (x Data..@? "cidrBlock")
      Prelude.<*> (x Data..@? "cidrBlockState")

instance Prelude.Hashable VpcCidrBlockAssociation where
  hashWithSalt _salt VpcCidrBlockAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` cidrBlockState

instance Prelude.NFData VpcCidrBlockAssociation where
  rnf VpcCidrBlockAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf cidrBlockState

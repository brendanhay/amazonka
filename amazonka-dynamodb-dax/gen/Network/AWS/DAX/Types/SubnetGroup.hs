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
-- Module      : Network.AWS.DAX.Types.SubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SubnetGroup where

import Network.AWS.DAX.Types.Subnet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of one of the following actions:
--
-- -   /CreateSubnetGroup/
--
-- -   /ModifySubnetGroup/
--
-- /See:/ 'newSubnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { -- | The description of the subnet group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group.
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets associated with the subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
    -- group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'subnetGroup_description' - The description of the subnet group.
--
-- 'subnetGroupName', 'subnetGroup_subnetGroupName' - The name of the subnet group.
--
-- 'subnets', 'subnetGroup_subnets' - A list of subnets associated with the subnet group.
--
-- 'vpcId', 'subnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
newSubnetGroup ::
  SubnetGroup
newSubnetGroup =
  SubnetGroup'
    { description = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The description of the subnet group.
subnetGroup_description :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_description = Lens.lens (\SubnetGroup' {description} -> description) (\s@SubnetGroup' {} a -> s {description = a} :: SubnetGroup)

-- | The name of the subnet group.
subnetGroup_subnetGroupName :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_subnetGroupName = Lens.lens (\SubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@SubnetGroup' {} a -> s {subnetGroupName = a} :: SubnetGroup)

-- | A list of subnets associated with the subnet group.
subnetGroup_subnets :: Lens.Lens' SubnetGroup (Prelude.Maybe [Subnet])
subnetGroup_subnets = Lens.lens (\SubnetGroup' {subnets} -> subnets) (\s@SubnetGroup' {} a -> s {subnets = a} :: SubnetGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
subnetGroup_vpcId :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_vpcId = Lens.lens (\SubnetGroup' {vpcId} -> vpcId) (\s@SubnetGroup' {} a -> s {vpcId = a} :: SubnetGroup)

instance Prelude.FromJSON SubnetGroup where
  parseJSON =
    Prelude.withObject
      "SubnetGroup"
      ( \x ->
          SubnetGroup'
            Prelude.<$> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SubnetGroupName")
            Prelude.<*> (x Prelude..:? "Subnets" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "VpcId")
      )

instance Prelude.Hashable SubnetGroup

instance Prelude.NFData SubnetGroup

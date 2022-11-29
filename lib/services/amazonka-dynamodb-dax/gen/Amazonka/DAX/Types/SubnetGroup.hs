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
-- Module      : Amazonka.DAX.Types.SubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.SubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types.Subnet
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of one of the following actions:
--
-- -   /CreateSubnetGroup/
--
-- -   /ModifySubnetGroup/
--
-- /See:/ 'newSubnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { -- | The name of the subnet group.
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets associated with the subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | The description of the subnet group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
    -- group.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroupName', 'subnetGroup_subnetGroupName' - The name of the subnet group.
--
-- 'subnets', 'subnetGroup_subnets' - A list of subnets associated with the subnet group.
--
-- 'description', 'subnetGroup_description' - The description of the subnet group.
--
-- 'vpcId', 'subnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
newSubnetGroup ::
  SubnetGroup
newSubnetGroup =
  SubnetGroup'
    { subnetGroupName = Prelude.Nothing,
      subnets = Prelude.Nothing,
      description = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The name of the subnet group.
subnetGroup_subnetGroupName :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_subnetGroupName = Lens.lens (\SubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@SubnetGroup' {} a -> s {subnetGroupName = a} :: SubnetGroup)

-- | A list of subnets associated with the subnet group.
subnetGroup_subnets :: Lens.Lens' SubnetGroup (Prelude.Maybe [Subnet])
subnetGroup_subnets = Lens.lens (\SubnetGroup' {subnets} -> subnets) (\s@SubnetGroup' {} a -> s {subnets = a} :: SubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The description of the subnet group.
subnetGroup_description :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_description = Lens.lens (\SubnetGroup' {description} -> description) (\s@SubnetGroup' {} a -> s {description = a} :: SubnetGroup)

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
subnetGroup_vpcId :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_vpcId = Lens.lens (\SubnetGroup' {vpcId} -> vpcId) (\s@SubnetGroup' {} a -> s {vpcId = a} :: SubnetGroup)

instance Core.FromJSON SubnetGroup where
  parseJSON =
    Core.withObject
      "SubnetGroup"
      ( \x ->
          SubnetGroup'
            Prelude.<$> (x Core..:? "SubnetGroupName")
            Prelude.<*> (x Core..:? "Subnets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "VpcId")
      )

instance Prelude.Hashable SubnetGroup where
  hashWithSalt _salt SubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` subnetGroupName
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData SubnetGroup where
  rnf SubnetGroup' {..} =
    Prelude.rnf subnetGroupName
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf vpcId

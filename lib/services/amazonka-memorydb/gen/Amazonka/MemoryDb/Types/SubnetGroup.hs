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
-- Module      : Amazonka.MemoryDb.Types.SubnetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.SubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.Subnet
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of one of the following operations:
--
-- -   CreateSubnetGroup
--
-- -   UpdateSubnetGroup
--
-- A subnet group is a collection of subnets (typically private) that you
-- can designate for your clusters running in an Amazon Virtual Private
-- Cloud (VPC) environment.
--
-- /See:/ 'newSubnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { -- | The ARN (Amazon Resource Name) of the subnet group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the subnet group
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of subnets associated with the subnet group.
    subnets :: Prelude.Maybe [Subnet],
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
-- 'arn', 'subnetGroup_arn' - The ARN (Amazon Resource Name) of the subnet group.
--
-- 'description', 'subnetGroup_description' - A description of the subnet group
--
-- 'name', 'subnetGroup_name' - The name of the subnet group
--
-- 'subnets', 'subnetGroup_subnets' - A list of subnets associated with the subnet group.
--
-- 'vpcId', 'subnetGroup_vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
newSubnetGroup ::
  SubnetGroup
newSubnetGroup =
  SubnetGroup'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the subnet group.
subnetGroup_arn :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_arn = Lens.lens (\SubnetGroup' {arn} -> arn) (\s@SubnetGroup' {} a -> s {arn = a} :: SubnetGroup)

-- | A description of the subnet group
subnetGroup_description :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_description = Lens.lens (\SubnetGroup' {description} -> description) (\s@SubnetGroup' {} a -> s {description = a} :: SubnetGroup)

-- | The name of the subnet group
subnetGroup_name :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_name = Lens.lens (\SubnetGroup' {name} -> name) (\s@SubnetGroup' {} a -> s {name = a} :: SubnetGroup)

-- | A list of subnets associated with the subnet group.
subnetGroup_subnets :: Lens.Lens' SubnetGroup (Prelude.Maybe [Subnet])
subnetGroup_subnets = Lens.lens (\SubnetGroup' {subnets} -> subnets) (\s@SubnetGroup' {} a -> s {subnets = a} :: SubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet
-- group.
subnetGroup_vpcId :: Lens.Lens' SubnetGroup (Prelude.Maybe Prelude.Text)
subnetGroup_vpcId = Lens.lens (\SubnetGroup' {vpcId} -> vpcId) (\s@SubnetGroup' {} a -> s {vpcId = a} :: SubnetGroup)

instance Data.FromJSON SubnetGroup where
  parseJSON =
    Data.withObject
      "SubnetGroup"
      ( \x ->
          SubnetGroup'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable SubnetGroup where
  hashWithSalt _salt SubnetGroup' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData SubnetGroup where
  rnf SubnetGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId

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
-- Module      : Amazonka.IoT.Types.VpcDestinationProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.VpcDestinationProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties of a virtual private cloud (VPC) destination.
--
-- /See:/ 'newVpcDestinationProperties' smart constructor.
data VpcDestinationProperties = VpcDestinationProperties'
  { -- | The ARN of a role that has permission to create and attach to elastic
    -- network interfaces (ENIs).
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The security groups of the VPC destination.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The subnet IDs of the VPC destination.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'vpcDestinationProperties_roleArn' - The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
--
-- 'securityGroups', 'vpcDestinationProperties_securityGroups' - The security groups of the VPC destination.
--
-- 'subnetIds', 'vpcDestinationProperties_subnetIds' - The subnet IDs of the VPC destination.
--
-- 'vpcId', 'vpcDestinationProperties_vpcId' - The ID of the VPC.
newVpcDestinationProperties ::
  VpcDestinationProperties
newVpcDestinationProperties =
  VpcDestinationProperties'
    { roleArn =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
vpcDestinationProperties_roleArn :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe Prelude.Text)
vpcDestinationProperties_roleArn = Lens.lens (\VpcDestinationProperties' {roleArn} -> roleArn) (\s@VpcDestinationProperties' {} a -> s {roleArn = a} :: VpcDestinationProperties)

-- | The security groups of the VPC destination.
vpcDestinationProperties_securityGroups :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe [Prelude.Text])
vpcDestinationProperties_securityGroups = Lens.lens (\VpcDestinationProperties' {securityGroups} -> securityGroups) (\s@VpcDestinationProperties' {} a -> s {securityGroups = a} :: VpcDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | The subnet IDs of the VPC destination.
vpcDestinationProperties_subnetIds :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe [Prelude.Text])
vpcDestinationProperties_subnetIds = Lens.lens (\VpcDestinationProperties' {subnetIds} -> subnetIds) (\s@VpcDestinationProperties' {} a -> s {subnetIds = a} :: VpcDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
vpcDestinationProperties_vpcId :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe Prelude.Text)
vpcDestinationProperties_vpcId = Lens.lens (\VpcDestinationProperties' {vpcId} -> vpcId) (\s@VpcDestinationProperties' {} a -> s {vpcId = a} :: VpcDestinationProperties)

instance Data.FromJSON VpcDestinationProperties where
  parseJSON =
    Data.withObject
      "VpcDestinationProperties"
      ( \x ->
          VpcDestinationProperties'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "securityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable VpcDestinationProperties where
  hashWithSalt _salt VpcDestinationProperties' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcDestinationProperties where
  rnf VpcDestinationProperties' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

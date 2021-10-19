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
-- Module      : Network.AWS.IoT.Types.VpcDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.VpcDestinationProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties of a virtual private cloud (VPC) destination.
--
-- /See:/ 'newVpcDestinationProperties' smart constructor.
data VpcDestinationProperties = VpcDestinationProperties'
  { -- | The security groups of the VPC destination.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The subnet IDs of the VPC destination.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a role that has permission to create and attach to elastic
    -- network interfaces (ENIs).
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'securityGroups', 'vpcDestinationProperties_securityGroups' - The security groups of the VPC destination.
--
-- 'subnetIds', 'vpcDestinationProperties_subnetIds' - The subnet IDs of the VPC destination.
--
-- 'vpcId', 'vpcDestinationProperties_vpcId' - The ID of the VPC.
--
-- 'roleArn', 'vpcDestinationProperties_roleArn' - The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
newVpcDestinationProperties ::
  VpcDestinationProperties
newVpcDestinationProperties =
  VpcDestinationProperties'
    { securityGroups =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The security groups of the VPC destination.
vpcDestinationProperties_securityGroups :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe [Prelude.Text])
vpcDestinationProperties_securityGroups = Lens.lens (\VpcDestinationProperties' {securityGroups} -> securityGroups) (\s@VpcDestinationProperties' {} a -> s {securityGroups = a} :: VpcDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | The subnet IDs of the VPC destination.
vpcDestinationProperties_subnetIds :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe [Prelude.Text])
vpcDestinationProperties_subnetIds = Lens.lens (\VpcDestinationProperties' {subnetIds} -> subnetIds) (\s@VpcDestinationProperties' {} a -> s {subnetIds = a} :: VpcDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
vpcDestinationProperties_vpcId :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe Prelude.Text)
vpcDestinationProperties_vpcId = Lens.lens (\VpcDestinationProperties' {vpcId} -> vpcId) (\s@VpcDestinationProperties' {} a -> s {vpcId = a} :: VpcDestinationProperties)

-- | The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
vpcDestinationProperties_roleArn :: Lens.Lens' VpcDestinationProperties (Prelude.Maybe Prelude.Text)
vpcDestinationProperties_roleArn = Lens.lens (\VpcDestinationProperties' {roleArn} -> roleArn) (\s@VpcDestinationProperties' {} a -> s {roleArn = a} :: VpcDestinationProperties)

instance Core.FromJSON VpcDestinationProperties where
  parseJSON =
    Core.withObject
      "VpcDestinationProperties"
      ( \x ->
          VpcDestinationProperties'
            Prelude.<$> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "vpcId")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable VpcDestinationProperties

instance Prelude.NFData VpcDestinationProperties

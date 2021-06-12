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

-- | The properties of a virtual private cloud (VPC) destination.
--
-- /See:/ 'newVpcDestinationProperties' smart constructor.
data VpcDestinationProperties = VpcDestinationProperties'
  { -- | The ARN of a role that has permission to create and attach to elastic
    -- network interfaces (ENIs).
    roleArn :: Core.Maybe Core.Text,
    -- | The subnet IDs of the VPC destination.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The security groups of the VPC destination.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'subnetIds', 'vpcDestinationProperties_subnetIds' - The subnet IDs of the VPC destination.
--
-- 'securityGroups', 'vpcDestinationProperties_securityGroups' - The security groups of the VPC destination.
--
-- 'vpcId', 'vpcDestinationProperties_vpcId' - The ID of the VPC.
newVpcDestinationProperties ::
  VpcDestinationProperties
newVpcDestinationProperties =
  VpcDestinationProperties'
    { roleArn = Core.Nothing,
      subnetIds = Core.Nothing,
      securityGroups = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
vpcDestinationProperties_roleArn :: Lens.Lens' VpcDestinationProperties (Core.Maybe Core.Text)
vpcDestinationProperties_roleArn = Lens.lens (\VpcDestinationProperties' {roleArn} -> roleArn) (\s@VpcDestinationProperties' {} a -> s {roleArn = a} :: VpcDestinationProperties)

-- | The subnet IDs of the VPC destination.
vpcDestinationProperties_subnetIds :: Lens.Lens' VpcDestinationProperties (Core.Maybe [Core.Text])
vpcDestinationProperties_subnetIds = Lens.lens (\VpcDestinationProperties' {subnetIds} -> subnetIds) (\s@VpcDestinationProperties' {} a -> s {subnetIds = a} :: VpcDestinationProperties) Core.. Lens.mapping Lens._Coerce

-- | The security groups of the VPC destination.
vpcDestinationProperties_securityGroups :: Lens.Lens' VpcDestinationProperties (Core.Maybe [Core.Text])
vpcDestinationProperties_securityGroups = Lens.lens (\VpcDestinationProperties' {securityGroups} -> securityGroups) (\s@VpcDestinationProperties' {} a -> s {securityGroups = a} :: VpcDestinationProperties) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC.
vpcDestinationProperties_vpcId :: Lens.Lens' VpcDestinationProperties (Core.Maybe Core.Text)
vpcDestinationProperties_vpcId = Lens.lens (\VpcDestinationProperties' {vpcId} -> vpcId) (\s@VpcDestinationProperties' {} a -> s {vpcId = a} :: VpcDestinationProperties)

instance Core.FromJSON VpcDestinationProperties where
  parseJSON =
    Core.withObject
      "VpcDestinationProperties"
      ( \x ->
          VpcDestinationProperties'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "subnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "securityGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpcId")
      )

instance Core.Hashable VpcDestinationProperties

instance Core.NFData VpcDestinationProperties

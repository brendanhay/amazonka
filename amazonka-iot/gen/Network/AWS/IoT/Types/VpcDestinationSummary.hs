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
-- Module      : Network.AWS.IoT.Types.VpcDestinationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.VpcDestinationSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The summary of a virtual private cloud (VPC) destination.
--
-- /See:/ 'newVpcDestinationSummary' smart constructor.
data VpcDestinationSummary = VpcDestinationSummary'
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
-- Create a value of 'VpcDestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'vpcDestinationSummary_roleArn' - The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
--
-- 'subnetIds', 'vpcDestinationSummary_subnetIds' - The subnet IDs of the VPC destination.
--
-- 'securityGroups', 'vpcDestinationSummary_securityGroups' - The security groups of the VPC destination.
--
-- 'vpcId', 'vpcDestinationSummary_vpcId' - The ID of the VPC.
newVpcDestinationSummary ::
  VpcDestinationSummary
newVpcDestinationSummary =
  VpcDestinationSummary'
    { roleArn = Core.Nothing,
      subnetIds = Core.Nothing,
      securityGroups = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
vpcDestinationSummary_roleArn :: Lens.Lens' VpcDestinationSummary (Core.Maybe Core.Text)
vpcDestinationSummary_roleArn = Lens.lens (\VpcDestinationSummary' {roleArn} -> roleArn) (\s@VpcDestinationSummary' {} a -> s {roleArn = a} :: VpcDestinationSummary)

-- | The subnet IDs of the VPC destination.
vpcDestinationSummary_subnetIds :: Lens.Lens' VpcDestinationSummary (Core.Maybe [Core.Text])
vpcDestinationSummary_subnetIds = Lens.lens (\VpcDestinationSummary' {subnetIds} -> subnetIds) (\s@VpcDestinationSummary' {} a -> s {subnetIds = a} :: VpcDestinationSummary) Core.. Lens.mapping Lens._Coerce

-- | The security groups of the VPC destination.
vpcDestinationSummary_securityGroups :: Lens.Lens' VpcDestinationSummary (Core.Maybe [Core.Text])
vpcDestinationSummary_securityGroups = Lens.lens (\VpcDestinationSummary' {securityGroups} -> securityGroups) (\s@VpcDestinationSummary' {} a -> s {securityGroups = a} :: VpcDestinationSummary) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC.
vpcDestinationSummary_vpcId :: Lens.Lens' VpcDestinationSummary (Core.Maybe Core.Text)
vpcDestinationSummary_vpcId = Lens.lens (\VpcDestinationSummary' {vpcId} -> vpcId) (\s@VpcDestinationSummary' {} a -> s {vpcId = a} :: VpcDestinationSummary)

instance Core.FromJSON VpcDestinationSummary where
  parseJSON =
    Core.withObject
      "VpcDestinationSummary"
      ( \x ->
          VpcDestinationSummary'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "subnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "securityGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpcId")
      )

instance Core.Hashable VpcDestinationSummary

instance Core.NFData VpcDestinationSummary

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
-- Module      : Network.AWS.EC2.Types.SecurityGroupReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupReference where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a VPC with a security group that references your security
-- group.
--
-- /See:/ 'newSecurityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | The ID of your security group.
    groupId :: Core.Maybe Core.Text,
    -- | The ID of the VPC with the referencing security group.
    referencingVpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroupReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnectionId', 'securityGroupReference_vpcPeeringConnectionId' - The ID of the VPC peering connection.
--
-- 'groupId', 'securityGroupReference_groupId' - The ID of your security group.
--
-- 'referencingVpcId', 'securityGroupReference_referencingVpcId' - The ID of the VPC with the referencing security group.
newSecurityGroupReference ::
  SecurityGroupReference
newSecurityGroupReference =
  SecurityGroupReference'
    { vpcPeeringConnectionId =
        Core.Nothing,
      groupId = Core.Nothing,
      referencingVpcId = Core.Nothing
    }

-- | The ID of the VPC peering connection.
securityGroupReference_vpcPeeringConnectionId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
securityGroupReference_vpcPeeringConnectionId = Lens.lens (\SecurityGroupReference' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@SecurityGroupReference' {} a -> s {vpcPeeringConnectionId = a} :: SecurityGroupReference)

-- | The ID of your security group.
securityGroupReference_groupId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
securityGroupReference_groupId = Lens.lens (\SecurityGroupReference' {groupId} -> groupId) (\s@SecurityGroupReference' {} a -> s {groupId = a} :: SecurityGroupReference)

-- | The ID of the VPC with the referencing security group.
securityGroupReference_referencingVpcId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
securityGroupReference_referencingVpcId = Lens.lens (\SecurityGroupReference' {referencingVpcId} -> referencingVpcId) (\s@SecurityGroupReference' {} a -> s {referencingVpcId = a} :: SecurityGroupReference)

instance Core.FromXML SecurityGroupReference where
  parseXML x =
    SecurityGroupReference'
      Core.<$> (x Core..@? "vpcPeeringConnectionId")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "referencingVpcId")

instance Core.Hashable SecurityGroupReference

instance Core.NFData SecurityGroupReference

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
-- Module      : Network.AWS.EC2.Types.SecurityGroupReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupReference where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC with a security group that references your security
-- group.
--
-- /See:/ 'newSecurityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of your security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC with the referencing security group.
    referencingVpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      groupId = Prelude.Nothing,
      referencingVpcId = Prelude.Nothing
    }

-- | The ID of the VPC peering connection.
securityGroupReference_vpcPeeringConnectionId :: Lens.Lens' SecurityGroupReference (Prelude.Maybe Prelude.Text)
securityGroupReference_vpcPeeringConnectionId = Lens.lens (\SecurityGroupReference' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@SecurityGroupReference' {} a -> s {vpcPeeringConnectionId = a} :: SecurityGroupReference)

-- | The ID of your security group.
securityGroupReference_groupId :: Lens.Lens' SecurityGroupReference (Prelude.Maybe Prelude.Text)
securityGroupReference_groupId = Lens.lens (\SecurityGroupReference' {groupId} -> groupId) (\s@SecurityGroupReference' {} a -> s {groupId = a} :: SecurityGroupReference)

-- | The ID of the VPC with the referencing security group.
securityGroupReference_referencingVpcId :: Lens.Lens' SecurityGroupReference (Prelude.Maybe Prelude.Text)
securityGroupReference_referencingVpcId = Lens.lens (\SecurityGroupReference' {referencingVpcId} -> referencingVpcId) (\s@SecurityGroupReference' {} a -> s {referencingVpcId = a} :: SecurityGroupReference)

instance Prelude.FromXML SecurityGroupReference where
  parseXML x =
    SecurityGroupReference'
      Prelude.<$> (x Prelude..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Prelude..@? "groupId")
      Prelude.<*> (x Prelude..@? "referencingVpcId")

instance Prelude.Hashable SecurityGroupReference

instance Prelude.NFData SecurityGroupReference

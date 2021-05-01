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
-- Module      : Network.AWS.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroup where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IpPermission
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | [VPC only] The outbound rules associated with the security group.
    ipPermissionsEgress :: Prelude.Maybe [IpPermission],
    -- | Any tags assigned to the security group.
    tags :: Prelude.Maybe [Tag],
    -- | The inbound rules associated with the security group.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | [VPC only] The ID of the VPC for the security group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the owner of the security group.
    ownerId :: Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Text,
    -- | A description of the security group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipPermissionsEgress', 'securityGroup_ipPermissionsEgress' - [VPC only] The outbound rules associated with the security group.
--
-- 'tags', 'securityGroup_tags' - Any tags assigned to the security group.
--
-- 'ipPermissions', 'securityGroup_ipPermissions' - The inbound rules associated with the security group.
--
-- 'vpcId', 'securityGroup_vpcId' - [VPC only] The ID of the VPC for the security group.
--
-- 'ownerId', 'securityGroup_ownerId' - The AWS account ID of the owner of the security group.
--
-- 'groupId', 'securityGroup_groupId' - The ID of the security group.
--
-- 'groupName', 'securityGroup_groupName' - The name of the security group.
--
-- 'description', 'securityGroup_description' - A description of the security group.
newSecurityGroup ::
  -- | 'ownerId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  SecurityGroup
newSecurityGroup
  pOwnerId_
  pGroupId_
  pGroupName_
  pDescription_ =
    SecurityGroup'
      { ipPermissionsEgress =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        ipPermissions = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        ownerId = pOwnerId_,
        groupId = pGroupId_,
        groupName = pGroupName_,
        description = pDescription_
      }

-- | [VPC only] The outbound rules associated with the security group.
securityGroup_ipPermissionsEgress :: Lens.Lens' SecurityGroup (Prelude.Maybe [IpPermission])
securityGroup_ipPermissionsEgress = Lens.lens (\SecurityGroup' {ipPermissionsEgress} -> ipPermissionsEgress) (\s@SecurityGroup' {} a -> s {ipPermissionsEgress = a} :: SecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags assigned to the security group.
securityGroup_tags :: Lens.Lens' SecurityGroup (Prelude.Maybe [Tag])
securityGroup_tags = Lens.lens (\SecurityGroup' {tags} -> tags) (\s@SecurityGroup' {} a -> s {tags = a} :: SecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The inbound rules associated with the security group.
securityGroup_ipPermissions :: Lens.Lens' SecurityGroup (Prelude.Maybe [IpPermission])
securityGroup_ipPermissions = Lens.lens (\SecurityGroup' {ipPermissions} -> ipPermissions) (\s@SecurityGroup' {} a -> s {ipPermissions = a} :: SecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | [VPC only] The ID of the VPC for the security group.
securityGroup_vpcId :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_vpcId = Lens.lens (\SecurityGroup' {vpcId} -> vpcId) (\s@SecurityGroup' {} a -> s {vpcId = a} :: SecurityGroup)

-- | The AWS account ID of the owner of the security group.
securityGroup_ownerId :: Lens.Lens' SecurityGroup Prelude.Text
securityGroup_ownerId = Lens.lens (\SecurityGroup' {ownerId} -> ownerId) (\s@SecurityGroup' {} a -> s {ownerId = a} :: SecurityGroup)

-- | The ID of the security group.
securityGroup_groupId :: Lens.Lens' SecurityGroup Prelude.Text
securityGroup_groupId = Lens.lens (\SecurityGroup' {groupId} -> groupId) (\s@SecurityGroup' {} a -> s {groupId = a} :: SecurityGroup)

-- | The name of the security group.
securityGroup_groupName :: Lens.Lens' SecurityGroup Prelude.Text
securityGroup_groupName = Lens.lens (\SecurityGroup' {groupName} -> groupName) (\s@SecurityGroup' {} a -> s {groupName = a} :: SecurityGroup)

-- | A description of the security group.
securityGroup_description :: Lens.Lens' SecurityGroup Prelude.Text
securityGroup_description = Lens.lens (\SecurityGroup' {description} -> description) (\s@SecurityGroup' {} a -> s {description = a} :: SecurityGroup)

instance Prelude.FromXML SecurityGroup where
  parseXML x =
    SecurityGroup'
      Prelude.<$> ( x Prelude..@? "ipPermissionsEgress"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "ipPermissions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpcId")
      Prelude.<*> (x Prelude..@ "ownerId")
      Prelude.<*> (x Prelude..@ "groupId")
      Prelude.<*> (x Prelude..@ "groupName")
      Prelude.<*> (x Prelude..@ "groupDescription")

instance Prelude.Hashable SecurityGroup

instance Prelude.NFData SecurityGroup

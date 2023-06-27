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
-- Module      : Amazonka.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpPermission
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a security group.
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | The inbound rules associated with the security group.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | [VPC only] The outbound rules associated with the security group.
    ipPermissionsEgress :: Prelude.Maybe [IpPermission],
    -- | Any tags assigned to the security group.
    tags :: Prelude.Maybe [Tag],
    -- | [VPC only] The ID of the VPC for the security group.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the security group.
    ownerId :: Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Text,
    -- | A description of the security group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipPermissions', 'securityGroup_ipPermissions' - The inbound rules associated with the security group.
--
-- 'ipPermissionsEgress', 'securityGroup_ipPermissionsEgress' - [VPC only] The outbound rules associated with the security group.
--
-- 'tags', 'securityGroup_tags' - Any tags assigned to the security group.
--
-- 'vpcId', 'securityGroup_vpcId' - [VPC only] The ID of the VPC for the security group.
--
-- 'ownerId', 'securityGroup_ownerId' - The Amazon Web Services account ID of the owner of the security group.
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
      { ipPermissions = Prelude.Nothing,
        ipPermissionsEgress = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        ownerId = pOwnerId_,
        groupId = pGroupId_,
        groupName = pGroupName_,
        description = pDescription_
      }

-- | The inbound rules associated with the security group.
securityGroup_ipPermissions :: Lens.Lens' SecurityGroup (Prelude.Maybe [IpPermission])
securityGroup_ipPermissions = Lens.lens (\SecurityGroup' {ipPermissions} -> ipPermissions) (\s@SecurityGroup' {} a -> s {ipPermissions = a} :: SecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | [VPC only] The outbound rules associated with the security group.
securityGroup_ipPermissionsEgress :: Lens.Lens' SecurityGroup (Prelude.Maybe [IpPermission])
securityGroup_ipPermissionsEgress = Lens.lens (\SecurityGroup' {ipPermissionsEgress} -> ipPermissionsEgress) (\s@SecurityGroup' {} a -> s {ipPermissionsEgress = a} :: SecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the security group.
securityGroup_tags :: Lens.Lens' SecurityGroup (Prelude.Maybe [Tag])
securityGroup_tags = Lens.lens (\SecurityGroup' {tags} -> tags) (\s@SecurityGroup' {} a -> s {tags = a} :: SecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | [VPC only] The ID of the VPC for the security group.
securityGroup_vpcId :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_vpcId = Lens.lens (\SecurityGroup' {vpcId} -> vpcId) (\s@SecurityGroup' {} a -> s {vpcId = a} :: SecurityGroup)

-- | The Amazon Web Services account ID of the owner of the security group.
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

instance Data.FromXML SecurityGroup where
  parseXML x =
    SecurityGroup'
      Prelude.<$> ( x
                      Data..@? "ipPermissions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "ipPermissionsEgress"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@ "ownerId")
      Prelude.<*> (x Data..@ "groupId")
      Prelude.<*> (x Data..@ "groupName")
      Prelude.<*> (x Data..@ "groupDescription")

instance Prelude.Hashable SecurityGroup where
  hashWithSalt _salt SecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` ipPermissions
      `Prelude.hashWithSalt` ipPermissionsEgress
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` description

instance Prelude.NFData SecurityGroup where
  rnf SecurityGroup' {..} =
    Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf ipPermissionsEgress
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf description

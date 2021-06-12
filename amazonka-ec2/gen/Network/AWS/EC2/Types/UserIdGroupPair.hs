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
-- Module      : Network.AWS.EC2.Types.UserIdGroupPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserIdGroupPair where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a security group and AWS account ID pair.
--
-- /See:/ 'newUserIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { -- | The ID of the VPC peering connection, if applicable.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | The name of the security group. In a request, use this parameter for a
    -- security group in EC2-Classic or a default VPC only. For a security
    -- group in a nondefault VPC, use the security group ID.
    --
    -- For a referenced security group in another VPC, this value is not
    -- returned if the referenced security group is deleted.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group.
    groupId :: Core.Maybe Core.Text,
    -- | The ID of an AWS account.
    --
    -- For a referenced security group in another VPC, the account ID of the
    -- referenced security group is returned in the response. If the referenced
    -- security group is deleted, this value is not returned.
    --
    -- [EC2-Classic] Required when adding or removing rules that reference a
    -- security group in another AWS account.
    userId :: Core.Maybe Core.Text,
    -- | The status of a VPC peering connection, if applicable.
    peeringStatus :: Core.Maybe Core.Text,
    -- | A description for the security group rule that references this user ID
    -- group pair.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
    description :: Core.Maybe Core.Text,
    -- | The ID of the VPC for the referenced security group, if applicable.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserIdGroupPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnectionId', 'userIdGroupPair_vpcPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
--
-- 'groupName', 'userIdGroupPair_groupName' - The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security
-- group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not
-- returned if the referenced security group is deleted.
--
-- 'groupId', 'userIdGroupPair_groupId' - The ID of the security group.
--
-- 'userId', 'userIdGroupPair_userId' - The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another AWS account.
--
-- 'peeringStatus', 'userIdGroupPair_peeringStatus' - The status of a VPC peering connection, if applicable.
--
-- 'description', 'userIdGroupPair_description' - A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
--
-- 'vpcId', 'userIdGroupPair_vpcId' - The ID of the VPC for the referenced security group, if applicable.
newUserIdGroupPair ::
  UserIdGroupPair
newUserIdGroupPair =
  UserIdGroupPair'
    { vpcPeeringConnectionId =
        Core.Nothing,
      groupName = Core.Nothing,
      groupId = Core.Nothing,
      userId = Core.Nothing,
      peeringStatus = Core.Nothing,
      description = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ID of the VPC peering connection, if applicable.
userIdGroupPair_vpcPeeringConnectionId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_vpcPeeringConnectionId = Lens.lens (\UserIdGroupPair' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@UserIdGroupPair' {} a -> s {vpcPeeringConnectionId = a} :: UserIdGroupPair)

-- | The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security
-- group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not
-- returned if the referenced security group is deleted.
userIdGroupPair_groupName :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_groupName = Lens.lens (\UserIdGroupPair' {groupName} -> groupName) (\s@UserIdGroupPair' {} a -> s {groupName = a} :: UserIdGroupPair)

-- | The ID of the security group.
userIdGroupPair_groupId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_groupId = Lens.lens (\UserIdGroupPair' {groupId} -> groupId) (\s@UserIdGroupPair' {} a -> s {groupId = a} :: UserIdGroupPair)

-- | The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another AWS account.
userIdGroupPair_userId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_userId = Lens.lens (\UserIdGroupPair' {userId} -> userId) (\s@UserIdGroupPair' {} a -> s {userId = a} :: UserIdGroupPair)

-- | The status of a VPC peering connection, if applicable.
userIdGroupPair_peeringStatus :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_peeringStatus = Lens.lens (\UserIdGroupPair' {peeringStatus} -> peeringStatus) (\s@UserIdGroupPair' {} a -> s {peeringStatus = a} :: UserIdGroupPair)

-- | A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
userIdGroupPair_description :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_description = Lens.lens (\UserIdGroupPair' {description} -> description) (\s@UserIdGroupPair' {} a -> s {description = a} :: UserIdGroupPair)

-- | The ID of the VPC for the referenced security group, if applicable.
userIdGroupPair_vpcId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
userIdGroupPair_vpcId = Lens.lens (\UserIdGroupPair' {vpcId} -> vpcId) (\s@UserIdGroupPair' {} a -> s {vpcId = a} :: UserIdGroupPair)

instance Core.FromXML UserIdGroupPair where
  parseXML x =
    UserIdGroupPair'
      Core.<$> (x Core..@? "vpcPeeringConnectionId")
      Core.<*> (x Core..@? "groupName")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "userId")
      Core.<*> (x Core..@? "peeringStatus")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "vpcId")

instance Core.Hashable UserIdGroupPair

instance Core.NFData UserIdGroupPair

instance Core.ToQuery UserIdGroupPair where
  toQuery UserIdGroupPair' {..} =
    Core.mconcat
      [ "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId,
        "GroupName" Core.=: groupName,
        "GroupId" Core.=: groupId,
        "UserId" Core.=: userId,
        "PeeringStatus" Core.=: peeringStatus,
        "Description" Core.=: description,
        "VpcId" Core.=: vpcId
      ]

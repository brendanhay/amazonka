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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group and Amazon Web Services account ID pair.
--
-- /See:/ 'newUserIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { -- | The ID of the VPC peering connection, if applicable.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the referenced security group, if applicable.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an Amazon Web Services account.
    --
    -- For a referenced security group in another VPC, the account ID of the
    -- referenced security group is returned in the response. If the referenced
    -- security group is deleted, this value is not returned.
    --
    -- [EC2-Classic] Required when adding or removing rules that reference a
    -- security group in another Amazon Web Services account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group. In a request, use this parameter for a
    -- security group in EC2-Classic or a default VPC only. For a security
    -- group in a nondefault VPC, use the security group ID.
    --
    -- For a referenced security group in another VPC, this value is not
    -- returned if the referenced security group is deleted.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A description for the security group rule that references this user ID
    -- group pair.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of a VPC peering connection, if applicable.
    peeringStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'vpcId', 'userIdGroupPair_vpcId' - The ID of the VPC for the referenced security group, if applicable.
--
-- 'userId', 'userIdGroupPair_userId' - The ID of an Amazon Web Services account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another Amazon Web Services account.
--
-- 'groupId', 'userIdGroupPair_groupId' - The ID of the security group.
--
-- 'groupName', 'userIdGroupPair_groupName' - The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security
-- group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not
-- returned if the referenced security group is deleted.
--
-- 'description', 'userIdGroupPair_description' - A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
--
-- 'peeringStatus', 'userIdGroupPair_peeringStatus' - The status of a VPC peering connection, if applicable.
newUserIdGroupPair ::
  UserIdGroupPair
newUserIdGroupPair =
  UserIdGroupPair'
    { vpcPeeringConnectionId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      userId = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      description = Prelude.Nothing,
      peeringStatus = Prelude.Nothing
    }

-- | The ID of the VPC peering connection, if applicable.
userIdGroupPair_vpcPeeringConnectionId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_vpcPeeringConnectionId = Lens.lens (\UserIdGroupPair' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@UserIdGroupPair' {} a -> s {vpcPeeringConnectionId = a} :: UserIdGroupPair)

-- | The ID of the VPC for the referenced security group, if applicable.
userIdGroupPair_vpcId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_vpcId = Lens.lens (\UserIdGroupPair' {vpcId} -> vpcId) (\s@UserIdGroupPair' {} a -> s {vpcId = a} :: UserIdGroupPair)

-- | The ID of an Amazon Web Services account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another Amazon Web Services account.
userIdGroupPair_userId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_userId = Lens.lens (\UserIdGroupPair' {userId} -> userId) (\s@UserIdGroupPair' {} a -> s {userId = a} :: UserIdGroupPair)

-- | The ID of the security group.
userIdGroupPair_groupId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_groupId = Lens.lens (\UserIdGroupPair' {groupId} -> groupId) (\s@UserIdGroupPair' {} a -> s {groupId = a} :: UserIdGroupPair)

-- | The name of the security group. In a request, use this parameter for a
-- security group in EC2-Classic or a default VPC only. For a security
-- group in a nondefault VPC, use the security group ID.
--
-- For a referenced security group in another VPC, this value is not
-- returned if the referenced security group is deleted.
userIdGroupPair_groupName :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_groupName = Lens.lens (\UserIdGroupPair' {groupName} -> groupName) (\s@UserIdGroupPair' {} a -> s {groupName = a} :: UserIdGroupPair)

-- | A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
userIdGroupPair_description :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_description = Lens.lens (\UserIdGroupPair' {description} -> description) (\s@UserIdGroupPair' {} a -> s {description = a} :: UserIdGroupPair)

-- | The status of a VPC peering connection, if applicable.
userIdGroupPair_peeringStatus :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_peeringStatus = Lens.lens (\UserIdGroupPair' {peeringStatus} -> peeringStatus) (\s@UserIdGroupPair' {} a -> s {peeringStatus = a} :: UserIdGroupPair)

instance Core.FromXML UserIdGroupPair where
  parseXML x =
    UserIdGroupPair'
      Prelude.<$> (x Core..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "userId")
      Prelude.<*> (x Core..@? "groupId")
      Prelude.<*> (x Core..@? "groupName")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "peeringStatus")

instance Prelude.Hashable UserIdGroupPair

instance Prelude.NFData UserIdGroupPair

instance Core.ToQuery UserIdGroupPair where
  toQuery UserIdGroupPair' {..} =
    Prelude.mconcat
      [ "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId,
        "VpcId" Core.=: vpcId,
        "UserId" Core.=: userId,
        "GroupId" Core.=: groupId,
        "GroupName" Core.=: groupName,
        "Description" Core.=: description,
        "PeeringStatus" Core.=: peeringStatus
      ]

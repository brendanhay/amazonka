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
-- Module      : Amazonka.EC2.Types.UserIdGroupPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UserIdGroupPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a security group and Amazon Web Services account ID pair.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newUserIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { -- | A description for the security group rule that references this user ID
    -- group pair.
    --
    -- Constraints: Up to 255 characters in length. Allowed characters are a-z,
    -- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group. In a request, use this parameter for a
    -- security group in EC2-Classic or a default VPC only. For a security
    -- group in a nondefault VPC, use the security group ID.
    --
    -- For a referenced security group in another VPC, this value is not
    -- returned if the referenced security group is deleted.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The status of a VPC peering connection, if applicable.
    peeringStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of an Amazon Web Services account.
    --
    -- For a referenced security group in another VPC, the account ID of the
    -- referenced security group is returned in the response. If the referenced
    -- security group is deleted, this value is not returned.
    --
    -- [EC2-Classic] Required when adding or removing rules that reference a
    -- security group in another Amazon Web Services account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the referenced security group, if applicable.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC peering connection, if applicable.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
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
-- 'description', 'userIdGroupPair_description' - A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
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
-- 'peeringStatus', 'userIdGroupPair_peeringStatus' - The status of a VPC peering connection, if applicable.
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
-- 'vpcId', 'userIdGroupPair_vpcId' - The ID of the VPC for the referenced security group, if applicable.
--
-- 'vpcPeeringConnectionId', 'userIdGroupPair_vpcPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
newUserIdGroupPair ::
  UserIdGroupPair
newUserIdGroupPair =
  UserIdGroupPair'
    { description = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      peeringStatus = Prelude.Nothing,
      userId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing
    }

-- | A description for the security group rule that references this user ID
-- group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z,
-- A-Z, 0-9, spaces, and ._-:\/()#,\@[]+=;{}!$*
userIdGroupPair_description :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_description = Lens.lens (\UserIdGroupPair' {description} -> description) (\s@UserIdGroupPair' {} a -> s {description = a} :: UserIdGroupPair)

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

-- | The status of a VPC peering connection, if applicable.
userIdGroupPair_peeringStatus :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_peeringStatus = Lens.lens (\UserIdGroupPair' {peeringStatus} -> peeringStatus) (\s@UserIdGroupPair' {} a -> s {peeringStatus = a} :: UserIdGroupPair)

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

-- | The ID of the VPC for the referenced security group, if applicable.
userIdGroupPair_vpcId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_vpcId = Lens.lens (\UserIdGroupPair' {vpcId} -> vpcId) (\s@UserIdGroupPair' {} a -> s {vpcId = a} :: UserIdGroupPair)

-- | The ID of the VPC peering connection, if applicable.
userIdGroupPair_vpcPeeringConnectionId :: Lens.Lens' UserIdGroupPair (Prelude.Maybe Prelude.Text)
userIdGroupPair_vpcPeeringConnectionId = Lens.lens (\UserIdGroupPair' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@UserIdGroupPair' {} a -> s {vpcPeeringConnectionId = a} :: UserIdGroupPair)

instance Data.FromXML UserIdGroupPair where
  parseXML x =
    UserIdGroupPair'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "groupId")
      Prelude.<*> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "peeringStatus")
      Prelude.<*> (x Data..@? "userId")
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@? "vpcPeeringConnectionId")

instance Prelude.Hashable UserIdGroupPair where
  hashWithSalt _salt UserIdGroupPair' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` peeringStatus
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcPeeringConnectionId

instance Prelude.NFData UserIdGroupPair where
  rnf UserIdGroupPair' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf peeringStatus
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId

instance Data.ToQuery UserIdGroupPair where
  toQuery UserIdGroupPair' {..} =
    Prelude.mconcat
      [ "Description" Data.=: description,
        "GroupId" Data.=: groupId,
        "GroupName" Data.=: groupName,
        "PeeringStatus" Data.=: peeringStatus,
        "UserId" Data.=: userId,
        "VpcId" Data.=: vpcId,
        "VpcPeeringConnectionId"
          Data.=: vpcPeeringConnectionId
      ]

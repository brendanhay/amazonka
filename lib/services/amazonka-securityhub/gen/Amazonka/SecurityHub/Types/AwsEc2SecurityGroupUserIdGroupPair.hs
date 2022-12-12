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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2SecurityGroupUserIdGroupPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2SecurityGroupUserIdGroupPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A relationship between a security group and a user.
--
-- /See:/ 'newAwsEc2SecurityGroupUserIdGroupPair' smart constructor.
data AwsEc2SecurityGroupUserIdGroupPair = AwsEc2SecurityGroupUserIdGroupPair'
  { -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group.
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
    -- security group in another VPC.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the referenced security group, if applicable.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC peering connection, if applicable.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2SecurityGroupUserIdGroupPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'awsEc2SecurityGroupUserIdGroupPair_groupId' - The ID of the security group.
--
-- 'groupName', 'awsEc2SecurityGroupUserIdGroupPair_groupName' - The name of the security group.
--
-- 'peeringStatus', 'awsEc2SecurityGroupUserIdGroupPair_peeringStatus' - The status of a VPC peering connection, if applicable.
--
-- 'userId', 'awsEc2SecurityGroupUserIdGroupPair_userId' - The ID of an Amazon Web Services account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another VPC.
--
-- 'vpcId', 'awsEc2SecurityGroupUserIdGroupPair_vpcId' - The ID of the VPC for the referenced security group, if applicable.
--
-- 'vpcPeeringConnectionId', 'awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId' - The ID of the VPC peering connection, if applicable.
newAwsEc2SecurityGroupUserIdGroupPair ::
  AwsEc2SecurityGroupUserIdGroupPair
newAwsEc2SecurityGroupUserIdGroupPair =
  AwsEc2SecurityGroupUserIdGroupPair'
    { groupId =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      peeringStatus = Prelude.Nothing,
      userId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcPeeringConnectionId =
        Prelude.Nothing
    }

-- | The ID of the security group.
awsEc2SecurityGroupUserIdGroupPair_groupId :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_groupId = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {groupId} -> groupId) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {groupId = a} :: AwsEc2SecurityGroupUserIdGroupPair)

-- | The name of the security group.
awsEc2SecurityGroupUserIdGroupPair_groupName :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_groupName = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {groupName} -> groupName) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {groupName = a} :: AwsEc2SecurityGroupUserIdGroupPair)

-- | The status of a VPC peering connection, if applicable.
awsEc2SecurityGroupUserIdGroupPair_peeringStatus :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_peeringStatus = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {peeringStatus} -> peeringStatus) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {peeringStatus = a} :: AwsEc2SecurityGroupUserIdGroupPair)

-- | The ID of an Amazon Web Services account.
--
-- For a referenced security group in another VPC, the account ID of the
-- referenced security group is returned in the response. If the referenced
-- security group is deleted, this value is not returned.
--
-- [EC2-Classic] Required when adding or removing rules that reference a
-- security group in another VPC.
awsEc2SecurityGroupUserIdGroupPair_userId :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_userId = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {userId} -> userId) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {userId = a} :: AwsEc2SecurityGroupUserIdGroupPair)

-- | The ID of the VPC for the referenced security group, if applicable.
awsEc2SecurityGroupUserIdGroupPair_vpcId :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_vpcId = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {vpcId} -> vpcId) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {vpcId = a} :: AwsEc2SecurityGroupUserIdGroupPair)

-- | The ID of the VPC peering connection, if applicable.
awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId :: Lens.Lens' AwsEc2SecurityGroupUserIdGroupPair (Prelude.Maybe Prelude.Text)
awsEc2SecurityGroupUserIdGroupPair_vpcPeeringConnectionId = Lens.lens (\AwsEc2SecurityGroupUserIdGroupPair' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AwsEc2SecurityGroupUserIdGroupPair' {} a -> s {vpcPeeringConnectionId = a} :: AwsEc2SecurityGroupUserIdGroupPair)

instance
  Data.FromJSON
    AwsEc2SecurityGroupUserIdGroupPair
  where
  parseJSON =
    Data.withObject
      "AwsEc2SecurityGroupUserIdGroupPair"
      ( \x ->
          AwsEc2SecurityGroupUserIdGroupPair'
            Prelude.<$> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "PeeringStatus")
            Prelude.<*> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "VpcPeeringConnectionId")
      )

instance
  Prelude.Hashable
    AwsEc2SecurityGroupUserIdGroupPair
  where
  hashWithSalt
    _salt
    AwsEc2SecurityGroupUserIdGroupPair' {..} =
      _salt `Prelude.hashWithSalt` groupId
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` peeringStatus
        `Prelude.hashWithSalt` userId
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` vpcPeeringConnectionId

instance
  Prelude.NFData
    AwsEc2SecurityGroupUserIdGroupPair
  where
  rnf AwsEc2SecurityGroupUserIdGroupPair' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf peeringStatus
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId

instance
  Data.ToJSON
    AwsEc2SecurityGroupUserIdGroupPair
  where
  toJSON AwsEc2SecurityGroupUserIdGroupPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupId" Data..=) Prelude.<$> groupId,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("PeeringStatus" Data..=) Prelude.<$> peeringStatus,
            ("UserId" Data..=) Prelude.<$> userId,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            ("VpcPeeringConnectionId" Data..=)
              Prelude.<$> vpcPeeringConnectionId
          ]
      )

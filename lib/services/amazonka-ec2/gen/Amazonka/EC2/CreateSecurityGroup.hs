{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security group.
--
-- A security group acts as a virtual firewall for your instance to control
-- inbound and outbound traffic. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 security groups>
-- in the /Amazon Elastic Compute Cloud User Guide/ and
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security groups for your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- When you create a security group, you specify a friendly name of your
-- choice. You can have a security group for use in EC2-Classic with the
-- same name as a security group for use in a VPC. However, you can\'t have
-- two security groups for use in EC2-Classic with the same name or two
-- security groups for use in a VPC with the same name.
--
-- You have a default security group for use in EC2-Classic and a default
-- security group for use in your VPC. If you don\'t specify a security
-- group when you launch an instance, the instance is launched into the
-- appropriate default security group. A default security group includes a
-- default rule that grants instances unrestricted network access to each
-- other.
--
-- You can add or remove rules from your security groups using
-- AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress.
--
-- For more information about VPC security group limits, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits>.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateSecurityGroup
  ( -- * Creating a Request
    CreateSecurityGroup (..),
    newCreateSecurityGroup,

    -- * Request Lenses
    createSecurityGroup_dryRun,
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_vpcId,
    createSecurityGroup_description,
    createSecurityGroup_groupName,

    -- * Destructuring the Response
    CreateSecurityGroupResponse (..),
    newCreateSecurityGroupResponse,

    -- * Response Lenses
    createSecurityGroupResponse_tags,
    createSecurityGroupResponse_httpStatus,
    createSecurityGroupResponse_groupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the security group.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A description for the security group. This is informational only.
    --
    -- Constraints: Up to 255 characters in length
    --
    -- Constraints for EC2-Classic: ASCII characters
    --
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
    -- ._-:\/()#,\@[]+=&;{}!$*
    description :: Prelude.Text,
    -- | The name of the security group.
    --
    -- Constraints: Up to 255 characters in length. Cannot start with @sg-@.
    --
    -- Constraints for EC2-Classic: ASCII characters
    --
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
    -- ._-:\/()#,\@[]+=&;{}!$*
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createSecurityGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createSecurityGroup_tagSpecifications' - The tags to assign to the security group.
--
-- 'vpcId', 'createSecurityGroup_vpcId' - [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
--
-- 'description', 'createSecurityGroup_description' - A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
--
-- 'groupName', 'createSecurityGroup_groupName' - The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@.
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
newCreateSecurityGroup ::
  -- | 'description'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  CreateSecurityGroup
newCreateSecurityGroup pDescription_ pGroupName_ =
  CreateSecurityGroup'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      description = pDescription_,
      groupName = pGroupName_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSecurityGroup_dryRun :: Lens.Lens' CreateSecurityGroup (Prelude.Maybe Prelude.Bool)
createSecurityGroup_dryRun = Lens.lens (\CreateSecurityGroup' {dryRun} -> dryRun) (\s@CreateSecurityGroup' {} a -> s {dryRun = a} :: CreateSecurityGroup)

-- | The tags to assign to the security group.
createSecurityGroup_tagSpecifications :: Lens.Lens' CreateSecurityGroup (Prelude.Maybe [TagSpecification])
createSecurityGroup_tagSpecifications = Lens.lens (\CreateSecurityGroup' {tagSpecifications} -> tagSpecifications) (\s@CreateSecurityGroup' {} a -> s {tagSpecifications = a} :: CreateSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
createSecurityGroup_vpcId :: Lens.Lens' CreateSecurityGroup (Prelude.Maybe Prelude.Text)
createSecurityGroup_vpcId = Lens.lens (\CreateSecurityGroup' {vpcId} -> vpcId) (\s@CreateSecurityGroup' {} a -> s {vpcId = a} :: CreateSecurityGroup)

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
createSecurityGroup_description :: Lens.Lens' CreateSecurityGroup Prelude.Text
createSecurityGroup_description = Lens.lens (\CreateSecurityGroup' {description} -> description) (\s@CreateSecurityGroup' {} a -> s {description = a} :: CreateSecurityGroup)

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@.
--
-- Constraints for EC2-Classic: ASCII characters
--
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and
-- ._-:\/()#,\@[]+=&;{}!$*
createSecurityGroup_groupName :: Lens.Lens' CreateSecurityGroup Prelude.Text
createSecurityGroup_groupName = Lens.lens (\CreateSecurityGroup' {groupName} -> groupName) (\s@CreateSecurityGroup' {} a -> s {groupName = a} :: CreateSecurityGroup)

instance Core.AWSRequest CreateSecurityGroup where
  type
    AWSResponse CreateSecurityGroup =
      CreateSecurityGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSecurityGroupResponse'
            Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "groupId")
      )

instance Prelude.Hashable CreateSecurityGroup where
  hashWithSalt _salt CreateSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData CreateSecurityGroup where
  rnf CreateSecurityGroup' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders CreateSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSecurityGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSecurityGroup where
  toQuery CreateSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateSecurityGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "VpcId" Data.=: vpcId,
        "GroupDescription" Data.=: description,
        "GroupName" Data.=: groupName
      ]

-- | /See:/ 'newCreateSecurityGroupResponse' smart constructor.
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
  { -- | The tags assigned to the security group.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the security group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSecurityGroupResponse_tags' - The tags assigned to the security group.
--
-- 'httpStatus', 'createSecurityGroupResponse_httpStatus' - The response's http status code.
--
-- 'groupId', 'createSecurityGroupResponse_groupId' - The ID of the security group.
newCreateSecurityGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'groupId'
  Prelude.Text ->
  CreateSecurityGroupResponse
newCreateSecurityGroupResponse pHttpStatus_ pGroupId_ =
  CreateSecurityGroupResponse'
    { tags =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      groupId = pGroupId_
    }

-- | The tags assigned to the security group.
createSecurityGroupResponse_tags :: Lens.Lens' CreateSecurityGroupResponse (Prelude.Maybe [Tag])
createSecurityGroupResponse_tags = Lens.lens (\CreateSecurityGroupResponse' {tags} -> tags) (\s@CreateSecurityGroupResponse' {} a -> s {tags = a} :: CreateSecurityGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSecurityGroupResponse_httpStatus :: Lens.Lens' CreateSecurityGroupResponse Prelude.Int
createSecurityGroupResponse_httpStatus = Lens.lens (\CreateSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityGroupResponse' {} a -> s {httpStatus = a} :: CreateSecurityGroupResponse)

-- | The ID of the security group.
createSecurityGroupResponse_groupId :: Lens.Lens' CreateSecurityGroupResponse Prelude.Text
createSecurityGroupResponse_groupId = Lens.lens (\CreateSecurityGroupResponse' {groupId} -> groupId) (\s@CreateSecurityGroupResponse' {} a -> s {groupId = a} :: CreateSecurityGroupResponse)

instance Prelude.NFData CreateSecurityGroupResponse where
  rnf CreateSecurityGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf groupId

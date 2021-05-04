{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security group.
--
-- A security group acts as a virtual firewall for your instance to control
-- inbound and outbound traffic. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups>
-- in the /Amazon Elastic Compute Cloud User Guide/ and
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
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
module Network.AWS.EC2.CreateSecurityGroup
  ( -- * Creating a Request
    CreateSecurityGroup (..),
    newCreateSecurityGroup,

    -- * Request Lenses
    createSecurityGroup_tagSpecifications,
    createSecurityGroup_dryRun,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { -- | The tags to assign to the security group.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createSecurityGroup_tagSpecifications' - The tags to assign to the security group.
--
-- 'dryRun', 'createSecurityGroup_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      description = pDescription_,
      groupName = pGroupName_
    }

-- | The tags to assign to the security group.
createSecurityGroup_tagSpecifications :: Lens.Lens' CreateSecurityGroup (Prelude.Maybe [TagSpecification])
createSecurityGroup_tagSpecifications = Lens.lens (\CreateSecurityGroup' {tagSpecifications} -> tagSpecifications) (\s@CreateSecurityGroup' {} a -> s {tagSpecifications = a} :: CreateSecurityGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSecurityGroup_dryRun :: Lens.Lens' CreateSecurityGroup (Prelude.Maybe Prelude.Bool)
createSecurityGroup_dryRun = Lens.lens (\CreateSecurityGroup' {dryRun} -> dryRun) (\s@CreateSecurityGroup' {} a -> s {dryRun = a} :: CreateSecurityGroup)

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

instance Prelude.AWSRequest CreateSecurityGroup where
  type
    Rs CreateSecurityGroup =
      CreateSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSecurityGroupResponse'
            Prelude.<$> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "groupId")
      )

instance Prelude.Hashable CreateSecurityGroup

instance Prelude.NFData CreateSecurityGroup

instance Prelude.ToHeaders CreateSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateSecurityGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSecurityGroup where
  toQuery CreateSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateSecurityGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "VpcId" Prelude.=: vpcId,
        "GroupDescription" Prelude.=: description,
        "GroupName" Prelude.=: groupName
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
createSecurityGroupResponse_tags = Lens.lens (\CreateSecurityGroupResponse' {tags} -> tags) (\s@CreateSecurityGroupResponse' {} a -> s {tags = a} :: CreateSecurityGroupResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
createSecurityGroupResponse_httpStatus :: Lens.Lens' CreateSecurityGroupResponse Prelude.Int
createSecurityGroupResponse_httpStatus = Lens.lens (\CreateSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityGroupResponse' {} a -> s {httpStatus = a} :: CreateSecurityGroupResponse)

-- | The ID of the security group.
createSecurityGroupResponse_groupId :: Lens.Lens' CreateSecurityGroupResponse Prelude.Text
createSecurityGroupResponse_groupId = Lens.lens (\CreateSecurityGroupResponse' {groupId} -> groupId) (\s@CreateSecurityGroupResponse' {} a -> s {groupId = a} :: CreateSecurityGroupResponse)

instance Prelude.NFData CreateSecurityGroupResponse

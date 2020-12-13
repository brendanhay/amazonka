{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security group.
--
-- A security group acts as a virtual firewall for your instance to control inbound and outbound traffic. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ and <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
-- When you create a security group, you specify a friendly name of your choice. You can have a security group for use in EC2-Classic with the same name as a security group for use in a VPC. However, you can't have two security groups for use in EC2-Classic with the same name or two security groups for use in a VPC with the same name.
-- You have a default security group for use in EC2-Classic and a default security group for use in your VPC. If you don't specify a security group when you launch an instance, the instance is launched into the appropriate default security group. A default security group includes a default rule that grants instances unrestricted network access to each other.
-- You can add or remove rules from your security groups using 'AuthorizeSecurityGroupIngress' , 'AuthorizeSecurityGroupEgress' , 'RevokeSecurityGroupIngress' , and 'RevokeSecurityGroupEgress' .
-- For more information about VPC security group limits, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html Amazon VPC Limits> .
module Network.AWS.EC2.CreateSecurityGroup
  ( -- * Creating a request
    CreateSecurityGroup (..),
    mkCreateSecurityGroup,

    -- ** Request lenses
    csgfVPCId,
    csgfTagSpecifications,
    csgfGroupName,
    csgfDescription,
    csgfDryRun,

    -- * Destructuring the response
    CreateSecurityGroupResponse (..),
    mkCreateSecurityGroupResponse,

    -- ** Response lenses
    csgrsGroupId,
    csgrsTags,
    csgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { -- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The tags to assign to the security group.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The name of the security group.
    --
    -- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
    -- Constraints for EC2-Classic: ASCII characters
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
    groupName :: Lude.Text,
    -- | A description for the security group. This is informational only.
    --
    -- Constraints: Up to 255 characters in length
    -- Constraints for EC2-Classic: ASCII characters
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
    description :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityGroup' with the minimum fields required to make a request.
--
-- * 'vpcId' - [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
-- * 'tagSpecifications' - The tags to assign to the security group.
-- * 'groupName' - The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
-- * 'description' - A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateSecurityGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateSecurityGroup
mkCreateSecurityGroup pGroupName_ pDescription_ =
  CreateSecurityGroup'
    { vpcId = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      groupName = pGroupName_,
      description = pDescription_,
      dryRun = Lude.Nothing
    }

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfVPCId :: Lens.Lens' CreateSecurityGroup (Lude.Maybe Lude.Text)
csgfVPCId = Lens.lens (vpcId :: CreateSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateSecurityGroup)
{-# DEPRECATED csgfVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The tags to assign to the security group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfTagSpecifications :: Lens.Lens' CreateSecurityGroup (Lude.Maybe [TagSpecification])
csgfTagSpecifications = Lens.lens (tagSpecifications :: CreateSecurityGroup -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSecurityGroup)
{-# DEPRECATED csgfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfGroupName :: Lens.Lens' CreateSecurityGroup Lude.Text
csgfGroupName = Lens.lens (groupName :: CreateSecurityGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateSecurityGroup)
{-# DEPRECATED csgfGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDescription :: Lens.Lens' CreateSecurityGroup Lude.Text
csgfDescription = Lens.lens (description :: CreateSecurityGroup -> Lude.Text) (\s a -> s {description = a} :: CreateSecurityGroup)
{-# DEPRECATED csgfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDryRun :: Lens.Lens' CreateSecurityGroup (Lude.Maybe Lude.Bool)
csgfDryRun = Lens.lens (dryRun :: CreateSecurityGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSecurityGroup)
{-# DEPRECATED csgfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateSecurityGroup where
  type Rs CreateSecurityGroup = CreateSecurityGroupResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateSecurityGroupResponse'
            Lude.<$> (x Lude..@ "groupId")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSecurityGroup where
  toQuery CreateSecurityGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "GroupName" Lude.=: groupName,
        "GroupDescription" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateSecurityGroupResponse' smart constructor.
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
  { -- | The ID of the security group.
    groupId :: Lude.Text,
    -- | The tags assigned to the security group.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the security group.
-- * 'tags' - The tags assigned to the security group.
-- * 'responseStatus' - The response status code.
mkCreateSecurityGroupResponse ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateSecurityGroupResponse
mkCreateSecurityGroupResponse pGroupId_ pResponseStatus_ =
  CreateSecurityGroupResponse'
    { groupId = pGroupId_,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsGroupId :: Lens.Lens' CreateSecurityGroupResponse Lude.Text
csgrsGroupId = Lens.lens (groupId :: CreateSecurityGroupResponse -> Lude.Text) (\s a -> s {groupId = a} :: CreateSecurityGroupResponse)
{-# DEPRECATED csgrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The tags assigned to the security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsTags :: Lens.Lens' CreateSecurityGroupResponse (Lude.Maybe [Tag])
csgrsTags = Lens.lens (tags :: CreateSecurityGroupResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSecurityGroupResponse)
{-# DEPRECATED csgrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsResponseStatus :: Lens.Lens' CreateSecurityGroupResponse Lude.Int
csgrsResponseStatus = Lens.lens (responseStatus :: CreateSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSecurityGroupResponse)
{-# DEPRECATED csgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csgVPCId,
    csgTagSpecifications,
    csgDryRun,
    csgDescription,
    csgGroupName,

    -- * Destructuring the response
    CreateSecurityGroupResponse (..),
    mkCreateSecurityGroupResponse,

    -- ** Response lenses
    csgrsTags,
    csgrsResponseStatus,
    csgrsGroupId,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { vpcId ::
      Lude.Maybe Lude.Text,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    description :: Lude.Text,
    groupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupName' - The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
-- * 'tagSpecifications' - The tags to assign to the security group.
-- * 'vpcId' - [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
mkCreateSecurityGroup ::
  -- | 'description'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  CreateSecurityGroup
mkCreateSecurityGroup pDescription_ pGroupName_ =
  CreateSecurityGroup'
    { vpcId = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      description = pDescription_,
      groupName = pGroupName_
    }

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVPCId :: Lens.Lens' CreateSecurityGroup (Lude.Maybe Lude.Text)
csgVPCId = Lens.lens (vpcId :: CreateSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateSecurityGroup)
{-# DEPRECATED csgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The tags to assign to the security group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTagSpecifications :: Lens.Lens' CreateSecurityGroup (Lude.Maybe [TagSpecification])
csgTagSpecifications = Lens.lens (tagSpecifications :: CreateSecurityGroup -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSecurityGroup)
{-# DEPRECATED csgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDryRun :: Lens.Lens' CreateSecurityGroup (Lude.Maybe Lude.Bool)
csgDryRun = Lens.lens (dryRun :: CreateSecurityGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSecurityGroup)
{-# DEPRECATED csgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CreateSecurityGroup Lude.Text
csgDescription = Lens.lens (description :: CreateSecurityGroup -> Lude.Text) (\s a -> s {description = a} :: CreateSecurityGroup)
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgGroupName :: Lens.Lens' CreateSecurityGroup Lude.Text
csgGroupName = Lens.lens (groupName :: CreateSecurityGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateSecurityGroup)
{-# DEPRECATED csgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest CreateSecurityGroup where
  type Rs CreateSecurityGroup = CreateSecurityGroupResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateSecurityGroupResponse'
            Lude.<$> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "groupId")
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
        "DryRun" Lude.=: dryRun,
        "GroupDescription" Lude.=: description,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkCreateSecurityGroupResponse' smart constructor.
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
  { tags ::
      Lude.Maybe [Tag],
    responseStatus :: Lude.Int,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the security group.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags assigned to the security group.
mkCreateSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'groupId'
  Lude.Text ->
  CreateSecurityGroupResponse
mkCreateSecurityGroupResponse pResponseStatus_ pGroupId_ =
  CreateSecurityGroupResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_,
      groupId = pGroupId_
    }

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

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsGroupId :: Lens.Lens' CreateSecurityGroupResponse Lude.Text
csgrsGroupId = Lens.lens (groupId :: CreateSecurityGroupResponse -> Lude.Text) (\s a -> s {groupId = a} :: CreateSecurityGroupResponse)
{-# DEPRECATED csgrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

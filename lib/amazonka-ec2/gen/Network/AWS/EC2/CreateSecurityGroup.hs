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
    csgfDescription,
    csgfGroupName,
    csgfDryRun,
    csgfTagSpecifications,
    csgfVpcId,

    -- * Destructuring the response
    CreateSecurityGroupResponse (..),
    mkCreateSecurityGroupResponse,

    -- ** Response lenses
    csgrrsGroupId,
    csgrrsTags,
    csgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSecurityGroup' smart constructor.
data CreateSecurityGroup = CreateSecurityGroup'
  { -- | A description for the security group. This is informational only.
    --
    -- Constraints: Up to 255 characters in length
    -- Constraints for EC2-Classic: ASCII characters
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
    description :: Types.String,
    -- | The name of the security group.
    --
    -- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
    -- Constraints for EC2-Classic: ASCII characters
    -- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
    groupName :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to assign to the security group.
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityGroup' value with any optional fields omitted.
mkCreateSecurityGroup ::
  -- | 'description'
  Types.String ->
  -- | 'groupName'
  Types.String ->
  CreateSecurityGroup
mkCreateSecurityGroup description groupName =
  CreateSecurityGroup'
    { description,
      groupName,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A description for the security group. This is informational only.
--
-- Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDescription :: Lens.Lens' CreateSecurityGroup Types.String
csgfDescription = Lens.field @"description"
{-# DEPRECATED csgfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the security group.
--
-- Constraints: Up to 255 characters in length. Cannot start with @sg-@ .
-- Constraints for EC2-Classic: ASCII characters
-- Constraints for EC2-VPC: a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfGroupName :: Lens.Lens' CreateSecurityGroup Types.String
csgfGroupName = Lens.field @"groupName"
{-# DEPRECATED csgfGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfDryRun :: Lens.Lens' CreateSecurityGroup (Core.Maybe Core.Bool)
csgfDryRun = Lens.field @"dryRun"
{-# DEPRECATED csgfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to assign to the security group.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfTagSpecifications :: Lens.Lens' CreateSecurityGroup (Core.Maybe [Types.TagSpecification])
csgfTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED csgfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | [EC2-VPC] The ID of the VPC. Required for EC2-VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgfVpcId :: Lens.Lens' CreateSecurityGroup (Core.Maybe Types.VpcId)
csgfVpcId = Lens.field @"vpcId"
{-# DEPRECATED csgfVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.AWSRequest CreateSecurityGroup where
  type Rs CreateSecurityGroup = CreateSecurityGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateSecurityGroup")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "GroupDescription" description)
                Core.<> (Core.toQueryValue "GroupName" groupName)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
                Core.<> (Core.toQueryValue "VpcId" Core.<$> vpcId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSecurityGroupResponse'
            Core.<$> (x Core..@ "groupId")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSecurityGroupResponse' smart constructor.
data CreateSecurityGroupResponse = CreateSecurityGroupResponse'
  { -- | The ID of the security group.
    groupId :: Types.String,
    -- | The tags assigned to the security group.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSecurityGroupResponse' value with any optional fields omitted.
mkCreateSecurityGroupResponse ::
  -- | 'groupId'
  Types.String ->
  -- | 'responseStatus'
  Core.Int ->
  CreateSecurityGroupResponse
mkCreateSecurityGroupResponse groupId responseStatus =
  CreateSecurityGroupResponse'
    { groupId,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsGroupId :: Lens.Lens' CreateSecurityGroupResponse Types.String
csgrrsGroupId = Lens.field @"groupId"
{-# DEPRECATED csgrrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The tags assigned to the security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsTags :: Lens.Lens' CreateSecurityGroupResponse (Core.Maybe [Types.Tag])
csgrrsTags = Lens.field @"tags"
{-# DEPRECATED csgrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsResponseStatus :: Lens.Lens' CreateSecurityGroupResponse Core.Int
csgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

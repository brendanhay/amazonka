{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateMountTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mount target for a file system. You can then mount the file system on EC2 instances by using the mount target.
--
-- You can create one mount target in each Availability Zone in your VPC. All EC2 instances in a VPC within a given Availability Zone share a single mount target for a given file system. If you have multiple subnets in an Availability Zone, you create a mount target in one of the subnets. EC2 instances do not need to be in the same subnet as the mount target in order to access their file system. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works> .
-- In the request, you also specify a file system ID for which you are creating the mount target and the file system's lifecycle state must be @available@ . For more information, see 'DescribeFileSystems' .
-- In the request, you also provide a subnet ID, which determines the following:
--
--     * VPC in which Amazon EFS creates the mount target
--
--
--     * Availability Zone in which Amazon EFS creates the mount target
--
--
--     * IP address range from which Amazon EFS selects the IP address of the mount target (if you don't specify an IP address in the request)
--
--
-- After creating the mount target, Amazon EFS returns a response that includes, a @MountTargetId@ and an @IpAddress@ . You use this IP address when mounting the file system in an EC2 instance. You can also use the mount target's DNS name when mounting the file system. The EC2 instance on which you mount the file system by using the mount target can resolve the mount target's DNS name to its IP address. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation How it Works: Implementation Overview> .
-- Note that you can create mount targets for a file system in only one VPC, and there can be only one mount target per Availability Zone. That is, if the file system already has one or more mount targets created for it, the subnet specified in the request to add another mount target must meet the following requirements:
--
--     * Must belong to the same VPC as the subnets of the existing mount targets
--
--
--     * Must not be in the same Availability Zone as any of the subnets of the existing mount targets
--
--
-- If the request satisfies the requirements, Amazon EFS does the following:
--
--     * Creates a new mount target in the specified subnet.
--
--
--     * Also creates a new network interface in the subnet as follows:
--
--     * If the request provides an @IpAddress@ , Amazon EFS assigns that IP address to the network interface. Otherwise, Amazon EFS assigns a free address in the subnet (in the same way that the Amazon EC2 @CreateNetworkInterface@ call does when a request does not specify a primary private IP address).
--
--
--     * If the request provides @SecurityGroups@ , this network interface is associated with those security groups. Otherwise, it belongs to the default security group for the subnet's VPC.
--
--
--     * Assigns the description @Mount target /fsmt-id/ for file system /fs-id/ @ where @/fsmt-id/ @ is the mount target ID, and @/fs-id/ @ is the @FileSystemId@ .
--
--
--     * Sets the @requesterManaged@ property of the network interface to @true@ , and the @requesterId@ value to @EFS@ .
--
--
-- Each Amazon EFS mount target has one corresponding requester-managed EC2 network interface. After the network interface is created, Amazon EFS sets the @NetworkInterfaceId@ field in the mount target's description to the network interface ID, and the @IpAddress@ field to its address. If network interface creation fails, the entire @CreateMountTarget@ operation fails.
--
--
-- We recommend that you create a mount target in each of the Availability Zones. There are cost considerations for using a file system in an Availability Zone through a mount target created in another Availability Zone. For more information, see <http://aws.amazon.com/efs/ Amazon EFS> . In addition, by always using a mount target local to the instance's Availability Zone, you eliminate a partial failure scenario. If the Availability Zone in which your mount target is created goes down, then you can't access your file system through that mount target.
-- This operation requires permissions for the following action on the file system:
--
--     * @elasticfilesystem:CreateMountTarget@
--
--
-- This operation also requires permissions for the following Amazon EC2 actions:
--
--     * @ec2:DescribeSubnets@
--
--
--     * @ec2:DescribeNetworkInterfaces@
--
--
--     * @ec2:CreateNetworkInterface@
module Network.AWS.EFS.CreateMountTarget
  ( -- * Creating a request
    CreateMountTarget (..),
    mkCreateMountTarget,

    -- ** Request lenses
    cmtFileSystemId,
    cmtSubnetId,
    cmtIpAddress,
    cmtSecurityGroups,

    -- * Destructuring the response
    Types.MountTargetDescription (..),
    Types.mkMountTargetDescription,

    -- ** Response lenses
    Types.mtdMountTargetId,
    Types.mtdFileSystemId,
    Types.mtdSubnetId,
    Types.mtdLifeCycleState,
    Types.mtdAvailabilityZoneId,
    Types.mtdAvailabilityZoneName,
    Types.mtdIpAddress,
    Types.mtdNetworkInterfaceId,
    Types.mtdOwnerId,
    Types.mtdVpcId,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateMountTarget' smart constructor.
data CreateMountTarget = CreateMountTarget'
  { -- | The ID of the file system for which to create the mount target.
    fileSystemId :: Types.FileSystemId,
    -- | The ID of the subnet to add the mount target in.
    subnetId :: Types.SubnetId,
    -- | Valid IPv4 address within the address range of the specified subnet.
    ipAddress :: Core.Maybe Types.IpAddress,
    -- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
    securityGroups :: Core.Maybe [Types.SecurityGroup]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMountTarget' value with any optional fields omitted.
mkCreateMountTarget ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  -- | 'subnetId'
  Types.SubnetId ->
  CreateMountTarget
mkCreateMountTarget fileSystemId subnetId =
  CreateMountTarget'
    { fileSystemId,
      subnetId,
      ipAddress = Core.Nothing,
      securityGroups = Core.Nothing
    }

-- | The ID of the file system for which to create the mount target.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtFileSystemId :: Lens.Lens' CreateMountTarget Types.FileSystemId
cmtFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED cmtFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The ID of the subnet to add the mount target in.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtSubnetId :: Lens.Lens' CreateMountTarget Types.SubnetId
cmtSubnetId = Lens.field @"subnetId"
{-# DEPRECATED cmtSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Valid IPv4 address within the address range of the specified subnet.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtIpAddress :: Lens.Lens' CreateMountTarget (Core.Maybe Types.IpAddress)
cmtIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED cmtIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtSecurityGroups :: Lens.Lens' CreateMountTarget (Core.Maybe [Types.SecurityGroup])
cmtSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED cmtSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

instance Core.FromJSON CreateMountTarget where
  toJSON CreateMountTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileSystemId" Core..= fileSystemId),
            Core.Just ("SubnetId" Core..= subnetId),
            ("IpAddress" Core..=) Core.<$> ipAddress,
            ("SecurityGroups" Core..=) Core.<$> securityGroups
          ]
      )

instance Core.AWSRequest CreateMountTarget where
  type Rs CreateMountTarget = Types.MountTargetDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-02-01/mount-targets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

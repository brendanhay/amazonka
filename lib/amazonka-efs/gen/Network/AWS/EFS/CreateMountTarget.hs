{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cmtIPAddress,
    cmtSecurityGroups,
    cmtFileSystemId,
    cmtSubnetId,

    -- * Destructuring the response
    MountTargetDescription (..),
    mkMountTargetDescription,

    -- ** Response lenses
    mtdIPAddress,
    mtdAvailabilityZoneId,
    mtdVPCId,
    mtdAvailabilityZoneName,
    mtdNetworkInterfaceId,
    mtdOwnerId,
    mtdMountTargetId,
    mtdFileSystemId,
    mtdSubnetId,
    mtdLifeCycleState,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateMountTarget' smart constructor.
data CreateMountTarget = CreateMountTarget'
  { ipAddress ::
      Lude.Maybe Lude.Text,
    securityGroups :: Lude.Maybe [Lude.Text],
    fileSystemId :: Lude.Text,
    subnetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMountTarget' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - The ID of the file system for which to create the mount target.
-- * 'ipAddress' - Valid IPv4 address within the address range of the specified subnet.
-- * 'securityGroups' - Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
-- * 'subnetId' - The ID of the subnet to add the mount target in.
mkCreateMountTarget ::
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  CreateMountTarget
mkCreateMountTarget pFileSystemId_ pSubnetId_ =
  CreateMountTarget'
    { ipAddress = Lude.Nothing,
      securityGroups = Lude.Nothing,
      fileSystemId = pFileSystemId_,
      subnetId = pSubnetId_
    }

-- | Valid IPv4 address within the address range of the specified subnet.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtIPAddress :: Lens.Lens' CreateMountTarget (Lude.Maybe Lude.Text)
cmtIPAddress = Lens.lens (ipAddress :: CreateMountTarget -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: CreateMountTarget)
{-# DEPRECATED cmtIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtSecurityGroups :: Lens.Lens' CreateMountTarget (Lude.Maybe [Lude.Text])
cmtSecurityGroups = Lens.lens (securityGroups :: CreateMountTarget -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: CreateMountTarget)
{-# DEPRECATED cmtSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of the file system for which to create the mount target.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtFileSystemId :: Lens.Lens' CreateMountTarget Lude.Text
cmtFileSystemId = Lens.lens (fileSystemId :: CreateMountTarget -> Lude.Text) (\s a -> s {fileSystemId = a} :: CreateMountTarget)
{-# DEPRECATED cmtFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The ID of the subnet to add the mount target in.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmtSubnetId :: Lens.Lens' CreateMountTarget Lude.Text
cmtSubnetId = Lens.lens (subnetId :: CreateMountTarget -> Lude.Text) (\s a -> s {subnetId = a} :: CreateMountTarget)
{-# DEPRECATED cmtSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.AWSRequest CreateMountTarget where
  type Rs CreateMountTarget = MountTargetDescription
  request = Req.postJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateMountTarget where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateMountTarget where
  toJSON CreateMountTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IpAddress" Lude..=) Lude.<$> ipAddress,
            ("SecurityGroups" Lude..=) Lude.<$> securityGroups,
            Lude.Just ("FileSystemId" Lude..= fileSystemId),
            Lude.Just ("SubnetId" Lude..= subnetId)
          ]
      )

instance Lude.ToPath CreateMountTarget where
  toPath = Lude.const "/2015-02-01/mount-targets"

instance Lude.ToQuery CreateMountTarget where
  toQuery = Lude.const Lude.mempty

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
-- Module      : Network.AWS.EFS.CreateMountTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mount target for a file system. You can then mount the file
-- system on EC2 instances by using the mount target.
--
-- You can create one mount target in each Availability Zone in your VPC.
-- All EC2 instances in a VPC within a given Availability Zone share a
-- single mount target for a given file system. If you have multiple
-- subnets in an Availability Zone, you create a mount target in one of the
-- subnets. EC2 instances do not need to be in the same subnet as the mount
-- target in order to access their file system. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works>.
--
-- In the request, you also specify a file system ID for which you are
-- creating the mount target and the file system\'s lifecycle state must be
-- @available@. For more information, see DescribeFileSystems.
--
-- In the request, you also provide a subnet ID, which determines the
-- following:
--
-- -   VPC in which Amazon EFS creates the mount target
--
-- -   Availability Zone in which Amazon EFS creates the mount target
--
-- -   IP address range from which Amazon EFS selects the IP address of the
--     mount target (if you don\'t specify an IP address in the request)
--
-- After creating the mount target, Amazon EFS returns a response that
-- includes, a @MountTargetId@ and an @IpAddress@. You use this IP address
-- when mounting the file system in an EC2 instance. You can also use the
-- mount target\'s DNS name when mounting the file system. The EC2 instance
-- on which you mount the file system by using the mount target can resolve
-- the mount target\'s DNS name to its IP address. For more information,
-- see
-- <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation How it Works: Implementation Overview>.
--
-- Note that you can create mount targets for a file system in only one
-- VPC, and there can be only one mount target per Availability Zone. That
-- is, if the file system already has one or more mount targets created for
-- it, the subnet specified in the request to add another mount target must
-- meet the following requirements:
--
-- -   Must belong to the same VPC as the subnets of the existing mount
--     targets
--
-- -   Must not be in the same Availability Zone as any of the subnets of
--     the existing mount targets
--
-- If the request satisfies the requirements, Amazon EFS does the
-- following:
--
-- -   Creates a new mount target in the specified subnet.
--
-- -   Also creates a new network interface in the subnet as follows:
--
--     -   If the request provides an @IpAddress@, Amazon EFS assigns that
--         IP address to the network interface. Otherwise, Amazon EFS
--         assigns a free address in the subnet (in the same way that the
--         Amazon EC2 @CreateNetworkInterface@ call does when a request
--         does not specify a primary private IP address).
--
--     -   If the request provides @SecurityGroups@, this network interface
--         is associated with those security groups. Otherwise, it belongs
--         to the default security group for the subnet\'s VPC.
--
--     -   Assigns the description
--         @Mount target fsmt-id for file system fs-id @ where @ fsmt-id @
--         is the mount target ID, and @ fs-id @ is the @FileSystemId@.
--
--     -   Sets the @requesterManaged@ property of the network interface to
--         @true@, and the @requesterId@ value to @EFS@.
--
--     Each Amazon EFS mount target has one corresponding requester-managed
--     EC2 network interface. After the network interface is created,
--     Amazon EFS sets the @NetworkInterfaceId@ field in the mount
--     target\'s description to the network interface ID, and the
--     @IpAddress@ field to its address. If network interface creation
--     fails, the entire @CreateMountTarget@ operation fails.
--
-- The @CreateMountTarget@ call returns only after creating the network
-- interface, but while the mount target state is still @creating@, you can
-- check the mount target creation status by calling the
-- DescribeMountTargets operation, which among other things returns the
-- mount target state.
--
-- We recommend that you create a mount target in each of the Availability
-- Zones. There are cost considerations for using a file system in an
-- Availability Zone through a mount target created in another Availability
-- Zone. For more information, see <http://aws.amazon.com/efs/ Amazon EFS>.
-- In addition, by always using a mount target local to the instance\'s
-- Availability Zone, you eliminate a partial failure scenario. If the
-- Availability Zone in which your mount target is created goes down, then
-- you can\'t access your file system through that mount target.
--
-- This operation requires permissions for the following action on the file
-- system:
--
-- -   @elasticfilesystem:CreateMountTarget@
--
-- This operation also requires permissions for the following Amazon EC2
-- actions:
--
-- -   @ec2:DescribeSubnets@
--
-- -   @ec2:DescribeNetworkInterfaces@
--
-- -   @ec2:CreateNetworkInterface@
module Network.AWS.EFS.CreateMountTarget
  ( -- * Creating a Request
    CreateMountTarget (..),
    newCreateMountTarget,

    -- * Request Lenses
    createMountTarget_securityGroups,
    createMountTarget_ipAddress,
    createMountTarget_fileSystemId,
    createMountTarget_subnetId,

    -- * Destructuring the Response
    MountTargetDescription (..),
    newMountTargetDescription,

    -- * Response Lenses
    mountTargetDescription_ownerId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateMountTarget' smart constructor.
data CreateMountTarget = CreateMountTarget'
  { -- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
    -- be for the same VPC as subnet specified.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Valid IPv4 address within the address range of the specified subnet.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the file system for which to create the mount target.
    fileSystemId :: Prelude.Text,
    -- | The ID of the subnet to add the mount target in.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateMountTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'createMountTarget_securityGroups' - Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
-- be for the same VPC as subnet specified.
--
-- 'ipAddress', 'createMountTarget_ipAddress' - Valid IPv4 address within the address range of the specified subnet.
--
-- 'fileSystemId', 'createMountTarget_fileSystemId' - The ID of the file system for which to create the mount target.
--
-- 'subnetId', 'createMountTarget_subnetId' - The ID of the subnet to add the mount target in.
newCreateMountTarget ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  CreateMountTarget
newCreateMountTarget pFileSystemId_ pSubnetId_ =
  CreateMountTarget'
    { securityGroups =
        Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      subnetId = pSubnetId_
    }

-- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
-- be for the same VPC as subnet specified.
createMountTarget_securityGroups :: Lens.Lens' CreateMountTarget (Prelude.Maybe [Prelude.Text])
createMountTarget_securityGroups = Lens.lens (\CreateMountTarget' {securityGroups} -> securityGroups) (\s@CreateMountTarget' {} a -> s {securityGroups = a} :: CreateMountTarget) Prelude.. Lens.mapping Prelude._Coerce

-- | Valid IPv4 address within the address range of the specified subnet.
createMountTarget_ipAddress :: Lens.Lens' CreateMountTarget (Prelude.Maybe Prelude.Text)
createMountTarget_ipAddress = Lens.lens (\CreateMountTarget' {ipAddress} -> ipAddress) (\s@CreateMountTarget' {} a -> s {ipAddress = a} :: CreateMountTarget)

-- | The ID of the file system for which to create the mount target.
createMountTarget_fileSystemId :: Lens.Lens' CreateMountTarget Prelude.Text
createMountTarget_fileSystemId = Lens.lens (\CreateMountTarget' {fileSystemId} -> fileSystemId) (\s@CreateMountTarget' {} a -> s {fileSystemId = a} :: CreateMountTarget)

-- | The ID of the subnet to add the mount target in.
createMountTarget_subnetId :: Lens.Lens' CreateMountTarget Prelude.Text
createMountTarget_subnetId = Lens.lens (\CreateMountTarget' {subnetId} -> subnetId) (\s@CreateMountTarget' {} a -> s {subnetId = a} :: CreateMountTarget)

instance Prelude.AWSRequest CreateMountTarget where
  type Rs CreateMountTarget = MountTargetDescription
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateMountTarget

instance Prelude.NFData CreateMountTarget

instance Prelude.ToHeaders CreateMountTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateMountTarget where
  toJSON CreateMountTarget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityGroups" Prelude..=)
              Prelude.<$> securityGroups,
            ("IpAddress" Prelude..=) Prelude.<$> ipAddress,
            Prelude.Just
              ("FileSystemId" Prelude..= fileSystemId),
            Prelude.Just ("SubnetId" Prelude..= subnetId)
          ]
      )

instance Prelude.ToPath CreateMountTarget where
  toPath = Prelude.const "/2015-02-01/mount-targets"

instance Prelude.ToQuery CreateMountTarget where
  toQuery = Prelude.const Prelude.mempty

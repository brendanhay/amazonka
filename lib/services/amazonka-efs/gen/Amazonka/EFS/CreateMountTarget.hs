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
-- Module      : Amazonka.EFS.CreateMountTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- target in order to access their file system.
--
-- You can create only one mount target for an EFS file system using One
-- Zone storage classes. You must create that mount target in the same
-- Availability Zone in which the file system is located. Use the
-- @AvailabilityZoneName@ and @AvailabiltyZoneId@ properties in the
-- DescribeFileSystems response object to get this information. Use the
-- @subnetId@ associated with the file system\'s Availability Zone when
-- creating the mount target.
--
-- For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works>.
--
-- To create a mount target for a file system, the file system\'s lifecycle
-- state must be @available@. For more information, see
-- DescribeFileSystems.
--
-- In the request, provide the following:
--
-- -   The file system ID for which you are creating the mount target.
--
-- -   A subnet ID, which determines the following:
--
--     -   The VPC in which Amazon EFS creates the mount target
--
--     -   The Availability Zone in which Amazon EFS creates the mount
--         target
--
--     -   The IP address range from which Amazon EFS selects the IP
--         address of the mount target (if you don\'t specify an IP address
--         in the request)
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
--         @Mount target @/@fsmt-id@/@ for file system @/@fs-id@/@ @ where
--         @ @/@fsmt-id@/@ @ is the mount target ID, and @ @/@fs-id@/@ @ is
--         the @FileSystemId@.
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
module Amazonka.EFS.CreateMountTarget
  ( -- * Creating a Request
    CreateMountTarget (..),
    newCreateMountTarget,

    -- * Request Lenses
    createMountTarget_ipAddress,
    createMountTarget_securityGroups,
    createMountTarget_fileSystemId,
    createMountTarget_subnetId,

    -- * Destructuring the Response
    MountTargetDescription (..),
    newMountTargetDescription,

    -- * Response Lenses
    mountTargetDescription_availabilityZoneId,
    mountTargetDescription_availabilityZoneName,
    mountTargetDescription_ipAddress,
    mountTargetDescription_networkInterfaceId,
    mountTargetDescription_ownerId,
    mountTargetDescription_vpcId,
    mountTargetDescription_mountTargetId,
    mountTargetDescription_fileSystemId,
    mountTargetDescription_subnetId,
    mountTargetDescription_lifeCycleState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateMountTarget' smart constructor.
data CreateMountTarget = CreateMountTarget'
  { -- | Valid IPv4 address within the address range of the specified subnet.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
    -- be for the same VPC as subnet specified.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the file system for which to create the mount target.
    fileSystemId :: Prelude.Text,
    -- | The ID of the subnet to add the mount target in. For file systems that
    -- use One Zone storage classes, use the subnet that is associated with the
    -- file system\'s Availability Zone.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMountTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'createMountTarget_ipAddress' - Valid IPv4 address within the address range of the specified subnet.
--
-- 'securityGroups', 'createMountTarget_securityGroups' - Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
-- be for the same VPC as subnet specified.
--
-- 'fileSystemId', 'createMountTarget_fileSystemId' - The ID of the file system for which to create the mount target.
--
-- 'subnetId', 'createMountTarget_subnetId' - The ID of the subnet to add the mount target in. For file systems that
-- use One Zone storage classes, use the subnet that is associated with the
-- file system\'s Availability Zone.
newCreateMountTarget ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  CreateMountTarget
newCreateMountTarget pFileSystemId_ pSubnetId_ =
  CreateMountTarget'
    { ipAddress = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      subnetId = pSubnetId_
    }

-- | Valid IPv4 address within the address range of the specified subnet.
createMountTarget_ipAddress :: Lens.Lens' CreateMountTarget (Prelude.Maybe Prelude.Text)
createMountTarget_ipAddress = Lens.lens (\CreateMountTarget' {ipAddress} -> ipAddress) (\s@CreateMountTarget' {} a -> s {ipAddress = a} :: CreateMountTarget)

-- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@. These must
-- be for the same VPC as subnet specified.
createMountTarget_securityGroups :: Lens.Lens' CreateMountTarget (Prelude.Maybe [Prelude.Text])
createMountTarget_securityGroups = Lens.lens (\CreateMountTarget' {securityGroups} -> securityGroups) (\s@CreateMountTarget' {} a -> s {securityGroups = a} :: CreateMountTarget) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the file system for which to create the mount target.
createMountTarget_fileSystemId :: Lens.Lens' CreateMountTarget Prelude.Text
createMountTarget_fileSystemId = Lens.lens (\CreateMountTarget' {fileSystemId} -> fileSystemId) (\s@CreateMountTarget' {} a -> s {fileSystemId = a} :: CreateMountTarget)

-- | The ID of the subnet to add the mount target in. For file systems that
-- use One Zone storage classes, use the subnet that is associated with the
-- file system\'s Availability Zone.
createMountTarget_subnetId :: Lens.Lens' CreateMountTarget Prelude.Text
createMountTarget_subnetId = Lens.lens (\CreateMountTarget' {subnetId} -> subnetId) (\s@CreateMountTarget' {} a -> s {subnetId = a} :: CreateMountTarget)

instance Core.AWSRequest CreateMountTarget where
  type
    AWSResponse CreateMountTarget =
      MountTargetDescription
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateMountTarget where
  hashWithSalt _salt CreateMountTarget' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData CreateMountTarget where
  rnf CreateMountTarget' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToHeaders CreateMountTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMountTarget where
  toJSON CreateMountTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IpAddress" Data..=) Prelude.<$> ipAddress,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("FileSystemId" Data..= fileSystemId),
            Prelude.Just ("SubnetId" Data..= subnetId)
          ]
      )

instance Data.ToPath CreateMountTarget where
  toPath = Prelude.const "/2015-02-01/mount-targets"

instance Data.ToQuery CreateMountTarget where
  toQuery = Prelude.const Prelude.mempty

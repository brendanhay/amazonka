{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticFileSystem.CreateMountTarget
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a mount target for a file system. You can then mount the file
-- system on EC2 instances via the mount target.
--
-- You can create one mount target in each Availability Zone in your VPC.
-- All EC2 instances in a VPC within a given Availability Zone share a
-- single mount target for a given file system. If you have multiple
-- subnets in an Availability Zone, you create a mount target in one of the
-- subnets. EC2 instances do not need to be in the same subnet as the mount
-- target in order to access their file system. For more information, see
-- <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works>.
--
-- In the request, you also specify a file system ID for which you are
-- creating the mount target and the file system\'s lifecycle state must be
-- \"available\" (see DescribeFileSystems).
--
-- In the request, you also provide a subnet ID, which serves several
-- purposes:
--
-- -   It determines the VPC in which Amazon EFS creates the mount target.
-- -   It determines the Availability Zone in which Amazon EFS creates the
--     mount target.
-- -   It determines the IP address range from which Amazon EFS selects the
--     IP address of the mount target if you don\'t specify an IP address
--     in the request.
--
-- After creating the mount target, Amazon EFS returns a response that
-- includes, a @MountTargetId@ and an @IpAddress@. You use this IP address
-- when mounting the file system in an EC2 instance. You can also use the
-- mount target\'s DNS name when mounting the file system. The EC2 instance
-- on which you mount the file system via the mount target can resolve the
-- mount target\'s DNS name to its IP address. For more information, see
-- <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation How it Works: Implementation Overview>
--
-- Note that you can create mount targets for a file system in only one
-- VPC, and there can be only one mount target per Availability Zone. That
-- is, if the file system already has one or more mount targets created for
-- it, the request to add another mount target must meet the following
-- requirements:
--
-- -   The subnet specified in the request must belong to the same VPC as
--     the subnets of the existing mount targets.
--
-- -   The subnet specified in the request must not be in the same
--     Availability Zone as any of the subnets of the existing mount
--     targets.
--
-- If the request satisfies the requirements, Amazon EFS does the
-- following:
--
-- -   Creates a new mount target in the specified subnet.
-- -   Also creates a new network interface in the subnet as follows:
--
--     -   If the request provides an @IpAddress@, Amazon EFS assigns that
--         IP address to the network interface. Otherwise, Amazon EFS
--         assigns a free address in the subnet (in the same way that the
--         Amazon EC2 @CreateNetworkInterface@ call does when a request
--         does not specify a primary private IP address).
--     -   If the request provides @SecurityGroups@, this network interface
--         is associated with those security groups. Otherwise, it belongs
--         to the default security group for the subnet\'s VPC.
--     -   Assigns the description
--         @\"Mount target fsmt-id for file system fs-id\"@ where @fsmt-id@
--         is the mount target ID, and @fs-id@ is the @FileSystemId@.
--     -   Sets the @requesterManaged@ property of the network interface to
--         \"true\", and the @requesterId@ value to \"EFS\".
--
--     Each Amazon EFS mount target has one corresponding requestor-managed
--     EC2 network interface. After the network interface is created,
--     Amazon EFS sets the @NetworkInterfaceId@ field in the mount
--     target\'s description to the network interface ID, and the
--     @IpAddress@ field to its address. If network interface creation
--     fails, the entire @CreateMountTarget@ operation fails.
--
-- The @CreateMountTarget@ call returns only after creating the network
-- interface, but while the mount target state is still \"creating\". You
-- can check the mount target creation status by calling the
-- DescribeFileSystems API, which among other things returns the mount
-- target state.
--
-- We recommend you create a mount target in each of the Availability
-- Zones. There are cost considerations for using a file system in an
-- Availability Zone through a mount target created in another Availability
-- Zone. For more information, go to
-- <http://aws.amazon.com/efs/ Amazon EFS> product detail page. In
-- addition, by always using a mount target local to the instance\'s
-- Availability Zone, you eliminate a partial failure scenario; if the
-- Availablity Zone in which your mount target is created goes down, then
-- you won\'t be able to access your file system through that mount target.
--
-- This operation requires permission for the following action on the file
-- system:
--
-- -   @elasticfilesystem:CreateMountTarget@
--
-- This operation also requires permission for the following Amazon EC2
-- actions:
--
-- -   @ec2:DescribeSubnets@
-- -   @ec2:DescribeNetworkInterfaces@
-- -   @ec2:CreateNetworkInterface@
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateMountTarget.html>
module Network.AWS.ElasticFileSystem.CreateMountTarget
    (
    -- * Request
      CreateMountTarget
    -- ** Request constructor
    , createMountTarget
    -- ** Request lenses
    , cmtIPAddress
    , cmtSecurityGroups
    , cmtFileSystemId
    , cmtSubnetId

    -- * Response
    , MountTargetDescription
    -- ** Response constructor
    , mountTargetDescription
    -- ** Response lenses
    , mtdIPAddress
    , mtdNetworkInterfaceId
    , mtdOwnerId
    , mtdMountTargetId
    , mtdFileSystemId
    , mtdSubnetId
    , mtdLifeCycleState
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticFileSystem.Types

-- | /See:/ 'createMountTarget' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmtIPAddress'
--
-- * 'cmtSecurityGroups'
--
-- * 'cmtFileSystemId'
--
-- * 'cmtSubnetId'
data CreateMountTarget = CreateMountTarget'{_cmtIPAddress :: Maybe Text, _cmtSecurityGroups :: Maybe [Text], _cmtFileSystemId :: Text, _cmtSubnetId :: Text} deriving (Eq, Read, Show)

-- | 'CreateMountTarget' smart constructor.
createMountTarget :: Text -> Text -> CreateMountTarget
createMountTarget pFileSystemId pSubnetId = CreateMountTarget'{_cmtIPAddress = Nothing, _cmtSecurityGroups = Nothing, _cmtFileSystemId = pFileSystemId, _cmtSubnetId = pSubnetId};

-- | A valid IPv4 address within the address range of the specified subnet.
cmtIPAddress :: Lens' CreateMountTarget (Maybe Text)
cmtIPAddress = lens _cmtIPAddress (\ s a -> s{_cmtIPAddress = a});

-- | Up to 5 VPC security group IDs, of the form \"sg-xxxxxxxx\". These must
-- be for the same VPC as subnet specified.
cmtSecurityGroups :: Lens' CreateMountTarget [Text]
cmtSecurityGroups = lens _cmtSecurityGroups (\ s a -> s{_cmtSecurityGroups = a}) . _Default;

-- | The ID of the file system for which to create the mount target.
cmtFileSystemId :: Lens' CreateMountTarget Text
cmtFileSystemId = lens _cmtFileSystemId (\ s a -> s{_cmtFileSystemId = a});

-- | The ID of the subnet to add the mount target in.
cmtSubnetId :: Lens' CreateMountTarget Text
cmtSubnetId = lens _cmtSubnetId (\ s a -> s{_cmtSubnetId = a});

instance AWSRequest CreateMountTarget where
        type Sv CreateMountTarget = ElasticFileSystem
        type Rs CreateMountTarget = MountTargetDescription
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateMountTarget where
        toHeaders = const mempty

instance ToJSON CreateMountTarget where
        toJSON CreateMountTarget'{..}
          = object
              ["IpAddress" .= _cmtIPAddress,
               "SecurityGroups" .= _cmtSecurityGroups,
               "FileSystemId" .= _cmtFileSystemId,
               "SubnetId" .= _cmtSubnetId]

instance ToPath CreateMountTarget where
        toPath = const "/2015-02-01/mount-targets"

instance ToQuery CreateMountTarget where
        toQuery = const mempty

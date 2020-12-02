{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateMountTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mount target for a file system. You can then mount the file system on EC2 instances via the mount target.
--
--
-- You can create one mount target in each Availability Zone in your VPC. All EC2 instances in a VPC within a given Availability Zone share a single mount target for a given file system. If you have multiple subnets in an Availability Zone, you create a mount target in one of the subnets. EC2 instances do not need to be in the same subnet as the mount target in order to access their file system. For more information, see <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works> .
--
-- In the request, you also specify a file system ID for which you are creating the mount target and the file system's lifecycle state must be @available@ . For more information, see 'DescribeFileSystems' .
--
-- In the request, you also provide a subnet ID, which determines the following:
--
--     * VPC in which Amazon EFS creates the mount target
--
--     * Availability Zone in which Amazon EFS creates the mount target
--
--     * IP address range from which Amazon EFS selects the IP address of the mount target (if you don't specify an IP address in the request)
--
--
--
-- After creating the mount target, Amazon EFS returns a response that includes, a @MountTargetId@ and an @IpAddress@ . You use this IP address when mounting the file system in an EC2 instance. You can also use the mount target's DNS name when mounting the file system. The EC2 instance on which you mount the file system via the mount target can resolve the mount target's DNS name to its IP address. For more information, see <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html#how-it-works-implementation How it Works: Implementation Overview> .
--
-- Note that you can create mount targets for a file system in only one VPC, and there can be only one mount target per Availability Zone. That is, if the file system already has one or more mount targets created for it, the subnet specified in the request to add another mount target must meet the following requirements:
--
--     * Must belong to the same VPC as the subnets of the existing mount targets
--
--     * Must not be in the same Availability Zone as any of the subnets of the existing mount targets
--
--
--
-- If the request satisfies the requirements, Amazon EFS does the following:
--
--     * Creates a new mount target in the specified subnet.
--
--     * Also creates a new network interface in the subnet as follows:
--
--     * If the request provides an @IpAddress@ , Amazon EFS assigns that IP address to the network interface. Otherwise, Amazon EFS assigns a free address in the subnet (in the same way that the Amazon EC2 @CreateNetworkInterface@ call does when a request does not specify a primary private IP address).
--
--     * If the request provides @SecurityGroups@ , this network interface is associated with those security groups. Otherwise, it belongs to the default security group for the subnet's VPC.
--
--     * Assigns the description @Mount target /fsmt-id/ for file system /fs-id/ @ where @/fsmt-id/ @ is the mount target ID, and @/fs-id/ @ is the @FileSystemId@ .
--
--     * Sets the @requesterManaged@ property of the network interface to @true@ , and the @requesterId@ value to @EFS@ .
--
--
--
-- Each Amazon EFS mount target has one corresponding requester-managed EC2 network interface. After the network interface is created, Amazon EFS sets the @NetworkInterfaceId@ field in the mount target's description to the network interface ID, and the @IpAddress@ field to its address. If network interface creation fails, the entire @CreateMountTarget@ operation fails.
--
--
--
-- We recommend you create a mount target in each of the Availability Zones. There are cost considerations for using a file system in an Availability Zone through a mount target created in another Availability Zone. For more information, see <http://aws.amazon.com/efs/ Amazon EFS> . In addition, by always using a mount target local to the instance's Availability Zone, you eliminate a partial failure scenario. If the Availability Zone in which your mount target is created goes down, then you won't be able to access your file system through that mount target.
--
-- This operation requires permissions for the following action on the file system:
--
--     * @elasticfilesystem:CreateMountTarget@
--
--
--
-- This operation also requires permissions for the following Amazon EC2 actions:
--
--     * @ec2:DescribeSubnets@
--
--     * @ec2:DescribeNetworkInterfaces@
--
--     * @ec2:CreateNetworkInterface@
--
--
--
module Network.AWS.EFS.CreateMountTarget
    (
    -- * Creating a Request
      createMountTarget
    , CreateMountTarget
    -- * Request Lenses
    , cmtIPAddress
    , cmtSecurityGroups
    , cmtFileSystemId
    , cmtSubnetId

    -- * Destructuring the Response
    , mountTargetDescription
    , MountTargetDescription
    -- * Response Lenses
    , mtdIPAddress
    , mtdNetworkInterfaceId
    , mtdOwnerId
    , mtdMountTargetId
    , mtdFileSystemId
    , mtdSubnetId
    , mtdLifeCycleState
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createMountTarget' smart constructor.
data CreateMountTarget = CreateMountTarget'
  { _cmtIPAddress      :: !(Maybe Text)
  , _cmtSecurityGroups :: !(Maybe [Text])
  , _cmtFileSystemId   :: !Text
  , _cmtSubnetId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMountTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmtIPAddress' - Valid IPv4 address within the address range of the specified subnet.
--
-- * 'cmtSecurityGroups' - Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
--
-- * 'cmtFileSystemId' - ID of the file system for which to create the mount target.
--
-- * 'cmtSubnetId' - ID of the subnet to add the mount target in.
createMountTarget
    :: Text -- ^ 'cmtFileSystemId'
    -> Text -- ^ 'cmtSubnetId'
    -> CreateMountTarget
createMountTarget pFileSystemId_ pSubnetId_ =
  CreateMountTarget'
    { _cmtIPAddress = Nothing
    , _cmtSecurityGroups = Nothing
    , _cmtFileSystemId = pFileSystemId_
    , _cmtSubnetId = pSubnetId_
    }


-- | Valid IPv4 address within the address range of the specified subnet.
cmtIPAddress :: Lens' CreateMountTarget (Maybe Text)
cmtIPAddress = lens _cmtIPAddress (\ s a -> s{_cmtIPAddress = a})

-- | Up to five VPC security group IDs, of the form @sg-xxxxxxxx@ . These must be for the same VPC as subnet specified.
cmtSecurityGroups :: Lens' CreateMountTarget [Text]
cmtSecurityGroups = lens _cmtSecurityGroups (\ s a -> s{_cmtSecurityGroups = a}) . _Default . _Coerce

-- | ID of the file system for which to create the mount target.
cmtFileSystemId :: Lens' CreateMountTarget Text
cmtFileSystemId = lens _cmtFileSystemId (\ s a -> s{_cmtFileSystemId = a})

-- | ID of the subnet to add the mount target in.
cmtSubnetId :: Lens' CreateMountTarget Text
cmtSubnetId = lens _cmtSubnetId (\ s a -> s{_cmtSubnetId = a})

instance AWSRequest CreateMountTarget where
        type Rs CreateMountTarget = MountTargetDescription
        request = postJSON efs
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateMountTarget where

instance NFData CreateMountTarget where

instance ToHeaders CreateMountTarget where
        toHeaders = const mempty

instance ToJSON CreateMountTarget where
        toJSON CreateMountTarget'{..}
          = object
              (catMaybes
                 [("IpAddress" .=) <$> _cmtIPAddress,
                  ("SecurityGroups" .=) <$> _cmtSecurityGroups,
                  Just ("FileSystemId" .= _cmtFileSystemId),
                  Just ("SubnetId" .= _cmtSubnetId)])

instance ToPath CreateMountTarget where
        toPath = const "/2015-02-01/mount-targets"

instance ToQuery CreateMountTarget where
        toQuery = const mempty

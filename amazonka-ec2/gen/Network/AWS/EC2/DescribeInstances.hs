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
-- Module      : Network.AWS.EC2.DescribeInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your instances.
--
-- If you specify one or more instance IDs, Amazon EC2 returns information
-- for those instances. If you do not specify instance IDs, Amazon EC2
-- returns information for all relevant instances. If you specify an
-- instance ID that is not valid, an error is returned. If you specify an
-- instance that you do not own, it is not included in the returned
-- results.
--
-- Recently terminated instances might appear in the returned results. This
-- interval is usually less than one hour.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstances.html AWS API Reference> for DescribeInstances.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstances
    (
    -- * Creating a Request
      describeInstances
    , DescribeInstances
    -- * Request Lenses
    , diiFilters
    , diiNextToken
    , diiInstanceIds
    , diiDryRun
    , diiMaxResults

    -- * Destructuring the Response
    , describeInstancesResponse
    , DescribeInstancesResponse
    -- * Response Lenses
    , dirsNextToken
    , dirsReservations
    , dirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
    { _diiFilters     :: !(Maybe [Filter])
    , _diiNextToken   :: !(Maybe Text)
    , _diiInstanceIds :: !(Maybe [Text])
    , _diiDryRun      :: !(Maybe Bool)
    , _diiMaxResults  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diiFilters'
--
-- * 'diiNextToken'
--
-- * 'diiInstanceIds'
--
-- * 'diiDryRun'
--
-- * 'diiMaxResults'
describeInstances
    :: DescribeInstances
describeInstances =
    DescribeInstances'
    { _diiFilters = Nothing
    , _diiNextToken = Nothing
    , _diiInstanceIds = Nothing
    , _diiDryRun = Nothing
    , _diiMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   'architecture' - The instance architecture ('i386' | 'x86_64').
--
-- -   'availability-zone' - The Availability Zone of the instance.
--
-- -   'block-device-mapping.attach-time' - The attach time for an EBS
--     volume mapped to the instance, for example,
--     '2010-09-15T17:15:20.000Z'.
--
-- -   'block-device-mapping.delete-on-termination' - A Boolean that
--     indicates whether the EBS volume is deleted on instance termination.
--
-- -   'block-device-mapping.device-name' - The device name for the EBS
--     volume (for example, '\/dev\/sdh' or 'xvdh').
--
-- -   'block-device-mapping.status' - The status for the EBS volume
--     ('attaching' | 'attached' | 'detaching' | 'detached').
--
-- -   'block-device-mapping.volume-id' - The volume ID of the EBS volume.
--
-- -   'client-token' - The idempotency token you provided when you
--     launched the instance.
--
-- -   'dns-name' - The public DNS name of the instance.
--
-- -   'group-id' - The ID of the security group for the instance.
--     EC2-Classic only.
--
-- -   'group-name' - The name of the security group for the instance.
--     EC2-Classic only.
--
-- -   'hypervisor' - The hypervisor type of the instance ('ovm' | 'xen').
--
-- -   'iam-instance-profile.arn' - The instance profile associated with
--     the instance. Specified as an ARN.
--
-- -   'image-id' - The ID of the image used to launch the instance.
--
-- -   'instance-id' - The ID of the instance.
--
-- -   'instance-lifecycle' - Indicates whether this is a Spot Instance
--     ('spot').
--
-- -   'instance-state-code' - The state of the instance, as a 16-bit
--     unsigned integer. The high byte is an opaque internal value and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are: 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   'instance-state-name' - The state of the instance ('pending' |
--     'running' | 'shutting-down' | 'terminated' | 'stopping' |
--     'stopped').
--
-- -   'instance-type' - The type of instance (for example, 't2.micro').
--
-- -   'instance.group-id' - The ID of the security group for the instance.
--
-- -   'instance.group-name' - The name of the security group for the
--     instance.
--
-- -   'ip-address' - The public IP address of the instance.
--
-- -   'kernel-id' - The kernel ID.
--
-- -   'key-name' - The name of the key pair used when the instance was
--     launched.
--
-- -   'launch-index' - When launching multiple instances, this is the
--     index for the instance in the launch group (for example, 0, 1, 2,
--     and so on).
--
-- -   'launch-time' - The time when the instance was launched.
--
-- -   'monitoring-state' - Indicates whether monitoring is enabled for the
--     instance ('disabled' | 'enabled').
--
-- -   'owner-id' - The AWS account ID of the instance owner.
--
-- -   'placement-group-name' - The name of the placement group for the
--     instance.
--
-- -   'platform' - The platform. Use 'windows' if you have Windows
--     instances; otherwise, leave blank.
--
-- -   'private-dns-name' - The private DNS name of the instance.
--
-- -   'private-ip-address' - The private IP address of the instance.
--
-- -   'product-code' - The product code associated with the AMI used to
--     launch the instance.
--
-- -   'product-code.type' - The type of product code ('devpay' |
--     'marketplace').
--
-- -   'ramdisk-id' - The RAM disk ID.
--
-- -   'reason' - The reason for the current state of the instance (for
--     example, shows \"User Initiated [date]\" when you stop or terminate
--     the instance). Similar to the state-reason-code filter.
--
-- -   'requester-id' - The ID of the entity that launched the instance on
--     your behalf (for example, AWS Management Console, Auto Scaling, and
--     so on).
--
-- -   'reservation-id' - The ID of the instance\'s reservation. A
--     reservation ID is created any time you launch an instance. A
--     reservation ID has a one-to-one relationship with an instance launch
--     request, but can be associated with more than one instance if you
--     launch multiple instances using the same launch request. For
--     example, if you launch one instance, you\'ll get one reservation ID.
--     If you launch ten instances using the same launch request, you\'ll
--     also get one reservation ID.
--
-- -   'root-device-name' - The name of the root device for the instance
--     (for example, '\/dev\/sda1' or '\/dev\/xvda').
--
-- -   'root-device-type' - The type of root device that the instance uses
--     ('ebs' | 'instance-store').
--
-- -   'source-dest-check' - Indicates whether the instance performs
--     source\/destination checking. A value of 'true' means that checking
--     is enabled, and 'false' means checking is disabled. The value must
--     be 'false' for the instance to perform network address translation
--     (NAT) in your VPC.
--
-- -   'spot-instance-request-id' - The ID of the Spot Instance request.
--
-- -   'state-reason-code' - The reason code for the state change.
--
-- -   'state-reason-message' - A message that describes the state change.
--
-- -   'subnet-id' - The ID of the subnet for the instance.
--
-- -   'tag':/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource, where 'tag':/key/ is the tag\'s key.
--
-- -   'tag-key' - The key of a tag assigned to the resource. This filter
--     is independent of the 'tag-value' filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- -   'tag-value' - The value of a tag assigned to the resource. This
--     filter is independent of the 'tag-key' filter.
--
-- -   'tenancy' - The tenancy of an instance ('dedicated' | 'default').
--
-- -   'virtualization-type' - The virtualization type of the instance
--     ('paravirtual' | 'hvm').
--
-- -   'vpc-id' - The ID of the VPC that the instance is running in.
--
-- -   'network-interface.description' - The description of the network
--     interface.
--
-- -   'network-interface.subnet-id' - The ID of the subnet for the network
--     interface.
--
-- -   'network-interface.vpc-id' - The ID of the VPC for the network
--     interface.
--
-- -   'network-interface.network-interface.id' - The ID of the network
--     interface.
--
-- -   'network-interface.owner-id' - The ID of the owner of the network
--     interface.
--
-- -   'network-interface.availability-zone' - The Availability Zone for
--     the network interface.
--
-- -   'network-interface.requester-id' - The requester ID for the network
--     interface.
--
-- -   'network-interface.requester-managed' - Indicates whether the
--     network interface is being managed by AWS.
--
-- -   'network-interface.status' - The status of the network interface
--     ('available') | 'in-use').
--
-- -   'network-interface.mac-address' - The MAC address of the network
--     interface.
--
-- -   'network-interface-private-dns-name' - The private DNS name of the
--     network interface.
--
-- -   'network-interface.source-dest-check' - Whether the network
--     interface performs source\/destination checking. A value of 'true'
--     means checking is enabled, and 'false' means checking is disabled.
--     The value must be 'false' for the network interface to perform
--     network address translation (NAT) in your VPC.
--
-- -   'network-interface.group-id' - The ID of a security group associated
--     with the network interface.
--
-- -   'network-interface.group-name' - The name of a security group
--     associated with the network interface.
--
-- -   'network-interface.attachment.attachment-id' - The ID of the
--     interface attachment.
--
-- -   'network-interface.attachment.instance-id' - The ID of the instance
--     to which the network interface is attached.
--
-- -   'network-interface.attachment.instance-owner-id' - The owner ID of
--     the instance to which the network interface is attached.
--
-- -   'network-interface.addresses.private-ip-address' - The private IP
--     address associated with the network interface.
--
-- -   'network-interface.attachment.device-index' - The device index to
--     which the network interface is attached.
--
-- -   'network-interface.attachment.status' - The status of the attachment
--     ('attaching' | 'attached' | 'detaching' | 'detached').
--
-- -   'network-interface.attachment.attach-time' - The time that the
--     network interface was attached to an instance.
--
-- -   'network-interface.attachment.delete-on-termination' - Specifies
--     whether the attachment is deleted when an instance is terminated.
--
-- -   'network-interface.addresses.primary' - Specifies whether the IP
--     address of the network interface is the primary private IP address.
--
-- -   'network-interface.addresses.association.public-ip' - The ID of the
--     association of an Elastic IP address with a network interface.
--
-- -   'network-interface.addresses.association.ip-owner-id' - The owner ID
--     of the private IP address associated with the network interface.
--
-- -   'association.public-ip' - The address of the Elastic IP address
--     bound to the network interface.
--
-- -   'association.ip-owner-id' - The owner of the Elastic IP address
--     associated with the network interface.
--
-- -   'association.allocation-id' - The allocation ID returned when you
--     allocated the Elastic IP address for your network interface.
--
-- -   'association.association-id' - The association ID returned when the
--     network interface was associated with an IP address.
--
diiFilters :: Lens' DescribeInstances [Filter]
diiFilters = lens _diiFilters (\ s a -> s{_diiFilters = a}) . _Default . _Coerce;

-- | The token to request the next page of results.
diiNextToken :: Lens' DescribeInstances (Maybe Text)
diiNextToken = lens _diiNextToken (\ s a -> s{_diiNextToken = a});

-- | One or more instance IDs.
--
-- Default: Describes all your instances.
diiInstanceIds :: Lens' DescribeInstances [Text]
diiInstanceIds = lens _diiInstanceIds (\ s a -> s{_diiInstanceIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
diiDryRun :: Lens' DescribeInstances (Maybe Bool)
diiDryRun = lens _diiDryRun (\ s a -> s{_diiDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned 'NextToken' value. This value
-- can be between 5 and 1000; if 'MaxResults' is given a value larger than
-- 1000, only 1000 results are returned. You cannot specify this parameter
-- and the instance IDs parameter in the same request.
diiMaxResults :: Lens' DescribeInstances (Maybe Int)
diiMaxResults = lens _diiMaxResults (\ s a -> s{_diiMaxResults = a});

instance AWSPager DescribeInstances where
        page rq rs
          | stop (rs ^. dirsNextToken) = Nothing
          | stop (rs ^. dirsReservations) = Nothing
          | otherwise =
            Just $ rq & diiNextToken .~ rs ^. dirsNextToken

instance AWSRequest DescribeInstances where
        type Sv DescribeInstances = EC2
        type Rs DescribeInstances = DescribeInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstancesResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "reservationSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeInstances where
        toHeaders = const mempty

instance ToPath DescribeInstances where
        toPath = const "/"

instance ToQuery DescribeInstances where
        toQuery DescribeInstances'{..}
          = mconcat
              ["Action" =: ("DescribeInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _diiFilters),
               "NextToken" =: _diiNextToken,
               toQuery
                 (toQueryList "InstanceId" <$> _diiInstanceIds),
               "DryRun" =: _diiDryRun,
               "MaxResults" =: _diiMaxResults]

-- | /See:/ 'describeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
    { _dirsNextToken    :: !(Maybe Text)
    , _dirsReservations :: !(Maybe [Reservation])
    , _dirsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsNextToken'
--
-- * 'dirsReservations'
--
-- * 'dirsStatus'
describeInstancesResponse
    :: Int -- ^ 'dirsStatus'
    -> DescribeInstancesResponse
describeInstancesResponse pStatus_ =
    DescribeInstancesResponse'
    { _dirsNextToken = Nothing
    , _dirsReservations = Nothing
    , _dirsStatus = pStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- 'null' when there are no more results to return.
dirsNextToken :: Lens' DescribeInstancesResponse (Maybe Text)
dirsNextToken = lens _dirsNextToken (\ s a -> s{_dirsNextToken = a});

-- | One or more reservations.
dirsReservations :: Lens' DescribeInstancesResponse [Reservation]
dirsReservations = lens _dirsReservations (\ s a -> s{_dirsReservations = a}) . _Default . _Coerce;

-- | The response status code.
dirsStatus :: Lens' DescribeInstancesResponse Int
dirsStatus = lens _dirsStatus (\ s a -> s{_dirsStatus = a});

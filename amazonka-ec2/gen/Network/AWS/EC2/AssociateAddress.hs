{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AssociateAddress
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

-- | Associates an Elastic IP address with an instance or a network
-- interface.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or
-- in a VPC. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- [EC2-Classic, VPC in an EC2-VPC-only account] If the Elastic IP address
-- is already associated with a different instance, it is disassociated
-- from that instance and associated with the specified instance.
--
-- [VPC in an EC2-Classic account] If you don\'t specify a private IP
-- address, the Elastic IP address is associated with the primary IP
-- address. If the Elastic IP address is already associated with a
-- different instance or a network interface, you get an error unless you
-- allow reassociation.
--
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn\'t return an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html>
module Network.AWS.EC2.AssociateAddress
    (
    -- * Request
      AssociateAddress
    -- ** Request constructor
    , associateAddress
    -- ** Request lenses
    , assInstanceId
    , assAllocationId
    , assNetworkInterfaceId
    , assAllowReassociation
    , assPrivateIPAddress
    , assPublicIP
    , assDryRun

    -- * Response
    , AssociateAddressResponse
    -- ** Response constructor
    , associateAddressResponse
    -- ** Response lenses
    , assAssociationId
    , assStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'assInstanceId'
--
-- * 'assAllocationId'
--
-- * 'assNetworkInterfaceId'
--
-- * 'assAllowReassociation'
--
-- * 'assPrivateIPAddress'
--
-- * 'assPublicIP'
--
-- * 'assDryRun'
data AssociateAddress = AssociateAddress'
    { _assInstanceId         :: Maybe Text
    , _assAllocationId       :: Maybe Text
    , _assNetworkInterfaceId :: Maybe Text
    , _assAllowReassociation :: Maybe Bool
    , _assPrivateIPAddress   :: Maybe Text
    , _assPublicIP           :: Maybe Text
    , _assDryRun             :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'AssociateAddress' smart constructor.
associateAddress :: AssociateAddress
associateAddress =
    AssociateAddress'
    { _assInstanceId = Nothing
    , _assAllocationId = Nothing
    , _assNetworkInterfaceId = Nothing
    , _assAllowReassociation = Nothing
    , _assPrivateIPAddress = Nothing
    , _assPublicIP = Nothing
    , _assDryRun = Nothing
    }

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC,
-- you can specify either the instance ID or the network interface ID, but
-- not both. The operation fails if you specify an instance ID unless
-- exactly one network interface is attached.
assInstanceId :: Lens' AssociateAddress (Maybe Text)
assInstanceId = lens _assInstanceId (\ s a -> s{_assInstanceId = a});

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
assAllocationId :: Lens' AssociateAddress (Maybe Text)
assAllocationId = lens _assAllocationId (\ s a -> s{_assAllocationId = a});

-- | [EC2-VPC] The ID of the network interface. If the instance has more than
-- one network interface, you must specify a network interface ID.
assNetworkInterfaceId :: Lens' AssociateAddress (Maybe Text)
assNetworkInterfaceId = lens _assNetworkInterfaceId (\ s a -> s{_assNetworkInterfaceId = a});

-- | [EC2-VPC] Allows an Elastic IP address that is already associated with
-- an instance or network interface to be re-associated with the specified
-- instance or network interface. Otherwise, the operation fails.
--
-- Default: @false@
assAllowReassociation :: Lens' AssociateAddress (Maybe Bool)
assAllowReassociation = lens _assAllowReassociation (\ s a -> s{_assAllowReassociation = a});

-- | [EC2-VPC] The primary or secondary private IP address to associate with
-- the Elastic IP address. If no private IP address is specified, the
-- Elastic IP address is associated with the primary private IP address.
assPrivateIPAddress :: Lens' AssociateAddress (Maybe Text)
assPrivateIPAddress = lens _assPrivateIPAddress (\ s a -> s{_assPrivateIPAddress = a});

-- | The Elastic IP address. This is required for EC2-Classic.
assPublicIP :: Lens' AssociateAddress (Maybe Text)
assPublicIP = lens _assPublicIP (\ s a -> s{_assPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
assDryRun :: Lens' AssociateAddress (Maybe Bool)
assDryRun = lens _assDryRun (\ s a -> s{_assDryRun = a});

instance AWSRequest AssociateAddress where
        type Sv AssociateAddress = EC2
        type Rs AssociateAddress = AssociateAddressResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AssociateAddressResponse' <$>
                   (x .@? "associationId") <*> (pure (fromEnum s)))

instance ToHeaders AssociateAddress where
        toHeaders = const mempty

instance ToPath AssociateAddress where
        toPath = const "/"

instance ToQuery AssociateAddress where
        toQuery AssociateAddress'{..}
          = mconcat
              ["Action" =: ("AssociateAddress" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceId" =: _assInstanceId,
               "AllocationId" =: _assAllocationId,
               "NetworkInterfaceId" =: _assNetworkInterfaceId,
               "AllowReassociation" =: _assAllowReassociation,
               "PrivateIpAddress" =: _assPrivateIPAddress,
               "PublicIp" =: _assPublicIP, "DryRun" =: _assDryRun]

-- | /See:/ 'associateAddressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'assAssociationId'
--
-- * 'assStatus'
data AssociateAddressResponse = AssociateAddressResponse'
    { _assAssociationId :: Maybe Text
    , _assStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'AssociateAddressResponse' smart constructor.
associateAddressResponse :: Int -> AssociateAddressResponse
associateAddressResponse pStatus =
    AssociateAddressResponse'
    { _assAssociationId = Nothing
    , _assStatus = pStatus
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP
-- address with an instance.
assAssociationId :: Lens' AssociateAddressResponse (Maybe Text)
assAssociationId = lens _assAssociationId (\ s a -> s{_assAssociationId = a});

-- | FIXME: Undocumented member.
assStatus :: Lens' AssociateAddressResponse Int
assStatus = lens _assStatus (\ s a -> s{_assStatus = a});

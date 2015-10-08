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
-- Module      : Network.AWS.EC2.AssociateAddress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Elastic IP address with an instance or a network
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html AWS API Reference> for AssociateAddress.
module Network.AWS.EC2.AssociateAddress
    (
    -- * Creating a Request
      associateAddress
    , AssociateAddress
    -- * Request Lenses
    , aasInstanceId
    , aasAllocationId
    , aasNetworkInterfaceId
    , aasAllowReassociation
    , aasPrivateIPAddress
    , aasPublicIP
    , aasDryRun

    -- * Destructuring the Response
    , associateAddressResponse
    , AssociateAddressResponse
    -- * Response Lenses
    , arsAssociationId
    , arsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateAddress' smart constructor.
data AssociateAddress = AssociateAddress'
    { _aasInstanceId         :: !(Maybe Text)
    , _aasAllocationId       :: !(Maybe Text)
    , _aasNetworkInterfaceId :: !(Maybe Text)
    , _aasAllowReassociation :: !(Maybe Bool)
    , _aasPrivateIPAddress   :: !(Maybe Text)
    , _aasPublicIP           :: !(Maybe Text)
    , _aasDryRun             :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aasInstanceId'
--
-- * 'aasAllocationId'
--
-- * 'aasNetworkInterfaceId'
--
-- * 'aasAllowReassociation'
--
-- * 'aasPrivateIPAddress'
--
-- * 'aasPublicIP'
--
-- * 'aasDryRun'
associateAddress
    :: AssociateAddress
associateAddress =
    AssociateAddress'
    { _aasInstanceId = Nothing
    , _aasAllocationId = Nothing
    , _aasNetworkInterfaceId = Nothing
    , _aasAllowReassociation = Nothing
    , _aasPrivateIPAddress = Nothing
    , _aasPublicIP = Nothing
    , _aasDryRun = Nothing
    }

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC,
-- you can specify either the instance ID or the network interface ID, but
-- not both. The operation fails if you specify an instance ID unless
-- exactly one network interface is attached.
aasInstanceId :: Lens' AssociateAddress (Maybe Text)
aasInstanceId = lens _aasInstanceId (\ s a -> s{_aasInstanceId = a});

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
aasAllocationId :: Lens' AssociateAddress (Maybe Text)
aasAllocationId = lens _aasAllocationId (\ s a -> s{_aasAllocationId = a});

-- | [EC2-VPC] The ID of the network interface. If the instance has more than
-- one network interface, you must specify a network interface ID.
aasNetworkInterfaceId :: Lens' AssociateAddress (Maybe Text)
aasNetworkInterfaceId = lens _aasNetworkInterfaceId (\ s a -> s{_aasNetworkInterfaceId = a});

-- | [EC2-VPC] Allows an Elastic IP address that is already associated with
-- an instance or network interface to be re-associated with the specified
-- instance or network interface. Otherwise, the operation fails.
--
-- Default: 'false'
aasAllowReassociation :: Lens' AssociateAddress (Maybe Bool)
aasAllowReassociation = lens _aasAllowReassociation (\ s a -> s{_aasAllowReassociation = a});

-- | [EC2-VPC] The primary or secondary private IP address to associate with
-- the Elastic IP address. If no private IP address is specified, the
-- Elastic IP address is associated with the primary private IP address.
aasPrivateIPAddress :: Lens' AssociateAddress (Maybe Text)
aasPrivateIPAddress = lens _aasPrivateIPAddress (\ s a -> s{_aasPrivateIPAddress = a});

-- | The Elastic IP address. This is required for EC2-Classic.
aasPublicIP :: Lens' AssociateAddress (Maybe Text)
aasPublicIP = lens _aasPublicIP (\ s a -> s{_aasPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
aasDryRun :: Lens' AssociateAddress (Maybe Bool)
aasDryRun = lens _aasDryRun (\ s a -> s{_aasDryRun = a});

instance AWSRequest AssociateAddress where
        type Rs AssociateAddress = AssociateAddressResponse
        request = postQuery eC2
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
               "Version" =: ("2015-10-01" :: ByteString),
               "InstanceId" =: _aasInstanceId,
               "AllocationId" =: _aasAllocationId,
               "NetworkInterfaceId" =: _aasNetworkInterfaceId,
               "AllowReassociation" =: _aasAllowReassociation,
               "PrivateIpAddress" =: _aasPrivateIPAddress,
               "PublicIp" =: _aasPublicIP, "DryRun" =: _aasDryRun]

-- | /See:/ 'associateAddressResponse' smart constructor.
data AssociateAddressResponse = AssociateAddressResponse'
    { _arsAssociationId  :: !(Maybe Text)
    , _arsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateAddressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arsAssociationId'
--
-- * 'arsResponseStatus'
associateAddressResponse
    :: Int -- ^ 'arsResponseStatus'
    -> AssociateAddressResponse
associateAddressResponse pResponseStatus_ =
    AssociateAddressResponse'
    { _arsAssociationId = Nothing
    , _arsResponseStatus = pResponseStatus_
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP
-- address with an instance.
arsAssociationId :: Lens' AssociateAddressResponse (Maybe Text)
arsAssociationId = lens _arsAssociationId (\ s a -> s{_arsAssociationId = a});

-- | The response status code.
arsResponseStatus :: Lens' AssociateAddressResponse Int
arsResponseStatus = lens _arsResponseStatus (\ s a -> s{_arsResponseStatus = a});

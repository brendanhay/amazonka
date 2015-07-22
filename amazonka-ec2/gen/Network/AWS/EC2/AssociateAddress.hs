{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateAddress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssociateAddress.html>
module Network.AWS.EC2.AssociateAddress
    (
    -- * Request
      AssociateAddress
    -- ** Request constructor
    , associateAddress
    -- ** Request lenses
    , arqInstanceId
    , arqAllocationId
    , arqNetworkInterfaceId
    , arqAllowReassociation
    , arqPrivateIPAddress
    , arqPublicIP
    , arqDryRun

    -- * Response
    , AssociateAddressResponse
    -- ** Response constructor
    , associateAddressResponse
    -- ** Response lenses
    , arsAssociationId
    , arsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arqInstanceId'
--
-- * 'arqAllocationId'
--
-- * 'arqNetworkInterfaceId'
--
-- * 'arqAllowReassociation'
--
-- * 'arqPrivateIPAddress'
--
-- * 'arqPublicIP'
--
-- * 'arqDryRun'
data AssociateAddress = AssociateAddress'
    { _arqInstanceId         :: !(Maybe Text)
    , _arqAllocationId       :: !(Maybe Text)
    , _arqNetworkInterfaceId :: !(Maybe Text)
    , _arqAllowReassociation :: !(Maybe Bool)
    , _arqPrivateIPAddress   :: !(Maybe Text)
    , _arqPublicIP           :: !(Maybe Text)
    , _arqDryRun             :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateAddress' smart constructor.
associateAddress :: AssociateAddress
associateAddress =
    AssociateAddress'
    { _arqInstanceId = Nothing
    , _arqAllocationId = Nothing
    , _arqNetworkInterfaceId = Nothing
    , _arqAllowReassociation = Nothing
    , _arqPrivateIPAddress = Nothing
    , _arqPublicIP = Nothing
    , _arqDryRun = Nothing
    }

-- | The ID of the instance. This is required for EC2-Classic. For EC2-VPC,
-- you can specify either the instance ID or the network interface ID, but
-- not both. The operation fails if you specify an instance ID unless
-- exactly one network interface is attached.
arqInstanceId :: Lens' AssociateAddress (Maybe Text)
arqInstanceId = lens _arqInstanceId (\ s a -> s{_arqInstanceId = a});

-- | [EC2-VPC] The allocation ID. This is required for EC2-VPC.
arqAllocationId :: Lens' AssociateAddress (Maybe Text)
arqAllocationId = lens _arqAllocationId (\ s a -> s{_arqAllocationId = a});

-- | [EC2-VPC] The ID of the network interface. If the instance has more than
-- one network interface, you must specify a network interface ID.
arqNetworkInterfaceId :: Lens' AssociateAddress (Maybe Text)
arqNetworkInterfaceId = lens _arqNetworkInterfaceId (\ s a -> s{_arqNetworkInterfaceId = a});

-- | [EC2-VPC] Allows an Elastic IP address that is already associated with
-- an instance or network interface to be re-associated with the specified
-- instance or network interface. Otherwise, the operation fails.
--
-- Default: @false@
arqAllowReassociation :: Lens' AssociateAddress (Maybe Bool)
arqAllowReassociation = lens _arqAllowReassociation (\ s a -> s{_arqAllowReassociation = a});

-- | [EC2-VPC] The primary or secondary private IP address to associate with
-- the Elastic IP address. If no private IP address is specified, the
-- Elastic IP address is associated with the primary private IP address.
arqPrivateIPAddress :: Lens' AssociateAddress (Maybe Text)
arqPrivateIPAddress = lens _arqPrivateIPAddress (\ s a -> s{_arqPrivateIPAddress = a});

-- | The Elastic IP address. This is required for EC2-Classic.
arqPublicIP :: Lens' AssociateAddress (Maybe Text)
arqPublicIP = lens _arqPublicIP (\ s a -> s{_arqPublicIP = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
arqDryRun :: Lens' AssociateAddress (Maybe Bool)
arqDryRun = lens _arqDryRun (\ s a -> s{_arqDryRun = a});

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
               "InstanceId" =: _arqInstanceId,
               "AllocationId" =: _arqAllocationId,
               "NetworkInterfaceId" =: _arqNetworkInterfaceId,
               "AllowReassociation" =: _arqAllowReassociation,
               "PrivateIpAddress" =: _arqPrivateIPAddress,
               "PublicIp" =: _arqPublicIP, "DryRun" =: _arqDryRun]

-- | /See:/ 'associateAddressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arsAssociationId'
--
-- * 'arsStatus'
data AssociateAddressResponse = AssociateAddressResponse'
    { _arsAssociationId :: !(Maybe Text)
    , _arsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateAddressResponse' smart constructor.
associateAddressResponse :: Int -> AssociateAddressResponse
associateAddressResponse pStatus =
    AssociateAddressResponse'
    { _arsAssociationId = Nothing
    , _arsStatus = pStatus
    }

-- | [EC2-VPC] The ID that represents the association of the Elastic IP
-- address with an instance.
arsAssociationId :: Lens' AssociateAddressResponse (Maybe Text)
arsAssociationId = lens _arsAssociationId (\ s a -> s{_arsAssociationId = a});

-- | FIXME: Undocumented member.
arsStatus :: Lens' AssociateAddressResponse Int
arsStatus = lens _arsStatus (\ s a -> s{_arsStatus = a});

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
-- Module      : Network.AWS.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network interface in the specified subnet.
--
-- For more information about network interfaces, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html AWS API Reference> for CreateNetworkInterface.
module Network.AWS.EC2.CreateNetworkInterface
    (
    -- * Creating a Request
      createNetworkInterface
    , CreateNetworkInterface
    -- * Request Lenses
    , cniPrivateIPAddresses
    , cniGroups
    , cniPrivateIPAddress
    , cniSecondaryPrivateIPAddressCount
    , cniDryRun
    , cniDescription
    , cniSubnetId

    -- * Destructuring the Response
    , createNetworkInterfaceResponse
    , CreateNetworkInterfaceResponse
    -- * Response Lenses
    , cnirsNetworkInterface
    , cnirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createNetworkInterface' smart constructor.
data CreateNetworkInterface = CreateNetworkInterface'
    { _cniPrivateIPAddresses             :: !(Maybe [PrivateIPAddressSpecification])
    , _cniGroups                         :: !(Maybe [Text])
    , _cniPrivateIPAddress               :: !(Maybe Text)
    , _cniSecondaryPrivateIPAddressCount :: !(Maybe Int)
    , _cniDryRun                         :: !(Maybe Bool)
    , _cniDescription                    :: !(Maybe Text)
    , _cniSubnetId                       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cniPrivateIPAddresses'
--
-- * 'cniGroups'
--
-- * 'cniPrivateIPAddress'
--
-- * 'cniSecondaryPrivateIPAddressCount'
--
-- * 'cniDryRun'
--
-- * 'cniDescription'
--
-- * 'cniSubnetId'
createNetworkInterface
    :: Text -- ^ 'cniSubnetId'
    -> CreateNetworkInterface
createNetworkInterface pSubnetId_ =
    CreateNetworkInterface'
    { _cniPrivateIPAddresses = Nothing
    , _cniGroups = Nothing
    , _cniPrivateIPAddress = Nothing
    , _cniSecondaryPrivateIPAddressCount = Nothing
    , _cniDryRun = Nothing
    , _cniDescription = Nothing
    , _cniSubnetId = pSubnetId_
    }

-- | One or more private IP addresses.
cniPrivateIPAddresses :: Lens' CreateNetworkInterface [PrivateIPAddressSpecification]
cniPrivateIPAddresses = lens _cniPrivateIPAddresses (\ s a -> s{_cniPrivateIPAddresses = a}) . _Default . _Coerce;

-- | The IDs of one or more security groups.
cniGroups :: Lens' CreateNetworkInterface [Text]
cniGroups = lens _cniGroups (\ s a -> s{_cniGroups = a}) . _Default . _Coerce;

-- | The primary private IP address of the network interface. If you don\'t
-- specify an IP address, Amazon EC2 selects one for you from the subnet
-- range. If you specify an IP address, you cannot indicate any IP
-- addresses specified in 'privateIpAddresses' as primary (only one IP
-- address can be designated as primary).
cniPrivateIPAddress :: Lens' CreateNetworkInterface (Maybe Text)
cniPrivateIPAddress = lens _cniPrivateIPAddress (\ s a -> s{_cniPrivateIPAddress = a});

-- | The number of secondary private IP addresses to assign to a network
-- interface. When you specify a number of secondary IP addresses, Amazon
-- EC2 selects these IP addresses within the subnet range. You can\'t
-- specify this option and specify more than one private IP address using
-- 'privateIpAddresses'.
--
-- The number of IP addresses you can assign to a network interface varies
-- by instance type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI Private IP Addresses Per ENI Per Instance Type>
-- in the /Amazon Elastic Compute Cloud User Guide/.
cniSecondaryPrivateIPAddressCount :: Lens' CreateNetworkInterface (Maybe Int)
cniSecondaryPrivateIPAddressCount = lens _cniSecondaryPrivateIPAddressCount (\ s a -> s{_cniSecondaryPrivateIPAddressCount = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
cniDryRun :: Lens' CreateNetworkInterface (Maybe Bool)
cniDryRun = lens _cniDryRun (\ s a -> s{_cniDryRun = a});

-- | A description for the network interface.
cniDescription :: Lens' CreateNetworkInterface (Maybe Text)
cniDescription = lens _cniDescription (\ s a -> s{_cniDescription = a});

-- | The ID of the subnet to associate with the network interface.
cniSubnetId :: Lens' CreateNetworkInterface Text
cniSubnetId = lens _cniSubnetId (\ s a -> s{_cniSubnetId = a});

instance AWSRequest CreateNetworkInterface where
        type Rs CreateNetworkInterface =
             CreateNetworkInterfaceResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 CreateNetworkInterfaceResponse' <$>
                   (x .@? "networkInterface") <*> (pure (fromEnum s)))

instance ToHeaders CreateNetworkInterface where
        toHeaders = const mempty

instance ToPath CreateNetworkInterface where
        toPath = const "/"

instance ToQuery CreateNetworkInterface where
        toQuery CreateNetworkInterface'{..}
          = mconcat
              ["Action" =:
                 ("CreateNetworkInterface" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "PrivateIpAddresses" <$>
                    _cniPrivateIPAddresses),
               toQuery
                 (toQueryList "SecurityGroupId" <$> _cniGroups),
               "PrivateIpAddress" =: _cniPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _cniSecondaryPrivateIPAddressCount,
               "DryRun" =: _cniDryRun,
               "Description" =: _cniDescription,
               "SubnetId" =: _cniSubnetId]

-- | /See:/ 'createNetworkInterfaceResponse' smart constructor.
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
    { _cnirsNetworkInterface :: !(Maybe NetworkInterface)
    , _cnirsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateNetworkInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnirsNetworkInterface'
--
-- * 'cnirsStatus'
createNetworkInterfaceResponse
    :: Int -- ^ 'cnirsStatus'
    -> CreateNetworkInterfaceResponse
createNetworkInterfaceResponse pStatus_ =
    CreateNetworkInterfaceResponse'
    { _cnirsNetworkInterface = Nothing
    , _cnirsStatus = pStatus_
    }

-- | Information about the network interface.
cnirsNetworkInterface :: Lens' CreateNetworkInterfaceResponse (Maybe NetworkInterface)
cnirsNetworkInterface = lens _cnirsNetworkInterface (\ s a -> s{_cnirsNetworkInterface = a});

-- | The response status code.
cnirsStatus :: Lens' CreateNetworkInterfaceResponse Int
cnirsStatus = lens _cnirsStatus (\ s a -> s{_cnirsStatus = a});

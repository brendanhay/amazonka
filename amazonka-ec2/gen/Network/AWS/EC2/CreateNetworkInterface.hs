{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a network interface in the specified subnet.
--
-- For more information about network interfaces, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html>
module Network.AWS.EC2.CreateNetworkInterface
    (
    -- * Request
      CreateNetworkInterface
    -- ** Request constructor
    , createNetworkInterface
    -- ** Request lenses
    , cnirqPrivateIPAddresses
    , cnirqGroups
    , cnirqPrivateIPAddress
    , cnirqSecondaryPrivateIPAddressCount
    , cnirqDryRun
    , cnirqDescription
    , cnirqSubnetId

    -- * Response
    , CreateNetworkInterfaceResponse
    -- ** Response constructor
    , createNetworkInterfaceResponse
    -- ** Response lenses
    , cnirsNetworkInterface
    , cnirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnirqPrivateIPAddresses'
--
-- * 'cnirqGroups'
--
-- * 'cnirqPrivateIPAddress'
--
-- * 'cnirqSecondaryPrivateIPAddressCount'
--
-- * 'cnirqDryRun'
--
-- * 'cnirqDescription'
--
-- * 'cnirqSubnetId'
data CreateNetworkInterface = CreateNetworkInterface'
    { _cnirqPrivateIPAddresses             :: !(Maybe [PrivateIPAddressSpecification])
    , _cnirqGroups                         :: !(Maybe [Text])
    , _cnirqPrivateIPAddress               :: !(Maybe Text)
    , _cnirqSecondaryPrivateIPAddressCount :: !(Maybe Int)
    , _cnirqDryRun                         :: !(Maybe Bool)
    , _cnirqDescription                    :: !(Maybe Text)
    , _cnirqSubnetId                       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkInterface' smart constructor.
createNetworkInterface :: Text -> CreateNetworkInterface
createNetworkInterface pSubnetId =
    CreateNetworkInterface'
    { _cnirqPrivateIPAddresses = Nothing
    , _cnirqGroups = Nothing
    , _cnirqPrivateIPAddress = Nothing
    , _cnirqSecondaryPrivateIPAddressCount = Nothing
    , _cnirqDryRun = Nothing
    , _cnirqDescription = Nothing
    , _cnirqSubnetId = pSubnetId
    }

-- | One or more private IP addresses.
cnirqPrivateIPAddresses :: Lens' CreateNetworkInterface [PrivateIPAddressSpecification]
cnirqPrivateIPAddresses = lens _cnirqPrivateIPAddresses (\ s a -> s{_cnirqPrivateIPAddresses = a}) . _Default;

-- | The IDs of one or more security groups.
cnirqGroups :: Lens' CreateNetworkInterface [Text]
cnirqGroups = lens _cnirqGroups (\ s a -> s{_cnirqGroups = a}) . _Default;

-- | The primary private IP address of the network interface. If you don\'t
-- specify an IP address, Amazon EC2 selects one for you from the subnet
-- range. If you specify an IP address, you cannot indicate any IP
-- addresses specified in @privateIpAddresses@ as primary (only one IP
-- address can be designated as primary).
cnirqPrivateIPAddress :: Lens' CreateNetworkInterface (Maybe Text)
cnirqPrivateIPAddress = lens _cnirqPrivateIPAddress (\ s a -> s{_cnirqPrivateIPAddress = a});

-- | The number of secondary private IP addresses to assign to a network
-- interface. When you specify a number of secondary IP addresses, Amazon
-- EC2 selects these IP addresses within the subnet range. You can\'t
-- specify this option and specify more than one private IP address using
-- @privateIpAddresses@.
--
-- The number of IP addresses you can assign to a network interface varies
-- by instance type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI Private IP Addresses Per ENI Per Instance Type>
-- in the /Amazon Elastic Compute Cloud User Guide/.
cnirqSecondaryPrivateIPAddressCount :: Lens' CreateNetworkInterface (Maybe Int)
cnirqSecondaryPrivateIPAddressCount = lens _cnirqSecondaryPrivateIPAddressCount (\ s a -> s{_cnirqSecondaryPrivateIPAddressCount = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cnirqDryRun :: Lens' CreateNetworkInterface (Maybe Bool)
cnirqDryRun = lens _cnirqDryRun (\ s a -> s{_cnirqDryRun = a});

-- | A description for the network interface.
cnirqDescription :: Lens' CreateNetworkInterface (Maybe Text)
cnirqDescription = lens _cnirqDescription (\ s a -> s{_cnirqDescription = a});

-- | The ID of the subnet to associate with the network interface.
cnirqSubnetId :: Lens' CreateNetworkInterface Text
cnirqSubnetId = lens _cnirqSubnetId (\ s a -> s{_cnirqSubnetId = a});

instance AWSRequest CreateNetworkInterface where
        type Sv CreateNetworkInterface = EC2
        type Rs CreateNetworkInterface =
             CreateNetworkInterfaceResponse
        request = post
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
                 (toQueryList "item" <$> _cnirqPrivateIPAddresses),
               toQuery
                 (toQueryList "SecurityGroupId" <$> _cnirqGroups),
               "PrivateIpAddress" =: _cnirqPrivateIPAddress,
               "SecondaryPrivateIpAddressCount" =:
                 _cnirqSecondaryPrivateIPAddressCount,
               "DryRun" =: _cnirqDryRun,
               "Description" =: _cnirqDescription,
               "SubnetId" =: _cnirqSubnetId]

-- | /See:/ 'createNetworkInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnirsNetworkInterface'
--
-- * 'cnirsStatus'
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
    { _cnirsNetworkInterface :: !(Maybe NetworkInterface)
    , _cnirsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateNetworkInterfaceResponse' smart constructor.
createNetworkInterfaceResponse :: Int -> CreateNetworkInterfaceResponse
createNetworkInterfaceResponse pStatus =
    CreateNetworkInterfaceResponse'
    { _cnirsNetworkInterface = Nothing
    , _cnirsStatus = pStatus
    }

-- | Information about the network interface.
cnirsNetworkInterface :: Lens' CreateNetworkInterfaceResponse (Maybe NetworkInterface)
cnirsNetworkInterface = lens _cnirsNetworkInterface (\ s a -> s{_cnirsNetworkInterface = a});

-- | FIXME: Undocumented member.
cnirsStatus :: Lens' CreateNetworkInterfaceResponse Int
cnirsStatus = lens _cnirsStatus (\ s a -> s{_cnirsStatus = a});

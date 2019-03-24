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
-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute. You can only modify one attribute at a time.
--
--
module Network.AWS.EC2.ModifySubnetAttribute
    (
    -- * Creating a Request
      modifySubnetAttribute
    , ModifySubnetAttribute
    -- * Request Lenses
    , msaAssignIPv6AddressOnCreation
    , msaMapPublicIPOnLaunch
    , msaSubnetId

    -- * Destructuring the Response
    , modifySubnetAttributeResponse
    , ModifySubnetAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { _msaAssignIPv6AddressOnCreation :: !(Maybe AttributeBooleanValue)
  , _msaMapPublicIPOnLaunch         :: !(Maybe AttributeBooleanValue)
  , _msaSubnetId                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySubnetAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msaAssignIPv6AddressOnCreation' - Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address).  If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
--
-- * 'msaMapPublicIPOnLaunch' - Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned a public IPv4 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives a public IPv4 address).
--
-- * 'msaSubnetId' - The ID of the subnet.
modifySubnetAttribute
    :: Text -- ^ 'msaSubnetId'
    -> ModifySubnetAttribute
modifySubnetAttribute pSubnetId_ =
  ModifySubnetAttribute'
    { _msaAssignIPv6AddressOnCreation = Nothing
    , _msaMapPublicIPOnLaunch = Nothing
    , _msaSubnetId = pSubnetId_
    }


-- | Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned an IPv6 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives an IPv6 address).  If you enable the IPv6 addressing feature for your subnet, your network interface or instance only receives an IPv6 address if it's created using version @2016-11-15@ or later of the Amazon EC2 API.
msaAssignIPv6AddressOnCreation :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msaAssignIPv6AddressOnCreation = lens _msaAssignIPv6AddressOnCreation (\ s a -> s{_msaAssignIPv6AddressOnCreation = a})

-- | Specify @true@ to indicate that network interfaces created in the specified subnet should be assigned a public IPv4 address. This includes a network interface that's created when launching an instance into the subnet (the instance therefore receives a public IPv4 address).
msaMapPublicIPOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msaMapPublicIPOnLaunch = lens _msaMapPublicIPOnLaunch (\ s a -> s{_msaMapPublicIPOnLaunch = a})

-- | The ID of the subnet.
msaSubnetId :: Lens' ModifySubnetAttribute Text
msaSubnetId = lens _msaSubnetId (\ s a -> s{_msaSubnetId = a})

instance AWSRequest ModifySubnetAttribute where
        type Rs ModifySubnetAttribute =
             ModifySubnetAttributeResponse
        request = postQuery ec2
        response = receiveNull ModifySubnetAttributeResponse'

instance Hashable ModifySubnetAttribute where

instance NFData ModifySubnetAttribute where

instance ToHeaders ModifySubnetAttribute where
        toHeaders = const mempty

instance ToPath ModifySubnetAttribute where
        toPath = const "/"

instance ToQuery ModifySubnetAttribute where
        toQuery ModifySubnetAttribute'{..}
          = mconcat
              ["Action" =: ("ModifySubnetAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AssignIpv6AddressOnCreation" =:
                 _msaAssignIPv6AddressOnCreation,
               "MapPublicIpOnLaunch" =: _msaMapPublicIPOnLaunch,
               "SubnetId" =: _msaSubnetId]

-- | /See:/ 'modifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse =
  ModifySubnetAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySubnetAttributeResponse' with the minimum fields required to make a request.
--
modifySubnetAttributeResponse
    :: ModifySubnetAttributeResponse
modifySubnetAttributeResponse = ModifySubnetAttributeResponse'


instance NFData ModifySubnetAttributeResponse where

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
-- Module      : Network.AWS.ELBv2.SetIPAddressType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.
--
--
-- Note that Network Load Balancers must use @ipv4@ .
--
module Network.AWS.ELBv2.SetIPAddressType
    (
    -- * Creating a Request
      setIPAddressType
    , SetIPAddressType
    -- * Request Lenses
    , siatLoadBalancerARN
    , siatIPAddressType

    -- * Destructuring the Response
    , setIPAddressTypeResponse
    , SetIPAddressTypeResponse
    -- * Response Lenses
    , siatrsIPAddressType
    , siatrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setIPAddressType' smart constructor.
data SetIPAddressType = SetIPAddressType'
  { _siatLoadBalancerARN :: !Text
  , _siatIPAddressType   :: !IPAddressType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIPAddressType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siatLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'siatIPAddressType' - The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
setIPAddressType
    :: Text -- ^ 'siatLoadBalancerARN'
    -> IPAddressType -- ^ 'siatIPAddressType'
    -> SetIPAddressType
setIPAddressType pLoadBalancerARN_ pIPAddressType_ =
  SetIPAddressType'
    { _siatLoadBalancerARN = pLoadBalancerARN_
    , _siatIPAddressType = pIPAddressType_
    }


-- | The Amazon Resource Name (ARN) of the load balancer.
siatLoadBalancerARN :: Lens' SetIPAddressType Text
siatLoadBalancerARN = lens _siatLoadBalancerARN (\ s a -> s{_siatLoadBalancerARN = a})

-- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
siatIPAddressType :: Lens' SetIPAddressType IPAddressType
siatIPAddressType = lens _siatIPAddressType (\ s a -> s{_siatIPAddressType = a})

instance AWSRequest SetIPAddressType where
        type Rs SetIPAddressType = SetIPAddressTypeResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "SetIpAddressTypeResult"
              (\ s h x ->
                 SetIPAddressTypeResponse' <$>
                   (x .@? "IpAddressType") <*> (pure (fromEnum s)))

instance Hashable SetIPAddressType where

instance NFData SetIPAddressType where

instance ToHeaders SetIPAddressType where
        toHeaders = const mempty

instance ToPath SetIPAddressType where
        toPath = const "/"

instance ToQuery SetIPAddressType where
        toQuery SetIPAddressType'{..}
          = mconcat
              ["Action" =: ("SetIpAddressType" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "LoadBalancerArn" =: _siatLoadBalancerARN,
               "IpAddressType" =: _siatIPAddressType]

-- | /See:/ 'setIPAddressTypeResponse' smart constructor.
data SetIPAddressTypeResponse = SetIPAddressTypeResponse'
  { _siatrsIPAddressType  :: !(Maybe IPAddressType)
  , _siatrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetIPAddressTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siatrsIPAddressType' - The IP address type.
--
-- * 'siatrsResponseStatus' - -- | The response status code.
setIPAddressTypeResponse
    :: Int -- ^ 'siatrsResponseStatus'
    -> SetIPAddressTypeResponse
setIPAddressTypeResponse pResponseStatus_ =
  SetIPAddressTypeResponse'
    {_siatrsIPAddressType = Nothing, _siatrsResponseStatus = pResponseStatus_}


-- | The IP address type.
siatrsIPAddressType :: Lens' SetIPAddressTypeResponse (Maybe IPAddressType)
siatrsIPAddressType = lens _siatrsIPAddressType (\ s a -> s{_siatrsIPAddressType = a})

-- | -- | The response status code.
siatrsResponseStatus :: Lens' SetIPAddressTypeResponse Int
siatrsResponseStatus = lens _siatrsResponseStatus (\ s a -> s{_siatrsResponseStatus = a})

instance NFData SetIPAddressTypeResponse where

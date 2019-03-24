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
-- Module      : Network.AWS.EC2.AdvertiseByoipCidr
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Advertises an IPv4 address range that is provisioned for use with your AWS resources through bring your own IP addresses (BYOIP).
--
--
-- You can perform this operation at most once every 10 seconds, even if you specify different address ranges each time.
--
-- We recommend that you stop advertising the BYOIP CIDR from other locations when you advertise it from AWS. To minimize down time, you can configure your AWS resources to use an address from a BYOIP CIDR before it is advertised, and then simultaneously stop advertising it from the current location and start advertising it through AWS.
--
-- It can take a few minutes before traffic to the specified addresses starts routing to AWS because of BGP propagation delays.
--
-- To stop advertising the BYOIP CIDR, use 'WithdrawByoipCidr' .
--
module Network.AWS.EC2.AdvertiseByoipCidr
    (
    -- * Creating a Request
      advertiseByoipCidr
    , AdvertiseByoipCidr
    -- * Request Lenses
    , abcDryRun
    , abcCidr

    -- * Destructuring the Response
    , advertiseByoipCidrResponse
    , AdvertiseByoipCidrResponse
    -- * Response Lenses
    , abcrsByoipCidr
    , abcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'advertiseByoipCidr' smart constructor.
data AdvertiseByoipCidr = AdvertiseByoipCidr'
  { _abcDryRun :: !(Maybe Bool)
  , _abcCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdvertiseByoipCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'abcCidr' - The IPv4 address range, in CIDR notation. This must be the exact range that you provisioned. You can't advertise only a portion of the provisioned range.
advertiseByoipCidr
    :: Text -- ^ 'abcCidr'
    -> AdvertiseByoipCidr
advertiseByoipCidr pCidr_ =
  AdvertiseByoipCidr' {_abcDryRun = Nothing, _abcCidr = pCidr_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
abcDryRun :: Lens' AdvertiseByoipCidr (Maybe Bool)
abcDryRun = lens _abcDryRun (\ s a -> s{_abcDryRun = a})

-- | The IPv4 address range, in CIDR notation. This must be the exact range that you provisioned. You can't advertise only a portion of the provisioned range.
abcCidr :: Lens' AdvertiseByoipCidr Text
abcCidr = lens _abcCidr (\ s a -> s{_abcCidr = a})

instance AWSRequest AdvertiseByoipCidr where
        type Rs AdvertiseByoipCidr =
             AdvertiseByoipCidrResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AdvertiseByoipCidrResponse' <$>
                   (x .@? "byoipCidr") <*> (pure (fromEnum s)))

instance Hashable AdvertiseByoipCidr where

instance NFData AdvertiseByoipCidr where

instance ToHeaders AdvertiseByoipCidr where
        toHeaders = const mempty

instance ToPath AdvertiseByoipCidr where
        toPath = const "/"

instance ToQuery AdvertiseByoipCidr where
        toQuery AdvertiseByoipCidr'{..}
          = mconcat
              ["Action" =: ("AdvertiseByoipCidr" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _abcDryRun, "Cidr" =: _abcCidr]

-- | /See:/ 'advertiseByoipCidrResponse' smart constructor.
data AdvertiseByoipCidrResponse = AdvertiseByoipCidrResponse'
  { _abcrsByoipCidr      :: !(Maybe ByoipCidr)
  , _abcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdvertiseByoipCidrResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abcrsByoipCidr' - Information about the address range.
--
-- * 'abcrsResponseStatus' - -- | The response status code.
advertiseByoipCidrResponse
    :: Int -- ^ 'abcrsResponseStatus'
    -> AdvertiseByoipCidrResponse
advertiseByoipCidrResponse pResponseStatus_ =
  AdvertiseByoipCidrResponse'
    {_abcrsByoipCidr = Nothing, _abcrsResponseStatus = pResponseStatus_}


-- | Information about the address range.
abcrsByoipCidr :: Lens' AdvertiseByoipCidrResponse (Maybe ByoipCidr)
abcrsByoipCidr = lens _abcrsByoipCidr (\ s a -> s{_abcrsByoipCidr = a})

-- | -- | The response status code.
abcrsResponseStatus :: Lens' AdvertiseByoipCidrResponse Int
abcrsResponseStatus = lens _abcrsResponseStatus (\ s a -> s{_abcrsResponseStatus = a})

instance NFData AdvertiseByoipCidrResponse where

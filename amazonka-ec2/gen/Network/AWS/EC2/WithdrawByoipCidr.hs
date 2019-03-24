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
-- Module      : Network.AWS.EC2.WithdrawByoipCidr
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an IPv4 address range that is provisioned as an address pool.
--
--
-- You can perform this operation at most once every 10 seconds, even if you specify different address ranges each time.
--
-- It can take a few minutes before traffic to the specified addresses stops routing to AWS because of BGP propagation delays.
--
module Network.AWS.EC2.WithdrawByoipCidr
    (
    -- * Creating a Request
      withdrawByoipCidr
    , WithdrawByoipCidr
    -- * Request Lenses
    , wbcDryRun
    , wbcCidr

    -- * Destructuring the Response
    , withdrawByoipCidrResponse
    , WithdrawByoipCidrResponse
    -- * Response Lenses
    , wbcrsByoipCidr
    , wbcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'withdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { _wbcDryRun :: !(Maybe Bool)
  , _wbcCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WithdrawByoipCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'wbcCidr' - The public IPv4 address range, in CIDR notation.
withdrawByoipCidr
    :: Text -- ^ 'wbcCidr'
    -> WithdrawByoipCidr
withdrawByoipCidr pCidr_ =
  WithdrawByoipCidr' {_wbcDryRun = Nothing, _wbcCidr = pCidr_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
wbcDryRun :: Lens' WithdrawByoipCidr (Maybe Bool)
wbcDryRun = lens _wbcDryRun (\ s a -> s{_wbcDryRun = a})

-- | The public IPv4 address range, in CIDR notation.
wbcCidr :: Lens' WithdrawByoipCidr Text
wbcCidr = lens _wbcCidr (\ s a -> s{_wbcCidr = a})

instance AWSRequest WithdrawByoipCidr where
        type Rs WithdrawByoipCidr = WithdrawByoipCidrResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 WithdrawByoipCidrResponse' <$>
                   (x .@? "byoipCidr") <*> (pure (fromEnum s)))

instance Hashable WithdrawByoipCidr where

instance NFData WithdrawByoipCidr where

instance ToHeaders WithdrawByoipCidr where
        toHeaders = const mempty

instance ToPath WithdrawByoipCidr where
        toPath = const "/"

instance ToQuery WithdrawByoipCidr where
        toQuery WithdrawByoipCidr'{..}
          = mconcat
              ["Action" =: ("WithdrawByoipCidr" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _wbcDryRun, "Cidr" =: _wbcCidr]

-- | /See:/ 'withdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { _wbcrsByoipCidr      :: !(Maybe ByoipCidr)
  , _wbcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WithdrawByoipCidrResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbcrsByoipCidr' - Information about the address pool.
--
-- * 'wbcrsResponseStatus' - -- | The response status code.
withdrawByoipCidrResponse
    :: Int -- ^ 'wbcrsResponseStatus'
    -> WithdrawByoipCidrResponse
withdrawByoipCidrResponse pResponseStatus_ =
  WithdrawByoipCidrResponse'
    {_wbcrsByoipCidr = Nothing, _wbcrsResponseStatus = pResponseStatus_}


-- | Information about the address pool.
wbcrsByoipCidr :: Lens' WithdrawByoipCidrResponse (Maybe ByoipCidr)
wbcrsByoipCidr = lens _wbcrsByoipCidr (\ s a -> s{_wbcrsByoipCidr = a})

-- | -- | The response status code.
wbcrsResponseStatus :: Lens' WithdrawByoipCidrResponse Int
wbcrsResponseStatus = lens _wbcrsResponseStatus (\ s a -> s{_wbcrsResponseStatus = a})

instance NFData WithdrawByoipCidrResponse where

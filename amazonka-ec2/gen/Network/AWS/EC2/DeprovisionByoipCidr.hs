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
-- Module      : Network.AWS.EC2.DeprovisionByoipCidr
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified address range that you provisioned for use with your AWS resources through bring your own IP addresses (BYOIP) and deletes the corresponding address pool.
--
--
-- Before you can release an address range, you must stop advertising it using 'WithdrawByoipCidr' and you must not have any IP addresses allocated from its address range.
--
module Network.AWS.EC2.DeprovisionByoipCidr
    (
    -- * Creating a Request
      deprovisionByoipCidr
    , DeprovisionByoipCidr
    -- * Request Lenses
    , depDryRun
    , depCidr

    -- * Destructuring the Response
    , deprovisionByoipCidrResponse
    , DeprovisionByoipCidrResponse
    -- * Response Lenses
    , deprsByoipCidr
    , deprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deprovisionByoipCidr' smart constructor.
data DeprovisionByoipCidr = DeprovisionByoipCidr'
  { _depDryRun :: !(Maybe Bool)
  , _depCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprovisionByoipCidr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'depCidr' - The public IPv4 address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
deprovisionByoipCidr
    :: Text -- ^ 'depCidr'
    -> DeprovisionByoipCidr
deprovisionByoipCidr pCidr_ =
  DeprovisionByoipCidr' {_depDryRun = Nothing, _depCidr = pCidr_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
depDryRun :: Lens' DeprovisionByoipCidr (Maybe Bool)
depDryRun = lens _depDryRun (\ s a -> s{_depDryRun = a})

-- | The public IPv4 address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
depCidr :: Lens' DeprovisionByoipCidr Text
depCidr = lens _depCidr (\ s a -> s{_depCidr = a})

instance AWSRequest DeprovisionByoipCidr where
        type Rs DeprovisionByoipCidr =
             DeprovisionByoipCidrResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeprovisionByoipCidrResponse' <$>
                   (x .@? "byoipCidr") <*> (pure (fromEnum s)))

instance Hashable DeprovisionByoipCidr where

instance NFData DeprovisionByoipCidr where

instance ToHeaders DeprovisionByoipCidr where
        toHeaders = const mempty

instance ToPath DeprovisionByoipCidr where
        toPath = const "/"

instance ToQuery DeprovisionByoipCidr where
        toQuery DeprovisionByoipCidr'{..}
          = mconcat
              ["Action" =: ("DeprovisionByoipCidr" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _depDryRun, "Cidr" =: _depCidr]

-- | /See:/ 'deprovisionByoipCidrResponse' smart constructor.
data DeprovisionByoipCidrResponse = DeprovisionByoipCidrResponse'
  { _deprsByoipCidr      :: !(Maybe ByoipCidr)
  , _deprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeprovisionByoipCidrResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deprsByoipCidr' - Information about the address range.
--
-- * 'deprsResponseStatus' - -- | The response status code.
deprovisionByoipCidrResponse
    :: Int -- ^ 'deprsResponseStatus'
    -> DeprovisionByoipCidrResponse
deprovisionByoipCidrResponse pResponseStatus_ =
  DeprovisionByoipCidrResponse'
    {_deprsByoipCidr = Nothing, _deprsResponseStatus = pResponseStatus_}


-- | Information about the address range.
deprsByoipCidr :: Lens' DeprovisionByoipCidrResponse (Maybe ByoipCidr)
deprsByoipCidr = lens _deprsByoipCidr (\ s a -> s{_deprsByoipCidr = a})

-- | -- | The response status code.
deprsResponseStatus :: Lens' DeprovisionByoipCidrResponse Int
deprsResponseStatus = lens _deprsResponseStatus (\ s a -> s{_deprsResponseStatus = a})

instance NFData DeprovisionByoipCidrResponse where

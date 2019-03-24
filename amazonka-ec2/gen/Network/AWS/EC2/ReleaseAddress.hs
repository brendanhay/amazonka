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
-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified Elastic IP address.
--
--
-- [EC2-Classic, default VPC] Releasing an Elastic IP address automatically disassociates it from any instance that it's associated with. To disassociate an Elastic IP address without releasing it, use 'DisassociateAddress' .
--
-- [Nondefault VPC] You must use 'DisassociateAddress' to disassociate the Elastic IP address before you can release it. Otherwise, Amazon EC2 returns an error (@InvalidIPAddress.InUse@ ).
--
-- After releasing an Elastic IP address, it is released to the IP address pool. Be sure to update your DNS records and any servers or devices that communicate with the address. If you attempt to release an Elastic IP address that you already released, you'll get an @AuthFailure@ error if the address is already allocated to another AWS account.
--
-- [EC2-VPC] After you release an Elastic IP address for use in a VPC, you might be able to recover it. For more information, see 'AllocateAddress' .
--
module Network.AWS.EC2.ReleaseAddress
    (
    -- * Creating a Request
      releaseAddress
    , ReleaseAddress
    -- * Request Lenses
    , raAllocationId
    , raPublicIP
    , raDryRun

    -- * Destructuring the Response
    , releaseAddressResponse
    , ReleaseAddressResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'releaseAddress' smart constructor.
data ReleaseAddress = ReleaseAddress'
  { _raAllocationId :: !(Maybe Text)
  , _raPublicIP     :: !(Maybe Text)
  , _raDryRun       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAllocationId' - [EC2-VPC] The allocation ID. Required for EC2-VPC.
--
-- * 'raPublicIP' - [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
--
-- * 'raDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
releaseAddress
    :: ReleaseAddress
releaseAddress =
  ReleaseAddress'
    {_raAllocationId = Nothing, _raPublicIP = Nothing, _raDryRun = Nothing}


-- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
raAllocationId :: Lens' ReleaseAddress (Maybe Text)
raAllocationId = lens _raAllocationId (\ s a -> s{_raAllocationId = a})

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
raPublicIP :: Lens' ReleaseAddress (Maybe Text)
raPublicIP = lens _raPublicIP (\ s a -> s{_raPublicIP = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
raDryRun :: Lens' ReleaseAddress (Maybe Bool)
raDryRun = lens _raDryRun (\ s a -> s{_raDryRun = a})

instance AWSRequest ReleaseAddress where
        type Rs ReleaseAddress = ReleaseAddressResponse
        request = postQuery ec2
        response = receiveNull ReleaseAddressResponse'

instance Hashable ReleaseAddress where

instance NFData ReleaseAddress where

instance ToHeaders ReleaseAddress where
        toHeaders = const mempty

instance ToPath ReleaseAddress where
        toPath = const "/"

instance ToQuery ReleaseAddress where
        toQuery ReleaseAddress'{..}
          = mconcat
              ["Action" =: ("ReleaseAddress" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AllocationId" =: _raAllocationId,
               "PublicIp" =: _raPublicIP, "DryRun" =: _raDryRun]

-- | /See:/ 'releaseAddressResponse' smart constructor.
data ReleaseAddressResponse =
  ReleaseAddressResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReleaseAddressResponse' with the minimum fields required to make a request.
--
releaseAddressResponse
    :: ReleaseAddressResponse
releaseAddressResponse = ReleaseAddressResponse'


instance NFData ReleaseAddressResponse where

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Releases the specified Elastic IP address. After releasing an Elastic IP
-- address, it is released to the IP address pool and might be unavailable to
-- you. Be sure to update your DNS records and any servers or devices that
-- communicate with the address. If you attempt to release an Elastic IP
-- address that you already released, you'll get an AuthFailure error if the
-- address is already allocated to another AWS account. [EC2-Classic, default
-- VPC] Releasing an Elastic IP address automatically disassociates it from
-- any instance that it's associated with. To disassociate an Elastic IP
-- address without releasing it, use DisassociateAddress. [Nondefault VPC] You
-- must use the DisassociateAddress to disassociate the Elastic IP address
-- before you try to release it. Otherwise, Amazon EC2 returns an error
-- (InvalidIPAddress.InUse). Example for EC2-Classic This example releases the
-- specified Elastic IP address for EC2-Classic.
-- https://ec2.amazonaws.com/?Action=ReleaseAddress &amp;PublicIp=192.0.2.1
-- &amp;AUTHPARAMS Example for EC2-VPC This example releases the specified
-- Elastic IP address for EC2-VPC.
-- https://ec2.amazonaws.com/?Action=ReleaseAddress
-- &amp;AllocationId=eipalloc-5723d13e &amp;AUTHPARAMS.
module Network.AWS.EC2.ReleaseAddress
    (
    -- * Request
      ReleaseAddress
    -- ** Request constructor
    , mkReleaseAddress
    -- ** Request lenses
    , raPublicIp
    , raAllocationId

    -- * Response
    , ReleaseAddressResponse
    -- ** Response constructor
    , mkReleaseAddressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ReleaseAddress = ReleaseAddress
    { _raPublicIp :: Maybe Text
    , _raAllocationId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReleaseAddress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @AllocationId ::@ @Maybe Text@
--
mkReleaseAddress :: ReleaseAddress
mkReleaseAddress = ReleaseAddress
    { _raPublicIp = Nothing
    , _raAllocationId = Nothing
    }

-- | [EC2-Classic] The Elastic IP address.
raPublicIp :: Lens' ReleaseAddress (Maybe Text)
raPublicIp = lens _raPublicIp (\s a -> s { _raPublicIp = a })

-- | [EC2-VPC] The allocation ID.
raAllocationId :: Lens' ReleaseAddress (Maybe Text)
raAllocationId = lens _raAllocationId (\s a -> s { _raAllocationId = a })

instance ToQuery ReleaseAddress where
    toQuery = genericQuery def

data ReleaseAddressResponse = ReleaseAddressResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReleaseAddressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkReleaseAddressResponse :: ReleaseAddressResponse
mkReleaseAddressResponse = ReleaseAddressResponse

instance AWSRequest ReleaseAddress where
    type Sv ReleaseAddress = EC2
    type Rs ReleaseAddress = ReleaseAddressResponse

    request = post "ReleaseAddress"
    response _ = nullaryResponse ReleaseAddressResponse

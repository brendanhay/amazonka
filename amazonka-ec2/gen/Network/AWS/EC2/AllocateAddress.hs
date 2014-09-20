{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Acquires an Elastic IP address. An Elastic IP address is for use either in
-- the EC2-Classic platform or in a VPC. For more information, see Elastic IP
-- Addresses in the Amazon Elastic Compute Cloud User Guide. Example for
-- EC2-Classic This example request allocates an Elastic IP address for use
-- with instances in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AllocateAddress &amp;AUTHPARAMS
-- &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;192.0.2.1&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt; &lt;/AllocateAddressResponse&gt;
-- Example for EC2-VPC This example request allocates an Elastic IP address
-- for use with instances in a VPC.
-- https://ec2.amazonaws.com/?Action=AllocateAddress Domain=vpc
-- &amp;AUTHPARAMS &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;198.51.100.1&lt;/publicIp&gt;
-- &lt;domain&gt;vpc&lt;/domain&gt;
-- &lt;allocationId&gt;eipalloc-5723d13e&lt;/allocationId&gt;
-- &lt;/AllocateAddressResponse&gt;.
module Network.AWS.EC2.AllocateAddress
    (
    -- * Request
      AllocateAddress
    -- ** Request constructor
    , allocateAddress
    -- ** Request lenses
    , aaDomain

    -- * Response
    , AllocateAddressResponse
    -- ** Response constructor
    , allocateAddressResponse
    -- ** Response lenses
    , aarPublicIp
    , aarDomain
    , aarAllocationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype AllocateAddress = AllocateAddress
    { _aaDomain :: Maybe DomainType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateAddress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Maybe DomainType@
--
allocateAddress :: AllocateAddress
allocateAddress = AllocateAddress
    { _aaDomain = Nothing
    }

-- | Set to vpc to allocate the address for use with instances in a VPC.
-- Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\s a -> s { _aaDomain = a })

instance ToQuery AllocateAddress where
    toQuery = genericQuery def

data AllocateAddressResponse = AllocateAddressResponse
    { _aarPublicIp :: Maybe Text
    , _aarDomain :: Maybe DomainType
    , _aarAllocationId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateAddressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PublicIp ::@ @Maybe Text@
--
-- * @Domain ::@ @Maybe DomainType@
--
-- * @AllocationId ::@ @Maybe Text@
--
allocateAddressResponse :: AllocateAddressResponse
allocateAddressResponse = AllocateAddressResponse
    { _aarPublicIp = Nothing
    , _aarDomain = Nothing
    , _aarAllocationId = Nothing
    }

-- | The Elastic IP address.
aarPublicIp :: Lens' AllocateAddressResponse (Maybe Text)
aarPublicIp = lens _aarPublicIp (\s a -> s { _aarPublicIp = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
aarDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarDomain = lens _aarDomain (\s a -> s { _aarDomain = a })

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
aarAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarAllocationId = lens _aarAllocationId (\s a -> s { _aarAllocationId = a })

instance FromXML AllocateAddressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AllocateAddress where
    type Sv AllocateAddress = EC2
    type Rs AllocateAddress = AllocateAddressResponse

    request = post "AllocateAddress"
    response _ = xmlResponse
